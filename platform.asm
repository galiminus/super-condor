    processor 6502
    include "vcs.h"
    include "macro.h"
    include "xmacro.h"

PLAYFIELD_WIDTH             = 4
PLAYFIELD_HEIGHT            = 5
TILE_HEIGHT                 = 6
PLAYER_ANIM_SPEED           = 32
PLAYER_COLOR                = $2a
EYE_COLOR                   = $0e
LASER_COLOR                 = $48
PLAYER_START_Y              = 8

LASER_ENABLED_RANGE         = 4 ; n frames before we enable the laser
LASER_ENABLED_SPEED         = 6
LASER_STEPS                 = 6

EYE_START_X                 = 120

COLLISION_INVERT_DURATION   = 3

LEVEL_METADATA_SIZE         = 4

SPRITE_PADDING              = PLAYFIELD_HEIGHT * TILE_HEIGHT * 4 - 8

LEVEL_COUNT                 = 3

    SEG.U vars
    ORG $80

TMP                    ds 1
PLAYER_X               ds 1
PLAYER_Y               ds 1
PREVIOUS_PLAYER_X      ds 1
PREVIOUS_PLAYER_Y      ds 1

PLAYER_Y_ADDR          ds 1
PLAYER_ANIM_CTR        ds 1
PLAYER_CHAR_FRAME      ds 2
PLAYER_VEC_Y           ds 1
PLAYER_VEC_X           ds 1
EYE_X                  ds 1
KEY_X                  ds 1
CURRENT_EYE_FRAME      ds 2
LASER_TIMER            ds 1
LOST_TIMER             ds 1
IS_KEY_COLLECTED       ds 1

COLLISION_TIMER        ds 1

RANDOM                 ds 1
PLAYFIELD              ds PLAYFIELD_WIDTH * PLAYFIELD_HEIGHT * 2

TIMER_FRAME_COUNT      ds 1
TIMER_DIGITS           ds 8
TIMER_DIGITS_BUFFER    ds 1
TIMER                  ds 2
TIMER_DIGIT_DEC        ds 4

SIDE_DECORATION        ds 1

LEVEL_INDEX            ds 1
LEVEL                  ds 2
LEVEL_TILES            ds 2

MUST_RESET_LEVEL       ds 1

    SEG
    ORG $F000

Reset
    CLEAN_START
    lda #0
    sta LEVEL_INDEX

    SET_POINTER TIMER_DIGITS + 0, Number0
    SET_POINTER TIMER_DIGITS + 2, Number0
    SET_POINTER TIMER_DIGITS + 4, Number0
    SET_POINTER TIMER_DIGITS + 6, Number0

ResetLevel
    lda #0
    sta MUST_RESET_LEVEL

    jsr SetLevel

    lda #PLAYER_ANIM_SPEED
    sta PLAYER_ANIM_CTR

    lda #PLAYER_START_Y
    sta PLAYER_Y

    lda #10
    sta PLAYER_X

    SET_POINTER PLAYER_CHAR_FRAME, CharFrame0

    lda #EYE_START_X
    sta EYE_X

    lda #0
    sta LASER_TIMER

    lda #0
    sta COLLISION_TIMER

    lda #40
    sta KEY_X

    lda #0
    sta IS_KEY_COLLECTED

NextFrame
    VERTICAL_SYNC

    lda LEVEL_INDEX
    cmp #0
    bne .NotHomeScreen
    jsr HomeScreen
    jmp .DoneScreen

.NotHomeScreen
    jsr GameKernel

.DoneScreen
    lda MUST_RESET_LEVEL
    bne ResetLevel
    jmp NextFrame

HomeScreen
    TIMER_SETUP 37
    TIMER_WAIT

    TIMER_SETUP 192

    lda #PLAYER_COLOR
    sta COLUPF

    lda #%00000000
    sta PF0
    sta PF1
    sta PF2

    ldy #75
.HomeScreenLineTop
    sta WSYNC
    dey
    bne .HomeScreenLineTop

    ldy #5
.TitleSuperLine
    ldx #4
.TitleSuperSubLine
    lda #0
    sta PF0

    lda TitlePF1Super1,y
    sta PF1
    lda TitlePF2Super1,y
    sta PF2

    SLEEP 5

    lda TitlePF0Super2,y
    sta PF0
    lda TitlePF1Super2,y
    sta PF1

    lda #0
    sta PF2

    sta WSYNC

    dex
    bne .TitleSuperSubLine

    dey
    bne .TitleSuperLine

    lda #%00000000
    sta PF0
    sta PF1
    sta PF2

    sta WSYNC
    sta WSYNC
    sta WSYNC
    sta WSYNC

    ldy #5
.TitleCondorLine
    ldx #4
.TitleCondorSubLine
    lda #0
    sta PF0

    lda TitlePF1Condor1,y
    sta PF1
    lda TitlePF2Condor1,y
    sta PF2

    SLEEP 10

    lda TitlePF0Condor2,y
    sta PF0
    lda TitlePF1Condor2,y
    sta PF1

    lda #0
    sta PF2

    sta WSYNC

    dex
    bne .TitleCondorSubLine

    dey
    bne .TitleCondorLine

    lda #%00000000
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC

	bit INPT4
    bmi .DoneTitle

    lda #1
    sta LEVEL_INDEX
    lda #1
    sta MUST_RESET_LEVEL

    TIMER_WAIT

    TIMER_SETUP 30
    TIMER_WAIT

.DoneTitle
    rts

GameKernel
    ; VBLANK
    TIMER_SETUP 37

    jsr UpdateRandom

    jsr VBlankHandleBackground
    jsr VBlankHandlePlayfield  
    jsr VBlankHandlePlayer
    jsr VBlankHandleEye
    jsr VBlankHandleLaser
    jsr VBlankHandleKey
    jsr VBlankHandleWall
    jsr VBlankHandleTimer

    sta CXCLR ; clean collisions

    sta WSYNC
    sta HMOVE

    lda #%11111111
    sta PF0
    sta PF1
    sta PF2

    TIMER_WAIT

    TIMER_SETUP 192

    lda #0
    sta VBLANK

    jsr DrawUpperPart
    sta WSYNC
    jsr DrawEyes
    sta WSYNC
    jsr DrawLaser
    sta WSYNC
    ; jsr VBlankHandleWall ; no idea why I had to put it here again

    lda #%11111111
    sta PF0
    sta PF1
    sta PF2

    jsr DrawWall

    lda #192
    sta SIDE_DECORATION

    sta WSYNC

TILE_Y SET 0
    REPEAT PLAYFIELD_HEIGHT
    SUBROUTINE

    IF TILE_Y = PLAYFIELD_HEIGHT - 1
        lda #%00000000
        sta PF0
    ELSE
        lda SIDE_DECORATION ; some graphics stuff, looks like playstation 3D!!!
        sta PF0
    ENDIF ; Build entrance and exit

    ldx #TILE_HEIGHT * 4
.GameKernelLine
        sta WSYNC

        ldy PLAYER_Y_ADDR
        lda (PLAYER_CHAR_FRAME),y
        sta GRP0

        ldy TileDivideTable,x

        lda (PLAYFIELD + TILE_Y * PLAYFIELD_WIDTH * 2),y
        sta PF1
        
        lda (PLAYFIELD + TILE_Y * PLAYFIELD_WIDTH * 2 + 2),y
        sta PF2

        inc PLAYER_Y_ADDR ; put this here to avoid race condition

        SLEEP 4

        lda (PLAYFIELD + TILE_Y * PLAYFIELD_WIDTH * 2 + 4),y
        sta PF2

        lda (PLAYFIELD + TILE_Y * PLAYFIELD_WIDTH * 2 + 6),y
        sta PF1

        IF TILE_Y = 0 ; Draw the key
            iny
            sty ENAM1
        ELSE
            inc SIDE_DECORATION
        ENDIF

        dex
        bne .GameKernelLine
TILE_Y SET TILE_Y + 1
    REPEND

EndGameKernelLineLoop
    sta WSYNC

    lda #0
    sta GRP0
    sta ENAM0

    jsr DrawFloor
    sta WSYNC
    jsr DrawLava
    sta WSYNC

    jsr DrawTimer
    sta WSYNC

    jsr GenerateGameKernelClean

    TIMER_WAIT

    ; OVERSCAN
    TIMER_SETUP 30
	lda #2
    sta VBLANK

    jsr ClearGameKernelPlayfield
    jsr UpdateGameKernelTimers

    TIMER_WAIT
    rts

VBlankHandleTimer
    lda TIMER_FRAME_COUNT
    cmp #60 ; 60 frames per seconds
    beq .FrameCountReached

    inc TIMER_FRAME_COUNT
    rts

.FrameCountReached
    lda #0
    sta TIMER_FRAME_COUNT

    ; update the timer digits, I know it's ugly x)
    inc TIMER_DIGIT_DEC + 0
    lda TIMER_DIGIT_DEC + 0
    cmp #10
    bne .DoneUpdateTimerDigits

    lda #0
    sta TIMER_DIGIT_DEC + 0
    inc TIMER_DIGIT_DEC + 1
    lda TIMER_DIGIT_DEC + 1
    cmp #10
    bne .DoneUpdateTimerDigits

    lda #0
    sta TIMER_DIGIT_DEC + 1
    inc TIMER_DIGIT_DEC + 2
    lda TIMER_DIGIT_DEC + 2
    cmp #10
    bne .DoneUpdateTimerDigits

    lda #0
    sta TIMER_DIGIT_DEC + 2
    inc TIMER_DIGIT_DEC + 3
    lda TIMER_DIGIT_DEC + 3
    cmp #10
    bne .DoneUpdateTimerDigits

.DoneUpdateTimerDigits

; TIMER_DIGIT_DEC_INDEX SET 0
;     REPEAT 4
; TIMER_DIGIT_VALUE SET 0
;     REPEAT 10
;         SUBROUTINE
;         lda TIMER_DIGIT_DEC + TIMER_DIGIT_DEC_INDEX
;         cmp #TIMER_DIGIT_VALUE
;         bne .NextDigit
;         SET_POINTER TIMER_DIGITS + TIMER_DIGIT_DEC_INDEX * 2, Number0 + TIMER_DIGIT_VALUE * 8
; .NextDigit
; TIMER_DIGIT_VALUE SET TIMER_DIGIT_VALUE + 1
;     REPEND
; TIMER_DIGIT_DEC_INDEX SET TIMER_DIGIT_DEC_INDEX + 1
;     REPEND

    rts

VBlankHandlePlayer
    lda #PLAYER_COLOR
    sta COLUP0

    lda LOST_TIMER
    beq .DontFreezePlayer
    lda PLAYER_Y
    sta PLAYER_Y_ADDR
    lda PLAYER_X
    ldx #0
    jsr FineAdjustSprite

    lda #EYE_COLOR
    sta COLUP0 ; set the player color to white when we get hit

    rts

.DontFreezePlayer
    dec PLAYER_ANIM_CTR
    lda PLAYER_ANIM_CTR
    cmp #PLAYER_ANIM_SPEED / 2
    bne .NotFrame0

    SET_POINTER PLAYER_CHAR_FRAME, CharFrame0
    jmp .NotFrame1

.NotFrame0
    cmp #0
    bne .NotFrame1
    SET_POINTER PLAYER_CHAR_FRAME, CharFrame1

.NotFrame1

	lda #%10000000
	bit SWCHA
	bne DoneMoveRight

    lda IS_KEY_COLLECTED
    bne .LimitWithWallClosed
    lda PLAYER_X
    cmp #146
    beq DoneMoveRight

.LimitWithWallClosed
    lda PLAYER_X
    cmp #158
    bne .NotNextLevel

    inc LEVEL_INDEX
    lda LEVEL_INDEX
    cmp LEVEL_COUNT
    bne .NotTheEnd
    lda #0
    sta LEVEL_INDEX

.NotTheEnd
    lda #1
    sta MUST_RESET_LEVEL
    rts

.NotNextLevel
    lda PLAYER_ANIM_CTR
    clc
    cmp #PLAYER_ANIM_SPEED / 2
    bcs .NotMoveRightFrame0
    SET_POINTER PLAYER_CHAR_FRAME, CharFrameMoveRight0
    jmp .NotMoveRightFrame1
.NotMoveRightFrame0
    SET_POINTER PLAYER_CHAR_FRAME, CharFrameMoveRight1
.NotMoveRightFrame1

    lda COLLISION_TIMER
    bne .InvertMoveRight
        inc PLAYER_X
        inc PLAYER_X
        jmp DoneMoveRight
.InvertMoveRight
        dec PLAYER_X
        dec PLAYER_X
DoneMoveRight

	lda #%01000000
	bit SWCHA
	bne DoneMoveLeft
    lda PLAYER_X
    cmp #10
    beq DoneMoveLeft

    lda PLAYER_ANIM_CTR
    clc
    cmp #PLAYER_ANIM_SPEED / 2
    bcs .NotMoveLeftFrame0
    SET_POINTER PLAYER_CHAR_FRAME, CharFrameMoveLeft0
    jmp .NotMoveLeftFrame1
.NotMoveLeftFrame0
    SET_POINTER PLAYER_CHAR_FRAME, CharFrameMoveLeft1
.NotMoveLeftFrame1

    lda COLLISION_TIMER
    bne .InvertMoveLeft
        dec PLAYER_X
        dec PLAYER_X
        jmp DoneMoveLeft
.InvertMoveLeft
        inc PLAYER_X
        inc PLAYER_X

DoneMoveLeft

	lda #%00100000
	bit SWCHA
	bne DoneMoveDown

    lda COLLISION_TIMER
    bne .InvertMoveDown
        dec PLAYER_Y
        dec PLAYER_Y
        jmp DoneMoveDown
.InvertMoveDown
    inc PLAYER_Y
    inc PLAYER_Y
DoneMoveDown

	lda #%00010000
	bit SWCHA
	bne DoneMoveUp

    lda COLLISION_TIMER
    bne .InvertMoveUp
        inc PLAYER_Y
        inc PLAYER_Y
        jmp DoneMoveUp
.InvertMoveUp
    dec PLAYER_Y
    dec PLAYER_Y
DoneMoveUp

	; lda #%00010000
	; bit SWCHA
	; beq NoGravity

    ; lda COLLISION_TIMER
    ; bne NoGravity
    ; dec PLAYER_Y ; gravity

NoGravity
    lda PLAYER_Y
    sec
    cmp #PLAYER_START_Y
    bcs .PlayerStillWithinBoundaries
    lda #PLAYER_START_Y
    sta PLAYER_Y

.PlayerStillWithinBoundaries

.DoneWithPlayer
    lda PLAYER_ANIM_CTR
    bne .DontResetPlayerAnimCtr
    lda #PLAYER_ANIM_SPEED
    sta PLAYER_ANIM_CTR
.DontResetPlayerAnimCtr

    ; check for playfield/player collision

    bit CXP0FB
    bpl .NoPlayfieldPlayerCollision

     ; we got a collision, roll back to the previous position
    lda #COLLISION_INVERT_DURATION
    sta COLLISION_TIMER ; invert the control to give a feeling of "bounce"
    
    lda PREVIOUS_PLAYER_X
    sta PLAYER_X
    lda PREVIOUS_PLAYER_Y
    sta PLAYER_Y
    jmp .DonePlayfieldPlayerCollision

.NoPlayfieldPlayerCollision
    ; no collision occured, store the new position values
    ; lda PLAYER_X
    ; sta PREVIOUS_PLAYER_X
    ; lda PLAYER_Y
    ; sta PREVIOUS_PLAYER_Y
.DonePlayfieldPlayerCollision
    lda PLAYER_Y
    sta PLAYER_Y_ADDR

    lda PLAYER_X
    ldx #0
    jsr FineAdjustSprite

    rts

DrawUpperPart
    ldx #TILE_HEIGHT * 2
.UpperScreenLine
        and #$00000001
        bne .NoStripe
        lda #%11111111
        sta PF0
        sta PF1
        sta PF2
        jmp .HadStripe
.NoStripe
        lda #0
        sta PF0
        sta PF1
        sta PF2
.HadStripe
        sta WSYNC
        dex
        bne .UpperScreenLine
    rts

DrawEyes
    lda #EYE_COLOR
    sta COLUP1

    lda #0
    sta PF0
    sta PF1
    sta PF2

    ldy #0
.EyeLine
        sta WSYNC

        lda (CURRENT_EYE_FRAME),y
        sta GRP1

        iny
        cpy #9
        bne .EyeLine

    lda #0
    sta GRP0
    sta GRP1
    rts

DrawLaser
    SUBROUTINE
    sta WSYNC

    ldy #1
    lda (LEVEL),y
    sta COLUP1

    lda IS_KEY_COLLECTED
    bne .DontEnableLaserColor; the key wasn't collected, we enable the laser color for it since it's M1
    lda #LASER_COLOR
    sta COLUP1

.DontEnableLaserColor
    lda LASER_TIMER
    beq .DoneWithLaser ; the laser is not active

    sec
    sbc #LASER_ENABLED_RANGE
    bcc .DoneWithLaser ; the laser is not yet active but will be soon

    tay
    lda LaserFrames,y
    sta GRP1

    sec
    cpy #LASER_ENABLED_RANGE
    bcc .DoneWithLaser

    lda #LASER_COLOR
    sta COLUP1

.DoneWithLaser
    sta WSYNC
    rts

DrawWall
    lda IS_KEY_COLLECTED
    bne KeyWasCollected
    lda #%00001111
    sta ENABL
KeyWasCollected
    rts

VBlankHandleLaser
    lda EYE_X
    clc
    adc #4
    clc
    sbc PLAYER_X
    cmp #4

    bcs .NotInRange
    jsr EnableLaser

.NotInRange

    ; check for collision Player/Laser
    bit CXPPMM
    bpl .NoPlayerCollision

    jsr EnableLostRound

.NoPlayerCollision
    rts

VBlankHandleKey
    bit CXM1P
    bpl .NoKeyCollection
    lda #1
    sta IS_KEY_COLLECTED

.NoKeyCollection
    lda IS_KEY_COLLECTED
    beq .PutKeyInPlayfield

    lda EYE_X ; We hide the key in the laser beam because we don't have the time to check
              ; for IS_KEY_COLLECTED in the kernel
    clc
    adc #4
    jmp .DoneKeyPosition
.PutKeyInPlayfield
    lda KEY_X
.DoneKeyPosition
    ldx #3
    jsr FineAdjustSprite

    rts

VBlankHandleWall
    lda #155
    ldx #4
    jsr FineAdjustSprite
    rts

VBlankHandleEye
    lda #EYE_COLOR
    sta COLUP1

    SET_POINTER CURRENT_EYE_FRAME, EyeFrameAttack
    lda LASER_TIMER
    bne .DoneMove

    lda EYE_X
    
    sec
    cmp PLAYER_X
    beq .DoneMove
    bcc .MoveRight

    ; Move Left
    lda EYE_X
    sec
    cmp #24
    bcc .DoneMove

    SET_POINTER CURRENT_EYE_FRAME, EyeFrameLeft

    dec EYE_X
    jmp .DoneMove

.MoveRight
    SET_POINTER CURRENT_EYE_FRAME, EyeFrameRight

    lda EYE_X
    sec
    cmp #143
    bcs .DoneMove

    inc EYE_X

.DoneMove
    lda EYE_X
    ldx #1
    jsr FineAdjustSprite

    rts

DrawFloor
    lda #%11111111
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC
    sta WSYNC
    rts

DrawLava
    lda #%00000000
    sta ENABL

    lda LOST_TIMER ; don't draw lava if we are in the "Lost" screen
    bne .DontDrawLava

    lda #$23
    sta COLUPF ; set lava color

    lda #$20
    sta COLUBK    

.DontDrawLava
    REPEAT 4
    jsr UpdateRandom
    lda RANDOM
    sta PF0

    jsr UpdateRandom
    lda RANDOM
    sta PF1

    jsr UpdateRandom
    lda RANDOM
    sta PF2

        REPEAT 3
            sta WSYNC
        REPEND
    REPEND
    rts

DrawTimer
    sta WSYNC

    lda #$00
    sta COLUBK

    lda #0
    sta PF0
    sta PF1
    sta PF2

    lda #%00000000
    sta GRP1

    lda #00
    sta COLUP0

    lda #EYE_COLOR
    sta COLUP1

    lda #%00000010
    sta CTRLPF ; use player color to hide the left part

    sta WSYNC

    ldy #0
.TimerLine
    sta WSYNC

    lda (TIMER_DIGITS + 6),y
    and #%11110000
    sta TIMER_DIGITS_BUFFER

    lda (TIMER_DIGITS + 4),y
    lsr
    lsr
    lsr
    lsr
    and #%00001111
    ora TIMER_DIGITS_BUFFER
    asl
    sta PF1

    lda (TIMER_DIGITS + 2),y
    and #%00001111
    sta TIMER_DIGITS_BUFFER
    lda (TIMER_DIGITS + 0),y
    asl
    asl
    asl
    asl
    ora TIMER_DIGITS_BUFFER
    lsr
    sta PF2

    iny
    cpy #8
    bne .TimerLine

    lda #0
    sta PF1
    sta PF2

    rts

GenerateGameKernelClean
    lda #$00
    sta COLUPF

    lda #$00
    sta COLUBK

    lda #%00000000
    sta GRP1

    lda #%00000000
    sta ENABL

    bit CXP0FB
    bmi .PlayfieldPlayerCollision
    lda PLAYER_X
    sta PREVIOUS_PLAYER_X
    lda PLAYER_Y
    sta PREVIOUS_PLAYER_Y

.PlayfieldPlayerCollision
    rts

VBlankHandleBackground
    ldy #1
    lda (LEVEL),y
    sta COLUBK    

    rts

VBlankHandlePlayfield
    ldy #2
    lda (LEVEL),y
    sta COLUPF ; set playfield color

    lda #%00000001
    sta CTRLPF ; enable mirroring, that will come handy for PF0

    lda #%11110000
    sta PF0 ; build walls on the left and right
    lda #%00000000
    sta PF1
    sta PF2

    ldy #0
.CopyPlayfieldMatrix
    lda (LEVEL_TILES),y
    sta PLAYFIELD,y
    iny
    cpy #40
    bne .CopyPlayfieldMatrix

    rts

ClearGameKernelPlayfield
    lda #%00000000
    sta PF0
    sta PF1
    sta PF2
    rts

UpdateGameKernelTimers
    ; Handle all the laser timer part
    SUBROUTINE
    lda LASER_TIMER
    beq .DoneWithLaser

    cmp #LASER_ENABLED_RANGE + LASER_STEPS * LASER_ENABLED_SPEED
    beq .DisableLaser

    inc LASER_TIMER
    jmp .DoneWithLaser

.DisableLaser
    lda #0
    sta LASER_TIMER

.DoneWithLaser

    lda LOST_TIMER
    beq .DoneWithLostTimer
    dec LOST_TIMER
    bne .DontResetGame

    lda #1
    sta MUST_RESET_LEVEL    
    jmp .DoneWithCollisionTimer

.DontResetGame
.DoneWithLostTimer

    lda COLLISION_TIMER
    beq .DoneWithCollisionTimer
    dec COLLISION_TIMER

.DoneWithCollisionTimer
    rts

EnableLaser
    rts
    lda LASER_TIMER
    bne .LaserAlreadyEnabled
    lda #%00000001
    sta LASER_TIMER

.LaserAlreadyEnabled
    rts

UpdateRandom
    lda RANDOM
    asl
    asl
    clc
    adc RANDOM
    clc
    adc #17        ; RANDOM * 5 + 17
    sta RANDOM
    rts

EnableLostRound
    ldy #1
    lda (LEVEL),y
    sta COLUPF

    lda LOST_TIMER
    bne .LostTimerAlreadySet

    lda #35
    sta LOST_TIMER

.LostTimerAlreadySet
    rts

SetLevel
    lda LEVEL_INDEX
    cmp #1
    bne .Not1
    SET_POINTER LEVEL, Level1    
    SET_POINTER LEVEL_TILES, Level1Tiles  
.Not1
    lda LEVEL_INDEX
    cmp #2
    bne .Not2
    SET_POINTER LEVEL, Level2    
    SET_POINTER LEVEL_TILES, Level2Tiles    
.Not2
    lda LEVEL_INDEX
    cmp #3
    bne .Not3
    SET_POINTER LEVEL, Level3    
    SET_POINTER LEVEL_TILES, Level3Tiles    
.Not3
    lda LEVEL_INDEX
    cmp #4
    bne .Not4
    SET_POINTER LEVEL, Level4    
    SET_POINTER LEVEL_TILES, Level4Tiles    
.Not4
    lda LEVEL_INDEX
    cmp #5
    bne .Not5
    SET_POINTER LEVEL, Level5
    SET_POINTER LEVEL_TILES, Level5Tiles    
.Not5
    lda LEVEL_INDEX
    cmp #6
    bne .Not6
    SET_POINTER LEVEL, Level6
    SET_POINTER LEVEL_TILES, Level6Tiles    
.Not6
    lda LEVEL_INDEX
    cmp #7
    bne .Not7
    SET_POINTER LEVEL, Level7
    SET_POINTER LEVEL_TILES, Level7Tiles    
.Not7
    lda LEVEL_INDEX
    cmp #8
    bne .Not8
    SET_POINTER LEVEL, Level8
    SET_POINTER LEVEL_TILES, Level8Tiles    
.Not8
    lda LEVEL_INDEX
    cmp #9
    bne .Not9
    SET_POINTER LEVEL, Level9    
    SET_POINTER LEVEL_TILES, Level9Tiles    
.Not9

    rts

FineAdjustSprite
    sta WSYNC
    sec                      ; 02     Set the carry flag so no borrow will be applied during the division.
.Divideby15
    sbc #15                  ; 04     Waste the necessary amount of time dividing X-pos by 15!
    bcs .Divideby15           ; 06/07  11/16/21/26/31/36/41/46/51/56/61/66
    tay
    lda fineAdjustTable,y    ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
    sta HMP0,x
    sta RESP0,x              ; 21/ 26/31/36/41/46/51/56/61/66/71 - Set the rough position.
    rts

LaserFrames
    REPEAT LASER_ENABLED_RANGE
    .byte #%00000000
    REPEND
    REPEAT LASER_ENABLED_SPEED
    .byte #%01111100
    REPEND
    REPEAT LASER_ENABLED_SPEED
    .byte #%11111110
    REPEND
    REPEAT LASER_ENABLED_SPEED
    .byte #%01111100
    REPEND
    REPEAT LASER_ENABLED_SPEED
    .byte #%00111000
    REPEND
    REPEAT LASER_ENABLED_SPEED
    .byte #%00010000
    REPEND
    REPEAT LASER_ENABLED_SPEED
    .byte #%00000000
    REPEND


Level9
    .byte #187 ; decoration
    .byte #$02  ; background color
    .byte #$a3  ; playfield color
    .byte #$a1  ; lava color
Level9Tiles
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile5
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6

    BOUNDARY $00
CharFrame0
    REPEAT SPRITE_PADDING
    .byte #%00000000
    REPEND
        .byte #%00010000;--
        .byte #%00011100;--
        .byte #%00011010;--
        .byte #%00011110;--
        .byte #%11011100;--
        .byte #%01111100;--
        .byte #%00111100;--
        .byte #%01111100;--
        .byte #%01111111;--
        .byte #%01111111;--
        .byte #%00111110;--
        .byte #%00111100;--
        .byte #%00011100;--
        .byte #%00010100;--
        .byte #%00010100;--
        .byte #%00110110;--
    REPEAT SPRITE_PADDING
    .byte #%00000000
    REPEND

EyeFrameLeft
    .byte #%00111100;--
    .byte #%01111110;--
    .byte #%01111110;--
    .byte #%01001110;--
    .byte #%01011110;--
    .byte #%01001110;--
    .byte #%01001110;--
    .byte #%00111100;--

Tile1
    .byte #%10000001
    .byte #%00000001 
    .byte #%00000001
    .byte #%00000001
    .byte #%00000001
    .byte #%00000001

    BOUNDARY $00
CharFrame1
    REPEAT SPRITE_PADDING
    .byte #%00000000
    REPEND
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00010000;--
        .byte #%00011100;--
        .byte #%00011010;--
        .byte #%00011110;--
        .byte #%11011100;--
        .byte #%01111100;--
        .byte #%00111100;--
        .byte #%01111100;--
        .byte #%01111111;--
        .byte #%01111111;--
        .byte #%00111110;--
        .byte #%00011100;--
        .byte #%00011100;--
        .byte #%00110110;--
    REPEAT SPRITE_PADDING
    .byte #%00000000
    REPEND

EyeFrameRight
    .byte #%00111100;--
    .byte #%01111110;--
    .byte #%01111110;--
    .byte #%01110010;--
    .byte #%01111010;--
    .byte #%01110010;--
    .byte #%01110010;--
    .byte #%00111100;--

Tile2
    .byte #%11111111
    .byte #%10000000 
    .byte #%10001000
    .byte #%10000000
    .byte #%10000000
    .byte #%11111111 ; Tile 2

    BOUNDARY $00
CharFrameMoveRight0
    REPEAT SPRITE_PADDING
    .byte #%00000000
    REPEND
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%01110000;--
        .byte #%01111110;--
        .byte #%11111101;--
        .byte #%11111111;--
        .byte #%01111110;--
        .byte #%11111100;--
        .byte #%11111000;--
        .byte #%00101000;--
        .byte #%00111100;--
        .byte #%00000000;--
        .byte #%00000000;--
    REPEAT SPRITE_PADDING
    .byte #%00000000
    REPEND

EyeFrameAttack
    .byte #%01111100;--
    .byte #%11111110;--
    .byte #%11111110;--
    .byte #%11000110;--
    .byte #%11001110;--
    .byte #%11000110;--
    .byte #%11000110;--
    .byte #%01111100;--

Tile3
    .byte #%11111111 ; Tile 3
    .byte #%01111111
    .byte #%00111111
    .byte #%00011111
    .byte #%00001111
    .byte #%00000111

    BOUNDARY $00
CharFrameMoveRight1
    REPEAT SPRITE_PADDING
    .byte #%00000000
    REPEND
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%01110000;--
        .byte #%11111110;--
        .byte #%11111101;--
        .byte #%01111111;--
        .byte #%01111110;--
        .byte #%11111100;--
        .byte #%11111000;--
        .byte #%01111000;--
        .byte #%00111100;--
        .byte #%00000000;--
        .byte #%00000000;--
    REPEAT SPRITE_PADDING
    .byte #%00000000
    REPEND

Tile5
    .byte #%11111110
    .byte #%11111110 
    .byte #%11111110
    .byte #%11111110
    .byte #%11111110
    .byte #%11111110 ; Tile 5

Tile6
    .byte #%00000000
    .byte #%00000000 
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000 ; Tile 6

    BOUNDARY $00
CharFrameMoveLeft0
    REPEAT SPRITE_PADDING
    .byte #%00000000
    REPEND
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00001110;--
        .byte #%01111110;--
        .byte #%10111111;--
        .byte #%11111111;--
        .byte #%01111110;--
        .byte #%00111111;--
        .byte #%00011111;--
        .byte #%00010100;--
        .byte #%00111100;--
        .byte #%00000000;--
        .byte #%00000000;--
    REPEAT SPRITE_PADDING
    .byte #%00000000
    REPEND

TitlePF1Super1
   .byte #%00000000
   .byte #%00000001
   .byte #%00000000
   .byte #%00000001
   .byte #%00000001
   .byte #%00000001
TitlePF2Super1
   .byte #%00000000
   .byte #%10111011
   .byte #%10101010
   .byte #%10101011
   .byte #%10101000
   .byte #%10101011

    BOUNDARY $00
CharFrameMoveLeft1
    REPEAT SPRITE_PADDING
    .byte #%00000000
    REPEND
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00001110;--
        .byte #%01111111;--
        .byte #%10111111;--
        .byte #%11111110;--
        .byte #%01111110;--
        .byte #%00111111;--
        .byte #%00011111;--
        .byte #%00011110;--
        .byte #%00111100;--
        .byte #%00000000;--
        .byte #%00000000;--
    REPEAT SPRITE_PADDING
    .byte #%00000000
    REPEND

;; Numbers
Number0
    .byte #%01111110
    .byte #%01011010 
    .byte #%01011010
    .byte #%01011010
    .byte #%01011010
    .byte #%01011010
    .byte #%01011010
    .byte #%01111110

Number1
    .byte #%00101000
    .byte #%01101100 
    .byte #%01101100 
    .byte #%00101000
    .byte #%00101000
    .byte #%00101000
    .byte #%00101000
    .byte #%00101000

Number2
    .byte #%01111110
    .byte #%00011000
    .byte #%00011000
    .byte #%00011000
    .byte #%01111110
    .byte #%01000010
    .byte #%01000010
    .byte #%01111110

Number3
    .byte #%01111110
    .byte #%00011000 
    .byte #%00011000 
    .byte #%01111110
    .byte #%01111110
    .byte #%00011000
    .byte #%00011000
    .byte #%01111110

Number4
    .byte #%01000010
    .byte #%01000010 
    .byte #%01000010
    .byte #%01111110
    .byte #%00011000
    .byte #%00011000
    .byte #%00011000
    .byte #%00011000

Number5
    .byte #%01111110
    .byte #%01000010 
    .byte #%01000010
    .byte #%01111110
    .byte #%01111110
    .byte #%00011000
    .byte #%00011000
    .byte #%01111110

Number6
    .byte #%01111110
    .byte #%01000010 
    .byte #%01000010 
    .byte #%01000010
    .byte #%01111110
    .byte #%01011010
    .byte #%01011010
    .byte #%01111110

Number7
    .byte #%01111110
    .byte #%00011000 
    .byte #%00011000
    .byte #%00011000
    .byte #%00100100
    .byte #%00100100
    .byte #%00100100
    .byte #%00100100

Number8
    .byte #%01111110
    .byte #%01011010 
    .byte #%01011010
    .byte #%01111110
    .byte #%01111110
    .byte #%01011010
    .byte #%01011010
    .byte #%01111110

Number9
    .byte #%01111110
    .byte #%01011010 
    .byte #%01011010
    .byte #%01111110
    .byte #%01111110
    .byte #%00011000
    .byte #%00011000
    .byte #%01111110

TitlePF0Super2
   .byte #%00000000
   .byte #%10000001
   .byte #%10000001
   .byte #%10111101
   .byte #%10100101
   .byte #%10111101
TitlePF1Super2
   .byte #%00000000
   .byte #%11010100
   .byte #%00011000
   .byte #%11011100
   .byte #%00010100
   .byte #%11011100

TitlePF1Condor1
   .byte #%00000000
   .byte #%00000111
   .byte #%00000100
   .byte #%00000100
   .byte #%00000100
   .byte #%00000111
TitlePF2Condor1
   .byte #%00000000
   .byte #%10101110
   .byte #%11101010
   .byte #%11101010
   .byte #%11101010
   .byte #%10101110

TitlePF0Condor2
   .byte #%00000000
   .byte #%11100000
   .byte #%11000000
   .byte #%11000000
   .byte #%11000000
   .byte #%11100000
TitlePF1Condor2
   .byte #%00000000
   .byte #%01110101
   .byte #%01010110
   .byte #%01010111
   .byte #%01010101
   .byte #%01110111

Level1
    .byte #192 ; decoration
    .byte #$01  ; background color
    .byte #$02  ; playfield color
    .byte #$02  ; lava color
Level1Tiles
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile5
    .word Tile6
    .word Tile5
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile5
    .word Tile6
    .word Tile6
    .word Tile3
    .word Tile6
    .word Tile6

Level2
    .byte #187 ; decoration
    .byte #$02  ; background color
    .byte #$a3  ; playfield color
    .byte #$a1  ; lava color
Level2Tiles
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile5
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6

Level3
    .byte #187 ; decoration
    .byte #$02  ; background color
    .byte #$a3  ; playfield color
    .byte #$a1  ; lava color
Level3Tiles
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile5
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6

Level4
    .byte #187 ; decoration
    .byte #$02  ; background color
    .byte #$a3  ; playfield color
    .byte #$a1  ; lava color
Level4Tiles
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile5
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6

Level5
    .byte #187 ; decoration
    .byte #$02  ; background color
    .byte #$a3  ; playfield color
    .byte #$a1  ; lava color
Level5Tiles
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile5
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6

Level6
    .byte #187 ; decoration
    .byte #$02  ; background color
    .byte #$a3  ; playfield color
    .byte #$a1  ; lava color
Level6Tiles
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile5
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6

Level7
    .byte #187 ; decoration
    .byte #$02  ; background color
    .byte #$a3  ; playfield color
    .byte #$a1  ; lava color
Level7Tiles
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile5
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6

Level8
    .byte #187 ; decoration
    .byte #$02  ; background color
    .byte #$a3  ; playfield color
    .byte #$a1  ; lava color
Level8Tiles
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile5
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6
    .word Tile6

TileDivideTable
.TileDivideTableY SET 0
    REPEAT TILE_HEIGHT
        REPEAT 5
        .byte .TileDivideTableY
        REPEND
.TileDivideTableY SET .TileDivideTableY + 1
    REPEND

fineAdjustBegin
    DC.B %01110000 ; Left 7
    DC.B %01100000 ; Left 6
    DC.B %01010000 ; Left 5
    DC.B %01000000 ; Left 4
    DC.B %00110000 ; Left 3
    DC.B %00100000 ; Left 2
    DC.B %00010000 ; Left 1
    DC.B %00000000 ; No movement.
    DC.B %11110000 ; Right 1
    DC.B %11100000 ; Right 2
    DC.B %11010000 ; Right 3
    DC.B %11000000 ; Right 4
    DC.B %10110000 ; Right 5
    DC.B %10100000 ; Right 6
    DC.B %10010000 ; Right 7

fineAdjustTable EQU fineAdjustBegin - %11110001 ; NOTE: %11110001 = -15

    ORG $FFFA
InterruptVectors
    .word Reset          ; NMI
    .word Reset          ; RESET
    .word Reset          ; IRQ

    END
