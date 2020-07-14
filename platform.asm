    processor 6502
    include "vcs.h"
    include "macro.h"
    include "xmacro.h"

PLAYFIELD_WIDTH             = 4
PLAYFIELD_HEIGHT            = 5
TILE_HEIGHT                 = 6
PLAYFIELD_COLOR             = $02
LAVA_COLOR                  = $38
BACKGROUND_COLOR            = $01
PLAYER_ANIM_SPEED           = 64
PLAYER_COLOR                = $2a
EYE_COLOR                   = $0e
LASER_COLOR                 = $48

LASER_ENABLED_RANGE         = 6 ; n frames before we enable the laser
LASER_ENABLED_SPEED         = 6
LASER_STEPS                 = 6

EYE_START_X                 = 120

    SEG.U vars
    ORG $80

TMP                    ds 1
PLAYER_X               ds 1
PLAYER_Y               ds 1
PLAYER_Y_ADDR          ds 1
PLAYER_ANIM_CTR        ds 1
PLAYER_CHAR_FRAME      ds 2
EYE_X                  ds 1
KEY_X                  ds 1
LASER_TIMER            ds 1
LOST_TIMER             ds 1
IS_KEY_COLLECTED       ds 1

COLLISION_X            ds 1
COLLISION_Y            ds 1

COLLISION_TILE_X       ds 1
COLLISION_TILE_Y       ds 1

KEY_SPRITE             ds 6


RANDOM                 ds 1
PLAYFIELD              ds PLAYFIELD_WIDTH * PLAYFIELD_HEIGHT * 2

    SEG
    ORG $F000

Reset
    CLEAN_START

    lda #PLAYER_ANIM_SPEED
    sta PLAYER_ANIM_CTR

    lda #8
    sta PLAYER_Y

    lda #10
    sta PLAYER_X

    SET_POINTER PLAYER_CHAR_FRAME, CharFrame0

    lda #EYE_START_X
    sta EYE_X

    lda #0
    sta LASER_TIMER

    lda #40
    sta KEY_X

    lda #0
    sta IS_KEY_COLLECTED

NextFrame
    VERTICAL_SYNC

    ; SCREEN (remove me maybe)
    jsr GameKernel

    jmp NextFrame

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
    jsr DrawEyes
    jsr DrawLaser
    jsr DrawWall
    sta WSYNC

    lda #%11111111
    sta PF1
    sta PF2

TILE_Y SET 0
    REPEAT PLAYFIELD_HEIGHT
    SUBROUTINE

    IF TILE_Y = PLAYFIELD_HEIGHT - 1
        lda #%00000000
        sta PF0
    ELSE
        lda #%11110000
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
            lda #0
            sta ENAM1
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

    jsr GenerateGameKernelFloor
    jsr GenerateGameKernelLava
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

VBlankHandlePlayer
    lda #PLAYER_COLOR
    sta COLUP0

    lda LOST_TIMER
    bne .DoneWithPlayer

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

    lda #PLAYER_ANIM_SPEED
    sta PLAYER_ANIM_CTR

.NotFrame1

	lda #%10000000
	bit SWCHA
	bne DoneMoveRight
    lda PLAYER_X
    cmp #158
    beq DoneMoveRight

    inc PLAYER_X
    inc PLAYER_X
DoneMoveRight

	lda #%01000000
	bit SWCHA
	bne DoneMoveLeft
    lda PLAYER_X
    cmp #10
    beq DoneMoveLeft

    dec PLAYER_X
    dec PLAYER_X
DoneMoveLeft

	lda #%00100000
	bit SWCHA
	bne DoneMoveDown

    dec PLAYER_Y
DoneMoveDown

	lda #%00010000
	bit SWCHA
	bne DoneMoveUp

    inc PLAYER_Y
DoneMoveUp

    ; Compute collision pixel
    lda PLAYER_X
    clc
    adc #4
    sta COLLISION_X

    lda PLAYER_Y
    clc
    adc #1
    sta COLLISION_Y

    ; Find the tile where this pixel lies
    lda COLLISION_X
    lsr
    lsr
    lsr
    lsr
    lsr ; divide by 64
    sta COLLISION_TILE_X

    ; lda COLLISION_Y
    lda #4
    sta COLLISION_TILE_Y


.DoneWithPlayer
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

    ldx #0
    lda #0
    sta PF0
    sta PF1
    sta PF2
.EyeLine
        sta WSYNC

        lda EyeFrame0,x
        sta GRP1

        inx
        cpx #8
        bne .EyeLine

    lda #0
    sta GRP0
    sta GRP1
    rts

DrawLaser
    SUBROUTINE
    sta WSYNC

    lda #0
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
    adc #8
    clc
    sbc PLAYER_X
    cmp #16

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
    lda #160
    ldx #4
    jsr FineAdjustSprite
    rts

VBlankHandleEye
    lda #EYE_COLOR
    sta COLUP1

    lda LASER_TIMER
    bne .DoneMove

    lda EYE_X
    
    sec
    cmp PLAYER_X
    beq .DoneMove
    bcc .MoveRight
    lda EYE_X
    sec
    cmp #24
    bcc .DoneMove

    dec EYE_X
    jmp .DoneMove

.MoveRight
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

GenerateGameKernelFloor
    lda #%11111111
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC
    sta WSYNC
    rts

GenerateGameKernelLava
    lda #$23
    sta COLUPF ; set lava color

    lda #$20
    sta COLUBK    

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

GenerateGameKernelClean
    lda #$00
    sta COLUPF

    lda #$00
    sta COLUBK

    lda #%00000000
    sta GRP1

    lda #%00000000
    sta ENABL

    rts

VBlankHandleBackground
    lda #PLAYFIELD_COLOR
    sta COLUPF ; set playfield color
    rts

VBlankHandlePlayfield
    lda #BACKGROUND_COLOR
    sta COLUBK    

    lda #%00000001
    sta CTRLPF ; enable mirroring, that will come handy for PF0

    lda #%11110000
    sta PF0 ; build walls on the left and right
    lda #%00000000
    sta PF1
    sta PF2

    SET_POINTER PLAYFIELD + 0, Tile6
    SET_POINTER PLAYFIELD + 2, Tile6
    SET_POINTER PLAYFIELD + 4, Tile6
    SET_POINTER PLAYFIELD + 6, Tile6

    SET_POINTER PLAYFIELD + 8, Tile5
    SET_POINTER PLAYFIELD + 10, Tile6
    SET_POINTER PLAYFIELD + 12, Tile5
    SET_POINTER PLAYFIELD + 14, Tile6

    SET_POINTER PLAYFIELD + 16, Tile6
    SET_POINTER PLAYFIELD + 18, Tile5
    SET_POINTER PLAYFIELD + 20, Tile6
    SET_POINTER PLAYFIELD + 22, Tile5

    SET_POINTER PLAYFIELD + 24, Tile6
    SET_POINTER PLAYFIELD + 26, Tile6
    SET_POINTER PLAYFIELD + 28, Tile6
    SET_POINTER PLAYFIELD + 30, Tile6

    SET_POINTER PLAYFIELD + 32, Tile6
    SET_POINTER PLAYFIELD + 34, Tile3B
    SET_POINTER PLAYFIELD + 36, Tile6
    SET_POINTER PLAYFIELD + 38, Tile6

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
    jmp Reset

.DontResetGame
.DoneWithLostTimer
    rts

EnableLaser
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
    lda #BACKGROUND_COLOR
    sta COLUPF

    lda LOST_TIMER
    bne .LostTimerAlreadySet

    lda #30
    sta LOST_TIMER

.LostTimerAlreadySet
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

EyeFrame0
    .byte #%00111100
    .byte #%11111110
    .byte #%11111111
    .byte #%11111111
    .byte #%01111010
    .byte #%01111110
    .byte #%00111100
    .byte #%00011000

EyeFrameAttack
    .byte #%00111100
    .byte #%00111110
    .byte #%01111111
    .byte #%11111111
    .byte #%11111010
    .byte #%11111110
    .byte #%00111100
    .byte #%00011000

LaserFrames
    REPEAT LASER_ENABLED_RANGE
    .byte #%00000000
    REPEND
    REPEAT LASER_ENABLED_SPEED
    .byte #%01111110
    REPEND
    REPEAT LASER_ENABLED_SPEED
    .byte #%11111111
    REPEND
    REPEAT LASER_ENABLED_SPEED
    .byte #%01111110
    REPEND
    REPEAT LASER_ENABLED_SPEED
    .byte #%00111100
    REPEND
    REPEAT LASER_ENABLED_SPEED
    .byte #%00011000
    REPEND
    REPEAT LASER_ENABLED_SPEED
    .byte #%00000000
    REPEND

    BOUNDARY $00 ;; Force a page boundary crossing so "lda CharFrame0,y" takes the right amount of time
CharFrame0
    REPEAT 112
    .byte #%00000000
    REPEND
    .byte #%00111100
    .byte #%11111110
    .byte #%11111111
    .byte #%11111111
    .byte #%01111010
    .byte #%01111110
    .byte #%00111100
    .byte #%00011000
    .byte #%00111100
    .byte #%11111110
    .byte #%11111111
    .byte #%11111111
    .byte #%01111010
    .byte #%01111110
    .byte #%00111100
    .byte #%00011000
    REPEAT 112
    .byte #%00000000
    REPEND

    BOUNDARY $00 ;; Force a page boundary crossing so "lda CharFrame0,y" takes the right amount of time
CharFrame1
    REPEAT 112
    .byte #%00000000
    REPEND
    .byte #%00111100
    .byte #%00111110
    .byte #%01111111
    .byte #%11111111
    .byte #%11111010
    .byte #%11111110
    .byte #%00111100
    .byte #%00011000
    .byte #%00111100
    .byte #%00111110
    .byte #%01111111
    .byte #%11111111
    .byte #%11111010
    .byte #%11111110
    .byte #%00111100
    .byte #%00011000
    REPEAT 112
    .byte #%00000000
    REPEND

    BOUNDARY $00
Tiles
Tile1
    .byte #%10000001
    .byte #%00000001 
    .byte #%00000001
    .byte #%00000001
    .byte #%00000001
    .byte #%00000001

Tile2
    .byte #%11111111
    .byte #%10000000 
    .byte #%10001000
    .byte #%10000000
    .byte #%10000000
    .byte #%11111111 ; Tile 2

Tile3
    .byte #%11111111 ; Tile 3
    .byte #%01111111
    .byte #%00111111
    .byte #%00011111
    .byte #%00001111
    .byte #%00000111

Tile3B
    .byte #%11111111 ; Tile 3B
    .byte #%11111110
    .byte #%11111100
    .byte #%11111000
    .byte #%11110000
    .byte #%11100000

Tile4
    .byte #%11111111
    .byte #%11111111 
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000 ; Tile 4

Tile5
    .byte #%11111111
    .byte #%11111111 
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111 ; Tile 5

Tile6
    .byte #%00000000
    .byte #%00000000 
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000 ; Tile 6

TileDivideTable
.TileDivideTableY SET 0
    REPEAT TILE_HEIGHT
        REPEAT 5
        .byte .TileDivideTableY
        REPEND
.TileDivideTableY SET .TileDivideTableY + 1
    REPEND

    ORG $FE00
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
