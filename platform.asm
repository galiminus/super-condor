    processor 6502
    include "vcs.h"
    include "macro.h"
    include "xmacro.h"

PLAYFIELD_WIDTH             = 4
PLAYFIELD_HEIGHT            = 5
TILE_HEIGHT                 = 6
PLAYFIELD_COLOR             = $a0
LAVA_COLOR                  = $38
BACKGROUND_COLOR            = $04
PLAYER_ANIM_SPEED           = 128

    SEG.U vars
    ORG $80

HAS_SPRITE             ds 0
PLAYER_X               ds 1
PLAYER_Y               ds 1
PLAYER_Y_ADDR          ds 1
PLAYER_ANIM_CTR        ds 1
PLAYER_CHAR_FRAME      ds 2
RANDOM                 ds 1
PLAYFIELD              ds PLAYFIELD_WIDTH * PLAYFIELD_HEIGHT * 2

    SEG
    ORG $F000

Reset
    CLEAN_START

    lda #PLAYER_ANIM_SPEED
    sta PLAYER_ANIM_CTR

    SET_POINTER PLAYER_CHAR_FRAME, CharFrame0

NextFrame
    VERTICAL_SYNC

    ; SCREEN (remove me maybe)
    jsr GameKernel

    jmp NextFrame

GameKernel
    ; VBLANK
    TIMER_SETUP 37

    jsr UpdateRandom
    jsr GenerateGameKernelBackground
    jsr GenerateGameKernelBasePlayfield
    jsr UpdateGameKernelPlayerPosition
    jsr UpdateGameKernelPlayerAnim

    lda #%11111111
    sta PF0
    sta PF1
    sta PF2

    lda PLAYER_Y
    sta PLAYER_Y_ADDR

    lda PLAYER_X
    ldx #0
    jsr FineAdjustSprite

    TIMER_WAIT

    TIMER_SETUP 192

    sta WSYNC
    sta HMOVE

    lda #0
    sta VBLANK

    ldx #TILE_HEIGHT * 4
.UpperScreenLine
        sta WSYNC
        dex
        bne .UpperScreenLine

TILE_Y SET 0
    REPEAT PLAYFIELD_HEIGHT
    SUBROUTINE

    IF TILE_Y = PLAYFIELD_HEIGHT - 1
        lda #%00010000
        sta PF0
    ENDIF ; Build entrance and exit

    ldx #TILE_HEIGHT * 4
.GameKernelLine
        sta WSYNC

        ldy TileDivideTable,x

        lda (PLAYFIELD + TILE_Y * PLAYFIELD_WIDTH * 2),y
        sta PF1
        
        lda (PLAYFIELD + TILE_Y * PLAYFIELD_WIDTH * 2 + 2),y
        sta PF2

        ldy PLAYER_Y_ADDR
        lda (PLAYER_CHAR_FRAME),y
        sta GRP0
        inc PLAYER_Y_ADDR

        ldy TileDivideTable,x

        lda (PLAYFIELD + TILE_Y * PLAYFIELD_WIDTH * 2 + 4),y
        sta PF2

        lda (PLAYFIELD + TILE_Y * PLAYFIELD_WIDTH * 2 + 6),y
        sta PF1

        dex
        bne .GameKernelLine

TILE_Y SET TILE_Y + 1
    REPEND
    sta WSYNC

    jsr GenerateGameKernelFloor
    jsr GenerateGameKernelLava

    TIMER_WAIT

    ; OVERSCAN
    TIMER_SETUP 30
	lda #2
    sta VBLANK

    jsr ClearGameKernelPlayfield

    TIMER_WAIT
    rts

UpdateGameKernelPlayerAnim
    dec PLAYER_ANIM_CTR
    lda PLAYER_ANIM_CTR
    cmp #128
    bne .NotFrame0

    SET_POINTER PLAYER_CHAR_FRAME, CharFrame0
    jmp .NotFrame1

.NotFrame0
    cmp #0
    bne .NotFrame1
    SET_POINTER PLAYER_CHAR_FRAME, CharFrame1

.NotFrame1
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

    REPEAT 3
    jsr UpdateRandom
    lda #%10101010
    eor RANDOM
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC
    REPEND
    rts

GenerateGameKernelBackground
    lda #PLAYFIELD_COLOR
    sta COLUPF ; set playfield color
    rts

GenerateGameKernelBasePlayfield
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
    SET_POINTER PLAYFIELD + 2, Tile5
    SET_POINTER PLAYFIELD + 4, Tile6
    SET_POINTER PLAYFIELD + 6, Tile5

    SET_POINTER PLAYFIELD + 8, Tile5
    SET_POINTER PLAYFIELD + 10, Tile6
    SET_POINTER PLAYFIELD + 12, Tile5
    SET_POINTER PLAYFIELD + 14, Tile6

    SET_POINTER PLAYFIELD + 16, Tile6
    SET_POINTER PLAYFIELD + 18, Tile5
    SET_POINTER PLAYFIELD + 20, Tile6
    SET_POINTER PLAYFIELD + 22, Tile5

    SET_POINTER PLAYFIELD + 24, Tile5
    SET_POINTER PLAYFIELD + 26, Tile6
    SET_POINTER PLAYFIELD + 28, Tile5
    SET_POINTER PLAYFIELD + 30, Tile6

    SET_POINTER PLAYFIELD + 32, Tile3
    SET_POINTER PLAYFIELD + 34, Tile5
    SET_POINTER PLAYFIELD + 36, Tile6
    SET_POINTER PLAYFIELD + 38, Tile5

    rts

ClearGameKernelPlayfield
    lda #%00000000
    sta PF0
    sta PF1
    sta PF2
    rts

UpdateGameKernelPlayerPosition
	lda #%10000000
	bit SWCHA
	bne DoneMoveRight

    inc PLAYER_X
DoneMoveRight

	lda #%01000000
	bit SWCHA
	bne DoneMoveLeft

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

    BOUNDARY $ff ;; Force a page boundary crossing so "lda CharFrame0,y" takes the right amount of time
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

    BOUNDARY $ff ;; Force a page boundary crossing so "lda CharFrame0,y" takes the right amount of time
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
    .byte #%10000000
    .byte #%10001000
    .byte #%10000000
    .byte #%10000000
    .byte #%11111111 ; Tile 2

Tile3
    .byte #%00000111
    .byte #%00001111
    .byte #%00011111
    .byte #%00111111
    .byte #%01111111
    .byte #%11111111 ; Tile 3

Tile4
    .byte #%11111111
    .byte #%11111111 
    .byte #%00000000
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
        REPEAT 4
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
