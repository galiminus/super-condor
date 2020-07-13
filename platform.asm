    processor 6502
    include "vcs.h"
    include "macro.h"
    include "xmacro.h"

PLAYFIELD_WIDTH             = 4
PLAYFIELD_HEIGHT            = 6
PLAYFIELD_COLOR             = $a0
BACKGROUND_COLOR            = $04

    SEG.U vars
    ORG $80

RANDOM                 ds 1
PLAYFIELD              ds PLAYFIELD_WIDTH * PLAYFIELD_HEIGHT * 2
PF_TILES               ds 8

    SEG
    ORG $F000

Reset
    CLEAN_START

NextFrame
    VERTICAL_SYNC

    ; SCREEN (remove me maybe)
    jsr GameKernel

    jmp NextFrame

GameKernel
    ; VBLANK
    TIMER_SETUP 37

    jsr GenerateGameKernelBackground
    jsr GenerateGameKernelBasePlayfield

    TIMER_WAIT

    sta WSYNC
    sta HMOVE

    lda #0
    sta VBLANK

TY SET 0
    REPEAT PLAYFIELD_HEIGHT

    lda #0
    sta PF1
    sta PF2

TX SET 0
    REPEAT PLAYFIELD_WIDTH

    lda PLAYFIELD + TY * PLAYFIELD_WIDTH * 2 + TX * 2
    sta PF_TILES + TX * 2

    lda PLAYFIELD + TY * PLAYFIELD_WIDTH * 2 + TX * 2 + 1
    sta PF_TILES + TX * 2 + 1
TX SET TX + 1
    REPEND

    ldx #0
    ldy #0
    sta WSYNC

    SUBROUTINE

.GameKernelLine
        txa                     ; 2
        lsr                     ; 2
        lsr                     ; 2
        and #%00000111          ; 2
        tay                     ; 2
    
        lda (PF_TILES + 0),y
        sta PF1
        
        lda (PF_TILES + 2),y
        sta PF2

        SLEEP 3

        lda (PF_TILES + 6),y
        sta PF1

        lda (PF_TILES + 4),y
        sta PF2
        
        inx
        sta WSYNC

        cpx #31
        bne .GameKernelLine

TY SET TY + 1
    REPEND

    lda #0
    sta PF1
    sta PF2

    ; OVERSCAN
    TIMER_SETUP 30
	lda #2
    sta VBLANK

    jsr ClearGameKernelPlayfield

    TIMER_WAIT

GenerateGameKernelBackground
    lda #PLAYFIELD_COLOR
    sta COLUPF ; set playfield color
    rts

GenerateGameKernelBasePlayfield
    lda #BACKGROUND_COLOR
    sta COLUBK    

    lda #%00000001
    sta CTRLPF ; enable mirroring, that's useful for PF0

    lda #%11110000
    sta PF0 ; build walls on the left and right
    lda #%00000000
    sta PF1
    sta PF2

    SET_POINTER PLAYFIELD + 0, Tile5
    SET_POINTER PLAYFIELD + 2, Tile5
    SET_POINTER PLAYFIELD + 4, Tile5
    SET_POINTER PLAYFIELD + 6, Tile5

    SET_POINTER PLAYFIELD + 8, Tile5
    SET_POINTER PLAYFIELD + 10, Tile5
    SET_POINTER PLAYFIELD + 12, Tile5
    SET_POINTER PLAYFIELD + 14, Tile5

    SET_POINTER PLAYFIELD + 16, Tile1
    SET_POINTER PLAYFIELD + 18, Tile1
    SET_POINTER PLAYFIELD + 20, Tile1
    SET_POINTER PLAYFIELD + 22, Tile1

    SET_POINTER PLAYFIELD + 24, Tile1
    SET_POINTER PLAYFIELD + 26, Tile1
    SET_POINTER PLAYFIELD + 28, Tile1
    SET_POINTER PLAYFIELD + 30, Tile1

    SET_POINTER PLAYFIELD + 32, Tile4
    SET_POINTER PLAYFIELD + 34, Tile4
    SET_POINTER PLAYFIELD + 36, Tile3
    SET_POINTER PLAYFIELD + 38, Tile3

    SET_POINTER PLAYFIELD + 40, Tile3
    SET_POINTER PLAYFIELD + 42, Tile4
    SET_POINTER PLAYFIELD + 44, Tile4
    SET_POINTER PLAYFIELD + 46, Tile4 ; Bottom Part

    rts

ClearGameKernelPlayfield
    lda #%00000000
    sta PF0
    sta PF1
    sta PF2
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

FineAdjust
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

Tiles
Tile1
    .byte #%10000001
    .byte #%00000001 
    .byte #%00000001
    .byte #%00000001
    .byte #%00000001
    .byte #%00000001
    .byte #%00000001
    .byte #%00000001 ; Tile 1

Tile2
    .byte #%11111111
    .byte #%10000000 
    .byte #%10000000
    .byte #%10001000
    .byte #%10000000
    .byte #%10000000
    .byte #%10000000
    .byte #%11111111 ; Tile 2

Tile3
    .byte #%00000001
    .byte #%00000011 
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
    .byte #%00000000
    .byte #%00000000 ; Tile 4

Tile5
    .byte #%11111111
    .byte #%11111111 
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111 ; Tile 5

LineToIndex
LINE_Y   SET     0
    REPEAT 4
    REPEAT 32
        .byte #LINE_Y
    REPEND
LINE_Y   SET     LINE_Y + 1
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
