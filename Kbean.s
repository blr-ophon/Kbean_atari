	processor 6502
	include "vcs.h"
	include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLES AND CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	seg.u Variables
	org $80
;Posiçoes x e y	
KBxpos		byte
KBypos		byte
Enemyxpos	byte
Enemyypos	byte
Missilexpos 	byte
Missileypos 	byte
;Ponteiros dos bitmaps
KBbmPtr 	word	
KBcolorPtr 	word	
EnemybmPtr	word
EnemycolorPtr	word
;Usado para controlar as animaçoes
KBAnimOffset 	byte
;Para numeros aleatorios
Random 		byte
;Scoreboard
Score 		byte
Timer 		byte
Temp 		byte
OnesOffset	word
TensOffset 	word
Scorebm 	byte
Timerbm		byte
;background colors
WallsColor	byte
AreaColor	byte
;MACROS
KB_HEIGHT = 10
ENEMY_HEIGHT = 10
DIGITS_HEIGHT = 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLE INICIALIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	seg Code
	org $f000

Reset:
	CLEAN_START

; X and Y positions
	lda #10
	sta KBypos
	lda #60
	sta KBxpos

	lda #83
	sta Enemyypos
	lda #54
	sta Enemyxpos

	lda #$05
	sta NUSIZ0

	lda #%11010100	;seed
	sta Random               

	lda #$00
	sta Score
	lda #$00
	sta Timer

; Bitmaps and colormaps Pointers
	lda #<KBbm1
	sta KBbmPtr
	lda #>KBbm1
	sta KBbmPtr+1

	lda #<KBcolor1
	sta KBcolorPtr
	lda #>KBcolor1
	sta KBcolorPtr+1

	lda #<Enemybm1
	sta EnemybmPtr
	lda #>Enemybm1
	sta EnemybmPtr+1

	lda #<Enemycolor1
	sta EnemycolorPtr
	lda #>Enemycolor1
	sta EnemycolorPtr+1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACRO to check if missile should be displayed in current scanline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	MAC DRAW_MISSILE	
		lda #%00000000
		cpx Missileypos 	;Compare X(current scanline) with ypos
		bne Skipmissiledraw	;if X>missileypos, don't display
DrawMissile:
		lda #%00000010
		inc Missileypos
		inc Missileypos
Skipmissiledraw:
		sta ENAM0
	ENDM


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VSYNC AND VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


Startframe:

	lda #2
	sta VSYNC
	sta VBLANK

	ldx #3
vsscan:
	sta WSYNC
	dex
	bne vsscan
	lda #0
	sta VSYNC

	ldx #33
vbscann:
	sta WSYNC
	dex
	bne vbscann

	;Subrotina de posiçao X
	lda KBxpos
	ldy #0
	jsr Setobjectxpos

	lda Enemyxpos
	ldy #1
	jsr Setobjectxpos

	lda Missilexpos
	ldy #2
	jsr Setobjectxpos

	jsr CalcDigitOffset
	
	sta WSYNC
	sta HMOVE

	lda #0
	sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCOREBOARD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #0
	sta PF0
	sta PF1
	sta PF2
	sta GRP0
	sta GRP1
	sta CTRLPF
	sta COLUBK

	lda #$1e
	sta COLUPF
	
	ldx #DIGITS_HEIGHT

ScoreDigitLoop:
	ldy TensOffset
	lda Digits,Y
	and #$F0	;Ignore right half of bitmap (Ex.: 20 instead of 22)
	sta Scorebm

	ldy OnesOffset
	lda Digits,Y
	and #$0F	;Ignore left half of bitmap (Ex.: 02 instead of 22)
	ora Scorebm 	;merge left and right halves to form full score digits
	sta Scorebm

	sta WSYNC
	sta PF1
	
;Same thing for TIMER
	ldy TensOffset+1
	lda Digits,Y
	and #$F0	;Ignore right half of bitmap (Ex.: 20 instead of 22)
	sta Timerbm

	ldy OnesOffset+1
	lda Digits,Y
	and #$0F	;Ignore left half of bitmap (Ex.: 02 instead of 22)
	ora Timerbm 	;merge left and right halves to form full score digits
	sta Timerbm

	jsr Sleep12cycles
	sta PF1

	;second scanline (2-line kernel)
	;ACC still has Timerbm and Y gets previous Scorebm
	;Y has to be stored in PF1 in exact time for correct horizontal position
	ldy Scorebm	;preload score in PF1 for next scanline
	sta WSYNC
	sty PF1

	;next bitmap line for the loop 
	inc TensOffset
	inc TensOffset+1
	inc OnesOffset
	inc OnesOffset+1

	jsr Sleep12cycles
	dex 		;Comes before sta PF1 to waste extra cycles
	sta PF1
	bne ScoreDigitLoop ;Loop until all bitmap lines have been rendered

	sta WSYNC

	;Extra space between scoreboard and gamefield
	lda #0
	sta PF0
	sta PF1
	sta PF2
	sta WSYNC
	sta WSYNC
	sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VISIBLE SCANLINES / 2-LINE KERNEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Gamevisible:
	lda WallsColor
	sta COLUPF
	lda AreaColor
	sta COLUBK

	lda #$01
	sta CTRLPF
	lda #$f0
	sta PF0
	lda #$fc
	sta PF1
	lda #0
	sta PF2

	ldx #84		;(192-20)/2   : 2-line kernel e 20 linhas de scoreboard
Gamelineloop:
	DRAW_MISSILE 

KBspritescan:
	txa 
	sec
	sbc KBypos
	cmp #KB_HEIGHT
	bcc DrawKB
	lda #0
DrawKB:
	;controle da animaçao/sprite a ser desenhada
	clc
	adc KBAnimOffset

	tay
	lda (KBbmPtr),Y	;O parentese implica 16 bits, olha KBbmPtr e KBbmPtr+1
	sta WSYNC
	sta GRP0
	lda (KBcolorPtr),Y
	sta COLUP0

Enemyspritescan:
	txa 
	sec
	sbc Enemyypos
	cmp #ENEMY_HEIGHT
	bcc DrawEnemy
	lda #0
DrawEnemy:
	tay

	lda #3
	sta NUSIZ1

	lda (EnemybmPtr),Y
	sta WSYNC 		;Como tem 2 WSYNCS, vira 2-line Kernel
	sta GRP1
	lda (EnemycolorPtr),Y
	sta COLUP1

	dex
	bne Gamelineloop

	lda #0
	sta KBAnimOffset 
	
	sta WSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OVERSCAN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda #2
	sta VBLANK
	ldx #30
ovscan:
	sta WSYNC
	dex
	bne ovscan
	lda #0
	sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT CHECK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Analisa os valores de SWCHA e compara caso a caso
;Bit works like logical AND. Sets the same flags without
;modifying ACC

CheckP0Up:
	lda #%00010000          
	bit SWCHA
	bne CheckP0Down         
	;Check top boundaries
	lda KBypos
	cmp #70
	bpl CheckP0Down

	inc KBypos
	lda #0
	sta KBAnimOffset       

CheckP0Down:
	lda #%00100000         
	bit SWCHA
	bne CheckP0Left        
	;Check bottom boundaries
	lda KBypos
	cmp #0
	bmi CheckP0Left

	dec KBypos
	lda #0
	sta KBAnimOffset     

CheckP0Left:
	lda #%01000000        
	bit SWCHA
	bne CheckP0Right      
	;Check left boundaries
	lda KBxpos
	cmp #33
	bmi CheckP0Right

	dec KBxpos
	lda #0
	sta KBAnimOffset     

CheckP0Right:
	lda #%10000000         
	bit SWCHA
	bne CheckButtonPressed
	;Check right boundaries
	lda KBxpos
	cmp #103
	bpl CheckButtonPressed

	inc KBxpos
	lda #0
	sta KBAnimOffset     

CheckButtonPressed:
	lda #%10000000
	bit INPT4
	bne Buttonunpressed
	lda #KB_HEIGHT
	sta KBAnimOffset
	jsr GenerateKBSound
	lda KBxpos
	clc
	adc #2		;centers missile in the middle of KB
	sta Missilexpos
	lda KBypos 	;misille starts at the top of KB
	clc
	adc #8
	sta Missileypos
	jmp EndInputCheck

Buttonunpressed:
	lda #0
	sta AUDV0
	sta AUDC0
	sta AUDF0

EndInputCheck:             


UpdateEnemyy:
	;Se a posição for 0 (parte de baixo da tela), volta pra o topo(y=96)
	lda Enemyypos
	clc
	cmp #0	
	bmi ResetEnemyy 
	dec Enemyypos
	jmp Endbyupdate
ResetEnemyy:
	jsr GetRandomEnemyPos

	;Convert timer value to BCD 
	sed
	lda Timer
	clc 
	adc #1
	sta Timer
	cld

Endbyupdate:

CheckCollisionP0P1:
	lda #$80
	bit CXPPMM
	bne CollisionP0P1
	jsr SetWallsArea
	jmp CheckCollisionM0P1
CollisionP0P1:
	jsr GetRandomEnemyPos
	jsr GameOver

CheckCollisionM0P1:
	lda #$80
	bit CXM0P
	bne CollisionM0P1
	jmp EndCollisionCheck
CollisionM0P1:
	jsr GetRandomEnemyPos
	;Convert score value to BCD 
	sed
	lda Score
	clc
	adc #1
	sta Score
	cld
	
	

EndCollisionCheck:
	sta CXCLR



	jmp Startframe

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Set Walls and Area colors to default
SetWallsArea subroutine
	lda #$ee
	sta WallsColor
	lda #$08
	sta AreaColor
	rts

GenerateKBSound subroutine
	;Sets volume, tone and frequency in this order 
	lda #1
	sta AUDV0
	lda #8
	sta AUDC0
	lda #15
	sta AUDF0
	rts


;Sets object in his x position on the screen
Setobjectxpos subroutine
	;set y=0 for Player 0, y=1 for Player 1 and y=2 for missile
	sta WSYNC
	sec
Divloop:
	sbc #15
	bcs Divloop
	eor #7
	asl
	asl
	asl
	asl

	sta HMP0,Y		;y=0:HMP0, y=1:HMP1
	sta RESP0,Y		;y=0:RESP0, y=1:RESP1
	rts

GameOver subroutine
	lda #$30
	sta WallsColor
	sta AreaColor
	lda #0
	sta Score
	rts

;; Subroutine to generate a Linear-Feedback Shift Register random number
;; Generate a LFSR random number for the X-position 
;; Divide the random value by 4 to limit the size of the result to match river.

GetRandomEnemyPos subroutine
	lda Random
	asl
	eor Random
	asl
	eor Random
	asl
	asl
	eor Random
	asl
	rol Random            

	
	lsr 			 ;Duble right shift to divide by 4
	lsr                    
	sta Enemyxpos          
	lda #30
	adc Enemyxpos           ; adds 30 + EnemyXPos to compensate for left PF
	sta Enemyxpos           

	lda #96
	sta Enemyypos           ; set the y-position to the top of the screen

	rts


;First digit offset = Least significant nibble of Score x5(DIGITS_HEIGHT)
;Multiply ACC by 5 using: A = 2*A + A
;Tens digit offset = (Score/16)x5(DIGITS_HEIGHT) = Score/4 + Score/16
;Score offset is returned on TensOffset and OnesOffset
;Timer offset is returned on TensOffset+1 and OnesOffset+1
CalcDigitOffset subroutine 		;Return scoreboard digits offset

	ldx #1
PrepareScoreLoop:
	lda Score,X		;x=0:Score, x=1:Timer
	and #$0F		;remove MSB's
	;Ones digit offset
	sta Temp
	asl 
	asl
	adc Temp
	sta OnesOffset,X

	;Tens digit offset
	lda Score,X
	and #$F0
	lsr
	lsr
	sta Temp
	lsr
	lsr
	adc Temp
	sta TensOffset,X

	dex
	bpl PrepareScoreLoop
	rts


;Simply wastes 12 cycles for x position offsets
Sleep12cycles subroutine
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BITMAPS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Digits:
	.byte %01110111          ; ### ###
	.byte %01010101          ; # # # #
	.byte %01010101          ; # # # #
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###

	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #

	.byte %01110111          ; ### ###
	.byte %00010001          ;   #   #
	.byte %01110111          ; ### ###
	.byte %01000100          ; #   #
	.byte %01110111          ; ### ###

	.byte %01110111          ; ### ###
	.byte %00010001          ;   #   #
	.byte %00110011          ;  ##  ##
	.byte %00010001          ;   #   #
	.byte %01110111          ; ### ###

	.byte %01010101          ; # # # #
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###
	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #

	.byte %01110111          ; ### ###
	.byte %01000100          ; #   #
	.byte %01110111          ; ### ###
	.byte %00010001          ;   #   #
	.byte %01110111          ; ### ###

	.byte %01110111          ; ### ###
	.byte %01000100          ; #   #
	.byte %01110111          ; ### ###
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###

	.byte %01110111          ; ### ###
	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #

	.byte %01110111          ; ### ###
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###

	.byte %01110111          ; ### ###
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###
	.byte %00010001          ;   #   #
	.byte %01110111          ; ### ###

	.byte %00100010          ;  #   #
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###
	.byte %01010101          ; # # # #
	.byte %01010101          ; # # # #

	.byte %01110111          ; ### ###
	.byte %01010101          ; # # # #
	.byte %01100110          ; ##  ##
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###

	.byte %01110111          ; ### ###
	.byte %01000100          ; #   #
	.byte %01000100          ; #   #
	.byte %01000100          ; #   #
	.byte %01110111          ; ### ###

	.byte %01100110          ; ##  ##
	.byte %01010101          ; # # # #
	.byte %01010101          ; # # # #
	.byte %01010101          ; # # # #
	.byte %01100110          ; ##  ##

	.byte %01110111          ; ### ###
	.byte %01000100          ; #   #
	.byte %01110111          ; ### ###
	.byte %01000100          ; #   #
	.byte %01110111          ; ### ###

	.byte %01110111          ; ### ###
	.byte %01000100          ; #   #
	.byte %01100110          ; ##  ##
	.byte %01000100          ; #   #
	.byte %01000100          ; #   #
KBbm1:
	.byte #0
	.byte #%00100100;$42
	.byte #%00100100;$42
	.byte #%00011000;$42
	.byte #%00111101;$42
	.byte #%01111110;$42
	.byte #%10111100;$42
	.byte #%10111100;$00
	.byte #%00111100;$F2
	.byte #%00011000;$F2

KBbm2:
	;Labels nao aparecem na memoria, logo e byte seguinte esta a 9 endereços do
	;inicio da sprite anterior
	.byte #0
	.byte #%00100100;$42
	.byte #%00100100;$42
	.byte #%00011000;$42
	.byte #%00111101;$42
	.byte #%11111110;$42
	.byte #%10111100;$42
	.byte #%00111100;$00
	.byte #%00111100;$F2
	.byte #%00011000;$F2

Enemybm1:
	.byte #0
	.byte #%00100100;$00
	.byte #%00100100;$00
	.byte #%00011000;$F2
	.byte #%10111101;$02
	.byte #%10111101;$02
	.byte #%01111110;$02
	.byte #%00111100;$F2
	.byte #%00111100;$00
	.byte #%00011000;$00

KBcolor1:
	.byte #0
	.byte #$42;
	.byte #$42;
	.byte #$42;
	.byte #$42;
	.byte #$42;
	.byte #$42;
	.byte #$00;
	.byte #$F2;
	.byte #$F2;

KBcolor2:
	.byte #0
	.byte #$42;
	.byte #$42;
	.byte #$42;
	.byte #$42;
	.byte #$42;
	.byte #$00;
	.byte #$00;
	.byte #$F2;
	.byte #$F2;

Enemycolor1:
	.byte #0
	.byte #$00;
	.byte #$00;
	.byte #$F2;
	.byte #$02;
	.byte #$02;
	.byte #$02;
	.byte #$F2;
	.byte #$00;
	.byte #$00;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	org $fffc
	word Reset
	word Reset

