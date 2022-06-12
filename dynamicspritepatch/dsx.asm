;code to provide sprites with dynamic video memory updating
;patched with xkas

header
lorom

!SLOTSUSED = $06FE	;how many slots have been used

;jump from original code's NMI routine endind

;CODE_0082BC: REP #$30
;PLB
;PLY
;PLX 
;PLA 
;PLP
;CODE_0082C3: TI
 
;fix up old

org $82BC
	REP #$30
	PLB
	PLY

;FastROM registration data
org $7FD5
	db $30

org $816A
	JML CodeStart

;new starting location

org $988000		;!!!POINT TO SOME FREE SPACE!!!

;RATS
db "STAR"
dw CodeEnd-CodeStart
dw CodeEnd-CodeStart^#$FFFF

CodeStart:
	SEI
	PHP
	REP #$30
	PHA
	PHX
	PHY
	PHB
	SEP #$30	;8bit AXY

	LDA #$01	;FastROM on
	STA $420D

	LDA $0100	;check game mode to see if in game
	CMP #$14	;must be in this mode
	BNE Return
	LDA !SLOTSUSED	;and only if there's actual stuff to transfer
	BEQ Return

Proceed:
	REP #$20	;16bit A
	LDY #$80	;word writes
	STY $2115

	LDA #$7C00	;dest VRAM
	STA $2116

	LDY #$01	;2 regs write once alternate
	STY $4300
	LDY #$18	;2118
	STY $4301
	LDA #$0B44	;7F:0B44
	STA $4302
	LDY #$7F	;!7F!
	STY $4304
	LDA #$0800	;2kb
	STA $4305

	LDY #$01
	STY $420B	;transfer	

Return:
	STZ !SLOTSUSED	;reset slots used

	SEP #$30	;8bit AXY	
	LDA #$80	;set bank to zero in FastROM area
	PHA
	PLB	
	JML $808176	;jump to code just after pushing, into FastROM area

CodeEnd: