;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Yoshi's Island Wild Ptooie Piranha Plant (v3.0)
; Programmed by SMWEdit, conversion to asar and sa-1 compatibility by spooonsss
;
; Uses first extra bit: YES
; It will be upside-down if the first extra bit is set
; (Yoshi's Island did not have upside-down Ptooies, but I decided
; to leave this feature in since this is just a major blowhard edit)
;
; You will need to patch SMKDan's dsx.asm to your ROM, unless your rom is sa-1
; this sprite, like all other dynamic sprites, uses the last 4 rows of sp4
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Variables:
		!SPRITE_NUM = $82		; custom sprite of needlenose bomb  https://www.smwcentral.net/?p=section&a=details&id=18520
		!SHOOTSND = $3A			; sound after needlenose fired  (in $1DFC)
		!KNOCKOUTSND = $37		; sound when you hit it with a shell (in $1DFC)
		!FLASHPALETTE = $0F		; palette for flashing head

;; Sprite Tables:
		!ACTSTATUS = !C2			; 0 = TRACKING, 1 = pausing before firing, 2 = aiming/firing, 3 = pausing while mouth open, 4 = pausing while mouth just closed, 5 = hit with shell, 6 = pause after hit, 7 = shrinking
		!OFFSET = !1504
		!SHOTSFIRED = !1528
		!HITCOUNT = !1534
		!FRAME = !1570
		!FBANK = !1602
		!TIMER = !163E
		!HEADPALETTE = !1626
		!HITDATA = !151C
		!EXTRA_BITS = !7FAB10
		!EXTRA_PROP1 = !7FAB28

;; Scratch RAM:
		!TMP1 = $00
		!TMP2 = $01
		!FIRINGADDR = $0A
		!HITPALADDR = $0C
		!EXTRA_BIT = $0E

;; Routines:
		!EXECUTEPTR = $0086DF
		!PHYSICS = $01802A
		!SPRSPRINTERACT = $018032
		!MARIOSPRINTERACT = $01A7DC
		!FINISHOAMWRITE = $01B7B3
		!GETSPRITECLIPPINGA = $03B69F
		!GETSPRITECLIPPINGB = $03B6E5
		!CHECKFORCONTACT = $03B72B
		!SPINJUMPSTARS = $07FC3B

prot gfx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INIT and MAIN JSL targets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		!FLASHPROP = !FLASHPALETTE*2&%00001110

CONFIGSADDR:	dw SHOTSADDR&$FFFF,HITSADDR&$FFFF

SHOTSADDR:	dw SHOTS1&$FFFF,SHOTS2&$FFFF
HITSADDR:	dw HITS1&$FFFF,HITS2&$FFFF

SHOTS1:		db $02 : db $05,$05,$02	; #shots-1, shots
HITS1:		db $02 : db $0D,$0A,$0C	; #hits-1, palettes

SHOTS2:		db $00 : db $05		; #shots-1, shots
HITS2:		db $02 : db $0D,$0A,$0C	; #hits-1, palettes

FLIP_YPOS_LO:	db $00,$FE
FLIP_YPOS_HI:	db $00,$FF

		PRINT "INIT ",pc
		PHB
		PHK
		PLB
		LDA !EXTRA_PROP1,x	; \
		ASL A			;  | get
		TAY			;  | config
		LDA SHOTSADDR,y		;  | addresses
		STA !FIRINGADDR		;  |
		LDA SHOTSADDR+1,y	;  |
		STA !FIRINGADDR+1	;  |
		LDA HITSADDR,y		;  |
		STA !HITPALADDR		;  |
		LDA HITSADDR+1,y	;  |
		STA !HITPALADDR+1	; /
		LDY #$01		; \ get first
		LDA (!FIRINGADDR)	; / offset
		PHA			; \
		%SubHorzPos()	;  | flip offset
		PLA			;  | if necessary
		CPY #$00		;  |
		BEQ SETINITOFFSET	;  |
		EOR #$FF		;  |
		CLC			;  |
		ADC #$3E		; /
SETINITOFFSET:	STA !OFFSET,x		; set initial rotation
		TAY			; \
		LDA FRAMES,y		;  | set GFX information
		STA !FRAME,x		;  | based on starting
		LDA FBANKS,y		;  | offset
		STA !FBANK,x		; /
		LDA #$02		; \ set status to
		STA !ACTSTATUS,x		; / taking aim
		LDA !EXTRA_BITS,x	; \
		LSR A			;  | set initial
		LSR A			;  | Y position
		AND #%00000001		;  | depending
		TAY			;  | on extra
		LDA FLIP_YPOS_LO,y	;  | bit
		CLC			;  |
		ADC !D8,x		;  |
		STA !D8,x		;  |
		LDA FLIP_YPOS_HI,y	;  |
		ADC !14D4,x		;  |
		STA !14D4,x		; /
		LDY #$01		; \ first
		LDA (!HITPALADDR),y	; / color
		ASL A			; \ convert to
		AND #%00001110		; / properties
		STA !HEADPALETTE,x	; set head palette
		PLB
		RTL

		PRINT "MAIN ",pc
		PHB
		PHK
		PLB
		JSR SPRITE_ROUTINE
		PLB
		RTL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SPRITE_ROUTINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

XCOORDS:		db $02,$03,$05,$06
		db $07,$08,$0A,$0B
		db $0C,$0D,$0D,$0E
		db $0E,$0F,$0F,$0F
		db $0F,$0F,$0F,$0F
		db $0E,$0E,$0E,$0E
		db $0D,$0D,$0D,$0D
		db $FF,$FF,$FF
		db $FF,$FF,$FF,$F3
		db $F3,$F3,$F3,$F2
		db $F2,$F2,$F2,$F1
		db $F1,$F1,$F1,$F1
		db $F1,$F1,$F2,$F2
		db $F3,$F3,$F4,$F5
		db $F6,$F8,$F9,$FA
		db $FB,$FD,$FE

YCOORDS:		db $EC,$EC,$EC,$EC
		db $ED,$ED,$EE,$EE
		db $EF,$EF,$F0,$F1
		db $F2,$F2,$F3,$F4
		db $F4,$F5,$F6,$F7
		db $F8,$F9,$FA,$FB
		db $FC,$FD,$FE,$FF
		db $FF,$FF,$FF
		db $FF,$FF,$FF,$FF
		db $FE,$FD,$FC,$FB
		db $FA,$F9,$F8,$F7
		db $F6,$F5,$F4,$F4
		db $F3,$F2,$F2,$F1
		db $F0,$EF,$EF,$EE
		db $EE,$ED,$ED,$EC
		db $EC,$EC,$EC

FRAMES:		db $33,$32,$31,$30
		db $23,$22,$21,$20
		db $13,$12,$11,$10
		db $03,$02,$01,$00
		db $60,$61,$62,$63
		db $70,$71,$72,$73
		db $80,$81,$82,$83
		db $FF,$FF,$FF
		db $FF,$FF,$FF,$83
		db $82,$81,$80,$73
		db $72,$71,$70,$63
		db $62,$61,$60,$00
		db $01,$02,$03,$10
		db $11,$12,$13,$20
		db $21,$22,$23,$30
		db $31,$32,$33

FBANKS:		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $01,$01,$01,$01
		db $01,$01,$01,$01
		db $01,$01,$01,$01
		db $FF,$FF,$FF
		db $FF,$FF,$FF,$01
		db $01,$01,$01,$01
		db $01,$01,$01,$01
		db $01,$01,$01,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00

FRAMES2:		db $73,$72,$71,$70
		db $63,$62,$61,$60
		db $53,$52,$51,$50
		db $43,$42,$41,$40
		db $90,$91,$92,$93
		db $A0,$A1,$A2,$A3
		db $B0,$B1,$B2,$B3
		db $FF,$FF,$FF
		db $FF,$FF,$FF,$B3
		db $B2,$B1,$B0,$A3
		db $A2,$A1,$A0,$93
		db $92,$91,$90,$40
		db $41,$42,$43,$50
		db $51,$52,$53,$60
		db $61,$62,$63,$70
		db $71,$72,$73

FRAMES3:		db $B3,$B2,$B1,$B0
		db $A3,$A2,$A1,$A0
		db $93,$92,$91,$90
		db $83,$82,$81,$80
		db $C0,$C1,$C2,$C3
		db $D0,$D1,$D2,$D3
		db $E0,$E1,$E2,$E3
		db $FF,$FF,$FF
		db $FF,$FF,$FF,$E3
		db $E2,$E1,$E0,$D3
		db $D2,$D1,$D0,$C3
		db $C2,$C1,$C0,$80
		db $81,$82,$83,$90
		db $91,$92,$93,$A0
		db $A1,$A2,$A3,$B0
		db $B1,$B2,$B3

FRAMES4:		db $F3,$F2,$F1,$F0
		db $E3,$E2,$E1,$E0
		db $D3,$D2,$D1,$D0
		db $C3,$C2,$C1,$C0
		db $FF,$FF,$FF,$FF
		db $FF,$FF,$FF,$FF
		db $FF,$FF,$FF,$FF
		db $FF,$FF,$FF
		db $FF,$FF,$FF,$FF
		db $FF,$FF,$FF,$FF
		db $FF,$FF,$FF,$FF
		db $FF,$FF,$FF,$C0
		db $C1,$C2,$C3,$D0
		db $D1,$D2,$D3,$E0
		db $E1,$E2,$E3,$F0
		db $F1,$F2,$F3

XDIRECTION:	db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00
		db $01,$01,$01,$01
		db $01,$01,$01,$01
		db $01,$01,$01,$01
		db $01,$01,$01,$01
		db $01,$01,$01,$01
		db $01,$01,$01,$01
		db $01,$01,$01,$01
		db $01,$01,$01

ROTATEDEC:	db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00
		db $00,$00,$06,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$01

ROTATEINC:	db $01,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $06,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00

XSPEEDS:		db $00,$08,$11,$19
		db $21,$28,$2F,$36
		db $3B,$41,$45,$49
		db $4C,$4E,$50,$50
		db $50,$4E,$4C,$49
		db $45,$41,$3B,$36
		db $2F,$28,$21,$19
		db $11,$08,$00
		db $00,$F8,$EF,$E7
		db $DF,$D8,$D1,$CA
		db $C5,$BF,$BB,$B7
		db $B4,$B2,$B0,$B0
		db $B0,$B2,$B4,$B7
		db $BB,$BF,$C5,$CA
		db $D1,$D8,$DF,$E7
		db $EF,$F8,$00

YSPEEDS:		db $B0,$B0,$B2,$B4
		db $B7,$BB,$BF,$C5
		db $CA,$D1,$D8,$DF
		db $E7,$EF,$F8,$00
		db $08,$11,$19,$21
		db $28,$2F,$36,$3B
		db $41,$45,$49,$4C
		db $4E,$50,$50
		db $50,$50,$4E,$4C
		db $49,$45,$41,$3B
		db $36,$2F,$28,$21
		db $19,$11,$08,$00
		db $F8,$EF,$E7,$DF
		db $D8,$D1,$CA,$C5
		db $BF,$BB,$B7,$B4
		db $B2,$B0,$B0

SHRINKFRAMES:	db $23,$23,$23,$23
		db $22,$22,$22,$22
		db $21,$21,$21,$21
		db $20,$20,$20,$20
		db $13,$13,$13,$13
		db $12,$12,$12,$12
		db $11,$11,$11,$11
		db $10,$10,$10,$10
		db $03,$03,$03,$03
		db $02,$02,$02,$02
		db $01,$01,$01,$01
		db $00,$00,$00,$00

STUNMOUTHSTATE:	db $00,$00,$00,$80,$00,$00,$00

HITOFFSETS:	db $08,$35

MARIOYOSHI:	db $10,$20,$20

FRAMESLIST:	dw FRAMES&$FFFF,FRAMES2&$FFFF,FRAMES3&$FFFF,FRAMES2&$FFFF

RETURN1:		RTS

SPRITE_ROUTINE:	LDA !EXTRA_BITS,x	; \
		AND #%00000100		;  | store extra bit to
		LSR A			;  | scratch RAM so it's
		LSR A			;  | easily accessible
		STA !EXTRA_BIT		; /
		JSR SUB_GFX		; GFX
		LDA !14C8,x		; \  return if
		CMP #$08		;  | sprite
		BNE RETURN1		; /  status != 8
		LDA $9D			; \ return if
		BNE RETURN1		; / sprites locked
		LDA #$00
		%SubOffScreen()	; only process sprite while on screen

		LDA !EXTRA_BIT		; get extra bit
		PHA			; push extra bit, some INTERACTION routines use the same scratch RAM address
		BNE NOFALL		; skip "physics" if it's set
		JSL !PHYSICS|!bank		; update position based on speed values
		JSL !SPRSPRINTERACT|!bank	; interact with other sprites
NOFALL:		LDA !EXTRA_PROP1,x	; \
		ASL A			;  | get
		TAY			;  | hit/palette
		LDA HITSADDR,y		;  | configuration
		STA !HITPALADDR		;  | address
		LDA HITSADDR+1,y	;  |
		STA !HITPALADDR+1	; /
		LDA (!HITPALADDR)	; get #hits-1
		CMP !HITCOUNT,x		; \ all hits taken?
		BCC NOINTERACT		; / then don't interact
		JSR HEADINTERACT	; head interactions
		JSL !MARIOSPRINTERACT|!bank	; check for mario/sprite contact
		JSR SHELLINTERACT	; knock out if hit with shell
NOINTERACT:	LDA !EXTRA_PROP1,x	; \
		ASL A			;  | get
		TAY			;  | config
		LDA SHOTSADDR,y		;  | addresses
		STA !FIRINGADDR		;  |
		LDA SHOTSADDR+1,y	;  |
		STA !FIRINGADDR+1	;  |
		LDA HITSADDR,y		;  |
		STA !HITPALADDR		;  |
		LDA HITSADDR+1,y	;  |
		STA !HITPALADDR+1	; /
		PLA			; \ pull
		STA !EXTRA_BIT		; / extra bit
		LDA !ACTSTATUS,x		; get status
		JSL !EXECUTEPTR|!bank		; jump to code for current status

		dw TRACKING&$FFFF
		dw PREFIREPAUSE&$FFFF
		dw TAKEAIM&$FFFF
		dw FIREPAUSE1&$FFFF
		dw FIREPAUSE2&$FFFF
		dw HIT&$FFFF
		dw HITPAUSE&$FFFF
		dw SHRINK&$FFFF

;;;;;;;;;;;;;;;;

TRACKING:	LDA $14			; \
		AND #%00000001		;  | only ROTATE every other frame
		BNE NOROTATE		; /
		LDY $187A|!addr		; get riding Yoshi status
		LDA $96			; get Mario Y low byte
		PHA			; back up
		CLC			; \ offset by different numbers depending
		ADC MARIOYOSHI,y	; / on whether Mario is riding Yoshi or not
		STA $96			; set Y low byte
		LDA $97			; get Mario Y high byte
		PHA			; back up
		ADC #$00		; add high byte of offset (allow carry)
		STA $97			; set Y high byte
		JSR CALCPTFULLFRAME	; calculate what frame to ROTATE to
		JSR ROTATE		; ROTATE towards that frame
		PLA			; \ load backed up
		STA $97			; / Y high byte
		PLA			; \ load backed up
		STA $96			; / Y low byte
NOROTATE:
if !sa1 == 1
		LDA #$01
		STA $2250
		LDA !TIMER,x	; \
		ASL A			;  | set
		STA $2251 		;  | head
		STZ $2252		;  | frame
		LDA #$09		;  |
		STA $2253
		STZ $2254
		NOP : BRA $00 ; wait 5 cycles
		LDA $2306		;  |
else
		LDA !TIMER,x	; \
		ASL A			;  | set
		STA $4204		;  | head
		STZ $4205		;  | frame
		LDA #$09		;  |
		STA $4206		;  |
		PHB : PLB : PHB : PLB : NOP ; wait 16 cycles
		LDA $4214		;  |
endif
		ASL A			;  |
		AND #%00000110		;  |
		TAY			;  |
		LDA FRAMESLIST,y	;  |
		STA !TMP1		;  |
		LDA FRAMESLIST+1,y	;  |
		STA !TMP2		;  |
		LDY !OFFSET,x		;  |
		LDA (!TMP1),y		;  |
		STA !FRAME,x		;  |
		LDA FBANKS,y		;  |
		STA !FBANK,x		; /
		LDA !TIMER,x		; \ don't advance to next
		BNE RETURNTRACK		; / status if not time
		LDA #$10		; \ time to
		STA !TIMER,x		; / pause
		INC !ACTSTATUS,x		; inc status to pre-fire pause
RETURNTRACK:	RTS

;;;;;;;;;;;;;;;;

PREFIREPAUSE:	LDA !TIMER,x		; \  advance when
		BNE RETURNPFPAUSE	;  | the timer
		INC !ACTSTATUS,x		; /  has run out
RETURNPFPAUSE:	RTS

;;;;;;;;;;;;;;;;

TAKEAIM:		LDA $14			; \  only process
		AND #%00000001		;  | every other
		BNE RETURNAIM		; /  frame
		LDY !SHOTSFIRED,x	; \  get offset
		INY			;  | of current
		LDA (!FIRINGADDR),y	; /  shot to fire
		LDY !OFFSET,x		; \
		PHA			;  | flip
		LDA XDIRECTION,y	;  | offset if
		TAY			;  | necessary
		PLA			;  |
		CPY #$00		;  |
		BEQ POSITIVEOFFSET	;  |
		EOR #$FF		;  |
		CLC			;  |
		ADC #$3E		; /
POSITIVEOFFSET:	PHA			; preserve goal offset
		JSR ROTATE		; ROTATE towards goal
		LDY !OFFSET,x		; get new offset -> Y
		LDA FRAMES,y		; \
		STA !FRAME,x		;  | set head
		LDA FBANKS,y		;  | frame
		STA !FBANK,x		; /
		PLA			; load preserved goal offset
		CMP !OFFSET,x		; \ don't fire shot
		BNE RETURNAIM		; / if not at goal
		LDA FRAMES4,y		; \
		STA !FRAME,x		;  | set open mouth
		LDA FBANKS,y		;  | head frame
		STA !FBANK,x		; /
		JSR SHOOT		; fire projectile
		LDA #!SHOOTSND		; \ play
		STA $1DFC|!addr		; / SFX
		INC !SHOTSFIRED,x	; another shot fired
		LDA #$10		; \ set open
		STA !TIMER,x		; / mouth timer
		INC !ACTSTATUS,x		; inc status to fire-pause 1
RETURNAIM:	RTS

;;;;;;;;;;;;;;;;

FIREPAUSE1:	LDA !TIMER,x		; \ if not done, don't
		BNE RETURNFPAUSE1	; / close mouth
		LDY !OFFSET,x		; \
		LDA FRAMES,y		;  | set
		STA !FRAME,x		;  | closed
		LDA FBANKS,y		;  | mouth
		STA !FBANK,x		; /
		LDA #$30		; \ set closed
		STA !TIMER,x		; / mouth timer
		INC !ACTSTATUS,x		; inc status to fire-pause 2
RETURNFPAUSE1:	RTS

;;;;;;;;;;;;;;;;

FIREPAUSE2:	LDA !TIMER,x		; \ if not done,
		BNE RETURNFPAUSE2	; / don't move on
		LDA (!FIRINGADDR)	; \  fire another shot
		CMP !SHOTSFIRED,x	;  | if not all shots
		BCS SOMESHOTSLEFT	; /  have been fired

NOSHOTSLEFT:	LDA #$70		; \ set TRACKING
		STA !TIMER,x		; / timer
		STZ !SHOTSFIRED,x	; reset shots fired
		STZ !ACTSTATUS,x		; set status to TRACKING
RETURNFPAUSE2:	RTS

SOMESHOTSLEFT:	LDA #$02		; \ set status
		STA !ACTSTATUS,x		; / to aiming
		RTS

;;;;;;;;;;;;;;;;

HIT:		LDA !HITDATA,x		; \  ROTATE
		AND #%01111111		;  | towards
		JSR ROTATE		; /  hit-goal
		LDY !OFFSET,x		; \
		LDA !HITDATA,x		;  | set
		AND #%10000000		;  | head
		BNE OPENMOUTH		;  | frame
CLOSEDMOUTH:	LDA FRAMES,y		;  |
		STA !FRAME,x		;  |
		LDA FBANKS,y		;  |
		STA !FBANK,x		;  |
		BRA GETHITCONFIG	;  |
OPENMOUTH:	LDA FRAMES4,y		;  |
		STA !FRAME,x		;  |
		LDA FBANKS,y		;  |
		STA !FBANK,x		; /
GETHITCONFIG:	LDA (!HITPALADDR)	; \  decide how to act
		CMP !HITCOUNT,x		;  | depending on whether
		BCS TIMERFLASH		; /  this is the last shot
		LDA #$FF		; \ set sprite not to re-appear
		STA !161A,x		; / if it goes offscreen
		LDY !HITCOUNT,x		; get hit-count, used for palette index
		BRA SKIPFLASH		; skip over code to flash using the timer
TIMERFLASH:	LDA !TIMER,x		; \
		LSR A			;  | get
		LSR A			;  | flash
		AND #%00000001		;  | index
		EOR #%00000001		; /
		CLC			; \ add hit
		ADC !HITCOUNT,x		; / count
		TAY			; index -> Y
SKIPFLASH:	LDA (!HITPALADDR),y	; get palette #
		ASL A			; \ convert to
		AND #%00001110		; / properties
		STA !HEADPALETTE,x	; set head palette
CHECKHITEND:	LDA !TIMER,x		; \ if timer expired,
		BNE RETURNHIT		; / then don't advance
		LDA #$10		; \ set hit-pause
		STA !TIMER,x		; / timer
		INC !ACTSTATUS,x		; inc status to hit pause
RETURNHIT:	RTS

;;;;;;;;;;;;;;;;

HITPAUSE:	LDA !TIMER,x		; \ if not done,
		BNE RETURNHPAUSE	; / don't move on
		LDA (!HITPALADDR)	; \  if this is the last hit,
		CMP !HITCOUNT,x		;  | then begin shrinking, if
		BCC INITSHRINK		; /  not, go back to TRACKING
		STZ !HITDATA,x		; reset hit data
		LDA #$70		; \ set TRACKING
		STA !TIMER,x		; / timer
		STZ !ACTSTATUS,x		; set TRACKING status
RETURNHPAUSE:	RTS

INITSHRINK:	LDA #$30		; \ set shrinking
		STA !TIMER,x		; / timer
		INC !ACTSTATUS,x		; inc status to shrinking
		RTS

;;;;;;;;;;;;;;;;

SHRINK:		LDA !HITDATA,x		; \
		AND #%10000000		;  | get
		LSR A			;  | mouth
		LSR A			;  | state
		STA !TMP1		;  | offset
		LSR A			;  |
		ORA !TMP1		; /
		LDY !TIMER,x		; timer -> Y
		CLC			; \ add frame
		ADC SHRINKFRAMES,y	; / offset
		STA !FRAME,x		; set frame
		LDA #$01		; \ set frame
		STA !FBANK,x		; / bank
		TYA			; \
		LSR A			;  | flash
		LSR A			;  | head
		AND #%00000001		;  | when
		BNE BLINKHEAD		;  | shrinking
NORMALHEAD:	LDA (!HITPALADDR)	;  |
		INC A			;  |
		TAY			;  |
		LDA (!HITPALADDR),y	;  |
		ASL A			;  |
		AND #%00001110		;  |
		BRA SETHEADPALETTE	;  |
BLINKHEAD:	LDA #!FLASHPROP		;  |
SETHEADPALETTE:	STA !HEADPALETTE,x	; /
		LDA !TIMER,x		; \ if not done,
		BNE RETURNSHRINK	; / don't move on
		LDA #$04                ; \ set sprite to
		STA !14C8,x             ; / spin-jump kill
		LDA #$1F                ; \ set spin jump
		STA !1540,x             ; / animation timer
		JSL !SPINJUMPSTARS|!bank	; do star animation
RETURNSHRINK:	RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		!ROT_TEMP1 = $00

ROTATE:		STA !ROT_TEMP1		; temporarily store destination offset
		SEC			; \ minus current
		SBC !OFFSET,x		; / frame offset
		BEQ ROTATEDONE		; if it's zero (they're equal) then don't ROTATE
		JSR WRAPOFFSET		; wrap offset at #$3E (means #$3E would be #$00)
		CMP #$1F		; \ CW (+) rotation if diff is less
		BCC INCROT		; / than #$1F, else CCW (-) rotation
DECROT:		LDA !OFFSET,x		; get offset
		DEC A			; subtract 1
		JSR WRAPOFFSET		; wrap at #$3E
		CMP !ROT_TEMP1		; \ if result is at destination,
		BEQ SETOFFSET		; / avoid any extra rotation
		TAY			; \  do extra rotation if there is any
		SEC			;  | to do (this makes 0/180 degree
		SBC ROTATEDEC,y		; /  FRAMES skip their mirror image)
		BRA FINISHOFFSET	; branch over CW rotation
INCROT:		LDA !OFFSET,x		; \ if result is at destination,
		INC A			; / avoid any extra rotation
		JSR WRAPOFFSET		; wrap at #$3E
		CMP !ROT_TEMP1		; \ if result is at destination,
		BEQ SETOFFSET		; / avoid any extra rotation
		TAY			; \  do extra rotation
		CLC			;  | if there is any
		ADC ROTATEINC,y		; /  to do
FINISHOFFSET:	JSR WRAPOFFSET		; wrap at #$3E
SETOFFSET:	STA !OFFSET,x		; set new offset
ROTATEDONE:	RTS

WRAPOFFSET:	BPL NOWRAP1		; \
		CLC			;  | wrap if negative
		ADC #$3E		; /
NOWRAP1:		CMP #$3E		; \
		BCC NOWRAP2		;  | wrap if
		SEC			;  | >= #$3E
		SBC #$3E		; /
NOWRAP2:		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		!FC_TEMP1 = $00		; will use 2 bytes
		!FC_TEMP2 = $02		; will use 2 bytes
		!FC_TEMP3 = $04		; will use 2 bytes
		!FC_TEMP4 = $06		; will use 2 bytes

CALCPTFULLFRAME:	JSR CALCFRAME
		JSR CALCPTQUADFLIP
		JSR PTOOIEFRAMEWRAP
		RTS

PTOOIEFRAMEWRAP:	CMP #$1C
		BCC NOPTWRAP
		CMP #$22
		BCS NOPTWRAP
		CMP #$1F
		BCS LEFTWRAP
		LDA #$1B
NOPTWRAP:	RTS
LEFTWRAP:	LDA #$22
		RTS

CALCPTQUADFLIP:	PHA
		%SubVertPos()
		TYA
		EOR !EXTRA_BIT
		TAY
		PLA
		CPY #$00
		BNE CHKHORZ
		EOR #$FF
		CLC
		ADC #$1F
CHKHORZ:		PHA
		%SubHorzPos()
		PLA
		CPY #$00
		BEQ ENDQUADCHK
		EOR #$FF
		CLC
		ADC #$3E
ENDQUADCHK:	RTS

CALCFRAME:	LDA !D8,x
		SEC
		SBC $96
		STA !FC_TEMP2
		LDA !14D4,x
		SBC $97
		STA !FC_TEMP2+1
		BNE HORZDIST
		LDA !FC_TEMP2
		BNE HORZDIST
		BRA SETHORZ
HORZDIST:	LDA !E4,x
		SEC
		SBC $94
		STA !FC_TEMP1
		LDA !14E0,x
		SBC $95
		STA !FC_TEMP1+1
		BNE BEGINMATH
		LDA !FC_TEMP1
		BNE BEGINMATH
		BRA SETVERT
BEGINMATH:	PHP
		REP #%00100000
		LDA !FC_TEMP2
		BPL CHKXDIST
		EOR #$FFFF
		INC A
		STA !FC_TEMP2
CHKXDIST:	LDA !FC_TEMP1
		BPL MULT
		EOR #$FFFF
		INC A
		STA !FC_TEMP1
MULT:		ASL A
		ASL A
		ASL A
		ASL A
		STA !FC_TEMP3
		LDA !FC_TEMP1
		CLC
		ADC !FC_TEMP2
		STA !FC_TEMP4
		LDY #$00
		LDA !FC_TEMP4
		DEC A
DIVLOOP:		CMP !FC_TEMP3
		BCS END_DIVIDE
		INY
		CLC
		ADC !FC_TEMP4
		BRA DIVLOOP
END_DIVIDE:	TYA
		PLP
		RTS
SETHORZ:		LDA #$0F
		RTS
SETVERT:		LDA #$00
		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

NOFREESLOTS:	PLA
		;STA !EXTRA_BIT
		RTS

SHOOT:		LDA !EXTRA_BIT		; \ back up extra
		PHA			; / bit scratch RAM
		JSL $02A9DE|!bank		; \ get slot or else
		BMI NOFREESLOTS		; / end if no SLOTS
		PLA			; \ load backed up extra
		STA !EXTRA_BIT		; / bit scratch RAM
		LDA #$01		; \ set sprite status
		STA !14C8,y		; / as a new sprite
		PHX			; back up X
		TYX			; new sprite index to X
		LDA #!SPRITE_NUM		; \ store custom
		STA !7FAB9E,x		; / sprite number
		JSL $07F7D2|!bank		; reset sprite properties
		JSL $0187A7|!bank		; get table values for custom sprite
		LDA #$08		; \ mark as a
		STA !7FAB10,x		; / custom sprite
		PLX			; load backed up X
		LDA !E4,x		; \
		STA !E4,y		;  | set center
		LDA !14E0,x		;  | position for
		STA !14E0,y		;  | needlenose
		LDA !D8,x		;  | so that in next
		STA !D8,y		;  | code, it can
		LDA !14D4,x		;  | be shifted
		STA !14D4,y		; /
		LDA !OFFSET,x		; \
		PHX			;  | X
		TAX			;  |
		LDA XCOORDS,x		;  |
		LDX #$00		;  |
		CMP #$00		;  |
		BPL ADDPROJXDISP	;  |
		DEX			;  |
ADDPROJXDISP:	CLC			;  |
		ADC !E4,y		;  |
		STA !E4,y		;  |
		TXA			;  |
		ADC !14E0,y		;  |
		STA !14E0,y		;  |
		PLX			; /
		LDA !OFFSET,x		; \
		PHX			;  | Y
		TAX			;  |
		LDA YCOORDS,x		;  |
		LDX !EXTRA_BIT		;  |
		BEQ NOFLIPPROJY		;  |
		EOR #$FF		;  |
		INC A			;  |
NOFLIPPROJY:	LDX #$00		;  |
		CMP #$00		;  |
		BPL ADDPROJYDISP	;  |
		DEX			;  |
ADDPROJYDISP:	CLC			;  |
		ADC !D8,y		;  |
		STA !D8,y		;  |
		TXA			;  |
		ADC !14D4,y		;  |
		STA !14D4,y		;  |
		PLX			; /
		LDA !OFFSET,x		; \
		PHX			;  | speeds
		TAX			;  |
		LDA XSPEEDS,x		;  |
		STA !B6,y		;  |
		LDA YSPEEDS,x		;  |
		LDX !EXTRA_BIT		;  |
		BEQ NOFLIPPROJYS	;  |
		EOR #$FF		;  |
		INC A			;  |
NOFLIPPROJYS:	STA !AA,y		;  |
		PLX			; /
		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HEADINTERACT:	LDA !E4,x		; \
		PHA			;  | back up
		LDA !14E0,x		;  | sprite
		PHA			;  | positions
		LDA !D8,x		;  |
		PHA			;  |
		LDA !14D4,x		;  |
		PHA			; /
		LDY !OFFSET,x		; \
		LDA XCOORDS,y		;  | X
		LDY #$00		;  |
		CMP #$00		;  |
		BPL ADDXDISP		;  |
		DEY			;  |
ADDXDISP:	CLC			;  |
		ADC !E4,x		;  |
		STA !E4,x		;  |
		TYA			;  |
		ADC !14E0,x		;  |
		STA !14E0,x		; /
		LDY !OFFSET,x		; \
		LDA YCOORDS,y		;  | Y
		LDY !EXTRA_BIT		;  |
		BEQ NOTUPSIDEDOWN	;  |
		EOR #$FF		;  |
		INC A			;  |
NOTUPSIDEDOWN:	LDY #$00		;  |
		CMP #$00		;  |
		BPL ADDYDISP		;  |
		DEY			;  |
ADDYDISP:	CLC			;  |
		ADC !D8,x		;  |
		STA !D8,x		;  |
		TYA			;  |
		ADC !14D4,x		;  |
		STA !14D4,x		; /
		LDA !1662,x		; \
		PHA			;  | set head clipping
		AND #%11000000		;  | offset and backup
		ORA #$27		;  | that sprite table
		STA !1662,x		; /
		JSR INTERACTION		; Mario/Yoshi INTERACTION
		JSR STOPFIREBALLS	; interact with fireballs
		JSR SHELLINTERACT	; interact with shells
		PLA			; \ load backed-up
		STA !1662,x		; / tweaker value $1662
		PLA			; \
		STA !14D4,x		;  | load backed up
		PLA			;  | sprite positions
		STA !D8,x		;  |
		PLA			;  |
		STA !14E0,x		;  |
		PLA			;  |
		STA !E4,x		; /
HEADINTERACTEND:	RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SHELLINTERACT:	LDY #!SprSize		; load number of times to go through loop
KO_LOOP:		CPY #$00		; \ zero? if so,
		BEQ HEADINTERACTEND	; / end loop
		DEY			; decrease # of times left+get index
		LDA !14C8,y		; \  if sprite's status
		CMP #$09		;  | is less than 9 (9,A,B = shell modes)
		BCC KO_LOOP		; /  ignore sprite
		LDA !1686,y		; \  if sprite doesn't
		AND #%00001000		;  | interact with others
		BNE KO_LOOP		; /  don't continue
		JSL !GETSPRITECLIPPINGA|!bank	; \
		PHX			;  | if sprite is
		TYX			;  | not touching
		JSL !GETSPRITECLIPPINGB|!bank	;  | this sprite
		PLX			;  | don't continue
		JSL !CHECKFORCONTACT|!bank	;  |
		BCC KO_LOOP		; /
		LDA !14C8,y		; \  speed doesn't matter
		CMP #$0B		;  | if Mario is holding
		BEQ GETHIT		; /  the shell (status=B)
		LDA !AA,y		; \ continue if sprite
		BNE GETHIT		; / has Y speed
		LDA !B6,y		; \ continue if sprite
		BNE GETHIT		; / has X speed
		BRA KO_LOOP		; no speed / not holding -> don't kill
GETHIT:		LDA #$04		; \ give mario
		JSL $02ACE5|!bank		; / 1000 points
		LDA #!KNOCKOUTSND	; \ play knockout
		STA $1DFC|!addr		; / sound
		LDA !9E,y		; \
		CMP #$53		;  | if throw block, don't do star animation
		BEQ NOSTARANI		; /
		LDA #$04                ; \ set sprite to
		STA !14C8,y             ; / spin-jump kill
		LDA #$1F                ; \ set spin jump
		STA !1540,y             ; / animation timer
		PHY			; \
		STY $15E9|!addr		;  | do star
		JSL $07FC3B|!bank		;  | animation
		STX $15E9|!addr		;  |
		PLY			; /
STARTKNOCKOUT:	LDA !B6,y		; \
		BNE GETHIBIT		;  | get index
		LDA !E4,x		;  | for direction
		CMP !E4,y		;  | to turn
		LDA !14E0,x		;  | towards
		SBC !14E0,y		;  |
GETHIBIT:	ROL A			;  |
		ROL A			;  |
		AND #%00000001		;  |
		TAY			; /
		LDA !HITDATA,x		; \  set data for
		AND #%10000000		;  | how to act when
		ORA HITOFFSETS,y	;  | hit with shell
		LDY !ACTSTATUS,x		;  | %abbbbbbb:
		ORA STUNMOUTHSTATE,y	;  | a=mouth closed/open
		STA !HITDATA,x		; /  b=offset
		STZ !SHOTSFIRED,x	; reset shots fired, in case shell was thrown between shots
		INC !HITCOUNT,x		; another hit was taken
		LDA #$20		; \ set
		STA !TIMER,x		; / timer
		LDA #$05		; \ set hit
		STA !ACTSTATUS,x		; / status
		LDA (!HITPALADDR)	; \  disable INTERACTION
		CMP !HITCOUNT,x		;  | if this is the last
		BCS ENDSHELLLOOP	; /  hit before it dies
		LDA !167A,x		; \
		ORA #%00000010		;  | consider sprite not to be an enemy
		STA !167A,x		; /
ENDSHELLLOOP:	RTS

NOSTARANI:	LDA !1656,y		; \  force shell
		ORA #%10000000		;  | to disappear
		STA !1656,y		; /  in smoke
		LDA #$02		; \ set shell into
		STA !14C8,y		; / death mode (status=2)
		BRA STARTKNOCKOUT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

INTERACTION:	LDA $77			; \
		AND #%00000100		;  | if on ground, always do Yoshi check
		BNE YOSHI_CHECK		; /
		LDA $7D			; \ if moving downward
		BPL MARIO_INTERACT	; / skip Yoshi check
YOSHI_CHECK:	LDA $187A|!addr		; \ yoshi interact
		BNE YOSHI_INTERACT	; / if riding Yoshi
MARIO_INTERACT:	JSL !MARIOSPRINTERACT|!bank	; normal interact with mario
		RTS
YOSHI_INTERACT:	LDA $1490|!addr		; \ Mario INTERACTION
		BNE MARIO_INTERACT	; / if Mario has star
		LDA !154C,x		; \ don't interact if disable
		BNE END_INTERACT	; / INTERACTION timer is set
		LDA !167A,x		; \
		PHA			;  | set "no default INTERACTION"
		ORA #%10000000		;  | flag temporarily and back-up
		STA !167A,x		; /
		JSL !MARIOSPRINTERACT|!bank	; detect if mario touching
		PLA			; \ load backed up
		STA !167A,x		; / interact flag
		BCC END_INTERACT	; if mario is not touching, don't lose yoshi
		JSR LOSEYOSHI		; else lose yoshi
END_INTERACT:	RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LOSEYOSHI:	PHX
		LDX $18E2|!addr
		LDA #$10
		STA !163E-1,x
		LDA #$03
		STA $1DFA|!addr
		LDA #$13
		STA $1DFC|!addr
		LDA #$02
		STA !C2-1,x
		STZ $187A|!addr
		STZ $0DC1|!addr
		LDA #$C0
		STA $7D
		STZ $7B
		LDY !157C-1|!addr,x
		PHX
		TYX
		LDA $02A4B3|!bank,x
		PLX
		STA !B6-1,x
		STZ !1594-1,x
		STZ !151C-1,x
		STZ $18AE|!addr
		LDA #$30
		STA $1497|!addr
		PLX
		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

STOPFIREBALLS:	LDY #$09		; index of first fireball
FB_LOOP_BEGIN:	LDA $170B|!addr,y		; \
		CMP #$05		;  | ignore if not fireball
		BNE FB_LOOP		; /
		JSL !GETSPRITECLIPPINGA|!bank	; \
		LDA $171F|!addr,y		;  | ignore
		SEC			;  | if not
		SBC #$02		;  | touching
		STA $00			;  | sprite
		LDA $1733|!addr,y		;  |
		SBC #$00		;  |
		STA $08			;  |
		LDA #$0C		;  |
		STA $02			;  |
		LDA $1715|!addr,y		;  |
		SEC			;  |
		SBC #$04		;  |
		STA $01			;  |
		LDA $1729|!addr,y		;  |
		SBC #$00		;  |
		STA $09			;  |
		LDA #$13		;  |
		STA $03			;  |
		JSL !CHECKFORCONTACT|!bank	;  |
		BCC FB_LOOP		; /
		LDA #$0F		; \
		STA $176F|!addr,y		;  | turn fireball
		LDA #$01		;  | into smoke
		STA $170B|!addr,y		; /
		LDA #$01		; \ play
		STA $1DF9|!addr		; / SFX
FB_LOOP:		DEY			; \
		CMP #$08		;  | loop if not reached end
		BCS FB_LOOP_BEGIN	; /
		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GRAPHICS ROUTINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		!GFXTMP_HORIZFLIP = $02
		!GFXTMP_TILECOUNT = $03
		!GFXTMP_DSLOTTILE = $04
		!GFXTMP_XPOSITION = $05
		!GFXTMP_YPOSITION = $06
		!GFXTMP_FRAMEBANK = $07

		!STEMTILE = $69

HEAD_TILES:	db $00,$02,$20,$22
HEAD_XPOS:	db $F8,$08,$F8,$08
HEAD_YPOS:	db $F8,$F8,$08,$08

SUB_GFX:
		%GetDrawInfo()
		LDA #$FF		; \ zero tiles
		STA !GFXTMP_TILECOUNT	; / drawn
		PHY			; \
		LDY !OFFSET,x		;  | set X
		LDA XDIRECTION,y	;  | flip
		STA !GFXTMP_HORIZFLIP	;  |
		PLY			; /
		JSR DRAW_HEAD		; draw head
		JSR DRAW_BASE		; draw base
		LDY #$02                ; #$02 means the tiles are 16x16
		LDA !GFXTMP_TILECOUNT	; # of tiles drawn -1
		;BMI RETURN_SUB_GFX	; return if no tiles drawn (commented out because there will never be zero tiles drawn)
		JSL !FINISHOAMWRITE|!bank	; don't draw if offscreen, set sizes
RETURN_SUB_GFX:	RTS

DRAW_HEAD:	PHY			; \
		LDY !OFFSET,x		;  | head
		LDA XCOORDS,y		;  | coords
		STA !GFXTMP_XPOSITION	;  | to
		LDA YCOORDS,y		;  | scratch
		STA !GFXTMP_YPOSITION	;  | RAM
		PLY			; /
		LDA !FBANK,x		; \ set frame bank (will be used
		STA !GFXTMP_FRAMEBANK	; / in modified dynamic routine)
		LDA !FRAME,x		; \ reserve dynamic
		JSR GETSLOT		; / sprite slot
		BEQ RETURN_SUB_GFX	; if none available, return
		STA !GFXTMP_DSLOTTILE	; store dynamic sprite slot to scratch RAM
		PHX			; back up sprite index
		LDX #$00		; load X with zero
HEADLOOP:	CPX #$04		; \ if end of loop,
		BCS ENDHEADLOOP		; / branch to end
		LDA HEAD_XPOS,x		; \
		PHX			;  | X
		LDX !GFXTMP_HORIZFLIP	;  |
		BNE HEADNOFLIPX		;  |
		EOR #$FF		;  |
		INC A			;  |
HEADNOFLIPX:	PLX			;  |
		CLC			;  |
		ADC !GFXTMP_XPOSITION	;  |
		CLC			;  |
		ADC $00			;  |
		STA $0300|!addr,y		; /
		LDA HEAD_YPOS,x		; \
		CLC			;  | Y
		ADC !GFXTMP_YPOSITION	;  |
		PHX			;  |
		LDX !EXTRA_BIT		;  |
		BEQ HEADNOFLIPY		;  |
		EOR #$FF		;  |
		INC A			;  |
HEADNOFLIPY:	PLX			;  |
		CLC			;  |
		ADC $01			;  |
		STA $0301|!addr,y		; /
		LDA HEAD_TILES,x	; \
		CLC			;  | set tile
		ADC !GFXTMP_DSLOTTILE	;  | number
		STA $0302|!addr,y		; /
		PHX			; \
		LDX $15E9|!addr		;  | sprite
		LDA !15F6,x		;  | props
		AND #%11110001		;  |
		ORA !HEADPALETTE,x	;  |
		LDX !GFXTMP_HORIZFLIP	;  |
		BNE HEADNOFLIPXT	;  |
		ORA #%01000000		;  |
HEADNOFLIPXT:	;PLX			;  |
		;PHX			;  |
		LDX !EXTRA_BIT		;  |
		BEQ HEADNOFLIPYT	;  |
		ORA #%10000000		;  |
HEADNOFLIPYT:	PLX			;  |
		ORA $64			;  |
		STA $0303|!addr,y		; /
		INY			; \
		INY			;  | next OAM
		INY			;  | index
		INY			; /
		INX			; next tile
		INC !GFXTMP_TILECOUNT	; another tile was drawn
		BRA HEADLOOP		; loop
ENDHEADLOOP:	PLX			; load backed up X
RETURNHEAD:	RTS

DRAW_BASE:	LDA $00			; \ X
		STA $0300|!addr,y		; /
		LDA $01			; \ Y
		STA $0301|!addr,y		; /
		LDA #!STEMTILE		; \ set tile
		STA $0302|!addr,y		; / number
		LDA !15F6,x		; \
		PHX			;  | set
		LDX !GFXTMP_HORIZFLIP	;  | sprite
		BNE BASENOFLIPXT	;  | props
		ORA #%01000000		;  |
BASENOFLIPXT:	;PLX			;  |
		;PHX			;  |
		LDX !EXTRA_BIT		;  |
		BEQ BASENOFLIPYT	;  |
		ORA #%10000000		;  |
BASENOFLIPYT:	PLX			;  |
		ORA $64			;  |
		STA $0303|!addr,y		; /
		INY			; \
		INY			;  | next OAM
		INY			;  | index
		INY			; /
		INC !GFXTMP_TILECOUNT	; another tile was drawn
		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Dynamic sprite routine
; Programmed mainly by SMKDan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!Temp = $09
!Timers = $0B

!SlotPointer = $0660|!Base2			;16bit pointer for source GFX
!SlotBank = $0662|!Base2			;bank
!SlotDestination = $0663|!Base2			;VRAM address
!SlotsUsed = $06FE|!Base2			;how many slots have been used

!MAXSLOTS = $04			;maximum selected slots

GETSLOT:
get_dynamic_slot:
	PHY		;preserve OAM index
	PHA		;preserve frame
	LDA !SlotsUsed	;test if slotsused == maximum allowed
	CMP #!MAXSLOTS
	BNE +

	PLA
	PLY
	LDA #$00	;zero on no free slots
	RTS

+	PLA		;pop frame
	REP #$20	;16bit A
	AND.w #$00FF	;wipe high
	XBA		;<< 8
	LSR A		;>> 1 = << 7
	STA !Temp	;back to scratch
	LDA.w #gfx	;Get 16bit address
	CLC
	ADC !Temp	;add frame offset	
	STA !SlotPointer	;store to pointer to be used at transfer time
	SEP #$20	;8bit store
    ; PHB : PLA
	LDA.b #gfx/$10000
	CLC
	ADC !GFXTMP_FRAMEBANK
	STA !SlotBank	;store bank to 24bit pointer

	PHX		;This is how I made your boi a routine
	LDX !SlotsUsed		;calculate VRAM address + tile number
	LDA.L SlotsTable,X	;get tile# in VRAM
	PLX
	PHA		;preserve for eventual pull
	SEC
	SBC #$C0	;starts at C0h, they start at C0 in tilemap
	REP #$20	;16bit math
	AND.w #$00FF	;wipe high byte
	ASL A		;multiply by 32, since 32 bytes/16 words equates to 1 32bytes tile
	ASL A
	ASL A
	ASL A
	ASL A
if !SA1 == 1
	CLC : ADC #$8000	;add 8000, base address of buffer
else
	CLC : ADC #$0B44	;add 0B44, base address of buffer	
endif
	STA !SlotDestination	;destination address in the buffer
	SEP #$20
	STZ !Timers

;;;;;;;;;;;;;;;;
;Transfer routine
;;;;;;;;;;;;;;;;

;DMA ROM -> RAM ROUTINE

if !SA1 == 1
;set destination RAM address
	REP #$20
	LDY #$C4
	STY $2230
	LDA.w !SlotDestination
	STA $2235	;16bit RAM dest
	        
	         	;set 7F as bank

;common DMA settings
	         	;1 reg only
	        	;to 2180, RAM write/read
	         

;first line
	LDA !SlotPointer
	STA $2232	;low 16bits
	LDY !SlotBank
	STY $2234	;bank
	LDY #$80	;128 bytes
	STZ $2238
	STY $2238
	LDY #$41
	STY $2237
	
	LDY $318C
	BEQ $FB
	LDY #$00
	STY $318C
	STY $2230	;transfer

;lines afterwards
-	LDY #$C4
	STY $2230
	LDA.w !SlotDestination	;update buffer dest
	CLC
	ADC #$0200	;512 byte rule for sprites
	STA !SlotDestination	;updated base
	STA $2235	;updated RAM address

	LDA !SlotPointer	;update source address
	CLC
	ADC #$0200	;512 bytes, next row
	STA !SlotPointer
	STA $2232	;low 16bits
	LDY !SlotBank
	STY $2234	;bank
	LDY #$80
	STZ $2238
	STY $2238
	LDY #$41
	STY $2237
	
	LDY $318C
	BEQ $FB
	LDY #$00
	STY $318C
	STY $2230	;transfer
	LDY !Timers
	CPY #$02
	BEQ +
	INC !Timers
	BRA -
+
else
;common DMA settings
	REP #$20
	STZ $4300	;1 reg only
	LDY #$80	;to 2180, RAM write/read
	STY $4301
	
;set destination RAM address
	LDA !SlotDestination
	STA $2181	;16bit RAM dest
	LDY #$7F
	STY $2183	;set 7F as bank

	LDA !SlotPointer
	STA $4302	;low 16bits
	LDY !SlotBank
	STY $4304	;bank
	LDY #$80	;128 bytes
	STY $4305
	LDY #$01
	STY $420B	;transfer

;second line
-	LDA !SlotDestination	;update buffer dest
	CLC
	ADC #$0200	;512 byte rule for sprites
	STA !SlotDestination	;updated base
	STA $2181	;updated RAM address

	LDA !SlotPointer	;update source address
	CLC
	ADC #$0200	;512 bytes, next row
	STA !SlotPointer
	STA $4302	;low 16bits
	LDY !SlotBank
	STY $4304	;bank
	LDY #$80
	STY $4305
	LDY #$01
	STY $420B	;transfer
	LDY !Timers
	CPY #$02
	BEQ +
	INC !Timers
	BRA -
+
endif

	SEP #$20	;8bit A	
	INC !SlotsUsed	;one extra slot has been used

	PLA		;return starting tile number
	PLY
	RTS

SlotsTable:			;avaliable slots.  Any more transfers and it's overflowing by a dangerous amount.
	db $CC,$C8,$C4,$C0		


incbin piranhagfx.bin -> gfx
