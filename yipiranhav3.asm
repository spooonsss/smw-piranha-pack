;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Yoshi's Island Wild Piranha Plant (v3.0)
; Programmed by SMWEdit
;
; Uses first extra bit: YES
; It will be upside-down if the first extra bit is set
;
; You will need to patch SMKDan's dsx.asm to your ROM with xkas
; this sprite, like all other dynamic sprites, uses the last 4 rows of sp4
;
; This sprite requires manual GFX insertion. Please refer to the README for more info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Variables:
		!XMAX = $0080			; \ range in which the
		!YMAX = $00B0			; / piranha head gets big
		!KNOCKOUTSND = $37		; sound when you hit it with a shell (in $1DFC)
		!FLASHPALETTE = $0F		; palette for flashing head

;; Sprite Tables:
		!ACTSTATUS = !C2			; 0 = SMALL, 1 = big, 1 = hit with shell, 2 = shrinking
		!OFFSET = !1504
		!FRAME = !1570
		!FBANK = !1602
		!TIMER = !163E
		!COUNTER = !1534
		!HEADPALETTE = !1626
		!HITOFFSET = !151C
		!GFXMODE = !1528
		!EXTRA_BITS = !7FAB10

;; Scratch RAM:
		!TMP1 = $00
		!TMP2 = $01
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INIT and MAIN JSL targets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		!FLASHPROP = !FLASHPALETTE*2&%00001110

INIT_OFFSET:	db $1F,$00
FLIP_YPOS_LO:	db $00,$FE
FLIP_YPOS_HI:	db $00,$FF

		PRINT "INIT ",pc
		PHB
		PHK
		PLB
		%SubHorzPos()
		LDA INIT_OFFSET,y	;  | initial
		STA !OFFSET,x		; /  rotation
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
		LDA !15F6,x		; \
		AND #%00001110		;  | set head palette
		STA !HEADPALETTE,x	; /
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

XCOORDS:		db $F1,$F1,$F1,$F2
		db $F2,$F3,$F3,$F4
		db $F5,$F6,$F8,$F9
		db $FA,$FB,$FD,$FE
		db $02,$03,$05,$06
		db $07,$08,$0A,$0B
		db $0C,$0D,$0D,$0E
		db $0E,$0F,$0F,$0F

YCOORDS:		db $F4,$F3,$F2,$F2
		db $F1,$F0,$EF,$EF
		db $EE,$EE,$ED,$ED
		db $EC,$EC,$EC,$EC
		db $EC,$EC,$EC,$EC
		db $ED,$ED,$EE,$EE
		db $EF,$EF,$F0,$F1
		db $F2,$F2,$F3,$F4

ROTATEDEC:	db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$01
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00

ROTATEINC:	db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $01,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00

FRAMESC:		db $00,$01,$02,$03
		db $10,$11,$12,$13
		db $20,$21,$22,$23
		db $30,$31,$32,$33
		db $33,$32,$31,$30
		db $23,$22,$21,$20
		db $13,$12,$11,$10
		db $03,$02,$01,$00

FRAMESO:		db $C0,$C1,$C2,$C3
		db $D0,$D1,$D2,$D3
		db $E0,$E1,$E2,$E3
		db $F0,$F1,$F2,$F3
		db $F3,$F2,$F1,$F0
		db $E3,$E2,$E1,$E0
		db $D3,$D2,$D1,$D0
		db $C3,$C2,$C1,$C0

XDIRECTION:	db $01,$01,$01,$01
		db $01,$01,$01,$01
		db $01,$01,$01,$01
		db $01,$01,$01,$01
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00
		db $00,$00,$00,$00

SHRINKFRAMES:	db $53,$53,$53,$53
		db $52,$52,$52,$52
		db $51,$51,$51,$51
		db $50,$50,$50,$50
		db $43,$43,$43,$43
		db $42,$42,$42,$42
		db $41,$41,$41,$41
		db $40,$40,$40,$40
		db $33,$33,$33,$33
		db $32,$32,$32,$32
		db $31,$31,$31,$31
		db $30,$30,$30,$30

HITOFFSETS:	db $18,$07

MARIOYOSHI:	db $10,$20,$20

FRAMESLIST:	dw FRAMESC&$FFFF,FRAMESO&$FFFF

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
		%SubOffScreen()

		LDA !EXTRA_BIT		; get extra bit
		PHA			; push extra bit, some INTERACTION routines use the same scratch RAM address
		BNE NOFALL		; skip "physics" if it's set
		JSL !PHYSICS		; update position based on speed values
		JSL !SPRSPRINTERACT	; interact with other sprites
NOFALL:		LDA !ACTSTATUS,x		; get currrent status
		CMP #$02		; \ if dying, then
		BCS NOINTERACT		; / skip INTERACTION
		LDA !GFXMODE,x		; \ if SMALL then skip
		BEQ SKIPHEADINTER	; / head INTERACTION
		JSR HEADINTERACT	; head interactions
SKIPHEADINTER:	JSL !MARIOSPRINTERACT	; check for mario/sprite contact
		JSR SHELLINTERACT	; knock out if hit with shell
NOINTERACT:	PLA			; \ pull
		STA !EXTRA_BIT		; / extra bit
		LDA !ACTSTATUS,x		; get status
		JSL !EXECUTEPTR		; jump to code for current status

		dw SMALL&$FFFF
		dw BIG&$FFFF
		dw HIT&$FFFF
		dw SHRINK&$FFFF

;;;;;;;;;;;;;;;;

SMALL:		JSR XPROXIMITY		; \
		BEQ RETURNSMALL		;  | if still out of range,
		JSR YPROXIMITY		;  | then don't make head big
		BEQ RETURNSMALL		; /
		INC !ACTSTATUS,x		; big head status
		INC !GFXMODE,x		; big head mode
RETURNSMALL:	RTS

;;;;;;;;;;;;;;;;

BIG:		LDA $14			; \
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
		JSR CALCPPFULLFRAME	; calculate frame
		STA !OFFSET,x		; set frame
		PLA			; \ load backed up
		STA $97			; / Y high byte
		PLA			; \ load backed up
		STA $96			; / Y low byte
NOROTATE:	LDA !COUNTER,x		; \
		LSR A			;  | set
		LSR A			;  | head
		AND #%00000010		;  | frame
		TAY			;  |
		LDA FRAMESLIST,y	;  |
		STA !TMP1		;  |
		LDA FRAMESLIST+1,y	;  |
		STA !TMP2		;  |
		LDY !OFFSET,x		;  |
		LDA (!TMP1),y		;  |
		STA !FRAME,x		;  |
		STZ !FBANK,x		; /
		INC !COUNTER,x		; increase counter
		JSR XPROXIMITY		; \
		BEQ OUTOFRANGE		;  | if still in range,
		JSR YPROXIMITY		;  | the don't make head SMALL
		BNE RETURNTRACK		; /
		STZ !COUNTER,x		; reset mouth counter
OUTOFRANGE:	STZ !ACTSTATUS,x		; SMALL head status
		STZ !GFXMODE,x		; SMALL head mode
RETURNTRACK:	RTS

;;;;;;;;;;;;;;;;

HIT:		LDA !HITOFFSET,x		; \  ROTATE towards
		JSR ROTATE		; /  hit-goal
		LDY !OFFSET,x		; \
		LDA FRAMESO,y		;  | set head
		STA !FRAME,x		;  | frame
		STZ !FBANK,x		; /
		LDA !TIMER,x		; \ if timer expired,
		BNE RETURNHIT		; / then don't advance
		LDA #$30		; \ set shrinking
		STA !TIMER,x		; / timer
		INC !ACTSTATUS,x		; inc status to shrinking
RETURNHIT:	RTS

;;;;;;;;;;;;;;;;

SHRINK:		LDY !TIMER,x		; timer -> Y
		LDA SHRINKFRAMES,y	; get frame
		STA !FRAME,x		; set frame
		LDA #$01		; \ set frame
		STA !FBANK,x		; / bank
		TYA			; \
		LSR A			;  | flash
		LSR A			;  | head
		AND #%00000001		;  | when
		BNE BLINKHEAD		;  | shrinking
NORMALHEAD:	LDA !15F6,x		;  |
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
		JSL !SPINJUMPSTARS	; do star animation
RETURNSHRINK:	RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

XPROXIMITY:	LDA !E4,x
		STA !TMP1
		LDA !14E0,x
		STA !TMP2
		REP #%00100000
		LDA !TMP1
		SEC
		SBC $94
		BPL CHKX
		EOR #$FFFF
		INC A
CHKX:		SEC
		SBC.w #!XMAX
		STA !TMP1
		SEP #%00100000
		LDA !TMP2
		ROL A
		ROL A
		AND #%00000001
		RTS

YPROXIMITY:	LDA !D8,x
		STA !TMP1
		LDA !14D4,x
		STA !TMP2
		REP #%00100000
		LDA !TMP1
		SEC
		SBC $96
		BPL CHKY
		EOR #$FFFF
		INC A
CHKY:		SEC
		SBC.w #!YMAX
		STA !TMP1
		SEP #%00100000
		LDA !TMP2
		ROL A
		ROL A
		AND #%00000001
		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		!ROT_TEMP1 = $00

ROTATE:		STA !ROT_TEMP1		; temporarily store destination offset
		CMP !OFFSET,x		; compare with current offset
		BEQ ROTATEDONE		; if already there, then skip
		BCS INCROT		; decide which direction
DECROT:		LDA !OFFSET,x		; get offset
		DEC A			; subtract 1
		CMP !ROT_TEMP1		; \ if result is at destination,
		BEQ SETOFFSET		; / avoid any extra rotation
		TAY			; \  do extra rotation if there is any
		SEC			;  | to do (this makes 0/180 degree
		SBC ROTATEDEC,y		; /  frames skip their mirror image)
		BRA SETOFFSET		; branch over CW rotation
INCROT:		LDA !OFFSET,x		; \ if result is at destination,
		INC A			; / avoid any extra rotation
		CMP !ROT_TEMP1		; \ if result is at destination,
		BEQ SETOFFSET		; / avoid any extra rotation
		TAY			; \  do extra rotation
		CLC			;  | if there is any
		ADC ROTATEINC,y		; /  to do
SETOFFSET:	STA !OFFSET,x		; set new offset
ROTATEDONE:	RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		!FC_TEMP1 = $00		; will use 2 bytes
		!FC_TEMP2 = $02		; will use 2 bytes
		!FC_TEMP3 = $04		; will use 2 bytes
		!FC_TEMP4 = $06		; will use 2 bytes

OFFSETSBELOW:	db $1F,$00

CALCPPFULLFRAME:
		%SubVertPos()
		TYA
		EOR !EXTRA_BIT
		BNE ABOVE
		%SubHorzPos()
		LDA OFFSETSBELOW,y
		RTS
ABOVE:		JSR CALCFRAME
		EOR #$FF
		CLC
		ADC #$10
		PHA
		%SubHorzPos()
		PLA
		CPY #$00
		BNE ENDSIDECHK
		EOR #$FF
		CLC
		ADC #$20
ENDSIDECHK:	RTS

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
		JSL !GETSPRITECLIPPINGA	; \
		PHX			;  | if sprite is
		TYX			;  | not touching
		JSL !GETSPRITECLIPPINGB	;  | this sprite
		PLX			;  | don't continue
		JSL !CHECKFORCONTACT	;  |
		BCC KO_LOOP		; /
		LDA !14C8,y		; \  speed doesn't matter
		CMP #$0B		;  | if Mario is holding
		BEQ GETHIT		; /  the shell (status=B)
		LDA !AA,y		; \ continue if sprite
		BNE GETHIT		; / has Y speed
		LDA !B6,y		; \ continue if sprite
		BNE GETHIT		; / has X speed
		BRA KO_LOOP		; no speed / not holding -> don't kill
GETHIT:		LDA !GFXMODE,x		; \ kill like a normal sprite if
		BEQ KILLSMALL		; / the head isn't in big mode
		LDA #$04		; \ give mario
		JSL $02ACE5|!bank		; / 1000 points
		LDA #!KNOCKOUTSND	; \ play knockout
		STA $1DFC|!addr		; / sound
		LDA !9E|!dp,y		; \
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
STARTKNOCKOUT:	LDA !B6|!dp,y		; \
		BNE GETHIBIT		;  | get index
		LDA !E4,x		;  | for direction
		CMP !E4|!dp,y		;  | to turn
		LDA !14E0,x		;  | towards
		SBC !14E0,y		;  |
GETHIBIT:	ROL A			;  |
		ROL A			;  |
		AND #%00000001		;  |
		TAY			; /
		LDA HITOFFSETS,y	; \ set hit
		STA !HITOFFSET,x		; / offset
		LDA #$30		; \ sets
		STA !TIMER,x		; / timer
		LDA #$02		; \ set hit
		STA !ACTSTATUS,x		; / status
ENDSHELLLOOP:	RTS

NOSTARANI:	LDA !1656,y		; \  force shell
		ORA #%10000000		;  | to disappear
		STA !1656,y		; /  in smoke
		LDA #$02		; \ set shell into
		STA !14C8,y		; / death mode (status=2)
		BRA STARTKNOCKOUT

KILLSMALL:	LDA !1686,x		; \  force sprite
		AND #%11110111		;  | to interact
		STA !1686,x		; /  with others
		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

INTERACTION:	LDA $77			; \
		AND #%00000100		;  | if on ground, always do Yoshi check
		BNE YOSHI_CHECK		; /
		LDA $7D			; \ if moving downward
		BPL MARIO_INTERACT	; / skip Yoshi check
YOSHI_CHECK:	LDA $187A|!addr		; \ yoshi interact
		BNE YOSHI_INTERACT	; / if riding Yoshi
MARIO_INTERACT:	JSL !MARIOSPRINTERACT	; normal interact with mario
		RTS
YOSHI_INTERACT:	LDA $1490|!addr		; \ Mario INTERACTION
		BNE MARIO_INTERACT	; / if Mario has star
		LDA !154C,x		; \ don't interact if disable
		BNE END_INTERACT	; / INTERACTION timer is set
		LDA !167A,x		; \
		PHA			;  | set "no default INTERACTION"
		ORA #%10000000		;  | flag temporarily and back-up
		STA !167A,x		; /
		JSL !MARIOSPRINTERACT	; detect if mario touching
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
		JSL !GETSPRITECLIPPINGA	; \
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
		JSL !CHECKFORCONTACT	;  |
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

STEMTILES:	db $67,$69

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
		LDA !GFXMODE,x		; \ if SMALL then
		BEQ SKIPTOBASE		; / don't draw head
		JSR DRAW_HEAD		; draw head
SKIPTOBASE:	JSR DRAW_BASE		; draw base
		LDY #$02                ; #$02 means the tiles are 16x16
		LDA !GFXTMP_TILECOUNT	; # of tiles drawn -1
		;BMI RETURN_SUB_GFX	; return if no tiles drawn (commented out because there will never be zero tiles drawn)
		JSL !FINISHOAMWRITE	; don't draw if offscreen, set sizes
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
		PHY			; \
		LDY !GFXMODE,x		;  | set tile
		LDA STEMTILES,y		;  | number
		PLY			;  |
		STA $0302|!addr,y		; /
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
	PHB : PLA
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
