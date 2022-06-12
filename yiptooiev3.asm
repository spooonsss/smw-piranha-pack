;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Yoshi's Island Wild Ptooie Piranha Plant (v3.0)
; Programmed by SMWEdit
;
; Uses first extra bit: YES
; It will be upside-down if the first extra bit is set
; (Yoshi's Island did not have upside-down Ptooies, but I decided
; to leave this feature in since this is just a major blowhard edit)
;
; You will need to patch SMKDan's dsx.asm to your ROM with xkas
; this sprite, like all other dynamic sprites, uses the last 4 rows of sp4
;
; This sprite requires manual GFX insertion. Please refer to the README for more info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		GFXADDR = $358000		; change this to where you inserted your GFX

;; Variables:
		SPRITE_NUM = $82		; custom sprite of needlenose bomb
		SHOOTSND = $3A			; sound after needlenose fired  (in $1DFC)
		KNOCKOUTSND = $37		; sound when you hit it with a shell (in $1DFC)
		FLASHPALETTE = $0F		; palette for flashing head

;; Sprite Tables:
		ACTSTATUS = $C2			; 0 = tracking, 1 = pausing before firing, 2 = aiming/firing, 3 = pausing while mouth open, 4 = pausing while mouth just closed, 5 = hit with shell, 6 = pause after hit, 7 = shrinking
		OFFSET = $1504
		SHOTSFIRED = $1528
		HITCOUNT = $1534
		FRAME = $1570
		FBANK = $1602
		TIMER = $163E
		HEADPALETTE = $1626
		HITDATA = $151C
		EXTRA_BITS = $7FAB10
		EXTRA_PROP1 = $7FAB28

;; Scratch RAM:
		TMP1 = $00
		TMP2 = $01
		FIRINGADDR = $0A
		HITPALADDR = $0C
		EXTRA_BIT = $0E

;; Routines:
		EXECUTEPTR = $0086DF
		PHYSICS = $01802A
		SPRSPRINTERACT = $018032
		MARIOSPRINTERACT = $01A7DC
		FINISHOAMWRITE = $01B7B3
		GETSPRITECLIPPINGA = $03B69F
		GETSPRITECLIPPINGB = $03B6E5
		CHECKFORCONTACT = $03B72B
		SPINJUMPSTARS = $07FC3B

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INIT and MAIN JSL targets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		FLASHPROP = FLASHPALETTE*2&%00001110

CONFIGSADDR	dcw SHOTSADDR&$FFFF,HITSADDR&$FFFF

SHOTSADDR	dcw SHOTS1&$FFFF,SHOTS2&$FFFF
HITSADDR	dcw HITS1&$FFFF,HITS2&$FFFF

SHOTS1		dcb $02 : dcb $05,$05,$02	; #shots-1, shots
HITS1		dcb $02 : dcb $0D,$0A,$0C	; #hits-1, palettes

SHOTS2		dcb $00 : dcb $05		; #shots-1, shots
HITS2		dcb $02 : dcb $0D,$0A,$0C	; #hits-1, palettes

FLIP_YPOS_LO	dcb $00,$FE
FLIP_YPOS_HI	dcb $00,$FF

		dcb "INIT"
		PHB
		PHK
		PLB
		LDA EXTRA_PROP1,x	; \
		ASL A			;  | get
		TAY			;  | config
		LDA SHOTSADDR,y		;  | addresses
		STA FIRINGADDR		;  |
		LDA SHOTSADDR+1,y	;  |
		STA FIRINGADDR+1	;  |
		LDA HITSADDR,y		;  |
		STA HITPALADDR		;  |
		LDA HITSADDR+1,y	;  |
		STA HITPALADDR+1	; /
		LDY #$01		; \ get first
		LDA (FIRINGADDR)	; / offset
		PHA			; \
		JSR SUB_HORZ_POS	;  | flip offset
		PLA			;  | if necessary
		CPY #$00		;  |
		BEQ SETINITOFFSET	;  |
		EOR #$FF		;  |
		CLC			;  |
		ADC #$3E		; /
SETINITOFFSET	STA OFFSET,x		; set initial rotation
		TAY			; \
		LDA FRAMES,y		;  | set GFX information
		STA FRAME,x		;  | based on starting
		LDA FBANKS,y		;  | offset
		STA FBANK,x		; /
		LDA #$02		; \ set status to
		STA ACTSTATUS,x		; / taking aim
		LDA EXTRA_BITS,x	; \
		LSR A			;  | set initial
		LSR A			;  | Y position
		AND #%00000001		;  | depending
		TAY			;  | on extra
		LDA FLIP_YPOS_LO,y	;  | bit
		CLC			;  |
		ADC $D8,x		;  |
		STA $D8,x		;  |
		LDA FLIP_YPOS_HI,y	;  |
		ADC $14D4,x		;  |
		STA $14D4,x		; /
		LDY #$01		; \ first
		LDA (HITPALADDR),y	; / color
		ASL A			; \ convert to
		AND #%00001110		; / properties
		STA HEADPALETTE,x	; set head palette
		PLB
		RTL

		dcb "MAIN"			
		PHB
		PHK			
		PLB
		JSR SPRITE_ROUTINE
		PLB
		RTL     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SPRITE_ROUTINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

XCOORDS		dcb $02,$03,$05,$06
		dcb $07,$08,$0A,$0B
		dcb $0C,$0D,$0D,$0E
		dcb $0E,$0F,$0F,$0F
		dcb $0F,$0F,$0F,$0F
		dcb $0E,$0E,$0E,$0E
		dcb $0D,$0D,$0D,$0D
		dcb $FF,$FF,$FF
		dcb $FF,$FF,$FF,$F3
		dcb $F3,$F3,$F3,$F2
		dcb $F2,$F2,$F2,$F1
		dcb $F1,$F1,$F1,$F1
		dcb $F1,$F1,$F2,$F2
		dcb $F3,$F3,$F4,$F5
		dcb $F6,$F8,$F9,$FA
		dcb $FB,$FD,$FE

YCOORDS		dcb $EC,$EC,$EC,$EC
		dcb $ED,$ED,$EE,$EE
		dcb $EF,$EF,$F0,$F1
		dcb $F2,$F2,$F3,$F4
		dcb $F4,$F5,$F6,$F7
		dcb $F8,$F9,$FA,$FB
		dcb $FC,$FD,$FE,$FF
		dcb $FF,$FF,$FF
		dcb $FF,$FF,$FF,$FF
		dcb $FE,$FD,$FC,$FB
		dcb $FA,$F9,$F8,$F7
		dcb $F6,$F5,$F4,$F4
		dcb $F3,$F2,$F2,$F1
		dcb $F0,$EF,$EF,$EE
		dcb $EE,$ED,$ED,$EC
		dcb $EC,$EC,$EC

FRAMES		dcb $33,$32,$31,$30
		dcb $23,$22,$21,$20
		dcb $13,$12,$11,$10
		dcb $03,$02,$01,$00
		dcb $60,$61,$62,$63
		dcb $70,$71,$72,$73
		dcb $80,$81,$82,$83
		dcb $FF,$FF,$FF
		dcb $FF,$FF,$FF,$83
		dcb $82,$81,$80,$73
		dcb $72,$71,$70,$63
		dcb $62,$61,$60,$00
		dcb $01,$02,$03,$10
		dcb $11,$12,$13,$20
		dcb $21,$22,$23,$30
		dcb $31,$32,$33

FBANKS		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $01,$01,$01,$01
		dcb $01,$01,$01,$01
		dcb $01,$01,$01,$01
		dcb $FF,$FF,$FF
		dcb $FF,$FF,$FF,$01
		dcb $01,$01,$01,$01
		dcb $01,$01,$01,$01
		dcb $01,$01,$01,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00

FRAMES2		dcb $73,$72,$71,$70
		dcb $63,$62,$61,$60
		dcb $53,$52,$51,$50
		dcb $43,$42,$41,$40
		dcb $90,$91,$92,$93
		dcb $A0,$A1,$A2,$A3
		dcb $B0,$B1,$B2,$B3
		dcb $FF,$FF,$FF
		dcb $FF,$FF,$FF,$B3
		dcb $B2,$B1,$B0,$A3
		dcb $A2,$A1,$A0,$93
		dcb $92,$91,$90,$40
		dcb $41,$42,$43,$50
		dcb $51,$52,$53,$60
		dcb $61,$62,$63,$70
		dcb $71,$72,$73

FRAMES3		dcb $B3,$B2,$B1,$B0
		dcb $A3,$A2,$A1,$A0
		dcb $93,$92,$91,$90
		dcb $83,$82,$81,$80
		dcb $C0,$C1,$C2,$C3
		dcb $D0,$D1,$D2,$D3
		dcb $E0,$E1,$E2,$E3
		dcb $FF,$FF,$FF
		dcb $FF,$FF,$FF,$E3
		dcb $E2,$E1,$E0,$D3
		dcb $D2,$D1,$D0,$C3
		dcb $C2,$C1,$C0,$80
		dcb $81,$82,$83,$90
		dcb $91,$92,$93,$A0
		dcb $A1,$A2,$A3,$B0
		dcb $B1,$B2,$B3

FRAMES4		dcb $F3,$F2,$F1,$F0
		dcb $E3,$E2,$E1,$E0
		dcb $D3,$D2,$D1,$D0
		dcb $C3,$C2,$C1,$C0
		dcb $FF,$FF,$FF,$FF
		dcb $FF,$FF,$FF,$FF
		dcb $FF,$FF,$FF,$FF
		dcb $FF,$FF,$FF
		dcb $FF,$FF,$FF,$FF
		dcb $FF,$FF,$FF,$FF
		dcb $FF,$FF,$FF,$FF
		dcb $FF,$FF,$FF,$C0
		dcb $C1,$C2,$C3,$D0
		dcb $D1,$D2,$D3,$E0
		dcb $E1,$E2,$E3,$F0
		dcb $F1,$F2,$F3

XDIRECTION	dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00
		dcb $01,$01,$01,$01
		dcb $01,$01,$01,$01
		dcb $01,$01,$01,$01
		dcb $01,$01,$01,$01
		dcb $01,$01,$01,$01
		dcb $01,$01,$01,$01
		dcb $01,$01,$01,$01
		dcb $01,$01,$01

ROTATEDEC	dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00
		dcb $00,$00,$06,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$01

ROTATEINC	dcb $01,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $06,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00,$00
		dcb $00,$00,$00

XSPEEDS		dcb $00,$08,$11,$19
		dcb $21,$28,$2F,$36
		dcb $3B,$41,$45,$49
		dcb $4C,$4E,$50,$50
		dcb $50,$4E,$4C,$49
		dcb $45,$41,$3B,$36
		dcb $2F,$28,$21,$19
		dcb $11,$08,$00
		dcb $00,$F8,$EF,$E7
		dcb $DF,$D8,$D1,$CA
		dcb $C5,$BF,$BB,$B7
		dcb $B4,$B2,$B0,$B0
		dcb $B0,$B2,$B4,$B7
		dcb $BB,$BF,$C5,$CA
		dcb $D1,$D8,$DF,$E7
		dcb $EF,$F8,$00

YSPEEDS		dcb $B0,$B0,$B2,$B4
		dcb $B7,$BB,$BF,$C5
		dcb $CA,$D1,$D8,$DF
		dcb $E7,$EF,$F8,$00
		dcb $08,$11,$19,$21
		dcb $28,$2F,$36,$3B
		dcb $41,$45,$49,$4C
		dcb $4E,$50,$50
		dcb $50,$50,$4E,$4C
		dcb $49,$45,$41,$3B
		dcb $36,$2F,$28,$21
		dcb $19,$11,$08,$00
		dcb $F8,$EF,$E7,$DF
		dcb $D8,$D1,$CA,$C5
		dcb $BF,$BB,$B7,$B4
		dcb $B2,$B0,$B0

SHRINKFRAMES	dcb $23,$23,$23,$23
		dcb $22,$22,$22,$22
		dcb $21,$21,$21,$21
		dcb $20,$20,$20,$20
		dcb $13,$13,$13,$13
		dcb $12,$12,$12,$12
		dcb $11,$11,$11,$11
		dcb $10,$10,$10,$10
		dcb $03,$03,$03,$03
		dcb $02,$02,$02,$02
		dcb $01,$01,$01,$01
		dcb $00,$00,$00,$00

STUNMOUTHSTATE	dcb $00,$00,$00,$80,$00,$00,$00

HITOFFSETS	dcb $08,$35

MARIOYOSHI	dcb $10,$20,$20

FRAMESLIST	dcw FRAMES&$FFFF,FRAMES2&$FFFF,FRAMES3&$FFFF,FRAMES2&$FFFF

RETURN1		RTS

SPRITE_ROUTINE	LDA EXTRA_BITS,x	; \
		AND #%00000100		;  | store extra bit to
		LSR A			;  | scratch RAM so it's
		LSR A			;  | easily accessible
		STA EXTRA_BIT		; /
		JSR SUB_GFX		; GFX
		LDA $14C8,x		; \  return if
		CMP #$08		;  | sprite
		BNE RETURN1		; /  status != 8
		LDA $9D			; \ return if
		BNE RETURN1		; / sprites locked
		JSR SUB_OFF_SCREEN_X0	; only process sprite while on screen

		LDA EXTRA_BIT		; get extra bit
		PHA			; push extra bit, some interaction routines use the same scratch RAM address
		BNE NOFALL		; skip "physics" if it's set
		JSL PHYSICS		; update position based on speed values
		JSL SPRSPRINTERACT	; interact with other sprites
NOFALL		LDA EXTRA_PROP1,x	; \
		ASL A			;  | get
		TAY			;  | hit/palette
		LDA HITSADDR,y		;  | configuration
		STA HITPALADDR		;  | address
		LDA HITSADDR+1,y	;  |
		STA HITPALADDR+1	; /
		LDA (HITPALADDR)	; get #hits-1
		CMP HITCOUNT,x		; \ all hits taken?
		BCC NOINTERACT		; / then don't interact
		JSR HEADINTERACT	; head interactions
		JSL MARIOSPRINTERACT	; check for mario/sprite contact
		JSR SHELLINTERACT	; knock out if hit with shell
NOINTERACT	LDA EXTRA_PROP1,x	; \
		ASL A			;  | get
		TAY			;  | config
		LDA SHOTSADDR,y		;  | addresses
		STA FIRINGADDR		;  |
		LDA SHOTSADDR+1,y	;  |
		STA FIRINGADDR+1	;  |
		LDA HITSADDR,y		;  |
		STA HITPALADDR		;  |
		LDA HITSADDR+1,y	;  |
		STA HITPALADDR+1	; /
		PLA			; \ pull
		STA EXTRA_BIT		; / extra bit
		LDA ACTSTATUS,x		; get status
		JSL EXECUTEPTR		; jump to code for current status

		dcw TRACKING&$FFFF
		dcw PREFIREPAUSE&$FFFF
		dcw TAKEAIM&$FFFF
		dcw FIREPAUSE1&$FFFF
		dcw FIREPAUSE2&$FFFF
		dcw HIT&$FFFF
		dcw HITPAUSE&$FFFF
		dcw SHRINK&$FFFF

;;;;;;;;;;;;;;;;

TRACKING	LDA $14			; \
		AND #%00000001		;  | only rotate every other frame
		BNE NOROTATE		; /
		LDY $187A		; get riding Yoshi status
		LDA $96			; get Mario Y low byte
		PHA			; back up
		CLC			; \ offset by different numbers depending
		ADC MARIOYOSHI,y	; / on whether Mario is riding Yoshi or not
		STA $96			; set Y low byte
		LDA $97			; get Mario Y high byte
		PHA			; back up
		ADC #$00		; add high byte of offset (allow carry)
		STA $97			; set Y high byte
		JSR CALCPTFULLFRAME	; calculate what frame to rotate to
		JSR ROTATE		; rotate towards that frame
		PLA			; \ load backed up
		STA $97			; / Y high byte
		PLA			; \ load backed up
		STA $96			; / Y low byte
NOROTATE	LDA TIMER,x		; \
		ASL A			;  | set
		STA $4204		;  | head
		STZ $4205		;  | frame
		LDA #$09		;  |
		STA $4206		;  |
		LDA $4214		;  |
		ASL A			;  |
		AND #%00000110		;  |
		TAY			;  |
		LDA FRAMESLIST,y	;  |
		STA TMP1		;  |
		LDA FRAMESLIST+1,y	;  |
		STA TMP2		;  |
		LDY OFFSET,x		;  |
		LDA (TMP1),y		;  |
		STA FRAME,x		;  |
		LDA FBANKS,y		;  |
		STA FBANK,x		; /
		LDA TIMER,x		; \ don't advance to next
		BNE RETURNTRACK		; / status if not time
		LDA #$10		; \ time to
		STA TIMER,x		; / pause
		INC ACTSTATUS,x		; inc status to pre-fire pause
RETURNTRACK	RTS

;;;;;;;;;;;;;;;;

PREFIREPAUSE	LDA TIMER,x		; \  advance when
		BNE RETURNPFPAUSE	;  | the timer
		INC ACTSTATUS,x		; /  has run out
RETURNPFPAUSE	RTS

;;;;;;;;;;;;;;;;

TAKEAIM		LDA $14			; \  only process
		AND #%00000001		;  | every other
		BNE RETURNAIM		; /  frame
		LDY SHOTSFIRED,x	; \  get offset
		INY			;  | of current
		LDA (FIRINGADDR),y	; /  shot to fire
		LDY OFFSET,x		; \
		PHA			;  | flip
		LDA XDIRECTION,y	;  | offset if
		TAY			;  | necessary
		PLA			;  |
		CPY #$00		;  |
		BEQ POSITIVEOFFSET	;  |
		EOR #$FF		;  |
		CLC			;  |
		ADC #$3E		; /
POSITIVEOFFSET	PHA			; preserve goal offset
		JSR ROTATE		; rotate towards goal
		LDY OFFSET,x		; get new offset -> Y
		LDA FRAMES,y		; \
		STA FRAME,x		;  | set head
		LDA FBANKS,y		;  | frame
		STA FBANK,x		; /
		PLA			; load preserved goal offset
		CMP OFFSET,x		; \ don't fire shot
		BNE RETURNAIM		; / if not at goal
		LDA FRAMES4,y		; \
		STA FRAME,x		;  | set open mouth
		LDA FBANKS,y		;  | head frame
		STA FBANK,x		; /
		JSR SHOOT		; fire projectile
		LDA #SHOOTSND		; \ play
		STA $1DFC		; / SFX
		INC SHOTSFIRED,x	; another shot fired
		LDA #$10		; \ set open
		STA TIMER,x		; / mouth timer
		INC ACTSTATUS,x		; inc status to fire-pause 1
RETURNAIM	RTS

;;;;;;;;;;;;;;;;

FIREPAUSE1	LDA TIMER,x		; \ if not done, don't
		BNE RETURNFPAUSE1	; / close mouth
		LDY OFFSET,x		; \
		LDA FRAMES,y		;  | set
		STA FRAME,x		;  | closed
		LDA FBANKS,y		;  | mouth
		STA FBANK,x		; /
		LDA #$30		; \ set closed
		STA TIMER,x		; / mouth timer
		INC ACTSTATUS,x		; inc status to fire-pause 2
RETURNFPAUSE1	RTS

;;;;;;;;;;;;;;;;

FIREPAUSE2	LDA TIMER,x		; \ if not done,
		BNE RETURNFPAUSE2	; / don't move on
		LDA (FIRINGADDR)	; \  fire another shot
		CMP SHOTSFIRED,x	;  | if not all shots
		BCS SOMESHOTSLEFT	; /  have been fired

NOSHOTSLEFT	LDA #$70		; \ set tracking
		STA TIMER,x		; / timer
		STZ SHOTSFIRED,x	; reset shots fired
		STZ ACTSTATUS,x		; set status to tracking
RETURNFPAUSE2	RTS

SOMESHOTSLEFT	LDA #$02		; \ set status
		STA ACTSTATUS,x		; / to aiming
		RTS

;;;;;;;;;;;;;;;;

HIT		LDA HITDATA,x		; \  rotate
		AND #%01111111		;  | towards
		JSR ROTATE		; /  hit-goal
		LDY OFFSET,x		; \
		LDA HITDATA,x		;  | set
		AND #%10000000		;  | head
		BNE OPENMOUTH		;  | frame
CLOSEDMOUTH	LDA FRAMES,y		;  |
		STA FRAME,x		;  |
		LDA FBANKS,y		;  |
		STA FBANK,x		;  |
		BRA GETHITCONFIG	;  |
OPENMOUTH	LDA FRAMES4,y		;  |
		STA FRAME,x		;  |
		LDA FBANKS,y		;  |
		STA FBANK,x		; /
GETHITCONFIG	LDA (HITPALADDR)	; \  decide how to act
		CMP HITCOUNT,x		;  | depending on whether
		BCS TIMERFLASH		; /  this is the last shot
		LDA #$FF		; \ set sprite not to re-appear
		STA $161A,x		; / if it goes offscreen
		LDY HITCOUNT,x		; get hit-count, used for palette index
		BRA SKIPFLASH		; skip over code to flash using the timer
TIMERFLASH	LDA TIMER,x		; \
		LSR A			;  | get
		LSR A			;  | flash
		AND #%00000001		;  | index
		EOR #%00000001		; /
		CLC			; \ add hit
		ADC HITCOUNT,x		; / count
		TAY			; index -> Y
SKIPFLASH	LDA (HITPALADDR),y	; get palette #
		ASL A			; \ convert to
		AND #%00001110		; / properties
		STA HEADPALETTE,x	; set head palette
CHECKHITEND	LDA TIMER,x		; \ if timer expired,
		BNE RETURNHIT		; / then don't advance
		LDA #$10		; \ set hit-pause
		STA TIMER,x		; / timer
		INC ACTSTATUS,x		; inc status to hit pause
RETURNHIT	RTS

;;;;;;;;;;;;;;;;

HITPAUSE	LDA TIMER,x		; \ if not done,
		BNE RETURNHPAUSE	; / don't move on
		LDA (HITPALADDR)	; \  if this is the last hit,
		CMP HITCOUNT,x		;  | then begin shrinking, if
		BCC INITSHRINK		; /  not, go back to tracking
		STZ HITDATA,x		; reset hit data
		LDA #$70		; \ set tracking
		STA TIMER,x		; / timer
		STZ ACTSTATUS,x		; set tracking status
RETURNHPAUSE	RTS

INITSHRINK	LDA #$30		; \ set shrinking
		STA TIMER,x		; / timer
		INC ACTSTATUS,x		; inc status to shrinking
		RTS

;;;;;;;;;;;;;;;;

SHRINK		LDA HITDATA,x		; \
		AND #%10000000		;  | get 
		LSR A			;  | mouth
		LSR A			;  | state
		STA TMP1		;  | offset
		LSR A			;  |
		ORA TMP1		; /
		LDY TIMER,x		; timer -> Y
		CLC			; \ add frame
		ADC SHRINKFRAMES,y	; / offset
		STA FRAME,x		; set frame
		LDA #$01		; \ set frame
		STA FBANK,x		; / bank
		TYA			; \ 
		LSR A			;  | flash
		LSR A			;  | head
		AND #%00000001		;  | when
		BNE BLINKHEAD		;  | shrinking
NORMALHEAD	LDA (HITPALADDR)	;  |
		INC A			;  |
		TAY			;  |
		LDA (HITPALADDR),y	;  |
		ASL A			;  |
		AND #%00001110		;  |
		BRA SETHEADPALETTE	;  |
BLINKHEAD	LDA #FLASHPROP		;  |
SETHEADPALETTE	STA HEADPALETTE,x	; /
		LDA TIMER,x		; \ if not done,
		BNE RETURNSHRINK	; / don't move on
		LDA #$04                ; \ set sprite to
		STA $14C8,x             ; / spin-jump kill
		LDA #$1F                ; \ set spin jump
		STA $1540,x             ; / animation timer
		JSL SPINJUMPSTARS	; do star animation
RETURNSHRINK	RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		ROT_TEMP1 = $00

ROTATE		STA ROT_TEMP1		; temporarily store destination offset
		SEC			; \ minus current
		SBC OFFSET,x		; / frame offset
		BEQ ROTATEDONE		; if it's zero (they're equal) then don't rotate
		JSR WRAPOFFSET		; wrap offset at #$3E (means #$3E would be #$00)
		CMP #$1F		; \ CW (+) rotation if diff is less
		BCC INCROT		; / than #$1F, else CCW (-) rotation
DECROT		LDA OFFSET,x		; get offset
		DEC A			; subtract 1
		JSR WRAPOFFSET		; wrap at #$3E
		CMP ROT_TEMP1		; \ if result is at destination,
		BEQ SETOFFSET		; / avoid any extra rotation
		TAY			; \  do extra rotation if there is any
		SEC			;  | to do (this makes 0/180 degree
		SBC ROTATEDEC,y		; /  frames skip their mirror image)
		BRA FINISHOFFSET	; branch over CW rotation
INCROT		LDA OFFSET,x		; \ if result is at destination,
		INC A			; / avoid any extra rotation
		JSR WRAPOFFSET		; wrap at #$3E
		CMP ROT_TEMP1		; \ if result is at destination,
		BEQ SETOFFSET		; / avoid any extra rotation
		TAY			; \  do extra rotation
		CLC			;  | if there is any
		ADC ROTATEINC,y		; /  to do
FINISHOFFSET	JSR WRAPOFFSET		; wrap at #$3E
SETOFFSET	STA OFFSET,x		; set new offset
ROTATEDONE	RTS

WRAPOFFSET	BPL NOWRAP1		; \
		CLC			;  | wrap if negative
		ADC #$3E		; /
NOWRAP1		CMP #$3E		; \
		BCC NOWRAP2		;  | wrap if
		SEC			;  | >= #$3E
		SBC #$3E		; /
NOWRAP2		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		FC_TEMP1 = $00		; will use 2 bytes
		FC_TEMP2 = $02		; will use 2 bytes
		FC_TEMP3 = $04		; will use 2 bytes
		FC_TEMP4 = $06		; will use 2 bytes

CALCPTFULLFRAME	JSR CALCFRAME
		JSR CALCPTQUADFLIP
		JSR PTOOIEFRAMEWRAP
		RTS

PTOOIEFRAMEWRAP	CMP #$1C
		BCC NOPTWRAP
		CMP #$22
		BCS NOPTWRAP
		CMP #$1F
		BCS LEFTWRAP
		LDA #$1B
NOPTWRAP	RTS
LEFTWRAP	LDA #$22
		RTS

CALCPTQUADFLIP	PHA
		JSR SUB_VERT_POS
		TYA
		EOR EXTRA_BIT
		TAY
		PLA
		CPY #$00
		BNE CHKHORZ
		EOR #$FF
		CLC
		ADC #$1F
CHKHORZ		PHA
		JSR SUB_HORZ_POS
		PLA
		CPY #$00
		BEQ ENDQUADCHK
		EOR #$FF
		CLC
		ADC #$3E
ENDQUADCHK	RTS

CALCFRAME	LDA $D8,x
		SEC
		SBC $96
		STA FC_TEMP2
		LDA $14D4,x
		SBC $97
		STA FC_TEMP2+1
		BNE HORZDIST
		LDA FC_TEMP2
		BNE HORZDIST
		BRA SETHORZ
HORZDIST	LDA $E4,x
		SEC
		SBC $94
		STA FC_TEMP1
		LDA $14E0,x
		SBC $95
		STA FC_TEMP1+1
		BNE BEGINMATH
		LDA FC_TEMP1
		BNE BEGINMATH
		BRA SETVERT
BEGINMATH	PHP
		REP #%00100000
		LDA FC_TEMP2
		BPL CHKXDIST
		EOR #$FFFF
		INC A
		STA FC_TEMP2
CHKXDIST	LDA FC_TEMP1
		BPL MULT
		EOR #$FFFF
		INC A
		STA FC_TEMP1
MULT		ASL A
		ASL A
		ASL A
		ASL A
		STA FC_TEMP3
		LDA FC_TEMP1
		CLC
		ADC FC_TEMP2
		STA FC_TEMP4
		LDY #$00
		LDA FC_TEMP4
		DEC A
DIVLOOP		CMP FC_TEMP3
		BCS END_DIVIDE
		INY
		CLC
		ADC FC_TEMP4
		BRA DIVLOOP
END_DIVIDE	TYA
		PLP
		RTS
SETHORZ		LDA #$0F
		RTS
SETVERT		LDA #$00
		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

NOFREESLOTS	PLA
		;STA EXTRA_BIT
		RTS

SHOOT		LDA EXTRA_BIT		; \ back up extra
		PHA			; / bit scratch RAM
		JSL $02A9DE		; \ get slot or else
		BMI NOFREESLOTS		; / end if no slots
		PLA			; \ load backed up extra
		STA EXTRA_BIT		; / bit scratch RAM
		LDA #$01		; \ set sprite status
		STA $14C8,y		; / as a new sprite
		PHX			; back up X
		TYX			; new sprite index to X
		LDA #SPRITE_NUM		; \ store custom
		STA $7FAB9E,x		; / sprite number
		JSL $07F7D2		; reset sprite properties
		JSL $0187A7		; get table values for custom sprite
		LDA #$08		; \ mark as a
		STA $7FAB10,x		; / custom sprite
		PLX			; load backed up X
		LDA $E4,x		; \
		STA $E4,y		;  | set center
		LDA $14E0,x		;  | position for
		STA $14E0,y		;  | needlenose
		LDA $D8,x		;  | so that in next
		STA $D8,y		;  | code, it can
		LDA $14D4,x		;  | be shifted
		STA $14D4,y		; /
		LDA OFFSET,x		; \
		PHX			;  | X
		TAX			;  |
		LDA XCOORDS,x		;  |
		LDX #$00		;  |
		CMP #$00		;  |
		BPL ADDPROJXDISP	;  |
		DEX			;  |
ADDPROJXDISP	CLC			;  |
		ADC $E4,y		;  |
		STA $E4,y		;  |
		TXA			;  |
		ADC $14E0,y		;  |
		STA $14E0,y		;  |
		PLX			; /
		LDA OFFSET,x		; \
		PHX			;  | Y
		TAX			;  |
		LDA YCOORDS,x		;  |
		LDX EXTRA_BIT		;  |
		BEQ NOFLIPPROJY		;  |
		EOR #$FF		;  |
		INC A			;  |
NOFLIPPROJY	LDX #$00		;  |
		CMP #$00		;  |
		BPL ADDPROJYDISP	;  |
		DEX			;  |
ADDPROJYDISP	CLC			;  |
		ADC $D8,y		;  |
		STA $D8,y		;  |
		TXA			;  |
		ADC $14D4,y		;  |
		STA $14D4,y		;  |
		PLX			; /
		LDA OFFSET,x		; \
		PHX			;  | speeds
		TAX			;  |
		LDA XSPEEDS,x		;  |
		STA $B6,y		;  |
		LDA YSPEEDS,x		;  |
		LDX EXTRA_BIT		;  |
		BEQ NOFLIPPROJYS	;  |
		EOR #$FF		;  |
		INC A			;  |
NOFLIPPROJYS	STA $AA,y		;  |
		PLX			; /
		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HEADINTERACT	LDA $E4,x		; \
		PHA			;  | back up
		LDA $14E0,x		;  | sprite
		PHA			;  | positions
		LDA $D8,x		;  |
		PHA			;  |
		LDA $14D4,x		;  |
		PHA			; /
		LDY OFFSET,x		; \
		LDA XCOORDS,y		;  | X
		LDY #$00		;  |
		CMP #$00		;  |
		BPL ADDXDISP		;  |
		DEY			;  |
ADDXDISP	CLC			;  |
		ADC $E4,x		;  |
		STA $E4,x		;  |
		TYA			;  |
		ADC $14E0,x		;  |
		STA $14E0,x		; /
		LDY OFFSET,x		; \
		LDA YCOORDS,y		;  | Y
		LDY EXTRA_BIT		;  |
		BEQ NOTUPSIDEDOWN	;  |
		EOR #$FF		;  |
		INC A			;  |
NOTUPSIDEDOWN	LDY #$00		;  |
		CMP #$00		;  |
		BPL ADDYDISP		;  |
		DEY			;  |
ADDYDISP	CLC			;  |
		ADC $D8,x		;  |
		STA $D8,x		;  |
		TYA			;  |
		ADC $14D4,x		;  |
		STA $14D4,x		; /
		LDA $1662,x		; \
		PHA			;  | set head clipping
		AND #%11000000		;  | offset and backup
		ORA #$27		;  | that sprite table
		STA $1662,x		; /
		JSR INTERACTION		; Mario/Yoshi interaction
		JSR STOPFIREBALLS	; interact with fireballs
		JSR SHELLINTERACT	; interact with shells
		PLA			; \ load backed-up
		STA $1662,x		; / tweaker value $1662
		PLA			; \
		STA $14D4,x		;  | load backed up
		PLA			;  | sprite positions
		STA $D8,x		;  |
		PLA			;  |
		STA $14E0,x		;  |
		PLA			;  |
		STA $E4,x		; /
HEADINTERACTEND	RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SHELLINTERACT	LDY #$0C		; load number of times to go through loop
KO_LOOP		CPY #$00		; \ zero? if so,
		BEQ HEADINTERACTEND	; / end loop
		DEY			; decrease # of times left+get index
		LDA $14C8,y		; \  if sprite's status
		CMP #$09		;  | is less than 9 (9,A,B = shell modes)
		BCC KO_LOOP		; /  ignore sprite
		LDA $1686,y		; \  if sprite doesn't
		AND #%00001000		;  | interact with others
		BNE KO_LOOP		; /  don't continue
		JSL GETSPRITECLIPPINGA	; \
		PHX			;  | if sprite is
		TYX			;  | not touching
		JSL GETSPRITECLIPPINGB	;  | this sprite
		PLX			;  | don't continue
		JSL CHECKFORCONTACT	;  |
		BCC KO_LOOP		; /
		LDA $14C8,y		; \  speed doesn't matter
		CMP #$0B		;  | if Mario is holding
		BEQ GETHIT		; /  the shell (status=B)
		LDA $AA,y		; \ continue if sprite
		BNE GETHIT		; / has Y speed
		LDA $B6,y		; \ continue if sprite
		BNE GETHIT		; / has X speed
		BRA KO_LOOP		; no speed / not holding -> don't kill
GETHIT		LDA #$04		; \ give mario
		JSL $02ACE5		; / 1000 points
		LDA #KNOCKOUTSND	; \ play knockout
		STA $1DFC		; / sound
		LDA $9E,y		; \
		CMP #$53		;  | if throw block, don't do star animation
		BEQ NOSTARANI		; /
		LDA #$04                ; \ set sprite to
		STA $14C8,y             ; / spin-jump kill
		LDA #$1F                ; \ set spin jump
		STA $1540,y             ; / animation timer
		PHY			; \
		STY $15E9		;  | do star
		JSL $07FC3B		;  | animation
		STX $15E9		;  |
		PLY			; /
STARTKNOCKOUT	LDA $B6,y		; \
		BNE GETHIBIT		;  | get index
		LDA $E4,x		;  | for direction
		CMP $E4,y		;  | to turn
		LDA $14E0,x		;  | towards
		SBC $14E0,y		;  |
GETHIBIT	ROL A			;  |
		ROL A			;  |
		AND #%00000001		;  |
		TAY			; /
		LDA HITDATA,x		; \  set data for
		AND #%10000000		;  | how to act when
		ORA HITOFFSETS,y	;  | hit with shell
		LDY ACTSTATUS,x		;  | %abbbbbbb:
		ORA STUNMOUTHSTATE,y	;  | a=mouth closed/open
		STA HITDATA,x		; /  b=offset
		STZ SHOTSFIRED,x	; reset shots fired, in case shell was thrown between shots
		INC HITCOUNT,x		; another hit was taken
		LDA #$20		; \ set
		STA TIMER,x		; / timer
		LDA #$05		; \ set hit
		STA ACTSTATUS,x		; / status
		LDA (HITPALADDR)	; \  disable interaction
		CMP HITCOUNT,x		;  | if this is the last
		BCS ENDSHELLLOOP	; /  hit before it dies
		LDA $167A,x		; \
		ORA #%00000010		;  | consider sprite not to be an enemy
		STA $167A,x		; /
ENDSHELLLOOP	RTS

NOSTARANI	LDA $1656,y		; \  force shell
		ORA #%10000000		;  | to disappear
		STA $1656,y		; /  in smoke
		LDA #$02		; \ set shell into
		STA $14C8,y		; / death mode (status=2)
		BRA STARTKNOCKOUT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

INTERACTION	LDA $77			; \
		AND #%00000100		;  | if on ground, always do Yoshi check
		BNE YOSHI_CHECK		; /
		LDA $7D			; \ if moving downward
		BPL MARIO_INTERACT	; / skip Yoshi check
YOSHI_CHECK	LDA $187A		; \ yoshi interact
		BNE YOSHI_INTERACT	; / if riding Yoshi
MARIO_INTERACT	JSL MARIOSPRINTERACT	; normal interact with mario
		RTS
YOSHI_INTERACT	LDA $1490		; \ Mario interaction
		BNE MARIO_INTERACT	; / if Mario has star
		LDA $154C,x		; \ don't interact if disable
		BNE END_INTERACT	; / interaction timer is set
		LDA $167A,x		; \
		PHA			;  | set "no default interaction"
		ORA #%10000000		;  | flag temporarily and back-up
		STA $167A,x		; /
		JSL MARIOSPRINTERACT	; detect if mario touching
		PLA			; \ load backed up
		STA $167A,x		; / interact flag
		BCC END_INTERACT	; if mario is not touching, don't lose yoshi
		JSR LOSEYOSHI		; else lose yoshi
END_INTERACT	RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LOSEYOSHI	PHX
		LDX $18E2
		LDA #$10
		STA $163D,x
		LDA #$03
		STA $1DFA
		LDA #$13
		STA $1DFC
		LDA #$02
		STA $C1,x
		STZ $187A
		STZ $0DC1
		LDA #$C0
		STA $7D
		STZ $7B
		LDY $157B,x
		PHX
		TYX
		LDA $02A4B3,x
		PLX
		STA $B5,x
		STZ $1593,x
		STZ $151B,x
		STZ $18AE
		LDA #$30
		STA $1497
		PLX
		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

STOPFIREBALLS	LDY #$09		; index of first fireball
FB_LOOP_BEGIN	LDA $170B,y		; \
		CMP #$05		;  | ignore if not fireball
		BNE FB_LOOP		; /
		JSL GETSPRITECLIPPINGA	; \
		LDA $171F,y		;  | ignore
		SEC			;  | if not
		SBC #$02		;  | touching
		STA $00			;  | sprite
		LDA $1733,y		;  |
		SBC #$00		;  |
		STA $08			;  |
		LDA #$0C		;  |
		STA $02			;  |
		LDA $1715,y		;  |
		SEC			;  |
		SBC #$04		;  |
		STA $01			;  |
		LDA $1729,y		;  |
		SBC #$00		;  |
		STA $09			;  |
		LDA #$13		;  |
		STA $03			;  |
		JSL CHECKFORCONTACT	;  |
		BCC FB_LOOP		; /
		LDA #$0F		; \
		STA $176F,y		;  | turn fireball
		LDA #$01		;  | into smoke
		STA $170B,y		; /
		LDA #$01		; \ play
		STA $1DF9		; / SFX
FB_LOOP		DEY			; \
		CMP #$08		;  | loop if not reached end
		BCS FB_LOOP_BEGIN	; /
		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GRAPHICS ROUTINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		GFXTMP_HORIZFLIP = $02
		GFXTMP_TILECOUNT = $03
		GFXTMP_DSLOTTILE = $04
		GFXTMP_XPOSITION = $05
		GFXTMP_YPOSITION = $06
		GFXTMP_FRAMEBANK = $07

		STEMTILE = $69

HEAD_TILES	dcb $00,$02,$20,$22
HEAD_XPOS	dcb $F8,$08,$F8,$08
HEAD_YPOS	dcb $F8,$F8,$08,$08

SUB_GFX		JSR GET_DRAW_INFO
		LDA #$FF		; \ zero tiles
		STA GFXTMP_TILECOUNT	; / drawn
		PHY			; \
		LDY OFFSET,x		;  | set X
		LDA XDIRECTION,y	;  | flip
		STA GFXTMP_HORIZFLIP	;  |
		PLY			; /
		JSR DRAW_HEAD		; draw head
		JSR DRAW_BASE		; draw base
		LDY #$02                ; #$02 means the tiles are 16x16
		LDA GFXTMP_TILECOUNT	; # of tiles drawn -1
		;BMI RETURN_SUB_GFX	; return if no tiles drawn (commented out because there will never be zero tiles drawn)
		JSL FINISHOAMWRITE	; don't draw if offscreen, set sizes
RETURN_SUB_GFX	RTS

DRAW_HEAD	PHY			; \
		LDY OFFSET,x		;  | head
		LDA XCOORDS,y		;  | coords
		STA GFXTMP_XPOSITION	;  | to
		LDA YCOORDS,y		;  | scratch
		STA GFXTMP_YPOSITION	;  | RAM
		PLY			; /
		LDA FBANK,x		; \ set frame bank (will be used
		STA GFXTMP_FRAMEBANK	; / in modified dynamic routine)
		LDA FRAME,x		; \ reserve dynamic
		JSR GETSLOT		; / sprite slot
		BEQ RETURN_SUB_GFX	; if none available, return
		STA GFXTMP_DSLOTTILE	; store dynamic sprite slot to scratch RAM
		PHX			; back up sprite index
		LDX #$00		; load X with zero
HEADLOOP	CPX #$04		; \ if end of loop,
		BCS ENDHEADLOOP		; / branch to end
		LDA HEAD_XPOS,x		; \
		PHX			;  | X
		LDX GFXTMP_HORIZFLIP	;  |
		BNE HEADNOFLIPX		;  |
		EOR #$FF		;  |
		INC A			;  |
HEADNOFLIPX	PLX			;  |
		CLC			;  |
		ADC GFXTMP_XPOSITION	;  |
		CLC			;  |
		ADC $00			;  |
		STA $0300,y		; /
		LDA HEAD_YPOS,x		; \
		CLC			;  | Y
		ADC GFXTMP_YPOSITION	;  |
		PHX			;  |
		LDX EXTRA_BIT		;  |
		BEQ HEADNOFLIPY		;  |
		EOR #$FF		;  |
		INC A			;  |
HEADNOFLIPY	PLX			;  |
		CLC			;  |
		ADC $01			;  |
		STA $0301,y		; /
		LDA HEAD_TILES,x	; \
		CLC			;  | set tile
		ADC GFXTMP_DSLOTTILE	;  | number
		STA $0302,y		; /
		PHX			; \
		LDX $15E9		;  | sprite
		LDA $15F6,x		;  | props
		AND #%11110001		;  |
		ORA HEADPALETTE,x	;  |
		LDX GFXTMP_HORIZFLIP	;  |
		BNE HEADNOFLIPXT	;  |
		ORA #%01000000		;  |
HEADNOFLIPXT	;PLX			;  |
		;PHX			;  |
		LDX EXTRA_BIT		;  |
		BEQ HEADNOFLIPYT	;  |
		ORA #%10000000		;  |
HEADNOFLIPYT	PLX			;  |
		ORA $64			;  |
		STA $0303,y		; /
		INY			; \
		INY			;  | next OAM
		INY			;  | index
		INY			; /
		INX			; next tile
		INC GFXTMP_TILECOUNT	; another tile was drawn
		BRA HEADLOOP		; loop
ENDHEADLOOP	PLX			; load backed up X
RETURNHEAD	RTS

DRAW_BASE	LDA $00			; \ X
		STA $0300,y		; /
		LDA $01			; \ Y
		STA $0301,y		; /
		LDA #STEMTILE		; \ set tile
		STA $0302,y		; / number
		LDA $15F6,x		; \
		PHX			;  | set
		LDX GFXTMP_HORIZFLIP	;  | sprite
		BNE BASENOFLIPXT	;  | props
		ORA #%01000000		;  |
BASENOFLIPXT	;PLX			;  |
		;PHX			;  |
		LDX EXTRA_BIT		;  |
		BEQ BASENOFLIPYT	;  |
		ORA #%10000000		;  |
BASENOFLIPYT	PLX			;  |
		ORA $64			;  |
		STA $0303,y		; /
		INY			; \
		INY			;  | next OAM
		INY			;  | index
		INY			; /
		INC GFXTMP_TILECOUNT	; another tile was drawn
		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Dynamic sprite routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TEMP = $09

SLOTPTR = $0660		;16bit pointer for source GFX
SLOTBANK = $0662	;bank
SLOTDEST = $0663	;VRAM address
SLOTSUSED = $06FE	;how many slots have been used

MAXSLOTS = $04		;maximum selected slots

SLOTS dcb $CC,$C8,$C4,$C0	;avaliable slots.  Any more transfers and it's overflowing by a dangerous amount.

GETSLOT
	PHY		;preserve OAM index
	PHA		;preserve frame
	LDA SLOTSUSED	;test if slotsused == maximum allowed
	CMP #MAXSLOTS
	BEQ NoneFree

	PLA		;pop frame
	REP #$20	;16bit A
	AND.w #$00FF	;wipe high
	XBA		;<< 8
	LSR A		;>> 1 = << 7
	STA TEMP	;back to scratch
	LDA.w #GFXADDR&$FFFF
	CLC
	ADC TEMP	;add frame offset	
	STA SLOTPTR	;store to pointer to be used at transfer time
	SEP #$20	;8bit store
	LDA #GFXADDR/$10000
	CLC
	ADC GFXTMP_FRAMEBANK
	STA SLOTBANK	;store bank to 24bit pointer

	LDY SLOTSUSED	;calculate VRAM address + tile number
	LDA SLOTS,y	;get tile# in VRAM
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
	CLC
	ADC #$0B44	;add 0B44, base address of buffer	
	STA SLOTDEST	;destination address in the buffer

	JSR DMABUFFER	;ROM -> RAM copy

	SEP #$20	;8bit A	
	INC SLOTSUSED	;one extra slot has been used

	PLA		;return starting tile number
	PLY
	RTS
		
NoneFree
	PLA
	PLY
	LDA #$00	;zero on no free slots
	RTS

;;;;;;;;;;;;;;;;
;Transfer routine
;;;;;;;;;;;;;;;;

;DMA ROM -> RAM ROUTINE

DMABUFFER
;set destination RAM address
	REP #$20
	LDA SLOTDEST
	STA $2181	;16bit RAM dest
	LDY #$7F
	STY $2183	;set 7F as bank

;common DMA settings
	STZ $4300	;1 reg only
	LDY #$80	;to 2180, RAM write/read
	STY $4301

;first line
	LDA SLOTPTR
	STA $4302	;low 16bits
	LDY SLOTBANK
	STY $4304	;bank
	LDY #$80	;128 bytes
	STY $4305
	LDY #$01
	STY $420B	;transfer

;second line
	LDA SLOTDEST	;update buffer dest
	CLC
	ADC #$0200	;512 byte rule for sprites
	STA SLOTDEST	;updated base
	STA $2181	;updated RAM address

	LDA SLOTPTR	;update source address
	CLC
	ADC #$0200	;512 bytes, next row
	STA SLOTPTR
	STA $4302	;low 16bits
	LDY SLOTBANK
	STY $4304	;bank
	LDY #$80
	STY $4305
	LDY #$01
	STY $420B	;transfer

;third line
	LDA SLOTDEST	;update buffer dest
	CLC
	ADC #$0200	;512 byte rule for sprites
	STA SLOTDEST	;updated base
	STA $2181	;updated RAM address

	LDA SLOTPTR	;update
	CLC
	ADC #$0200
	STA SLOTPTR
	STA $4302
	LDY SLOTBANK
	STY $4304
	LDY #$80
	STY $4305
	LDY #$01
	STY $420B	;transfer

;fourth line
	LDA SLOTDEST	;update buffer dest
	CLC
	ADC #$0200	;512 byte rule for sprites
	STA SLOTDEST	;updated base
	STA $2181	;updated RAM address

	LDA SLOTPTR
	CLC
	ADC #$0200
	STA SLOTPTR
	STA $4302
	LDY SLOTBANK
	STY $4304
	LDY #$80
	STY $4305
	LDY #$01
	STY $420B

	SEP #$20	;8bit A
	RTS		;all done, return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GET_DRAW_INFO
; This is a helper for the graphics routine.  It sets off screen flags, and sets up
; variables.  It will return with the following:
;
;       Y = index to sprite OAM ($300)
;       $00 = sprite x position relative to screen boarder
;       $01 = sprite y position relative to screen boarder  
;
; It is adapted from the subroutine at $03B760
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SPR_T1              dcb $0C,$1C
SPR_T2              dcb $01,$02

GET_DRAW_INFO       STZ $186C,x             ; reset sprite offscreen flag, vertical
                    STZ $15A0,x             ; reset sprite offscreen flag, horizontal
                    LDA $E4,x               ; \
                    CMP $1A                 ;  | set horizontal offscreen if necessary
                    LDA $14E0,x             ;  |
                    SBC $1B                 ;  |
                    BEQ ON_SCREEN_X         ;  |
                    INC $15A0,x             ; /

ON_SCREEN_X         LDA $14E0,x             ; \
                    XBA                     ;  |
                    LDA $E4,x               ;  |
                    REP #$20                ;  |
                    SEC                     ;  |
                    SBC $1A                 ;  | mark sprite invalid if far enough off screen
                    CLC                     ;  |
                    ADC.W #$0040            ;  |
                    CMP.W #$0180            ;  |
                    SEP #$20                ;  |
                    ROL A                   ;  |
                    AND #$01                ;  |
                    STA $15C4,x             ; / 
                    BNE INVALID             ; 
                    
                    LDY #$00                ; \ set up loop:
                    LDA $1662,x             ;  | 
                    AND #$20                ;  | if not smushed (1662 & 0x20), go through loop twice
                    BEQ ON_SCREEN_LOOP      ;  | else, go through loop once
                    INY                     ; / 
ON_SCREEN_LOOP      LDA $D8,x               ; \ 
                    CLC                     ;  | set vertical offscreen if necessary
                    ADC SPR_T1,y            ;  |
                    PHP                     ;  |
                    CMP $1C                 ;  | (vert screen boundry)
                    ROL $00                 ;  |
                    PLP                     ;  |
                    LDA $14D4,x             ;  | 
                    ADC #$00                ;  |
                    LSR $00                 ;  |
                    SBC $1D                 ;  |
                    BEQ ON_SCREEN_Y         ;  |
                    LDA $186C,x             ;  | (vert offscreen)
                    ORA SPR_T2,y            ;  |
                    STA $186C,x             ;  |
ON_SCREEN_Y         DEY                     ;  |
                    BPL ON_SCREEN_LOOP      ; /

                    LDY $15EA,x             ; get offset to sprite OAM
                    LDA $E4,x               ; \ 
                    SEC                     ;  | 
                    SBC $1A                 ;  | $00 = sprite x position relative to screen boarder
                    STA $00                 ; / 
                    LDA $D8,x               ; \ 
                    SEC                     ;  | 
                    SBC $1C                 ;  | $01 = sprite y position relative to screen boarder
                    STA $01                 ; / 
                    RTS                     ; return

INVALID             PLA                     ; \ return from *main gfx routine* subroutine...
                    PLA                     ;  |    ...(not just this subroutine)
                    RTS                     ; /


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUB_OFF_SCREEN
; This subroutine deals with sprites that have moved off screen
; It is adapted from the subroutine at $01AC0D
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    
SPR_T12             dcb $40,$B0
SPR_T13             dcb $01,$FF
SPR_T14             dcb $30,$C0,$A0,$C0,$A0,$F0,$60,$90		;bank 1 sizes
		            dcb $30,$C0,$A0,$80,$A0,$40,$60,$B0		;bank 3 sizes
SPR_T15             dcb $01,$FF,$01,$FF,$01,$FF,$01,$FF		;bank 1 sizes
					dcb $01,$FF,$01,$FF,$01,$00,$01,$FF		;bank 3 sizes

SUB_OFF_SCREEN_X1   LDA #$02                ; \ entry point of routine determines value of $03
                    BRA STORE_03            ;  | (table entry to use on horizontal levels)
SUB_OFF_SCREEN_X2   LDA #$04                ;  | 
                    BRA STORE_03            ;  |
SUB_OFF_SCREEN_X3   LDA #$06                ;  |
                    BRA STORE_03            ;  |
SUB_OFF_SCREEN_X4   LDA #$08                ;  |
                    BRA STORE_03            ;  |
SUB_OFF_SCREEN_X5   LDA #$0A                ;  |
                    BRA STORE_03            ;  |
SUB_OFF_SCREEN_X6   LDA #$0C                ;  |
                    BRA STORE_03            ;  |
SUB_OFF_SCREEN_X7   LDA #$0E                ;  |
STORE_03			STA $03					;  |            
					BRA START_SUB			;  |
SUB_OFF_SCREEN_X0   STZ $03					; /

START_SUB           JSR SUB_IS_OFF_SCREEN   ; \ if sprite is not off screen, return
                    BEQ RETURN_35           ; /
                    LDA $5B                 ; \  goto VERTICAL_LEVEL if vertical level
                    AND #$01                ; |
                    BNE VERTICAL_LEVEL      ; /     
                    LDA $D8,x               ; \
                    CLC                     ; | 
                    ADC #$50                ; | if the sprite has gone off the bottom of the level...
                    LDA $14D4,x             ; | (if adding 0x50 to the sprite y position would make the high byte >= 2)
                    ADC #$00                ; | 
                    CMP #$02                ; | 
                    BPL ERASE_SPRITE        ; /    ...erase the sprite
                    LDA $167A,x             ; \ if "process offscreen" flag is set, return
                    AND #$04                ; |
                    BNE RETURN_35           ; /
                    LDA $13                 ;A:8A00 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdiZcHC:0756 VC:176 00 FL:205
                    AND #$01                ;A:8A01 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizcHC:0780 VC:176 00 FL:205
                    ORA $03                 ;A:8A01 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizcHC:0796 VC:176 00 FL:205
                    STA $01                 ;A:8A01 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizcHC:0820 VC:176 00 FL:205
                    TAY                     ;A:8A01 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizcHC:0844 VC:176 00 FL:205
                    LDA $1A                 ;A:8A01 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizcHC:0858 VC:176 00 FL:205
                    CLC                     ;A:8A00 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdiZcHC:0882 VC:176 00 FL:205
                    ADC SPR_T14,y           ;A:8A00 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdiZcHC:0896 VC:176 00 FL:205
                    ROL $00                 ;A:8AC0 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:eNvMXdizcHC:0928 VC:176 00 FL:205
                    CMP $E4,x               ;A:8AC0 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:eNvMXdizCHC:0966 VC:176 00 FL:205
                    PHP                     ;A:8AC0 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizCHC:0996 VC:176 00 FL:205
                    LDA $1B                 ;A:8AC0 X:0009 Y:0001 D:0000 DB:01 S:01F0 P:envMXdizCHC:1018 VC:176 00 FL:205
                    LSR $00                 ;A:8A00 X:0009 Y:0001 D:0000 DB:01 S:01F0 P:envMXdiZCHC:1042 VC:176 00 FL:205
                    ADC SPR_T15,y           ;A:8A00 X:0009 Y:0001 D:0000 DB:01 S:01F0 P:envMXdizcHC:1080 VC:176 00 FL:205
                    PLP                     ;A:8AFF X:0009 Y:0001 D:0000 DB:01 S:01F0 P:eNvMXdizcHC:1112 VC:176 00 FL:205
                    SBC $14E0,x             ;A:8AFF X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizCHC:1140 VC:176 00 FL:205
                    STA $00                 ;A:8AFF X:0009 Y:0001 D:0000 DB:01 S:01F1 P:eNvMXdizCHC:1172 VC:176 00 FL:205
                    LSR $01                 ;A:8AFF X:0009 Y:0001 D:0000 DB:01 S:01F1 P:eNvMXdizCHC:1196 VC:176 00 FL:205
                    BCC SPR_L31             ;A:8AFF X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdiZCHC:1234 VC:176 00 FL:205
                    EOR #$80                ;A:8AFF X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdiZCHC:1250 VC:176 00 FL:205
                    STA $00                 ;A:8A7F X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizCHC:1266 VC:176 00 FL:205
SPR_L31             LDA $00                 ;A:8A7F X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizCHC:1290 VC:176 00 FL:205
                    BPL RETURN_35           ;A:8A7F X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizCHC:1314 VC:176 00 FL:205
ERASE_SPRITE        LDA $14C8,x             ; \ if sprite status < 8, permanently erase sprite
                    CMP #$08                ; |
                    BCC KILL_SPRITE         ; /    
                    LDY $161A,x             ;A:FF08 X:0007 Y:0001 D:0000 DB:01 S:01F3 P:envMXdiZCHC:1108 VC:059 00 FL:2878
                    CPY #$FF                ;A:FF08 X:0007 Y:0000 D:0000 DB:01 S:01F3 P:envMXdiZCHC:1140 VC:059 00 FL:2878
                    BEQ KILL_SPRITE         ;A:FF08 X:0007 Y:0000 D:0000 DB:01 S:01F3 P:envMXdizcHC:1156 VC:059 00 FL:2878
                    LDA #$00                ;A:FF08 X:0007 Y:0000 D:0000 DB:01 S:01F3 P:envMXdizcHC:1172 VC:059 00 FL:2878
                    STA $1938,y             ;A:FF00 X:0007 Y:0000 D:0000 DB:01 S:01F3 P:envMXdiZcHC:1188 VC:059 00 FL:2878
KILL_SPRITE         STZ $14C8,x             ; erase sprite
RETURN_35           RTS                     ; return

VERTICAL_LEVEL      LDA $167A,x             ; \ if "process offscreen" flag is set, return
                    AND #$04                ; |
                    BNE RETURN_35           ; /
                    LDA $13                 ; \
                    LSR A                   ; | 
                    BCS RETURN_35           ; /
                    LDA $E4,x               ; \ 
                    CMP #$00                ;  | if the sprite has gone off the side of the level...
                    LDA $14E0,x             ;  |
                    SBC #$00                ;  |
                    CMP #$02                ;  |
                    BCS ERASE_SPRITE        ; /  ...erase the sprite
                    LDA $13                 ;A:0000 X:0009 Y:00E4 D:0000 DB:01 S:01F3 P:eNvMXdizcHC:1218 VC:250 00 FL:5379
                    LSR A                   ;A:0016 X:0009 Y:00E4 D:0000 DB:01 S:01F3 P:envMXdizcHC:1242 VC:250 00 FL:5379
                    AND #$01                ;A:000B X:0009 Y:00E4 D:0000 DB:01 S:01F3 P:envMXdizcHC:1256 VC:250 00 FL:5379
                    STA $01                 ;A:0001 X:0009 Y:00E4 D:0000 DB:01 S:01F3 P:envMXdizcHC:1272 VC:250 00 FL:5379
                    TAY                     ;A:0001 X:0009 Y:00E4 D:0000 DB:01 S:01F3 P:envMXdizcHC:1296 VC:250 00 FL:5379
                    LDA $1C                 ;A:001A X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNvMXdizcHC:0052 VC:251 00 FL:5379
                    CLC                     ;A:00BD X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNvMXdizcHC:0076 VC:251 00 FL:5379
                    ADC SPR_T12,y           ;A:00BD X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNvMXdizcHC:0090 VC:251 00 FL:5379
                    ROL $00                 ;A:006D X:0009 Y:0001 D:0000 DB:01 S:01F3 P:enVMXdizCHC:0122 VC:251 00 FL:5379
                    CMP $D8,x               ;A:006D X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNVMXdizcHC:0160 VC:251 00 FL:5379
                    PHP                     ;A:006D X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNVMXdizcHC:0190 VC:251 00 FL:5379
                    LDA.W $001D             ;A:006D X:0009 Y:0001 D:0000 DB:01 S:01F2 P:eNVMXdizcHC:0212 VC:251 00 FL:5379
                    LSR $00                 ;A:0000 X:0009 Y:0001 D:0000 DB:01 S:01F2 P:enVMXdiZcHC:0244 VC:251 00 FL:5379
                    ADC SPR_T13,y           ;A:0000 X:0009 Y:0001 D:0000 DB:01 S:01F2 P:enVMXdizCHC:0282 VC:251 00 FL:5379
                    PLP                     ;A:0000 X:0009 Y:0001 D:0000 DB:01 S:01F2 P:envMXdiZCHC:0314 VC:251 00 FL:5379
                    SBC $14D4,x             ;A:0000 X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNVMXdizcHC:0342 VC:251 00 FL:5379
                    STA $00                 ;A:00FF X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNvMXdizcHC:0374 VC:251 00 FL:5379
                    LDY $01                 ;A:00FF X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNvMXdizcHC:0398 VC:251 00 FL:5379
                    BEQ SPR_L38             ;A:00FF X:0009 Y:0001 D:0000 DB:01 S:01F3 P:envMXdizcHC:0422 VC:251 00 FL:5379
                    EOR #$80                ;A:00FF X:0009 Y:0001 D:0000 DB:01 S:01F3 P:envMXdizcHC:0438 VC:251 00 FL:5379
                    STA $00                 ;A:007F X:0009 Y:0001 D:0000 DB:01 S:01F3 P:envMXdizcHC:0454 VC:251 00 FL:5379
SPR_L38             LDA $00                 ;A:007F X:0009 Y:0001 D:0000 DB:01 S:01F3 P:envMXdizcHC:0478 VC:251 00 FL:5379
                    BPL RETURN_35           ;A:007F X:0009 Y:0001 D:0000 DB:01 S:01F3 P:envMXdizcHC:0502 VC:251 00 FL:5379
                    BMI ERASE_SPRITE        ;A:8AFF X:0002 Y:0000 D:0000 DB:01 S:01F3 P:eNvMXdizcHC:0704 VC:184 00 FL:5490

SUB_IS_OFF_SCREEN   LDA $15A0,x             ; \ if sprite is on screen, accumulator = 0 
                    ORA $186C,x             ; |  
                    RTS                     ; / return


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUB_HORZ_POS
; This routine determines which side of the sprite Mario is on.  It sets the Y register
; to the direction such that the sprite would face Mario
; It is ripped from $03B817
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SUB_HORZ_POS		LDY #$00				;A:25D0 X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizCHC:1020 VC:097 00 FL:31642
					LDA $94					;A:25D0 X:0006 Y:0000 D:0000 DB:03 S:01ED P:envMXdiZCHC:1036 VC:097 00 FL:31642
					SEC                     ;A:25F0 X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizCHC:1060 VC:097 00 FL:31642
					SBC $E4,x				;A:25F0 X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizCHC:1074 VC:097 00 FL:31642
					;STA $0F					;A:25F4 X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizcHC:1104 VC:097 00 FL:31642
					LDA $95					;A:25F4 X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizcHC:1128 VC:097 00 FL:31642
					SBC $14E0,x				;A:2500 X:0006 Y:0000 D:0000 DB:03 S:01ED P:envMXdiZcHC:1152 VC:097 00 FL:31642
					BPL SPR_L16             ;A:25FF X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizcHC:1184 VC:097 00 FL:31642
					INY                     ;A:25FF X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizcHC:1200 VC:097 00 FL:31642
SPR_L16				RTS                     ;A:25FF X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:1214 VC:097 00 FL:31642


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUB_VERT_POS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SUB_VERT_POS		LDY #$00				;A:25D0 X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizCHC:1020 VC:097 00 FL:31642
					LDA $96					;A:25D0 X:0006 Y:0000 D:0000 DB:03 S:01ED P:envMXdiZCHC:1036 VC:097 00 FL:31642
					SEC                     ;A:25F0 X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizCHC:1060 VC:097 00 FL:31642
					SBC $D8,x				;A:25F0 X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizCHC:1074 VC:097 00 FL:31642
					;STA $0F					;A:25F4 X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizcHC:1104 VC:097 00 FL:31642
					LDA $97					;A:25F4 X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizcHC:1128 VC:097 00 FL:31642
					SBC $14D4,x				;A:2500 X:0006 Y:0000 D:0000 DB:03 S:01ED P:envMXdiZcHC:1152 VC:097 00 FL:31642
					BPL SPR_L17             ;A:25FF X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizcHC:1184 VC:097 00 FL:31642
					INY                     ;A:25FF X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizcHC:1200 VC:097 00 FL:31642
SPR_L17				RTS                     ;A:25FF X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:1214 VC:097 00 FL:31642