; Extra byte $00 is piranha
; Extra byte $80 is ptooie
; Use extra byte with ptooie instead of property byte:
;  Extra byte $81 is ptooie with SHOTS2 and HITS2


prot gfx

!yi_combined = 1
; don't namespace this so %SubHorzPos() et al isn't in a namespace
incsrc yiptooiev3.asm

namespace piranha
incsrc yipiranhav3.asm
namespace off

PRINT "INIT ",pc
combined_init:
    LDA !extra_byte_1,x
    BMI .ptooie

    JML piranha_init
.ptooie
    AND.b #$FF-$80
    STA !extra_prop_1,x
    JML init

PRINT "MAIN ",pc
combined_main:
    LDA !extra_byte_1,x
    BMI .ptooie

    JML piranha_main
.ptooie
    JML main

incbin piranhagfx.bin -> gfx
