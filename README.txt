patch dsx.asm (in the patch folder) to your ROM with xkas if you have not already done that with other sprites.

put these files in your sprites directory in sprite tool:
yipiranhav3.asm
yipiranhav3.cfg
yiptooiev3.asm
yiptooiev3.cfg
**piranhagfx.bin requires manual insertion... read below for details

insert the palette into your level

Feel free to report any glitches to me, SMWEdit, at SMWCentral (or wherever)

----------------------------------------------------------------------

INSTRUCTIONS FOR MANUAL GFX INSERTION FOR V3 PIRANHA GFX

the reason for this is because the total size of the sprite and the GFX is greater than one ROM bank's size, and data can't usually cross ROM banks

To insert the GFX, you will need:

- Translhextion
- Lunar Address

Steps to inserting GFX:

1. BACK UP YOUR ROM!!! You will always want an up-to-date copy if you corrupt it accidentally

2. Open your SMW ROM in translhextion

3. Keep scrolling down until you find a large blank area full of zeros.

4. Look for an address that ends in "0200" or "8200" (these are beginnings of ROM banks) ...or you can overwrite the old blowhard GFX. Also, make sure there are at least 8 zeros before that, for the RATS tag. (unless the space is used by the old RATS tag)

5. To make sure there is enough space, use the select block feature to select an area that is 0xF800 (63488) bytes long. To do this, go to Selection > Select Block, click "number of bytes to select", and type the decimal value of the space required. (in this case, it's 63488, which is 1 and 15/16 ROM banks)

6. If there are only zeros in the space, go to the next step, otherwise repeat from step 3 to find another space.

7. Go back to the address at the start and write it down (the PC offset)

8. Open "blowhardgfx.bin" in another translhextion window and copy/paste the data into the starting address in the SMW ROM's blank space you found. When the paste dialog box comes up, make sure you select "overwrite" and NOT "insert".

9. go back 8 bytes before your starting address and type 53 54 41 52 00 F8 FF 07 for your RATS tag

10. Open Lunar Address and select "LoRom - PC". If it doesn't automatically say PC and SNES under the text boxes at the bottom of the window, clear out one or both until it shows the labels so you know which is which.

11. Type the first address you wrote down into Lunar Address where it says PC and write down the address it says in "SNES".

12. Open blowhard.asm in notepad and search for "GFXADDR", it should be near the top.

    When you find it, you will see something like this:
		GFXADDR = $XXXXXX

13. Go back to the last address you wrote down (the converted address), it should look something like this: $12:3456.

14. For the converted address, enter it in like this:
		GFXADDR = $123456

15. save the ASM file and insert the sprite (yiblowhardv2.asm, yiblowhardv2.cfg) like you would any sprite.