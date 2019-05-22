.var bank=$8000

.macro SetVICBank0() {
	lda $DD00
	and #%11111100
	ora #%00000011
	sta $DD00
}

.macro SetVICBank1() {
	lda $DD00
	and #%11111100
	ora #%00000010
	sta $DD00
}

.macro SetVICBank2() {    
    .eval bank=$8000

	lda $DD00
	and #%11111100
	ora #%00000001
	sta $DD00
}

.macro SetVICBank3() {
	.eval bank=$c000
	lda $DD00
	and #%11111100
	ora #%00000000
	sta $DD00
}


.macro LoadSpriteFromPicture(filename) {
	.var picture = LoadPicture(filename, List().add($000000, $ffffff,$6c6c6c,$959595))
	.for (var y=0; y<21; y++)
		.for (var x=0; x<3; x++)
			.byte picture.getMulticolorByte(x,y) 
	.byte 0
}

.macro add16(mem,value) {	
	lda mem
    clc
    adc #<value
    sta mem
    
    lda DEST+1
    adc #>value
    sta DEST+1
}

.macro add8(mem,value) {	
	lda mem
    clc
    adc value
    sta mem
    
}
