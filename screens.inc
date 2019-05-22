objs_0:
	.byte $08,$02
	.byte $0d
	.byte $2c,$2d,$2e,$2f,$30,$31,$32,$33
	.byte $34,$35,$36,$37,$38,$39,$3a,$3b
objs_1:
	.byte $01,$02
	.byte $0d
	.byte $3d,$3c
objs_2:
	.byte $04,$01
	.byte $05
	.byte $03,$09,$01,$0f
objs_3:
	.byte $28,$02
	.byte $0d
	.byte $2c,$2d,$2e,$2f,$30,$31,$32,$2d
	.byte $2e,$2f,$30,$31,$32,$2d,$2e,$2f
	.byte $30,$31,$32,$2d,$2e,$2f,$30,$31
	.byte $32,$2d,$2e,$2f,$30,$31,$32,$2d
	.byte $2e,$2f,$30,$31,$32,$31,$32,$33
	.byte $34,$35,$36,$37,$38,$39,$3a,$35
	.byte $36,$37,$38,$39,$3a,$35,$36,$37
	.byte $38,$39,$3a,$35,$36,$37,$38,$39
	.byte $3a,$35,$36,$37,$38,$39,$3a,$35
	.byte $36,$37,$38,$39,$3a,$39,$3a,$3b
objs_4:
	.byte $1b,$01
	.byte $05
	.byte $00,$01,$02,$03,$04,$05,$06,$07
	.byte $08,$09,$0a,$0b,$0c,$0d,$0e,$0f
	.byte $10,$11,$12,$13,$14,$15,$16,$17
	.byte $18,$19,$1a
objs_5:
	.byte $06,$01
	.byte $05
	.byte $21,$22,$23,$24,$25,$26
objs_6:
	.byte $07,$01
	.byte $0d
	.byte $2b,$2b,$2b,$2b,$2b,$2b,$2b
objs_7:
	.byte $02,$02
	.byte $0d
	.byte $36,$37,$36,$37
objs_8:
	.byte $02,$0a
	.byte $0d
	.byte $37,$38,$37,$38,$37,$38,$37,$38
	.byte $37,$38,$37,$38,$37,$38,$37,$38
	.byte $37,$38,$37,$38
anim_data:
	.byte $03,$00,$14,$01,$14,$02,$14
	.byte $03,$03,$14,$04,$14,$05,$14
	.byte $03,$06,$14,$07,$14,$08,$14
	.byte $03,$09,$14,$0a,$14,$0b,$14
	.byte $03,$0c,$14,$0d,$14,$0e,$14

anim_offset:
	.byte $00,$07,$0e,$15,$1c

// Data for screen 0 
screen0_obj_nums:
	.byte $0b
screen0_level_objs:
	.byte <objs_3,>objs_3,$17,$00
	.byte <objs_8,>objs_8,$0d,$00
	.byte <objs_8,>objs_8,$00,$00
	.byte <objs_8,>objs_8,$00,$26
	.byte <objs_8,>objs_8,$0d,$26
	.byte <objs_0,>objs_0,$08,$1e
	.byte <objs_0,>objs_0,$08,$02
	.byte <objs_0,>objs_0,$0d,$17
	.byte <objs_0,>objs_0,$0d,$0a
	.byte <objs_0,>objs_0,$13,$0a
	.byte <objs_0,>objs_0,$13,$17
	.byte $ff,$ff
screen0_spr_nums:
	.byte $04

screen0_spr_data:
	.byte $05,$64,$00,$64,$00,$00,$2b,$01,$57,$00,$00,$29,$00,$d6,$01,$00,$31,$00,$56,$00,$00,$2b,$01,$d4,$00,$00
//dati per raster
	.byte $00,$01,$02,$03,$04,$ff,$00,$ff,$00,$ff
// Data for screen 1 
screen1_obj_nums:
	.byte $03
screen1_level_objs:
	.byte <objs_3,>objs_3,$07,$00
	.byte <objs_3,>objs_3,$0f,$00
	.byte <objs_3,>objs_3,$17,$00
	.byte $ff,$ff
screen1_spr_nums:
	.byte $0f

screen1_spr_data:
	.byte $10,$64,$00,$64,$00,$00,$36,$00,$51,$03,$00,$6e,$00,$51,$01,$00,$ed,$00,$54,$04,$00,$ba,$00,$52,$02,$00,$21,$01,$52,$03,$00,$22,$01,$9b,$01,$00,$e3,$00,$9b,$04,$00,$ac,$00,$9b,$03,$00,$76,$00,$9a,$02,$00,$36,$00,$9a,$01,$00,$20,$01,$da,$02,$00,$e6,$00,$da,$01,$00,$a2,$00,$dc,$04,$00,$6e,$00,$da,$03,$00,$39,$00,$db,$01,$00
//dati per raster
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$ff,$00,$ff,$00,$ff
// Data for screen 2 
screen2_obj_nums:
	.byte $03
screen2_level_objs:
	.byte <objs_3,>objs_3,$07,$00
	.byte <objs_3,>objs_3,$0f,$00
	.byte <objs_3,>objs_3,$17,$00
	.byte $ff,$ff
screen2_spr_nums:
	.byte $03

screen2_spr_data:
	.byte $04,$64,$00,$64,$00,$00,$a3,$00,$91,$00,$00,$a7,$00,$4c,$02,$00,$9e,$00,$d1,$03,$00
//dati per raster
	.byte $00,$01,$02,$03,$ff,$00,$ff,$00,$ff
screen_spr_table_lo:
 	.byte <screen0_spr_data
	.byte <screen1_spr_data
	.byte <screen2_spr_data
screen_spr_table_hi:
	.byte >screen0_spr_data
	.byte >screen1_spr_data
	.byte >screen2_spr_data
screen_obj_table_lo:
 	.byte <screen0_level_objs
	.byte <screen1_level_objs
	.byte <screen2_level_objs
screen_obj_table_hi:
	.byte >screen0_level_objs
	.byte >screen1_level_objs
	.byte >screen2_level_objs
