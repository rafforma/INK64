BasicUpstart2(main)

	* = $1000 "Code"
	
//postabley      incbin datay.bin
//postablex      incbin datax.bin
//rotationa       dc.b 0
//rotationb       dc.b 63
	
pause_raster:
	.byte $0
	
joydata:
	.byte $0

WaitBottom: {

	lda $d011
	bmi WaitBottom
L2: 
	lda $d011
	bpl L2
	rts
}

getjoy: {
	lda     #$FF
	sta     $DC00
	lda     $DC00
	eor     #$FF
	sta     joydata
	rts
}

clearscreen:{
	ldy #$0
	lda #32
rep:
	sta $400,y
	sta $500,y
	sta $600,y
	sta $700,y
	sta $800,y
	iny
	bne rep
	rts
}
spr_counter:
	.fill 24,0
spr_dir_lo:
	.fill 24,0
spr_dir_hi:
	.fill 24,0
platform_id:
	.byte 0
platform_x:
	.word 48,320,0
platform_y:
	.word 8*8+29-2,8*8+29+64-2,8*8+29+128-2

do_new_level: {
//	jsr WaitBottom
	screen_off();

	inc screen_id
	lda screen_id
	cmp #3
	bne zero_s
	lda #0
	sta screen_id
zero_s:
	jsr load_screen
	lda screen_id	
	tay
	lda screen_obj_table_lo,y
	sta $02
	lda screen_obj_table_hi,y
	sta $03
	
	jsr clearscreen
	jsr _draw_screen	
	
	lda #0
	sta platform_id
	jsr setup_player_coord
	
	lda #1
	sta spr_dir_lo
	lda #0
	sta spr_dir_hi
	
	screen_on();
	
	lda #$0
	sta pause_raster
	rts
}
setup_next_player_coord: {
	inc platform_id
	lda platform_id
	cmp #3
	beq new_level
	jmp setup_player_coord
new_level:	
	lda #0
	sta platform_id
	jmp do_new_level
}

setup_player_coord: {
	lda platform_id
	asl
	tay
	lda platform_x,y
	sta spr_coordx_lo
	lda platform_x+1,y
	sta spr_coordx_hi
	lda platform_y,y
	sta spr_coordy_lo

    /* reset if jump */
	lda #0
	sta num_iter
	lda #0
	sta spr_logic_id
	sta spr_logic_pc
	rts
}

is_collide:
	.byte $0
coll_posy:
	.byte $0

check_collision: {
	tya
	pha
	ldy $21
	lda spr_coordy_lo,y
	lsr
	lsr
	lsr
		
	sec
	sbc #4	
	
	asl	
	tax
		
	sta coll_posy
	
	lda lookup,x
	sta $02
	lda lookup+1,x
	sta $03
		
	ldy $21
	lda spr_coordx_lo,y
	lsr
	lsr
	lsr
	
	sec
	sbc #3
	sta $04
		
	lda spr_coordx_hi,y
	beq dosum
	clc
	lda $04
	adc #$20
	sta $04
	
dosum:		
	lda $02
	clc
	adc $04
	sta $02
	
	lda $03
	adc #0
	sta $03
		
	ldy #0	
	lda #0
	sta $04
next_char:	
	lda ($02),y	
	cmp #32
	beq no_coll
	lda #$1
	sta $04
	lda ($02),y	
	
no_coll:	
	sta $0413,y
	lda #1
	//sta ($02),y	
	sta $d813,y
	iny
	cpy $20
	bne next_char
	pla
	tay
	lda $04
	rts
}

collision_positions:
	.for(var i=0;i<25;i++)  .word 48+8*(i)-17

update_player: {
	lda jump_dir
	sta $0421
	
	lda #$0
	sta spr_dir_lo
	sta spr_dir_hi
	lda is_collide
	beq nojump_please
	
	lda joydata
	and #$1
	beq nojump_please
	lda cur_cmd
	cmp #6
	beq L10
	cmp #7
	bne L10
	lda #0
	sta num_iter
	sta spr_logic_pc
	lda #2
	sta spr_logic_id
	
	jmp L10
	
	/*lda jump_dir
	cmp #1
	beq L10	
	*/

nojump_please:		
	
ok:
	lda spr_coordy_lo	
	clc
	adc #1
	sta spr_coordy_lo
	
	lda cur_cmd
	cmp #6
	bne L30
	lda jump_dir
	cmp #1
	bne L10
L30:	
	lda #4
	sta $20
	lda #0
	sta $21
	jsr check_collision
	sta is_collide
	beq L10
	lda coll_posy
	tax
	lda collision_positions,x
	/*sec
	sbc #16*/
	sta spr_coordy_lo
	lda #0
	sta num_iter
	sta spr_logic_pc	
	sta spr_logic_id
	
L10:	
	lda joydata
	and #$4
	beq joy_right
	lda #$ff
	sta spr_dir_lo
	sta spr_dir_hi
	jmp move

joy_right:
	lda  joydata
	and   #$8
	beq  move
	lda #$1
	sta spr_dir_lo
	lda #$0
	sta spr_dir_hi
	
move:	
	
	lda  spr_coordx_lo
	sta $07
	lda spr_coordx_hi
	sta $08
	
	lda  spr_coordx_lo
	clc
	adc  spr_dir_lo
	sta spr_coordx_lo	
	lda spr_coordx_hi
	adc spr_dir_hi
	sta spr_coordx_hi

	lda #4
	sta $20
	lda #0
	sta $21
	jsr check_collision
	beq no_coll
	lda $07
	sta spr_coordx_lo
	lda $08
	sta spr_coordx_hi
	
	
no_coll:	
	rts
}
ready_for_change:
.byte 0
main:	

	lda #$1c         //charset at $3000
	sta $d018
	
	lda $d016
	ora #16
	sta $d016

	lda #0
	sta $d025
	lda #1
	sta $d026
	lda #12
	sta $d027

	
	jsr clearscreen
	ldy #0
nextimg:
	lda #$a3
	sta $07f8,y
	iny
	cpy #8
	bne nextimg

	lda #$ff
	sta $D01C 
	
	
	
	lda #0
	sta screen_id
	jsr load_screen
	
	lda screen_id
	tay
	lda screen_obj_table_lo,y
	sta $02
	lda screen_obj_table_hi,y
	sta $03
	
	jsr clearscreen
	jsr _draw_screen	
	
	jsr _init_raster
	
	
	screen_on()
	lda #0
	sta platform_id
	jsr setup_player_coord
wait:
	jsr getjoy

	/*lda _frame_ready
	bne check_joy
	*/
	jsr WaitBottom	
	jsr _no_irq_raster_service_band1
	jsr update_logic
	jsr update_anim
	jsr update_player
	
	lda #1
	sta _frame_ready
check_joy:	
	lda  joydata
	and   #$10
	cmp   #$00
	beq    wait
	
	jsr WaitBottom	
wait_unpress:
	jsr getjoy
	lda  joydata
	and   #$10
	cmp    #$00
	bne    wait_unpress
	lda #0
	sta ready_for_change
	lda #$1
	sta pause_raster
	
	jsr clearscreen
	lda #0
	sta $d015
ready:
	lda ready_for_change
	beq ready
	
	jsr do_new_level

	lda #$0
	sta pause_raster
 
	jmp wait
	
	
	rts

screen_id:
	.byte $0
	
.macro carica_raster(rast) {
	ldx #$0
next_1:	
	lda #0
	sta rast+1,x
	lda ($10),y	
	sta rast,x
	
	iny
	inx
	inx
	cmp #$ff
	bne  next_1

}

load_screen: {
	lda screen_id
	tay

	lda screen_spr_table_lo,y
	sta $10
	lda screen_spr_table_hi,y
	sta $11
	ldy #0
	ldx #0
	
	lda ($10),y
	sta spr_nums
	iny
	
next:	
	lda ($10),y
	sta spr_coordx_lo,x
	iny
	
	lda ($10),y
	sta spr_coordx_hi,x
	iny
	
	lda ($10),y
	sta spr_coordy_lo,x
	iny 
	
	lda ($10),y
	sta spr_anim_id,x
	iny
	
	lda ($10),y	
	lda #1
	sta spr_logic_id,x
	iny
	
	lda #0
	sta spr_image_id,x	
	sta spr_frame_pause,x
	sta spr_logic_pc,x
	sta num_iter,x
	sta acc_iter,x
	
	
	lda #3
	sta spr_logic_id,x
	
	/*lda $3d00,x
	and #1
	sta spr_logic_id,x
*/
	lda #$ff
	sta spr_cur_frame,x

	inx 
	cpx spr_nums
	bne next
	

	carica_raster(level_spr_ref_01)
	carica_raster(level_spr_ref_02)
	carica_raster(level_spr_ref_03)
	
	
	rts
}
	
savey:
	.byte $00
width:
	.byte $00
height:
	.byte $00
color:
	.byte $00

_draw_screen:
{
// $02,$03  dati
// $4 ,$05  oggetto da stampare
// $6 ,$7   vram
// $8 ,$9   cram

	ldy #0
	////// salva puntatore a oggetto 
there_is_a_command:
    lda ($02),y
	sta  $04
	iny  
	lda ($02),y
	sta $05
	cmp #$ff
	bne process_command
	jmp end
	
process_command:
	iny
	////// carica y	
	lda ($02),y
	asl
	tax

	lda lookup_color+1,x
	sta $9
	
	lda lookup+1,x
	sta $7

	iny
	
	clc
	lda lookup,x	
	adc ($02),y	
	sta $6
	bcc L1
	inc $7
L1:
	clc
	lda lookup_color,x	
	adc ($02),y	
	sta $8
	bcc L2
	inc $9
L2:
	iny
	
	sty savey
	
	ldy #0
	lda ($4),y
	sta width
	iny
	lda ($4),y
	sta height	
	iny
	lda ($4),y
	sta color
	
	clc
	lda #3
	adc $4
	sta $4
	lda #$0
	adc $5
	sta $5
	
next_row:
	ldy #0	
next_char:
	lda ($4),y
	sta ($6),y
	lda color
	sta ($8),y
	iny
	cpy width
	bne next_char
	dec height
	beq command_finished
	
	clc
	lda #40
	adc $6
	sta $6
	lda #$0
	adc $7
	sta $7
	
	clc
	lda #40
	adc $8
	sta $8
	lda #$0
	adc $9
	sta $9
	
	clc
	lda width
	adc $4
	sta $4
	lda #$0
	adc $5
	sta $5
	
	jmp next_row
command_finished:
	ldy savey
	jmp there_is_a_command
end:
	rts
}
_init_raster:
	sei
	lda #>_raster_service
	sta $0315
	sta $ffff
	lda #<_raster_service
	sta $0314
	sta $fffe
	lda #$7f
	sta $dc0d
	lda #$01
	sta $d01a
	sta $d011
	ora #$0b
	sta $d011
	lda #$30
	sta $d012
	lda #$00
	sta irqcnt
	cli
	rts
	

_stop_raster:
	sei
	lda #$ea
	sta $0315
	lda #$31
	sta $0314
	inc $d019
	lda #$81
	sta $dc0d
	lda #$00
	sta $d01a
	cli
	rts
	
_frame_ready:
	.word $1
raster_line:
	.byte 50
	.byte 50+64-2
	.byte 50+64+8
    .byte 50+128-2
	.byte 50+128+8
	.byte 50+192-2
    .byte 50+200

irqcnt:
	 .byte $0
	
num_lines:
//	.byte $7
	.byte $1

colors:
	//////.byte 2,3,4,11
	.byte 0,0,0,0,0,0,0,0
temp:
	.byte $0
temp2:
	.byte $0
temp3:
	.byte $0
	
raster_service_routine:
	/*.byte <_raster_service_band1,>_raster_service_band1
	.byte <_raster_service_nop,>_raster_service_nop
	.byte <_raster_service_band2,>_raster_service_band2
	.byte <_raster_service_nop,>_raster_service_nop
	.byte <_raster_service_band3,>_raster_service_band3
	.byte <_raster_service_nop,>_raster_service_nop*/
	.byte <_raster_service_update_anim,>_raster_service_update_anim
	
raster_jump:
	.word $0
frame_wait:
	.byte 0
	
_raster_service:
	pha
	txa
	pha
	tya
	pha
	php
	
	lda pause_raster
	bne pause_the_raster

	lda irqcnt
	tax
	lda colors,x	

LL4:
	sta $d021	
	
	lda irqcnt
	asl 
	tax
	lda raster_service_routine,x
	sta raster_jump
	lda raster_service_routine+1,x
	sta raster_jump+1
	
	jmp (raster_jump)
	
return_to_raster_service:	

	inc irqcnt	
	lda irqcnt
	cmp num_lines
	bne LL1
	lda #0
	
	sta irqcnt
LL1:
	lda #<_raster_service  
	sta $0314  
	lda #>_raster_service
	sta $0315
	
	lda irqcnt	
	tax
	lda raster_line,x
	sta $d012		
	
	inc    $d019
	
	nop
    nop
    nop
    nop
	nop
	lda 0

	plp
	pla
	tay
	pla
	tax
	pla
	
	jmp $ea81

	rti
	
pause_the_raster:
	lda #0
	sta $d020
	sta $d021
	inc frame_wait
	lda frame_wait
	cmp #25
	bne LL1
	lda #0
	sta frame_wait
	inc ready_for_change
	jmp LL1
	
_raster_service_nop:		
	jmp return_to_raster_service
	
_raster_service_update_anim:		
	lda #0
	sta   _frame_ready
	jmp return_to_raster_service
	
.macro service_band(ref_table)  {
	lda #9
	sta $d020
	ldy #0
	sty $d015
	sty $d010
	lda ref_table,y
	cmp #$ff
	
next_sprite_band:
	beq fine_band
	
	lda ref_table,y
	tax	

	lda spr_coordx_lo,x
	sta $d000,y
	lda spr_coordx_hi,x
	beq noextra
	lda $d010
	ora bit_table,y
	sta $d010	
noextra:
	lda spr_coordy_lo,x
	sta $d001,y
	
	lda $d015
    ora bit_table,y
	sta $d015
	
	lda spr_image_id,x
	ldx word2byte,y	
	sta $07f8,x
		
	iny
	iny
	lda ref_table,y
	cmp #$ff

	bne next_sprite_band
	
fine_band:
	lda #4
	sta $d020
	//jmp return_to_raster_service
	rts
}


_no_irq_raster_service_band1: {
	service_band(level_spr_ref_01)
}

	
_raster_service_band1:
	service_band(level_spr_ref_01)

 _raster_service_band2:
	service_band(level_spr_ref_02)
 
_raster_service_band3:
	service_band(level_spr_ref_03)
	
	
maxframe:
	.byte $0
animbeginaddr:
	.byte $0
newframe:
	.byte $0
	
spriteimagetable:
.for(var i=0;i<15;i++)
	.byte $f0+i
		
	

update_anim: {
	////char animid[32]//
	////char framecounter[32]
	////char framepause[32]
	ldy #0
	lda spr_nums
	sta spr_cur
nextanim:
	lda spr_cur
	beq nomoreanim
	
	lda spr_frame_pause,y
	bne nochangeframe
	
	////// carica il numero massimo di frame per l'animazione di animid
	
	lda spr_anim_id,y
	tax
	lda anim_offset,x
	sta animbeginaddr
	tax
	lda anim_data,x
	sta maxframe

	lda spr_cur_frame,y		
    clc
	adc #1
	sta spr_cur_frame,y	
	
	cmp maxframe
	bcc noresetframe
	
	lda #0
	sta spr_cur_frame,y	

noresetframe:	
	lda spr_cur_frame,y
	asl
	clc
	adc animbeginaddr	
	tax

	lda anim_data+2,x
	sta spr_frame_pause,y
	lda anim_data+1,x
	
	tax
	lda spriteimagetable,x
	sta spr_image_id,y
	
nochangeframe:	
	
	lda spr_frame_pause,y		
	sec
	sbc #1
	sta spr_frame_pause,y	
	
	iny
	dec spr_cur
	jmp nextanim
nomoreanim:

	rts
}

	
call_func: {
	jmp (temp)
	rts
}
update_logic: {
	ldy #0
	lda spr_nums
	
next:
	lda num_iter,y  // verifica se deve caricare un altro comando
	beq carica_nuovo_comando   
	lda cur_cmd,y     // carica il comando e salta alla routine che lo esegue
	sta $0400,y
	lda #1
	sta $d800,y
	lda cur_cmd,y
	beq reset_pc
	tax
	lda cmd_table_lo,x
	sta temp
	lda cmd_table_hi,x
	sta temp+1
	
	jsr call_func

	iny
	cpy spr_nums
	bne next
	rts
carica_nuovo_comando:
	lda spr_logic_id,y
	tax
	lda logic_offset,x
	clc
	adc spr_logic_pc,y
	
	tax
	lda logic_program,x     ///carica il comando
	beq reset_pc
	sta cur_cmd,y
	
	lda logic_program+1,x   /// carica il numero di iterazione
	sta num_iter,y
	lda logic_program+2,x   /// carica l'incremento per frame
	sta incr_iter,y
	
	lda #0
	sta acc_iter,y
	
	lda spr_logic_pc,y
	clc
	adc #3
	sta spr_logic_pc,y
	
	lda spr_coordy_lo,y
	sta float_posy_hi,y
	lda #0
	sta float_posy_lo,y
	
	jmp next	
reset_pc:
	lda #0
	sta spr_logic_pc,y
	jmp next
}

advance:  {
	lda acc_iter,y
	clc
	adc incr_iter,y
	sta acc_iter,y
	
	
	lda num_iter,y
	sec 
	sbc incr_iter,y
	sta num_iter,y
	
	lda num_iter,y
	cmp #0
	beq fine_comando
il_comando_continua:
	rts
fine_comando:
	lda #0
	sta num_iter,y
	sta acc_iter,y
	rts
}	

cmd_nop: {
}

cmd_wait: {
	jmp advance
}	

cmd_move_left:
{
	lda spr_coordx_lo,y
	sec
	sbc incr_iter,y
	sta spr_coordx_lo,y
	
	lda spr_coordx_hi,y
	sbc #0
	sta spr_coordx_hi,y
	
	jmp advance
}
	
cmd_move_right:
{
	lda spr_coordx_lo,y
	clc 
	adc incr_iter,y
	sta spr_coordx_lo,y
	
	lda spr_coordx_hi,y
	adc #0
	sta spr_coordx_hi,y
	
	jmp advance
}	
	
cmd_move_up:
{
	lda spr_coordy_lo,y
	clc 
	adc incr_iter,y
	sta spr_coordy_lo,y
		
	jmp advance
}	
cmd_move_down:
{
	lda spr_coordy_lo,y
	sec
	sbc incr_iter,y
	sta spr_coordy_lo,y
		
	jmp advance
}	
	
cmd_move_path:{

	lda acc_iter,y
	asl
	tax	
	lda float_posy_lo,y
	clc
	adc jump_path,x
	sta float_posy_lo,y
		
	lda float_posy_hi,y
	adc jump_path+1,x
	sta float_posy_hi,y		
	
	lda float_posy_hi,y
	sta spr_coordy_lo,y
	
	lda #0
	sta jump_dir
	
	lda jump_path+1,x
	bmi adv
	lda #1
	sta jump_dir
adv:
	jmp advance
}	

cmd_idiot_jump:{

	lda float_posy_hi,y
	sta $7
	lda float_posy_lo,y		
	sta $8

	
	lda acc_iter,y
	asl
	tax	
	lda float_posy_lo,y
	clc
	adc jump_path,x
	sta float_posy_lo,y
		
	lda float_posy_hi,y
	adc jump_path+1,x
	sta float_posy_hi,y		
	lda spr_coordy_lo,y
	sta $7
	
	lda float_posy_hi,y
	sta spr_coordy_lo,y
	
	
	lda jump_path+1,x
	bmi move_hor
	
	lda #4
	sta $20
	sty $21
	jsr check_collision
	beq move_hor
	
	lda $7
	sta float_posy_hi,y
    sta spr_coordy_lo,y
	
	lda $8
	sta  float_posy_lo,y	
move_hor:
no_coll:	
adv:
	jmp advance
}	

cmd_idiot_moving: {	
    
	lda spr_counter,y
	cmp #255
	beq nojump
	
	lda spr_counter,y
	clc
	adc #1
	sta spr_counter,y		
	
nojump:
	lda spr_dir_lo,y
	bne cont
	lda #1
	sta spr_dir_lo,y
	lda #0
	sta spr_dir_hi,y
cont:
	lda spr_coordy_lo,y	      //// applica la gravita
	clc
	adc #1
	sta spr_coordy_lo,y
	
	
	lda #4
	sta $20
	sty $21
	jsr check_collision     ////se collide aggiusta la posizione y
	beq horiz_moving
	lda coll_posy
	tax
	lda collision_positions,x
	sta spr_coordy_lo,y
	
	lda spr_counter,y
	cmp #255
	bne horiz_moving
	lda #0
	sta spr_counter,y
	sta num_iter,y
	sta spr_logic_pc,y
	lda #4
	sta spr_logic_id,y
    jmp advance

horiz_moving:	
	lda  spr_coordx_lo,y      /// salva la precedente posizione x
	sta $07
	lda spr_coordx_hi,y
	sta $08
	

	lda  spr_coordx_lo,y      /// muove orizontalmente
	clc
	adc  spr_dir_lo,y
	sta spr_coordx_lo,y	
	lda spr_coordx_hi,y
	adc spr_dir_hi,y
	sta spr_coordx_hi,y
	

	lda #4                 /// se trova un ostacolo inverte la direzione
	sta $20
	sty $21
	jsr check_collision
	beq no_coll
	lda $07
	sta spr_coordx_lo,y
	lda $08
	sta spr_coordx_hi,y
	
	lda spr_dir_hi,y
	cmp #$ff
	bne dir_sinistra
	lda #1
	sta spr_dir_lo,y
	lda #0
	sta spr_dir_hi,y
	jmp advance
dir_sinistra:
	lda #$ff
	sta spr_dir_lo,y
	sta spr_dir_hi,y
no_coll:	

	//rts
	jmp advance
}
	
bit_table:
	.word 1,2,4,8,16,32,64,128
word2byte:
	.byte 0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10
	
lookup_color:
	.word	$D800
	.word	$D828
	.word	$D850
	.word	$D878
	.word	$D8A0
	.word	$D8C8
	.word	$D8F0
	.word	$D918
	.word	$D940
	.word	$D968
	.word	$D990
	.word	$D9B8
	.word	$D9E0
	.word	$DA08
	.word	$DA30
	.word	$DA58
	.word	$DA80
	.word	$DAA8
	.word	$DAD0
	.word	$DAF8
	.word	$DB20
	.word	$DB48
	.word	$DB70
	.word	$DB98
	.word	$DBC0
lookup:
	.word	$0400
	.word	$0428
	.word	$0450
	.word	$0478
	.word	$04A0
	.word	$04C8
	.word	$04F0
	.word	$0518
	.word	$0540
	.word	$0568
	.word	$0590
	.word	$05B8
	.word	$05E0
	.word	$0608
	.word	$0630
	.word	$0658
	.word	$0680
	.word	$06A8
	.word	$06D0
	.word	$06F8
	.word	$0720
	.word	$0748
	.word	$0770
	.word	$0798
	.word	$07C0
		
	*= $3000 "Chars"
chars:
	.import binary "chars.bin"
	
	*= $3c00 "Sprites"
	
sprites:
	.import binary "lettere.bin"

	*=* "LEVEL DATA"
	#import "screens.inc"
	
	*=* "SCREEN DATA"
	
	/* INFORMAZIONI RELATIVE ALLO SCHERMO
	   sullo schermo possono esserci massimo 24 sprite
	  questa struttura deve essere caricata per ogni screen
	*/
	
spr_nums:                                         //// numero di sprites
	.byte $0
spr_cur:                                         //// 
	.byte $0
spr_coordx_lo:                                    //// coordinata x 
	.fill 24, 0
spr_coordx_hi:                                    //// coordinata x
	.fill 24, 0
spr_coordy_lo:                                    //// coordinata y
	.fill 24, 0
spr_anim_id:                                      //// animazione associata
	.fill 24,0
spr_logic_id:                                     //// logica associata
	.fill 24,0
spr_image_id:                                     //// immagine  hw_sprite in memory
	.fill 24,0
spr_cur_frame:                                    //// frame corrente
	.fill 24,0
spr_frame_pause:                                //// tempo di permanenza di un frame.
	.fill 24,0
spr_logic_pc:
	.fill 24,0

	/// logic_offset e logic_program, sono globali e caricati dal tool
cmd_table_lo:
	.byte 0,<cmd_move_left,<cmd_move_right,<cmd_move_up,<cmd_move_down,<cmd_wait,<cmd_move_path,<cmd_nop,<cmd_idiot_moving,<cmd_idiot_jump
cmd_table_hi:	
	.byte 0,>cmd_move_left,>cmd_move_right,>cmd_move_up,>cmd_move_down,>cmd_wait,>cmd_move_path,>cmd_nop,>cmd_idiot_moving,>cmd_idiot_jump

	
logic_offset:
	.byte 0,4,17,24,28
logic_program:
	.byte 7,1,0,0
	.byte 1,20,1,5,100,1,2,20,1,5,100,1,0
	.byte 6,73,1,7,1,0,0
	.byte 8,1,0,0
	.byte 9,73,1,8,1,0,0
	 /*.byte 4,10,1,5,24,1,3,10,1,5,25,1,0 */

level_spr_ref_01:
	.fill 16,$ff
level_spr_ref_02:
	.fill 16,$ff
level_spr_ref_03:
	.fill 16,$ff
	
float_posy_lo:
	.fill 24,0
float_posy_hi:
	.fill  24,0
	
acc_iter:
	.fill 24, 0	
num_iter:	
	.fill 24, 0
incr_iter:
	.fill 24, 0
cur_cmd:
	.fill 24, 0

.macro screen_off() {
	sei
	lda     $D011
	and     #$ef
	sta     $D011
	cli
}

.macro screen_on() {
	sei
	lda $D011
	ora #16
	sta $D011
	cli	
}

jump_path:
.word $fe81
.word $fe81
.word $fe82
.word $fe84
.word $fe86
.word $fe8a
.word $fe8e
.word $fe92
.word $fe98
.word $fe9e
.word $fea5
.word $feac
.word $feb4
.word $febd
.word $fec6
.word $fed0
.word $feda
.word $fee5
.word $fef1
.word $fefd
.word $ff0a
.word $ff17
.word $ff24
.word $ff32
.word $ff41
.word $ff4f
.word $ff5e
.word $ff6e
.word $ff7d
.word $ff8d
.word $ff9d
.word $ffad
.word $ffbe
.word $ffce
.word $ffdf
.word $fff0
.word $0000
.word $0010
.word $0021
.word $0032
.word $0042
.word $0053
.word $0063
.word $0073
.word $0083
.word $0092
.word $00a2
.word $00b1
.word $00bf
.word $00ce
.word $00dc
.word $00e9
.word $00f6
.word $0103
.word $010f
.word $011b
.word $0126
.word $0130
.word $013a
.word $0143
.word $014c
.word $0154
.word $015b
.word $0162
.word $0168
.word $016e
.word $0172
.word $0176
.word $017a
.word $017c
.word $017e
.word $017f
.word $017f
jump_dir:
	.byte $0
	

