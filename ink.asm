
#import "common.asm"
.var music = LoadSid("ink_01.sid")
.var OnGround=$0
.var Jump=$1
.var Falling=$2
.var clearcolor=$00

BasicUpstart2(main)
* = $1000 "Code"


.var PIXEL=$2
.var MAP=$8
.var px0=$04
.var px0_h=$05
.var py0=$06
.var width=$d5
.var height=$d6
.var temp=$7
.var temp1=$8
.var temp2=$0a

.var frame_delay=$81
.var num_frame=$82

.var SOURCE=$10
.var DEST=$12
.var BASE=$14
.var R0=$16
.var R1=$17
.var R2=$18
.var mask_1=$19
.var mask_2=$20
.var data_1=$30
.var save_x=$21

.var frame_offs_x =$d0
.var frame_offs_y =$d1
.var frame_width =$d2
.var frame_height =$d3
.var frame_img =$d4
.var LEVEL=$da
.var coll_code=$22
.var level_ok=$23
.var level_restart=$24
.var TILEMASK = $2a
.var TILEMASK_MIDDLE = $2c

.var TILEMASK_A1 = $70
.var TILEMASK_B1 = $72
.var TILEMASK_C1 = $74
.var TILEMASK_A2 = $76
.var TILEMASK_B2 = $78
.var TILEMASK_C2 = $7a
.var TILEMASK_A3 = $7c
.var TILEMASK_B3 = $7e
.var TILEMASK_C3 = $80



py0_h:
    .byte 0
countdown:
    .byte 0
effectid:
    .byte 0
frame_count:
    .byte 0
saved_frame_count:
    .byte 1

.macro select_bitmap(k) {
    lda #<(bitmap1+32*k)
    sta SOURCE
    lda #>(bitmap1+32*k)
    sta SOURCE+1

}
.macro waitsec (s) {
    lda #(s*50)
    sta countdown
    lda frame_count
    sta saved_frame_count
!:
    lda frame_count
    cmp saved_frame_count
    beq !-
    dec countdown
    beq fine
    
    jsr erase_pixel_tear    
    jsr update_tears
    jsr draw_pixel_tear
    lda frame_count
    sta saved_frame_count

    jmp !-
    //jsr music.play 
    //jsr vblank

fine:
}
.macro try_change_anim(id) {
	/*lda #id
	cmp model_anim,x
    beq nothing
*/
    lda model_anim,x
    cmp #6
    bne check_7
    lda model_frame_id,x
    cmp #7
    bne nothing
    jmp change
check_7:
    lda model_anim,x
    cmp #7
    bne change
    lda model_frame_id,x
    cmp #7
    bne nothing

change:
    lda #id
    sta model_anim,x
  	lda #0
	sta model_frame_id,x
    sta model_delay,x
    sta model_frame,x
nothing:
}
.macro setanim(id) {
	lda #id
	cmp model_anim,x
    beq !+
    lda #id
    sta model_anim,x
	lda #0
	sta model_frame_id,x
    sta model_delay,x
    sta model_frame,x
!:
}
cur_level: 
.byte $0

tabella_colori:
    .byte $d0,$50,$60,$70
tabella_colori_inv:
    .byte $0d,$05,$06,$07

idcolor:
    .byte $0
colore:
    .byte $70

col:
    .fill 320,<(floor(i/8)*8)

col_s:
    .fill 40,<(i*8)

coltable_lo:
    .fill 200,<(bank+$400+i*40)
coltable_hi:
    .fill 200,>(bank+$400+i*40)
mask_p:
    //.byte $80,$40,$20,$10,$08,$04,$02,$01
    .byte $c0,$60,$30,$18,$0c,$06,$03,$03

mask:
    .byte $FF,$7f,$3f,$1F,$0F,$07,$03,$01

mask_r:
    .byte $FF-$ff,$ff-$7f,$ff-$3f,$ff-$1F,$ff-$0F,$ff-$07,$ff-$03,$ff-$01

screen_lo:
    .fill 200,<(bank+$2000+floor(i/8)*320+mod(i,8))
screen_hi:
    .fill 200,>(bank+$2000+floor(i/8)*320+mod(i,8))

screen__lo:
    .fill 200,<(bank+$2000+floor(i/8)*320)
screen__hi:
    .fill 200,>(bank+$2000+floor(i/8)*320)

map_lo:
    .fill 25,<(map+40*i)
map_hi:
    .fill 25,>(map+40*i)

#import "muliplexer.asm"
.var IRQ1LINE = $FA                          // This is the place on screen where the sorting IRQ happens
.var IRQ2LINE = $2A                          // This is where sprite displaying begins...
init_raster: {
    sei
    lda #$35
    sta $01
    lda #<IRQ1
//;    STA $0314
    sta $FFFE
    lda #>IRQ1
//;    STA $0315
    sta $FFFF
    lda #$7F                            // CIA interrupt off
    sta $DC0D
    lda #$01                            // Raster interrupt on
    sta $D01A
    lda $d011                           // clear high bit of raster line
    and #$7f
    sta $d011

    lda #IRQ1LINE                       // Line where next IRQ happens
    sta $D012
    lda $DC0D                           // Acknowledge IRQ (to be sure)
    cli
    rts
}

STORE_A:
    .byte 0
STORE_X:
    .byte 0
STORE_Y:
    .byte 0
frame_ready:  
    .byte 0

IRQ1:{
    sta STORE_A
    stx STORE_X
    sty STORE_Y
    
    inc frame_count
    
    jsr music.play 
    lda frame_ready
    beq !+
    jsr erase_pixel_tear    
    jsr draw_pixel_tear

    lda sprite_count
    sta hw_sprite_count

    ldx #0
copy:
    lda sprite_list,x
    sta hw_sprite_list,x
    
    
    lda model_x,x
    sta hw_sprite_x,x
    lda model_y,x
    sta hw_sprite_y,x
    lda model_x_h,x
    sta hw_sprite_x_h,x
    lda model_img,x
    sta hw_sprite_img,x
    inx
    cpx #16
    bne copy

    lda #0
    sta frame_ready
!:

    lda #0
    sta $d010
    sta $d015

    lda #$FF                            // Move all sprites
    sta $D001                           // to the bottom to prevent
    sta $D003                           // weird effects when sprite
    sta $D005                           // moves lower than what it
    sta $D007                           // previously was
    sta $D009
    sta $D00B
    sta $D00D
    sta $D00F


    .for (var i=0;i<8;i++) {
        lda #i
        cmp hw_sprite_count
        bcc !+
        jmp end
    !:
        ldy hw_sprite_list+i

        lda hw_sprite_y,y
        clc
        adc #50
        sbc model_frame_offsy,y
        sta $d000+i*2+1


        lda hw_sprite_img,y       
        sta bank+$0400+$03f8+i

        lda #1
        sta $d027+i

        lda hw_sprite_x,y
        clc        
        adc model_frame_offsx,y
        sta $d000+i*2
        
        lda hw_sprite_x_h,y
        adc #0
        beq !+
        lda #(1<<i)
        ora $D010        
        sta $D010    
    !:
             
        lda  #(1<<i)
        ora $d015
        sta $d015
    }
end:
    lda hw_sprite_count
    cmp #9
    bcs !+
   
    jmp endirq
!:
    
    ldy hw_sprite_list+8
    lda hw_sprite_y,y
    clc                                 // THAT COORDINATE - $10 IS THE
    adc #50-7                            // position for next interrupt
    sbc model_frame_offsy,y
    cmp $D012                           // Already late from that?
    bcs !+
   jmp IRQ2_DIR                     // Then go directly to next IRQ
!:
    sta $D012
    lda #<IRQ2                          // Set up the sprite display IRQ
    sta $FFFE
    lda #>IRQ2
    sta $FFFF
endirq:
    ldy #$00
    lsr $D019

    lda STORE_A
    ldx STORE_X
    ldy STORE_Y
    rti

}

IRQ2:{
    sta STORE_A
    stx STORE_X
    sty STORE_Y
}

IRQ2_DIR: {

   /* lda #$FF                            // Move all sprites
    sta $D001                           // to the bottom to prevent
    sta $D003                           // weird effects when sprite
    sta $D005                           // moves lower than what it
    sta $D007                           // previously was
    sta $D009
    sta $D00B
    sta $D00D
    sta $D00F
*/
    lda #0
    sta $d010
    sta $d015
 
   .for (var i=0;i<8;i++) {
        lda #(i+8)
        cmp hw_sprite_count
        bcc !+
        jmp end
    !:
        ldy hw_sprite_list+(i+8)

        lda hw_sprite_y,y
        clc
        adc #50
        sbc model_frame_offsy,y
        sta $d000+i*2+1


        lda hw_sprite_img,y       
        sta bank+$0400+$03f8+i

        lda #1
        sta $d027+i

        lda hw_sprite_x,y
        clc        
        adc model_frame_offsx,y
        sta $d000+i*2
        
        lda hw_sprite_x_h,y
        adc #0
        beq !+
        lda #(1<<i)
        ora $D010        
        sta $D010    
    !:
             
        lda  #(1<<i)
        ora $d015
        sta $d015
    }
end:
endirq:

    lda #<IRQ1                          // Set up the sprite display IRQ
    sta $FFFE
    lda #>IRQ1
    sta $FFFF
    lda #IRQ1LINE                       // Line where next IRQ happens
    sta $D012

    

    ldy #$00
    lsr $D019

    lda STORE_A
    ldx STORE_X
    ldy STORE_Y
    rti

}
draw_box: {
    sta temp2

    lda #0
    sta temp
        
    ldy py0

    lda map_lo,y
    sta PIXEL
    lda map_hi,y
    sta PIXEL+1

    lda height
    sta temp1
next_row:
    ldy px0

    lda width
    sta temp

next_char:
    lda temp2
    sta (PIXEL),y
    iny
    dec temp
    bne next_char
    lda PIXEL
    clc 
    adc #40
    sta PIXEL

    lda PIXEL+1
    adc #0
    sta PIXEL+1

    dec temp1
    bne next_row

    rts
}
num_nemici:
    .byte 0
loadlevel:{
    
    ldy #   0
    sty num_nemici
   
    ldx #8
    lda #0
mm:
    sta model_state,x
    inx
    cpx #16
    bne mm

next:
    lda (LEVEL),y
    cmp #$10
    bne !+
    jmp carica_player
!:
    cmp #$20
    bne !+
    jmp carica_goal
!:
    cmp #$80
    bne !+
    jmp carica_blocco
!:
    cmp #$81
    bne !+
    jmp carica_blocco_goal
!:
    cmp #$82
    bne !+
    jmp carica_blocco_punte
!:
    cmp #$07
    bne !+
    jmp carica_nemico
!:
    jmp fine
carica_player:
    iny
    lda (LEVEL),y
    sta model_x
    sta player_x
    iny
    lda (LEVEL),y
    sta model_x_h
    sta player_x_h
    iny
    lda (LEVEL),y
    sta model_y
    sta player_y
    iny 
    lda #0
    sta model_y_h
    sta player_y_h
    sta model_state
    lda #3
    sta model_anim
    jmp next
carica_goal:
    iny
    lda (LEVEL),y
    sta model_x+1
    iny
    lda (LEVEL),y
    sta model_x_h+1
    iny
    lda (LEVEL),y
    sta model_y+1
    iny 
    lda #0
    sta model_y_h+1
    sta model_state+1
    ldx #1
    setanim(8)
    jmp next

carica_blocco:
    iny
    lda (LEVEL),y
    sta px0
    iny
    lda (LEVEL),y
    sta py0
    iny
    lda (LEVEL),y
    sta width
    iny
    lda (LEVEL),y
    sta height
    iny
    sty save_x
    lda #1
    jsr draw_box
    ldy save_x
    jmp next
carica_blocco_punte:
    iny
    lda (LEVEL),y
    sta px0
    iny
    lda (LEVEL),y
    sta py0
    iny
    lda (LEVEL),y
    sta width
    iny
    lda (LEVEL),y
    sta height
    iny
    sty save_x
    lda #3
    jsr draw_box
    ldy save_x
    jmp next

carica_blocco_goal:
    iny
    lda (LEVEL),y
    sta px0
    iny
    lda (LEVEL),y
    sta py0
    iny
    lda (LEVEL),y
    sta width
    iny
    lda (LEVEL),y
    sta height
    iny
    sty save_x
    lda #2
    jsr draw_box
    ldy save_x
    jmp next
carica_nemico:
    ldx num_nemici
    iny
    lda (LEVEL),y
    sta model_x+8,x
    iny
    lda (LEVEL),y
    sta model_x_h+8,x
    iny
    lda (LEVEL),y
    sta model_y+8,x    
    iny 
    lda #0
    sta model_y_h+8,x
    lda #1
    sta model_state+8,x
    lda #1
    sta model_anim+8,x
    inc num_nemici
    jmp next    
fine:
    rts

}
.macro fillbox (x,y,w,h) {
    lda #x
    sta px0
    lda #y
    sta py0
    lda #w
    sta width
    lda #h
    sta height
    jsr draw_box
}
draw_tile: {
    lda py0
    asl
    asl
    asl
    tay

    lda screen_lo,y
    sta PIXEL
    lda screen_hi,y
    sta PIXEL+1

    lda px0
    cmp #32
    bcc !+

    lda px0
    sbc #32
    sta px0

    lda PIXEL
    clc
    adc #$00
    sta PIXEL
    
    lda PIXEL+1
    adc #1
    sta PIXEL+1
    

!:
    ldy px0
    lda col_s,y
    tay

    lda #$ff    
    sta (PIXEL),y
    iny

    sta (PIXEL),y
    iny

    sta (PIXEL),y
    iny

    sta (PIXEL),y
    iny

    sta (PIXEL),y
    iny

    sta (PIXEL),y
    iny

    sta (PIXEL),y
    iny
    

    sta (PIXEL),y
    
    rts
}
draw_tile_2: {
    tay

    lda #<ink_scr_charset
    clc
    adc mult16_lo,y
    sta MAP

    lda #>ink_scr_charset
    adc mult16_hi,y
    sta MAP+1
    
    lda py0
    tay
    lda coltable_lo,y
    sta BASE
    lda coltable_hi,y
    sta BASE+1

    lda py0
    asl
    asl
    asl
    tay


    lda screen_lo,y
    sta PIXEL
    lda screen_hi,y
    sta PIXEL+1

    lda px0
    cmp #32
    bcc !+

    lda px0
    sbc #32
    sta px0

    lda PIXEL
    clc
    adc #$00
    sta PIXEL
    
    lda PIXEL+1
    adc #1
    sta PIXEL+1
    
   

!:


    ldy px0   
    lda colore
    sta (BASE),y


    ldy px0
    lda col_s,y
    clc
    adc PIXEL
    sta PIXEL


    lda PIXEL+1
    adc #0
    sta PIXEL+1

    
    

    ldy #0

    lda (MAP),y   
    sta (PIXEL),y
    iny

    lda (MAP),y   
    sta (PIXEL),y
    iny

    lda (MAP),y   
    sta (PIXEL),y

    iny

    lda (MAP),y   
    sta (PIXEL),y

    iny

    lda (MAP),y   
    sta (PIXEL),y

    iny

    lda (MAP),y   
    sta (PIXEL),y

    iny

    lda (MAP),y   
    sta (PIXEL),y

    iny
    

    lda (MAP),y   
    sta (PIXEL),y
    
    rts
}
rect_col:{
    lda #0
    sta temp

    lda py0
    lsr
    lsr
    lsr
    tay 

    lda coltable_lo,y
    sta PIXEL
    lda coltable_hi,y
    sta PIXEL+1

    lda map_lo,y
    sta MAP
    lda map_hi,y
    sta MAP+1


    lda px0_h
    beq !+

    lda #32
    sta temp
!:

    lda px0
    lsr
    lsr
    lsr
    clc 
    adc temp
    sta temp

    tay

    lda (MAP),y
    beq !+
    lda (PIXEL),y
    and #$f0
    //bne !+
    lda #clearcolor
    ora colore
    sta (PIXEL),y
!:
    iny
    lda (MAP),y
    beq !+
    lda (PIXEL),y
    and #$f0
    //bne !+

    lda #clearcolor
    ora colore
    sta (PIXEL),y
    
    lda px0
    and #7
    beq !+
    iny
    lda (MAP),y
    beq !+
    lda (PIXEL),y
    and #$f0
    //bne !+

    lda #clearcolor
    ora colore
    sta (PIXEL),y
!:

    lda PIXEL
    clc
    adc #40
    sta PIXEL

    lda PIXEL+1
    adc #0
    sta PIXEL+1

    lda MAP
    clc
    adc #40
    sta MAP

    lda MAP+1
    adc #0
    sta MAP+1

    /*lda px0
    lsr
    lsr
    lsr*/
    lda temp
    tay

    lda (MAP),y
    beq !+
    lda (PIXEL),y
    and #$f0
    //bne !+

    lda #clearcolor
    ora colore
    sta (PIXEL),y
!:
    iny
    lda (MAP),y
    beq !+
    lda (PIXEL),y
    and #$f0
    //bne !+

    lda #clearcolor
    ora colore
    sta (PIXEL),y
    
    lda px0
    and #7
    beq !+
    iny
    lda (MAP),y
    beq !+

    lda #clearcolor
    ora colore
    sta (PIXEL),y
!:

    lda py0
    and #7
    beq end
    lda PIXEL
    clc
    adc #40
    sta PIXEL

    lda PIXEL+1
    adc #0
    sta PIXEL+1

    lda MAP
    clc
    adc #40
    sta MAP

    lda MAP+1
    adc #0
    sta MAP+1

  /*  lda px0
    lsr
    lsr
    lsr*/
    lda temp
    tay

    lda (MAP),y
    beq !+
    lda (PIXEL),y
    and #$f0
    //bne !+

    lda #clearcolor
    ora colore
    sta (PIXEL),y
!:
    iny
    lda (MAP),y
    beq !+
    lda (PIXEL),y
    and #$f0
    //bne !+

    lda #clearcolor
    ora colore
    sta (PIXEL),y
    
    lda px0
    and #7
    beq !+
    iny
    lda (MAP),y
    beq !+
    lda (PIXEL),y
    and #$f0
    //bne !+

    lda #clearcolor
    ora colore
    sta (PIXEL),y
!:
end:    
    rts
}





prepare_mask:{
    lda #0
    sta temp

    lda py0
    lsr
    lsr
    lsr
    tay 


    lda map_lo,y
    sta MAP
    lda map_hi,y
    sta MAP+1


    lda px0_h
    beq !+

    lda #32
    sta temp
!:

    lda px0
    lsr
    lsr
    lsr
    clc 
    adc temp
    sta temp

    tay

    lda (MAP),y
    asl
    asl
    asl    
    sta R0

    lda #<(charset)
    clc
    adc R0
    sta TILEMASK_A1

    lda #>(charset)
    adc #0
    sta TILEMASK_A1+1

    lda py0
    and #7
    sta temp2

    lda TILEMASK_A1
    clc
    adc temp2
    sta TILEMASK_A1

    lda TILEMASK_A1+1
    adc #0
    sta TILEMASK_A1+1


    iny
    lda (MAP),y
    asl
    asl
    asl    
    sta R0

    lda #<(charset)
    clc
    adc R0
    sta TILEMASK_B1

    lda #>(charset)
    adc #0
    sta TILEMASK_B1+1
    

    lda TILEMASK_B1
    clc
    adc temp2
    sta TILEMASK_B1

    lda TILEMASK_B1+1
    adc #0
    sta TILEMASK_B1+1


    iny
    lda (MAP),y
    asl
    asl
    asl    
    sta R0

    lda #<(charset)
    clc
    adc R0
    sta TILEMASK_C1

    lda #>(charset)
    adc #0
    sta TILEMASK_C1+1


    lda TILEMASK_C1
    clc
    adc temp2
    sta TILEMASK_C1

    lda TILEMASK_C1+1
    adc #0
    sta TILEMASK_C1+1


///////////////// seconda riga
    lda MAP
    clc
    adc #40
    sta MAP

    lda MAP+1
    adc #0
    sta MAP+1

    lda temp
    tay

    lda (MAP),y
    asl
    asl
    asl    
    sta R0

    lda #<(charset)
    clc
    adc R0
    sta TILEMASK_A2

    lda #>(charset)
    adc #0
    sta TILEMASK_A2+1

    iny
    lda (MAP),y
    asl
    asl
    asl    
    sta R0

    lda #<(charset)
    clc
    adc R0
    sta TILEMASK_B2

    lda #>(charset)
    adc #0
    sta TILEMASK_B2+1


    iny
    lda (MAP),y
    asl
    asl
    asl    
    sta R0

    lda #<(charset)
    clc
    adc R0
    sta TILEMASK_C2

    lda #>(charset)
    adc #0
    sta TILEMASK_C2+1

///////////////// terza riga
    lda MAP
    clc
    adc #40
    sta MAP

    lda MAP+1
    adc #0
    sta MAP+1

    lda temp
    tay

    lda (MAP),y
    asl
    asl
    asl    
    sta R0

    lda #<(charset)
    clc
    adc R0
    sta TILEMASK_A3

    lda #>(charset)
    adc #0
    sta TILEMASK_A3+1

    iny
    lda (MAP),y
    asl
    asl
    asl    
    sta R0

    lda #<(charset)
    clc
    adc R0
    sta TILEMASK_B3

    lda #>(charset)
    adc #0
    sta TILEMASK_B3+1


    iny
    lda (MAP),y
    asl
    asl
    asl    
    sta R0

    lda #<(charset)
    clc
    adc R0
    sta TILEMASK_C3

    lda #>(charset)
    adc #0
    sta TILEMASK_C3+1
end:    
    rts
}



draw_bitmap_16x16:{
    
    stx save_x

    /*lda #<(charset+3*8)
    sta TILEMASK
    lda #>(charset+3*8)
    sta TILEMASK+1

    lda py0
    and #7
    sta temp

    lda TILEMASK
    clc
    adc temp
    sta TILEMASK_MIDDLE

    lda TILEMASK+1
    adc #0
    sta TILEMASK_MIDDLE+1

*/

    // BASE = floor(py0/8)*320
    ldy py0
    lda screen__lo,y
    sta BASE
    lda screen__hi,y
    sta BASE+1

    // SOURCE = bitmap1
   
    lda #$EA
 .for (var i=0;i<8;i++) {
    sta patch+i
    sta patch1+i
 }

    lda px0
    and #7
    beq no_patch

    tay
    
!:  
    lda #$4A  // LSR
    sta patch1,y

    dey
    bne !-


    lda px0
    and #7    
    sta R0
    beq no_patch
    lda #8
    sec
    sbc R0

    tay
    
!:  
    lda #$0A  // asl
    sta patch,y

    dey
    bne !-

no_patch:

   ldy #0
next:
    
    lda (SOURCE),y   
    //eor #$ff
patch:
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    sta data_1,y

    lda (SOURCE),y  
patch1:
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    
    ora data_1,y
    //eor #$ff
    sta data_1,y
    iny
    cpy #32
    bne next

    // if (px0_h !=0)  BASE+=0x100
    lda px0_h
    beq !+

    lda BASE+1
    clc
    adc #1
    sta BASE+1
    
    
!:    

    lda px0
    and #7
    tay
    lda mask,y
    sta mask_1

    lda mask_r,y
    sta mask_2


    // BASE = BASE + floor(px0/8)*8
    ldy px0
    lda col,y
    clc 
    adc BASE
    sta BASE

    lda BASE+1
    adc #0
    sta BASE+1
        
    // R0 = py0&7; R1=R0 ~ 7; R1++
    lda py0
    and #7
    sta R0
    eor #7
    sta R1
    inc R1

    // r2 =2

//////////////////////////////
/////////// DRAW FIRST COL
/////////////////////////////
    // DEST = BASE + R0
    lda BASE
    clc
    adc R0
    sta DEST

    lda BASE+1    
    adc #0
    sta DEST+1

    ldx #0
    
    // memcpy DEST,SOURCE,R1
    ldy #0
!:
    lda data_1,x
    and mask_1
    ora  (DEST),y
    //and (TILEMASK_MIDDLE),y
    and (TILEMASK_A1),y
    sta (DEST),y
    inx
    iny   
    cpy R1
    bne !-
    
    // DEST=BASE+320
    lda BASE
    clc
    adc #<320
    sta DEST

    lda BASE+1    
    adc #>320
    sta DEST+1

    // memcpy DEST,SOURCE,8
    ldy #0      
!:
    lda data_1,x 
    and mask_1
    ora  (DEST),y
    and (TILEMASK_A2),y
    sta (DEST),y

    inx
    iny   
    cpy #8
    bne !-
    
    // if r0==0 then goto no_third_row
    lda R0
    beq draw_second_col

    // DEST = BASE + 640
    lda BASE
    clc
    adc #<640
    sta DEST

    lda BASE+1    
    adc #>640
    sta DEST+1

    // memcpy DEST,SOURCE,R0
    ldy #0
!:
    lda data_1,x    
    and mask_1     
    ora  (DEST),y
    //and (TILEMASK),y
    and (TILEMASK_A3),y
    sta (DEST),y

    inx
    iny   
    cpy R0
    bne !-


draw_second_col:

//////////////////////////////
/////////// DRAW SECOND COL
/////////////////////////////

    // BASE = BASE +8
    lda BASE
    clc
    adc #8
    sta BASE

    lda BASE+1    
    adc #0
    sta BASE+1


    // DEST = BASE + R0
    lda BASE
    clc
    adc R0
    sta DEST

    lda BASE+1    
    adc #0
    sta DEST+1

    // memcpy DEST,SOURCE,R1
    ldx #0
    ldy #0
!:
    lda data_1,x
    and mask_2
    sta $90

    lda data_1+16,x
    and mask_1
    ora $90
    ora  (DEST),y
    //and (TILEMASK_MIDDLE),y
    and (TILEMASK_B1),y
    sta (DEST),y

    inx
    iny   
    cpy R1
    bne !-
    
    // DEST=BASE+320
    lda BASE
    clc
    adc #<320
    sta DEST

    lda BASE+1    
    adc #>320
    sta DEST+1

    // memcpy DEST,SOURCE,8
     ldy #0
!:
    lda data_1,x   
    and mask_2
    sta $90

    lda data_1+16,x
    and mask_1
    ora $90

    ora  (DEST),y
    //and (TILEMASK),y
    and (TILEMASK_B2),y
    sta (DEST),y

    inx
    iny   
    cpy #8
    bne !-
    
    // if r0==0 then goto no_third_row
    lda R0
    beq draw_third_col

    // DEST = BASE + 640
    lda BASE
    clc
    adc #<640
    sta DEST

    lda BASE+1    
    adc #>640
    sta DEST+1

    // memcpy DEST,SOURCE,R0
    ldy #0
!:        
    lda data_1,x
    and mask_2
    sta $90

    lda data_1+16,x  
    and mask_1
    ora $90

    ora  (DEST),y
    //and (TILEMASK),y
    and (TILEMASK_B3),y
    sta (DEST),y

    inx
    iny   
    cpy R0
    bne !-

draw_third_col:
    
    lda px0
    and #7
    beq end


//////////////////////////////
/////////// DRAW THIRD ROW
/////////////////////////////

    // BASE = BASE +8
    lda BASE
    clc
    adc #8
    sta BASE

    lda BASE+1    
    adc #0
    sta BASE+1


    // DEST = BASE + R0
    lda BASE
    clc
    adc R0
    sta DEST

    lda BASE+1    
    adc #0
    sta DEST+1

    // memcpy DEST,SOURCE,R1
    ldx #0
    ldy #0
!:
    lda data_1+16,x
    and mask_2
    ora  (DEST),y

    //and (TILEMASK_MIDDLE),y
    and (TILEMASK_C1),y
    sta (DEST),y

    inx
    iny   
    cpy R1
    bne !-
    
    // DEST=BASE+320
    lda BASE
    clc
    adc #<320
    sta DEST

    lda BASE+1    
    adc #>320
    sta DEST+1

    // memcpy DEST,SOURCE,8
    ldy #0
!:
    lda data_1+16,x
    and mask_2

    ora  (DEST),y
    //and (TILEMASK),y
    and (TILEMASK_C2),y
    sta (DEST),y

    inx
    iny   
    cpy #8
    bne !-
    
    // if r0==0 then goto end
    lda R0
    beq end

    // DEST = BASE + 640
    lda BASE
    clc
    adc #<640
    sta DEST

    lda BASE+1    
    adc #>640
    sta DEST+1

    // memcpy DEST,SOURCE,R0
    ldy #0
!:
    lda data_1+16,x
    and mask_2     
    ora (DEST),y
    //and (TILEMASK),y
    and (TILEMASK_C3),y
    sta (DEST),y

    inx
    iny   
    cpy R0
    bne !-
end:
!:
    ldx save_x
    rts
    
}





rect:{    
    ldy py0

    lda screen_lo,y
    sta PIXEL
    lda screen_hi,y
    sta PIXEL+1

    lda px0_h
    beq !+

    /*lda px0
    sbc #32
    sta px0
*/
    lda PIXEL
    clc
    adc #$00
    sta PIXEL
    
    lda PIXEL+1
    adc #1
    sta PIXEL+1
    
    
!:    

    ldy px0
    lda col,y
    sta temp
    
    
    lda px0
    and #7
    tay
    lda mask_r,y    
    ldy temp    
    ora (PIXEL),y
    sta (PIXEL),y

    lda PIXEL
    clc 
    adc #8
    sta PIXEL

    lda PIXEL+1
    adc #0
    sta PIXEL+1

    ldy temp
    lda #$00
    sta (PIXEL),y

    lda PIXEL
    clc 
    adc #8
    sta PIXEL

    lda PIXEL+1
    adc #0
    sta PIXEL+1

    lda px0
    and #7
    beq !+

    tay
    lda mask,y
    ldy temp
    ora (PIXEL),y
    sta (PIXEL),y

!:
    rts
    
}


check_pixel_coll:{

    lda #0
    sta temp
        
    lda py0
    lsr
    lsr
    lsr
    tay

    lda map_lo,y
    sta PIXEL
    lda map_hi,y
    sta PIXEL+1


    lda px0_h
    beq !+

    lda #32
    sta temp

!:
    lda px0
    lsr
    lsr
    lsr   
    //tay
    //sty temp
    clc
    adc temp
    sta temp

    tay


    lda (PIXEL),y

    rts
    
}
check_coll:{

    lda #0
    sta temp
        
    lda py0
    cmp #200-24
    bcc !+  //; if NUM1L < NUM2L then NUM1 < NUM2
    clc
    adc #8
    cmp #200-24
    bcc !+  //; if NUM1L < NUM2L then NUM1 < NUM2
    clc
    adc #8
    cmp #200-24
    bcc !+
    lda #0
    rts
!:
    lsr
    lsr
    lsr
    tay

    lda map_lo,y
    sta PIXEL
    lda map_hi,y
    sta PIXEL+1


    lda px0_h
    beq !+

    lda #32
    sta temp

!:
    lda px0
    lsr
    lsr
    lsr   
    //tay
    //sty temp
    clc
    adc temp
    sta temp

    tay

/// riga 1
    lda (PIXEL),y
    bne !+

    iny
    lda (PIXEL),y    
    bne !+

    lda px0
    and #7
    beq row2

    iny
    lda (PIXEL),y
    bne !+



////riga 2

row2:
    lda temp
    clc
    adc #40
    tay

    lda (PIXEL),y
    bne !+

    iny
    lda (PIXEL),y
    bne !+

    lda px0
    and #7
    beq row3

    iny
    lda (PIXEL),y
    bne !+

row3:
    lda py0
    and #7
    beq !+

    // riga 3
    lda temp
    clc
    adc #80
    tay

    lda (PIXEL),y
    bne !+

    iny
    lda (PIXEL),y
    bne !+

    lda px0
    and #7
    beq !+

    iny
    lda (PIXEL),y    

!:
    sta coll_code
    rts
    
}
draw_screen: {
    
    lda #<(bank+$2000)
    sta PIXEL
    lda #>(bank+$2000)
    sta PIXEL+1

.for (var i=0;i<4;i++) {
    ldx #0
loop:
    lda map+i*$100,x
    beq !+
    lda #$ff
    

    ldy #0
    lda #$00

    sta (PIXEL),y
    iny
    sta (PIXEL),y
    iny
    sta (PIXEL),y
    iny
    sta (PIXEL),y
    iny
    sta (PIXEL),y
    iny
    sta (PIXEL),y
    iny
    sta (PIXEL),y
    iny
    sta (PIXEL),y

!:
    lda PIXEL
    clc
    adc #8
    sta PIXEL
    lda PIXEL+1
    adc #0
    sta PIXEL+1
    inx
    bne loop
}
    rts
}

clearscreen: {    
    ldy #0
!:
lda #$ff
.for (var addr=$2000;addr<$3fff;addr+=$100)
        sta bank+addr,y

lda #clearcolor
.for (var addr=$400;addr<$700;addr+=$100)
        sta bank+addr,y


    iny
    bne !-


   ldy #0
!:
   lda #clearcolor
   sta bank+$700,y
   iny
   cpy #$f8
   bne !-
   rts
}
clear_map:{
    ldy #0
    lda #0
!:
    sta map,y
    sta map+$100,y
    sta map+$200,y
    sta map+$300,y
    iny
    bne !-
    rts
}
collide_h_flag:
.byte 0
main: {
    

    /*lda #6
    sta $1
*/
    SetVICBank2() //          ;  bank from $8000–$BFFF


    lda #$3b
    sta $d011   

    lda #$18   
    sta $d018

    /*lda #$18
    sta $d016
*/
    lda #0
    sta $d020
    
    lda #$0
    sta bank+$0400+$03f8
    lda #1
    sta bank+$0400+$03f9
    sta bank+$0400+$03fa
    sta bank+$0400+$03fb
    sta bank+$0400+$03fc
    sta bank+$0400+$03fd
    sta bank+$0400+$03fe

    lda #$80
    sta $d000
    sta $d001
    
    lda #$10
    sta model_x
    sta model_y

    lda #32
    sta model_y

    lda #1
    sta $d015


    jsr clearscreen
    jsr draw_title_screen
!:
    jsr getjoy
  	lda joy
	and #$10
    beq !-
    jsr clearscreen
    jsr copy_sprites
   
    ldy cur_level
    lda level_table_lo,y
    sta LEVEL
    lda level_table_hi,y
    sta LEVEL+1
    jsr loadlevel
    //fillbox(0,22,40,2)

    jsr draw_screen
    lda #0
    sta level_ok
    sta level_restart

    lda #music.startSong-1
    jsr music.init


    lda #0
    sta px0_h
    
    /*.for (var i=0;i<5;i++) {
        lda #0
        sta py0

        lda #<(32+i+i*32)
        sta px0
        lda #>(32+i+i*32)
        sta px0_h

        jsr draw_bitmap_16x16

    }*/
    

   
   /* .for (var i=0;i<40;i++) {
        lda #i
        sta px0
        lda #12
        sta py0
        jsr draw_tile
    }*/

    jsr init_raster
!:
    //lda #0
    //sta $d020
    
loop:
    //jsr vblank
    //lda #1
    //sta $d020

    lda  level_ok
    bne next_level
    lda level_restart
    bne next_level
    //jsr music.play 

    lda effectid
    beq noeffect
    ldx effectid
    lda sndfx_table_lo,x        //Start address of sound effect data
    ldy sndfx_table_hi,x
    ldx #14
    jsr music.location+6
    lda #0
    sta effectid
noeffect:
    //jsr vblank
  
    jsr update_tears
    jsr getjoy
    ldx #1
    jsr update_anim
    jsr update_player
    jsr update_enemy
    
    jsr check_collision
    




    /*lda frame_count
    cmp saved_frame_count
    beq !+    
    sta saved_frame_count
!: */
    
ww:

    lda frame_ready
    bne ww
    
    
    jsr draw_tears
    
    
    //lda #0
    //sta $d020
    
    lda #1
    sta frame_ready
    
    
    

    jmp loop
    rts

next_level:
    ldx #3
    lda sndfx_table_lo,x        //Start address of sound effect data
    ldy sndfx_table_hi,x
    ldx #14
    jsr music.location+6


    waitsec(1)
    jsr clear_map
    jsr clearscreen
    
    lda level_restart
    bne !+
    inc cur_level
!:


    lda cur_level
    cmp #3
    bne !+
    lda #0
    sta cur_level
!:
    ldy cur_level
    lda level_table_lo,y
    sta LEVEL
    lda level_table_hi,y
    sta LEVEL+1
    //fillbox(0,22,40,2)
    jsr loadlevel

    lda #1
    sta pre_up

    lda #0
    sta state

    jsr draw_screen
    lda #0
    sta level_ok
    sta level_restart
    sta jump_idx
    jmp loop
}


copy_sprites: {
    ldy #0
!:
	lda sprites,y  
	sta bank,y
	lda sprites+$100,y  
	sta bank+$100,y
	lda sprites+$200,y  
	sta bank+$200,y

  	lda sprites+$300,y  
	sta bank+$300,y

    lda gaetan_music,y
    sta music.location,y
    lda gaetan_music+$100,y
    sta music.location+$100,y
    lda gaetan_music+$200,y
    sta music.location+$200,y
    lda gaetan_music+$300,y
    sta music.location+$300,y
    lda gaetan_music+$400,y
    sta music.location+$400,y
    lda gaetan_music+$500,y
    sta music.location+$500,y
    lda gaetan_music+$600,y
    sta music.location+$600,y
    lda gaetan_music+$700,y
    sta music.location+$700,y
    lda gaetan_music+$800,y
    sta music.location+$800,y
    lda gaetan_music+$900,y
    sta music.location+$900,y
    lda gaetan_music+$a00,y
    sta music.location+$a00,y
    lda gaetan_music+$b00,y
    sta music.location+$b00,y







    iny
    bne !-

    rts
}
up_button:
    .byte 0
pre_up:
    .byte 0

spanw_tears:
    .byte $0
.var TOT=$7

alloc_tears: {

    lda #3
    sta effectid     

    lda #TOT-1
    sta spanw_tears 

    
    lda model_x
    clc
    adc #8
    sta px0

    lda model_x_h
    adc #0
    sta px0_h

    lda model_y
    sta py0

    lda px0
    .for (var i=2;i<=TOT;i++) {        
        sta model_x+i
    }
    lda px0_h
    .for (var i=2;i<=TOT;i++) {        
        sta model_x_h+i
    }

    lda py0
    .for (var i=2;i<=TOT;i++) {        
        sta model_y+i
    }

    lda #1
    .for (var i=2;i<=TOT;i++) {        
       sta model_state+i       
       sta model_img+i

    }

    lda #0
    .for (var i=2;i<=TOT;i++) {        
        sta model_path_idx+i
        sta model_anim+i
        sta model_frame_id+i
        sta model_frame+i
    }

    
    .for (var i=2;i<=TOT;i++) {        
        .if (i==2) {
            lda #4
            sta model_vx+i
            lda #0
            sta model_vx_h+i
        } 
        .if (i==3) {
            lda #2
            sta model_vx+i
            lda #0
            sta model_vx_h+i
        }
        .if (i==4) {
            lda #1
            sta model_vx+i
            lda #0
            sta model_vx_h+i
        } 
        .if (i==5) {
            lda #-4
            sta model_vx+i
            lda #$ff
            sta model_vx_h+i
        }
         .if (i==6) {
            lda #-2
            sta model_vx+i
            lda #$ff
            sta model_vx_h+i
        }
        .if (i==7) {
            lda #-1
            sta model_vx+i
            lda #$ff
            sta model_vx_h+i
        }

    }
    rts

}
/*update_tears:{
    lda spanw_tears
    bne !+
    rts
!:
    
move_tears:
    
    lda #<(bitmap1+32*4)
    sta SOURCE
    lda #>(bitmap1+32*4)
    sta SOURCE+1
    
    
    .for (var i=2;i<=TOT;i++) {
        ldx #i
        jsr update_anim
        lda model_state+i
        bne !+
        jmp next_tear
!:
        lda #4
        sta model_vy+i

        ldy model_path_idx+i
        lda jump_tear_table,y
        cmp #$ff
        beq !+
        sta model_vy+i
        inc model_path_idx+i
!:

       
        lda model_x+i
        clc
        adc model_vx+i
        sta model_x+i        
        sta px0
        

        lda model_x_h+i
        adc model_vx_h+i
        sta model_x_h+i                
        sta px0_h
        
        
        lda model_x_h+i
        cmp #$ff 
        bne L30
        lda #0
        sta model_state+i
        dec spanw_tears
        jmp next_tear

L30:
//// check boundary
        lda model_y+i
        cmp #200
        bcs del
        lda model_x_h+i
        cmp #>(319)
        bcc L31
        lda model_x+i
        cmp #<(319)
        bcc L31
del:
        lda #0
        sta model_state+i
        dec spanw_tears
        jmp next_tear


L31:
        lda model_y+i
        sta py0

        jsr check_pixel_coll
        beq !+
        //// horizontal collision

        lda model_vx+i
        bpl L123

        lda px0
        sec
        sbc #4   
        sta px0

        lda px0_h
        sbc #0
        sta px0_h

L123:
        jsr rect_col
        jsr prepare_mask
        jsr draw_bitmap_16x16

        lda #0
        sta model_state+i
        dec spanw_tears
        jmp next_tear
!:

        lda model_y+i
        clc
        adc model_vy+i
        sta py0
        sta model_y+i

        lda model_x+i
        sta px0    
        lda model_x_h+i
        sta px0_h    

        jsr check_pixel_coll
        beq next_tear
        
        //// vertical collision
        lda model_vy+i
        bpl L124

        lda py0
        sec
        sbc #8   
        sta py0        
L124:

        jsr rect_col
        jsr prepare_mask
        jsr draw_bitmap_16x16   
        lda #0
        sta model_state+i
        dec spanw_tears

next_tear:

    }

    rts
}*/
enemy_dir_l:
    .byte <-1,<1
enemy_dir_h:
    .byte >-1,>1
turn:
    .byte 8
update_enemy:{
    //i=8
    //if (frame_count%2!=0) return
    
    lda frame_count
    and #$1
    bne !+
    rts
!:



    ldx #8
loop:
    //if (model_state[i]==0) goto next_enemy
    lda model_state,x
    bne !+
    jmp next_enemy
!:
    //d = model_dir[i]
    ldy model_dir,x
    //model_vx[i] = enemy_dir[d]
    lda enemy_dir_l,y
    sta model_vx,x
    lda enemy_dir_h,y
    sta model_vx_h,x

    //px0=model_x[i]+model_vx[i]
    lda model_x,x
    clc
    adc model_vx,x    
    sta px0

    lda model_x_h,x
    adc model_vx_h,x
    sta px0_h
    
    lda model_y,x
    clc
    adc #16
    sta py0

    
    //if (check_pixel_coll(model_x[i],model_y[i]+16)==0) goto change_dir
    
    /*jsr check_pixel_coll
    bne !+
    jmp change_dir
*/
    // px0+=8    
    lda px0
    clc
    adc #8
    sta px0

    lda px0_h
    adc #0
    sta px0_h


    //if (check_pixel_coll(model_x[i]-8,model_y[i]+8)!=0) goto move_enemy

    jsr check_pixel_coll
    beq !+
    jmp move_enemy

!:
change_dir:            
    //model_dir[i]^=1
    lda model_dir,x
    eor #1
    sta model_dir,x
    setanim(2)
    jsr update_anim
    // goto next_enemy
    jmp next_enemy
move_enemy:

    //model_x[i]+=model_vx[i]
    select_bitmap(5)
    lda px0
    sec
    sbc #8
    sta px0
    lda px0_h
    sbc #0
    sta px0_h
    
    cpx turn
    bne no_splash

    jsr rect_col
    jsr prepare_mask
    jsr draw_bitmap_16x16
no_splash:
    jsr update_anim
    //if (update_anim==0) {
        bne !+
        setanim(1)

        lda model_x,x
        clc
        adc model_vx,x
        sta model_x,x

        lda model_x_h,x
        adc model_vx_h,x
        sta model_x_h,x

    
    //}
!:
next_enemy:
    //i=i+1
    inx
    //if (i<16) to goto loop
    cpx #16
    bcs !+
    jmp loop
!:
    inc turn
    lda turn
    cmp #16
    bcc !+
    lda #8
    sta turn
!:
    rts
}
//proc
update_tears:{
    /// i:REGX
  
    // if (spanw_tears==0) return

    ldx #2
!:
    lda model_state,x
    sta prev_model_state,x

    lda model_x,x
    sta prev_model_x,x

    lda model_x_h,x
    sta prev_model_x_h,x

    lda model_y,x
    sta prev_model_y,x
    inx
    cpx #8
    bne !-

    lda spanw_tears
    bne !+
    rts
!:

    // i=2
    ldx #2
    //sta i
loop:        
    // if (model_state[i]==0) goto next_tear
    lda model_state,x
    bne !+ 
    jmp next_tear
    
!:    
    // model_vy[i]=4;
    lda #4
    sta model_vy,x

    //idx:REGY
    //idx = model_path_idx[i];
    ldy model_path_idx,x

    //if (jump_tear_table[idx]!=255) {
        lda jump_tear_table,y
        cmp #$ff
        beq !+

        //model_vy[i]=jump_tear_table[idx];
        
        lda jump_tear_table,y
        sta model_vy,x
        
        lda jump_tear_table_hi,y
        sta model_vy_h

        //model_path_idx[i]++
        inc model_path_idx,x
    //}
!:
    //model_y[i]+=model_vy[i]
    lda model_y,x
    clc
    adc model_vy,x
    sta model_y,x

    //if (model_y[i]<=0 || model_y[i]>=200) goto remove_tear
    lda model_y,x
    cmp #200
    bcs remove_tear

    //if (check_pixel_coll (model_x[i],model_y[i])) then goto collision_detected
    lda model_y,x
    sta py0
    lda model_x,x
    sta px0
    lda model_x_h,x
    sta px0_h
    jsr check_pixel_coll
    beq !+
    jmp collision_detected

!:
    //model_x[i]+=model_vx[i];
    lda model_x,x
    clc
    adc model_vx,x
    sta model_x,x

    lda model_x_h,x
    adc model_vx_h,x
    sta model_x_h,x

    //if (model_x[i]<0 || model_y[i]>320)  goto remove_tear
    lda model_x_h,x
    bmi remove_tear

    lda model_x_h,x
    beq !+
    cmp #2
    bcs remove_tear
    lda model_x,x
    cmp #$40
    bcs remove_tear
!:

    //if (check_pixel_coll (model_x[i],model_y[i])==0) goto anim_tear
    lda model_x,x
    sta px0
    lda model_x_h,x
    sta px0_h
    jsr check_pixel_coll
    bne !+ 
    jmp anim_tear
!:
collision_detected:
    select_bitmap(4)

    lda model_vx_h,x
    bpl !+
    lda px0
    sec
    sbc #16
    sta px0

    lda px0_h    
    sbc #0
    sta px0_h
    
!:
    
    jsr rect_col
    jsr prepare_mask
    jsr draw_bitmap_16x16

remove_tear:
    //model_sta[i]=0;
    lda #0
    sta model_state,x
    
    //spanw_tears--
    dec spanw_tears

    //if (spanw_tears==0) goto end
    /*lda spanw_tears
    bne !+
    jmp end*/
!:
anim_tear:
    //if (model_state[i]!=0) update_anim()
    lda model_state,x
    beq !+
    jsr update_anim
!:
next_tear:
    //i=i+1
    inx
    //if (i<8) goto loop
    cpx #8
    bcs !+
    jmp loop

!:    

end:
    rts
}

hw_sprite_count:
    .byte 0
hw_sprite_list:
.fill 16,0
hw_sprite_y:
.fill 16,0
hw_sprite_x:
.fill 16,0
hw_sprite_x_h:
.fill 16,0
.fill 16,0
hw_sprite_img:
.fill 16,0

tear_pixel:
    .byte 0
tear_color:
    .byte 0
tear_pixel1:
.byte 0

erase_pixel_tear: {
    lda #$ff   
    sta tear_pixel1

    lda #$00
    sta tear_color
    ldx #2    
loop:
    lda prev_model_state,x
    bne !+
    jmp next_tear
!:
    lda #0
    sta temp2
  //// change color
    lda prev_model_y,x
    lsr
    lsr
    lsr
    tay 

    lda coltable_lo,y
    sta PIXEL
    lda coltable_hi,y
    sta PIXEL+1

    lda map_lo,y
    sta MAP
    lda map_hi,y
    sta MAP+1

    lda prev_model_x_h,x
    beq !+

    lda #32
    sta temp2
!:

    lda prev_model_x,x
    lsr
    lsr
    lsr
    clc 
    adc temp2
  //  sta temp

    tay
    /*lda (MAP),y
    bne next_tear*/
/*    lda (PIXEL),y
    bne next_tear*/
    lda #$00
    sta (PIXEL),y
  /*  lda model_x,x
    and #7
    tay
    lda mask_p,y    
    sta tear_pixel1
*/
    lda prev_model_y,x
    tay    
    lda screen__lo,y
    sta BASE
    lda screen__hi,y
    sta BASE+1

    lda prev_model_x_h,x
    beq !+

    lda BASE+1
    clc
    adc #1
    sta BASE+1
!:
    lda prev_model_x,x
    tay    

    lda BASE
    clc
    adc col,y
    sta BASE

    lda BASE+1
    adc #0
    sta BASE+1

    
    lda prev_model_y,x
    and #7
    tay
    lda #$ff
    sta (BASE),y

   

next_tear:    
    inx
    cpx #8
    beq !+
    jmp loop
!:
    rts
}

draw_pixel_tear: {
    
    lda #$01
    sta tear_color

    ldx #2    
loop:
    lda model_state,x
    bne !+
    jmp next_tear
!:

    lda #0
    sta temp2

    //// change color
    lda model_y,x
    lsr
    lsr
    lsr
    tay 

    lda coltable_lo,y
    sta PIXEL
    lda coltable_hi,y
    sta PIXEL+1

    lda map_lo,y
    sta MAP
    lda map_hi,y
    sta MAP+1

    lda model_x_h,x
    beq !+

    lda #32
    sta temp2
!:

    lda model_x,x
    lsr
    lsr
    lsr
    clc 
    adc temp2
  //  sta temp

    sta temp1
    tay
    lda (MAP),y
    bne next_tear
    lda (PIXEL),y
    bne next_tear
//// draw pize


    lda model_x,x
    and #7
    tay
    lda mask_p,y
    eor #$ff
    sta tear_pixel1

    lda model_y,x
    tay    
    lda screen__lo,y
    sta BASE
    lda screen__hi,y
    sta BASE+1

    lda model_x_h,x
    beq !+

    lda BASE+1
    clc
    adc #1
    sta BASE+1
!:
    lda model_x,x
    tay    

    lda BASE
    clc
    adc col,y
    sta BASE

    lda BASE+1
    adc #0
    sta BASE+1

    lda model_y,x
    and #7
    tay
    
    
    lda tear_pixel1
    sta (BASE),y

    lda tear_color
    ldy temp1
    sta (PIXEL),y

    //iny
    //sta (BASE),y


next_tear:    
    inx
    cpx #8
    beq !+
    jmp  loop
!:

    rts
}


draw_tears: {    
    
    
    lda #0
	sta bucketIdx

    ldx #0
    lda model_y,x

    jsr add_model_to_bucket_by_ypos

    ldx #1
    lda model_y,x
    jsr add_model_to_bucket_by_ypos


    ldx #8
n1:
    lda model_state,x
    beq !+
    lda model_y,x
    jsr add_model_to_bucket_by_ypos
!:
    inx
    cpx #16
    bne n1

    /*.for (var i=2;i<=TOT;i++) {

        lda model_state+i
        beq !+
        ldx #i
        lda model_y+i
        jsr add_model_to_bucket_by_ypos
    !:
            
    } */   
    jsr model_sort
   // jsr vblank
 
    rts
}
len:
.byte 0
len_h:
.byte 0
check_collision: {
    lda model_x_h
    bpl !+
    rts
!:
    ldx #8
loop:
    lda model_state,x
    bne !+
    jmp next_enemy
!:
        /////////////////// ASSE X
    //var length:Number = box2.x - box1.x;
    lda model_x,x
    sec
    sbc model_x
    sta len

    lda model_x_h,x
    sbc model_x_h
    sta len_h


    //var half_width_box1:Number = box1.width*0.5;
    //var half_width_box2:Number = box2.width*0.5;
 
    //var gap_between_boxes:Number = length - half_width_box1 - half_width_box2;

   /* lda len
    sec 
    sbc #16
    sta len

    lda len_h
    sbc #0
    sta len_h
*/
    
/*    if(gap_between_boxes > 0) trace("It's a big gap between boxes")
    else if(gap_between_boxes == 0) trace("Boxes are touching each other")
    else if(gap_between_boxes < 0) trace("Boxes are penetrating each other")
*/
  
    lda len_h
    
    bpl !+
    lda #0
    sec
    sbc len
    sta len
    
    lda #0
    sbc len_h
    sta len_h

   // jmp next_enemy
!:
    lda len
    cmp #$10
    bcc cont
    jmp next_enemy

cont:
/////////////////////////// ASSE Y
 //var length:Number = box2.x - box1.x;
    
    lda model_y,x
    sec
    sbc model_y
    sta len


    //var half_width_box1:Number = box1.width*0.5;
    //var half_width_box2:Number = box2.width*0.5;
 
    //var gap_between_boxes:Number = length - half_width_box1 - half_width_box2;

   /* lda len
    sec 
    sbc #16
    sta len

*/
    
/*    if(gap_between_boxes > 0) trace("It's a big gap between boxes")
    else if(gap_between_boxes == 0) trace("Boxes are touching each other")
    else if(gap_between_boxes < 0) trace("Boxes are penetrating each other")
*/
    lda len
    bpl !+
    lda #0
    sec
    sbc len
    sta len     
!:
    cmp #$10
    bcs next_enemy
    lda len
    cmp #$0C
    bcc death
    lda #0
    sta model_state,x
    lda #1
    sta effectid
    dec num_nemici

    lda #0
    sta collide_h_flag
    lda #Jump
    sta state

    lda #10
    sta jump_idx

    setanim(6)


next_enemy:
    inx
    cpx #16
    beq !+
    jmp loop
!:
    rts
death:
    jmp livello_da_ripetere
}
update_anim:   {
    // x contains model id
   

    lda model_anim,x
    tay
    lda anims_lo,y
    sta R0
    lda anims_hi,y
    sta R0+1

    ldy #0
    lda (R0),y
    sta frame_delay
    iny
    lda (R0),y
    sta num_frame

    lda model_frame_id,x
    tay
    iny
    iny
    lda (R0),y
    sta model_frame,x

    inc model_delay,x
    lda model_delay,x
    cmp frame_delay
    bcc nothing_to_do
    lda #0
    sta model_delay,x
    inc model_frame_id,x
    lda model_frame_id,x
    cmp num_frame
    beq nothing_to_do
    bcc nothing_to_do
    lda num_frame
    sta model_frame_id,x
nothing_to_do:
    lda model_frame,x

    /// moltiplica per 10    
    asl         //multiply by 2
    sta temp    //temp store in TEMP
    asl         //again multiply by 2 (*4)
    asl         //again multiply by 2 (*8)
    clc
    adc temp    // as result, A = x*8 + x*2

    tay

// type,offsx,offsy,width,height,img1,img2,img3,img4,flag
    lda frames,y    
    lda frames+1,y
    sta frame_offs_x    
    sta model_frame_offsx,x
    lda frames+2,y
    sta frame_offs_y
    sta model_frame_offsy,x
    lda frames+3,y
    sta frame_width
    lda frames+4,y
    sta frame_height
    lda frames+5,y
    sta frame_img
    sta model_img,x
    
    lda model_frame_id,x
    cmp num_frame

    rts
    
    
}
was_on_ground:
.byte 0

check_out_of_screen: {
    lda px0_h  //; compare high bytes
    cmp #$ff
    bmi !+  //; if NUM1H < NUM2H then NUM1 < NUM2
    bne L1  //; if NUM1H <> NUM2H then NUM1 > NUM2 (so NUM1 >= NUM2)
    lda px0  //; compare low bytes
    cmp #$f0
    bmi !+  //; if NUM1L < NUM2L then NUM1 < NUM2
L1:
    lda #>(320)  //; compare high bytes
    cmp px0_h
    bmi !+  //; if NUM1H < NUM2H then NUM1 < NUM2
    bne L2  //; if NUM1H <> NUM2H then NUM1 > NUM2 (so NUM1 >= NUM2)
    lda #<(320)   //; compare low bytes
    cmp px0
    bmi !+  //; if NUM1L < NUM2L then NUM1 < NUM2
L2:
    lda py0_h  //; compare high bytes
/*    cmp #$ff
    bmi !+  //; if NUM1H < NUM2H then NUM1 < NUM2
    bne L3  //; if NUM1H <> NUM2H then NUM1 > NUM2 (so NUM1 >= NUM2)*/
    and #$80
    beq L3
    lda py0  //; compare low bytes
    cmp #$f0
    bmi !+  //; if NUM1L < NUM2L then NUM1 < NUM2
    jmp L4
L3:

    lda #>(200)  //; compare high bytes
    cmp py0_h
    bcc !+  //; if NUM1H < NUM2H then NUM1 < NUM2
    bne L4  //; if NUM1H <> NUM2H then NUM1 > NUM2 (so NUM1 >= NUM2)
    lda #<(200)   //; compare low bytes
    cmp py0
    bcc !+  //; if NUM1L < NUM2L then NUM1 < NUM2
L4:
    lda #0
    rts
!:      
    lda #1
    rts

}
update_player:{
    ldx #0
    stx px0_h
    stx py0_h
    //stx $d010

    try_change_anim(3)
    
    lda #0
    sta model_vx
    sta model_vx_h
    sta model_vy
    sta model_vy_h
    sta model_img

!:
	lda joy
	and #$10
    sta pre_up
	beq !+
    lda collide_h_flag
    bne L9
    lda state
    cmp #OnGround
    beq L9
    lda up_button
    bne !+
    lda spanw_tears
    bne !+   
    jsr alloc_tears
    lda #0
    sta collide_h_flag
    lda #Jump
    sta state
    lda #5
    sta jump_idx
    jmp !+
L9:    
    lda up_button
    bne !+
    lda #0
    sta collide_h_flag
    sta jump_idx
    lda #Jump
    sta state
    lda #1
    sta effectid

    setanim(6)
    //inc colore
    
    ldy idcolor
    lda tabella_colori,y
    //lda #$70
    sta colore
    
    inc idcolor
    lda idcolor
    and #$3
    sta idcolor

!:
    lda pre_up
    sta up_button
    lda #0
    sta collide_h_flag
	lda joy
	and #$4
	beq !+
    lda #-2
    sta model_vx,x
    lda #3
    sta model_img,x
    try_change_anim(4)

    lda #$ff
    sta model_vx_h,x
	jmp do

!:
	lda  joy
	and  #$8
    beq do
    lda #2
    sta model_vx,x
    lda #2
    sta model_img,x
    try_change_anim(5)
do:
    jsr update_anim
    lda model_x,x
    clc
    adc model_vx,x       
    sta px0

    lda model_x_h,x
    adc model_vx_h,x
    sta px0_h

    lda model_y,x
    sta py0
    lda model_y_h,x
    sta py0_h
    jsr check_out_of_screen
    beq L241
    jmp livello_da_ripetere
L241:
    jsr check_coll
    bne collide_h   
    lda px0
    sta model_x,x
    lda px0_h
    sta model_x_h,x
    jmp L5
collide_h:    
    lda coll_code
    cmp #2
    bne L141
    lda num_nemici
    bne L141
    jmp livello_completato
L141:
    /*lda coll_code
    cmp #3
    bne L142
    jsr rect_col
    jsr prepare_mask
    jsr draw_bitmap_16x16
    jmp livello_da_ripetere*/
L142:
    lda #1
    sta collide_h_flag

    lda model_x,x
    clc
    adc #16
    sta px0

    lda model_x_h
    adc #0
    sta px0_h

    select_bitmap(1)

    lda model_vx_h,x
    bpl L4

    lda model_x,x
    clc
    adc #-16
    sta px0

    lda model_x_h,x
    adc #$ff
    sta px0_h

    select_bitmap(2)


L4:    
    lda model_y,x
    sta py0
    jsr rect_col
    jsr prepare_mask
    jsr draw_bitmap_16x16

    lda coll_code
    cmp #3
    bne L5
    jmp livello_da_ripetere
L5:
    lda state
    cmp #Jump
    bne !+
    ldy jump_idx
    lda jump_table,y
    cmp #$ff
    beq !+
    sta model_vy,x
    and #$80
    beq L22
    lda #$ff
    sta model_vy_h,x
L22:

    inc jump_idx
    jmp  elab
    

!:
    lda #0
    sta was_on_ground
    lda state
    cmp #OnGround
    bne !+
    lda #1
    sta was_on_ground
!:
    lda #Falling
    sta state
    
    lda #2
    sta model_vy,x
    lda #0
    sta model_vy_h,x
    lda collide_h_flag
    beq elab
    lda #1
    sta model_vy,x
    lda #0
    sta model_vy_h,x
elab:
    lda model_vy,x
    beq draw_hw_sprites

    lda model_x,x
    sta px0
    lda model_x_h,x
    sta px0_h

    lda model_y,x
    clc    
    adc model_vy,x
    sta py0    

    lda model_y_h,x    
    adc model_vy_h,x
    sta py0_h


    jsr check_out_of_screen
    beq L242
    jmp livello_da_ripetere
L242:
    jsr check_coll
    bne collide_vert
    lda py0
    sta model_y,x
    lda py0_h
    sta model_y_h,x

draw_hw_sprites:    
/*draw_hw_sprites:
    ldx #0
    stx $d010

    lda model_x,x
    clc
    adc #24
    sta $d000

    lda model_x_h,x
    adc #0
    beq !+
    lda #1
    sta $D010       
!:

    lda model_y,x
    clc
    adc #50
    sta $d001
*/
    rts

collide_vert:
    lda coll_code
    cmp #2
    bne L98
    lda num_nemici
    bne L98
    jmp livello_completato
L98:
    lda coll_code
    cmp #3
    bne L99
    //jsr rect_col
    //jsr prepare_mask
    //jsr draw_bitmap_16x16
    //jmp livello_da_ripetere
L99:
    ldx #0    
    lda model_vy,x
    bmi L32
    lda model_y,x
    clc
    adc #3
    lsr
    lsr
    lsr
    asl
    asl
    asl
    sta model_y,x
L32:
    lda state
    cmp #Falling
    bne L2
    lda #OnGround
    sta state
    lda was_on_ground
    bne L1
    /*lda #2
    sta effectid
    */
    setanim(7)
    jmp L1
 L2:
    lda #Falling
    sta state

 L1:
    lda model_y,x
    clc
    adc #16
    sta py0

    lda model_x,x
    sta px0
    lda model_x_h,x
    sta px0_h

    select_bitmap(0)
    lda model_vy,x
    bpl L7
    lda model_y,x
    clc
    adc #-16
    sta py0

    select_bitmap(3)
L7:
    

    jsr rect_col
    jsr prepare_mask
    jsr draw_bitmap_16x16
 
    lda coll_code
    cmp #3
    bne L851
    jmp livello_da_ripetere
L851:
    jmp draw_hw_sprites

livello_completato:
    lda #1
    sta level_ok
    rts

}

livello_da_ripetere:
    
    ldx #3
    lda sndfx_table_lo,x        //Start address of sound effect data
    ldy sndfx_table_hi,x
    ldx #14
    jsr music.location+6

    waitsec(1)


    lda player_x
    sta model_x
    lda player_x_h
    sta model_x_h
    lda player_y_h
    sta model_y_h
    lda player_y
    sta model_y
    //lda #1
    //sta level_restart
    rts

vblank: {

	lda $d011
	bmi vblank
L2: 
	lda $d011
	bpl L2
	rts


}
joy:
.byte $0
getjoy: {
	lda     #$FF
	sta     $DC00
	lda     $DC00
	eor     #$FF
	sta     joy
	rts
}


/*.fill 1000-40,0
.fill 40,1
.fill 1000,0*/
//.import binary "map.bin"
draw_title_screen:{
    ldx #0
    lda #0
    sta px0_h
    
    
    sta temp

    lda #6
    sta py0

    lda #11
    sta px0
    lda #8
    sta R0
loop:
    lda ink_scr_map,x
    tay
    lda ink_scr_color,y
    asl
    asl
    asl
    asl
    and #$f0
    sta colore

    lda ink_scr_map,x
    jsr draw_tile_2
    inc px0
    
    inx
    inc temp
    lda temp
    cmp #18
    bne loop
    inc py0

    lda #11
    sta px0

    lda #0
    sta temp
    dec R0
    bne loop 

    rts
}
sprites:    
    .import binary "sprites2.bin"

state:
    .byte $0
jump_idx:
    .byte $0
jump_table:
//    .byte -4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-2,-2,-2,-2,-2,-2,-2,-2,-2,-2,-2,-2,0,0,0,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,$ff
     // .byte -4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-2,-2,-2,-2,-2,-2,-2,-2,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,$ff
 .byte -4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-2,-2,-2,-2,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,$ff     

jump_tear_table:
    .byte <-4,<-4,<-4,<-4,<-4,<-4,<-4,<-2,<-2,<-2,<-2,<0,<0,<0,<0,<0,<0,<0,<0,<0,<0,<0,<0,<2,<2,<2,<2,<2,<2,<2,<2,$ff            
jump_tear_table_hi:
    .byte >-4,>-4,>-4,>-4,>-4,>-4,>-4,>-2,>-2,>-2,>-2,>0,>0,>0,>0,>0,>0,>0,>0,>0,>0,>0,>0,>2,>2,>2,>2,>2,>2,>2,>2,$ff            
model_dir:
    .fill 20,$1 
model_img:
    .fill 20,$0
model_x:
    .fill 20,$0
model_x_h:
    .fill 20,$0
model_y:
    .fill 20,$0
model_y_h:
    .fill 20,$0
model_vx:
    .fill 20,$0
model_vx_h:
    .fill 20,$0

model_vy:
    .fill 20,$0
model_vy_h:
    .fill 20,$0
model_state:
    .fill 20,$0
model_path_idx:
    .fill 20,$0
model_anim:
    .fill 20,$0
model_frame:
    .fill 20,$0
model_frame_id:
    .fill 20,$0
model_delay:
    .fill 20,$0
model_frame_offsx:
    .fill 20,$0
model_frame_offsy:
    .fill 20,$0
player_x:
    .byte 0
player_y:
    .byte 0
player_x_h:
    .byte 0
player_y_h:
    .byte 0


prev_model_x:
    .fill 20,$0
prev_model_x_h:
    .fill 20,$0
prev_model_y:
    .fill 20,$0
prev_model_state:
    .fill 20,$0

frames: // list of frame
        // type,offsx,offsy,width,height,img1,img2,img3,img4,flag
.byte 0 ,24-0, 9, 2, 2, 0, 0, 0, 0, 0  /// particle ink
.byte 0 ,24-4, 4,16,21, 1, 0, 0, 0, 0  /// target
.byte 0 ,24-4, 0,16,21, 2, 0, 0, 0, 0  // target hit
.byte 0 ,24-4, 2,20,19, 3, 0, 0, 0, 0  // enemy frame 1
.byte 0 ,24-4, 0,18,21, 4, 0, 0, 0, 0  // enemy frame 2
.byte 0 ,24-4, 4,24,17, 5, 0, 0, 0, 0  // enemy frame 3
.byte 0 ,24-4, 4,16,16, 6, 0, 0, 0, 0  // player jump frame 1
.byte 0 ,24-4, 4,12,20, 7, 0, 0, 0, 0  // player jump frame 2
.byte 0 ,24-4, 4,24,12, 8, 0, 0, 0, 0  // player jump frame 3
.byte 0 ,24-4, 4,12,18, 9, 0 ,0 ,0 ,0  // player jump frame 4
.byte 0 ,24-4, 4, 8,21,10, 0 ,0 ,0 ,0  // player jump frame 5
.byte 0 ,24-4, 4,18,18,11, 0 ,0 ,0 ,0  // player move left
.byte 0 ,24-4, 4,18,18,12, 0 ,0 ,0 ,0  // player move right
.byte 0 ,24-4, 4,18,18,13, 0 ,0 ,0 ,0  // player additional left
.byte 0 ,24-4, 4,18,18,14, 0 ,0 ,0 ,0  // player addition right

    //anim table
    //frame delay , num. frames, sprite id, offsx,offsy
        
anim_ink_particle:
.byte 0,0,0    /// ink particle
anim_enemy_walk:
.byte 4,0,3    ///enemy walk
anim_enemy_change_dir:
.byte 2,4,3,4,5,5  ///enemy change dir
anim_player_idle:
.byte 0,0,6      // player idle
anim_player_move_right:
.byte 0,0,13     /// player move right
anim_player_move_left:
.byte 0,0,14     /// player move left
anim_player_jump:
.byte 8,7,7,8,7,6,9,10,9,6  // player jump
anim_player_land:
.byte 8,7,07,8,7,6,9,6,7,6  // player landed
anim_goal:
.byte 8,0,1  // player landed

anims_lo:    
    .byte <anim_ink_particle,<anim_enemy_walk,<anim_enemy_change_dir,<anim_player_idle,<anim_player_move_right,<anim_player_move_left,<anim_player_jump,<anim_player_land,<anim_goal
anims_hi:    
    .byte >anim_ink_particle,>anim_enemy_walk,>anim_enemy_change_dir,>anim_player_idle,>anim_player_move_right,>anim_player_move_left,>anim_player_jump,>anim_player_land,>anim_goal
    
bitmap1:
.import binary "ink_n.bin"
charset:
.import binary "charset.bin"
ink_scr_charset:
.import binary "ink_screen_charset.bin"
ink_scr_map:
.import binary "ink_screen_map.bin"
ink_scr_color:
.import binary "ink_screen_attr.bin"

* = * "MUSIC"
gaetan_music:
    .fill music.size, music.getData(i)
        
.print ""
.print "SID Data"
.print "--------"
.print "location=$"+toHexString(music.location)
.print "init=$"+toHexString(music.init)
.print "play=$"+toHexString(music.play)
.print "songs="+music.songs
.print "startSong="+music.startSong
.print "size=$"+toHexString(music.size)
.print "name="+music.name
.print "author="+music.author
.print "copyright="+music.copyright

.print ""
.print "Additional tech data"
.print "--------------------"
.print "header="+music.header
.print "header version="+music.version
.print "flags="+toBinaryString(music.flags)
.print "speed="+toBinaryString(music.speed)
.print "startpage="+music.startpage
.print "pagelength="+music.pagelength
.var tot=$0
.macro ent(id,x,y,w,h,s) {
    .if (id==1) {        
        .print "player x="+(>x)+" y="+y+" w="+w+" h="+h
        .byte $10,<x,>x,y
        .eval tot=tot+4
    }else .if (id==7) {        
        .print "nemico x="+(>x)+" y="+y+" w="+w+" h="+h
        .byte $07,<x,>x,y
        .eval tot=tot+4
    }  else .if (id==200) {
        .print "goal x="+(>x)+" y="+y+" w="+w+" h="+h
        .byte $20,<x,>x,y
        .byte $81,x/8,y/8,2,2
        .eval tot=tot+9
    } else .if (id==51) {
        .print "block x="+x+" y="+y+" w="+w+" h="+h
        .byte $82,x/8,y/8,(w-x)/8,(h-y)/8
        .eval tot=tot+5
    }  else .if (id!=1) {
        .print "block x="+x+" y="+y+" w="+w+" h="+h
        .byte $80,x/8,y/8,(w-x)/8,(h-y)/8
        .eval tot=tot+5
    }

    
}

room_0:
#import "editor/data/livello_0.room"
.print "total bytes "+tot
.byte $ff

room_1:
#import "editor/data/livello_1.room"
.print "total bytes "+tot
.byte $ff

room_2:
#import "editor/data/livello_2.room"
.print "total bytes "+tot
.byte $ff

room_3:
#import "editor/data/livello_3.room"
.print "total bytes "+tot
.byte $ff

room_4:
#import "editor/data/livello_4.room"
.print "total bytes "+tot
.byte $ff

level_table_lo:
.byte <room_0,<room_1,<room_2,<room_3,<room_4
level_table_hi:
.byte >room_0,>room_1,>room_2,>room_3,>room_4

.fill 4*8,0
map:    
.fill 1000,0
.fill 4*8,0


sndfx_jump:
.byte $07,$00,$08,$A8,$41,$AB,$21,$AF,$B1,$B4,$41,$00


sndfx_onground:
.byte $00,$0A,$00,$65,$D0,$81,$B4,$41,$AF,$A8,$A3,$9C,$D2,$81,$00

sndfx_splash:
.byte $10,$59,$08,$DF,$81,$A8,$11,$A9,$AB,$10,$AF,$B2,$DF,$80,$B3,$10
.byte $B4,$00

sndfx_table_lo:
.byte 0,<sndfx_jump,<sndfx_onground,<sndfx_splash

sndfx_table_hi:
.byte 0,>sndfx_jump,>sndfx_onground,>sndfx_splash
mult16_lo:
    .fill 255,<i*8
mult16_hi:
    .fill 255,>i*8