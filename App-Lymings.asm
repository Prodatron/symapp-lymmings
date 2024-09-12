;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                               L y m I N G S                                @
;@                      Lemmings Mobile Clone for SymbOS                      @
;@                                 MAIN CODE                                  @
;@                                                                            @
;@             (c) 2022-2024 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;FEATURE
;- demo mode starts automatically in mainmenu for one level
;- demo mode keeps fast forward
;- settings for music/effect volumes

;Bugs
;- stopper at portal door -> wrong display at other portal / bridge left up at door on right side, blocker direkt at connected door, returning lemmings dont restore

;- new2 lev1 bugs with bridges, blocker

;- parachuter display too deep when hitting ground sometimes (falling right, stopper at ground)
;- restore
;  - stopper on bru -> coming from left and reverse walking -> upper line
;  - falling on lower part of bridge left up coming from right
;  - stopper below bridge right up -> reverse moving coming from left

;- building bridge -> field not clickable with bridge again

;Hangs
;- bridge right up with stopper directly right to wall -> approaching lemming -> hangs
;- lemmings captured in one field, activate stopper -> crash


;Todo
;- traps
;  - bgrrst for parachute fallers not deep enough
;  - spikes

;- settings
;  - sound on/off/volume
;  - load alternative level file



;optimization
;+ rstlft/rgt -> optimized for normal left/write walking (no yofs calc, fixed 8height)
;+ predefined background restore bitmaps for empty field

;- summarize same bridge/movement/lemdatmad code in subroutines


LEM_MAX     equ 12  ;maximum number of lemmings
ANI_MAX     equ 4   ;maximum number of animated fields
TRP_MAX     equ 8   ;maximum number of active traps
WIN_DLY     equ 50  ;delay before dialogue window opens when level loaded/game ended



macro anirac
        bit 6,(ix+lemdatrac)
        jr z,$+4
        inc h:inc h
mend

;==============================================================================
;### CODE AREA ################################################################
;==============================================================================


;### PRGPRZ -> Application process
wingam_id  db 0    ;main      window ID
winsel_id  db 0    ;selection window ID
windia_id  db 0    ;dialogue  window ID

prgprz  ld de,winbot
        ld a,(App_BnkNum)
        call SyDesktop_WINOPN       ;open boot window
        jp c,prgend
        push af

        call SySystem_HLPINI        ;init help (?)
        call bmplod                 ;load extended bitmaps
        call bmpset                 ;set/patch extended bitmaps
        call bmprac                 ;prepare and copy lemming race bitmaps

        call sndini                 ;init and load sounds

        call cfglod                 ;load config/level progress
        call lvlhed                 ;load level header

        call prgsys
        call clcrin                 ;init random number generator

        ld hl,gam_count             ;add counter for game timing
        ld de,(App_BnkNum)
        ld a,(App_PrcID)
        ld b,1
        call SyKernel_TMADDT
        jp c,prgend

        ld a,(plcur)
        call menpos

        pop af
        call SyDesktop_WINCLS

        ld de,wingam
        ld a,(App_BnkNum)
        call SyDesktop_WINOPN       ;open game window
        jp c,prgend                 ;memory full -> quit process
        ld (wingam_id),a            ;window has been opened -> store ID
        ld a,64
        ld (winmendat_hid+2),a      ;hide progress-init bitmap
        jp menprc

prgprz0 scf
        call gamlop
        jp nc,gamend
        rst #30
        call prgopt                 ;optimize window x-position

        ld ix,(App_PrcID)           ;check for messages (idle)
        db #dd:ld h,-1
        ld iy,App_MsgBuf
        rst #18
        db #dd:dec l
        jr nz,prgprz0
        ld a,(App_MsgBuf+0)
        or a
        jr z,prgend
        cp MSR_DSK_WMODAL
        jp z,mapusl                 ;modal window unclick
        cp MSR_DSK_WCLICK
        jr nz,prgprz0
        ld a,(App_MsgBuf+2)
        cp DSK_ACT_KEY
        jr z,prgkey
        cp DSK_ACT_CLOSE
        jr z,prgend
        cp DSK_ACT_CONTENT
        jr nz,prgprz0
        ld hl,(App_MsgBuf+8)
        ld a,l
        or h
        jr z,prgprz0
        jp (hl)

;### PRGKEY -> key clicked
prgkey  ld a,(App_MsgBuf+4)
        call clcucs
        cp "B":jp z,mappsla     ;stopper
        cp "P":jp z,mappslb     ;parachute
        cp "L":jp z,mappsld     ;bridge left up
        cp "R":jp z,mappsle     ;bridge right up
        cp "S":jp z,mappslf     ;suicider
        cp "F":jp z,ctrspd      ;fast forward
        cp "%":jp z,gamskp      ;skip level
        call hlpkey
        jr prgprz0

;### PRGEND -> End program
prgend  ld hl,cfgdatflg
        bit 0,(hl)
        call nz,cfgsav
        call sndfre
        ld hl,(App_BegCode+prgpstnum)
        call SySystem_PRGEND
prgend0 rst #30
        jr prgend0

;### PRGINF -> open info window
prginf  ld a,(App_BnkNum)
        ld hl,prgmsginf
        ld b,8*2+1+64+128
        ld de,wingam
        call SySystem_SYSWRN
        jp menprc0

;### PRGHLP -> shows help
prghlp  call SySystem_HLPOPN
        jp menprc0

;### PRGOPT -> optimize window x-position
prgopt  ld a,(wingam+4)
        inc a
        and 7
        ret z
        ld de,(wingam+4)
        ld hl,(wingam+6)
        inc de
        ld a,e
        and #f8
        ld e,a
        dec de
        ld a,(wingam_id)
        jp SyDesktop_WINMOV

;### PRGSYS -> graphic system specific initilization
prgsysptc
dw rstupw2  :add a      ;background restore horizontal
dw rstupw5  :db #ed
dw rstrgt3  :add a      ;background restore vertical
dw profld1  :add a      ;progress bar
dw profld2  :add a
dw profld3+1:db 6
dw profld4+1:db 52-6
dw 0

prgsys  ld hl,jmp_sysinf    ;get computer and hardware type
        ld de,256*1+5
        ld ix,cfghrdtyp
        ld iy,66+2+6+8
        rst #28
        ld a,(cfghrdtyp)
        rla
        ret nc              ;cpc encoding -> no patches required
        ld hl,rsmclm
        ld (rstclm0+1),hl   ;patch routines
        ld hl,prgsysptc
prgsys3 ld e,(hl)
        inc hl
        ld d,(hl)
        ld a,e
        or d
        jr z,prgsys4
        inc hl
        ldi
        jr prgsys3
prgsys4 ld ix,bkgrrest      ;set encoding type for background restore (due to no initial display)
        ld b,LEM_MAX
        ld de,26+9+22
prgsys1 ld (ix+9),5
        ld (ix+26+9+9),5
        add ix,de
        djnz prgsys1
        ld hl,bmp_16c       ;use 16colour bitmap versions
prgsys2 ld e,(hl):inc hl
        ld d,(hl):inc hl
        ld a,e:or d
        ret z
        ld c,(hl):inc hl
        ld b,(hl):inc hl
        ldir
        jr prgsys2


;==============================================================================
;### CONFIG/PROGRESS ##########################################################
;==============================================================================

cfgnam  db "lymings.cfg",0          ;same length!

;### CFGLOD -> load config/progress
cfglod  call cfglod1
        ld hl,(datpth)
        ld ix,(App_BnkNum-1)
        call SyFile_FILOPN      ;open config file
        ret c
        ld hl,cfgdatmem
        ld bc,cfgdatlen
        ld de,(App_BnkNum)
        push af
        call SyFile_FILINP      ;load config/progress
        pop af
        jp SyFile_FILCLO        ;close level file
cfglod1 ld hl,cfgnam
cfglod0 ld de,(datfil)
        ld bc,datnam0-datnam
        ldir                    ;set data path to config file
        ret

;### CFGSAV -> save config/progress
cfgsav  call cfglod1
        ld hl,(datpth)
        ld ix,(App_BnkNum-1)
        call SyFile_FILNEW      ;create config file
        ret c
        ld hl,cfgdatmem
        ld bc,cfgdatlen
        ld de,(App_BnkNum)
        push af
        call SyFile_FILOUT      ;save config/progress
        pop af
        jp SyFile_FILCLO        ;close level file
        

;==============================================================================
;### LEVEL MANAGEMENT #########################################################
;==============================================================================

pldonelen   equ 99

cfgdatmem
cfgdatflg   db 0            ;+1=save on quit application instead of after each solved level, +2=load alternative level file
cfgdatres   ds 15           ;*res*
plcur       db 0            ;current planet in menu
pldone      ds pldonelen    ;solved levels per planet
cfgdatpth   ds 256          ;path of alternative level
cfgdatend

cfgdatlen   equ cfgdatend-cfgdatmem


pllod   db -1       ;loaded planet levels

lvllen  equ 140+9
lvlnum  db 0        ;number of levels
lvlmem  dw 0        ;start of level memory
lvladr  dw 0        ;address of current level data
lvlcur  db 0        ;current planet level


;### LVLHED -> load level header
lvlhedl dw 0

lvlhed  ld hl,lvlnam
        call cfglod0
        ld hl,(datpth)
        ld ix,(App_BnkNum-1)
        call SyFile_FILOPN      ;open level file
        ;jp c,...
        ld hl,lvlhedl
        ld de,(App_BnkNum)
        ld bc,2
        push af
        call SyFile_FILINP      ;load length information
        jp c,bmplode
        pop af
        ld hl,plnum
        ld de,(App_BnkNum)
        ld bc,(lvlhedl)
        push af
        call SyFile_FILINP      ;load level header
        jp c,bmplode
        pop af
        call SyFile_FILCLO      ;close level file

        ld hl,pltab
        ld bc,640               ;16*40
        add hl,bc
        ld (lvlmem),hl          ;set level memory start

        ld a,(plnum)
        ld hl,pltab
        ld bc,plnum-2
lvlhed1 ld e,(hl):inc hl        ;relocate planet data pointers
        ld d,(hl):dec hl
        ex de,hl
        add hl,bc
        ex de,hl
        ld (hl),e:inc hl
        ld (hl),d:inc hl
        dec a
        jr nz,lvlhed1
        ret

;### LVLLOD -> loads all levels of one planet
;### Input      (plcur)=planet
lvllod  ld hl,lvlnam
        call cfglod0
        ld a,(plcur)
        ld hl,pllod
        cp (hl)
        ret z                   ;planet levels already loaded
        call menpos7
        ex de,hl
        push hl
        ld a,(hl)
        ld (lvlnum),a
        ld l,a
        ld h,lvllen
        call clcmu8
        ld (lvllod1+1),hl
        pop hl
        inc hl:inc hl
        ld e,(hl):inc hl
        ld d,(hl)
        push de
        ld hl,(datpth)
        ld ix,(App_BnkNum-1)
        call SyFile_FILOPN      ;open level file
        ;jp c,...
        ld c,0
        pop ix
        ld iy,0
        push af
        call SyFile_FILPOI      ;set file pointer
        jp c,bmplode
        pop af
        ld hl,(lvlmem)
        ld de,(App_BnkNum)
lvllod1 ld bc,0
        push af
        scf
        call SyFile_FILCPR      ;load level data
        jp c,bmplode
        pop af
        call SyFile_FILCLO      ;close level file
        ld a,(plcur)
        ld (pllod),a
        ret

;### LVLCON -> continues current planet levels *EVENT*
lvlcon  call lvllod
        ld a,(plcur)
        push af
        call menpos7    ;(de)=number of levels
        pop af
        call lvlcon1    ;(hl)=levels already done
        ld a,(de)
        cp (hl)
        jr z,lvlrst1    ;all levels done -> restart planet
        ld a,(hl)
        ld (lvlcur),a
        ld l,a
        ld h,lvllen
        call clcmu8
        ld bc,(lvlmem)
        add hl,bc
        ld (lvladr),hl
        jp lvlgam
lvlcon1 ld c,a
        ld b,0
        ld hl,pldone
        add hl,bc
        ret

;### LVLRST -> restarts current planet levels *EVENT*
lvlrst  call lvllod
lvlrst1 ld a,(plcur)
        call lvlcon1
        xor a
        ld (hl),a
        ld (lvlcur),a
        ld hl,(lvlmem)
        ld (lvladr),hl
        jp lvlgam

;### LVLDEM -> starts demo mode *EVENT*
lvldema dw 0

lvldem  call lvldem0
        ld a,64
        ld (wingamdat_mapclk+2),a
        ld (xcb_pause+2),a      ;deactive gamecontrols
        ld hl,lvldem3
        ld (xcb_quit+0),hl
        ld hl,msg_demo
        ld (ui_msgctr),hl
        ld a,#c9
        ld (dspmsg),a
        ld hl,wingamgrp         ;set game window
        call lvlgam1
        rst #30
lvldem2 ld hl,(lvldema)
        call gambeg7            ;prepare game start
        xor a
        ld (gamlop),a
        inc a
        ld (gamdemmod),a
        jp gamsta1
lvldem1 call lvldem0                ;** start next demo
        ld a,(wingam_id)
        ld e,-1
        ld hl,0
        ld bc,0
        ld ix,120
        ld iy,173
        call SyDesktop_WINPIN   ;display map
        rst #30
        jr lvldem2
lvldem3 ld l,-1                     ;** returns from demo mode
        call sndmus
        ld hl,ctrqit
        ld (xcb_quit+0),hl      ;restore stuff
        ld hl,txtzero
        ld (ui_msgctr),hl
        xor a
        ld (dspmsg),a
        ld (gamdemmod),a
        ld a,19
        ld (wingamdat_mapclk+2),a
        ld a,10
        ld (xcb_pause+2),a
        ld a,(gamlop0)
        or a
        call z,ctrspd0          ;deactivate speed mode
        call figini             ;hide figures
        scf
        jr lvlmen

lvldem0 call clcrnd                 ;** select random demolevel
        ld h,map_demo_num   ;l=0..255
        call clcmu8         ;h=demonum * 0..255 / 256 -> 0..demonum
        ld l,lvllen
        call clcmu8
        ld bc,map_demo
        add hl,bc
        ex de,hl
        ld hl,(lvldema)
        or a
        sbc hl,de
        jr z,lvldem0        ;dont show same demolevel twice
        ex de,hl
        ld (lvldema),hl
        jp maplod

;### LVLGAM -> switches to game mode and starts current level
lvlgam  ld hl,(lvladr)          ;load map
        call maplod
        ld hl,wingamgrp         ;set game window
        call lvlgam1
        rst #30
        ld hl,(lvladr)
        call gambeg0            ;prepare game start
        xor a
        ld (gamlop),a
        jp prgprz0
lvlgam1 ld (wingam+36),hl       ;redraw menu/game/ui area
        ld a,(wingam_id)
        ld e,-1
        ld hl,0
        ld bc,0
        ld ix,120
        ld iy,173
        push af
        call SyDesktop_WINPIN   ;display map
        pop af
        ld e,-1
        ld hl,map_xlen*fld_xlen+map_xofs
        ld bc,48
        ld ix,224-120
        ld iy,173-48
        jp SyDesktop_WINPIN     ;display ui

;### LVLMEN -> switches to menu mode
;### Input      CF=0 planet completed
lvlmen  push af
        ld a,(plcur)            ;refresh planet status
        call menpos
        ld hl,winmengrp         ;set menu window
        call lvlgam1
        pop af
        jp c,menprc0
        ld b,7                  ;planet completed -> animate flag and scroll to next planet
lvlmen1 push bc
        ld e,winmendat_clr_id+1
        call lvlmen2
        ld e,winmendat_flag_id+1
        call lvlmen2
        pop bc
        djnz lvlmen1
        jp mennxt
lvlmen2 ld a,(wingam_id)
        call SyDesktop_WINDIN
        ld b,6
lvlmen3 rst #30
        djnz lvlmen3
        ret


;==============================================================================
;### PLANET MENU ##############################################################
;==============================================================================

;### MENPRC -> main menu process
menprc  
menprc0 call mensta                 ;blinking stars
        rst #30
        call prgopt                 ;optimize window x-position
        ld ix,(App_PrcID)           ;check for messages (idle)
        db #dd:ld h,-1
        ld iy,App_MsgBuf
        rst #18
        db #dd:dec l
        jr nz,menprc0
        ld a,(App_MsgBuf+0)
        or a
        jp z,prgend
        cp MSR_DSK_WCLICK
        jr nz,menprc0
        ld a,(App_MsgBuf+2)
        cp DSK_ACT_KEY
        jr z,menkey
        cp DSK_ACT_CLOSE
        jp z,prgend
        cp DSK_ACT_CONTENT
        jr nz,menprc0
        ld hl,(App_MsgBuf+8)
        ld a,h
        or h
        jr z,menprc0
        jp (hl)

menkey  ld a,(App_MsgBuf+4)
        call clcucs
        cp 136:jp z,mennxt          ;next planet
        cp 137:jp z,menprv          ;previous planet
        cp "R":jp z,lvlrst          ;restart
        cp "D":jp z,lvldem          ;demo mode
        cp "S":;jp z,...settings
        cp "H":jp z,menhlp          ;help
        cp "A":jp z,prginf          ;about
        cp "I":jp z,prghlp          ;info
        call hlpkey
        jr menprc0

;### MENHLP -> opens help in mainmenu *EVENT*
menhlp  ld hl,menprc0
        ld (hlppag0+1),hl
        ld hl,menhlp0
        ld (hlpcnc0+1),hl
        call hlpopn1
        jr menprc0
menhlp0 ld hl,prgprz0
        ld (hlppag0+1),hl
        ld hl,ctrpau1
        ld (hlpcnc0+1),hl
        jp menprc0

;### MENPOS -> set planet position
;### Input      A=planet number (0..max-1)
menpos  ld (plcur),a
        ld c,a                  ;show/hide flags (completed planets)
        ld b,0
        ld hl,pldone-1
        add hl,bc
        sub 1
        ld ix,winmendat_flag+32
        ld (ix+2),64
        call nc,menpos5
        inc a
        inc hl
        ld ix,winmendat_flag+16
        ld (ix+2),64
        call menpos5
        inc a
        inc hl
        ld de,(plnum)
        cp e
        ld ix,winmendat_flag+00
        ld (ix+2),64
        call nz,menpos5

        ld a,(plcur)
        ld c,a
        inc a
        call clcdec
        ld (menpl1txt+0),hl     ;set planet number
        ld b,0
        ld hl,pldone
        add hl,bc
        ld a,(hl)
        add "0"
        ld (menpl2txt+0),a      ;set levels done
        sla c
        call menpos4            ;de=planet data
        ld a,(de):inc de
        add "0"
        ld (menpl2txt+2),a      ;set levels total
        ld a,(de):inc de        ;a=planet type
        call menpos3
        ld (xcb_planet2+4),bc   ;set planet bitmap
        ex de,hl
        inc hl:inc hl
        ld de,menpl1txt+4
        ld bc,15
        ldir                    ;copy planet name
        ld ix,xcb_planet3
        ld (ix-16+2),64         ;hide lower line
        ld (ix+00+2),64         ;hide lower planet
        ld a,(plcur)
        push af
        sub 1
        jr c,menpos1
        ld (ix-16+2),10         ;show lower line
        call menpos2            ;set lower planet
menpos1 pop af
        ld ix,xcb_planet1
        ld (ix+16+2),64         ;hide upper line
        ld (ix+00+2),64         ;hide upper planet
        inc a
        ld hl,plnum
        cp (hl)
        ret z
        ld (ix+16+2),10         ;show upper line, set upper planet
menpos2 call menpos7            ;a=planet id, ix=control data record -> set planet
        inc de
        ld a,(de)
        call menpos3
        ld (ix+00+2),10
        ld (ix+00+4),c
        ld (ix+00+5),b
        ret
menpos3 add xid_planet1-2       ;a=planet type -> bc=planet bitmap
        add a
        ld c,a
        ld b,0
        ld hl,bmpdato
        add hl,bc
        ld c,(hl):inc hl
        ld b,(hl)
        ret
menpos7 add a
        ld c,a
        ld b,0
menpos4 ld hl,pltab
        add hl,bc
        ld e,(hl):inc hl
        ld d,(hl)               ;de=planet data
        ret

menpos5 push af
        push hl
        call menpos7            ;de=planet data
        pop hl
        ld a,(de)
        cp (hl)
        jr nz,menpos6
        ld (ix+2),10
menpos6 pop af
        ret

;### MENNXT -> scrolls to upper planet *EVENT*
mennxt  ld a,(plcur)
        inc a
        ld hl,plnum
        cp (hl)
        jp z,menprc0            ;already last planet -> finish
        push af
        push hl
        ld hl,xcb_planet3+15
        ld de,xcb_planet4+15
        ld bc,5*16
        lddr
        pop hl
        inc a
        ld ix,xcb_planet1
        ld (ix+00+8),00-70
        ld (ix+00+9),255
        ld (ix+16+8),32-70
        ld (ix+16+9),255
        ld (ix+00+2),64         ;hide upper planet
        ld (ix+16+2),64         ;hide upper line
        cp (hl)
        jr z,mennxt1
        ld (ix+16+2),10         ;show upper line
        call menpos2            ;set new upper planet
mennxt1 call menhid             ;hide texts, flags
        ld de,2
        call menscl             ;scroll downwards
mennxt2 pop af
        call menpos
        call menshw
        jp menprc0

;### MENPRV -> scrolls to lower planet *EVENT*
menprv  ld a,(plcur)
        or a
        jp z,menprc0
        ld ix,xcb_planet4
        ld (ix-16+8),172
        ld (ix-16+9),0
        ld (ix+00+8),210
        ld (ix+00+9),0
        ld (ix-16+2),64         ;hide lower line
        ld (ix+00+2),64         ;hide lower planet
        dec a
        push af
        jr z,menprv1
        ld (ix-16+2),10         ;show lower line
        dec a
        call menpos2            ;set new lower planet
menprv1 call menhid
        ld de,-2
        call menscl
        ld hl,xcb_planet2
        ld de,xcb_planet1
        ld bc,5*16
        ldir
        jr mennxt2

;### MENHID -> hides flags/texts
menhid  ld a,(wingam_id)
        ld de,winmendat_clr_id*256+256-5
        jp SyDesktop_WINDIN

;### MENSHW -> shows flags/texts
menshw  ld a,(wingam_id)
        ld de,winmendat_flag_id*256+256-5
        jp SyDesktop_WINDIN

;### MENSCL -> scrolls planets
;### Input      DE=y-offset (2, -2)
menscl  ld b,35
menscl1 push bc
        rst #30
        ld ix,xcb_planet1
        ld a,7
        ld bc,16
menscl2 ld l,(ix+00+8)
        ld h,(ix+00+9)
        add hl,de
        ld (ix+00+8),l
        ld (ix+00+9),h
        add ix,bc
        dec a
        jr nz,menscl2
        push de
        ld a,(wingam_id)
        ld de,winmendat_planet_id*256+256-7
        call SyDesktop_WINDIN   ;scroll planets
        pop de
        pop bc
        djnz menscl1
        ret

;### MENSTA -> animates blinking stars
menstac db 0        ;counter 255..0

menstaa dw winmendat_star       ;control address
menstai db winmendat_star_id    ;control id

menstab dw star0                ;original bitmap
menstap db 9,0,5                ;original position

menstat             ;animation tab
db     0:dw star1
db  0,-1:dw star2
db  0,-1:dw star3
db  0,-1:dw star2
db  0, 0:dw star1

mensta  ld hl,menstac       ;decrease counter
        dec (hl)
        dec (hl)
        ld a,(hl)
        ld c,0
        ld hl,menstab
        jr z,mensta1        ;0=restore original star
        cp 64
        jr z,mensta2        ;64=blink new star
        ret nc              ;>64=do nothing
        ld l,a
        ld h,0
        ld c,12
        call clcdiv         ;l=0..5
        inc h:dec h
        ret nz              ;next animation only if modulo=0
        sla l
        sla l
        ld h,0
        ld bc,menstat-4
        add hl,bc
        ld c,(hl):inc hl    ;c=posdif
mensta1 ld ix,(menstaa)
        ld a,(menstap+0)
        add c
        ld (ix+6),a
        ld a,(menstap+2)
        add c
        ld (ix+8),a         ;set x/ypos
        ld a,(hl):ld (ix+4),a:inc hl
        ld a,(hl):ld (ix+5),a:inc hl    ;set bitmap
        ld a,(wingam_id)
        ld de,(menstai)
        jp SyDesktop_WINDIN ;plot bitmap
mensta2 call clcrnd             ;*** select new star
        ld a,l
        and 15              ;a=0..15
        sub 1
        ret c               ;if 0, use old one
        ld l,a
        add winmendat_star_id
        ld (menstai),a
        ld h,0
        add hl,hl:add hl,hl:add hl,hl:add hl,hl
        ld bc,winmendat_star
        add hl,bc
        ld (menstaa),hl
        ld bc,4
        add hl,bc
        ld de,menstab       ;store original bitmap/position
        inc c
        ldir
        ret


;==============================================================================
;### UI CONTROLS ##############################################################
;==============================================================================

;### CTRPAU -> pauses/continues game
ctrpau  ld a,#c9
        ld (gamlop),a
ctrpau1 ld h,VOL_DIALOGUE
        call sndvol
        ld de,winpau
        ld hl,windia_id
        call winopn
        jp prgprz0

;### CTRSPD -> speed mode on/off
ctrspd  call ctrspd0
        jp prgprz0
ctrspd0 ld hl,gamlop0
        ld a,(hl)
        xor #d8     ;=ret c/nop
        ld (hl),a
        ld a,#21
        jr nz,ctrspd1
        ld a,#cd
ctrspd1 ld (dspupd),a
        call nz,dspupd8
        ret

;### CTRQIT -> opens quit menu
ctrqit  ld a,#c9
        ld (gamlop),a
        ld h,VOL_DIALOGUE
        call sndvol
        ld de,winqit
        ld hl,windia_id
        ld bc,256*70+58
        call winopn1
        jp prgprz0


;==============================================================================
;### GAME CONTROL #############################################################
;==============================================================================

gamstawin   db 0        ;1=enough saved
gamstarst   db 0        ;1=restart at once
gamdemmod   db 0        ;1=demo mode


;### GAMBEG -> begins a new level *SUB*
;### Input      HL=level data
gambeg  push hl
        call maplod                         ;load map
        ld a,(wingam_id)
        ld e,-1
        ld hl,map_xofs
        ld bc,map_yofs
        ld ix,map_xlen*fld_xlen
        ld iy,map_ylen*fld_ylen
        call SyDesktop_WINPIN               ;display map
        pop hl

gambeg0 call gambeg7
        ld hl,txtzero:ld (savln4ctr),hl
                      ld (savln5ctr),hl
        ld a,64:ld (winsavdat_lem+16+2),a   ;deactivate second race
        ld a,(0*lemraclen+lemracmem+lemracmin)
        ld hl,(0*lemraclen+lemracmem+lemracmax)
        ld bc,txtsav3
        ld de,savln2ctr
        call gambeg6                        ;set first race number
        ld a,(lemracnum)
        dec a
        jr z,gambeg1
        ld a,(1*lemraclen+lemracmem+lemracmin)
        ld hl,(1*lemraclen+lemracmem+lemracmax)
        ld bc,txtsav4
        ld de,savln4ctr
        call gambeg6                        ;set second race number
        ld hl,txtsav5:ld (savln5ctr),hl
        ld a,10:ld (winsavdat_lem+16+2),a   ;activate second race
gambeg1 ld hl,diabt2txt:ld (winsavdat_but+16+4),hl  ;set buttons
        ld bc,txtsav1
        ld de,diabt1txt
        ld hl,gamsta
gambeg2 ld (savln1ctr),bc                   ;set titel text
        ld (winsavdat_but+00+4),de          ;set button text
        ld (winsavdat_but+00+0),hl          ;set button jump
        ld de,winsav                        ;open window
        ld hl,windia_id
        ld b,WIN_DLY
        jp windly

gambeg7 push hl                             ;init timer/item/progress display, deactivate routines
        ld hl,(gamtimv)
        call gamtim4
        ld a,(gamitmv)
        call gamitm
        call figini                         ;reset figures
        pop hl
        call proini                         ;init and display progress bar
        ld a,#c9
        ld (figdrp),a                       ;deactivate figure dropper
        ld (gamtim),a                       ;deactivate timer
        ld (gamlop4),a                      ;deactivate end check
        ret

gambeg6 cp l                ;a=number, l=max, bc=text if not all, de=control
        jr z,gambeg4
        call clcdec
        ld a,l
        cp "0"
        jr nz,gambeg3
        ld l,"<"
gambeg3 ld a,l:ld (bc),a:inc bc
        ld a,h:ld (bc),a:dec bc
        jr gambeg5
gambeg4 ld bc,txtsav8
gambeg5 ex de,hl
        ld (hl),c
        inc hl
        ld (hl),b
        ret

;### GAMSTA -> closes dialogue and starts the actual level *EVENT*
gamsta  call wincls         ;close dialogue
gamsta1 xor a
        ld (gamstawin),a
        ld (gamstarst),a
        ld (figdrp),a       ;activate dropper 
        ld (figdrp0),a
        ld (gamtim),a       ;activate timer
        ld (gamlop4),a      ;activate end check
        ld (gamkal),a       ;activate kill all
        call trpsta         ;start traps
        ld hl,256*1+SND_DOOR_START
        call sndefx
        ld bc,256*50*3+00
        ld hl,msg_letsgo
        call dspmsg         ;display "lets go"
        jp prgprz0

;### GAMSKP -> skip level (cheat)
gamskp  ld a,1
        ld (gamstawin),a
        jr gamend

;### GAMEND -> game ends
;### Input      (gamstawin)=reason (0=too many died, 1=enough saved)
gamend  ld l,-1
        call sndmus         ;stop music

        ld a,#c9
        ld (gamlop4),a      ;deactivate end check
        ld (gamtim),a       ;deactivate timer
        ld a,(gamlop0)
        or a
        call z,ctrspd0      ;deactivate speed mode
        ld a,(winsel_id)
        or a
        call nz,wincls      ;closes selection window, if opened
        ld a,64
        ld (wingamdat_select+2),a
        ld a,(gamdemmod)
        or a
        jp nz,lvldem1

        ld a,(gamstarst)
        cp 1
        jr z,gamrtr1
        jp nc,gammen
        ld a,(gamstawin)
        or a
        jr nz,gamend1

        ld de,winfai            ;*** failed
        ld hl,windia_id
        ld b,WIN_DLY
        call windly
        jp prgprz0

gamend1 ld hl,txtsav2           ;*** success
        ld (savln1ctr),hl   ;set saved lemmings
        ld a,(0*lemraclen+lemracmem+lemracsav)
        ld hl,(0*lemraclen+lemracmem+lemracmax)
        ld bc,txtsav3
        ld de,savln2ctr
        call gambeg6
        ld hl,txtzero:ld (savln4ctr),hl
        ld a,(lemracnum)
        dec a
        jr z,gamend2
        ld a,(1*lemraclen+lemracmem+lemracsav)
        ld hl,(1*lemraclen+lemracmem+lemracmax)
        ld bc,txtsav4
        ld de,savln4ctr
        call gambeg6
gamend2 ld a,(plcur)
        call lvlcon1
        inc (hl)            ;current level done
        ld hl,cfgdatflg
        bit 0,(hl)
        call z,cfgsav
        ld bc,txtsav2       ;"you saved"
        ld de,diabt3txt     ;"continue"
        ld hl,gamnxt        ;next jump
        call gambeg2
        jp prgprz0

;### GAMRTR -> closes dialogue and retries current level
gamrtr  call wincls
gamrtr1 ld hl,(lvladr)
        call gambeg
        jp prgprz0

;### GAMRST -> closes pause dialogue, kills all and restarts game
gamrst  ld a,1
gamrst1 push af
        call wincls
        ld h,VOL_GAME
        call sndvol
        xor a
        ld (gamlop),a
        call gamkal
        ld hl,gamstarst
        pop af
        ld (hl),a
        jp prgprz0

;### GAMNXT -> closes dialogue and loads next level
gamnxt  call wincls
        ld a,(lvlcur)
        inc a
        ld hl,lvlnum
        cp (hl)
        jp z,lvlmen         ;planet done, back to main
        ld (lvlcur),a
        ld hl,(lvladr)      ;begin next level
        ld bc,lvllen
        add hl,bc
        ld (lvladr),hl
        call gambeg
        jp prgprz0

;### GAMQIT -> starts game end (explode lemmings and goes back)
gamqit  ld h,VOL_GAME
        call sndvol
        ld a,2
        jr gamrst1

;### GAMCNC -> closes dialogue and returns to menu
gamcnc  call wincls
;### GAMMEN -> returns to menu
gammen  call figini
        ld a,(plcur)
        call lvlcon1
        ld a,(lvlnum)
        cp (hl)
        jp z,lvlmen
        scf
        jp lvlmen

;### GAMCON -> closes pause dialogue and continues game
gamcon  call wincls
        xor a
        ld (gamlop),a
        ld h,VOL_GAME
        call sndvol
        jp prgprz0

;### GAMLOP -> does one game step
;### Output     CF=0 -> game end
gam_frames  equ 4   ;frames for one full loop
gam_count   db 0    ;kernel counter

gamlop  nop
        call dspupd
        call gamtim
        ld hl,gam_count         ;check if new loop start
        ld a,(hl)
        cp gam_frames
gamlop0 ret c
        ld (hl),0           ;*** new loop

        call trplop             ;trap loop
        ld a,(trpctrnum)
        sub 1
        jr c,gamlop6
        ld e,wingamdat_trp_id
        jr z,gamlop5
        ld d,e
        cpl
        ld e,a
gamlop5 ld a,(wingam_id)
        call SyDesktop_WINDIN   ;plot active trap
gamlop6 call figdrp             ;drop new lemmings

        ld ix,lemdatmem
        ld b,LEM_MAX
gamlop1 push bc
        call lemmov             ;move lemmings
        call c,figdel
        ld bc,lemdatlen
        add ix,bc
        pop bc
        djnz gamlop1
        ld a,(winsel_id)
        or a
        ld a,(wingam_id)        ;refresh selection frame
        ld e,wingamdat_select_id
        call nz,SyDesktop_WINDIN

        ld a,(ctractnum)        ;plot lemmings
        or a
        jr z,gamlop3
        cp 8+1
        jr c,gamlop2
        ld a,8                  ;a=1..8
gamlop2 neg                     ;a=-1..-8
        add a
        ld e,a
        ld d,wingamdat_lem_id
        ld a,(wingam_id)
        call SyDesktop_WINDIN   ;plot first  8 lemmings (up to)
        ld a,(ctractnum)
        sub 8+1                 ;a=0..3
        jr c,gamlop3
        cpl                     ;a=-1..-4
        add a
        ld e,a
        ld d,wingamdat_lem_id+16
        ld a,(wingam_id)
        call SyDesktop_WINDIN   ;plot second 4 lemmings (up to)

gamlop3 call anilop             ;plot animations
        scf
gamlop4 ret
        ld a,(ctractnum)        ;check if game end
        or a
        scf
        ret nz
        ld a,(figdrp)
        sub 1
        ret


;### GAMTIM -> updates timer
gamtimv db 30,1     ;sec,min

gamtim  nop
gamtim0 ld a,1
        dec a
        jr z,gamtim1
        ld (gamtim0+1),a
        ret
gamtim1 ld a,50
        ld (gamtim0+1),a
        ld hl,(gamtimv)
        dec l
        bit 7,l
        jr z,gamtim2
        dec h
        bit 7,h
        jr nz,gamtim3
        ld l,59
gamtim2 ld (gamtimv),hl
gamtim4 ld a,h
        ld e,l
        call clcdec
        ld (ui_timtxt+0),hl
        ld a,e
        call clcdec
        ld (ui_timtxt+3),hl
        ld e,wingamdat_tim_id
        ld a,(wingam_id)
        jp SyDesktop_WINDIN
gamtim3 ld a,#c9
        ld (gamtim),a
        ld bc,256*50*3+00
        ld hl,msg_timeup
        call dspmsg
        jr gamkal

;### GAMITM -> sets item counter
;### Input      A=number
;### Output     (gamitmv)=number
gamitmv db 10

gamitm  ld (gamitmv),a
        push af
        call clcdec
        ld (ui_itmtxt),hl
        pop af
        or a
        ld a,16*01+6
        jr nz,gamitm1
        ld a,16*15+6
gamitm1 ld (ui_itmctr+2),a
        ld e,wingamdat_itm_id
        ld a,(wingam_id)
        jp SyDesktop_WINDIN

;### GAMKAL -> kills all lemmings
gamkal  nop
        ld a,#c9
        ld (gamkal),a
        ld (figdrp),a           ;stop dropper
        ld hl,256*1+SND_DIE_OHNO
        call sndefx
        ld ix,lemdatmem
        ld bc,LEM_MAX*256+256-1
        ld hl,lemexb
        ld de,lemdatlen
gamkal1 bit 7,(ix+lemdatdir)
        jr nz,gamkal2
        ld (ix+lemdatsub),c
        ld (ix+lemdatjmp+0),l
        ld (ix+lemdatjmp+1),h
gamkal2 add ix,de
        djnz gamkal1
        ret


;==============================================================================
;### LEMMING CONTROL ##########################################################
;==============================================================================

                    ; only for noting
                    ;   alive -> 1=move right, 2=move left, 3=falling fast, 4=falling parachute, 5=climb right, 6=climb left, 7=dig right, 8=dig left, 9=stopper, 10=suicider
                    ;   dying -> 11=splatter on ground after falling, 12=splashing at trap, 13=exploding at trap, 14=drowning in water


lemdatdir   equ 0   ;1B last direction (0=right, 1=left, 128=no lemming)
lemdatsub   equ 1   ;1B sub counter for movement/dying
                    ;   left/right -> 0-23 (within a field including 1bit fixed point)
                    ;   falling    -> 0-7 animation
                    ;   parachute  -> 0-7 animation
                    ;   climb      -> ...
                    ;   animation  -> 0-x (stopper, goal, dying, suicider)
lemdatxps   equ 2   ;1B current xpos in pixel (0,4,8)
lemdatyps   equ 3   ;1B current ypos in pixel (exact)
lemdatfxp   equ 4   ;1B current field xpos in pixel
lemdatfyp   equ 5   ;1B current field ypos in pixel
lemdatmad   equ 6   ;1W current field address in map
lemdatjmp   equ 8   ;1W address of typ routine
lemdatctr   equ 10  ;1W address of GUI controls (1x restore, 1x display)
lemdatrcv   equ 12  ;1W V   bgr restore, address of control vert-bitmap data    *fixed*
lemdatrch   equ 14  ;1W  H  bgr restore, address of control hori-bitmap header  *fixed*
lemdatrcf   equ 16  ;1W   F bgr restore, address of control fall-bitmap header  *fixed*
lemdatfal   equ 18  ;1B   F bgr restore, y position in field (0-11)
lemdatrxp   equ 19  ;1B V   bgr restore, last xposition / falling counter (1-16), bit7=right side falling
lemdatrbm   equ 20  ;1W   F bgr restore, base address of current background bitmap / digger down -> last sub
lemdatrac   equ 22  ;1B lemming race (0=race0, 64=race1)
lemdatwdt   equ 23  ;1B current lemming width in pixel
lemdatctn   equ 24  ;1B control ID (0-11)
lemdatstu   equ 25  ;1B stupid flag for stopper check

lemdatlen   equ 26

lemdatmem   ds LEM_MAX*lemdatlen


;### LEMMOV -> lemming moves one step
;### Input      IX=lemming record
;### Handover   IY=GUI controls (3x restore, 1x display)
;### Output     CF=1 lemming removed (A=type, 0=dead, 1=saved)
lemmov  ld a,(ix+lemdatctr+0):ld iyl,a
        ld a,(ix+lemdatctr+1):ld iyh,a  ;IY=control
        ld l,(ix+lemdatjmp+0)
        ld h,(ix+lemdatjmp+1)
        jp (hl)


;*** lemming not existing
lemn    or a
        ret


;*** bridge right down
lembrd  ld a,(ix+lemdatsub)
        inc a
        cp 1
        jp z,lembrdc                    ;check for stopper
        cp 7
        jp z,lembrda                    ;field middle reached (check for marked stopper/suicider)
        cp 20
        jp z,lembrd8                    ;check for bridge right up
        cp 17
        jr z,lembrd5                    ;try entering next field
        cp 24
        jr z,lembrd4                    ;next field entered -> continue with right walking
lembrd0 ld (ix+lemdatsub),a
        ld b,(ix+lemdatxps)
        and 7
        jr nz,lembrd1
        ld c,a                      ;*** next 4pixels
        ld a,b
        add 4
        ld (ix+lemdatxps),a
        ld b,a
        ld a,c
lembrd1 ld hl,lemanirgt             ;*** update animation (a=sub[bit0-2], d=xpos)
        anirac
        add a
        add a
        add l
        ld l,a                          ;hl=animation data
        ld a,(hl):inc hl:ld (iy+16+4),a
        ld a,(hl):inc hl:ld (iy+16+5),a ;set bitmap
        ld a,(hl)
        add b                           ;calculate new x position
        cp (iy+16+6)
        jr z,lembrd3
        ld (iy+16+6),a                  ;set new x position
        call rstrgt                     ;restore v-background
        or a
        ret
lembrd3 call brgmdw                     ;increase yposition
        call rstdwn                     ;restore h-background
        or a
        ret

lembrd4 ld hl,lemmrg                ;*** next field entered -> continue with right walking
        ld (ix+lemdatjmp+0),l
        ld (ix+lemdatjmp+1),h
        ld (ix+lemdatrxp),-1
        jp lemmrg6

lembrd5 ld l,(ix+lemdatmad+0)       ;*** try entering next field
        ld h,(ix+lemdatmad+1)
        inc hl:inc hl
        ld a,(hl)
        cp idm_brdg_rmrk
        jp z,brgbrg
        bit 6,a
        jr nz,lembrd6
        cp idm_door_lu
        jp nc,lemmrgh

        cp idm_grnd_mark
        jr c,lembrd7
        cp idm_grqm_br3m+1
        jr nc,lembrd7
        call lemmrgg                    ;digger field
        ld a,(iy+16+6)
        sub 4
        ld (iy+16+6),a
        ld (iy+00+2),64
        ld (ix+lemdatdir),2             ;mark as right bridge digger
        ld hl,lemdrg6
        jp jmpnew

lembrd6 ld a,17                         ;open -> continue climbing down
        jp lembrd0
lembrd7 ld (ix+lemdatdir),1             ;solid -> reverse moving
        call brgmup
        ld hl,lemblu
        jp jmpnew

lembrd8 ld l,(ix+lemdatmad+0)       ;*** check for bridge right up
        ld h,(ix+lemdatmad+1)
        inc hl:inc hl
        ld a,(hl)
        cp idm_brdg_rf
        jr c,lembrd9
        cp idm_brdg_rf_x+1
        jr nc,lembrd9
        ld hl,lembru
        jp jmpnew
lembrd9 ld a,20
        jp lembrd0

lembrda ld l,(ix+lemdatmad+0)       ;*** check for (marked) stopper/suicider
        ld h,(ix+lemdatmad+1)
        push hl
        call stpchk
        pop hl
        jr nc,lembrdm
        call brgmup
        call brgmup
        jr lembrdl
lembrdm ld a,(hl)
        cp idm_brdg_lf_s
        jr c,lembrdb
        cp idm_brdg_lf_x
        jp c,lembrua
        jr nz,lembrdb
        ;...suicider
lembrdb ld a,7
        jp lembrd0

lembrdc ld l,(ix+lemdatmad+0)       ;*** check for stopper
        ld h,(ix+lemdatmad+1)
        call stpchk
        ld c,(ix+lemdatstu)
        ld (ix+lemdatstu),0
        ld a,1
        jp nc,lembrd0
        dec c
        call nz,brgmup                  ;don't call, when coming from lemmrg
lembrdl ld (ix+lemdatdir),1             ;stopper -> reverse moving
        ld hl,lemblu
        jp jmpnew


;*** bridge right up
lembru  ld a,(ix+lemdatsub)
        cp 8
        jr nz,lembruj
        ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        call falchr0
        jr z,lembruk
        call fldrdw0
        call fldmup
        ld a,6
        jp falbeg

lembruk ld a,(ix+lemdatsub)
lembruj inc a
        cp 1
        jp z,lembrub                    ;check for stopper
        cp 7
        jp z,lembru8                    ;field middle reached (check for marked stopper/suicider)
        cp 16
        ;jr z,lembru?                    ;check for falling from half-bridge
        cp 17
        jp z,lembru5                    ;try entering next field -> continue with right walking
        cp 24
        jr z,lembru4                    ;this field entered
lembru0 ld (ix+lemdatsub),a
        ld b,(ix+lemdatxps)
        and 7
        jr nz,lembru1
        ld c,a                      ;*** next 4pixels
        ld a,b
        add 4
        ld (ix+lemdatxps),a
        ld b,a
        ld a,c
lembru1 ld hl,lemanirgt             ;*** update animation (a=sub[bit0-2], d=xpos)
        anirac
        add a
        add a
        add l
        ld l,a                          ;hl=animation data
        ld a,(hl):inc hl:ld (iy+16+4),a
        ld a,(hl):inc hl:ld (iy+16+5),a ;set bitmap
        ld a,(hl)
        add b                           ;calculate new x position
        cp (iy+16+6)
        jr z,lembru3
        ld (iy+16+6),a                  ;set new x position
        call rstrgt                     ;restore v-background
        or a
        ret
lembru3 call brgmup                     ;decrease yposition
        call rstupw                     ;restore h-background
        or a
        ret
lembru4 call fldmrg                 ;*** this field entered
        ld (ix+lemdatsub),0  
        ld a,(ix+lemdatxps)
        add 4
        ld (ix+lemdatxps),a
        ld b,(ix+lemdatxps)
        xor a
        jp lembru1
lembru5 call fldmup                 ;*** try entering next field
        ld a,(hl)                       ;check if upper field is bridge up left/marked
        cp idm_brdg_lmrk
        ld c,1
        jr z,lembruc
        dec c
        cp idm_brdg_lf
        jr c,lembru6
        cp idm_brdg_lf_x+1
        jr nc,lembru6
lembruc inc hl:inc hl                   ;yes -> check, if next field is solid and no digger
        ld a,(hl)
        bit 6,a                         ;solid?
        jr nz,lembru6
        cp idm_door_lu
        jr nc,lembru6
        cp idm_grnd_mark                ;digger?
        jr c,lembru7
        cp idm_grqm_br3m+1
        jr c,lembru6                    ;yes
lembru7 ld (ix+lemdatdir),1
        dec c
        jr z,lembrud
        ld a,(ix+lemdatyps)         ;*** solid, finished -> reverse moving on upper bridge
        sub 5
        ld (ix+lemdatyps),a             ;jump 5 pixels up
        ld a,(iy+00+8):sub 5:ld (iy+00+8),a
        ld a,(iy+16+8):sub 5:ld (iy+16+8),a
        call fldrdw0
        ld hl,lemblu
        jp jmpnew
lembrud call fldrdw0                ;*** solid, marked   -> build bridge left up on upper field
        call fldmrg                     ;move lemming to right field
        ld a,(ix+lemdatfyp)
        add 4
        ld (iy+16+8),a
        jp brgblf1                      ;build bridge left up

lembru6 ld hl,lemmrg                    ;continue with right walking
        ld (ix+lemdatjmp+0),l
        ld (ix+lemdatjmp+1),h
        ld (ix+lemdatrxp),-1
        jp lemmrg4

lembru8 ld l,(ix+lemdatmad+0)       ;*** check for marked stopper/suicider
        ld h,(ix+lemdatmad+1)
        push hl
        call stpchk
        pop hl
        jr nc,lembrum
        call brgmdw
        jr lembrul
lembrum ld a,(hl)
        cp idm_brdg_rf_s
        jr c,lembru9
        cp idm_brdg_rf_x
        jr c,lembrua
        jr nz,lembru9
        ;...suicider
lembru9 ld a,7
        jp lembru0
lembrua call fldcen
        call stpadd                     ;add stopper
        ld a,(hl)
        sub 2
        ld (hl),a                       ;remove stop sign
        call fldchg
        call fldupd
        ld (ix+lemdatdir),2             ;mark as bridge stopper
        ld hl,lemstp0                   ;turn into bridge stopper
        call fldact
        or a
        ret

lembrub ld l,(ix+lemdatmad+0)       ;*** check for stopper
        ld h,(ix+lemdatmad+1)
        call stpchk
        ld a,1
        jp nc,lembru0
lembrul call brgmdw                     ;stopper -> reverse moving
        ld (ix+lemdatdir),1
        ld hl,lembld
        jp jmpnew


;*** move right
lemmrg  ld a,(ix+lemdatsub)
        cp 16
        jr nz,lemmrgj
        call falchkr
        jr lemmrgk
lemmrgj call falchk
lemmrgk ld a,4-2
        jp nz,falbeg

        ld a,(ix+lemdatsub)             ;increase sub counter
        inc a
        cp 1
        scf:ccf
        call z,lemmrgc                  ;check for stopper
        jp c,lemmrgd
        cp 7
        jp z,lemmrga                    ;field middle reached
        cp 17
        jr z,lemmrg4                    ;try entering next field
        cp 24
        jr nz,lemmrg0
        ld (ix+lemdatstu),1
        jp lemmrg6                      ;next field entered
lemmrg0 ld (ix+lemdatsub),a
        ld b,(ix+lemdatxps)
        and 7
        jr nz,lemmrg1
        ld c,a                      ;*** next 4pixels
        ld a,b
        add 4
        ld (ix+lemdatxps),a
        ld b,a
        ld a,c
lemmrg1 ld hl,lemanirgt             ;*** update animation (a=sub[bit0-2], d=xpos)
        anirac
        add a
        add a
        add l
        ld l,a                          ;hl=animation data
        ld a,(hl):inc hl:ld (iy+16+4),a
        ld a,(hl):inc hl:ld (iy+16+5),a ;set bitmap
        ld a,(hl)
        add b                           ;calculate new x position
        cp (iy+16+6)
        jr z,lemmrg3
        ld (iy+16+6),a                  ;set new x position
        ld (iy+00+2),10                 ;restore background
        call rstrgt
        or a
        ret
lemmrg3 ld (iy+00+2),64                 ;no background to restore
        or a
        ret

lemmrgc push af
        ld l,(ix+lemdatmad+0)       ;*** check for stopper
        ld h,(ix+lemdatmad+1)
        call stpchk
        pop bc
        jr c,lemmrgl
        ld a,b
        ret
lemmrgl ld a,4-1                        ;stopper -> reverse moving
        ld c,0              ;(don't check bridge left up)
        ret

lemmrg4 ld l,(ix+lemdatmad+0)       ;*** try entering next field
        ld h,(ix+lemdatmad+1)
        inc hl:inc hl
        ld a,(hl)
        cp idm_brdg_rmrk
        jp z,brgbrg                     ;marked bridge right up
        cp idm_grnd_mark
        jr c,lemmrgf
        cp idm_grqm_br3m+1
        jr nc,lemmrgf
        call lemmrgg                    ;digger field
        ld a,(iy+16+6)
        sub 4
        ld (iy+16+6),a
        ld (iy+00+2),64
        ld hl,lemdrg
        jp jmpnew
lemmrgg sub 8
        ld (hl),a
        call fldchg
        jp fldupd
lemmrgf cp idm_door_lu
        jr c,lemmrg5                    ;solid -> reverse moving
        bit 6,a
        jr z,lemmrgh                    ;door -> enter
        cp idm_brdg_rf
        jr c,lemmrgi
        cp idm_brdg_rf_x+1
        jp c,brgrgu                     ;bridge -> right up
lemmrgi ld a,17
        jp lemmrg0                      ;open -> go on

lemmrgh call dorrgt                     ;right door entered
        jr nz,lemmrg5
        call fldmrg
        ld (ix+lemdatdir),1
        jp brgblf1

lemmrg5 ld a,4-12                   ;*** hit wall -> reverse moving    ##!!## plots wrong first row -> restores the first row of the block, better would be to prevent plotting the first time after reversing at all
        ld c,1              ;(check bridge left up)
lemmrgd add (iy+16+6)
        ld (ix+lemdatrxp),a
        ld (ix+lemdatdir),1 

        or a:dec c                      ;don't check bridge left up, when hit stopper in field middle
        ld bc,0
        call z,brgckr0                  ;check if bridge left up here
        jp c,lembru7        ;(c still 0 -> don't build, but walk)
        call brgckl                     ;check if bridge down
        jp c,brglfd

        ld hl,lemmlf
        jp jmpnew

lemmrga call lemmrgc                ;*** field middle reached
        jp c,lemmrgd                    ;stopper
        ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        push hl
        ld de,map_xlen*2
        add hl,de
        ld a,(hl)
        ld hl,fldlowjmp                 ;check field below
        call fldevt
        pop de
        jr nc,lemmrgb
        jp (hl)                         ;-> event happend
lemmrgb ld a,(de)
        and 63                          ;always open (bit6=1)
        ld hl,fldmidjmp                 ;check field itself
        call fldevt
        ld a,7
        jp nc,lemmrg0
        jp (hl)                         ;-> event happend

lemmrg6 call fldmrg                 ;*** next field entered
        ld (ix+lemdatsub),0  
        ld a,(ix+lemdatxps)
        add 4
        ld (ix+lemdatxps),a

        call brgckr                     ;check if bridge down
        jp c,brgrgd

        call falchk                     ;check if falling
        ld a,4-2
        jp nz,falbeg
        ld b,(ix+lemdatxps)
        xor a
        jp lemmrg1


;*** bridge left down
lembld  ld a,(ix+lemdatsub)
        sub 1
        jr c,lembld5                    ;try entering previous field
        cp 16
        jr z,lembld4                    ;previous field entered
        cp 20
        jp z,lembld8
        cp 7
        jp z,lembldb                    ;field middle reached (check for marked stopper, suicider)
        cp 15
        jp z,lembldc                    ;check for stopper

lembld0 ld (ix+lemdatsub),a
        ld b,(ix+lemdatxps)
        and 7
        cp 7
        jr nz,lembld1
        ld c,a                      ;*** previous 4pixels
        ld a,b
        sub 4
        ld (ix+lemdatxps),a
        ld b,a
        ld a,c
lembld1 ld hl,lemanilft             ;*** update animation (a=sub[bit0-2], d=xpos)
        anirac
        add a
        add a
        add l
        ld l,a                          ;hl=animation data
        ld a,(hl):inc hl:ld (iy+16+4),a
        ld a,(hl):inc hl:ld (iy+16+5),a ;set bitmap
        ld a,(hl):inc hl
        add b                           ;calculate new x position
        ld (iy+16+6),a                  ;set new x position
        add (hl)
        cp (ix+lemdatrxp)
        jr z,lembld3
        ld (ix+lemdatrxp),a             ;restore background
        call rstlft
        or a
        ret
lembld3 call brgmdw                     ;increase yposition
        call rstdwn                     ;set hori background restore
        or a
        ret

lembld4 ;...update bgr restore      ;*** next field entered -> continue with right walking
        ld hl,lemmlf
        ld (ix+lemdatjmp+0),l
        ld (ix+lemdatjmp+1),h
        ld (ix+lemdatrxp),-1
        jp lemmlf6

lembld5 ld l,(ix+lemdatmad+0)       ;*** try entering next field
        ld h,(ix+lemdatmad+1)
        dec hl:dec hl
        ld a,(hl)
        cp idm_brdg_lmrk
        jp z,brgblf
        bit 6,a
        jr nz,lembld6
        cp idm_door_lu
        jp nc,lemmlfe
        cp idm_grnd_mark
        jr c,lembld7
        cp idm_grqm_br3m+1
        jr nc,lembld7
        call lemmrgg                    ;digger field
        ld (iy+00+2),64
        ld (ix+lemdatdir),3             ;mark as left bridge digger
        ld hl,lemdlf6
        jp jmpnew

lemblde call fldmlf
        jp lemmlfh

lembld6 call fldmlf0                    ;open -> continue climbing down
        ld a,23
        jp lembld0
lembld7 ld (ix+lemdatdir),0             ;solid -> reverse moving
        ld hl,lembru
        jp jmpnew

lembld8 ld l,(ix+lemdatmad+0)       ;*** check for bridge left up
        ld h,(ix+lemdatmad+1)
        ld a,(hl)
        cp idm_brdg_lf
        jr c,lembld9
        cp idm_brdg_lf_x+1
        jr nc,lembld9
        ld hl,lemblu
        jp jmpnew
lembld9 ld a,20
        jp lembld0

lembldb ld l,(ix+lemdatmad+0)       ;*** check for marked stopper/suicider
        ld h,(ix+lemdatmad+1)
        push hl
        call stpchk
        pop hl
        jr c,lembldl
        ld a,(hl)
        cp idm_brdg_rf_s
        jr c,lembldd
        cp idm_brdg_rf_x
        jp c,lembrua
        jr nz,lembldd
        ;...suicider
lembldd ld a,7
        jp lembld0

lembldc ld l,(ix+lemdatmad+0)       ;*** check for stopper
        ld h,(ix+lemdatmad+1)
        call stpchk
        ld a,15
        jp nc,lembld0
lembldl ld (ix+lemdatdir),0             ;stopper -> reverse moving
        ld a,(ix+lemdatxps)
        sub 4
        ld (ix+lemdatxps),a
        dec (ix+lemdatsub)
        call brgmdw
        ld hl,lembru
        ld (ix+lemdatjmp+0),l
        ld (ix+lemdatjmp+1),h
        or a
        ret


;*** bridge left up
lemblu  ld a,(ix+lemdatsub)
        cp 7
        jr nz,lembluj
        ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        call falchl0
        jr z,lembluk
        call fldrdw0
        call fldmup
        inc (ix+lemdatyps)
        ld a,6
        jp falbeg

lembluk ld a,(ix+lemdatsub)
lembluj sub 1
        jp c,lemblu5                    ;try entering previous field
        cp 7
        jp z,lemblub                    ;field middle reached (check for marked stopper, suicider)
        cp 15
        jp z,lembluc                    ;check for stopper
        ;cp 16
        ;jp z,lemblu?                    ;previous field entered
lemblu0 ld (ix+lemdatsub),a
        ld b,(ix+lemdatxps)
        and 7
        cp 7
        jr nz,lemblu1
        ld c,a                      ;*** previous 4pixels
        ld a,b
        sub 4
        ld (ix+lemdatxps),a
        ld b,a
        ld a,c
lemblu1 ld hl,lemanilft             ;*** update animation (a=sub[bit0-2], d=xpos)
        anirac
        add a
        add a
        add l
        ld l,a                          ;hl=animation data
        ld a,(hl):inc hl:ld (iy+16+4),a
        ld a,(hl):inc hl:ld (iy+16+5),a ;set bitmap
        ld a,(hl):inc hl
        add b                           ;calculate new x position
        ld (iy+16+6),a                  ;set new x position
        add (hl)
        cp (ix+lemdatrxp)
        jr z,lemblu3
        ld (ix+lemdatrxp),a             ;restore background
        call rstlft
        or a
        ret
lemblu3 call brgmup                     ;decrease yposition
        call rstupw                     ;set hori background restore
        or a
        ret

lemblu8 call fldmlf0                ;*** continue with next bridge left up
        ld a,23
        jr lemblu0

lemblu5 call fldmup                 ;*** try entering previous field
        dec hl:dec hl
        ld a,(hl)
        cp idm_brdg_lf
        jr c,lemblu2
        cp idm_brdg_lf_x+1
        jr c,lemblu8                    ;bridge left up -> continue
lemblu2 inc hl:inc hl
        ld a,(hl)                       ;check if upper field is bridge up right
        cp idm_brdg_rmrk
        ld c,1
        jr z,lemblue
        dec c
        cp idm_brdg_rf
        jr c,lemblu6
        cp idm_brdg_rf_x+1
        jr nc,lemblu6
lemblue dec hl:dec hl                   ;yes -> check, if previous field is solid and no digger
        ld a,(hl)
        bit 6,a                         ;solid?
        jr nz,lemblu6
        cp idm_door_lu
        jr nc,lemblu6
        cp idm_grnd_mark                ;digger?
        jr c,lemblu7
        cp idm_grqm_br3m+1
        jr c,lemblu6                    ;yes
lemblu7 ld (ix+lemdatdir),0
        dec c
        jr z,lemblud
        ld a,(ix+lemdatyps)         ;*** solid, finished -> reverse moving on upper bridge
        sub 4
        ld (ix+lemdatyps),a             ;jump 4 pixels up
        ld a,(iy+00+8):sub 4:ld (iy+00+8),a
        ld a,(iy+16+8):sub 4:ld (iy+16+8),a
        call fldrdw0
        ld hl,lembru
        jp jmpnew
lemblud call fldrdw0                ;*** solid, marked   -> build bridge left up on upper field
        call fldmlf                     ;move lemming to left field
        ld a,(ix+lemdatfyp)
        add 4
        ld (iy+16+8),a
        jp brgbrg1                      ;build bridge left up

lemblu6 ld hl,lemmlf                    ;continue with left walking
        ld (ix+lemdatjmp+0),l
        ld (ix+lemdatjmp+1),h
        ld (ix+lemdatrxp),-1
        jp lemmlf4

lemblub ld l,(ix+lemdatmad+0)       ;*** check for (marked) stopper/suicider
        ld h,(ix+lemdatmad+1)
        push hl
        call stpchk
        pop hl
        jr c,lemblul
        ld a,(hl)
        cp idm_brdg_lf_s
        jr c,lemblu9
        cp idm_brdg_lf_x
        jp c,lembrua
        jr nz,lemblu9
        ;...suicider
lemblu9 ld a,7
        jp lemblu0

lembluc ld l,(ix+lemdatmad+0)       ;*** check for stopper
        ld h,(ix+lemdatmad+1)
        call stpchk
        ld a,15
        jp nc,lemblu0
lemblul call brgmup                     ;stopper -> reverse moving
        ld (ix+lemdatdir),0
        ld a,(ix+lemdatxps)
        sub 4
        ld (ix+lemdatxps),a
        dec (ix+lemdatsub)
        ld hl,lembrd
        ld (ix+lemdatjmp+0),l
        ld (ix+lemdatjmp+1),h
        or a
        ret


;*** move left
lemmlf  ld a,(ix+lemdatsub)
        or a
        jr nz,lemmlfd
        call falchkl
        jr lemmlfe
lemmlfd call falchk
lemmlfe ld a,4-2
        jp nz,falbeg

        ld a,(ix+lemdatsub)             ;decrease sub counter
        sub 1
        jp c,lemmlf4                    ;try entering previous field
        cp 7
        jp z,lemmlfa                    ;field middle reached
        cp 15
        scf:ccf
        call z,lemmlfc
        jp c,lemmlfg
        cp 16
        jp z,lemmlf6                    ;previous field entered
lemmlf0 ld (ix+lemdatsub),a
        ld b,(ix+lemdatxps)
        and 7
        cp 7
        jr nz,lemmlf1
        ld c,a                      ;*** previous 4pixels
        ld a,b
        sub 4
        ld (ix+lemdatxps),a
        ld b,a
        ld a,c
lemmlf1 ld hl,lemanilft             ;*** update animation (a=sub[bit0-2], d=xpos)
        anirac
        add a
        add a
        add l
        ld l,a                          ;hl=animation data
        ld a,(hl):inc hl:ld (iy+16+4),a
        ld a,(hl):inc hl:ld (iy+16+5),a ;set bitmap
        ld a,(hl):inc hl
        add b                           ;calculate new x position
        ld (iy+16+6),a                  ;set new x position
        add (hl)
        cp (ix+lemdatrxp)
        jr z,lemmlf3
        ld (ix+lemdatrxp),a             ;restore background
        ld (iy+00+2),10
        call rstlft
        or a
        ret
lemmlf3 ld (iy+00+2),64                 ;no background to restore
        or a
        ret

lemmlfc push af
        ld l,(ix+lemdatmad+0)       ;*** check for stopper
        ld h,(ix+lemdatmad+1)
        call stpchk
        pop bc
        jr c,lemmlfl
        ld a,b
        ret
lemmlfl ld a,(ix+lemdatxps)             ;stopper -> reverse moving
        sub 4
        ld (ix+lemdatxps),a
        dec (ix+lemdatsub)
        ld c,0                          ;don't check bridge right up
        scf
        ret

lemmlf4 ld l,(ix+lemdatmad+0)       ;*** try entering previous field
        ld h,(ix+lemdatmad+1)
        dec hl:dec hl
        ld a,(hl)
        cp idm_brdg_lmrk
        jp z,brgblf                     ;marked bridge left up
        cp idm_brdg_lf
        jr c,lemmlf7
        cp idm_brdg_lf_x+1
        jp c,brglfu                     ;bridge -> left up
lemmlf7 cp idm_grnd_mark
        jr c,lemmlff
        cp idm_grqm_br3m+1
        jr nc,lemmlff
        call lemmrgg                    ;digger field
        ld (iy+00+2),64
        ld hl,lemdlf
        jp jmpnew
lemmlff cp idm_door_lu
        jr c,lemmlf5                    ;solid -> reverse moving
        bit 6,a
        push af
        call fldmlf0
        pop af
        ld a,23
        jp nz,lemmlf0                   ;open and not a door -> go on
lemmlfh call dorlft                     ;left door entered
        jr nz,lemmlf5
        call fldmlf
        ld (ix+lemdatdir),0
        jp brgbrg1

lemmlf5 ld c,1
lemmlfg ld (ix+lemdatdir),0         ;*** hit wall -> reverse moving
        or a:dec c
        ld bc,0
        call z,brgckl0                  ;check if bridge right here
        jp c,lemblu7    ;(c still 0 -> don't build, but walk)
        call brgckr                     ;check if bridge down
        jp c,brgrgd0

        ld hl,lemmrg
        ld (ix+lemdatjmp+0),l
        ld (ix+lemdatjmp+1),h
        or a                            ;(prevent endless loop)
        ret

lemmlfa call lemmlfc                ;*** field middle reached
        jp c,lemmlfg
        ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        push hl
        ld de,map_xlen*2
        add hl,de
        ld a,(hl)
        ld hl,fldlowjmp                 ;check field below
        call fldevt
        pop de
        jr nc,lemmlfb
        jp (hl)                         ;-> event happend
lemmlfb ld a,(de)
        and 63                          ;always open (bit6=1)
        ld hl,fldmidjmp                 ;check field itself
        call fldevt
        ld a,7
        jp nc,lemmlf0
        jp (hl)                         ;-> event happend

lemmlf6 ld (ix+lemdatsub),16        ;*** previous field entered

        call brgckl                     ;check if bridge down
        jp c,brglfd

        call falchk                     ;check if falling
        ld a,4-2
        jp nz,falbeg
        ld b,(ix+lemdatxps)
        xor a
        jp lemmlf1


;*** build bridge right
lembbr  ld a,(ix+lemdatsub)
        inc a
        ld (ix+lemdatsub),a
        ld c,a
        and 7
        ld hl,lemanibbr
        push af
        call aniset0
        pop af

        push bc
        ld hl,256*1+SND_TOOL_BRIDGE
        call z,sndefx
        pop bc

        ld a,c
        cp 80
        jr z,lembbr1
        cp 40
        scf:ccf
        ret nz
        ld a,(ix+lemdatfxp)         ;*** bridge half
        add 8+6
        ld (iy+16+06),a                 ;set real xpos
        sub 2
        ld (ix+lemdatxps),a             ;set anim xpos
        ld a,(ix+lemdatfyp)
        add 4-6
        ld (iy+16+08),a                 ;set real ypos
        ld (ix+lemdatyps),a
        call fldrdw0                    ;redraw old field
        ld l,(ix+lemdatmad+0)       
        ld h,(ix+lemdatmad+1)
        inc hl:inc hl
        ld (hl),idm_brdg_rh             ;set half bridge
        jp lemdrg3
lembbr1 call fldmrg                 ;*** bridge full
        dec (hl)                        ;turn from half to full (with/without stopper)
        ld (ix+lemdatsub),6
        ld (ix+lemdatrxp),-1
        ld (iy+00+2),10
        call brgmup
        call brgmup
        call lemdrg3
        ld hl,lembru
        jp jmpnew


;*** build bridge left
lembbl  ld a,(ix+lemdatsub)
        inc a
        ld (ix+lemdatsub),a
        ld c,a
        and 7
        ld hl,lemanibbl
        push af
        call aniset0
        pop af

        push bc
        ld hl,256*1+SND_TOOL_BRIDGE
        call z,sndefx
        pop bc

        ld a,c
        cp 80
        jr z,lembbl1
        cp 40
        scf:ccf
        ret nz

        ld a,(ix+lemdatfxp)         ;*** bridge half
        add -2-6
        ld (iy+16+06),a                 ;set real xpos !!!
        ld (ix+lemdatxps),a             ;set anim xpos
        ld a,(ix+lemdatfyp)
        add 4-6
        ld (iy+16+08),a                 ;set real ypos
        ld (ix+lemdatyps),a
        call fldrdw0                    ;redraw old field
        ld l,(ix+lemdatmad+0)       
        ld h,(ix+lemdatmad+1)
        dec hl:dec hl
        ld (hl),idm_brdg_lh             ;set half bridge
        jp lemdrg3
lembbl1 call fldmlf                 ;*** bridge full
        dec (hl)                        ;turn from half to full (with/without stopper)
        ld (ix+lemdatsub),9
        ld (ix+lemdatrxp),-1
        ld (iy+00+2),10
        call brgmup
        call brgmup
        call lemdrg3
        ld l,(ix+lemdatmad+0)       
        ld h,(ix+lemdatmad+1)
        ld bc,-map_xlen*2
        add hl,bc
        call fldrdw
        ld hl,lemblu
        jp jmpnew


;*** dig right
lemdrg  call falchkr
        jp nz,lemdfi1
lemdrg6 ld hl,lemanidrg
        ld bc,2+1
lemdrg7 call lemdrg0
        ret nc
        jp jmpnew

lemdrg0 ld a,(ix+lemdatsub)         ;increase sub counter
        inc a
        cp 16
        jr c,lemdrg1
        push bc
        push hl
        ld hl,256*1+SND_TOOL_DIG
        call sndefx
        pop hl
        pop bc
        xor a
lemdrg1 call aniset                 ;set animation
        ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        add hl,bc
        ld a,(hl)
        add 16                      ;increase erosion
        cp 13*16
        jr nc,lemdrg2
        ld (hl),a
        or a
        ret
lemdrg2 and 15                      ;next erosion reached
        ld (hl),a
        dec hl
        ld a,(hl)
        and #03
        jr z,lemdrg4
        inc (hl)                    ;stone more broken
lemdrg3 call fldchg
        call fldupd
        or a
        ret
lemdrg4 ld a,(hl)                   ;stone completely broken
        ld (hl),idm_empt
        push af
        push hl
        call lemdrg3
        pop hl
        pop af
        call keyopn
        ld (ix+lemdatsub),-1        ;stone removed -> start finished animation
        ld hl,lemdfi
        scf
        ret


;*** dig left
lemdlf  call falchkl
        jr nz,lemdfi2
lemdlf6 ld hl,lemanidlf
        ld bc,-2+1
        jr lemdrg7


;*** dig finished
lemdfi  ld a,(ix+lemdatsub)
        inc a
        cp 24
        jr nc,lemdfi0
        ld hl,lemanidfi
        ld (ix+lemdatsub),a
        rra
        and 3
        jp aniset0

lemdfi0 call fldrdw0                ;digging finished
        bit 0,(ix+lemdatdir)
        jr nz,lemdfi2
lemdfi1 ld (ix+lemdatsub),16        ;continue right walking
        ld a,(iy+16+6)
        add 4
        ld (iy+16+6),a
        bit 1,(ix+lemdatdir)
        res 1,(ix+lemdatdir)
        ld hl,lemmrg
        jp z,jmpnew
        ld (iy+00+2),10
        ld hl,lembrd
        jp jmpnew
lemdfi2 ld (ix+lemdatsub),0         ;continue left walking
        bit 1,(ix+lemdatdir)
        res 1,(ix+lemdatdir)
        ld hl,lemmlf
        jp z,jmpnew
        ld (iy+00+2),10
        ld hl,lembld
        jp jmpnew


;*** dig down
lemddw  call falchk0
        jr nz,lemddw1
        ld hl,lemaniddw
        ld bc,map_xlen*2+1
        call lemdrg0
        ret nc
        call fldrdw0
lemddw1 ld a,(ix+lemdatrbm)         ;stone removed -> continue walking
        ld (ix+lemdatsub),a
        ld a,(iy+16+8)
        sub 2
        ld (iy+16+8),a
        bit 0,(ix+lemdatdir)
        ld hl,lemmrg
        jp z,jmpnew
        ld hl,lemmlf
        jp jmpnew


;*** stopper
lemstp  call falchk0                    ;check, if ground removed
        jr nz,lemstp1
lemstp0 ld a,(ix+lemdatsub)         ;*** stopper on bridge
        inc a                           ;increase sub counter
        and 15
        ld hl,lemanistp
        jp aniset
lemstp1 bit 0,(ix+lemdatdir)            ;turn into faller
        ld c,1
        jr z,lemstp2
        ld a,(ix+lemdatxps)
        add 4
        ld (ix+lemdatxps),a
        ld c,15
lemstp2 ld (ix+lemdatsub),c
        ld a,4-2
        jp falbeg


;*** suicider
lemsui  ;...
        ret


;*** goal reached
lemgol  ld a,(ix+lemdatsub)             ;increase sub counter
        inc a
        cp 10
        ld hl,lemanigol
        jp nz,aniset
lemgol1 call fldrdw0                    ;lemming in goal -> remove it
        ld c,1
        scf
        ret


;*** explosion begins
lemexb  ld a,(ix+lemdatsub)
        inc a
        cp 16
        jr z,lemexb1
        ld hl,lemaniexb
        jp aniset
lemexb1 ld (ix+lemdatsub),-1
        ld a,(ix+lemdatyps)
        sub 4
        ld (ix+lemdatyps),a
        ld hl,256*1+SND_DIE_POP
        call sndefx
        ld hl,lemexp
        jp jmpnew


;*** smashed by trap
lemsma  ld a,(ix+lemdatsub)
        inc a
        cp 12
        ld hl,lemanisma
        jp nz,aniset
        ld l,(ix+lemdatmad+0)   ;finished -> restore background
        ld h,(ix+lemdatmad+1)
        ld a,(iy+00+08)
        call rstyof
        push hl
        push af
        call fldrdw             ;restore field up/left
        pop af
        pop hl
        cp 5
        jr c,lemsma1
        push hl                 ;lemming partly in lower field
        ld bc,map_xlen*2
        add hl,bc
        call fldrdw
        pop hl
lemsma1 ld a,(ix+lemdatxps)     ;0,4,8
        cp 4
        jr c,lemexp1
        inc hl:inc hl           ;lemming partly in next field
        call fldrdw
        jr lemexp1


;*** explosion
lemexp  ld a,(ix+lemdatsub)
        inc a
        cp 12
        ld hl,lemaniexp
        call lemdro0
        ret nc
        call fldrdw0
lemexp1 ld c,0
        scf
        ret


;*** drowning in water
lemdro  ld a,(ix+lemdatsub)             ;increase sub counter
        inc a
        cp 21
        ld hl,lemanidro
lemdro0 ccf
        ld c,0
        ret c                           ;lemming dead -> remove it
lemdro1 ld (ix+lemdatsub),a
        add a
        call aniset0
        inc hl
        ld a,(hl)
        add (ix+lemdatyps)
        ld (iy+16+8),a
        or a
        ret


;*** splashing after falling too fast
lemspl  ld a,(ix+lemdatsub)             ;increase sub counter
        inc a
        cp 9
        ld hl,lemanispl
        jr c,lemdro1
        call lemgol1
        dec c
        ret


;*** splashing after falling too fast on bridge
lemsplb ld a,(ix+lemdatsub)             ;increase sub counter
        inc a
        cp 9
        ld hl,lemanispl
        jr c,lemdro1
        call fldrdw0
        ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        ld bc,map_xlen*2
        add hl,bc
        call fldrdw
        ld c,0
        scf
        ret


;*** falling with parachute
lempar  ld bc,6*256+0                   ;test for bridge (lower ofs 8-2, upper ofs 2-2)
        call falbrg
        jr c,lemfale

        ld a,(ix+lemdatfal)
        cp 11
        jr nz,lempar1
        ld l,(ix+lemdatmad+0)       ;*** check for ground hit
        ld h,(ix+lemdatmad+1)
        ld bc,map_xlen*2*2
        add hl,bc
        bit 6,(hl)
        jr nz,lempar1
        ld a,(ix+lemdatyps)             ;solid -> switch to walking
        add 4
        ld (ix+lemdatyps),a
        ld a,(iy+16+8)
        add 4
        ld (iy+16+8),a
        ld bc,-map_xlen*2
        add hl,bc
        call fldmdw0
        jp lemfal9
lempar1 ld a,6                          ;not solid -> keep falling
        ld c,1
        ld hl,lemanipar
        jp lemfal0


;*** falling fast
lemfal  ld a,(ix+lemdatrxp)             ;update fast falling counter
        bit 4,a
        jr nz,lemfal3                   ;16 reached -> already falling to fast
        inc a
        ld (ix+lemdatrxp),a
lemfal3 ld bc,10*256+4                  ;test for bridge (lower ofs 12-2, upper ofs 6-2)
        call falbrg
        jr nc,lemfald
        bit 4,(ix+lemdatrxp)            ;falling on bridge
        jr z,lemfale
        ld (ix+lemdatsub),-1            ;falling too fast -> splashing
        ld hl,256*1+SND_DIE_SPLASH
        call sndefx
        ld hl,lemsplb
        jp jmpnew
lemfale ld (ix+lemdatsub),c             ;turn into brigde walker
        ld (ix+lemdatxps),a
        ld (ix+lemdatyps),b
        ld (iy+16+6),a
        ld (iy+16+8),b
        push hl
        call fldrdw0
        call fldmdw
        pop hl
        ld (ix+lemdatrxp),-1
        jp jmpnew

lemfald ld a,(ix+lemdatfal)
        cp 4-2
        jr nz,lemfal4
        ld l,(ix+lemdatmad+0)       ;*** check for ground hit
        ld h,(ix+lemdatmad+1)
        ld bc,map_xlen*2
        add hl,bc
        bit 6,(hl)
        jr nz,lemfal4                   ;not solid -> keep falling
        bit 4,(ix+lemdatrxp)
        jr z,lemfal5
        ld (ix+lemdatsub),-1            ;falling too fast -> splashing
        ld hl,256*1+SND_DIE_SPLASH
        call sndefx
        ld hl,lemspl
        jp jmpnew

lemfal5 ld bc,-map_xlen*2           ;*** soft ground hit -> switch to walking
        add hl,bc
lemfal9 call fldrdw                     ;redraw field
        bit 7,(ix+lemdatrxp)
        jr nz,lemfal6
        ld (ix+lemdatsub),0             ;left side falling
        ld a,0
        jr lemfal7
lemfal6 ld (ix+lemdatsub),16            ;right side falling
        ld a,(ix+lemdatxps)
        add 2
        ld (ix+lemdatxps),a
        ld a,8

lemfal7 bit 0,(ix+lemdatdir)
        ld hl,lemmrg
        jr z,lemfal8
        ld hl,lemmlf
        add 4
lemfal8 ld (ix+lemdatfal),a
        ld (ix+lemdatrxp),-1
        jp jmpnew

lemfal4 call falpar                 ;*** check for parachute
        jr nc,lemfala

        ld hl,256*1+SND_TOOL_PARACHUTE
        call sndefx

        ld hl,lempar                    ;switch to parachute handler
        ld (ix+lemdatjmp+0),l
        ld (ix+lemdatjmp+1),h
        ld a,(ix+lemdatfyp)
        ld (ix+lemdatyps),a
        ld (ix+lemdatfal),11
        call fldmup
        ld c,11*6
        call falbeg4
        ld l,(ix+lemdatrcf+0)
        ld h,(ix+lemdatrcf+1)
        inc hl:inc hl
        ld (hl),1
        jp lempar1

lemfala ld a,12
        ld c,2
        ld hl,lemanifal

lemfal0 push hl                     ;*** fall down (c=yskip 1/2, a=bitmap line skip 6/12)
        ld (lemfal1+1),a
        ld a,(ix+lemdatyps)             ;increase ypos, set restore and lemming
        ld (iy+00+8),a
        add c
        ld (ix+lemdatyps),a
        ld (iy+16+8),a
        ld l,(ix+lemdatrcf+0)           ;increase restore bitmap pointer
        ld h,(ix+lemdatrcf+1)
        inc hl:inc hl:inc hl            ;(hl)=bitmap pointer
        ld a,(ix+lemdatfal)
        add c
        cp 12
        jr nz,lemfal1
        ex de,hl                    ;*** new field -> get new field bitmap pointer
        call fldmdw                     ;update map address

        ld a,(hl)
        add a
        ld hl,gmp_table
        ld l,a
        ld c,(hl):inc hl
        ld b,(hl)
        ld hl,10 
        add hl,bc
        ex de,hl                        ;de=bitmap address
        ld (ix+lemdatrbm+0),e
        ld (ix+lemdatrbm+1),d
        ld (hl),e:inc hl
        ld (hl),d:inc hl
        dec de                          ;de=encoding byte address
        ld (hl),e:inc hl
        ld (hl),d
        xor a
        jr lemfal2
lemfal1 ld bc,0                     ;*** same field -> increase pointer in current bitmap
        ld e,(hl):inc hl
        ld d,(hl)
        ex de,hl
        add hl,bc
        ex de,hl
        ld (hl),d:dec hl
        ld (hl),e                       ;store bitmap pointer
lemfal2 ld (ix+lemdatfal),a             ;update field position
        ld a,(ix+lemdatsub)
        inc a
        and 7
        pop hl
        jp aniset


;*** starting
lemsta  ld a,(ix+lemdatsub)
        inc a
        cp 4+1
        jr z,lemsta2
        ld (ix+lemdatsub),a
        add a
        ld (lemsta1+1),a
        ld b,a
        add a
        add a       ; 8/ 16/ 24/ 32
        cpl         ;-9/-17/-25/-33
        ld c,a
        bit 6,(ix+lemdatrac)
        ld hl,lemanifal0
        jr z,lemsta5
        ld hl,lemanifal1
lemsta5 ld a,b
        add l
        ld l,a
        ld e,(hl):inc hl
        ld d,(hl)
        ex de,hl
        inc hl:inc hl:inc hl
        ld e,(hl):inc hl
        ld d,(hl)
        push de                 ;de=bitmap address
        ld hl,4*8+1
        add hl,de   ;33->24,16,8,0
        ld b,-1
        add hl,bc
        ex de,hl                ;de=moved bitmap pointer
        ld l,(ix+lemdatrcf+0)
        ld h,(ix+lemdatrcf+1)
        inc hl:inc hl
lemsta1 ld (hl),0               ;set y-size
        inc hl
        ld (hl),e:inc hl        ;store bitmap pointer
        ld (hl),d:inc hl
        pop de
        dec de
        ld (hl),e:inc hl        ;store encoding pointer
        ld (hl),d
        or a
        ret
lemsta2 bit 0,(ix+lemdatdir)    ;entrance left -> turn into falling
        jr nz,lemsta3
        ld (ix+lemdatsub),0 
        jr lemsta4
lemsta3 ld (ix+lemdatsub),16
        ld a,(ix+lemdatxps)
        add 8
        ld (ix+lemdatxps),a
lemsta4 ld (iy+16+2),10
        ld a,10
        ld e,4
        jp falbeg0


;==============================================================================
;### BRIDGES ##################################################################
;==============================================================================

;### BRGBLF -> begin bridge left building
brgblf  call fldrdw0        ;redraw current field
brgblf1 ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        call brgblf0
        jp jmpnew
brgblf0 dec hl:dec hl
        ld (hl),idm_empt    ;remove marked bridge and update
        call fldchg
        call fldupd
        ld (iy+00+02),64
        ld a,(ix+lemdatfxp)
        sub 2
        ld (iy+16+06),a
        ld (ix+lemdatsub),0
        ld hl,lembbl
        ret

;### BRGBRG -> begin bridge right building
brgbrg  call fldrdw0        ;redraw current field
brgbrg1 ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        call brgbrg0
        jp jmpnew
brgbrg0 inc hl:inc hl
        ld (hl),idm_empt    ;remove marked bridge and update
        call fldchg
        call fldupd
        ld (iy+00+02),64
        ld a,(ix+lemdatfxp)
        add 8
        ld (iy+16+06),a
        ld (ix+lemdatsub),0
        ld hl,lembbr
        ret

;### BRGMUP -> decreases y-position
brgmup  dec (ix+lemdatyps)
        dec (iy+16+8)
        dec (iy+00+8)
        ret

;### BRGMDW -> increases y-position
brgmdw  inc (ix+lemdatyps)
        inc (iy+16+8)
        inc (iy+00+8)
        ret

;### BRGCKR -> check for bridge right down
;### Output     CF=1 yes
brgckr  ld bc,map_xlen*2
brgckr0 ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        add hl,bc
        ld a,(hl)
        cp idm_brdg_lf
        ccf
        ret nc
        cp idm_brdg_lf_x+1
        ret

;### BRGCKL -> check for bridge left down
;### Output     CF=1 yes
brgckl  ld bc,map_xlen*2
brgckl0 ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        add hl,bc
        ld a,(hl)
        cp idm_brdg_rf
        ccf
        ret nc
        cp idm_brdg_rf_x+1
        ret

;### BRGRGD -> start bridge right down climbing
;### Input      HL=mapmem next line
brgrgd0 call brgmup                     ;decrease yposition
brgrgd  call fldmdw0
        ld a,(ix+lemdatxps)
        sub 4
        ld (ix+lemdatxps),a
        xor a
        ld bc,lembrd
        ld de,lembrd0
        jr brglfu1

;### BRGLFD -> start bridge left down climbing
;### Input      HL=mapmem next line
brglfd  call fldmdw0
        ld a,16
        ld bc,lembld
        ld de,lembld0
        jr brglfu1

;### BRGRGU -> start bridge right up climbing
brgrgu  ld a,17
        ld de,lembru0
        ld bc,lembru
        ld (iy+00+2),10
        jr brglfu1

;### BRGLFU -> start bridge left up climbing
brglfu  ld de,lemblu0
        ld bc,lemblu
        call fldmlf
        ld a,23
brglfu1 ld (ix+lemdatjmp+0),c
        ld (ix+lemdatjmp+1),b
        ex de,hl
        jp (hl)




;==============================================================================
;### FIGURE DATA ##############################################################
;==============================================================================

;### FIGDRP -> checks, if new lemmings should be dropped
figdrpnum   equ 0   ;number of lemmings
figdrpxps   equ 1   ;xpos below "go"
figdrpyps   equ 2   ;ypos below "go"
figdrpdir   equ 3   ;direction
figdrpdly   equ 4   ;delay between dropping
figdrpcnt   equ 5   ;start delay/delay counter
figdrprac   equ 6   ;lemming race (0=race0, 1=race1, 2=mixed)
figdrprsv   equ 7   ;*reserved*

figdrplen   equ 8
figdrpmax   equ 4

figdrpmem
ds figdrplen*figdrpmax

ctractnum   db 0            ;number of active lemmings/last used control pair
ctrrectab   ds LEM_MAX*2    ;lemming data record for each control pair


figdrp  nop
        ld ix,figdrpmem
        ld bc,figdrpmax*256+figdrpmax   ;b=counter, c=number of active droppers
figdrp1 dec c
        ld a,(ix+figdrpnum)
        or a
        jr z,figdrp2            ;dropper empty -> decrease c
        inc c
        dec (ix+figdrpcnt)
        jr nz,figdrp2
        push ix
        push bc
        ld a,(ix+figdrpdly)     ;delay finished -> update it
        ld (ix+figdrpcnt),a
        dec (ix+figdrpnum)
        ld c,(ix+figdrpxps)
        ld b,(ix+figdrpyps)
        ld a,(ix+figdrprac)     ;0/1, 2=mixed
        cp 2
        jr nz,figdrp3
        ld a,(1*lemraclen+lemracmem+lemraccnt)
        or a
        jr z,figdrp3            ;race1 complete -> use race0
        ld a,(0*lemraclen+lemracmem+lemraccnt)
        or a
        ld a,1
        jr z,figdrp3            ;race0 complete -> use race1
        push bc
        call clcrnd
        pop bc
        ld a,l
        and 1                   ;both not complete -> random race
figdrp3 or a
        ld hl,0*lemraclen+lemracmem+lemraccnt
        jr z,figdrp4
        ld hl,1*lemraclen+lemracmem+lemraccnt
figdrp4 dec (hl)
        rrca:rrca               ;a=0/1 -> 0/128, 0/64
        ld l,a
        ld a,(ix+figdrpdir)
        call figadd             ;drop new lemming
        call figdrp0
        pop bc
        pop ix
figdrp2 ld de,figdrplen
        add ix,de
        djnz figdrp1
        inc c:dec c             ;check if droppers left
        ret nz
        ld a,#c9
        ld (figdrp),a           ;no -> deactive this routine
        ret

figdrp0 nop                 ;start music with first dropped lemming
        ld h,VOL_GAME
        call sndvol
        ld a,r
        and 3
        cp 3
        jr nz,figdrp5
        ld a,2
figdrp5 ld l,a
        call sndmus
        ld a,#c9
        ld (figdrp0),a
        ret

;### FIGINI -> initialize lemming figure data
figini  xor a
        ld (ctractnum),a
        ld (stpnum),a
        ld ix,lemdatmem
        ld hl,wingamdat_lem+2
        ld de,bkgrrest+10
        ld iyl,LEM_MAX
figini1 ld (ix+lemdatdir),128   ;no lemming
        ld bc,lemn              ;inactive jump
        ld (ix+lemdatjmp+0),c
        ld (ix+lemdatjmp+1),b
        ld (hl),64              ;deactivate controls
        ld bc,16
        add hl,bc
        ld (hl),64
        ld c,16
        add hl,bc
        ex de,hl
        ld (ix+lemdatrcv+0),l   ;restore v
        ld (ix+lemdatrcv+1),h
        ld c,2*8
        add hl,bc
        ld (ix+lemdatrcf+0),l   ;restore f
        ld (ix+lemdatrcf+1),h
        ld c,9
        add hl,bc
        ld (ix+lemdatrch+0),l   ;restore h
        ld (ix+lemdatrch+1),h
        ld c,10+12+10
        add hl,bc
        ex de,hl
        ld c,lemdatlen
        add ix,bc
        dec iyl
        jr nz,figini1
        ret

;### FIGADD -> creates a new lemming
;### Input      C=map xpos, B=map ypos, A=move direction (0=right, 1=left), L=race (0=race0, 64=race1)
;### Output     ?
figaddd db 4,6,1:dw lmasp0+10,lmasp0+9,4*8

figadd  ld ix,lemdatmem         ;find free lemming
        ld de,lemdatlen
figadd1 bit 7,(ix+lemdatdir)
        jr nz,figadd2
        add ix,de
        jr figadd1
figadd2 ld (ix+lemdatdir),a     ;direction
        ld (ix+lemdatsub),0     ;sub = 0
        ld (ix+lemdatrac),l     ;race
        push bc
        ld h,b
        ld l,fld_ylen
        call clcmu8
        ld a,map_yofs
        add l
        ld (ix+lemdatyps),a     ;ypos
        sub 12
        ld (ix+lemdatfyp),a
        ld h,c
        ld l,fld_xlen
        call clcmu8
        ld a,map_xofs
        add l
        ld (ix+lemdatxps),a     ;xpos
        ld (ix+lemdatfxp),a
        pop hl
        ld l,map_xlen
        call clcmu8
        ld b,0
        add hl,bc
        add hl,hl
        ld bc,mapmem
        add hl,bc
        ld (ix+lemdatmad+0),l   ;map address
        ld (ix+lemdatmad+1),h
        ld hl,lemsta            ;jump "lemming start"
        ld (ix+lemdatjmp+0),l
        ld (ix+lemdatjmp+1),h
        ld hl,ctractnum             ;*** set control address
        ld a,(hl)
        ld (ix+lemdatctn),a     ;set control id
        inc (hl)
        add a
        ld l,a
        ld h,0
        ld bc,ctrrectab
        add hl,bc
        ld e,ixl:ld (hl),e:inc hl
        ld e,ixh:ld (hl),e      ;store record address in control table
        add a:add a:add a
        ld l,a
        ld h,0
        add hl,hl
        ld bc,wingamdat_lem     ;calculate control address
        add hl,bc
        ld (ix+lemdatctr+0),l
        ld (ix+lemdatctr+1),h   ;set control address
        push hl:pop iy
        ld (iy+16+2),64         ;init gui controls
        ld (iy+00+2),10
        ld e,(ix+lemdatrcf+0):ld (iy+00+4),e
        ld d,(ix+lemdatrcf+1):ld (iy+00+5),d
        bit 0,(ix+lemdatdir)
        ld a,(ix+lemdatxps)
        jr z,figadd3
        add 6
figadd3 ld (iy+00+6),a
        ld a,(ix+lemdatyps)
        ld (iy+00+8),a
        ld hl,figaddd
        ld bc,9
        ldir
        ret

;### FIGDEL -> removes lemming
;### Input      IX=lemming data record, C=reason (0=dead, 1=saved)
;### Destroyed  AF,BC,DE,HL,IY
figdel  push ix
        ld hl,ctractnum
        dec (hl)
        ld e,(hl)
        ld a,(ix+lemdatctn)
        cp e
        jr nz,figdel5
        ld l,(ix+lemdatctr+0)
        ld h,(ix+lemdatctr+1)
        ld de,16+2
        jr figdel6

figdel5 push bc             ;*** move last control to current freed one
        add a
        ld l,a
        ld h,0
        ld bc,ctrrectab
        add hl,bc
        ex de,hl                ;de=current entry in ctrrectab
        ld h,0
        add hl,hl
        add hl,bc               ;hl=last entry -> to be moved
        ld a,(hl):ld iyl,a
        ldi
        ld a,(hl):ld iyh,a      ;iy=record with control to be moved
        ldi                     ;move last entry to current
        ld e,(ix+lemdatctr+0)
        ld d,(ix+lemdatctr+1)   ;de=dst
        ld l,(iy+lemdatctr+0)
        ld h,(iy+lemdatctr+1)   ;hl=src
        ld (iy+lemdatctr+0),e
        ld (iy+lemdatctr+1),d   ;set new control address for moved one
        ld bc,2*16
        ldir                    ;move control records
        ld a,(ix+lemdatctn)
        ld (iy+lemdatctn),a     ;set new control id
        pop bc
        ld de,-16+2

figdel6 add hl,de               ;hide controls
        ld (hl),64
        ld de,-16
        add hl,de
        ld (hl),64

        ld (ix+lemdatdir),128
        ld hl,lemn
        ld (ix+lemdatjmp+0),l
        ld (ix+lemdatjmp+1),h
        ld a,(ix+lemdatrac)
        rrca:rrca               ;0/64 -> 0/32 -> 0/16
        ld ix,lemracmem
        ld e,a
        ld d,0
        add ix,de
        dec c
        jr z,figdel1
        call proded             ;CF=1 too many dead lemmings, ZF=1 no active lemmings
        jr c,figdel4
        scf
        jr z,figdel2
        pop ix
        ret
figdel4 ld a,(gamkal)           ;too many dead -> kill all remaining, too
        cp #c9
        jr z,figdel7
        ld bc,256*50*3+00
        ld hl,msg_ohno
        call dspmsg
        call gamkal
figdel7 pop ix
        ret
figdel1 call prosav
figdel2 push ix:pop iy
        pop ix
        ret nc
        ld a,1
        ld (iy+lemracall),a
        ld iy,lemracmem
        ld bc,(lemracnum-1)
        ld de,lemraclen
figdel3 and (iy+lemracall)
        add iy,de
        djnz figdel3
        or a
        ret z
        ld (gamstawin),a           ;all required of all races saved
        ret

;### FIGSRC -> search for lemming (walker 0-15, digger, stopper) on field
;### Input      HL=map address
;### Output     CF=0 no lemming
;###            CF=1 lemming found, IX=data record, IY=control record, C=stopper (0=no, 1=yes)
;### Destroyed  AF,BC,DE,HL,IX,IY
figsrcjmp   dw lemmrg,lemmlf,lemstp,lemdrg,lemdlf,lemddw
figsrcnum   equ 6

figsrc  ld (figsrc7+1),hl
        ld ix,lemdatmem
        ld iyl,LEM_MAX
figsrc1 bit 7,(ix+lemdatdir)
        jr nz,figsrc6
figsrc7 ld bc,0
        ld l,(ix+lemdatmad+0)   ;search map address
        ld h,(ix+lemdatmad+1)
        or a
        sbc hl,bc
        jr nz,figsrc6
        ld hl,figsrcjmp         ;map address found -> search type jump
        ld iyh,figsrcnum
        ld c,(ix+lemdatjmp+0)
        ld b,(ix+lemdatjmp+1)
figsrc2 ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ex de,hl
        or a
        sbc hl,bc
        ex de,hl
        jr z,figsrc4
figsrc3 dec iyh                 ;not found -> next type jump
        jr nz,figsrc2
figsrc6 ld bc,lemdatlen         ;not found -> next lemming
        add ix,bc
        dec iyl
        jr nz,figsrc1
        or a
        ret
figsrc4 ld a,iyh
        cp figsrcnum-2
        ld c,1
        jr z,figsrc5
        dec c
        jr c,figsrc5
        ld a,(ix+lemdatsub)
        cp 16+1
        jr nc,figsrc3
figsrc5 ld a,(ix+lemdatctr+0):ld iyl,a
        ld a,(ix+lemdatctr+1):ld iyh,a
        scf
        ret


;==============================================================================
;### MISC #####################################################################
;==============================================================================

;### JMPNEW -> changes the lemming jump routine and runs it
;### Input      HL=new address
jmpnew  ld (ix+lemdatjmp+0),l
        ld (ix+lemdatjmp+1),h
        jp (hl)

;### ANISET -> updates animation counter and set bitmap
;### Input      A=animation counter, HL=animation table
;### Output     CF=0
;### Destroyed  AF,HL
aniset  ld (ix+lemdatsub),a
aniset0 anirac
        add a
        add l
        ld l,a
        ld a,(hl):ld (iy+16+4),a:inc hl
        ld a,(hl):ld (iy+16+5),a        ;set bitmap
        or a
        ret

;### FALCHKL -> check if falling should start on left side (include left bridge)
;### Output     NZ=yes
falchkl call falchk0
        ret z
falchl0 ld a,(hl)
        cp idm_brdg_lf
        ret z
        cp idm_brdg_lf_s
        ret z
        cp idm_brdg_lf_x
        ret

;### FALCHKR -> check if falling should start on right side (include right bridge)
;### Output     NZ=yes
falchkr call falchk0
        ret z
falchr0 ld a,(hl)
        cp idm_brdg_rf
        ret z
        cp idm_brdg_rf_s
        ret z
        cp idm_brdg_rf_x
        ret

;### FALCHK -> check if falling should start
;### Output     NZ=yes
falchk  ld a,(ix+lemdatsub)
        cp 17
        jr nc,falchk1
falchk0 ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        ld bc,map_xlen*2
        add hl,bc
        or a
        bit 6,(hl)
        ret
falchk1 xor a
        ret

;### FALBEG -> prepares everything for a falling lemming
;### Input      A=start yofs in field, (ix+lemdatsub)=walk position (0-16)
falbeg  ld e,0
falbeg0 push af
        ld a,(ix+lemdatsub)
        ld c,a
        and #f8 ;0/8/16
        rra     ;0/4/8
        neg
        add (ix+lemdatxps)
        ld (iy+00+6),a                  ;set field xpos for restoring
        ld b,a
        ld a,c
        cp 8
        ld a,0
        jr c,falbeg1                    ;0-7  -> left side falling
        set 7,e                         ;8-16 -> right side falling
        ld a,6
falbeg1 ld (ix+lemdatrxp),e
        add b
        ld (iy+16+6),a
        ld (ix+lemdatxps),a
        ld a,c
        or a                            ;just entered from left?
        ld de,-2
        jr z,falbeg2
        cp 16                           ;just entered from right?
        ld de,2
        jr z,falbeg2
        ld e,0                          ;was already inside field
falbeg2 ld (ix+lemdatsub),0             ;animation=0

        pop af
        ld (ix+lemdatfal),a             ;position=4-2
        ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        push af
        push hl
        add hl,de
        call fldrdw
        pop hl
        pop af
        call falbeg3
        ld hl,lemfal
        jp jmpnew

falbeg3 ld c,a                  ;a=yofs, hl=mad
        add a
        add c
        add a                   ;a=a*6
        ld c,a
falbeg4 ld b,0                  ;bc=offset in bitmap
        ld a,(hl)
        add a
        ld hl,gmp_table
        ld l,a
        ld e,(hl):inc hl
        ld d,(hl)
        ex de,hl                ;hl=source      field   header
        ld e,(ix+lemdatrcf+0)   ;de=destination restore header
        ld d,(ix+lemdatrcf+1)
        ld (iy+00+2),10         ;set control
        ld (iy+00+4),e
        ld (iy+00+5),d
        ldi:inc bc              ;width in bytes
        ldi:inc bc              ;width in pixels
        ld a,2
        ld (de),a               ;height in pixels
        inc hl:inc de
        push de                 ;(sp)=dst
        ld e,(hl):inc hl
        ld d,(hl):inc hl        ;de=bmpadr
        ld (ix+lemdatrbm+0),e
        ld (ix+lemdatrbm+1),d
        ex de,hl                ;de=src
        add hl,bc
        ld c,l
        ld b,h                  ;bc=bmpadr+ofs
        pop hl                  ;hl=dst
        ld (hl),c:inc hl
        ld (hl),b:inc hl        ;store bmpadr+ofs
        ex de,hl                ;hl=src, de=dst
        ldi:ldi:ldi:ldi         ;copy 1W encoding pointer, 1W total length
        ret


;### FALPAR -> checks, if parachute on field and converts into one
;### Output     CF=1 -> converted to parachute, ZF=1 parachute was already used
falpar  ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        ld a,(hl)
        cp idm_para_used
        scf
        ret z       ;cf=1, zf=1 -> already used
        cp idm_para_mark
        scf:ccf
        ret nz      ;cf=0
        ld (hl),idm_para_used
        call fldchg
        call fldupd             ;turn field into used parachute
        xor a
        sub 1       ;cf=1, zf=0 -> marked to used
        ret

;### FALBRG -> checks, if falling on bridge
;### Input      (lemdatfal), (lemdatmad), C=upper ofs, B=lower ofs
;### Output     CF=1 yes, hl=jmpnew, c=sub, a=xpos, b=ypos
falbrg  ld a,(ix+lemdatfal)
        cp b
        jr z,falbrg3
        cp c
        scf:ccf
        ret nz
        ld a,4                              ;*** upper position
        ld (falbrg6+1),a
        ld a,#86
        ld (falbrg6+2+1),a
        bit 7,(ix+lemdatrxp)
        jr nz,falbrg2
        call brgckr                     ;left  side falling -> check for bridge left  up
        ld hl,lembrd
        ld de,lemblu
        ld c,0
falbrg1 ret nc
        or a
        bit 0,a                         ;check, if full bridge
        ret nz
        jr falbrg5                      ;yes -> hit
falbrg2 call brgckl                     ;right side falling -> check for bridge right up
        ld hl,lembru
        ld de,lembld
        ld c,8
        jr falbrg1
falbrg3 ld a,12                             ;*** lower position
        ld (falbrg6+1),a
        ld a,#96
        ld (falbrg6+2+1),a
        bit 7,(ix+lemdatrxp)
        jr nz,falbrg4
        call brgckl                     ;left  side falling -> check for bridge right up
        ret nc
        ld hl,lembru
        ld de,lembld
        ld c,0
        jr falbrg5
falbrg4 call brgckr                     ;right side falling -> check for bridge left  up
        ret nc
        ld hl,lembrd
        ld de,lemblu
        ld c,8                          ;c=xofs
falbrg5 bit 0,(ix+lemdatdir)                ;*** hit bridge
        jr z,falbrg6
        ex de,hl                        ;hl=new jmp
falbrg6 ld a,0
        add (ix+lemdatdir)              ;upper add, lower sub
        add (ix+lemdatfyp)
        ld b,a                          ;b=ypos
        ld a,c
        add (ix+lemdatfxp)              ;a=xpos
        sla c
        scf
        ret


;==============================================================================
;### FIELD ####################################################################
;==============================================================================

;### FLDMUP -> moves one field up
;### Output     lemdatmad, lemdatfxp, lemdatfyp updated, HL=map address
;### Destroyed  AF,BC
fldmup  ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        ld bc,-map_xlen*2
        add hl,bc
fldmup0 ld (ix+lemdatmad+0),l
        ld (ix+lemdatmad+1),h
        ld a,(ix+lemdatfyp)
        sub 12
        ld (ix+lemdatfyp),a
        ret

;### FLDMDW -> moves one field down
;### Output     lemdatmad, lemdatfxp, lemdatfyp updated, HL=map address
;### Destroyed  AF,BC
fldmdw  ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        ld bc,map_xlen*2
        add hl,bc
fldmdw0 ld (ix+lemdatmad+0),l
        ld (ix+lemdatmad+1),h
        ld a,(ix+lemdatfyp)
        add 12
        ld (ix+lemdatfyp),a
        ret

;### FLDMLF -> moves one field left
;### Output     lemdatmad, lemdatfxp, lemdatfyp updated, HL=map address
;### Destroyed  AF,BC
fldmlf  ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        dec hl:dec hl
fldmlf0 ld (ix+lemdatmad+0),l
        ld (ix+lemdatmad+1),h
fldmlf1 ld a,(ix+lemdatfxp)
        sub 12
        ld (ix+lemdatfxp),a
        ret

;### FLDMRG -> moves one field right
;### Output     lemdatmad, lemdatfxp, lemdatfyp updated, HL=map address
;### Destroyed  AF,BC
fldmrg  ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        inc hl:inc hl
fldmrg0 ld (ix+lemdatmad+0),l
        ld (ix+lemdatmad+1),h
fldmrg1 ld a,(ix+lemdatfxp)
        add 12
        ld (ix+lemdatfxp),a
        ret


;### FLDEVT -> check, if field event occures
;### Input      A=field type , HL=jumptable, IX=lemming data, IY=controls
;### Output     CF=1 event occured, HL=new type handler
;###            CF=0 no event, keep moving
;### Destroyed  AF,BC,DE,HL
fldevt  add a
        add l
        ld l,a
        ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        jp (hl)

;### FLDACT -> activates new move type
;### Input      HL=new type handler
;### Output     CF=1, HL=handler
;### Destroyed  -
fldact  ld (ix+lemdatjmp+0),l
        ld (ix+lemdatjmp+1),h
        scf
        ret

;### FLDCEN -> align lemming for centered placement after reaching field middle
;### Input      IX=lemming data, IY=controls
;### Destroyed  AF
fldcen  bit 0,(ix+lemdatdir)
        ld a,(ix+lemdatxps)
        jr z,fldcen1
        sub 4
        ld (ix+lemdatxps),a     ;set correct xpos
fldcen1 add 3
        ld (iy+16+6),a          ;set control xpos (field +3)
        ld (iy+00+2),64         ;no restore
        ret

;### FLDCHG -> changes field control with new type
;### Input      HL=field address in map, (HL)=new type
;### Output     E=control ID
;### Destroyed  AF,BC,D,HL
fldchg  ld d,(hl)
        call fldchg0
        ld h,0
        add hl,hl:add hl,hl:add hl,hl:add hl,hl
        ld bc,wingamdat_map+4
        add hl,bc
        ld c,e
        ex de,hl
        ld a,h
        add a
        ld hl,gmp_table
        ld l,a
        ld a,c
        ldi:ldi
        ld e,a
        ret
fldchg0 ld bc,map_xlen*2+mapmem
        or a
        sbc hl,bc
        srl h
        rr l
        ld a,l
        add wingamdat_map_id
        ld e,a
        ret

;### FLDUPD -> redraws field control
;### Input      E=control ID
;### Destroyed  AF,BC,DE,HL
fldupd  ld a,(wingam_id)
        push ix
        push iy
        call SyDesktop_WINDIN
        pop iy
        pop ix
        ret

;### FLDRDW -> redraw field
;### Input      HL=field address in map
;### Destroyed  AF,BC,DE,HL
fldrdw0 ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
fldrdw  call fldchg0
        jr fldupd


;*** nothing happens
midn
lown    or a
        ret

;### FIELD MIDDLE

;*** goal reached
midgol  ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        call keyent             ;CF=0 exit will be entered
        ccf
        ret nc
        call fldcen
        inc (iy+16+6)
        call fldrdw0
        ld (ix+lemdatsub),-1
        ld hl,256*1+SND_DOOR_GOAL
        call sndefx
        ld hl,lemgol
        jp fldact


;*** stopper
midstp  call fldcen
        call stpadd             ;add stopper
        ld a,(hl)
        cp idm_stop_mark
        ld c,idm_empt
        jr z,midstp1
        sub 2
        ld c,a
midstp1 ld (hl),c               ;remove stop sign
        call fldchg
        call fldupd
        ld hl,lemstp
        jp fldact


;*** build bridge right
midbbr  ld (ix+lemdatdir),0
        call fldmlf
        call brgbrg0
        jp fldact


;*** build bridge left
midbbl  ld (ix+lemdatdir),1
        call fldmrg
        call brgblf0
        jp fldact


;### FIELD BELOW

;*** mine trap
lowmin  ld hl,256*1+SND_DIE_EXPLODE
        call sndefx
        push ix
        push iy
        ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        push hl
        ld de,map_xlen*2
        add hl,de
        ld (hl),idm_wall        ;remove mine
        call fldchg
        call fldupd
        pop hl
        call lowmin1            ;search on field
        dec hl:dec hl
        call lowmin1            ;search left field
        ld bc,4
        add hl,bc
        call lowmin1            ;search right field
        pop iy
        pop ix
        scf
        ld hl,lemexp
        ret
;hl=mapadr -> search all lemmings on field and start exploding
lowmin1 push hl
        call figsrc             ;search next lemming
        pop hl
        ret nc                  ;no lemming -> this field is finished
        dec c
        jr nz,lowmin2
        push hl                 ;stopper -> remove it
        call stpdel
        pop hl
lowmin2 push hl
        call fldcen
        dec (iy+16+6)           ;set position
        ld a,(ix+lemdatyps)
        sub 4
        ld (ix+lemdatyps),a
        ld (ix+lemdatsub),-1
        ld hl,lemexp            ;activate mine explosion
        ld (ix+lemdatjmp+0),l
        ld (ix+lemdatjmp+1),h
        pop hl
        jr lowmin1


;*** digging down
lowdig  call fldrdw0
        ld c,(ix+lemdatxps)     ;adjust position
        call fldcen
        ld (ix+lemdatxps),c
        dec (iy+16+6)
lowdig0 ld a,(iy+16+8)
        add 2
        ld (iy+16+8),a
        ld a,(ix+lemdatsub)     ;save sub
        ld (ix+lemdatrbm),a
        ld l,(ix+lemdatmad+0)   ;remove dig-mark
        ld h,(ix+lemdatmad+1)
        ld bc,map_xlen*2
        add hl,bc
        ld a,(hl)
        call lemmrgg
        ld hl,lemddw            ;activate digger
        jp fldact


;*** drowning in water
lowdro  ld hl,256*1+SND_DIE_WATER
        call sndefx
        call fldcen
        call fldrdw0
        ld hl,lemdro
lowdro1 ld (ix+lemdatsub),-1
        jp fldact


;*** electric trap
lowele  ld hl,256*1+SND_DIE_SCREAM
        call sndefx
        call fldcen
        call fldrdw0
        ld hl,lemsma
        jr lowdro1


;==============================================================================
;### STOPPER ##################################################################
;==============================================================================

stptab  ds LEM_MAX*4    ;mapadr, lemadr
stpnum  db 0

;### STPADD -> add new stopper
;### Input      IX=lemming data record
;### Output     HL=map address
;### Destroyed  AF,BC,DE
stpadd  ld hl,stpnum
        ld a,(hl)
        inc (hl)
        add a:add a
        ld c,a
        ld b,0
        ld hl,stptab
        add hl,bc
        ld e,(ix+lemdatmad+0)
        ld d,(ix+lemdatmad+1)
        ld (hl),e:inc hl
        ld (hl),d:inc hl
        ld a,ixl:ld (hl),a:inc hl
        ld a,ixh:ld (hl),a
        ex de,hl
        ret

;### STPCHK -> check, if stopper on field
;### Input      HL=map address
;### Output     CF=1 stopper, DE=lemming data record
;### Destroyed  AF,BC,HL
stpchk  ld a,(stpnum)
        inc a
        ld c,l
        ld b,h
        ld hl,stptab-2
stpchk1 or a
        dec a
        ret z
        inc hl:inc hl
        ld e,(hl):inc hl
        ld d,(hl):inc hl
        ex de,hl
        or a
        sbc hl,bc
        ex de,hl
        jr nz,stpchk1
        ld e,(hl):inc hl
        ld d,(hl)
        scf
        ret

;### STPDEL -> removes stopper
;### Input      HL=map address
;### Destroyed  AF,BC,DE,HL
stpdel  call stpchk
        dec a
        jr z,stpdel1    ;last one -> finished
        ld e,l
        ld d,h
        inc hl
        dec de:dec de:dec de
        add a:add a
        ld c,a
        ld b,0
        ldir            ;move remaining stopper records to deleted one
stpdel1 ld hl,stpnum
        dec (hl)
        ret


;==============================================================================
;### DOORS ####################################################################
;==============================================================================

;### DORRGT -> lemming entered right door
;### Input      HL=door field in map
;### Output     (ix+lemdatmad)=new map address, (iy+lemdatyps),(iy+controls ypos)=new ypos
;###            ZF=1 -> start bridge right up building
dorrgt  push hl
        dec hl:dec hl
        call fldrdw
        pop hl
        ld a,(hl)
        cp idm_door_ru
        ld bc,map_xlen*2            ;right upper -> go down
        ld de,#0c00+idm_door_rl     ;and search for right lower
        jr z,dorrgt1
        ld bc,-map_xlen*2           ;right lower -> go up
        ld de,#f400+idm_door_ru     ;and search for right upper
dorrgt1 ld a,(ix+lemdatfyp)
        add d
        ld (ix+lemdatfyp),a
        add hl,bc           ;map up/down
        ld a,e
        cp (hl)
        jr nz,dorrgt1
        ld a,(ix+lemdatfyp)
        add 4
        ld (ix+lemdatyps),a
        dec hl:dec hl
        ld c,idm_brdg_lmrk
dorrgt2 ld (ix+lemdatmad+0),l       ;store new map address
        ld (ix+lemdatmad+1),h
        ld a,(ix+lemdatyps)         ;set new ypos for bgr-rst and lemming
        ld (iy+00+8),a
        ld (iy+16+8),a
        ld a,(hl)
        cp c
        ret

;### DORLFT -> lemming entered left door
;### Input      HL=door field in map
;### Output     (ix+lemdatmad)=new map address, (iy+lemdatyps),(iy+controls ypos)=new ypos
;###            ZF=1 -> start bridge right up building
dorlft  push hl
        inc hl:inc hl
        call fldrdw
        pop hl
        ld a,(hl)
        cp idm_door_lu
        ld bc,map_xlen*2            ;left upper -> go down
        ld de,#0c00+idm_door_ll     ;and search for left lower
        jr z,dorlft1
        ld bc,-map_xlen*2           ;left lower -> go up
        ld de,#f400+idm_door_lu     ;and search for left upper
dorlft1 ld a,(ix+lemdatfyp)
        add d
        ld (ix+lemdatfyp),a
        add hl,bc           ;map up/down
        ld a,e
        cp (hl)
        jr nz,dorlft1
        ld a,(ix+lemdatfyp)
        add 4
        ld (ix+lemdatyps),a
        inc hl:inc hl
        ld (ix+lemdatsub),0
        call fldmrg1
        ld c,idm_brdg_rmrk
        jr dorrgt2


;==============================================================================
;### BACKGROUND RESTORE #######################################################
;==============================================================================

;### RSTYOF -> gets yofs in field and field address in map
;### Input      HL=figure mad, A=new ypos
;### Output     HL=field  mad, A=yofs
rstyof  sub (ix+lemdatfyp)
        jr nc,rstyof1
        add 12
        ld bc,-map_xlen*2
        add hl,bc
        ret
rstyof1 cp 12
        ret c
        sub 12
        ld bc,map_xlen*2
        add hl,bc
        ret

;### RSTDWN -> calculates restore row after downwards moving
;### Output     (IY+00+4/6/8),bitmap prepared
rstdwn  ld a,(ix+lemdatyps)
        dec a
        jr rstupw0

;### RSTUPW -> calculates restore row after upwards moving
;### Output     (IY+00+4/6/8),bitmap prepared
rstupw  ld a,(ix+lemdatyps)
        add 8
rstupw0 ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        ld (iy+00+8),a          ;set ypos
        call rstyof
        ld c,a                  ;c=yoffset in field (0-11), hl=map address
        push hl
        ld l,(ix+lemdatrch+0)
        ld h,(ix+lemdatrch+1)
        ld (iy+00+4),l
        ld (iy+00+5),h          ;set control
        ld de,10
        add hl,de
        ld b,d
        ld (rstupw3+1),hl       ;store control bitmap
        ld a,(ix+lemdatfxp)
        ld (iy+00+6),a          ;set xpos
        pop hl
        ld e,(hl)               ;e=first field
        inc hl:inc hl
        ld d,(hl)               ;d=second field
        push de
        ld a,c
        add a
        add c
rstupw2 nop                     ;msx = add a
        add 10
        ld c,a                  ;bc=offset in bitmap
        push bc
        ld a,e                  ;a=first field
rstupw3 ld de,0
        call rstupw4
        pop bc
        pop af                  ;a=second field

rstupw4 add a
        ld hl,gmp_table
        ld l,a
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        add hl,bc
        ldi:ldi:ldi
rstupw5 ret                     ;msx = #ed -> #ed,#a0=ldi
        db #a0
        ldi:ldi
        ret

;### RSTLFT -> builds restore column after left movement
;### Output     (HL)=figure width-1, (IY+00+4/6/8),bitmap prepared
rstlft  ld a,(hl)
        inc a
        add (iy+16+6)
        jr rstrgt0

;### RSTRGT -> builds restore column after right movement
;### Output     (IY+00+4/6/8),bitmap prepared
rstrgt  ld a,(iy+16+6)
        dec a
rstrgt0 ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        ld (iy+00+6),a          ;set xpos
        sub (ix+lemdatfxp)      ;a = rst xpos - fld xpos = xoffset in field
        jr nc,rstrgt1
        add 12
        dec hl:dec hl
        jr rstrgt2
rstrgt1 cp 12
        jr c,rstrgt2
        sub 12
        inc hl:inc hl
rstrgt2 ld (rstclm+1),a         ;a=xoffset in field (0-11), hl=map address
        ld a,(ix+lemdatyps)
        ld (iy+00+8),a          ;set ypos
        call rstyof             ;a=yoffset in field (0-11), hl=map address
        ld c,(hl)               ;c=first field
        ld de,map_xlen*2
        add hl,de
        ld b,(hl)               ;b=second field
        ld e,(ix+lemdatrcv+0)
        ld d,(ix+lemdatrcv+1)   ;de=destination bitmap
        ld hl,-10
        add hl,de
        ld (iy+00+4),l          ;set v-restore control
        ld (iy+00+5),h
        push bc
        ld b,a                      ;*** first field, b=yofs
        add a
        add b                   ;a=yofs*3
rstrgt3 nop                     ;msx = add a
        add 10                  ;skip header
        ld l,a                  ;l=offset in bitmap
        ld a,12
        sub b
        cp 8+1
        jr c,rstrgt4
        ld a,8
rstrgt4 ld (rstrgt5+1),a
        ld iyl,a                ;iyl=height
        ld a,c                  ;a=first field
        ld c,l
        ld b,0                  ;bc=offset in bitmap
        add a
        ld hl,gmp_table
        ld l,a
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        add hl,bc               ;hl=source bitmap
        ld (rstclm+3),hl
        call rstclm
        pop bc                      ;*** b=second field
        ld a,8
rstrgt5 sub 0
        ret z                   ;already 8pixels in column -> finished
        ld iyl,a                ;iyl=height
        ld a,b
        add a
        ld hl,gmp_table
        ld l,a
        ld a,(hl)
        inc hl
        ld h,(hl)
        add 10                  ;skip header
        ld l,a
        jr nc,rstrgt6
        inc h                   ;hl=source bitmap
rstrgt6 ld (rstclm+3),hl
        jr rstclm

;### RSTCLM -> copy a 1pixel column from a field bitmap
;### Input      (rstclm+1)=column (0-11), (rstclm+3)=bitmap (12 pixel width), IYL=height, DE=destination bitmap (1 byte width) ##!!## 2byte min spec?
;### Destroyed  AF,BC,DE,HL,IYL
rstclm  ld c,0
        ld hl,0
rstclm0 jp rscclm           ;msx patch = rsmclm

;cpc 4colour encoding
rscclm  ld b,c          ;1
        ld a,c          ;1
        and 3           ;2
        cpl             ;1
        add 3+1         ;2
        ld (rscclm2+1),a ;4
        ld a,b          ;1
        rrca            ;1
        rrca            ;1
        and 3           ;2
        ld c,a          ;1
        ld b,0          ;2
        add hl,bc       ;3
        ld c,3          ;2 24

rscclm1 ld a,(hl)       ;2
rscclm2 jr rscclm3      ;3
        rlca:rlca:rlca  ;0-3 -> 1,5
rscclm3 ld (de),a       ;2
        inc de          ;2
        add hl,bc       ;3
        dec iyl         ;2
        jr nz,rscclm1   ;3 18,5 -> 8*18,5 = 148+24 = 172 = ~3rl
        ret

;msx 16colour encoding
rsmclm  ld a,c          ;1
        srl c           ;2
        ld b,0          ;2
        add hl,bc       ;3
        ld c,6          ;2
        rra             ;1
        jr nc,rsmclm2   ;2,5 13,5

rsmclm1 ld a,(hl)       ;2
        rrca:rrca:rrca:rrca ;4/0->2
        ld (de),a       ;2
        inc de:inc de   ;4
        add hl,bc       ;3
        dec iyl         ;2
        jr nz,rsmclm1   ;3 18 -> 8*18 = 144+13,5 = 157,5 = ~2,5rl
        ret
rsmclm2 ld a,(hl)
        ld (de),a
        inc de:inc de
        add hl,bc
        dec iyl
        jr nz,rsmclm2
        ret


;==============================================================================
;### ANIMATED ELEMENTS ########################################################
;==============================================================================

anidatmax   equ 0   ;number of animations
anidattab   equ 1   ;animation tab
anidatcnt   equ 3   ;current animation
anidatctr   equ 4   ;control address

anidatlen   equ 6

anidatnum   db 0
anidatmem
ds 4:dw 0*16+wingamdat_ani
ds 4:dw 1*16+wingamdat_ani
ds 4:dw 2*16+wingamdat_ani
ds 4:dw 3*16+wingamdat_ani
ds 4:dw 4*16+wingamdat_ani
ds 4:dw 5*16+wingamdat_ani
ds 4:dw 6*16+wingamdat_ani
ds 4:dw 7*16+wingamdat_ani

anitabwat   dw gmp_watr1,gmp_watr2,gmp_watr3
anitabele   dw gmp_trap_ele1,gmp_trap_ele1,gmp_trap_ele2,gmp_trap_ele1,gmp_trap_ele2,gmp_trap_ele3,gmp_trap_ele2
anitabghs   dw gmp_trap_ghs1,gmp_trap_ghs2,gmp_trap_ghs3,gmp_trap_ghs3,gmp_trap_ghs2,gmp_trap_ghs1,gmp_trap_ghs1

anichknum   equ 3
anichktab
db idm_watr1    ,3:dw anitabwat
db idm_trap_ele1,7:dw anitabele
db idm_trap_ghs1,7:dw anitabghs


;### ANIINI -> resets animations
aniini  xor a
        ld (anidatnum),a
        ld b,ANI_MAX
        ld hl,wingamdat_ani+2
        ld de,16
aniini1 ld (hl),64
        add hl,de
        djnz aniini1
        ret

;### ANICHK -> checks, if element is animated and adds it to the list
;### Input      A=element ID, (iy+6)=xpos, (iy+8)=ypos
;### Destroyed  AF,BC,HL
anichk  ld hl,anichktab
        ld b,anichknum
anichk1 cp (hl)
        inc hl
        jr z,anichk2
        inc hl:inc hl:inc hl
        djnz anichk1
        ret
anichk2 push de
        push ix
        push hl
        ld hl,anidatnum
        inc (hl)
        ld l,(hl)
        ld h,anidatlen
        call clcmu8
        ld de,anidatmem-anidatlen
        add hl,de
        ex de,hl
        pop hl
        ldi:ldi:ldi
        xor a
        ld (de),a
        dec hl
        ld b,(hl)
        dec hl
        ld c,(hl)                   ;bc=anitab
        inc de:ld a,(de):ld ixl,a
        inc de:ld a,(de):ld ixh,a
        ld (ix+2),10                ;activate control
        ld a,(iy+6):ld (ix+6),a     ;set x
        ld a,(iy+8):ld (ix+8),a     ;set y
        ld a,(bc):ld (ix+4),a:inc bc
        ld a,(bc):ld (ix+5),a       ;set first bitmap
        pop ix
        pop de
        ret

;### ANILOP -> does one animation loop
anilopf db 0

anilop  ld hl,anilopf
        inc (hl)
        bit 0,(hl)
        ret z
        ld a,(anidatnum)
        or a
        ret z
        ld iyl,a
        ld ix,anidatmem
        ld de,wingamdat_ani+4
anilop1 ld a,(ix+anidatcnt)
        inc a
        cp (ix+anidatmax)
        jr c,anilop2
        xor a
anilop2 ld (ix+anidatcnt),a
        add a
        ld l,(ix+anidattab+0)
        ld h,(ix+anidattab+1)
        ld c,a
        ld b,0
        add hl,bc
        ldi:ldi
        ex de,hl
        ld bc,16-2
        add hl,bc
        ex de,hl
        ld c,anidatlen
        add ix,bc
        dec iyl
        jr nz,anilop1
        ld de,256*wingamdat_ani_id+256-ANI_MAX
        ld a,(wingam_id)
        jp SyDesktop_WINDIN


;==============================================================================
;### ACTIVE TRAPS #############################################################
;==============================================================================

idt_rcks    equ 0
idt_rckh    equ 4
idt_firl    equ 8
idt_firr    equ 12
idt_firu    equ 16
idt_elec    equ 20
idt_saws    equ 24
idt_sawb    equ 28
idt_ghst    equ 32

trpdattab
dw trprksi,trprks
dw trprkhi,trprkh
dw trpfili,trpfil
dw trpfiri,trpfir
dw trpfiui,trpfiu
dw trpelei,trpele
dw trpswsi,trpsws
dw trpswbi,trpswb
dw trpghsi,trpghs

trpdatini   equ 0   ;1W init routine
trpdatjmp   equ 2   ;1W handler routine
trpdatmad   equ 4   ;1W map address of base/start field
trpdatxps   equ 6   ;1B total xpos of field
trpdatyps   equ 7   ;1B total ypos of field
trpdatdat   equ 8   ;6B storage for internal data (first byte=0 on init)

trpdatlen   equ 14

trpdatnum   db 0    ;number of traps
trpdatcnt   db 0    ;trap counter
trpdatadr   dw 0    ;current data record
trpdattim   db 0    ;time counter for one full trap pass (256/50*4 = 20 seconds min until traps repeat)

trpdatmem   ds trpdatlen*TRP_MAX

trpctrnum   db 0    ;number of used controls for actual trap


;### TRPINI -> inits trap management
trpini  xor a
        ld (trpdatnum),a
        ld iy,wingamdat_trp
        ld c,64
        call trpctr4
        ld hl,trpdatmem
        ld (trpdatadr),hl
        ld a,#c9
trpini1 ld (trplop),a
        ret

;### TRPSTA -> starts traps
trpsta  ld a,(trpdatnum)        ;check, if existing traps
        or a
        ret z
        call trpnxt             ;prepare next trap loop
        xor a
        jr trpini1              ;activate traps

;### TRPLOP -> does a trap loop
trplop  ret
trplop0 jr trplop0

trpprc  ld hl,trpdattim             ;*** proceed
        inc (hl)
        jr nz,trpprc1
        dec (hl)
trpprc1 ld ix,(trpdatadr)
        ld l,(ix+2)
        ld h,(ix+3)
        ld iy,wingamdat_trp
        jp (hl)                 ;call proceed handler

trpend  ld bc,trpdatlen             ;*** end
        add ix,bc
        ld (trpdatadr),ix       ;next trap
        ld hl,trpdatcnt
        dec (hl)
        jr nz,trpnxt1
        ld a,trpdly-trplop0-2   ;set delay handler
        ld (trplop0+1),a

trpdly  ld hl,trpdattim             ;*** delay
        inc (hl)
        ret nz

trpnxt  ld ix,trpdatmem             ;*** new trap cycle
        ld (trpdatadr),ix       ;reset variables
        ld a,(trpdatnum)
        ld (trpdatcnt),a
        xor a
        ld (trpdattim),a
        ld a,trpprc-trplop0-2   ;set proceed handler
        ld (trplop0+1),a
trpnxt1 ld l,(ix+0)
        ld h,(ix+1)
        ld iy,wingamdat_trp
        jp (hl)                 ;call init handler

;### TRPADD -> adds a new trap
;### Input      IY-2=map address, C=type, (maplodx)=xpos, (maplody)=ypos
;### Destroyed  AF,DE,HL
trpadd  push bc
        ld b,0
        ld hl,trpdattab
        add hl,bc
        ld de,(trpdatadr)
        ld c,4
        ldir                            ;store init and handler
        ex de,hl
        push iy:pop de
        dec de:dec de
        ld (hl),e:inc hl
        ld (hl),d:inc hl
        ld a,(maplodx):ld (hl),a:inc hl
        ld a,(maplody):ld (hl),a:inc hl
        ld (hl),b
        ld c,6
        add hl,bc
        ld (trpdatadr),hl
        ld hl,trpdatnum
        inc (hl)
        pop bc
        ret

;### TRPCTRx -> show/hide trap controls
;### Input      IY=control data record, A=number of controls, C=show/hide (10/64)
trpctr4 ld (iy+48+2),c
trpctr3 ld (iy+32+2),c
trpctr2 ld (iy+16+2),c
trpctr1 ld (iy+00+2),c
        ld (trpctrnum),a
        ret

;### TRPCOL -> collision detection
;### Input      E,D=trap x/ypos, (trpcol2+1)=trap xlen-1/ylen-1, (trpcols+1)=collision sound
;### Destroyed  AF,BC,HL,IX
trpcol  ld ix,lemdatmem
        ld b,LEM_MAX
trpcol1 push bc
        bit 7,(ix+lemdatdir)
        jr nz,trpcol3
        ld c,(ix+lemdatjmp+0)
        ld b,(ix+lemdatjmp+1)
        ld hl,lemsma
        or a
        sbc hl,bc
        jr z,trpcol3            ;already smashing
        ld hl,lemexp
        or a
        sbc hl,bc
        jr z,trpcol3            ;already exploding
        ld l,(ix+lemdatctr+0)
        ld h,(ix+lemdatctr+1)
        ld bc,16+6
        add hl,bc
trpcol2 ld bc,0
        ld a,(hl)               ;lem xpos
        add 5
        sub e
        jr c,trpcol3    ;(lemxp + lemxs - 1) < trpxp -> no collision
        ld a,e
        add c
        cp (hl)
        jr c,trpcol3    ;(trpxp + trpxs - 1) < lemxp -> no collision
        inc hl:inc hl
        ld a,(hl)               ;lem ypos
        add 5
        sub d
        jr c,trpcol3    ;(lemyp + lemys - 1) < trpyp -> no collision
        ld a,d
        add b
        cp (hl)
        jr c,trpcol3    ;(trpyp + trpys - 1) < lemyp -> no collision

        ld c,(ix+lemdatjmp+0)       ;*** collision detected
        ld b,(ix+lemdatjmp+1)
        ld hl,lembbr            ;check, if lemming is bridge builder to right
        or a
        sbc hl,bc
        jr z,trpcol6
        ld hl,lembbl            ;check, if lemming is bridge builder to left
        or a
        sbc hl,bc
        jr z,trpcol7
        ld hl,lemstp            ;check, if lemming is stopper
        or a
        sbc hl,bc
        jr z,trpcol4
        ld hl,lemstp0
        or a
        sbc hl,bc
        jr nz,trpcol5
trpcol4 ld l,(ix+lemdatmad+0)   ;remove stopper
        ld h,(ix+lemdatmad+1)
        call stpdel
trpcol5 ld hl,lemsma
        ld (ix+lemdatjmp+0),l   ;start smashing
        ld (ix+lemdatjmp+1),h
        ld (ix+lemdatsub),-1

trpcols ld hl,256*1+SND_DIE_SCREAM
        call sndefx

trpcol3 ld bc,lemdatlen
        add ix,bc
        pop bc
        dec b
        jp nz,trpcol1
        ret
trpcol6 ld l,(ix+lemdatmad+0)   ;remove half bridge right up
        ld h,(ix+lemdatmad+1)
        inc hl:inc hl
trpcol8 ld (hl),idm_empt
        call fldchg
        call fldupd
        jr trpcol5
trpcol7 ld l,(ix+lemdatmad+0)   ;remove half bridge left up
        ld h,(ix+lemdatmad+1)
        dec hl:dec hl
        jr trpcol8


;*** INIT AND PROCEED *********************************************************

;init
;Input      IX=trap data record, IY=4 x control data record


;### TRPRKS -> rock soft
trprksi ld (ix+trpdatdat+3),0       ;no end phase (bit7), soft (bit0)

trprksj ld hl,256*1+SND_TRAP_ROCKBEG    ;*** init
        call sndefx
        ld a,(ix+trpdatxps)
              ld (iy+00+06),a
        add 2:ld (iy+16+06),a
        inc a
        ld (trprksc+1),a

        ld a,(ix+trpdatyps)
        add 6:ld (iy+00+08),a
        add 3:ld (iy+16+08),a       ;set xy for bgr and rock
        ld hl,gtr_rock_bg
        ld (iy+00+04),l
        ld (iy+00+05),h             ;set background restore bitmap
        ld hl,gtr_rock
        ld (iy+16+04),l
        ld (iy+16+05),h             ;set rock bitmap
        ld l,(ix+trpdatmad+0)
        ld h,(ix+trpdatmad+1)
        ld (ix+trpdatdat+0),l
        ld (ix+trpdatdat+1),h       ;store current mad for bgr
        ld c,6*6
        call trprks1                ;generate background
        ld (ix+trpdatdat+2),2       ;set bgr ypos counter (4..1)
        ld hl,256*5+6
        ld (trpcol2+1),hl           ;set trap size (7x6)
        ld a,2
        ld c,10
        jp trpctr2                  ;use 2 controls

trprks  bit 7,(ix+trpdatdat+3)          ;*** proceed
        jp nz,trprks8
        dec (ix+trpdatdat+2)
        jr nz,trprks2
        ld l,(ix+trpdatdat+0)       ;* background enters new field
        ld h,(ix+trpdatdat+1)
        ld de,map_xlen*2
        add hl,de
        ld (ix+trpdatdat+0),l
        ld (ix+trpdatdat+1),h       ;next mad
        ld (ix+trpdatdat+2),4       ;restart ypos counter
        ld c,0
        call trprks1                ;generate background for new field
        jr trprks3
trprks2 ld hl,(gtr_rock_bg+3)       ;* background is still same field
        ld de,3*6
        add hl,de                   ;increase bitmap pointer in background headxer
        ld (gtr_rock_bg+3),hl
trprks3 ld a,(iy+00+8):add 3:ld (iy+00+8),a
        ld a,(iy+16+8):add 3:ld (iy+16+8),a ;increase ypos
        ld d,a
trprksc ld e,0
        push ix
        ld a,SND_DIE_CRUSH
        ld (trpcols+1),a
        call trpcol                 ;collision detection
        pop ix
        ld a,(ix+trpdatdat+2)
        cp 2
        jr z,trprks4
        cp 4
        ret nz
        ld l,(ix+trpdatdat+0)       ;check, if bridge reached
        ld h,(ix+trpdatdat+1)
        ld a,(hl)
        cp idm_brdg_rf
        ret c
        cp idm_brdg_lf_x+1
        ret nc
        cp idm_brdg_rmrk
        ret z
        res 0,(ix+trpdatdat+3)      ;bridge -> turn into rock soft, don't break anything below
        jr trprks5
trprks4 ld l,(ix+trpdatdat+0)       ;check, if solid field reached
        ld h,(ix+trpdatdat+1)
        ld de,map_xlen*2
        add hl,de
        bit 6,(hl)
        ret nz
trprks5 dec (iy+16+8)                   ;*** start "crumble"
        ld hl,256*1+SND_TRAP_ROCKEND
        call sndefx
        ld a,(ix+trpdatdat+3)
        ld (ix+trpdatdat+2),a       ;store soft/hard type
        ld (ix+trpdatdat+3),-3      ;set end phase
        ld hl,gtr_rock_c1
trprks6 ld (iy+16+04),l
        ld (iy+16+05),h             ;set crash1 bitmap
        ret

trprks8 inc (ix+trpdatdat+3)            ;*** "crumble" animation
        jr z,trprks9
        ld (iy+00+2),64             ;stop background restore
        bit 0,(ix+trpdatdat+3)
        ld hl,gtr_rock_c2
        jr z,trprks6
        ld hl,gtr_rock_c3
        jr trprks6
trprks9 bit 0,(ix+trpdatdat+2)          ;*** end
        jr z,trprksb

        ld de,map_xlen*2
        ld l,(ix+trpdatdat+0)       ;rock hard
        ld h,(ix+trpdatdat+1)       ;test, if field below is breakable ground
        call trprksd
trprksb xor a                 
        ld c,64
        call trpctr2                ;hide 2 controls
        ld l,(ix+trpdatdat+0)
        ld h,(ix+trpdatdat+1)
        call fldrdw                 ;redraw field
        jp trpend

trprksd add hl,de
        ld a,(hl)
        cp idm_grqm_br3m+1
        ret nc
        inc (hl)                    ;break ground
        and 3
        jr nz,trprksa
        ld a,(hl)
        dec a
        ld (hl),idm_empt            ;remove ground
        push hl
        call keyopn
        pop hl
trprksa call fldchg
        jp fldupd

;hl=map address, c=offset (0/18/36/54) -> generates background restore
trprks1 ld a,(hl)
        ld hl,gmp_table
        add a
        ld l,a
        ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        inc hl:inc hl:inc hl
        ld e,(hl):inc hl
        ld d,(hl):inc hl
        ex de,hl
        ld b,0
        add hl,bc
        ld (gtr_rock_bg+3),hl
        ex de,hl
        ld de,gtr_rock_bg+5
        ldi:ldi:ldi:ldi
        ret


;### TRPRKH -> rock hard
trprkhi ld (ix+trpdatdat+3),1       ;no end phase, hard
        jp trprksj
trprkh  equ trprks


;### TRPFIL -> fire left
trpfilt dw gtr_fire_l1,gtr_fire_l2,gtr_fire_l3,gtr_fire_l4,gtr_fire_l3,gtr_fire_l2,gtr_fire_l3,gtr_fire_l4,gtr_fire_l4,gtr_fire_l3,gtr_fire_l2,gtr_fire_l3,gtr_fire_l4,gtr_fire_l3,gtr_fire_l2,gtr_fire_l1
trpfili ld bc,-256*12+0                 ;*** init
        ld hl,(trpfilt)
        jr trpfiu1
trpfil  ld de,-2                        ;*** proceed
        ld bc,trpfilt
        jp trpfiu2


;### TRPFIR -> fire right
trpfirt dw gtr_fire_r1,gtr_fire_r2,gtr_fire_r3,gtr_fire_r4,gtr_fire_r3,gtr_fire_r2,gtr_fire_r3,gtr_fire_r4,gtr_fire_r4,gtr_fire_r3,gtr_fire_r2,gtr_fire_r3,gtr_fire_r4,gtr_fire_r3,gtr_fire_r2,gtr_fire_r1
trpfiri ld bc,256*12+0                  ;*** init
        ld hl,(trpfirt)
        jr trpfiu1
trpfir  ld de,2                         ;*** proceed
        ld bc,trpfirt
        jr trpfiu2


;### TRPFIU -> fire up
trpfiut dw gtr_fire_u1,gtr_fire_u2,gtr_fire_u3,gtr_fire_u4,gtr_fire_u3,gtr_fire_u2,gtr_fire_u3,gtr_fire_u4,gtr_fire_u4,gtr_fire_u3,gtr_fire_u2,gtr_fire_u3,gtr_fire_u4,gtr_fire_u3,gtr_fire_u2,gtr_fire_u1  ;##!!## 256b aligned for optimisation

trpfiui ld bc,256*0+256-12              ;*** init
        ld hl,(trpfiut)
trpfiu1 
        push bc
        push hl
        ld hl,256*1+SND_TRAP_FIRE
        call sndefx
        pop hl
        pop bc

        ld a,(ix+trpdatxps)
        add b
        ld (iy+00+6),a
        inc a
        ld (trpfiu4+1),a
        ld a,(ix+trpdatyps)
        add c
        ld (iy+00+8),a
        inc a
        ld (trpfiu4+2),a
        ld (ix+trpdatdat+0),16      ;duration = 2,5secs
        ld (iy+00+4),l
        ld (iy+00+5),h
        ld hl,9*256+9
        ld (trpcol2+1),hl           ;set trap size (10x10)
        ld a,1
        ld c,10
        jp trpctr1                  ;use 1 control

trpfiu  ld de,-map_xlen*2               ;*** proceed
        ld bc,trpfiut
trpfiu2 dec (ix+trpdatdat+0)
        jr z,trpfiu3
        ld a,(ix+trpdatdat+0)
        and 15
        add a
        ld l,a
        ld h,0
        add hl,bc
        ld a,(hl):ld (iy+00+4),a:inc hl
        ld a,(hl):ld (iy+00+5),a
trpfiu4 ld de,0
        ld a,SND_DIE_SCREAM
        ld (trpcols+1),a
        jp trpcol                   ;collision detection

trpfiu3 xor a                           ;*** end
        ld c,64
        call trpctr1                    ;hide 1 control
        ld l,(ix+trpdatmad+0)
        ld h,(ix+trpdatmad+1)
        add hl,de:call fldrdw
        jp trpend


;### TRPELE -> electric
trpelet dw gtr_elec_l1,gtr_elec_r1,gtr_elec_u1,gtr_elec_d1
        dw gtr_elec_l2,gtr_elec_r2,gtr_elec_u2,gtr_elec_d2

trpelei ld hl,256*1+SND_TRAP_ELECTRIC
        call sndefx

        ld c,(ix+trpdatxps)             ;*** init
        ld b,(ix+trpdatyps)             ;c,b=x/y
        ld (ix+trpdatdat+0),2*50/4      ;2 seconds duration
        ld a,-12+4:add c:ld (iy+00+6),a ;xleft
        ld a, 12  :add c:ld (iy+16+6),a ;xright
        ld a, 6-2 :add b:ld (iy+00+8),a ;yleft/yright
                         ld (iy+16+8),a
        ld a, 6-2 :add c:ld (iy+32+6),a ;xup/xdown
                         ld (iy+48+6),a
        ld a,-12+4:add b:ld (iy+32+8),a ;yup
        ld a, 12  :add b:ld (iy+48+8),a ;ydown
        ld a,4
        ld c,10
        call trpctr4                    ;use 4 controls
        ld hl,trpelet+0
        jr trpele1                      ;set animation 0

trpele  dec (ix+trpdatdat+0)            ;*** proceed
        jr z,trpele4
        bit 0,(ix+trpdatdat+0)
        ld hl,trpelet+0
        jr z,trpele3
        ld hl,trpelet+8
trpele3 call trpele1        ;set animation 0/1
        ld hl,256*1+7
        ld (trpcol2+1),hl   ;collision detection
        ld e,(iy+00+6)
        ld d,(iy+00+8):inc d
        ld a,SND_DIE_SCREAM
        ld (trpcols+1),a
        call trpcol
        ld e,(iy+16+6)
        ld d,(iy+16+8):inc d
        call trpcol
        ld hl,256*7+1
        ld (trpcol2+1),hl
        ld e,(iy+32+6):inc e
        ld d,(iy+32+8)
        call trpcol
        ld e,(iy+48+6):inc e
        ld d,(iy+48+8)
        jp trpcol

trpele4 xor a                           ;*** end
        ld c,64
        call trpctr4        ;hide 4 controls
        ld l,(ix+trpdatmad+0)
        ld h,(ix+trpdatmad+1)
        push hl
        dec hl:                     call fldrdw
        pop hl:push hl
        inc hl:inc hl:              call fldrdw
        pop hl:push hl
        ld bc,-map_xlen*2:add hl,bc:call fldrdw
        pop hl
        ld bc,map_xlen*2:add hl,bc: call fldrdw
        jp trpend

trpele1 ld a,4              ;hl=bitmap tab
        ld de,wingamdat_trp+4
        ld bc,14
trpele2 ldi:ldi
        inc c:inc c
        ex de,hl
        add hl,bc
        ex de,hl
        dec a
        jr nz,trpele2
        ret


;### TRPSWS -> saw small
trpswsd equ 35

trpswsi ld hl,gtr_saw_s                 ;*** init
        ld (iy+00+4),l
        ld (iy+00+5),h              ;set bitmap
        ld a,(ix+trpdatxps):add  2:ld (iy+00+6),a:inc a:ld (trpsws7+1),a
        ld a,(ix+trpdatyps):add 12:ld (iy+00+8),a:      ld (trpsws7+2),a
        ld (ix+trpdatdat+0),trpswsd ;step counter (d..d-10 move out, d-11..11 stay out, 10..0 move in)
        ld (ix+trpdatdat+1),1       ;animation counter (3..1)
        ld a,6
        ld (trpcol2+1),a
        ld a,1
        ld c,10
        call trpctr1                ;use 1 control
        ld bc,256*10+0
        ld a,11
        jr trpsws1                  ;set first animation

trpsws  ld a,(ix+trpdatdat+1)           ;*** proceed
        dec a
        jr nz,trpsws3
        ld a,3
trpsws3 ld (ix+trpdatdat+1),a
        dec a
        ld c,a                      ;c=animation
        dec (ix+trpdatdat+0)
        ld de,map_xlen*2
        jp z,trpfiu3                ;end -> redraw lower field
        ld a,(ix+trpdatdat+0)       ;a=step (29..1)
        cp 11
        jr c,trpsws6
        sub trpswsd-10              ;a=9..0 or neg
        ld b,0
        jr c,trpsws4                    ;** stay out
        ld b,a                          ;** move out
        cp 5                        ;b=9..0
        jr c,trpsws4

        push bc
        ld hl,256*1+SND_TRAP_SAW
        call z,sndefx
        pop bc

        ld c,0                      ;no spin during first 5 steps
trpsws4 ld a,11
trpsws5 push af
        call trpsws1
        pop af
        ret nc
trpsws7 ld de,0
        ld a,SND_DIE_SCREAM
        ld (trpcols+1),a
        jp trpcol                   ;collision check
trpsws6 cpl                             ;** move in
        add 12                      ;a=10..1 -> -11..-2 -> 1..10
        ld b,a
        cp 6
        ld a,12
        jr c,trpsws5
        ld c,0                      ;no spin during last 5 steps
        jr trpsws5

;c=animation (0..2), b=yofs (0-10), a=11(move down, stay)/12(move up) -> set bitmap header for spin/movement animation
trpsws1 dec c
        ld hl,gtr_saw_s+10+72
        jr z,trpsws2
        dec c
        ld hl,gtr_saw_s+10+144
        jr z,trpsws2
        ld hl,gtr_saw_s+10+0
trpsws2 sub b                   ;a=1..11/2..12
        ld (gtr_saw_s+2),a      ;set ysize
        sub 4
        ld (trpcol2+2),a
        ld a,b
        add a:add b:add a
        ld c,a
        ld b,0                  ;bc=yofs*6
        add hl,bc
        ld (gtr_saw_s+3),hl     ;set bitmap data start
        ret


;### TRPSWB -> saw big
trpswbd equ 42
trpswbt dw gtr_sbc_1,gtr_sbl_1,gtr_sbr_1
        dw gtr_sbc_2,gtr_sbl_2,gtr_sbr_2
        dw gtr_sbc_3,gtr_sbl_3,gtr_sbr_3

trpswbi ld a,(ix+trpdatxps)             ;*** init
               ld (iy+00+6),a
        sub  6:ld (iy+16+6),a
        add 18:ld (iy+32+6),a
        ld a,(ix+trpdatyps)
        add 12:ld (iy+00+8),a
        inc a :ld (iy+16+8),a
               ld (iy+32+8),a
        ld (ix+trpdatdat+0),trpswbd ;step counter (41..31 move out, 30 expand, 29..13 spin, 12 fold, 11..1 move in)
        ld (ix+trpdatdat+1),1       ;animation counter (3..1)
        ld a,1
        ld c,10
        call trpctr1                ;first use 1 control
        ld b,10
        ld a,11
        jr trpswb1                  ;set first animation

trpswb  dec (ix+trpdatdat+0)            ;*** proceed
        ld de,map_xlen*2
        jp z,trpfiu3                ;end -> redraw lower field
        ld a,(ix+trpdatdat+0)       ;a=step (37..1)
        cp 12
        jr c,trpswb2                ;move in (11..1)
        jr z,trpswb3                ;fold (12)
        sub trpswbd-12
        jr z,trpswb4                ;expand (26)
        jp c,trpswb5                ;spin (25..12)
        dec a                           ;*** move out (37..27 -> 11..1)
        ld b,a                      ;10..0
        ld a,11
        jr trpswb1
trpswb2 cpl                             ;*** move in (-12..-2)
        add 12                      ;0..10
        ld b,a
        jr nz,trpswba
        ld (iy+32+2),64             ;use 1 control
        ld (iy+16+2),64
        ld a,1
        ld (trpctrnum),a
        push bc
        ld l,(ix+trpdatmad+0)
        ld h,(ix+trpdatmad+1)
        ld de,map_xlen*2-2
        add hl,de
        push hl
        call fldrdw
        pop hl
        ld de,4
        add hl,de
        call fldrdw
        pop bc
trpswba ld a,12
;b=yofs (0..10), a=11(move down)/12(move up) -> set bitmap header for up/down movement animation
trpswb1 bit 0,b
        ld hl,gtr_sbc_f1
        jr z,trpswbb
        ld hl,gtr_sbc_f2
trpswbb ld (iy+00+4),l              ;set bitmap
        ld (iy+00+5),h
        sub b                       ;a=1..11/2..12
        inc hl:inc hl
        ld (hl),a                   ;set ysize
        inc hl
        ld e,l
        ld d,h                      ;(de)=bitmap pointer
        ld a,b
        add a:add b:add a
        add 10-3
        ld c,a
        ld b,0                      ;bc=yofs*6
        add hl,bc
        ex de,hl                    ;de=bitmap address, (hl)=bitmap pointer
        ld (hl),e                   ;set bitmap address
        inc hl
        ld (hl),d
        ret
trpswb4 ld a,3                          ;*** expand
        ld c,10
        call trpctr3                ;use 3 controls

        ld hl,256*1+SND_TRAP_SAW
        call z,sndefx

        jr trpswb6
trpswb3 inc (iy+16+06)                  ;*** fold
        ld l,(ix+trpdatmad+0)
        ld h,(ix+trpdatmad+1)
        push hl
        ld de,map_xlen*2-2
        call trprksd                ;break ground left
        pop hl
        ld de,map_xlen*2+2
        call trprksd                ;break ground right
trpswb6 ld bc,gtr_sbc_e
        ld de,gtr_sbl_e
        ld hl,gtr_sbr_e
trpswb7 ld (iy+00+4),c
        ld (iy+00+5),b
        ld (iy+16+4),e
        ld (iy+16+5),d
        ld (iy+32+4),l
        ld (iy+32+5),h
        ret
trpswb5 inc a                           ;*** spin
        jr nz,trpswb9
        dec (iy+16+06)
trpswb9 ld a,(ix+trpdatxps)
        sub 12
        ld e,a
        ld a,(ix+trpdatyps)
        add 12
        ld d,a
        ld hl,256*3+35              ;4/36
        ld (trpcol2+1),hl
        push ix
        ld a,SND_DIE_CRUSH
        ld (trpcols+1),a
        call trpcol                 ;collision detection upper stripe
        pop ix
        ld a,(ix+trpdatyps)
        add 12+4
        ld d,a
        ld a,(ix+trpdatxps)
        sub 2
        ld e,a
        ld hl,256*7+15              ;8/16
        ld (trpcol2+1),hl
        push ix
        call trpcol                 ;collision detection wider main saw (don't touch blocker/digger)
        pop ix
        ld a,(ix+trpdatdat+1)
        dec a
        jr nz,trpswb8
        ld a,3
trpswb8 ld (ix+trpdatdat+1),a
        add a
        ld c,a
        add a
        add c
        ld c,a
        ld b,0
        ld hl,trpswbt-6
        add hl,bc
        ld c,(hl):inc hl
        ld b,(hl):inc hl
        ld e,(hl):inc hl
        ld d,(hl):inc hl
        ld a,(hl):inc hl
        ld h,(hl)
        ld l,a
        jr trpswb7
        

;### TRPGHS -> ghosts
trpghst dw gtr_ghost1,gtr_ghost2,gtr_ghost3,gtr_ghost3,gtr_ghost2,gtr_ghost1,gtr_ghost2,gtr_ghost3,gtr_ghost3,gtr_ghost2,gtr_ghost1,gtr_ghost2,gtr_ghost3,gtr_ghost3,gtr_ghost2
trpghsu dw gtr_ghost4,gtr_ghost5,gtr_ghost5,gtr_ghost5,gtr_ghost5,gtr_ghost5,gtr_ghost5

trpghsi ld a,(ix+trpdatdat+0)           ;*** init
        or a
        jr z,trpghs3                ;first run -> start with next phase
        ld (iy+00+6),a
        ld (trpghs1+1),a
        ld a,(ix+trpdatdat+1)
        ld (iy+00+8),a
        ld (trpghs1+2),a
        ld hl,gtr_ghost1
        ld (iy+00+4),l
        ld (iy+00+5),h
        ld (ix+trpdatdat+4),32      ;duration = 2,5secs dead + 1,2sec nothing + 1,2sec next

        ld hl,256*1+SND_TRAP_GHOST
        call sndefx

        ld hl,8*256+4
        ld (trpcol2+1),hl
        ld a,1
        ld c,10
        jp trpctr1

trpghs  dec (ix+trpdatdat+4)            ;*** proceed
        jr nz,trpghs5
        call trpghs2
        jp trpend
trpghs5 ld a,(ix+trpdatdat+4)
        cp 8
        jr z,trpghs3                ;* start next phase
        jr c,trpghs6
        sub 16
        jr z,trpghs2                ;* start hide phase
        ret c
        ld bc,trpghst-2             ;* do death phase
        call trpghs7
trpghs1 ld de,0
        ld a,SND_DIE_CRUSH
        ld (trpcols+1),a
        jp trpcol                   ;collision detection
trpghs6 ld bc,trpghsu-2             ;* do next phase
trpghs7 add a        
        ld l,a
        ld h,0
        add hl,bc
        ld a,(hl):ld (iy+00+4),a:inc hl
        ld a,(hl):ld (iy+00+5),a
        ret

trpghs2 xor a                       ;* hide ghost
        ld c,64
        call trpctr1                ;hide 1 control
        ld l,(ix+trpdatdat+2)
        ld h,(ix+trpdatdat+3)
        jp fldrdw

trpghs3 call clcrnd                     ;*** choose and calculate new field
        ld h,0
        ld c,l:ld b,h
        add hl,hl
        add hl,bc
        add hl,hl               ;h=0..5
        xor a
        ld l,h
        ld h,a
        bit 0,l
        ld bc,-map_xlen*4-2
        jr z,trpghs4
        ld a,12
        ld bc,-map_xlen*2-2
trpghs4 add (ix+trpdatyps)
        add 2-24
        ld (ix+trpdatdat+1),a   ;store ypos
        inc a
        inc a
        ld (iy+00+8),a
        res 0,l
        ld a,l                  ;a=0/2/4
        add hl,bc
        ld c,(ix+trpdatmad+0)
        ld b,(ix+trpdatmad+1)
        add hl,bc
        ld (ix+trpdatdat+2),l
        ld (ix+trpdatdat+3),h   ;store mad
        ld l,a
        add a:add l:add a       ;a=0/12/24
        add (ix+trpdatxps)
        add 4-12
        ld (ix+trpdatdat+0),a   ;store xpos
        ld (iy+00+6),a
        ld hl,gtr_ghost4
        ld (iy+00+4),l
        ld (iy+00+5),h          ;set "preview" ghost
        ld (ix+trpdatdat+4),8   ;start next phase
        ld a,1
        ld c,10
        jp trpctr1              ;show 1 control


;==============================================================================
;### DISPLAY ##################################################################
;==============================================================================

;### DSPMSG -> shows new message
;### Input      HL=text, B=duration in 1/50s, C=0/20 (3/2 blinks)
;### Destroyed  AF,BC,DE,HL
dspmsg  nop
        ld (ui_msgctr),hl
        ld a,b
        ld (dspupd2+1),a
        ld a,c
        ld (dspupd1+1),a
        xor a
        ld (dspupd0),a
        ld c,16*8+1
        push ix
        push iy
        call dspupd4
        pop iy
        pop ix
        ret

;### DSPUPD -> updates displays
dspupd  ld hl,dspupd7
dspupd0 ret                     ;message
dspupd1 ld a,0
        inc a
        ld (dspupd1+1),a
dspupd2 cp 100
        jr z,dspupd6
        ld c,16*2+1
        cp  10:jr z,dspupd4
        cp  30:jr z,dspupd4
        ld c,16*8+1
        cp  20:jr z,dspupd4
        cp  40:ret nz
dspupd4 ld a,c
        ld (ui_msgctr+2),a
        ld e,wingamdat_msg_id
dspupd5 ld a,(wingam_id)
        jp SyDesktop_WINDIN
dspupd6 ld hl,txtzero
        ld (ui_msgctr),hl
        ld a,#c9
        ld (dspupd0),a
        ld e,wingamdat_msg_id-1
        jr dspupd5
dspupd7 ld a,0                  ;speed
        inc a
        ld (dspupd7+1),a
        and 16
        ld hl,(xid_speedr*2+bmpdato-2)
        jr z,dspupd9
dspupd8 ld hl,(xid_speed*2+bmpdato-2)
dspupd9 ld (wingamdat_spd+4),hl
        ld e,wingamdat_spd_id
        ld a,(wingam_id)
        jp SyDesktop_WINDIN



;==============================================================================
;### PROGRESS BAR #############################################################
;==============================================================================

lemracmax   equ 0   ;total number of lemmings
lemracmin   equ 1   ;minimum to be saved
lemracsav   equ 2   ;number of saved lemmings
lemracded   equ 3   ;number of dead lemmings
lemracpct   equ 4   ;progress bar controls
lemracpbm   equ 6   ;progress bar bitmap
lemracpfi   equ 8   ;progress bar first field
lemracpci   equ 9   ;progress bar control ID
lemracall   equ 10  ;flag, if all saved (1=yes)
lemraccnt   equ 11  ;number of undropped lemmings

lemraclen   equ 16

lemracnum   db 1

lemracmem   db 0,0,0,0
            dw wingamdat_prgrs1+32
            dw ui_progress1+10
            db 0
            db wingamdat_prgrs1_id
            db 0,0
            ds 4

            db 0,0,0,0
            dw wingamdat_prgrs2+32
            dw ui_progress2+10
            db 0
            db wingamdat_prgrs2_id
            db 0,0
            ds 4

;### PROFLD -> sets one field in the progress bar
;### Input      C=position (0-7), B=type (0=saved, 1=todo, 2=empty, 3=todo reached, 4=all reached, 5=dead), HL=progress bar bitmap
;### Destroyed  AF,BC,DE,HL
profld  ld a,c
        add a
        add c       ;a=a*3
        add 26*3+1  ;a=a*3 + 26*3+1
profld1 nop             ;msx -> add a
        ld e,a
        ld d,0
        add hl,de
        ex de,hl        ;de=destination
        ld a,b
profld2 nop             ;msx -> add a
        add a
        ld c,a
        add a:add a:add a
        add c       ;a=a*18
        ld c,a
        ld b,0
        ld hl,ui_prgr_fields+10
        add hl,bc
        ld a,6
profld3 ld bc,3         ;msx -> 6
        ldir
        ex de,hl
profld4 ld bc,26-3      ;msx -> 52-6
        add hl,bc
        ex de,hl
        dec a
        jr nz,profld3
        ret

;### PROINI -> resets progress bar
;### Input      HL=mapdata (race 0 total, race 0 min, race 1 total, race 1 min)
proini  push hl
        ld c,64
        call proini6
        pop hl        
        ld c,(hl)
        inc hl
        ld b,(hl)
        ld a,(lemracnum)
        dec a
        jr nz,proini5
        ld e,wingamdat_prgrs1_id-1
        ld a,(wingam_id)
        push bc
        push ix
        call SyDesktop_WINDIN
        pop ix
        pop bc
        ld ix,0*lemraclen+lemracmem
        jr proini0

proini6 ld hl,wingamdat_prgrs2+2
        ld de,16
        ld b,6
proini7 ld a,(hl)
        and 63
        or c
        ld (hl),a
        add hl,de
        djnz proini7
        ret

proini5 push bc
        ld c,0
        call proini6
        pop bc
        srl c
        srl b
        push bc
        ld ix,0*lemraclen+lemracmem
        call proini0
        pop bc
        ld ix,1*lemraclen+lemracmem

proini0 ld (ix+lemracmax),c     ;C=number of lemmings (2,4,6,8), B=minimum lemmings, IX=lemming race data record
        ld (ix+lemraccnt),c
        ld (ix+lemracmin),b
        ld (ix+lemracsav),0
        ld (ix+lemracded),0
        push bc
        ld l,(ix+lemracpbm+0)
        ld h,(ix+lemracpbm+1)
        ld bc,256*2+8   ;clear field
proini1 dec c
        push bc
        push hl
        call profld
        pop hl
        pop bc
        inc c:dec c
        jr nz,proini1
        pop bc
        xor a
        ld (ix+lemracpfi),a
        ld b,c
        ld c,a
proini2 push bc
        ld b,1
        push hl
        call profld
        pop hl
        pop bc
        inc c
        djnz proini2
        ld a,(ix+lemracpct+0):ld iyl,a
        ld a,(ix+lemracpct+1):ld iyh,a  ;iy=controls
        ld a,(ix+lemracpfi)     ;set lemming symbol
        push af
        ld hl,ui_prgr_lem
        call proini4
        ld a,(iy+00+6)
        add 3
        ld (iy+16+6),a          ;adjust lemming figure
        ld bc,32
        add iy,bc
        pop bc
        dec b
        ld a,(ix+lemracmin)
        cp (ix+lemracmax)
        ld (iy+2),64
        jr z,proini3            ;min=total -> no minimum symbol
        ld (iy+2),10
        add b
        push bc
        ld hl,ui_prgr_mi0
        call proini4            ;set minimum symbol
        pop bc
proini3 ld de,16
        add iy,de
        ld a,(ix+lemracmax)
        add b
        ld hl,ui_prgr_mx0
        call proini4            ;set maximum symbol
        ld a,(wingam_id)
        ld d,(ix+lemracpci)
        ld e,-6
        jp SyDesktop_WINDIN

proini4 ld (iy+4),l             ;a=position, hl=bitmap, iy=control record
        ld (iy+5),h
        add a:add a
        ld c,a
        add a
        add c           ;a=a*12
        add map_xofs+map_xlen*fld_xlen+4
        ld (iy+6),a
        ret

;### PROSAV -> lemming saved, update progress bar
;### Input      IX=lemming race data record
;### Output     CF=1 all remaining lemmings saved
;### Destroyed  AF,BC,DE,HL,IY
prosav  inc (ix+lemracsav)      ;increase saved lemmings
        ld a,(ix+lemracsav)
        cp (ix+lemracmin)
        ld b,3
        jr z,prosav1
        cp (ix+lemracmax)
        ld b,4
        jr z,prosav1
        ld b,0                  ;b=type (0=normal, 3=min reached, 4=max reached)
prosav1 dec a
        ld c,a
        push bc
        call prosav4            ;add field
        pop af
        sub 3
        jr c,prosav3
        push af                 ;max reached -> update symbol
        ld bc,256*80+20
        ld hl,msg_perfect
        call nz,dspmsg
        pop af
        ld de,ui_prgr_mx1
        ld bc,3*16+4
        ld a,5
        jr nz,prosav2
        ld bc,256*80+20         ;min reached -> update symbol
        ld hl,msg_great
        call dspmsg
        ld de,ui_prgr_mi1
        ld bc,2*16+4
        ld a,4
prosav2 add (ix+lemracpci)
        ld l,(ix+lemracpct+0)
        ld h,(ix+lemracpct+1)
        add hl,bc
        ld (hl),e
        inc hl
        ld (hl),d
        ld e,a
        ld a,(wingam_id)
        push ix
        call SyDesktop_WINDIN
        pop ix
prosav3 ld a,(ix+lemracsav)     ;check, if all remaining saved
        add (ix+lemracded)
        cp (ix+lemracmax)
        scf
        ret z                   ;saved+dead = total -> cf=1
        or a
        ret
prosav4 ld l,(ix+lemracpbm+0)   ;add new field
        ld h,(ix+lemracpbm+1)
        ld a,(ix+lemracpfi)
        add c
        ld c,a
        call profld
        ld e,(ix+lemracpci)     ;show progress bar
        inc e
        ld a,(wingam_id)
        push ix
        call SyDesktop_WINDIN
        pop ix
        ret

;### PRODED -> lemming died, update progress bar
;### Input      IX=lemming race data record
;### Output     CF=1 too many dead lemmings
;###            ZF=1 no active lemmings
;### Destroyed  AF,BC,DE,HL,IY
proded  inc (ix+lemracded)
        ld a,(ix+lemracmax)
        sub (ix+lemracded)      ;a=remaining lemmings
        push af
        ld c,a
        ld b,5
        call prosav4
        pop af
        cp (ix+lemracmin)       ;remaining<min -> cf=1
        ret c
        call prosav3            ;a<255, cf=1 remaining saved
        inc a
        ret nc                  ;not all saved -> cf=0, zf=0
        xor a                   ;all saved     -> cf=0, zf=1
        ret



;==============================================================================
;### KEYS AND EXITS ###########################################################
;==============================================================================

exidatmax   equ 4
exitabmad   ds exidatmax*2  ;exit map address of upper part
exidatnum   db 0            ;number of exits
exidatlim   db 0            ;limit for each exit (0=no limits)
exidatpnt   dw 0
exidatrac   db 0            ;race type (0=one, 1=two, 3=ladies)

keytabexi   ds exidatmax    ;exit id (1-max, 0=none) for every key
keydattyp   db 0            ;0=collect one for opening one exit, 1=collect all for opening all exits)
keydatnum   db 0            ;number of keys (=flag [>0], if keys are used)
keydatdig   dw 0            ;current dig field


;### KEYINI -> inits key/exit management, selects a key for every exit
;### Input      C=number of exits, A=number of keys, B=limit for each exit (0=no limits), E=key type (0=one, 1=all), D=race type (0=one, 1=two, 3=ladies)
;### Output     (exitabkey) set
keyini  ld (keydatnum),a
        ld (keyini3+1),a
        or a
        push af
        ld a,b
        ld (exidatlim),a
        ld a,e
        ld (keydattyp),a
        ld a,d
        ld (exidatrac),a
        and 1
        inc a
        ld (lemracnum),a

        pop af
        jr z,keyini4        ;no keys -> skip key-exit link
        ld a,c
        ld (exidatnum),a
        dec e
        jr z,keyini4        ;need all keys -> skip key-exit link
        ld hl,keytabexi+1
        ld de,keytabexi
        ld bc,exidatmax-1
        ld (hl),0           ;reset key table
        ldir
keyini1 push af                 ;*** select a key for every exit
keyini2 call clcrnd         ;hl=random number
keyini3 ld h,0              ;l=0-255, h=num of keys
        call clcmu8         ;h=selected key (0 .. num-1)
        ld l,h
        ld h,0
        ld bc,keytabexi
        add hl,bc           ;(hl)=exit for this key
        ld a,(hl)
        or a
        jr nz,keyini2       ;this key already opens an exit -> select another one
        pop af
        ld (hl),a           ;set exit for this key
        dec a
        jr nz,keyini1
keyini4 ld hl,exitabmad
        ld (exidatpnt),hl
        ld hl,keytabexi
        ld (keydatdig),hl
        ret

;### KEYENT -> try to enter an exit
;### Input      HL=map address upper part, IX=lemming data record
;### Output     CF=0 exit will be entered
;### Destroyed  AF,BC,DE,HL
keyent  inc hl
        ld a,(hl)
        bit 7,a
        jr z,keyent1
        and 64
        cp (ix+lemdatrac)
        ret z               ;same race -> enter
        scf                 ;wrong race -> don't enter
        ret
keyent1 or a
        ret z               ;no limit -> enter
        dec (hl)            ;decrease limit counter
        jr nz,keyent2
        push hl
        dec hl
        ld (hl),idm_excr_t  ;zero reached -> set upper to crashed
        call fldchg
        call fldupd
        pop hl
keyent2 ld de,map_xlen*2-1  ;decrease limit symbol
        add hl,de
        dec (hl)
        call fldchg
        call fldupd
        or a
        ret

;### KEYOPN -> may open exit(s) after digging a field
;### Input      HL=map address, A=old field
;### Destroyed  AF,BC,DE,HL
keyopn  dec a
        bit 2,a
        ret z
        ld a,(keydattyp)
        or a
        jr nz,keyopn4
        inc hl                      ;*** one key for one exit
        ld a,(hl)
        or a
        ret z
keyopn1 push af                     ;*** open exit (A=id, 1..max)
        ld hl,SND_DOOR_UNLOCK
        call sndefx
        pop af
        add a
        ld l,a
        ld h,0
        ld bc,exitabmad-2
        add hl,bc
        ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        inc hl
        ld a,(hl)               ;a=exit type (0-9=limit, 128=race0, 192=race1)
        dec hl
        or a
        jp m,keyopn3
        ld c,idm_exit_b         ;all and not limited
        jr z,keyopn2
        add idm_exit_0          ;all and limited
        ld c,a
keyopn2 ld (hl),idm_exit_t
        ld de,map_xlen*2
        add hl,de
        ld (hl),c
        push hl
        call fldchg             ;redraw lower part
        call fldupd
        pop hl
        ld de,-map_xlen*2
        add hl,de
        call fldchg             ;redraw upper part
        jp fldupd
keyopn3 ld c,idm_exit_b1        ;race 0/1
        bit 6,a
        jr z,keyopn2
        inc c
        jr keyopn2
keyopn4 ld hl,keydatnum             ;*** all keys for all exits
        dec (hl)
        ld a,(exidatnum)
        jr nz,keyopn6
keyopn5 push af                 ;all digged -> open all exits
        call keyopn1
        pop af
        dec a
        jr nz,keyopn5
        ret
keyopn6 ld hl,exitabmad         ;not all digged -> decrease counter symbols
keyopn7 push af
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        push hl
        ld hl,map_xlen*2
        add hl,de
        dec (hl)                ;decrease counter symbol
        call fldchg             ;redraw lower part
        call fldupd
        pop hl
        pop af
        dec a
        jr nz,keyopn7
        ret

;### KEYSPC -> adds a special map element (exit, digger field, dropper)
;### Input      IY=map address, A=type+128 (0=exit all, 1=digger field, 2=exit race0, 3=exit race1)
;### Output     IY=IY+2
;### Destroyed  AF,C,DE,HL
keyspc  push iy:pop hl
        inc iy:inc iy
        res 7,a
        cp 1
        jp z,keydig         ;1 -> digger
        push bc
        rrca:rrca           ;0/2/3 -> exit
        call keyexi
        pop bc
        ret

;### KEYDIG -> adds a digger field
;### Input      HL=map address
;### Output     (mapmem+1) updated (0 or exit ID)
;### Destroyed  F,DE,HL
keydig  ld (hl),idm_grqm
        ex de,hl
        inc de
        ld hl,(keydatdig)
        ldi
        ld (keydatdig),hl
        inc bc
        ret

;### KEYEXI -> adds an exit
;### Input      HL=map address of lower part, A=exit type (0=all, 128=race0, 192=race1), (keyexi3+1)=limit per exit (0=none)
;### Output     (mapmem) updated (upper and lower part of exit set)
;### Destroyed  AF,BC,DE,HL
keyexi  ld de,-map_xlen*2
        add hl,de
        ex de,hl            ;de=map address of upper part
        ld hl,(exidatpnt)
        ld (hl),e
        inc hl
        ld (hl),d           ;store mad
        inc hl
        ld (exidatpnt),hl
        inc de
        ld hl,exidatlim
        or (hl)
        ld c,a
        ld (de),a           ;store exit type or limit in map
        dec de
        ex de,hl            ;hl=map
        ld a,(keydatnum)
        or a
        jr z,keyexi2
        ld (hl),idm_excl_t      ;*** closed exit, set upper to closed
        ld de,map_xlen*2
        add hl,de
        ld a,(keydattyp)
        or a
        jr nz,keyexi1
        ld (hl),idm_excl_b  ;closed and one key -> lower = closed
        ret
keyexi1 ld a,(keydatnum)
        add idm_exit_0
        ld (hl),a           ;closed and all keys -> lower = counter
        ret
keyexi2 ld (hl),idm_exit_t      ;*** open exit, set upper to open
        ld de,map_xlen*2
        add hl,de
        bit 7,c
        jr nz,keyexi4
keyexi3 ld a,0            
        ld (hl),idm_exit_b  ;open, lower = all
        or a
        ret z               ;not limited
        add idm_exit_0
        ld (hl),a           ;limited
        ret
keyexi4 rl c:rl c           ;open, lower = race
        ld a,idm_exit_b1
        adc 0
        ld (hl),a
        ret


;==============================================================================
;### MAP ######################################################################
;==============================================================================

read "App-Lymings-MapsDemo.asm"

fld_xlen    equ 12
fld_ylen    equ 12

map_xofs    equ 0
map_yofs    equ 2

map_xlen    equ 10
map_ylen    equ 16
map_xlenv   equ map_xlen-2
map_ylenv   equ map_ylen-2

map_len     equ 2*map_xlen*map_ylen

mapmem      ds map_len      ;1byte map element type, 1byte map element status, 1+8+1 columns, 1+14+1 rows


map_trns
;base
db " ",idm_empt,0       ;empty
db "#",idm_wall,0       ;hard wall
db "*",idm_grnd,0       ;soft ground

;entrance/keys/exits
db "A",idm_entrance,0   ;entrance race 0 going right
db "a",idm_entrance,0   ;entrance race 0 going left
db "B",idm_entrance,0   ;entrance race 1 going right
db "b",idm_entrance,0   ;entrance race 1 going left
db "C",idm_entrance,0   ;entrance mixed  going right
db "c",idm_entrance,0   ;entrance mixed  going left

db "?",128+1,0          ;key
db "x",idm_exit_t,0     ;exit top
db "X",128+0,0          ;bottom all
db "1",128+2,0          ;bottom race0
db "2",128+3,0          ;bottom race1

;passive traps
db "M",idm_trap_mine,0      ;mine
db "W",idm_watr1,1          ;water

;active traps
db "r",idm_trap_rock,128+idt_rcks   ;falling rocks soft
db "R",idm_trap_rock,128+idt_rckh   ;falling rocks hard (ground breakers)
db "<",idm_trap_firl,128+idt_firl   ;fire left
db ">",idm_trap_firr,128+idt_firr   ;fire right
db "^",idm_trap_firu,128+idt_firu   ;fire up
db "+",idm_trap_ele1,128+idt_elec   ;electric
db "o",idm_trap_saw ,128+idt_saws   ;saw small
db "O",idm_trap_saw ,128+idt_sawb   ;saw big
db "G",idm_trap_ghs1,128+idt_ghst   ;ghost

;marked (only for demo mode)
db "/",idm_brdg_rf,0        ;bridge right
db "\",idm_brdg_lf,0        ;bridge left

db "S",idm_stop_mark,0      ;stopper
db "P",idm_para_mark,0      ;parachute
db "d",idm_grnd_mark,0      ;digging
db "D",idm_grqm_mark,0      ;digging with question mark

db ";",idm_brdg_rh,0    ;for quick testing
db ":",idm_brdg_lh,0    ;for quick testing

db 0


;### MAPLOD -> loads and initializes a map
;### Input      HL=map data (number of lemmings, min to save, start delay, drop delay, key type, race type, seconds, minutes, items,
;###                         14*10 ascii data)
;### Output     (figdrpmem)=dropper data, (mapmem)=map
;### Destroyed  AF,BC,DE,HL,IX,IY
maplodl db idm_bord_l
maplodr db idm_bord_r
maplodx db 0
maplody db 0

maplod  push hl                 ;*** count exits, keys, get dropper coordinates
        ld bc,9
        add hl,bc
        ld bc,map_xlen*map_ylenv*256+0  ;b=field counter, c=dropper counter
        ld de,0                         ;e=exit counter,  d=key counter
        ld ix,figdrpmem
        ld iy,256*1+0
maplode ld a,(hl)
        cp "X":jr nz,maplodo:inc e:jr maplodh
maplodo cp "1":jr nz,maplodn:inc e:jr maplodh
maplodn cp "2":jr nz,maplodf:inc e:jr maplodh
maplodf cp "?":jr nz,maplodg:inc d:jr maplodh
maplodg call clcucs
        cp "A"
        jr c,maplodh
        cp "C"+1
        jr nc,maplodh
        sub "A"
        ld (ix+figdrprac),a             ;set dropper race (0/1, 2=mixed)
        ld a,(hl)
        inc c
        sub "a"     ;"A" -> cf=1, "a" -> cf=0
        ld a,1
        sbc 0       ;"A" -> 0 right, "a" -> 1 left
        ld (ix+figdrpdir),a             ;set dropper direction
        ld a,iyl:ld (ix+figdrpxps),a    ;set dropper x/y
        ld a,iyh:ld (ix+figdrpyps),a
        push bc
        ld bc,figdrplen
        add ix,bc
        pop bc
maplodh inc iyl
        ld a,10
        cp iyl
        jr nz,maplodi
        ld iyl,0
        inc iyh
maplodi inc hl
        djnz maplode                    ;-> c=droppers, e=exits, d=keys
        pop hl
        push hl
        push de

        ld e,(hl)           ;e=number of lemmings
        push de
        inc hl
        inc hl
        ld d,(hl)           ;d=start delay
        inc hl
        ld b,(hl)           ;b=drop delay
        ld l,b
        ld h,0
        push bc
        call clcdiv         ;l=start delay difference
        pop bc
        ld a,l
        ld (maplodk+1),a
        ld l,e
        ld h,0
        push bc
        call clcdiv         ;l=lemmings per dropper
        pop bc
        ld a,d              ;a=start delay
        add 15
        ld ix,figdrpmem
        ld de,figdrplen
maplodj ld (ix+figdrpnum),l
        ld (ix+figdrpdly),b
        ld (ix+figdrpcnt),a
maplodk add 0
        add ix,de
        dec c
        jr nz,maplodj
        pop hl              ;l=number of lemmings
        pop de              ;e=exits, d=keys
        ld a,e
        dec a
        jr z,maplodm
        ld h,0
        ld c,e
        call clcdiv
        ld a,l              ;a=limit per exit
maplodm ld (keyexi3+1),a
        pop hl
        inc hl:inc hl:inc hl:inc hl
        ld c,e              ;c=number of exits
        ld b,a              ;b=limit per exit
        ld a,d              ;a=number of keys
        ld e,(hl):inc hl    ;e=key type
        ld d,(hl):inc hl    ;d=race type
        push hl
        call keyini             ;init keys and exits
        call trpini             ;init active traps
        pop hl

        ld de,gamtimv           ;copy timer
        ldi:ldi
        ld de,gamitmv           ;copy items
        ldi

        push hl                 ;generate map
        ld hl,idm_bord_r*256+idm_bord_l
        ld (maplodl),hl
        ld hl,mapmem
        ld de,mapmem+1
        ld (hl),idm_wall        ;*** fill map with walls
        ld bc,map_len-1
        ldir

        ld a,map_yofs-fld_ylen
        ld (maplody),a

        ld iy,2*map_xlen+mapmem     ;iy=dest
        pop ix                      ;ix=source
        ld b,map_ylen-2             ;b=ycount
maplod1 push bc                 ;*** load inner map

        ld a,map_xofs
        ld (maplodx),a
        ld a,(maplody)
        add fld_ylen
        ld (maplody),a              ;update x/ypos for trap handling

        ld hl,idm_door_lu*256+idm_door_ll       ;h=upper door, l=lower door
        ld de,idm_bord_l *256+idm_bord_llnk     ;d=border    , e=border with link
        ld bc,maplodl
        call maplod2                ;set left border
        ld b,map_xlen-2             ;b=xcount
maplodb ld a,(maplodx)
        add fld_xlen
        ld (maplodx),a
        ld hl,map_trns
maplod6 ld a,(hl)                   ;search for map element ID
        inc hl
        or a
        jr z,maplod8
        cp (ix+0)
        jr z,maplod7
        inc hl:inc hl
        jr maplod6
maplod8 ld a,idm_wall               ;unknown char -> use wall as dummy
        ld c,0
        jr maplod5
maplod7 ld a,(hl)                   ;char found -> copy id and status
        bit 7,a
        jr z,maplodc
        call keyspc
        jr maplodd
maplodc inc hl
        ld c,(hl)
maplod5 call maplod4
        bit 7,c
        res 7,c
        ld (iy-1),c
        call nz,trpadd
maplodd inc ix
        djnz maplodb
        ld hl,idm_door_ru*256+idm_door_rl       ;h=upper door, l=lower door
        ld de,idm_bord_r *256+idm_bord_rlnk     ;d=border    , e=border with link
        ld bc,maplodr
        call maplod2                ;set right border
        pop bc
        djnz maplod1

        call aniini                 ;resets animations
        ld ix,2*map_xlen+mapmem ;*** generate controls
        ld iy,wingamdat_map
        ld c,map_ylenv
        ld de,map_yofs              ;de=ypos
maplod9 ld b,map_xlen
        ld hl,map_xofs              ;hl=xpos
maploda ld (iy+02),10               ;set type, pos, size
        ld (iy+03),255
        ld (iy+06),l
        ld (iy+07),h
        ld (iy+08),e
        ld (iy+09),d
        ld (iy+10),fld_xlen
        ld (iy+12),fld_ylen
        push bc
        push hl
        ld a,(ix+0)
        push af
        call anichk
        pop af
        inc ix:inc ix
        add a
        ld hl,gmp_table
        ld l,a
        ld a,(hl)                   ;set gfx adr
        ld (iy+04),a
        inc hl
        ld a,(hl)
        ld (iy+05),a
        ld bc,16
        add iy,bc
        pop hl
        ld c,fld_xlen
        add hl,bc
        pop bc
        djnz maploda
        ld hl,fld_ylen
        add hl,de
        ex de,hl
        dec c
        jr nz,maplod9
        ret

maplod2 ld a,(ix+0)
        inc ix
        cp "|"
        ld a,(bc)
        jr z,maplod4    ;no door, take current in A
        cp d
        jr nz,maplod3
        ld a,e          ;link begins -> current = border with link
        ld (bc),a
        ld a,h          ;set upper door
        jr maplod4
maplod3 ld a,d
        ld (bc),a       ;link ends   -> current = border
        ld a,l          ;set lower door
maplod4 ld (iy+0),a
        ld (iy+1),0
        inc iy
        inc iy
        ret

;### MAPCLK -> user clicked on map
mapclk_adr  dw 0    ;address in map
mapclk_ctr  dw 0    ;address of control
mapclk_cid  db 0    ;control ID
mapclk_lem  dw 0    ;selected lemming data record (stopper, suicider)

mapclk_rules
        ;can be marked for bridges, stop, parachute
        dw mapclke:db idm_empt,idm_brdg_rh,idm_brdg_rf,idm_brdg_lh,idm_brdg_lf,0
        ;will be marked for digging
        dw mapclkg:db idm_grnd,idm_grqm,idm_grnd_brk1,idm_grnd_brk2,idm_grnd_brk3,idm_grqm_brk1,idm_grqm_brk2,idm_grqm_brk3,0
        ;will be unmarked
        dw mapclkm:db idm_grnd_mark,idm_grqm_mark,idm_grnd_br1m,idm_grnd_br2m,idm_grnd_br3m,idm_grqm_br1m,idm_grqm_br2m,idm_grqm_br3m
                   db idm_brdg_rmrk,idm_brdg_lmrk,idm_para_used,idm_para_mark,idm_stop_mark
                   db idm_brdg_rh_s,idm_brdg_rf_s,idm_brdg_lh_s,idm_brdg_lf_s,idm_brdg_rf_x,idm_brdg_lf_x,0
        dw 0

mapclk_dig      ;marked for digging - translation
        db idm_grnd,idm_grnd_mark
        db idm_grqm,idm_grqm_mark
        db idm_grnd_brk1,idm_grnd_br1m
        db idm_grnd_brk2,idm_grnd_br2m
        db idm_grnd_brk3,idm_grnd_br3m
        db idm_grqm_brk1,idm_grqm_br1m
        db idm_grqm_brk2,idm_grqm_br2m
        db idm_grqm_brk3,idm_grqm_br3m
        db 0

mapclk_unmark   ;unmark - translation
        db idm_grnd_mark,idm_grnd
        db idm_grqm_mark,idm_grqm
        db idm_grnd_br1m,idm_grnd_brk1
        db idm_grnd_br2m,idm_grnd_brk2
        db idm_grnd_br3m,idm_grnd_brk3
        db idm_grqm_br1m,idm_grqm_brk1
        db idm_grqm_br2m,idm_grqm_brk2
        db idm_grqm_br3m,idm_grqm_brk3
        db idm_brdg_rmrk,idm_empt
        db idm_brdg_lmrk,idm_empt
        db idm_para_used,idm_empt
        db idm_para_mark,idm_empt
        db idm_stop_mark,idm_empt
        db idm_brdg_rh_s,idm_brdg_rh
        db idm_brdg_rf_s,idm_brdg_rf
        db idm_brdg_lh_s,idm_brdg_lh
        db idm_brdg_lf_s,idm_brdg_lf
        db idm_brdg_rf_x,idm_brdg_rf
        db idm_brdg_lf_x,idm_brdg_lf
        db 0

mapclk_select   ;select symbols
        dw mapsel_stp, gsl_stop
        dw mapsel_par, gsl_para
        dw mapsel_blf, gsl_brdg_left
        dw mapsel_brg, gsl_brdg_rght
        dw mapsel_sui, gsl_suic
        dw mapsel_gol, gsl_go_left
        dw mapsel_gor, gsl_go_rght

mapclk  ld a,(App_MsgBuf+4)     ;mouse xpos
        sub map_xofs+fld_xlen
        jp c,prgprz0
        ld l,a:ld h,0
        ld c,fld_xlen
        call clcdiv
        ld e,l                  ;e=xfield
        ld a,(App_MsgBuf+6)     ;mouse ypos
        sub map_yofs
        jp c,prgprz0
        ld l,a:ld h,0
        ld c,fld_ylen
        call clcdiv             ;l=yfield
        ld c,e
        ld b,l
        ld (mapclkb+1),bc
        inc l
        ld h,map_xlen
        call clcmu8
        ld a,l
        add c
        inc a                   ;a=field inside map
        ld c,a
        ld b,0
        add wingamdat_map_id-map_xlen
        ld (mapclk_cid),a       ;a=control ID
        ld l,a
        ld h,0                  ;calculated control address
        add hl,hl:add hl,hl:add hl,hl:add hl,hl
        ld de,wingamdat
        add hl,de
        ld (mapclk_ctr),hl
        ld ix,mapmem
        add ix,bc
        add ix,bc               ;(ix)=map data
        push ix:pop hl
        call stpchk
        ld (mapclk_lem),de
        ld c,32+64
        jr c,mapclk8            ;stopper -> select go left/right

        ld (mapclk_adr),ix
        ld c,(ix+0)
        ld hl,mapclk_rules
mapclk1 ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ld a,e:or d
        jp z,prgprz0            ;** field not clickable
mapclk2 ld a,(hl)
        inc hl
        or a
        jr z,mapclk1
        cp c
        jr nz,mapclk2
        ex de,hl
        jp (hl)

mapclke ld a,(ix+0)             ;** bridge or empty  field
        cp idm_empt
        jr nz,mapclk6
        bit 6,(ix+map_xlen+map_xlen)    ;empty
        ld a,1                  ;test below
        jr z,mapclk4            ;solid -> bit0=stop
        ld a,2                  ;open  -> bit1=parachute
mapclk4 bit 6,(ix-map_xlen-map_xlen)    ;test above
        jr z,mapclk5
        add 4+8                 ;open  -> bit2,3=bridge to right/left
mapclk5 ld c,a
        jr mapclkh

mapclk6 ld c,1                  ;bridge -> bit0=stop
        cp idm_brdg_rf          ;check if on a completed bridge
        jr z,mapclk7
        cp idm_brdg_lf
        jr nz,mapclkh
mapclk7 set 4,c                 ;bit4=suicide

mapclkh ld a,(gamitmv)
        or a
        jp z,prgprz0            ;no items left -> don't open selection

mapclk8 ld a,c
        cp 1:jp z,mapsel_stp    ;only one stopper   -> select directly
        cp 2:jp z,mapsel_par    ;only one parachute -> select directly
        ld hl,mapclk_select     ;prepare selection window
        ld ix,winseldat_symbols
        ld b,7
        ld a,winseldat_symbols_id
mapclk9 rr c
        jr nc,mapclka
        push hl
        ld e,(hl):ld (ix+0),e:inc hl
        ld d,(hl):ld (ix+1),d:inc hl
        push hl
        ld hl,ui_selsta
        bit 0,(hl)
        jr z,mapclkn
        ld hl,(mappslv)
        or a
        sbc hl,de
        jp z,mapclko                ;*** preselection fits
mapclkn pop hl
        ld e,(hl):ld (ix+4),e:inc hl
        ld e,(hl):ld (ix+5),e
        pop hl
        ld de,16
        add ix,de
        inc a
mapclka ld de,4
        add hl,de
        djnz mapclk9
        ld (winselgrp+0),a
        sub winseldat_symbols_id+4
        ld hl,256*1+1       ;2 lines
        ld de,31*256+46
        ld bc,6-23
        jr nc,mapclkc
        ld l,5              ;1 line
        ld d,24         ;3 rows
        inc a
        jr z,mapclkc
        ld bc,6-23+7    ;2 rows
        ld e,33
        ld h,2
        inc a
        jr z,mapclkc
        ld h,9          ;1 row
mapclkc ld (mapclkf+1),bc               ;xofs
        ld a,e
        ld (winsel+08),a                ;winxsize
        ld (winsel+20),a
        ld (winsel+24),a
        ld a,d
        ld (winsel+10),a                ;winysize
        ld (winsel+22),a
        ld (winsel+26),a
        ld a,h
        ld (winseldat_symbols+00+6),a
        add 15
        ld (winseldat_symbols+16+6),a
        ld a,l
        ld (0*16+winseldat_symbols+8),a ;symbls ypos 1st row
        ld (1*16+winseldat_symbols+8),a
        ld (2*16+winseldat_symbols+8),a
mapclkb ld bc,0                 ;c=xfield, b=yfield
        inc c
        ld l,b                  ;set marked field position
        ld h,fld_ylen
        call clcmu8
        ld a,l
        add map_yofs
        ld (wingamdat_select+8),a
        push af
        ld l,c
        ld h,fld_xlen
        call clcmu8
        ld a,l
        add map_xofs
        ld (wingamdat_select+6),a
        ld hl,(wingam+4)        ;set selection window position
        ld c,a
        ld b,0
        add hl,bc
mapclkf ld bc,6-23              ;fld_xsiz/2-selwinxsize/2
        add hl,bc
        ld (winsel+4),hl        ;set x
        pop af
        ld hl,winsel+10
        add 9-1
        sub (hl)
        jp nc,mapclkd
        add (hl)
        add 2+fld_ylen
mapclkd ld hl,(wingam+6)
        ld c,a
        ld b,0
        add hl,bc
        ld (winsel+6),hl        ;set y

        ld de,winsel
        ld hl,winsel_id
        call winopn0            ;open selection window
        ld a,2
        ld (wingamdat_select+2),a
        ld e,wingamdat_select_id
        ld a,(wingam_id)
        call SyDesktop_WINDIN
        jp prgprz0

mapclkg ld a,(gamitmv)          ;** ground field
        or a
        jp z,prgprz0            ;no items left -> don't open selection
        sub 2
        ld (gamitmv),a
        ld hl,mapclk_dig-1
        jr mapclk3

mapclkm ld hl,mapclk_unmark-1   ;** marked field
mapclk3 inc hl
        ld a,(hl)
        or a
        jp z,prgprz0
        inc hl
        cp c
        jr nz,mapclk3
        cp idm_para_used
        push af
        ld a,(hl)
        ld (ix+0),a
        call mapsel0
        pop af
        jp z,mapusl2
        ld a,(gamitmv)
        inc a
        call gamitm
        jp mapusl2

mapclko pop hl
        pop hl
        ex de,hl
        jp (hl)

;### MAPSEL -> click selection window
mapsel_gol                      ;select go left
        ld c,1
        ld de,lemmlf
        jr mapsel3
mapsel_gor                      ;select go right
        ld c,0
        ld de,lemmrg
mapsel3 ld ix,(mapclk_lem)
        ld b,(ix+lemdatdir)
        ld (ix+lemdatdir),c
        ld (ix+lemdatsub),7
        ld l,(ix+lemdatmad+0)
        ld h,(ix+lemdatmad+1)
        push bc
        push de
        push hl
        call stpdel
        pop hl
        pop de
        pop bc
        ld a,(ix+lemdatctr+0):ld iyl,a
        ld a,(ix+lemdatctr+1):ld iyh,a
        dec b:dec b
        jr z,mapsel5
        ld bc,map_xlen*2
        add hl,bc
        ld a,(hl)
        ex de,hl
        cp idm_grnd_mark
        jr c,mapsel4
        cp idm_grqm_br3m+1
        jr nc,mapsel4
        ld (ix+lemdatsub),7     ;fall left side and continue right
        bit 0,(ix+lemdatdir)
        jr z,mapsel8
        inc (ix+lemdatsub)
        ld a,(ix+lemdatfxp)
        add 4
        ld (ix+lemdatxps),a
        ld (ix+lemdatrxp),-1    ;fall right side and continue left
mapsel8 call lowdig0
        jp mapusl
mapsel4 ld (ix+lemdatjmp+0),l
        ld (ix+lemdatjmp+1),h
        jp mapusl
mapsel5 ld (iy+00+2),10         ;stopper on bridge -> activate background restore
        ld a,(hl)
        cp idm_brdg_lf
        jr nc,mapsel6
        ld hl,lembru            ;on bridge right up
        dec c
        jr nz,mapsel4               ;right -> bright right up
        ld hl,lembld                ;left  -> bridge left down
        ld a,-2
        jr mapsel7
mapsel6 ld hl,lembrd            ;on bridge left up
        dec c
        jr nz,mapsel4               ;right -> bridge right down
        ld hl,lemblu                ;left  -> bridge left up
        ld a,-6
mapsel7 ld (ix+lemdatrxp),-1
        add (ix+lemdatfyp)
        ld (ix+lemdatyps),a
        ld (iy+00+8),a
        ld (iy+16+8),a
        jr mapsel4

mapsel_sui                      ;select suicider
        ld hl,(mapclk_adr)
        ld a,(hl)
        cp idm_brdg_rf
        ld a,idm_brdg_rf_x
        jr z,mapsel1
        ld a,idm_brdg_lf_x
        jr mapsel1
mapsel_stp                      ;select stopper
        ld hl,(mapclk_adr)
        ld a,(hl)
        cp idm_empt
        jr z,mapsel2
        add idm_brdg_rf_s-idm_brdg_rf
        jr mapsel
mapsel2 ld a,idm_stop_mark
        jr mapsel
mapsel_par                      ;select parachute
        ld a,idm_para_mark
        jr mapsel
mapsel_brg                      ;select bridge right
        ld a,idm_brdg_rmrk
        jr mapsel
mapsel_blf                      ;select bridge left
        ld a,idm_brdg_lmrk

mapsel  ld hl,(mapclk_adr)      ;set map
mapsel1 ld (hl),a
        inc hl
        ld b,0
        ld (hl),b
        call mapsel0
        ld a,(gamitmv)
        dec a
        call gamitm
        jr mapusl
mapsel0 add a                   ;set control
        ld hl,gmp_table
        ld l,a
        ld ix,(mapclk_ctr)
        ld a,(hl):ld (ix+4),a:inc hl
        ld a,(hl):ld (ix+5),a
        ret

;### MAPUSL -> cancel selection window
mapusl  ld a,(winsel_id)
        or a
        call nz,wincls
        ld a,64
        ld (wingamdat_select+2),a
mapusl2 ld a,(mapclk_cid)
        ld e,a
        ld a,(wingam_id)
        call SyDesktop_WINDIN
        jp prgprz0

;### MAPPSLx -> preselect item
mappsls db 0
mappslv dw mapsel_stp

mappsla ld b,0      ;stopper
        ld hl,mapsel_stp
        jr mappsl
mappslb ld b,1      ;parachute
        ld hl,mapsel_par
        jr mappsl
mappslc ld b,2      ;digger (useless)
        jr mappsl
mappsld ld b,3      ;bridge left
        ld hl,mapsel_blf
        jr mappsl
mappsle ld b,4      ;bridge right
        ld hl,mapsel_brg
        jr mappsl
mappslf ld b,5      ;suicider
        ld hl,mapsel_sui

mappsl  ld (mappslv),hl
        ld hl,mappsls
        ld a,(hl)
        ld (hl),b
        add wingamdat_sel_id
        ld e,a
        ld a,(wingam_id)
        push bc
        call SyDesktop_WINDIN
        pop af
        add a:add a:add a:add a
        add map_xofs+map_xlen*fld_xlen+5
        ld (16*6+wingamdat_sel+6),a
        ld e,wingamdat_sel_id+6
        ld a,(wingam_id)
        call SyDesktop_WINDIN
        ld hl,ui_selsta
        ld a,(hl)
        or a
        jp nz,prgprz0
        ld (hl),1
        ld e,wingamdat_sel_id-2
        ld a,(wingam_id)
        call SyDesktop_WINDIN
        jp prgprz0


;==============================================================================
;### HELP #####################################################################
;==============================================================================

hlpflg  db 0
hlpnum  db 1

;### HLPOPN -> opens help from pause dialugue
hlpopn  call wincls
        call hlpopn1
        jp prgprz0
hlpopn1 ld a,1
        ld (hlpflg),a
        ld de,winhlp
        ld hl,windia_id
        ld bc,256*2+42
        jp winopn1

;### HLPCNC -> closes help and returns to pause dialugue
hlpcnc  call wincls
        xor a
        ld (hlpflg),a
hlpcnc0 jp ctrpau1

;### HLPPGx -> switch help page 1/2/3/4/5
hlppg1  ld a,1              ;\
        ld hl,winhlpgrp1    ; 7bytes, see hlpkey1
        jr hlppag           ;/
hlppg2  ld a,2
        ld hl,winhlpgrp2
        jr hlppag
hlppg3  ld a,3
        ld hl,winhlpgrp3
        jr hlppag
hlppg4  ld a,4
        ld hl,winhlpgrp4
        jr hlppag
hlppg5  ld a,5
        ld hl,winhlpgrp5
        jr hlppag
hlppg6  ld a,6
        ld hl,winhlpgrp6

hlppag  ld (winhlp+36),hl
        ld (hlpnum),a
        ld e,-1
        ld a,(windia_id)
        call SyDesktop_WINDIN
hlppag0 jp prgprz0

;### HLPKEY -> checks cursor keys, if help is open
hlpkey  ld hl,hlpflg
        inc (hl):dec (hl)
        ret z
        cp 138
        ld c,-1
        jr z,hlpkey1
        cp 139
        ret nz
        ld c,1
hlpkey1 ld a,(hlpnum)
        add c
        ret z
        cp 7
        ret z
        ld c,a
        add a:add a:add a:sub c
        ld c,a
        ld b,0
        ld hl,hlppg1-7
        add hl,bc
        jp (hl)


;==============================================================================
;### EXTERNAL BITMAPS #########################################################
;==============================================================================

;### PRGFIL -> Generates datafile path
datnam  db "lymings.dat",0:datnam0
lvlnam  db "lymings.lvl",0          ;same length!
sndnam  db "lymings.snd",0

datpth  dw 0
datfil  dw 0

prgfil  ld hl,(App_BegCode)
        ld de,App_BegCode
        dec h
        add hl,de           ;HL = CodeEnd = path
        ld (datpth),hl
        ld e,l
        ld d,h              ;DE=HL
        ld b,255
prgfil1 ld a,(hl)           ;search end of path
        or a
        jr z,prgfil2
        inc hl
        djnz prgfil1
        jr prgfil4
        ld a,255
        sub b
        jr z,prgfil4
        ld b,a
prgfil2 dec hl              ;search start of filename
        ld a,(hl)
        cp "/"
        jr z,prgfil3
        cp "\"
        jr z,prgfil3
        cp ":"
        jr z,prgfil3
        djnz prgfil2
        jr prgfil4
prgfil3 inc hl
        ex de,hl
prgfil4 ld (datfil),de
        ld hl,datnam        ;replace application filename with config filename
        ld bc,datnam0-datnam
        ldir
        ret


bmpdato ds 64   ;offset table (max 64bytes/32 entries)

bmpdatb equ 0*5+prgmemtab+0     ;bitmap ram bank
bmpdata equ 0*5+prgmemtab+1     ;bitmap start address
bmpdatl equ 0*5+prgmemtab+3     ;bitmap length

bmplemb equ 1*5+prgmemtab+0     ;bitmap ram bank
bmplema equ 1*5+prgmemtab+1     ;bitmap start address
bmpleml equ 1*5+prgmemtab+3     ;bitmap length

;### BMPSET -> links external bitmaps into control records
bmpsett
db xid_gamlog  :dw 3+xcb_gamlog1
db xid_gamlog  :dw 3+xcb_gamlog2
db xid_pause   :dw 3+xcb_pause
db xid_speed   :dw 3+xcb_speed1
db xid_speed   :dw 3+xcb_speed2
db xid_quit    :dw 3+xcb_quit
db xid_timer   :dw 3+xcb_timer
db xid_items   :dw 3+xcb_items
db xid_end_u   :dw 3+xcb_end_u
db xid_end_d   :dw 3+xcb_end_d
db xid_planet1 :dw 3+xcb_planet1
db xid_planet2 :dw 3+xcb_planet2
db xid_planet3 :dw 3+xcb_planet3
db xid_planet4 :dw 3+xcb_planet4
db xid_line    :dw 3+xcb_line1
db xid_line    :dw 3+xcb_line2
db xid_line    :dw 3+xcb_line3
db xid_flag    :dw 3+xcb_flag1
db xid_flag    :dw 3+xcb_flag2
db xid_flag    :dw 3+xcb_flag3
db xid_arwrgt  :dw 3+xcb_arwrgt
db xid_arwlft  :dw 3+xcb_arwlft
db xid_arwup   :dw 3+xcb_arwup
db xid_arwdw   :dw 3+xcb_arwdw
db 0

bmpset  ld hl,bmpsett
bmpset1 ld a,(hl)
        or a
        ret z
        add a
        ld c,a
        ld b,0
        inc hl
        ld e,(hl):inc hl
        ld d,(hl):inc hl
        push hl
        ld a,(bmpdatb)
        ld (de),a
        inc de
        ld hl,bmpdato-2
        add hl,bc
        ldi:ldi
        pop hl
        jr bmpset1


;### BMPLOD -> loads external bitmaps
bmplodl dw 0    ;entry table size
        dw 0    ;bitmap size

bmplod  call prgfil             ;generate path
        ld hl,(datpth)
        ld ix,(App_BnkNum-1)
        call SyFile_FILOPN      ;open data file
        jp c,bmplod0
        ld hl,bmplodl
        ld de,(App_BnkNum)
        ld bc,4
        push af
        call SyFile_FILINP      ;load length information
        jr c,bmplode
        pop af
        ld hl,bmpdato
        ld de,(App_BnkNum)
        ld bc,(bmplodl+0)
        push af
        call SyFile_FILINP      ;load bitmap table
        jr c,bmplode
        xor a
        ld e,1
        ld bc,(bmplodl+2)
        ld (bmpdatl),bc
        push bc
        rst #20:dw jmp_memget   ;reserve bitmap memory
        pop bc
        jr c,bmplode
        ld (bmpdatb),a          ;register additional memory
        ld (bmpdata),hl
        ld e,a
        pop af
        push af
        scf
        call SyFile_FILCPR      ;load bitmap data
        jr c,bmplode
        jr nz,bmplode
        pop af
        call SyFile_FILCLO      ;close data file

        ld a,(bmplodl+0)        ;relocate bitmap headers
        srl a
        ld b,a
        ld a,(bmpdatb)
        ld hl,bmpdato
bmplod1 push bc
        ld e,(hl):inc hl
        ld d,(hl):dec hl
        ex de,hl
        ld bc,(bmpdata)
        add hl,bc
        ex de,hl
        ld (hl),e:inc hl
        ld (hl),d:inc hl
        push hl
        ex de,hl
        inc hl:inc hl:inc hl
        call bmplod2
        call bmplod2
        pop hl
        pop bc
        djnz bmplod1
        ret

bmplod2 rst #20:dw jmp_bnkrwd
        ex de,hl
        ld hl,(bmpdata)
        add hl,bc
        ld c,l
        ld b,h
        ex de,hl
        dec hl:dec hl
        rst #20:dw jmp_bnkwwd
        ret

bmplode pop af
        call SyFile_FILCLO
bmplod0 ;...show error
        jp prgend

;### BMPRAC -> copies and prepares lemming race bitmaps
bmpracb
dw winsavdat_lem+00
dw winhlpdat1a+00, winhlpdat1a+16, winhlpdat1a+32, winhlpdat1b+00, winhlpdat1c+00
dw winhlpdat2a+00, winhlpdat2b+00, winhlpdat2c+00, winhlpdat2d+00
dw winhlpdat3a+00, winhlpdat3b+00
dw winhlpdat4a+00, winhlpdat4b+00, winhlpdat4b+16, winhlpdat4b+32, winhlpdat4c+00, winhlpdat4c+16, winhlpdat4c+32
dw winhlpdat5a+00
dw wingamdat_prgrs1a+00
dw 0

bmprac  ld bc,bmplemend-bmplembeg+bmplemend-bmplembeg       ;2x all lemming figure bitmaps

        ld (bmpleml),bc
        xor a
        ld e,1
        rst #20:dw jmp_memget   ;reserve figure memory
        ;...show error
        jp c,prgend
        ld (bmplemb),a          ;register/store figure memory
        push af
        ld (bmplema),hl

        push hl
        push af
        ld de,bmplembeg
        or a
        sbc hl,de
        ld (bmprac7+1),hl
        ex de,hl
        call bmprac4            ;relocate 1st race bmp header pointer
        pop af
        pop de

        add a:add a:add a:add a
        ld hl,App_BnkNum
        add (hl)
        ld hl,bmplembeg
        ld bc,bmplemend-bmplembeg

        push hl     ;src begin
        push de     ;dst begin

        push hl
        push bc
        push de
        push af
        rst #20:dw jmp_bnkcop   ;copy first  race to memory

        ld hl,bmplembeg         ;modify bitmaps for second race
bmprac9 ld a,(hl)
        or a
        jr z,bmpraca
        ld bc,7
        add hl,bc
        ld c,(hl):inc hl
        ld b,(hl):inc hl
        inc hl
bmprace ld e,(hl)
        call bmpracc
        call bmpracc
        ld (hl),e
        inc hl
        dec bc
        ld a,c
        or b
        jr nz,bmprace
        jr bmprac9

bmpracc ld a,e
        and #0f
        cp  8:ld d,14:jr z,bmpracd
        cp 10:ld d, 3:jr z,bmpracd
        ld a,e
        and #0f
        ld d,a
bmpracd ld a,e
        and #f0
        or d
        rrca:rrca:rrca:rrca
        ld e,a
        ret

bmpraca ld de,bmplemend-bmplembeg
        call bmprac4            ;relocate 2nd race bmp header pointer

        pop af
        pop hl
        pop bc
        add hl,bc
        ex de,hl
        pop hl
        rst #20:dw jmp_bnkcop   ;copy second race to memory

        pop hl
        pop bc

        or a                    ;relocate animation pointers
        sbc hl,bc
        ex de,hl    ;de=dst-src
        ld ix,lemanibeg
        ld iyl,0
        ld bc,bmplemend-bmplembeg
bmprac1 ld l,(ix+0)
        ld h,(ix+1)
        ld a,h
        cp 6                    
        jr c,bmprac2            ;highbyte <6 -> parameter, just copy this to 2nd race
        add hl,de
        ld (ix+0),l
        ld (ix+1),h             ;set 1st race
        add hl,bc
bmprac2 inc ixh:inc ixh
        ld (ix+0),l
        ld (ix+1),h             ;set 2nd race
        dec ixh:dec ixh
        inc ix:inc ix
        dec iyl
        jr nz,bmprac1

        ld b,LEM_MAX
        ld hl,wingamdat_lem+16+3
        pop af
        ld de,32
bmprac3 ld (hl),a
        add hl,de
        djnz bmprac3
        ld (bmprac8+3),a

        ld (wingamdat_prgrs2a+3),a
        ld (winsavdat_lem+16+3),a
        ld hl,(lemanirgt+4+512)
        ld (wingamdat_prgrs2a+4),hl
        ld (winsavdat_lem+16+4),hl

        ld de,bmpracb
bmprac7 ld bc,0
bmprac6 ld a,(de):ld ixl,a:inc de
        ld a,(de):ld ixh,a:inc de
        or a
        ret z
bmprac8 ld (ix+3),0
        ld l,(ix+4)
        ld h,(ix+5)
        add hl,bc
        ld (ix+4),l
        ld (ix+5),h
        jr bmprac6

;de=dif -> relocate bmp header pointer
bmprac4 ld ix,bmplembeg
bmprac5 ld a,(ix+0)
        or a
        ret z
        ld l,(ix+3)
        ld h,(ix+4)
        add hl,de
        ld (ix+3),l
        ld (ix+4),h
        dec hl
        ld (ix+5),l
        ld (ix+6),h
        ld c,(ix+7)
        ld b,(ix+8)
        add ix,bc
        ld bc,10
        add ix,bc
        jr bmprac5


;==============================================================================
;### SOUND ROUTINES ###########################################################
;==============================================================================

VOL_GAME            equ 192
VOL_DIALOGUE        equ 64
VOL_DEMO            equ 128

SND_DIE_CRUSH       equ 0
SND_DIE_EXPLODE     equ 1
SND_DIE_OHNO        equ 2
SND_DIE_POP         equ 3
SND_DIE_SCREAM      equ 4
SND_DIE_SPLASH      equ 5
SND_DIE_WATER       equ 6
SND_DOOR_GOAL       equ 7
SND_DOOR_KEY        equ 8
SND_DOOR_START      equ 9
SND_DOOR_UNLOCK     equ 10
SND_TOOL_BRIDGE     equ 11
SND_TOOL_DIG        equ 12
SND_TOOL_PARACHUTE  equ 13
SND_TRAP_ELECTRIC   equ 14
SND_TRAP_FIRE       equ 15
SND_TRAP_GHOST      equ 16
SND_TRAP_ROCKBEG    equ 17
SND_TRAP_ROCKEND    equ 18
SND_TRAP_SAW        equ 19


snddvcflg   db 0    ;0=no hardware, 1=use psg, 2=use opl4
sndhndefx   db -1   ;effect handler
sndhndmus   db -1   ;music handler

snddathnd   db 0
snddatbuf   ds 4

;### SNDINI -> inits sound module and loads music and effects
sndini  call SySound_SNDINI         ;search and set Sound Daemon
        ret c
        ld a,l
        ld (snddvcflg),a
        ld hl,sndnam
        call cfglod0
        ld hl,(datpth)
        ld ix,(App_BnkNum-1)
        call SyFile_FILOPN
        ret c
        ld (snddathnd),a
        ld hl,(snddvcflg)
        dec l
        ld ix,4
        ld iy,0
        ld b,a
        ld a,1
        jr z,sndini1
        ld a,b
        ld hl,snddatbuf
        ld bc,4
        ld de,(App_BnkNum)
        push af
        call SyFile_FILINP
        pop bc
        jr c,sndini2
        ld ix,(snddatbuf+0)
        ld iy,(snddatbuf+2)
        ld a,128
sndini1 ld (sndefx0+1),a
        ld a,b
        ld c,0
        call SyFile_FILPOI
        jr c,sndini2
        ld a,(snddvcflg)
        ld d,a
        ld e,0
        ld a,(snddathnd)
        push af
        push de
        call SySound_EFXLOD
        pop de
        pop bc
        jr c,sndini2
        ld (sndhndefx),a
        ld a,b
        call SySound_MUSLOD
        jr c,sndini2
        ld (sndhndmus),a
        ld h,192
        call sndvol     ;##!!## remove
sndini2 ld a,(snddathnd)
        jp SyFile_FILCLO

;### SNDFRE -> removes music and effects
sndfre  ld a,(sndhndefx)
        inc a
        jr z,sndfre1
        dec a
        call SySound_EFXFRE
sndfre1 ld a,(sndhndmus)
        inc a
        ret z
        dec a
        jp SySound_MUSFRE

;### SNDVOL -> sets music volume
;### Input      H=volume
sndvol  ld a,(sndhndmus)
        jp SySound_MUSVOL

;### SNDMUS -> starts or stops music
;### Input      L=subsong ID (255=stop)
;### Destroyed  AF,BC,DE,HL,IX,IY
sndmus  ld a,(sndhndmus)
        inc a
        ret z
        dec a
        inc l
        jp z,SySound_MUSSTP
        dec l
        jp SySound_MUSRST

;### SNDEFX -> play effect
;### Input      L=effect ID, H=priority (1=high, 2=low)
;### Destroyed  AF,BC,DE,HL
sndefx  ld a,(sndhndefx)
        inc a
        ret z
        dec a
        ld b,h
        ld h,255
sndefx0 ld c,2
        ld de,0
        push ix
        push iy
        call SySound_EFXPLY
        pop iy
        pop ix
        ret


;==============================================================================
;### SUB ROUTINES #############################################################
;==============================================================================

winidp  dw 0

;### WINDLY -> opens a modal window delayed
;### Input      DE=window data record, HL=id storage, B=delay in 1/50s
windly  push de
        push hl
windly1 push bc
        call gamlop
        rst #30
        pop bc
        djnz windly1
        pop hl
        pop de
        call winopn
        ;...clear desktop manager messages ##!!##
        ret

;### WINOPN -> opens a modal window
;### Input      DE=window data record, HL=id storage
winopn  ld bc,256*54+58
winopn1 push hl
        push de
        inc de:inc de:inc de:inc de
        ld hl,(wingam+4)
        ld a,b
        ld b,0
        add hl,bc
        ex de,hl
        ld (hl),e:inc hl
        ld (hl),d:inc hl
        ex de,hl
        ld hl,(wingam+6)
        ld c,a
        add hl,bc
        ex de,hl
        ld (hl),e:inc hl
        ld (hl),d
        pop de
        pop hl
winopn0 push hl
        ld a,(App_BnkNum)
        call SyDesktop_WINOPN
        jp c,prgend
        pop hl
        ld (winidp),hl
        ld (hl),a
        inc a
        ld (wingam+51),a
        ret

;### WINCLS -> closes a modal window
wincls  ld hl,(winidp)
        ld a,(hl)
        or a
        jp z,prgprz0
        ld (hl),0
        call SyDesktop_WINCLS
        xor a
        ld (wingam+51),a
        ret

;### CLCUCS -> upper case
;### Input      A=char
;### Output     A=ucase(A)
;### Destroyed  F
clcucs  cp "a"
        ret c
        cp "z"+1
        ret nc
        add "A"-"a"
        ret

;### CLCDEC -> calculates 2digit ascii number
;### Input      A=value
;### Output     L=10er-Ascii, H=1er-Ascii
;### Destroyed  AF
clcdec  ld l,0
clcdec1 sub 10
        jr c,clcdec2
        inc l
        jr clcdec1
clcdec2 add "0"+10
        ld h,a
        ld a,"0"
        add l
        ld l,a
        ret

;### CLCMU8 -> 8bit unsigned multiplication
;### Input      L,H=values
;### Output     HL=L*H
;### Destroyed  F,B,DE
clcmu8  ld e,l
        ld l,0
        ld d,0
        ld b,8
clcmu81 add hl,hl
        jr nc,clcmu82
        add hl,de
clcmu82 djnz clcmu81
        ret

;### CLCDIV -> division
;### Output     L=HL/C, H=HL mod C
;### Destroyed  AF,BC
clcdiv  ld b,8
clcdiv1 add hl,hl       ;3
        ld a,h          ;1
        sub c           ;1
        jr c,clcdiv2    ;2/3
        ld h,a          ;1
        inc l           ;1 9/8
clcdiv2 djnz clcdiv1
        ret

;### CLCRIN -> initialize random number generator seed
clcrin  rst #20:dw #810c    ;a=second, b=minute, c=hour
        ld h,a
        ld a,r
        and 127
        ld l,a
        ld (clcrnd0+1),hl
        ld (clcrnd1+1),bc
        set 7,l
        ld (clcrnd2+1),bc
        ld (clcrnd3+1),hl
        ret

;### CLCRND -> generates random number
;### Input      (clcrnd0+1),(clcrnd1+1)=seed 1,(clcrnd2+1),(clcrnd2+1)=seed 2 (>0)
;### Output     HL=random number (0-65535)
;### Destroyed  AF,BC,DE
clcrnd
clcrnd0 ld hl,0
clcrnd1 ld de,-1
        ld b,h
        ld c,l
        add hl,hl:rl e:rl d
        add hl,hl:rl e:rl d
        inc l
        add hl,bc
        ld (clcrnd0+1),hl
        ld hl,(clcrnd1+1)
        adc hl,de
        ld (clcrnd1+1),hl
        ex de,hl
clcrnd2 ld hl,0
clcrnd3 ld bc,-1
        add hl,hl:rl c:rl b
        ld (clcrnd3+1),bc
        sbc a,a
        and %11000101
        xor l
        ld l,a
        ld (clcrnd2+1),hl
        ex de,hl
        add hl,bc
        ret


;==============================================================================
;### 16COLOUR VERSIONS ########################################################
;==============================================================================

bmp_16c

dw gmp_stop_mark+10,6*12
db #17,#17,#17,#17,#17,#17, #71,#11,#11,#11,#11,#11, #11,#11,#1F,#FF,#11,#11, #71,#11,#F1,#11,#F1,#11, #11,#1F,#11,#11,#1F,#11, #71,#F1,#11,#11,#11,#F1
db #11,#F1,#FF,#FF,#F1,#F1, #71,#F1,#11,#11,#11,#F1, #11,#1F,#11,#11,#1F,#11, #71,#11,#F1,#11,#F1,#11, #11,#11,#1F,#FF,#11,#11, #71,#11,#11,#11,#11,#11

dw gmp_brdg_rh_s+10,6*12
db #17,#17,#17,#17,#17,#17, #71,#11,#FF,#FF,#11,#11, #11,#1F,#11,#11,#F1,#11, #71,#F1,#11,#11,#1F,#11, #1F,#11,#11,#11,#11,#F1, #7F,#1F,#FF,#FF,#F1,#F1
db #1F,#1F,#FF,#FF,#F1,#F1, #7F,#11,#16,#66,#11,#F1, #11,#F1,#FF,#F1,#1F,#11, #71,#1F,#66,#11,#F1,#11, #11,#FF,#FF,#FF,#11,#11, #76,#66,#11,#11,#11,#11
dw gmp_brdg_rf_s+10,6*12
db #17,#17,#17,#17,#17,#17, #71,#11,#FF,#FF,#11,#16, #11,#1F,#11,#11,#F1,#FF, #71,#F1,#11,#11,#1F,#66, #1F,#11,#11,#11,#FF,#F1, #7F,#1F,#FF,#FF,#F6,#F1
db #1F,#1F,#FF,#FF,#F1,#F1, #7F,#11,#16,#66,#11,#F1, #11,#F1,#FF,#F1,#1F,#11, #71,#1F,#66,#11,#F1,#11, #11,#FF,#FF,#FF,#11,#11, #76,#66,#11,#11,#11,#11
dw gmp_brdg_rf_x+10,6*12
db #17,#17,#17,#17,#17,#17, #71,#11,#F1,#11,#11,#16, #11,#F1,#11,#11,#11,#FF, #71,#11,#F1,#11,#16,#66, #1F,#1F,#FF,#1F,#FF,#F1, #71,#11,#FF,#FF,#FF,#11
db #11,#11,#1F,#FF,#FF,#F1, #71,#11,#FF,#FF,#FF,#F1, #11,#11,#FF,#FF,#FF,#F1, #71,#16,#6F,#FF,#FF,#11, #11,#FF,#F1,#FF,#F1,#11, #76,#66,#11,#11,#11,#11
dw gmp_brdg_lh_s+10,6*12
db #17,#17,#17,#17,#17,#17, #71,#11,#FF,#FF,#11,#11, #11,#1F,#11,#11,#F1,#11, #71,#F1,#11,#11,#1F,#11, #1F,#11,#11,#11,#11,#F1, #7F,#1F,#FF,#FF,#F1,#F1
db #1F,#1F,#FF,#FF,#F1,#F1, #7F,#11,#66,#61,#11,#F1, #11,#F1,#1F,#FF,#1F,#11, #71,#1F,#11,#66,#F1,#11, #11,#11,#FF,#FF,#FF,#11, #71,#11,#11,#11,#66,#61
dw gmp_brdg_lf_s+10,6*12
db #17,#17,#17,#17,#17,#17, #61,#11,#FF,#FF,#11,#11, #FF,#1F,#11,#11,#F1,#11, #66,#F1,#11,#11,#1F,#11, #1F,#FF,#11,#11,#11,#F1, #7F,#6F,#FF,#FF,#F1,#F1
db #1F,#1F,#FF,#FF,#F1,#F1, #7F,#11,#66,#61,#11,#F1, #11,#F1,#1F,#FF,#1F,#11, #71,#1F,#11,#66,#F1,#11, #11,#11,#FF,#FF,#FF,#11, #71,#11,#11,#11,#66,#61
dw gmp_brdg_lf_x+10,6*12
db #17,#17,#17,#17,#17,#17, #61,#11,#F1,#11,#11,#11, #FF,#F1,#11,#11,#11,#11, #66,#61,#F1,#11,#11,#11, #1F,#FF,#FF,#1F,#F1,#11, #71,#66,#FF,#FF,#FF,#11
db #11,#1F,#FF,#FF,#FF,#F1, #71,#11,#FF,#FF,#FF,#F1, #11,#11,#FF,#FF,#FF,#F1, #71,#11,#1F,#FF,#FF,#11, #11,#11,#11,#FF,#FF,#11, #71,#11,#11,#11,#66,#61

dw gsl_stop+10,8*14
db #88,#88,#88,#88,#88,#88,#88,#11, #61,#11,#11,#11,#11,#11,#18,#11, #61,#11,#1F,#FF,#F1,#11,#18,#11, #61,#11,#F1,#11,#1F,#11,#18,#11, #61,#1F,#11,#11,#11,#F1,#18,#11, #61,#F1,#11,#11,#11,#1F,#18,#11, #61,#F1,#FF,#FF,#FF,#1F,#18,#11
db #61,#F1,#FF,#FF,#FF,#1F,#18,#11, #61,#F1,#11,#11,#11,#1F,#18,#11, #61,#1F,#11,#11,#11,#F1,#18,#11, #61,#11,#F1,#11,#1F,#11,#18,#11, #61,#11,#1F,#FF,#F1,#11,#18,#11, #61,#11,#11,#11,#11,#11,#18,#11, #66,#66,#66,#66,#66,#66,#66,#11
dw gsl_suic+10,8*14
db #88,#88,#88,#88,#88,#88,#88,#11, #61,#11,#11,#11,#11,#11,#18,#11, #61,#11,#1F,#11,#11,#11,#18,#11, #61,#1F,#11,#11,#11,#11,#18,#11, #61,#11,#1F,#11,#11,#11,#18,#11, #61,#F1,#FF,#F1,#FF,#11,#18,#11, #61,#11,#1F,#FF,#FF,#F1,#18,#11
db #61,#11,#11,#FF,#FF,#FF,#18,#11, #61,#11,#1F,#FF,#FF,#FF,#18,#11, #61,#11,#1F,#FF,#FF,#FF,#18,#11, #61,#11,#11,#FF,#FF,#F1,#18,#11, #61,#11,#11,#1F,#FF,#11,#18,#11, #61,#11,#11,#11,#11,#11,#18,#11, #66,#66,#66,#66,#66,#66,#66,#11

dw ui_prgr_fields1,6*12
db #AA,#AA,#AA,#AA,#AA,#A8, #AA,#AA,#AA,#AA,#AA,#A8, #AA,#AA,#AA,#AA,#AA,#A8, #AA,#AA,#AA,#AA,#AA,#A8, #AA,#AA,#AA,#AA,#AA,#A8, #AA,#AA,#AA,#AA,#AA,#A8
db #22,#22,#22,#22,#22,#28, #22,#22,#22,#22,#22,#28, #22,#22,#22,#22,#22,#28, #22,#22,#22,#22,#22,#28, #22,#22,#22,#22,#22,#28, #22,#22,#22,#22,#22,#28

dw 0


read "App-Lymings-Sprites.asm"


;==============================================================================
;### PLANET/LEVEL DATA ########################################################
;==============================================================================

plnum   db 0        ;total number of planets
pltab   db 0        ;*** last label in code area!!! ***


;==============================================================================
;### DATA AREA ################################################################
;==============================================================================

App_BegData

;### 256 BYTE ALIGNED DATA

;field bitmap table
gmp_table
dw 0
;solid
dw gmp_grnd,gmp_grnd_brk1,gmp_grnd_brk2,gmp_grnd_brk3,gmp_grqm,gmp_grqm_brk1,gmp_grqm_brk2,gmp_grqm_brk3,gmp_grnd_mark,gmp_grnd_br1m,gmp_grnd_br2m,gmp_grnd_br3m,gmp_grqm_mark,gmp_grqm_br1m,gmp_grqm_br2m,gmp_grqm_br3m
dw gmp_wall,gmp_trap_rock
dw gmp_entrance,gmp_exit_b,gmp_exit_b1,gmp_exit_b2,gmp_excl_b,gmp_exit_0,gmp_exit_1,gmp_exit_2,gmp_exit_3,gmp_exit_4,gmp_exit_5,gmp_exit_6,gmp_exit_7,gmp_exit_8,gmp_exit_9
dw gmp_watr1,gmp_watr2,gmp_watr3
dw gmp_bord_l,gmp_bord_r,gmp_bord_llnk,gmp_bord_rlnk
dw gmp_trap_ele1,gmp_trap_ele2,gmp_trap_ele3,gmp_trap_ghs1,gmp_trap_ghs2,gmp_trap_ghs3,gmp_trap_firl,gmp_trap_firr,gmp_trap_firu,gmp_trap_saw,gmp_trap_mine
dw gmp_door_lu,gmp_door_ll,gmp_door_ru,gmp_door_rl

ds 128-$+gmp_table
;open
dw gmp_empt,gmp_exit_t,0,0,gmp_excl_t,gmp_excr_t
dw gmp_brdg_rf,gmp_brdg_rh,gmp_brdg_rf_s,gmp_brdg_rh_s,gmp_brdg_rf_x,gmp_brdg_rmrk, gmp_brdg_lf,gmp_brdg_lh,gmp_brdg_lf_s,gmp_brdg_lh_s,gmp_brdg_lf_x,gmp_brdg_lmrk
dw gmp_para_used,gmp_para_mark,gmp_stop_mark,gmp_stop_mark

ds 256-$+gmp_table  ;256

;field middle event jumps
fldmidjmp
dw midn  ,midgol,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midstp,midstp,midn  ,midbbr,midn  ,midn  ,midstp,midstp  ; 64- 79
dw midn  ,midbbl,midn  ,midn  ,midn  ,midstp,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn    ; 80- 95
dw midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn    ; 96-111
dw midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn  ,midn    ;112-127    128

;field below  event jumps
fldlowjmp
dw lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lowdig,lowdig,lowdig,lowdig,lowdig,lowdig,lowdig  ;  0- 15
dw lowdig,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown    ; 16- 31
dw lown  ,lown  ,lowdro,lowdro,lowdro,lown  ,lown  ,lown  ,lown  ,lowele,lowele,lowele,lown  ,lown  ,lown  ,lown    ; 32- 47
dw lown  ,lown  ,lown  ,lowmin,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown  ,lown    ; 48- 63    128

;### FIGURE ANIMATION TABLES
lemanibeg

;right/left animation   8 x [bitmap adr, pixel ofs]
lemanirgt   dw lmawr0:db 0,0: dw lmawr1:db 0,0: dw lmawr2:db 0,0: dw lmawr3:db 1,0: dw lmawr4:db 2,0: dw lmawr5:db 2,0: dw lmawr6:db 2,0: dw lmawr7:db 3,0 ;32
lemanilft   dw lmawl0:db 0,3: dw lmawl1:db 0,4: dw lmawl2:db 0,5: dw lmawl3:db 1,4: dw lmawl4:db 2,3: dw lmawl5:db 2,4: dw lmawl6:db 2,5: dw lmawl7:db 3,4 ;32 64
;stopper    animation  16 x [bitmap adr]
lemanistp   dw lmast0,lmast0,lmast1,lmast1,lmast2,lmast2,lmast3,lmast3,lmast0,lmast0,lmast5,lmast5,lmast6,lmast6,lmast7,lmast7              ;32 96
;drowning   animation  21 x [bitmap adr, xofs]
lemanidro   dw lmado0,0,lmado0,0,lmado1,1,lmado1,1,lmado2,2,lmado2,2,lmado3,3,lmado3,3,lmado4,3,lmado4,3
            dw lmado5,3,lmado5,3,lmado4,3,lmado4,3,lmado7,4,lmado7,4,lmado8,5,lmado8,5,lmado9,5,lmado9,5,lmadoa,6   ;84 180
;goal rchd  animation  10 x [bitmap adr]
lemanigol   dw lmagl0,lmagl0,lmagl1,lmagl1,lmagl2,lmagl2,lmagl3,lmagl3,lmagl4,lmagl4                                ;20 200
;falling    animation   8 x [bitmap adr]
lemanifal   dw lmaff0,lmaff1,lmaff2,lmaff3,lmaff4,lmaff5,lmaff6,lmaff7                                              ;16 216
;splashing  animation   9 x [bitmap adr, yofs]
lemanispl   dw lmasp0,0,lmasp1,0,lmasp2,2,lmasp3,3,lmasp4,0,lmasp5,0,lmasp6,1,lmasp7,4,lmadoa,6                     ;36 252

ds 256-252  ;256

;dig right  animation  16 x [bitmap adr]
lemanidrg   dw lmadr0,lmadr1,lmadr2,lmadr3,lmadr4,lmadr5,lmadr6,lmadr7,lmadr0,lmadr1,lmadr2,lmadr3,lmadr4,lmadr5,lmadr6,lmadr7  ;32 032
;dig left   animation  16 x [bitmap adr]
lemanidlf   dw lmadl0,lmadl1,lmadl2,lmadl3,lmadl4,lmadl5,lmadl6,lmadl7,lmadl0,lmadl1,lmadl2,lmadl3,lmadl4,lmadl5,lmadl6,lmadl7  ;32 064
;dig down   animation  16 x [bitmap adr]
lemaniddw   dw lmadd0,lmadd1,lmadd2,lmadd3,lmadd4,lmadd5,lmadd6,lmadd7,lmadd8,lmadd9,lmadda,lmaddb,lmaddc,lmaddd,lmadde,lmaddf  ;32 096
;parachute  animation   8 x [bitmap adr]
lemanipar   dw lmafp0,lmafp0,lmafp1,lmafp1,lmafp2,lmafp2,lmafp3,lmafp3                                              ;16 112

;explosion  animation  12 x [bitmap adr, yofs]
lemaniexp   dw lmaxp0,0,lmaxp1,0,lmaxp2,0,lmaxp3,0,lmaxp4,0,lmaxp5,0,lmaxp6,4,lmaxp7,9,lmaxp7,9,lmaxp7,9,lmaxp7,9,lmaxp7,9  ;48 160
;bbld right animation   8 x [bitmap adr]
lemanibbr   dw lmabr0,lmabr0,lmabr1,lmabr2,lmabr3,lmabr4,lmabr5,lmabr6                                              ;16 176
;bbld left  animation   8 x [bitmap adr]
lemanibbl   dw lmabl0,lmabl0,lmabl1,lmabl2,lmabl3,lmabl4,lmabl5,lmabl6                                              ;16 192
;dig finish animation   8 x [bitmap adr]
lemanidfi   dw lmadf0,lmadf1,lmadf1,lmadf0                                                                          ; 8 200
;expbegins  animation  16 x [bitmap adr]
lemaniexb   dw lmaxb0,lmaxb0,lmaxb1,lmaxb1,lmaxb2,lmaxb2,lmaxb3,lmaxb3,lmaxb4,lmaxb4,lmaxb5,lmaxb5,lmaxb4,lmaxb4,lmaxb7,lmaxb7  ;32 232
;smashing   animation  12 x [bitmap adr]
lemanisma   dw lmaxs0,lmaxs0,lmaxs1,lmaxs1,lmaxs2,lmaxs2,lmaxs3,lmaxs3,lmaxs4,lmaxs4,lmaxs5,lmaxs5                  ;24 256

;ds 256-240  ;256

ds 512      ;anitabs for 2nd race

lemanifal0  dw lmaff0a,lmaff1a,lmaff2a,lmaff3a,lmaff4a,lmaff5a,lmaff6a,lmaff7a                                      ;16
lemanifal1  dw lmaff0b,lmaff1b,lmaff2b,lmaff3b,lmaff4b,lmaff5b,lmaff6b,lmaff7b                                      ;16


read "App-Lymings-Bitmaps.asm"


;==============================================================================
;### TRANSFER AREA ############################################################
;==============================================================================

App_BegTrns

prgicn16c db 12,24,24:dw $+7:dw $+4,12*24:db 5
db #17,#17,#17,#17,#17,#17,#17,#17,#17,#17,#17,#17,#71,#11,#11,#11,#11,#11,#71,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#17,#71,#11,#11,#11,#11,#11,#71,#11,#11,#11,#11,#11
db #11,#A1,#A1,#11,#11,#1A,#AA,#11,#11,#11,#11,#17,#71,#AA,#A1,#11,#11,#AA,#81,#11,#11,#11,#11,#11,#11,#1A,#81,#11,#11,#18,#88,#11,#11,#11,#11,#17,#71,#18,#88,#11,#11,#18,#71,#11,#11,#11,#11,#11
db #11,#88,#71,#11,#11,#87,#71,#11,#11,#11,#11,#17,#71,#87,#71,#11,#11,#87,#71,#81,#11,#11,#11,#11,#11,#77,#77,#11,#11,#77,#18,#11,#11,#11,#11,#17,#78,#81,#18,#81,#11,#88,#11,#11,#11,#11,#11,#11
db #10,#00,#00,#00,#00,#01,#31,#14,#41,#14,#41,#13,#33,#33,#33,#33,#33,#30,#34,#47,#44,#44,#44,#43,#33,#33,#33,#33,#33,#33,#34,#44,#44,#44,#44,#43,#33,#22,#23,#22,#23,#32,#34,#44,#44,#74,#44,#43
db #33,#22,#32,#32,#23,#33,#34,#44,#44,#44,#47,#43,#33,#23,#22,#23,#23,#32,#34,#47,#44,#44,#44,#43,#33,#32,#22,#22,#33,#33,#34,#44,#44,#44,#44,#43,#33,#23,#22,#23,#23,#32,#34,#44,#44,#74,#44,#43
db #33,#22,#32,#32,#23,#33,#34,#44,#44,#44,#47,#43,#33,#22,#23,#22,#23,#32,#34,#47,#44,#44,#44,#43,#33,#33,#33,#33,#33,#33,#33,#44,#44,#44,#44,#33,#13,#33,#33,#33,#33,#31,#33,#33,#33,#33,#33,#33

;### PRGPRZS -> Stack for application process
        ds 128
prgstk  ds 6*2
        dw prgprz
App_PrcID  db 0
App_MsgBuf ds 14

;### COMPUTER TYPE ############################################################

cfghrdtyp   db 0    ;bit[0-4] Computer type     0=464, 1=664, 2=6128, 3=464Plus, 4=6128Plus,
                    ;                               5=*reserved*
                    ;                           6=Enterprise 64/128,
                    ;                           7=MSX1, 8=MSX2, 9=MSX2+, 10=MSX TurboR,
                    ;                               11=*reserved*
                    ;                           12=PCW8xxx, 13=PCW9xxx
                    ;                           14=PcW16
                    ;                           15=NC100, 16=NC150, 17=NC200
                    ;                               18-31=*reserved*
                    ;bit[5-6] BackGrGfxSize     0=320x200x4, 1=512x212x4/16, 2=256x192x2, 3=specific
                    ;bit[7]   BitmapEncoding    0=CPC type (CPC,PCW,EP), 1=MSX type (MSX)


;### FONT MESSAGE DISPLAY #####################################################
fntmsg
db 9,48
db 7,%01111000,%11111100,%11001100,%11001100,%11001100,%11001100,%11111100,%01111000,%00000000,0,0,0,0,0,0   ;0
db 6,%00111000,%01111000,%11111000,%11011000,%00011000,%00011000,%00011000,%00011000,%00000000,0,0,0,0,0,0   ;1
db 7,%01111000,%11111100,%11001100,%00011100,%00111000,%01110000,%11111100,%11111100,%00000000,0,0,0,0,0,0   ;2
db 7,%11111000,%11111100,%00011100,%01111000,%01111000,%00011100,%11111100,%11111000,%00000000,0,0,0,0,0,0   ;3
db 7,%11000000,%11001100,%11001100,%11111100,%11111100,%00001100,%00001100,%00001100,%00000000,0,0,0,0,0,0   ;4
db 7,%11111100,%11111100,%11000000,%11111000,%01111100,%00001100,%11111100,%11111000,%00000000,0,0,0,0,0,0   ;5
db 7,%01111100,%11111100,%11000000,%11111000,%11111100,%11001100,%11111100,%01111000,%00000000,0,0,0,0,0,0   ;6
db 7,%11111100,%11111100,%00001100,%00011100,%00011000,%00111000,%00110000,%00110000,%00000000,0,0,0,0,0,0   ;7
db 7,%01111000,%11111100,%11001100,%01111000,%11111100,%11001100,%11111100,%01111000,%00000000,0,0,0,0,0,0   ;8
db 7,%01111000,%11111100,%11001100,%11111100,%01111100,%00001100,%11111100,%11111000,%00000000,0,0,0,0,0,0   ;9
ds 16
db 3,%11100000,%11100000,%11100000,%11100000,%11000000,%00000000,%11000000,%11000000,%00000000,0,0,0,0,0,0   ;! (;)
db 3:ds 15                                                                                                   ;  (<)
db 3,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%11000000,%11000000,%00000000,0,0,0,0,0,0   ;. (=)
db 7,%11111000,%11111100,%00001100,%00111100,%00111000,%00000000,%00110000,%00110000,%00000000,0,0,0,0,0,0   ;? (>)
ds 16*2
db 8,%00111000,%00111000,%01101100,%01101100,%01111100,%11111110,%11000110,%11000110,%00000000,0,0,0,0,0,0   ;A
db 7,%11111000,%11111100,%11001100,%11111000,%11111100,%11001100,%11111100,%11111000,%00000000,0,0,0,0,0,0   ;B
db 7,%01111000,%11111100,%11001100,%11000000,%11000000,%11001100,%11111100,%01111000,%00000000,0,0,0,0,0,0   ;C
db 7,%11111000,%11111100,%11001100,%11001100,%11001100,%11001100,%11111100,%11111000,%00000000,0,0,0,0,0,0   ;D
db 6,%11111000,%11111000,%11000000,%11110000,%11110000,%11000000,%11111000,%11111000,%00000000,0,0,0,0,0,0   ;E
db 6,%11111000,%11111000,%11000000,%11110000,%11110000,%11000000,%11000000,%11000000,%00000000,0,0,0,0,0,0   ;F
db 7,%01111100,%11111100,%11000000,%11011100,%11011100,%11001100,%11111100,%01111100,%00000000,0,0,0,0,0,0   ;G
db 7,%11001100,%11001100,%11001100,%11111100,%11111100,%11001100,%11001100,%11001100,%00000000,0,0,0,0,0,0   ;H
db 3,%11000000,%11000000,%11000000,%11000000,%11000000,%11000000,%11000000,%11000000,%00000000,0,0,0,0,0,0   ;I
db 6,%11111000,%11111000,%00011000,%00011000,%00011000,%00011000,%11111000,%11110000,%00000000,0,0,0,0,0,0   ;J
db 7,%11001100,%11001100,%11011100,%11111000,%11111000,%11011100,%11001100,%11001100,%00000000,0,0,0,0,0,0   ;K
db 6,%11000000,%11000000,%11000000,%11000000,%11000000,%11000000,%11111000,%11111000,%00000000,0,0,0,0,0,0   ;L
db 8,%11000110,%11101110,%11111110,%11111110,%11010110,%11000110,%11000110,%11000110,%00000000,0,0,0,0,0,0   ;M
db 7,%11001100,%11001100,%11101100,%11111100,%11111100,%11011100,%11001100,%11001100,%00000000,0,0,0,0,0,0   ;N
db 7,%01111000,%11111100,%11001100,%11001100,%11001100,%11001100,%11111100,%01111000,%00000000,0,0,0,0,0,0   ;O
db 7,%11111000,%11111100,%11001100,%11001100,%11111100,%11111000,%11000000,%11000000,%00000000,0,0,0,0,0,0   ;P
db 7,%01111000,%11111100,%11001100,%11001100,%11001100,%11011000,%11111100,%01101100,%00000000,0,0,0,0,0,0   ;Q
db 7,%11111000,%11111100,%11001100,%11001100,%11111000,%11111100,%11001100,%11001100,%00000000,0,0,0,0,0,0   ;R
db 7,%01111100,%11111100,%11000000,%11111000,%01111100,%00001100,%11111100,%11111000,%00000000,0,0,0,0,0,0   ;S
db 7,%11111100,%11111100,%00110000,%00110000,%00110000,%00110000,%00110000,%00110000,%00000000,0,0,0,0,0,0   ;T
db 7,%11001100,%11001100,%11001100,%11001100,%11001100,%11001100,%11111100,%01111000,%00000000,0,0,0,0,0,0   ;U
db 8,%11000110,%11000110,%11101110,%01101100,%01111100,%00111000,%00111000,%00010000,%00000000,0,0,0,0,0,0   ;V
db 8,%11000110,%11000110,%11000110,%11010110,%11111110,%11111110,%11101110,%11000110,%00000000,0,0,0,0,0,0   ;W
db 8,%11000110,%11101110,%01111100,%00111000,%00111000,%01111100,%11101110,%11000110,%00000000,0,0,0,0,0,0   ;X
db 7,%11001100,%11001100,%11001100,%11111100,%01111000,%00110000,%00110000,%00110000,%00000000,0,0,0,0,0,0   ;Y
db 7,%11111100,%11111100,%00011100,%00111000,%01110000,%11100000,%11111100,%11111100,%00000000,0,0,0,0,0,0   ;Z
ds 16*6
db 6,%00000000,%11110000,%11111000,%00011000,%11111000,%11011000,%11111000,%01111000,%00000000,0,0,0,0,0,0   ;a
db 6,%11000000,%11110000,%11111000,%11011000,%11011000,%11011000,%11111000,%11110000,%00000000,0,0,0,0,0,0   ;b
db 6,%00000000,%01110000,%11111000,%11011000,%11000000,%11011000,%11111000,%01110000,%00000000,0,0,0,0,0,0   ;c
db 6,%00011000,%01111000,%11111000,%11011000,%11011000,%11011000,%11111000,%01111000,%00000000,0,0,0,0,0,0   ;d
db 6,%00000000,%01110000,%11111000,%11011000,%11111000,%11000000,%11111000,%01111000,%00000000,0,0,0,0,0,0   ;e
db 5,%01110000,%11110000,%11000000,%11110000,%11110000,%11000000,%11000000,%11000000,%00000000,0,0,0,0,0,0   ;f
db 6,%00000000,%01111000,%11111000,%11011000,%11111000,%01111000,%00011000,%11111000,%11110000,0,0,0,0,0,0   ;g
db 6,%11000000,%11110000,%11111000,%11011000,%11011000,%11011000,%11011000,%11011000,%00000000,0,0,0,0,0,0   ;h
db 3,%11000000,%11000000,%00000000,%11000000,%11000000,%11000000,%11000000,%11000000,%00000000,0,0,0,0,0,0   ;i
db 5,%00110000,%00110000,%00000000,%00110000,%00110000,%00110000,%00110000,%11110000,%11100000,0,0,0,0,0,0   ;j
db 6,%11000000,%11011000,%11011000,%11111000,%11110000,%11011000,%11011000,%11011000,%00000000,0,0,0,0,0,0   ;k
db 4,%11000000,%11000000,%11000000,%11000000,%11000000,%11000000,%11100000,%01100000,%00000000,0,0,0,0,0,0   ;l
db 8,%00000000,%01101100,%11111110,%11111110,%11010110,%11000110,%11000110,%11000110,%00000000,0,0,0,0,0,0   ;m
db 6,%00000000,%11110000,%11111000,%11011000,%11011000,%11011000,%11011000,%11011000,%00000000,0,0,0,0,0,0   ;n
db 6,%00000000,%01110000,%11111000,%11011000,%11011000,%11011000,%11111000,%01110000,%00000000,0,0,0,0,0,0   ;o
db 6,%00000000,%11110000,%11111000,%11011000,%11011000,%11111000,%11110000,%11000000,%11000000,0,0,0,0,0,0   ;p
db 6,%00000000,%01111000,%11111000,%11011000,%11011000,%11111000,%01111000,%00011000,%00011000,0,0,0,0,0,0   ;q
db 6,%00000000,%11011000,%11111000,%11110000,%11000000,%11000000,%11000000,%11000000,%00000000,0,0,0,0,0,0   ;r
db 6,%00000000,%01111000,%11111000,%11000000,%11111000,%00011000,%11111000,%11110000,%00000000,0,0,0,0,0,0   ;s
db 5,%01100000,%11110000,%11110000,%01100000,%01100000,%01100000,%01110000,%00110000,%00000000,0,0,0,0,0,0   ;t
db 6,%00000000,%11011000,%11011000,%11011000,%11011000,%11011000,%11111000,%01110000,%00000000,0,0,0,0,0,0   ;u
db 6,%00000000,%11011000,%11011000,%11011000,%11111000,%01110000,%01110000,%00100000,%00000000,0,0,0,0,0,0   ;v
db 8,%00000000,%11000110,%11000110,%11010110,%11111110,%11111110,%11101110,%11000110,%00000000,0,0,0,0,0,0   ;w
db 8,%00000000,%11000110,%11101110,%01111100,%00111000,%01111100,%11101110,%11000110,%00000000,0,0,0,0,0,0   ;x
db 6,%00000000,%11011000,%11011000,%11011000,%11111000,%01111000,%00011000,%01111000,%01110000,0,0,0,0,0,0   ;y
db 6,%00000000,%11111000,%11111000,%00110000,%01100000,%11000000,%11111000,%11111000,%00000000,0,0,0,0,0,0   ;z

;### INFO-FENSTER #############################################################

prgmsginf  dw prgmsginf1,4*1+2,prgmsginf2,4*1+2,prgmsginf3,4*1+2,prgicnbig

prgmsginf1 db "Lymings for SymbOS",0
prgmsginf2 db " Version 0.1 (Build "
read "..\..\..\..\SVN-Main\trunk\build.asm"
           db "pdt)",0
prgmsginf3 db " Copyright <c> 2024 SymbiosiS",0

;### SELECT WINDOW ############################################################

winsel       dw #0001,4+8+16,70,10,  46, 31,0,0,  46, 31,  46, 31,  46, 31,0,0,0,0,winselgrp,0,0:ds 136+14

winselgrp    db 5,0: dw winseldat,0,0,0,0,0,0
winseldat
dw      0,255*256+ 0,7+128,     0, 0,1000,1000,0
winseldat_symbols_id    equ 1
winseldat_symbols
dw      0,255*256+10,gsl_stop     , 1, 1,14,14,0
dw      0,255*256+10,gsl_brdg_rght,16, 1,14,14,0
dw      0,255*256+10,gsl_brdg_left,31, 1,14,14,0
dw      0,255*256+10,gsl_para     , 1,16,14,14,0
dw      0,255*256+10,gsl_suic     ,16,16,14,14,0

;### SAVE(ED) WINDOW ##########################################################

winsav       dw #0001,4+8+16,58+63,54, 108, 82,0,0, 108, 82, 108, 82, 108, 82,0,0,0,0,winsavgrp,0,0:ds 136+14

winsavgrp    db 13,0: dw winsavdat,0,0:db 12,13:dw 0,0,0
winsavdat
dw      0,255*256+ 0,1+128,     0, 0,1000,1000,0
dw      0,255*256+ 2,128+#6600, 0, 0, 108,  82,0

dw      0,255*256+ 5,savln1ctr, 0, 8, 108,   9,0

winsavdat_lem
dw      0,255*256+10,lmawr1   ,13,24,   5,   8,0
dw      0,255*256+10,lmawr1   ,13,40,   5,   8,0

dw      0,255*256+ 5,savln2ctr,23,24,  20,   9,0
dw      0,255*256+ 5,savln3ctr,43,24,  60,   9,0
dw      0,255*256+ 5,savln4ctr,23,40,  20,   9,0
dw      0,255*256+ 5,savln5ctr,43,40,  60,   9,0

dw      0,255*256+ 0,6+128,     7,62,  49,  14,0
dw      0,255*256+ 0,6+128,    62,62,  39,  14,0
winsavdat_but
dw gamsta,255*256+16,diabt1txt, 8,63,  47,  12,0
dw gamcnc,255*256+16,diabt2txt,63,63,  37,  12,0

savln1ctr   dw txtsav1:db 16*8+1,2+128:dw fntmsg
savln2ctr   dw txtsav3:db 16*2+1,1+128:dw fntmsg
savln3ctr   dw txtsav5:db 16*8+1,0+128:dw fntmsg
savln4ctr   dw txtsav4:db 16*2+1,1+128:dw fntmsg
savln5ctr   dw txtsav5:db 16*8+1,0+128:dw fntmsg

txtsav1     db "Save===",0
txtsav2     db "You<saved===",0
txtsav3     db "00",0
txtsav4     db "00",0
txtsav5     db "<Lymings;",0
txtsav6     db "<Ladies;",0
txtsav7     db "<Protectors;",0
txtsav8     db "ALL",0

;### FAILED WINDOW ############################################################

winfai       dw #0001,4+8+16,58+63,54, 108, 82,0,0, 108, 82, 108, 82, 108, 82,0,0,0,0,winfaigrp,0,0:ds 136+14

winfaigrp    db 9,0: dw winfaidat,0,0:db 8,9:dw 0,0,0
winfaidat
dw      0,255*256+ 0,1+128,     0, 0,1000,1000,0
dw      0,255*256+ 2,128+#6600, 0, 0, 108,  82,0

dw      0,255*256+ 5,failn1ctr, 0, 8, 108,   9,0
dw      0,255*256+ 5,failn2ctr, 0,24, 108,   9,0
dw      0,255*256+ 5,failn3ctr, 0,40, 108,   9,0

dw      0,255*256+ 0,6+128,     7,62,  49,  14,0
dw      0,255*256+ 0,6+128,    62,62,  39,  14,0
dw gamrtr,255*256+16,diabt4txt, 8,63,  47,  12,0
dw gamcnc,255*256+16,diabt2txt,63,63,  37,  12,0

failn1ctr   dw txtfai1:db 16*2+1,2+128:dw fntmsg
failn2ctr   dw txtfai2:db 16*8+1,2+128:dw fntmsg
failn3ctr   dw txtfai3:db 16*8+1,2+128:dw fntmsg

txtfai1     db "Oh<no;",0
txtfai2     db "You<didnt<save",0
txtfai3     db "enough<Lymings;",0

;### PAUSE WINDOW #############################################################

winpau       dw #0001,4+8+16,58+63,54, 108, 95,0,0, 108, 95, 108, 95, 108, 95,0,0,0,0,winpaugrp,0,0:ds 136+14

winpaugrp    db 11,0: dw winpaudat,0,0:db 0,8:dw 0,0,0
winpaudat
dw      0,255*256+ 0,1+128,     0, 0,1000,1000,0
dw      0,255*256+ 2,128+#6600, 0, 0, 108,  95,0

dw      0,255*256+ 5,pauln1ctr, 0, 8, 108,   9,0

dw      0,255*256+ 0,6+128,    25,24,  58,  14,0
dw      0,255*256+ 0,6+128,    25,40,  58,  14,0
dw      0,255*256+ 0,6+128,    25,56,  58,  14,0
dw      0,255*256+ 0,6+128,    25,72,  58,  14,0

dw gamcon,255*256+16,diabt3txt,26,25,  56,  12,0
dw gamrst,255*256+16,diabt5txt,26,41,  56,  12,0
dw hlpopn,255*256+16,diabt6txt,26,57,  56,  12,0
dw gamqit,255*256+16,diabt7txt,26,73,  56,  12,0

pauln1ctr   dw txtpau1:db 16*8+1,2+128:dw fntmsg

;### QUIT WINDOW ##############################################################

winqit       dw #0001,4+8+16,58+63,70, 108, 63,0,0, 108, 63, 108, 63, 108, 63,0,0,0,0,winqitgrp,0,0:ds 136+14

winqitgrp    db 7,0: dw winqitdat,0,0:db 6,7:dw 0,0,0
winqitdat
dw      0,255*256+ 0,1+128,     0, 0,1000,1000,0
dw      0,255*256+ 2,128+#6600, 0, 0, 108,  63,0

dw      0,255*256+ 5,qitln1ctr, 0, 8, 108,   9,0

dw      0,255*256+ 0,6+128,    25,24,  58,  14,0
dw      0,255*256+ 0,6+128,    25,40,  58,  14,0

dw gamqit,255*256+16,diabt7txt,26,25,  56,  12,0
dw gamcon,255*256+16,diabt3txt,26,41,  56,  12,0

qitln1ctr   dw txtqit1:db 16*8+1,2+128:dw fntmsg

;### BOOT WINDOW ##############################################################

winbot       dw #0081,4+8+16,58+63,70, 108, 63,0,0, 108, 63, 108, 63, 108, 63,0,0,0,0,winbotgrp,0,0:ds 136+14

winbotgrp    db 5,0: dw winbotdat,0,0:db 6,7:dw 0,0,0
winbotdat
dw      0,255*256+ 0,1+128,     0, 0,1000,1000,0
dw      0,255*256+ 2,128+#6600, 0, 0, 108,  63,0

dw      0,255*256+ 5,botln1ctr, 0, 9, 108,   9,0
dw      0,255*256+ 5,botln2ctr, 0,28, 108,   9,0
dw      0,255*256+ 1,botln3ctr, 0,46, 108,   9,0


botln1ctr   dw txtbot1:db 16*8+1,2+128:dw fntmsg
botln2ctr   dw txtbot2:db 16*2+1,2+128:dw fntmsg
botln3ctr   dw txtbot3:db 16*6+1,2+128

txtbot3 db "Loading...",0

;### HELP WINDOW ##############################################################

winhlp      dw #0001,4+8+16,58+63,54, 140,178,0,0, 140,178, 140,178, 140,178,0,0,0,0,winhlpgrp1,0,0:ds 136+14

txthlpp db "<",0
txthlpn db ">",0
txthlpx db "X",0

;how to play
winhlpgrp1  db 26,0: dw winhlpdat1,0,0:db 0,5:dw 0,0,0
winhlpdat1
dw      0,255*256+ 0,1+128,     0, 0,1000,1000,0
dw      0,255*256+ 2,128+#6600, 0, 0, 140, 178,0
dw      0,255*256+ 5,hlpl100ctr,0, 5, 140,   9,0
dw      0,255*256+ 0,6+128,    124,  2, 14, 14,0
dw hlpcnc,255*256+16,txthlpx,  125,  3, 12, 12,0
winhlpdat1a
dw      0,255*256+10,lmawr1    ,48, 47,  5,  8,0
dw      0,255*256+10,lmawr1    ,58, 47,  5,  8,0
dw      0,255*256+10,lmawr1    ,68, 47,  5,  8,0
dw      0,255*256+10,gmp_exit_t,80, 43, 12, 12,0
dw      0,255*256+ 1,hlpl111ctr,0, 60,140,   9,0
dw      0,255*256+ 1,hlpl112ctr,0, 68,140,   9,0
winhlpdat1b
dw      0,255*256+10,lmawl7    , 23,107,  5,  8,0
dw      0,255*256+10,hlp_ret   , 26, 96,  9,  8,0
dw      0,255*256+10,gmp_wall  , 35,104, 12, 12,0
dw      0,255*256+ 1,hlpl121ctr, 0,121,70,   8,0
dw      0,255*256+ 1,hlpl122ctr, 0,129,70,   8,0
dw      0,255*256+ 1,hlpl123ctr, 0,137,70,   8,0
winhlpdat1c
dw      0,255*256+10,lmawr1    , 96, 95,  5,  8,0
dw      0,255*256+10,gmp_wall  , 92,104, 12, 12,0
dw      0,255*256+10,gmp_watr1 ,104,104, 12, 12,0
dw      0,255*256+ 1,hlpl131ctr,70,121,70,   8,0
dw      0,255*256+ 1,hlpl132ctr,70,129,70,   8,0
dw      0,255*256+ 1,hlpl133ctr,70,137,70,   8,0
dw      0,255*256+ 1,hlpl103ctr, 0,167,140,  8,0
dw      0,255*256+ 0,6+128,    124,162, 14, 14,0
dw hlppg2,255*256+16,txthlpn,  125,163, 12, 12,0

hlpl100ctr  dw txthlp100:db 16*8+1,2+128:dw fntmsg
hlpl103ctr  dw txthlp103:db 16*6+1,2+128
hlpl111ctr  dw txthlp111:db 16*6+1,2+128
hlpl112ctr  dw txthlp112:db 16*6+1,2+128
hlpl121ctr  dw txthlp121:db 16*6+1,2+128
hlpl122ctr  dw txthlp122:db 16*6+1,2+128
hlpl123ctr  dw txthlp123:db 16*6+1,2+128
hlpl131ctr  dw txthlp131:db 16*6+1,2+128
hlpl132ctr  dw txthlp132:db 16*6+1,2+128
hlpl133ctr  dw txthlp133:db 16*6+1,2+128

txthlp100   db "How<to<play",0
txthlp103   db "1/6",0
txthlp111   db "Guide the lymings to",0
txthlp112   db "the exit to save them",0
txthlp121   db "When they",0
txthlp122   db "reach walls",0
txthlp123   db "they turn back",0
txthlp131   db "Avoid traps",0
txthlp132   db "to escape",0
txthlp133   db "death",0

;instructions
winhlpgrp2  db 32,0: dw winhlpdat2,0,0:db 0,5:dw 0,0,0
winhlpdat2
dw      0,255*256+ 0,1+128,     0, 0,1000,1000,0
dw      0,255*256+ 2,128+#6600, 0, 0, 140, 178,0
dw      0,255*256+ 5,hlpl200ctr,0, 5, 140,   9,0
dw      0,255*256+ 0,6+128,    124,  2, 14, 14,0
dw hlpcnc,255*256+16,txthlpx,  125,  3, 12, 12,0
dw      0,255*256+ 1,hlpl201ctr,0,21, 140,   8,0
dw      0,255*256+ 1,hlpl202ctr,0,29, 140,   8,0
winhlpdat2a
dw      0,255*256+10,lmadr0    ,13, 56, 5,   8,0
dw      0,255*256+10,gsl_digg  ,24, 50, 14, 14,0
dw      0,255*256+ 5,hlpl210ctr,44, 45,80,   9,0
dw      0,255*256+ 1,hlpl211ctr,44, 54,80,   9,0
dw      0,255*256+ 1,hlpl212ctr,44, 62,80,   9,0
winhlpdat2b
dw      0,255*256+10,lmabr3    ,13, 86, 5,   8,0
dw      0,255*256+10,gsl_brdg_rght,24,80,14,14,0
dw      0,255*256+ 5,hlpl220ctr,44, 75,80,   9,0
dw      0,255*256+ 1,hlpl221ctr,44, 84,80,   9,0
dw      0,255*256+ 1,hlpl222ctr,44, 92,80,   9,0
winhlpdat2c
dw      0,255*256+10,lmafp0    ,13,112, 5,  12,0
dw      0,255*256+10,gsl_para  ,24,110, 14, 14,0
dw      0,255*256+ 5,hlpl230ctr,44,105,80,   9,0
dw      0,255*256+ 1,hlpl231ctr,44,114,80,   9,0
dw      0,255*256+ 1,hlpl232ctr,44,122,80,   9,0
winhlpdat2d
dw      0,255*256+10,lmast1    ,13,146, 5,   8,0
dw      0,255*256+10,gsl_stop  ,24,140, 14, 14,0
dw      0,255*256+ 5,hlpl240ctr,44,135,80,   9,0
dw      0,255*256+ 1,hlpl241ctr,44,144,80,   9,0
dw      0,255*256+ 1,hlpl242ctr,44,152,80,   9,0
dw      0,255*256+ 1,hlpl203ctr, 0,167,140,  8,0
dw      0,255*256+ 0,6+128,     2, 162, 14, 14,0
dw      0,255*256+ 0,6+128,   124, 162, 14, 14,0
dw hlppg1,255*256+16,txthlpp,    3,163, 12, 12,0
dw hlppg3,255*256+16,txthlpn,  125,163, 12, 12,0

txthlp200   db "Instructions",0
txthlp201   db "Mark actions on fields for",0
txthlp202   db "Lymings to execute them",0
txthlp203   db "2/6",0
txthlp210   db "Digging",0
txthlp211   db "Dig to remove",0
txthlp212   db "boulders",0
txthlp220   db "Stairs",0
txthlp221   db "Build stairs to",0
txthlp222   db "reach higher areas",0
txthlp230   db "Parachute",0
txthlp231   db "Protect Lymings",0
txthlp232   db "from deeper falls",0
txthlp240   db "Blocker",0
txthlp241   db "Stop Lymings and",0
txthlp242   db "let them turn back",0

hlpl200ctr  dw txthlp200:db 16*8+1,2+128:dw fntmsg
hlpl201ctr  dw txthlp201:db 16*6+1,2+128
hlpl202ctr  dw txthlp202:db 16*6+1,2+128
hlpl203ctr  dw txthlp203:db 16*6+1,2+128
hlpl210ctr  dw txthlp210:db 16*6+1,0+128:dw fntmsg
hlpl211ctr  dw txthlp211:db 16*8+1,0+128
hlpl212ctr  dw txthlp212:db 16*8+1,0+128
hlpl220ctr  dw txthlp220:db 16*6+1,0+128:dw fntmsg
hlpl221ctr  dw txthlp221:db 16*8+1,0+128
hlpl222ctr  dw txthlp222:db 16*8+1,0+128
hlpl230ctr  dw txthlp230:db 16*6+1,0+128:dw fntmsg
hlpl231ctr  dw txthlp231:db 16*8+1,0+128
hlpl232ctr  dw txthlp232:db 16*8+1,0+128
hlpl240ctr  dw txthlp240:db 16*6+1,0+128:dw fntmsg
hlpl241ctr  dw txthlp241:db 16*8+1,0+128
hlpl242ctr  dw txthlp242:db 16*8+1,0+128

;navigation
winhlpgrp3  db 18,0: dw winhlpdat3,0,0:db 0,5:dw 0,0,0
winhlpdat3
dw      0,255*256+ 0,1+128,     0, 0,1000,1000,0
dw      0,255*256+ 2,128+#6600, 0, 0, 140, 178,0
dw      0,255*256+ 5,hlpl300ctr,0, 5, 140,   9,0
dw      0,255*256+ 0,6+128,    124,  2, 14, 14,0
dw hlpcnc,255*256+16,txthlpx,  125,  3, 12, 12,0
dw      0,255*256+ 5,hlpl301ctr,0, 61,140,   9,0
dw      0,255*256+ 5,hlpl302ctr,0, 72,140,   9,0
dw      0,255*256+ 0,6+128,     28, 93, 33, 14,0
xcb_speed2
dw      0,255*256+10,0,         29, 94, 32, 12,0
dw      0,255*256+10,hlp_spd   ,72, 97,  7,  7,0
winhlpdat3a
dw      0,255*256+10,lmawr1    ,82, 97,  5,  8,0
dw      0,255*256+10,hlp_spd   ,97, 97,  7,  7,0
winhlpdat3b
dw      0,255*256+10,lmawr2   ,107, 97,  5,  8,0
dw      0,255*256+ 1,hlpl303ctr, 0,167,140,  8,0
dw      0,255*256+ 0,6+128,     2, 162,14,  14,0
dw      0,255*256+ 0,6+128,   124, 162,14,  14,0
dw hlppg2,255*256+16,txthlpp,   3, 163,12,  12,0
dw hlppg4,255*256+16,txthlpn, 125, 163,12,  12,0

txthlp300   db "Navigation",0
txthlp301   db "With<the<fast<forward",0
txthlp302   db "you<speed<up<the<game",0

hlpl300ctr  dw txthlp300:db 16*8+1,2+128:dw fntmsg
hlpl301ctr  dw txthlp301:db 16*6+1,2+128:dw fntmsg
hlpl302ctr  dw txthlp302:db 16*6+1,2+128:dw fntmsg
hlpl303ctr  dw txthlp303:db 16*6+1,2+128

txthlp303   db "3/6",0

;stairs
winhlpgrp4  db 38,0: dw winhlpdat4,0,0:db 0,5:dw 0,0,0
winhlpdat4
dw      0,255*256+ 0,1+128,     0, 0,1000,1000,0
dw      0,255*256+ 2,128+#6600, 0, 0, 140, 178,0
dw      0,255*256+ 5,hlpl400ctr,0, 5, 140,   9,0
dw      0,255*256+ 0,6+128,    124,  2, 14, 14,0
dw hlpcnc,255*256+16,txthlpx,  125,  3,12,  12,0

dw      0,255*256+ 1,hlpl411ctr,0, 28,140,   8,0
dw      0,255*256+ 1,hlpl412ctr,0, 36,140,   8,0

dw      0,255*256+10,hlp_emp      ,31,55,12,12,0
dw      0,255*256+10,hlp_arw      ,49,57, 7, 7,0
dw      0,255*256+10,gsl_brdg_rght,63,54,14,14,0
dw      0,255*256+10,hlp_arw      ,83,57, 7, 7,0
dw      0,255*256+10,gmp_brdg_rf,  97,55,12,12,0
winhlpdat4a
dw      0,255*256+10,lmawr1      ,100,52, 5, 8,0

dw      0,255*256+ 1,hlpl421ctr,0, 88,140,   8,0
dw      0,255*256+ 1,hlpl422ctr,0, 96,140,   8,0
dw      0,255*256+10,gmp_brdg_rf,28,124, 12, 12,0
dw      0,255*256+10,gmp_wall  , 40,124, 12, 12,0
dw      0,255*256+10,gmp_wall  , 16,136, 12, 12,0
dw      0,255*256+10,gmp_watr1 , 28,136, 12, 12,0
dw      0,255*256+10,gmp_wall  , 40,136, 12, 12,0
winhlpdat4b
dw      0,255*256+10,lmawr1    , 43,116,  5,  8,0
dw      0,255*256+10,lmawr1    , 31,121,  5,  8,0
dw      0,255*256+10,lmawr1    , 19,128,  5,  8,0
dw      0,255*256+10,gmp_brdg_lf, 68,114,12, 12,0
dw      0,255*256+10,gmp_brdg_lf, 80,126,12, 12,0
dw      0,255*256+10,gmp_brdg_lf, 92,138,12, 12,0
dw      0,255*256+10,gmp_brdg_rf, 80,114,12, 12,0
dw      0,255*256+10,gmp_brdg_rf, 92,150,12, 12,0
dw      0,255*256+10,gmp_brdg_rf,104,138,12, 12,0
dw      0,255*256+10,gmp_brdg_rf,116,126,12, 12,0
winhlpdat4c
dw      0,255*256+10,lmawr1    , 94,148,  5,  8,0
dw      0,255*256+10,lmawl7    , 90,129,  5,  8,0
dw      0,255*256+10,lmast1    , 72,111,  5,  8,0

dw      0,255*256+ 1,hlpl403ctr, 0,167,140,  8,0
dw      0,255*256+ 0,6+128,     2, 162,14,  14,0
dw      0,255*256+ 0,6+128,   124, 162,14,  14,0
dw hlppg3,255*256+16,txthlpp,   3, 163,12,  12,0
dw hlppg5,255*256+16,txthlpn, 125, 163,12,  12,0

hlpl400ctr  dw txthlp400:db 16*8+1,2+128:dw fntmsg
hlpl403ctr  dw txthlp403:db 16*6+1,2+128

hlpl411ctr  dw txthlp411:db 16*6+1,2+128
hlpl412ctr  dw txthlp412:db 16*6+1,2+128
hlpl421ctr  dw txthlp221:db 16*6+1,2+128
hlpl422ctr  dw txthlp222:db 16*6+1,2+128

txthlp400   db "Stairs",0
txthlp403   db "4/6",0
txthlp411   db "Choose an empty area",0
txthlp412   db "to build stairs",0

;secret blocks
winhlpgrp5  db 19,0: dw winhlpdat5,0,0:db 0,5:dw 0,0,0
winhlpdat5
dw      0,255*256+ 0,1+128,     0, 0,1000,1000,0
dw      0,255*256+ 2,128+#6600, 0, 0, 140, 178,0
dw      0,255*256+ 5,hlpl500ctr,0, 5, 140,   9,0
dw      0,255*256+ 0,6+128,    124,  2, 14, 14,0
dw hlpcnc,255*256+16,txthlpx,  125,  3, 12, 12,0
dw      0,255*256+ 1,hlpl511ctr,0, 40,140,   8,0
winhlpdat5a
dw      0,255*256+10,lmadr0    ,59, 63,  5,  8,0
dw      0,255*256+10,gmp_grqm  ,68, 60, 12, 12,0
dw      0,255*256+ 1,hlpl512ctr,0,100,140,   8,0
dw      0,255*256+10,gmp_excl_t,48,120, 12, 12,0
dw      0,255*256+10,gmp_excl_b,48,132, 12, 12,0
dw      0,255*256+10,hlp_arw   ,66,129,  7,  7,0
dw      0,255*256+10,gmp_exit_t,80,120, 12, 12,0
dw      0,255*256+10,gmp_exit_b,80,132, 12, 12,0
dw      0,255*256+ 1,hlpl503ctr, 0,167,140,  8,0
dw      0,255*256+ 0,6+128,     2, 162, 14,  14,0
dw      0,255*256+ 0,6+128,   124, 162, 14, 14,0
dw hlppg4,255*256+16,txthlpp,   3, 163, 12, 12,0
dw hlppg6,255*256+16,txthlpn, 125, 163,12,  12,0

hlpl500ctr  dw txthlp500:db 16*8+1,2+128:dw fntmsg
hlpl503ctr  dw txthlp503:db 16*6+1,2+128
hlpl511ctr  dw txthlp511:db 16*6+1,2+128
hlpl512ctr  dw txthlp512:db 16*6+1,2+128

txthlp500   db "Secret<blocks",0
txthlp503   db "5/6",0
txthlp511   db "Dig through secret blocks...",0
txthlp512   db "...to find the key to the exit!",0

;shortcut keys
winhlpgrp6  db 25,0: dw winhlpdat6,0,0:db 0,5:dw 0,0,0
winhlpdat6
dw      0,255*256+ 0,1+128,     0, 0,1000,1000,0
dw      0,255*256+ 2,128+#6600, 0, 0, 140, 178,0
dw      0,255*256+ 5,hlpl600ctr,0, 5, 140,   9,0
dw      0,255*256+ 0,6+128,    124,  2, 14, 14,0
dw hlpcnc,255*256+16,txthlpx,  125,  3, 12, 12,0
dw      0,255*256+ 1,hlpl601ctr,0,21, 140,   8,0
dw      0,255*256+ 1,hlpl602ctr,0,29, 140,   8,0

dw      0,255*256+ 5,hlpl610ctr,28, 53,80,   9,0
dw      0,255*256+10,gsl_stop  ,44, 50, 14, 14,0
dw      0,255*256+ 1,hlpl611ctr,64, 54,80,   9,0

dw      0,255*256+ 5,hlpl620ctr,28, 73,80,   9,0
dw      0,255*256+10,gsl_para  ,44,70,14,14,0
dw      0,255*256+ 1,hlpl621ctr,64, 74,80,   9,0

dw      0,255*256+ 5,hlpl630ctr,28, 93,80,   9,0
dw      0,255*256+10,gsl_brdg_rght,44,90,14,14,0
dw      0,255*256+ 1,hlpl631ctr,64, 94,80,   9,0

dw      0,255*256+ 5,hlpl640ctr,28,113,80,   9,0
dw      0,255*256+10,gsl_brdg_left,44,110,14,14,0
dw      0,255*256+ 1,hlpl641ctr,64,114,80,   9,0

dw      0,255*256+ 5,hlpl650ctr,28,133,80,   9,0
dw      0,255*256+10,gsl_suic  ,44,130, 14, 14,0
dw      0,255*256+ 1,hlpl651ctr,64,134,80,   9,0

dw      0,255*256+ 1,hlpl603ctr, 0,167,140,  8,0
dw      0,255*256+ 0,6+128,     2, 162, 14, 14,0
dw hlppg5,255*256+16,txthlpp,    3,163, 12, 12,0

txthlp600   db "Shortcut<Keys",0
txthlp601   db "Use keyboard shortcuts",0
txthlp602   db "for quick preselection",0
txthlp603   db "6/6",0
txthlp610   db "B",0
txthlp611   db "Blocker",0
txthlp620   db "P",0
txthlp621   db "Parachute",0
txthlp630   db "R",0
txthlp631   db "Stairs top right",0
txthlp640   db "L",0
txthlp641   db "Stairs top left",0
txthlp650   db "S",0
txthlp651   db "Suicider",0

hlpl600ctr  dw txthlp600:db 16*8+1,2+128:dw fntmsg
hlpl601ctr  dw txthlp601:db 16*6+1,2+128
hlpl602ctr  dw txthlp602:db 16*6+1,2+128
hlpl603ctr  dw txthlp603:db 16*6+1,2+128

hlpl610ctr  dw txthlp610:db 16*8+1,0+128:dw fntmsg
hlpl611ctr  dw txthlp611:db 16*6+1,0+128
hlpl620ctr  dw txthlp620:db 16*8+1,0+128:dw fntmsg
hlpl621ctr  dw txthlp621:db 16*6+1,0+128
hlpl630ctr  dw txthlp630:db 16*8+1,0+128:dw fntmsg
hlpl631ctr  dw txthlp631:db 16*6+1,0+128
hlpl640ctr  dw txthlp640:db 16*8+1,0+128:dw fntmsg
hlpl641ctr  dw txthlp641:db 16*6+1,0+128
hlpl650ctr  dw txthlp650:db 16*8+1,0+128:dw fntmsg
hlpl651ctr  dw txthlp651:db 16*6+1,0+128

;### MAIN MENU ################################################################

winmengrp    db 49,0: dw winmendat,0,0:db winmendat_but_id+1,0:dw 0,0,0
winmendat
dw      0,255*256+0,128+1,             0,0,10000,10000,0

winmendat_star_id   equ 1
winmendat_star
dw 0,255*256+10,star0,   9,  5, 7,7,0
dw 0,255*256+10,star0,  36, 18, 7,7,0
dw 0,255*256+10,star1,  25, 42, 7,7,0
dw 0,255*256+10,star0,   6, 63, 7,7,0
dw 0,255*256+10,star0,  35, 86, 7,7,0
dw 0,255*256+10,star1,  14,117, 7,7,0
dw 0,255*256+10,star0,  31,138, 7,7,0
dw 0,255*256+10,star0,  10,157, 7,7,0

dw 0,255*256+10,star1,  82, 28, 7,7,0
dw 0,255*256+10,star0, 105, 41, 7,7,0
dw 0,255*256+10,star0,  95, 69, 7,7,0
dw 0,255*256+10,star0,  85,107, 7,7,0
dw 0,255*256+10,star1, 109,123, 7,7,0
dw 0,255*256+10,star0,  91,135, 7,7,0
dw 0,255*256+10,star1,  85,166, 7,7,0

winmendat_clr_id    equ 16
            dw 0,255*256+0,128+1,      62+14, 05,20,15,0    ;remove flags, texts
            dw 0,255*256+0,128+1,      62+14, 75,20,15,0
            dw 0,255*256+0,128+1,      62+14,145,20,15,0
            dw 0,255*256+0,128+1,         16, 61,86, 8,0
            dw 0,255*256+0,128+1,         54,104,16, 8,0

xcb_arwup   dw mennxt,255*256+10,0,      102, 01,16,16,0
xcb_arwdw   dw menprv,255*256+10,0,      102,156,16,16,0

winmendat_planet_id equ winmendat_clr_id+7
xcb_planet1 dw 0,255*256+10,0,         62-14, 00,28,32,0
xcb_line1   dw 0,255*256+10,0,         62-2,  32, 4,42,0
xcb_planet2 dw 0,255*256+10,0,         62-14, 70,28,32,0
xcb_line2   dw 0,255*256+10,0,         62-2, 102, 4,42,0
xcb_planet3 dw 0,255*256+10,0,         62-14,140,28,32,0
xcb_line3   dw 0,255*256+10,0,         62-2, 172, 4,42,0
xcb_planet4 dw 0,255*256+10,0,         62-14,210,28,32,0
xcb_arwrgt  dw 0,255*256+10,0,            14, 78, 8,16,0
xcb_arwlft  dw 0,255*256+10,0,           102, 78, 8,16,0

winmendat_flag_id   equ winmendat_planet_id+9
winmendat_flag
xcb_flag1   dw 0,255*256+10,0,         62+14, 05,20,15,0
xcb_flag2   dw 0,255*256+10,0,         62+14, 75,20,15,0
xcb_flag3   dw 0,255*256+10,0,         62+14,145,20,15,0

winmendat_ptxt_id   equ winmendat_flag_id+3
winmendat_ptxt
dw      0,255*256+ 1,menpl1ctr,           4, 61,118, 8,0
dw      0,255*256+ 1,menpl2ctr,           4,104,118, 8,0

winmendat_hid
dw      0,255*256+10,ui_prgr_fields0,  map_xofs+map_xlen*fld_xlen,0,1,1,0

;*** UI
xcb_gamlog2
dw      0,255*256+10,0,                map_xofs+map_xlen*fld_xlen,    0,104, 48,0
dw      0,255*256+0, 128+6,            map_xofs+map_xlen*fld_xlen,   48,999,999,0

dw      0,255*256+1, mencr1ctr,        map_xofs+map_xlen*fld_xlen+2, 50,100,  8,0
dw      0,255*256+1, mencr2ctr,        map_xofs+map_xlen*fld_xlen+2, 58,100,  8,0

winmendat_but_id   equ winmendat_ptxt_id+7
dw lvlcon,255*256+16,menbt1txt,        map_xofs+map_xlen*fld_xlen+16, 74,72, 12,0
dw lvlrst,255*256+16,menbt2txt,        map_xofs+map_xlen*fld_xlen+16, 88,72, 12,0

dw lvldem,255*256+16,menbt3txt,        map_xofs+map_xlen*fld_xlen+16,109,72, 12,0
dw      0,255*256+16,menbt4txt,        map_xofs+map_xlen*fld_xlen+16,130,72, 12,0
dw menhlp,255*256+16,menbt5txt,        map_xofs+map_xlen*fld_xlen+16,144,72, 12,0
dw prginf,255*256+16,menbt6txt,        map_xofs+map_xlen*fld_xlen+16,158,35, 12,0
dw prghlp,255*256+16,menbt7txt,        map_xofs+map_xlen*fld_xlen+53,158,35, 12,0

mencr1ctr   dw mencr1txt:db 16*8+1,2+128+64
mencr2ctr   dw mencr2txt:db 16*2+1,2+128+64

mencr1txt   db "(c) by Prodatron",0
mencr2txt   db "SymbiosiS 2022",0

menbt1txt   db "Let's go!",0
menbt2txt   db "Restart world",0
menbt3txt   db "Demo mode",0
menbt4txt   db "Settings",0
menbt5txt   db "Help",0
menbt6txt   db "About",0
menbt7txt   db "Info",0


menpl1ctr   dw menpl1txt:db 16*12+1,2+128
menpl2ctr   dw menpl2txt:db 16*12+1,2+128

menpl1txt   db "xx. ":ds 16
menpl2txt   db "x/x",0


;### GAME WINDOW ##############################################################

wingam       dw #1501,0,63,1, 224,173,0,0, 224,173, 224,173, 224,173,prgicnsml,wintittxt,0,0,winmengrp,0,0:ds 136+14

wingamgrp    db map_xlen*map_ylenv+5+LEM_MAX+LEM_MAX+ANI_MAX+4+34,0: dw wingamdat,0,0:db wingamdat_spd_id-1+1,wingamdat_spd_id+1+1:dw 0,0,0
wingamdat
dw      0,255*256+0, 1,                0,0,10000,10000,0

xcb_end_u
dw      0,255*256+10,0,map_xofs,0,                    120,2,0

;*** map
wingamdat_map_id    equ 2
wingamdat_map
ds      16*map_xlen*map_ylenv

xcb_end_d
dw      0,255*256+10,0,map_xofs,map_ylenv*12+map_yofs,120,3,0

wingamdat_select_id equ map_xlen*map_ylenv+wingamdat_map_id+1
wingamdat_select
dw      0,255*256+64,128+#8808,        12,120,12,12,0

;*** lemmings
wingamdat_lem_id    equ wingamdat_select_id+1
wingamdat_lem
repeat LEM_MAX
dw      0,255*256+64,0,0,0,6,8,0
dw      0,255*256+64,0,0,0,4,8,0
rend

;*** animations
wingamdat_ani_id    equ LEM_MAX*2+wingamdat_lem_id
wingamdat_ani
repeat ANI_MAX
dw      0,255*256+64,0,0,0,12,12,0
rend

;*** active trap
wingamdat_trp_id    equ wingamdat_ani_id+ANI_MAX
wingamdat_trp
repeat 4
dw      0,255*256+64,0,0,0,12,12,0
rend

wingamdat_mapclk
dw mapclk,255*256+19,0,map_xofs+fld_xlen,map_yofs,map_xlenv*fld_xlen,map_ylenv*fld_xlen,0

;*** UI
xcb_gamlog1
dw      0,255*256+10,0,                map_xofs+map_xlen*fld_xlen,    0,104, 48,0
dw      0,255*256+0, 128+6,            map_xofs+map_xlen*fld_xlen,   48,999,999,0

dw      0,255*256+2, #ff00+128,        map_xofs+map_xlen*fld_xlen+1, 50,101, 13,0
dw      0,255*256+0, 128+1,            map_xofs+map_xlen*fld_xlen+2, 51, 99, 11,0
wingamdat_msg_id    equ wingamdat_trp_id+4+5
dw      0,255*256+5, ui_msgctr,        map_xofs+map_xlen*fld_xlen+2, 52, 99,  9,0

dw      0,255*256+0, 128+6,            map_xofs+map_xlen*fld_xlen,   89,104, 21,0
wingamdat_prgrs1_id equ wingamdat_msg_id+2
wingamdat_prgrs1
dw      0,255*256+0, 128+6,            map_xofs+map_xlen*fld_xlen,   67, 99, 09,0
dw      0,255*256+10,ui_progress1,     map_xofs+map_xlen*fld_xlen,   76,104, 12,0
dw      0,255*256+10,ui_prgr_lem,      255                          ,67, 12,  9,0
wingamdat_prgrs1a
dw      0,255*256+10,lmawr1,           255                          ,68,  5,  8,0
dw      0,255*256+10,ui_prgr_mi1,      255                          ,67, 12,  9,0
dw      0,255*256+10,ui_prgr_mx1,      255                          ,67, 12,  9,0

wingamdat_prgrs2_id equ wingamdat_prgrs1_id+6
wingamdat_prgrs2
dw      0,255*256+0, 128+6,            map_xofs+map_xlen*fld_xlen,   89, 99, 09,0
dw      0,255*256+10,ui_progress2,     map_xofs+map_xlen*fld_xlen,   98,104, 12,0
dw      0,255*256+10,ui_prgr_lem,      255                          ,89, 12,  9,0
wingamdat_prgrs2a
dw      0,255*256+10,lmawr1,           map_xofs+map_xlen*fld_xlen+5, 90,  5,  8,0
dw      0,255*256+10,ui_prgr_mi1,      map_xofs+map_xlen*fld_xlen+55,89, 12,  9,0
dw      0,255*256+10,ui_prgr_mx1,      255                          ,89, 12,  9,0

dw      0,255*256+17,ui_selctr,        map_xofs+map_xlen*fld_xlen+ 4,113,96,  8,0
dw      0,255*256+0 ,128+1,            map_xofs+map_xlen*fld_xlen+ 4,122,96, 16,0

wingamdat_sel_id    equ wingamdat_prgrs2_id+8
wingamdat_sel
dw mappsla,255*256+10,gsl_stop,        map_xofs+map_xlen*fld_xlen+ 5,123,14, 14,0
dw mappslb,255*256+10,gsl_para,        map_xofs+map_xlen*fld_xlen+21,123,14, 14,0
dw mappslc,255*256+10,gsl_digg,        map_xofs+map_xlen*fld_xlen+37,123,14, 14,0
dw mappsld,255*256+10,gsl_brdg_left,   map_xofs+map_xlen*fld_xlen+53,123,14, 14,0
dw mappsle,255*256+10,gsl_brdg_rght,   map_xofs+map_xlen*fld_xlen+69,123,14, 14,0
dw mappslf,255*256+10,gsl_suic,        map_xofs+map_xlen*fld_xlen+85,123,14, 14,0
dw      0,255*256+2,256*#77+128,       map_xofs+map_xlen*fld_xlen+ 5,123,14, 14,0

xcb_timer
dw      0,255*256+10,0,                map_xofs+map_xlen*fld_xlen+14,141,14, 14,0
wingamdat_tim_id    equ wingamdat_sel_id+8
dw      0,255*256+ 1,ui_timctr,        map_xofs+map_xlen*fld_xlen+30,144,24,  8,0
xcb_items
dw      0,255*256+10,0,                map_xofs+map_xlen*fld_xlen+60,141,14, 14,0
wingamdat_itm_id    equ wingamdat_tim_id+2
dw      0,255*256+ 1,ui_itmctr,        map_xofs+map_xlen*fld_xlen+76,144,12,  8,0

xcb_pause
dw ctrpau,255*256+10,0,                map_xofs+map_xlen*fld_xlen+ 3,159,32, 12,0
wingamdat_spd_id    equ wingamdat_itm_id+2
wingamdat_spd
xcb_speed1
dw ctrspd,255*256+10,0,                map_xofs+map_xlen*fld_xlen+36,159,32, 12,0
xcb_quit
dw ctrqit,255*256+10,0,                map_xofs+map_xlen*fld_xlen+69,159,32, 12,0

ui_msgctr   dw txtzero:db 16*8+1,2+128:dw fntmsg
ui_selctr   dw ui_selsta,ui_seltxt:db 2+4
ui_selsta   db 0

ui_timctr   dw ui_timtxt,256*128+6+16
ui_itmctr   dw ui_itmtxt,256*128+6+16



wintittxt  db "Lymings",0

txtzero     db 0

ui_seltxt   db "Preselect",0
ui_timtxt   db "00:00",0
ui_itmtxt   db "10",0

msg_ohno    db "Oh<no;",0
msg_timeup  db "Time<is<up;",0
msg_great   db "Great;",0
msg_perfect db "Perfect;",0
msg_letsgo  db "Lets<Go;",0
msg_demo    db "Demomode",0

txtpau1     db "Pause",0
txtqit1     db "Quit<Level>",0
txtbot1     db "LymINGS",0
txtbot2     db "by<SymbiosiS",0

diabt1txt   db "Start!",0
diabt2txt   db "Back",0
diabt3txt   db "Continue",0
diabt4txt   db "Retry!",0
diabt5txt   db "Restart",0
diabt6txt   db "Help",0
diabt7txt   db "Quit",0


prgtrnend

;Recorded on a 4Mhz Z80 CPU system - Amstrad CPC 6128, original speed.
;Using a Graphics9000 and a Moonsound (OPL4) and some ram, which are connected to the Z80 bus.
;But the CPU is still just... 8bit 4Mhz.
;You can see Lymings, a Lemmings Mobile Clone, PacMan for SymbOS and SymAmp - playing Amiga MODs - at the same time, just on a little Amstrad CPC with just an 8bit 4MHz Z80 CPU.
