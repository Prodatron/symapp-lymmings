nolist

org #1000

WRITE "f:\symbos\apps\lymings.exe"
READ "..\..\..\..\SVN-Main\trunk\SymbOS-Constants.asm"

relocate_start

App_BegCode

;### APPLICATION HEADER #######################################################

;header structure
prgdatcod       equ 0           ;Length of the code area (OS will place this area everywhere)
prgdatdat       equ 2           ;Length of the data area (screen manager data; OS will place this area inside a 16k block of one 64K bank)
prgdattra       equ 4           ;Length of the transfer area (stack, message buffer, desktop manager data; placed between #c000 and #ffff of a 64K bank)
prgdatorg       equ 6           ;Original origin of the assembler code
prgdatrel       equ 8           ;Number of entries in the relocator table
prgdatstk       equ 10          ;Length of the stack in bytes
prgdatrsv       equ 12          ;*reserved* (3 bytes)
prgdatnam       equ 15          ;program name (24+1[0] chars)
prgdatflg       equ 40          ;flags (+1=16colour icon available)
prgdat16i       equ 41          ;file offset of 16colour icon
prgdatrs2       equ 43          ;*reserved* (5 bytes)
prgdatidn       equ 48          ;"SymExe10" SymbOS executable file identification
prgdatcex       equ 56          ;additional memory for code area (will be reserved directly behind the loaded code area)
prgdatdex       equ 58          ;additional memory for data area (see above)
prgdattex       equ 60          ;additional memory for transfer area (see above)
prgdatres       equ 62          ;*reserved* (26 bytes)
prgdatver       equ 88          ;required OS version (3.0)
prgdatism       equ 90          ;Application icon (small version), 8x8 pixel, SymbOS graphic format
prgdatibg       equ 109         ;Application icon (big version), 24x24 pixel, SymbOS graphic format
prgdatlen       equ 256         ;length of header

prgpstdat       equ 6           ;start address of the data area
prgpsttra       equ 8           ;start address of the transfer area
prgpstspz       equ 10          ;additional sub process or timer IDs (4*1)
prgpstbnk       equ 14          ;64K ram bank (1-15), where the application is located
prgpstmem       equ 48          ;additional memory areas; 8 memory areas can be registered here, each entry consists of 5 bytes
                                ;00  1B  Ram bank number (1-8; if 0, the entry will be ignored)
                                ;01  1W  Address
                                ;03  1W  Length
prgpstnum       equ 88          ;Application ID
prgpstprz       equ 89          ;Main process ID

            dw App_BegData-App_BegCode  ;length of code area
            dw App_BegTrns-App_BegData  ;length of data area
            dw App_EndTrns-App_BegTrns  ;length of transfer area
prgdatadr   dw #1000                ;original origin                    POST address data area
prgtrnadr   dw relocate_count       ;number of relocator table entries  POST address transfer area
prgprztab   dw prgstk-App_BegTrns   ;stack length                       POST table processes
            dw 0                    ;*reserved*
App_BnkNum  db 0                    ;*reserved*                         POST bank number
            db "Lymings":ds 17:db 0 ;name
            db 1                    ;flags (+1=16c icon)
            dw prgicn16c-App_BegCode ;16 colour icon offset
            ds 5                    ;*reserved*
prgmemtab   db "SymExe10"           ;SymbOS-EXE-identifier              POST table reserved memory areas
            dw 9*149+640+256        ;additional code memory (planet+level data+256 path)
            dw 0                    ;additional data memory
            dw 0                    ;additional transfer memory
            ds 26                   ;*reserved*
            db 1,3                  ;required OS version (3.1)
prgicnsml   db 2,8,8,#E1,#3C,#C3,#70,#E0,#30,#E0,#F8,#D1,#F8,#D1,#D8,#F3,#B0,#C0,#F0
prgicnbig   db 6,24,24
            db #F5,#F5,#F5,#F5,#F5,#F5,#F8,#F0,#F0,#F8,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F1,#F8,#F0,#F0,#F8,#F0,#F0,#D2,#78,#E1,#3C,#F0,#F1,#CB,#78,#C3,#70,#F0,#F0,#E1,#70,#E0,#30,#F0,#F1,#E8,#30,#E0,#F8,#F0,#F0
            db #C0,#F8,#D1,#F8,#F0,#F1,#D9,#F8,#D1,#D8,#F0,#F0,#F3,#FC,#F3,#B0,#F0,#F1,#98,#90,#C0,#F0,#F0,#F0,#80,#00,#10,#E8,#60,#71,#FF,#FF,#EE,#99,#00,#11,#FF,#FF,#FF,#88,#00,#11,#CF,#4F,#6F,#88,#22,#11
            db #CF,#AF,#7F,#88,#00,#55,#DF,#1F,#6F,#99,#00,#11,#EF,#0F,#FF,#88,#00,#11,#DF,#1F,#6F,#88,#22,#11,#CF,#AF,#7F,#88,#00,#55,#CF,#4F,#6F,#99,#00,#11,#FF,#FF,#FF,#CC,#00,#33,#F7,#FF,#FE,#FF,#FF,#FF


;*** KERNEL LIBRARY USAGE
use_SyKernel_MTADDP     equ 0   ;Adds a new process and starts it
use_SyKernel_MTDELP     equ 0   ;Stops an existing process and deletes it
use_SyKernel_MTADDT     equ 0   ;Adds a new timer and starts it
use_SyKernel_MTDELT     equ 0   ;Stops an existing timer and deletes it
use_SyKernel_MTSLPP     equ 0   ;Puts an existing process into sleep mode
use_SyKernel_MTWAKP     equ 0   ;Wakes up a process, which was sleeping
use_SyKernel_TMADDT     equ 1   ;Adds a counter for a process
use_SyKernel_TMDELT     equ 0   ;Stops a counter of a process
use_SyKernel_TMDELP     equ 0   ;Stops all counters of one process
use_SyKernel_MTPRIO     equ 0   ;Changes the priority of a process

;*** SYSTEM MANAGER LIBRARY USAGE
use_SySystem_PRGRUN     equ 1   ;Starts an application or opens a document
use_SySystem_PRGEND     equ 1   ;Stops an application and frees its resources
use_SySystem_PRGSRV     equ 1   ;Manages shared services or finds applications
use_SySystem_SYSWRN     equ 1   ;Opens an info, warning or confirm box
use_SySystem_SELOPN     equ 0   ;Opens the file selection dialogue
use_SySystem_HLPOPN     equ 1   ;HLP file handling

;*** DESKTOP MANAGER LIBRARY USAGE
use_SyDesktop_WINOPN    equ 1   ;Opens a new window
use_SyDesktop_WINMEN    equ 0   ;Redraws the menu bar of a window
use_SyDesktop_WININH    equ 0   ;Redraws the content of a window
use_SyDesktop_WINTOL    equ 0   ;Redraws the content of the window toolbar
use_SyDesktop_WINTIT    equ 0   ;Redraws the title bar of a window
use_SyDesktop_WINSTA    equ 0   ;Redraws the status bar of a window
use_SyDesktop_WINMVX    equ 0   ;Sets the X offset of a window content
use_SyDesktop_WINMVY    equ 0   ;Sets the Y offset of a window content
use_SyDesktop_WINTOP    equ 0   ;Takes a window to the front position
use_SyDesktop_WINMAX    equ 0   ;Maximizes a window
use_SyDesktop_WINMIN    equ 0   ;Minimizes a window
use_SyDesktop_WINMID    equ 0   ;Restores a window or the size of a window
use_SyDesktop_WINMOV    equ 1   ;Moves a window to another position
use_SyDesktop_WINSIZ    equ 0   ;Resizes a window
use_SyDesktop_WINCLS    equ 1   ;Closes a window
use_SyDesktop_WINDIN    equ 1   ;Redraws the content of a window (always)
use_SyDesktop_WINSLD    equ 0   ;Redraws the two slider of a window
use_SyDesktop_WINPIN    equ 1   ;Redraws the content of a window (clipped)
use_SyDesktop_WINSIN    equ 0   ;Redraws the content of a control collection
use_SyDesktop_MENCTX    equ 0   ;Opens a context menu
use_SyDesktop_STIADD    equ 0   ;Adds an icon to the systray
use_SyDesktop_STIREM    equ 0   ;Removes an icon from the systray
use_SyDesktop_Service   equ 0   ;[REQUIRED FOR THE FOLLOWING FUNCTIONS]
use_SyDesktop_MODGET    equ 0   ;Returns the current screen mode
use_SyDesktop_MODSET    equ 0   ;Sets the current screen 
use_SyDesktop_COLGET    equ 0   ;Returns the definition of a colours
use_SyDesktop_COLSET    equ 0   ;Defines one colours
use_SyDesktop_DSKSTP    equ 0   ;Stops the Desktop Manager
use_SyDesktop_DSKCNT    equ 0   ;Continues the Desktop Manager
use_SyDesktop_DSKPNT    equ 0   ;Fills the screen
use_SyDesktop_DSKBGR    equ 0   ;Redraws the desktop background
use_SyDesktop_DSKPLT    equ 0   ;Redraws the complete screen

;*** FILE MANAGER LIBRARY USAGE
use_SyFile_STOTRN       equ 0   ;Reads or writes a number of sectors
use_SyFile_FILNEW       equ 1   ;Creates a new file and opens it
use_SyFile_FILOPN       equ 1   ;Opens an existing file
use_SyFile_FILCLO       equ 1   ;Closes an opened file
use_SyFile_FILINP       equ 1   ;Reads an amount of bytes out of an opened file
use_SyFile_FILOUT       equ 1   ;Writes an amount of bytes into an opened file
use_SyFile_FILPOI       equ 1   ;Moves the file pointer to another position
use_SyFile_FILF2T       equ 0   ;Decodes the file timestamp
use_SyFile_FILT2F       equ 0   ;Encodes the file timestamp
use_SyFile_FILLIN       equ 0   ;Reads one text line out of an opened file
use_SyFile_FILCPR       equ 1   ;Reads (un)compressed data out of an opened file
use_SyFile_DIRDEV       equ 0   ;Sets the current drive
use_SyFile_DIRPTH       equ 0   ;Sets the current path
use_SyFile_DIRPRS       equ 0   ;Changes a property of a file or a directory
use_SyFile_DIRPRR       equ 0   ;Reads a property of a file or a directory
use_SyFile_DIRREN       equ 0   ;Renames a file or a directory
use_SyFile_DIRNEW       equ 0   ;Creates a new directory
use_SyFile_DIRINP       equ 0   ;Reads the content of a directory
use_SyFile_DIRDEL       equ 0   ;Deletes one or more files
use_SyFile_DIRRMD       equ 0   ;Deletes a sub directory
use_SyFile_DIRMOV       equ 0   ;Moves a file or sub directory
use_SyFile_DIRINF       equ 0   ;Returns information about one drive
use_SyFile_DEVDIR       equ 0   ;Reads the content of a directory (extended)

;*** SOUND DAEMON LIBRARY USAGE
use_SySound_RMTACT      equ 0   ;activates remote playing
use_SySound_RMTDCT      equ 0   ;deactivates remote playing
use_SySound_MUSLOD      equ 1   ;loads and inits music data
use_SySound_MUSFRE      equ 1   ;removes music data
use_SySound_MUSRST      equ 1   ;restarts a music
use_SySound_MUSCON      equ 0   ;continues playing a music
use_SySound_MUSSTP      equ 1   ;pauses and mutes music
use_SySound_MUSVOL      equ 1   ;sets music volume
use_SySound_EFXLOD      equ 1   ;loads and inits effect data
use_SySound_EFXFRE      equ 1   ;removes effect data
use_SySound_EFXPLY      equ 1   ;starts playing an effect
use_SySound_EFXSTP      equ 0   ;stop effects

READ "..\..\..\..\SVN-Main\trunk\Docs-Developer\symbos_lib-Kernel.asm"
READ "..\..\..\..\SVN-Main\trunk\Docs-Developer\symbos_lib-SystemManager.asm"
READ "..\..\..\..\SVN-Main\trunk\Docs-Developer\symbos_lib-DesktopManager.asm"
READ "..\..\..\..\SVN-Main\trunk\Docs-Developer\symbos_lib-FileManager.asm"
READ "..\..\..\..\SVN-Main\trunk\Docs-Developer\symbos_lib-SoundDaemon.asm"
READ "App-Lymings.asm"

App_EndTrns

relocate_table
relocate_end
