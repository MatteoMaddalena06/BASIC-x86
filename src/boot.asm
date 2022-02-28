;------------------------------------------------------
; Bootloader's Macro

    SEG_TO_LOAD_BOOT      EQU     50h 
    OFF_TO_LOAD_BOOT      EQU     0h 
 
    SEG_TO_LOAD_BASIC     EQU     70h 
    OFF_TO_LOAD_BASIC     EQU     0h

    SEG_BIOS_LOAD         EQU     0h 
    OFF_BIOS_LOAD         EQU     7C00h 

    DIM_BOOTLOADER        EQU     200h 
    DIM_BASIC             EQU     4C00h

    jmpfar MACRO segm, off 

        db  0EAh 
        dw  off 
        dw  segm 

    ENDM

    write_msg MACRO offm, dim

        LOCAL _write

        mov     ax, 0B800h 
        mov     es, ax

        xor     ax, ax 
        xor     di, di
        mov     cx, 0FFFh 
        rep     stosw

        mov     si, offm 
        xor     di, di
        mov     cx, dim 

        _write:
            movsb 
            mov     BYTE PTR es:[di], 0Fh 
            inc     di 
            loop    _write 

    ENDM
;------------------------------------------------------

_text SEGMENT PARA PUBLIC 

_start:
    mov     ax, SEG_BIOS_LOAD
    mov     ds, ax 
    mov     ax, SEG_TO_LOAD_BOOT 
    mov     es, ax 
    mov     ss, ax 
    mov     sp, 0FFFFh 

    cld
    ;load bootloader at 0500h(0050h:0000h)
    mov     cx, DIM_BOOTLOADER/2h
    mov     si, OFF_BIOS_LOAD 
    xor     di, di 
    rep     movsw

    ;jump to new bootloader's position
    jmpfar  SEG_TO_LOAD_BOOT, _continue

_continue:
    mov     ax, SEG_TO_LOAD_BASIC
    mov     es, ax 
    mov     ax, SEG_TO_LOAD_BOOT
    mov     ds, ax 

    ;read BASIC interpreter to disk and load it
    ;start at 0700h(0070h:0000h)
    mov     ah, 2h 
    mov     al, DIM_BASIC/200h
    xor     ch, ch 
    mov     cl, 2h 
    xor     dh, dh 
    mov     bx, OFF_TO_LOAD_BASIC
    int     13h 

    ;if an error has been occured
    jc      _error_disk

    write_msg  OFFSET ok_msg, 1Dh

    jmpfar   SEG_TO_LOAD_BASIC, OFF_TO_LOAD_BASIC 

_error_disk:
    write_msg OFFSET err_msg, 3Dh

    ;waiting key press
    xor    ah, ah 
    int    16h 

    ;restart system
    mov    ax, 40h 
    mov    es, ax 
    mov    WORD PTR es:[72h], 0h 
    jmpfar  0FFFFh, 0h


    ok_msg      db     "Starting BASIC interpreter..."
    err_msg     db     "An error has been occured, press any key to restart system..."

_text ENDS 
    END _start