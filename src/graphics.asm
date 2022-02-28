;-------------------------------------------------------------
; Questo modulo contiene le procedure per la gestione dell'
; interfaccia testuale.
;-------------------------------------------------------------

INCLUDE BASIC.INC

LEFT_SCROLL    EQU   0h 
RIGHT_SCROLL   EQU   1h 

_text SEGMENT PARA PUBLIC 

    ;-----------------------------------------
    ; Visualizza a schermo l'interfaccia
    ; testuale

        _draw_shell PROC NEAR  

            push    di 
            push    si 
            push    cx 

            mov     si, OFFSET _shell 
            xor     di, di 
            mov     cx, 7D0h
            rep     movsw

            mov     ah, 2h 
            xor     bx, bx 
            mov     dx, 0D05h
            int     10h 

            mov     WORD PTR es:[826h], 0F3Eh 

            pop     cx 
            pop     si 
            pop     di 
            ret  

        _draw_shell ENDP

    ;-----------------------------------------

    _delete_screen PROC NEAR 

        push    ax 
        push    bx 
        push    cx 
        push    di 

        cld 
        mov     cx, 15h 
        mov     di, 0A4h 

        _screen_void:
            push    di
            push    cx 
            mov     ax, 0F20h 
            mov     cx, 4Dh  
            rep     stosw  
            pop     cx 
            pop     di 
            add     di, 0A0h 
            loop    _screen_void

        mov     ah, 2h 
        xor     bx, bx 
        mov     dx, 0103h 
        int     10h 

        mov     ax, 3Eh 
        push    ax 
        call    _draw_char
        add     sp, 2h

        mov     ah, 2h 
        xor     bx, bx 
        add     dl, 2h
        int     10h 

        pop     di 
        pop     cx 
        pop     bx 
        pop     ax
        ret 

    _delete_screen ENDP 
        
    ;-----------------------------------------
    ; Visualizza un caratere nella posizione
    ; corrente del cursore, gestendo il 
    ; ritorno a capo e lo scorrimento 
    ; verticale verso l'alto dello schermo.
    ;
    ; Parametri: carattere da visualizzare 

        _draw_char PROC NEAR 

            push    bp 

            mov     bp, sp 

            push    ax 
            push    bx
            push    cx
            push    dx 
            push    es 

            mov     ax, 0B800h 
            mov     es, ax

            mov     ah, 3h 
            xor     bh, bh
            int     10h 

            cmp     BYTE PTR [bp+5h], 4Bh 
            je      _go_left 
            cmp     BYTE PTR [bp+5h], 4Dh 
            je      _go_right 
            cmp     BYTE PTR [bp+5h], 3Bh 
            je      _change_mode_gr
            jmp     _continue_gr

            _go_left:
                cmp    dl, 5h 
                je     _end_gr 
                dec    dl 
                mov    ah, 2h 
                int    10h 
                jmp    _end_gr 

            _go_right: 
                cmp    dl, 4Bh 
                je     _end_gr 
                inc    dl
                mov    ah, 2h 
                int    10h 
                jmp    _end_gr
                
            _change_mode_gr:
                cmp    BYTE PTR es:[0E73h], 70h 
                je     _ch_to_multi 

                ;change to single
                mov     di, 0E9Bh 
                mov     si, 0E73h 
                mov     cl, 0Fh 
                mov     ch, 10h 
                jmp     _off_on 

                _ch_to_multi:
                    mov    di, 0E73h 
                    mov    si, 0E9Bh
                    mov    cl, 10h 
                    mov    ch, 0Fh 

                _off_on: 
                    mov     al, 0Fh

                    _off:
                        mov    BYTE PTR es:[di], al 
                        add    di, 2h 
                        dec    cl 
                        jnz    _off  

                    mov     al, 70h

                    _on:
                        mov    BYTE PTR es:[si], al 
                        add    si, 2h 
                        dec    ch 
                        jnz    _on
                    
                jmp    _end_gr 

            _continue_gr:
                push    dx
                mov     al, dh 
                mov     cl, 50h 
                mul     cl 
                inc     dl 
                xor     dh, dh 
                add     ax, dx 
                shl     ax, 1h 
                pop     dx

            mov     bx, ax 
            sub     bx, 2h

            cmp     BYTE PTR [bp+4h], 08h 
            je      _delete_char 
            cmp     BYTE PTR [bp+5h], 53h 
            je      _canc_char
            cmp     BYTE PTR [bp+4h], 0Dh 
            jne     _normal_char     

            ;cr char
            cmp     dh, 15h 
            je      _go_up

            mov     ah, 2h 
            mov     dl, 5h 
            inc     dh 
            xor     bh, bh 
            int     10h 

            push    dx
            mov     al, dh 
            mov     cl, 50h 
            mul     cl 
            inc     dl 
            xor     dh, dh 
            add     ax, dx 
            shl     ax, 1h 
            pop     dx

            mov     bx, ax 
            sub     bx, 2h

            mov     WORD PTR es:[bx-4h], 0F3Eh 
            jmp     _end_gr 

            _go_up:
                mov    ah, 2h 
                mov    dl, 5h
                xor    bh, bh 
                int    10h 
                call   _scroll_up_screen
                mov    WORD PTR es:[0D26h], 0F3Eh 
                jmp    _end_gr 

            _canc_char:
                cmp     dl, 4Bh 
                je      _end_gr 

                cmp     dl, 4Ah 
                je      _delete_last_char 

                add     bx, 4h 
                mov     ax, LEFT_SCROLL 
                push    ax 
                call    _horizontal_screen_scroll
                add     sp, 2h 
                jmp     _end_gr 

                _delete_last_char:
                    mov    WORD PTR [bx+2h], 0F00h 
                    jmp    _end_gr 

            _delete_char:
                cmp     dl, 5h 
                je      _end_gr 

                mov     ax, LEFT_SCROLL 
                push    ax 
                call    _horizontal_screen_scroll
                add     sp, 2h
                mov     ah, 02h
                dec     dl 
                xor     bx, bx 
                int     10h 
                jmp     _end_gr 

            _normal_char:
                mov     al, BYTE PTR [bp+4h]
                mov     ah, 0Fh 
                mov     WORD PTR es:[bx], ax 
                inc     dl 
                xor     bh, bh 
                mov     ah, 2h 
                int     10h 

            _end_gr:
                pop     es
                pop     dx   
                pop     cx 
                pop     bx 
                pop     ax 
                pop     bp 
                ret 

        _draw_char ENDP 
    
    ;-----------------------------------------

    ;-----------------------------------------
    ; Effettua lo scorrimento verticale verso
    ; l'alto dello schermo

        _scroll_up_screen PROC NEAR 

            push    si 
            push    di 
            push    cx 
            push    ds

            mov     ax, 0B800h 
            mov     ds, ax 

            mov     di, 0A2h 
            mov     si, 142h 

            _scroll:
                mov     cx, 4Eh 
                rep     movsw 

                add     si, 4h 
                add     di, 4h

                cmp     si, 0DC2h
                jbe     _scroll 

            pop     ds
            pop     cx 
            pop     di 
            pop     si
            ret 

        _scroll_up_screen ENDP 

    ;-----------------------------------------

    _horizontal_screen_scroll PROC NEAR 

        push    ax
        push    bx
        push    dx 
        push    cx
        push    si 
        push    di 
        push    ds 

        mov     ax, 0B800h 
        mov     ds, ax 
       
        cld 
        mov     si, bx 
        sub     bx, 2h 
        mov     di, bx 

        mov     ah, 3h 
        xor     bh, bh 
        int     10h 

        xchg    dl, dh 
        mov     dl, 50h 
        sub     dl, dh 
        sub     dl, 3h 
        xor     dh, dh 
        mov     cx, dx 

        rep     movsw 

        mov     WORD PTR es:[di], 0F00h 
        
        pop     ds
        pop     di 
        pop     si 
        pop     cx
        pop     dx 
        pop     bx
        pop     ax 
        ret     

    _horizontal_screen_scroll ENDP 

_text ENDS 