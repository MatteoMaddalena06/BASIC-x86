;--------------------------------------------------------------------
; Modulo contenente procedure di generica utilità per l'interprete
; BASIC.
;--------------------------------------------------------------------

_set_time_format MACRO 

    push    ax
    mov     al, 0Bh 
    out     70h, al 
    in      al, 71h 
    or      al, 4h 
    push    ax 
    mov     al, 0Bh 
    out     70h, al 
    pop     ax 
    out     71h, al 
    pop     ax

ENDM

_text SEGMENT PARA PUBLIC 

    syn_err_msg          db    "A syntactic error has occurred.", 0h 
    sem_err_std          db    "A semantic error has occured.", 0h 
    sem_err_msg0         db    "Maximum engagement level reached.", 0h 
    sem_err_msg1         db    "Malformed expression.", 0h 
    sem_err_msg2         db    "Cannot use this statement in single line mode.", 0h 
    sem_err_msg3         db    "Line number not compliant with the rules.", 0h 
    gen_err_msg0         db    "Nonconforming types.", 0h 
    gen_err_msg1         db    "Runt time: Nonconforming types.", 0h 
    gen_err_msg2         db    "Redefined symbol.", 0h 
    gen_err_msg3         db    "The length of the strings is different.", 0h 
    gen_err_msg4         db    "String too long.", 0h
    gen_err_msg5         db    "A generation error has occured", 0h 
    generic_err          db    "An error has occured", 0h 
    div_err_msg          db    "Division by 0 was performed. **Aborted program **", 0h 

    help_msg            db    " CLS   - clears the screen.", 0Dh
                        db    " HELP  - show this message.", 0Dh 
                        db    " RESET - allows you to reset the BASIC interpreter.", 0Dh
                        db    " RUN   - allows you to run programs in multi-line mode.", 0Dh
                        db    " SHVT  - allows you to see the variables used by the program.", 0Dh
                        db    " TIME  - allows you to see the current time.", 0Dh, 0h

    hex_conv_table       db    "0123456789ABCDEF"
    int_type_s           db    "INT", 0h 
    str_type_s           db    "STR", 0h
    ins_pres             db     0h 

    ;------------------------------------- 
    ; Permette di acquisire caratteri 
    ; da tastiera, e visualizzare quest 
    ; ultimi.

        _take_input PROC NEAR 

            push    ax
            push    di 

            xor     ah, ah 
            int     16h 

            cmp     ah, 3Bh 
            je      _switch_mod
            cmp     ah, 4Bh 
            je      _left_move_on_buffer
            cmp     ah, 4Dh  
            je      _right_move_on_buffer
            cmp     al, 0Dh 
            je      _car_char 
            cmp     ah, 53h 
            je      _canc_char_on_buffer
            cmp     al, 08h 
            je      _delete_char_on_buffer
            cmp     al, 20h 
            jb      _stop_sub
            cmp     al, 61h 
            jb      _normal_char_store 
            cmp     al, 7Ah 
            ja      _normal_char_store 

            cmp     bx, OFFSET input_buffer+48h 
            je      _stop_sub 
            push    ax 
            call    _draw_char 
            add     sp, 2h 
            and     al, 0DFh
            jmp     _snc 

            _normal_char_store:
                cmp     bx, OFFSET input_buffer+48h 
                je      _stop_sub 
                push    ax 
                call    _draw_char 
                add     sp, 2h 

            _snc:    
                mov     BYTE PTR [bx], al 
                inc     bx
                jmp     _stop_sub

            _car_char:
                push    ax 
                call    _draw_char 
                add     sp, 2h 
                mov     BYTE PTR [bx], al 
                inc     bx
                clc
                jmp     _end_take_input

            _switch_mod:    
                push    ax 
                call    _draw_char 
                add     sp, 2h 
                mov     di, OFFSET mode_interpreter
                cmp     BYTE PTR [di], SINGLE_LINE_MODE
                je      _to_mmode

                ;to smode 
                mov     BYTE PTR [di], SINGLE_LINE_MODE
                jmp     _change_wmode

                _to_mmode:
                    mov    BYTE PTR [di], MULTI_LINE_MODE

                _change_wmode:
                    call    _change_work_mode
                    jmp     _stop_sub 

            _left_move_on_buffer:
                push    ax 
                call    _draw_char 
                add     sp, 2h 
                cmp     bx, OFFSET input_buffer
                je      _stop_sub
                dec     bx 
                jmp     _stop_sub

            _right_move_on_buffer:
                push    ax 
                call    _draw_char 
                add     sp, 2h 
                cmp     bx, OFFSET input_buffer+4Ch 
                je      _stop_sub 
                inc     bx
                jmp     _stop_sub 

            _canc_char_on_buffer:
                push    ax 
                call    _draw_char 
                add     sp, 2h 
                cmp     bx, OFFSET input_buffer+4Ch 
                je      _stop_sub 

                cmp     bx, OFFSET input_buffer+4Ah 
                je      _delete_last_char_on_buf 

                push    bx 
                inc     bx
                jmp     _scroll_char

                _delete_last_char_on_buf:
                    mov     BYTE PTR [bx+1h], 0h 
                    jmp     _stop_sub 

            _delete_char_on_buffer:
                push    ax 
                call    _draw_char 
                add     sp, 2h 
                cmp     bx, OFFSET input_buffer
                je      _stop_sub

                dec     bx 
                push    bx

                _scroll_char:
                    mov     al, BYTE PTR [bx+1h]
                    mov     BYTE PTR [bx], al 
                    inc     bx 
                    cmp     bx, OFFSET input_buffer+4Ch 
                    jb      _scroll_char

                mov     BYTE PTR [bx], 0h
                pop     bx 
                jmp     _stop_sub 

            _stop_sub:
                stc 

            _end_take_input:
                pop     di 
                pop     ax
                ret 

        _take_input ENDP 

    ;-------------------------------------

    ;-------------------------------------
    ; Effettua la chiamate alle procedure
    ; utilizzate per  la compilazione 
    ; dell'istruzione corrente.

        _compilation_work PROC NEAR 

            push     bx   

            call     _tokenize_input 

            mov      bx, OFFSET syn_error
            cmp      BYTE PTR [bx], 1h 
            je       _error_compilation 
            
            mov      bx, OFFSET token_buffer
            cmp      BYTE PTR [bx], SHVT_ID 
            je       _continue_compilation
            cmp      BYTE PTR [bx], RUN_ID 
            je       _continue_compilation 
            cmp      BYTE PTR [bx], TIME_ID 
            je       _continue_compilation
            cmp      BYTE PTR [bx], RESET_ID
            je       _continue_compilation
            cmp      BYTE PTR [bx], CLEAR_ID 
            je       _continue_compilation 
            cmp      BYTE PTR [bx], HELP_ID 
            je       _continue_compilation

            mov      bx, OFFSET error_occured
            cmp      BYTE PTR [bx], 1h 
            je       _no_err_compilation

            _continue_compilation:
                call     _parsing_token 

            mov      bx, OFFSET sem_error   
            cmp      BYTE PTR [bx], 1h 
            je       _error_compilation

            mov      bx, OFFSET token_buffer
            cmp      BYTE PTR [bx], SHVT_ID 
            je       _show_var_table
            cmp      BYTE PTR [bx], RUN_ID 
            je       _run_command 
            cmp      BYTE PTR [bx], TIME_ID 
            je       _show_time 
            cmp      BYTE PTR [bx], RESET_ID 
            je       _do_reset
            cmp      BYTE PTR [bx], CLEAR_ID 
            je       _clear_screen
            cmp      BYTE PTR [bx], HELP_ID 
            je       _help_users

            call     _generate_instruction

            mov      bx, OFFSET gen_error
            cmp      BYTE PTR [bx], 1h 
            je       _error_compilation

            mov      bx, OFFSET ins_pres
            mov      BYTE PTR [bx], 1h

            _no_err_compilation:
                xor      ax, ax 
                jmp      _end_compilation

            _error_compilation:
                mov     ax, 1h 
                jmp     _end_compilation

            _show_var_table:
                call   _write_var_table 
                jmp    _end_run

            _run_command:
                mov    bx, OFFSET mode_interpreter
                cmp    BYTE PTR [bx], SINGLE_LINE_MODE
                je     _end_run 

                mov    bx, OFFSET error_occured
                cmp    BYTE PTR [bx], 1h 
                jne    _execute 

                call   _print_err_msg
                call   _clear_flags
                xor    ax, ax 
                jmp    _end_compilation

            _execute:
                call   _execute_program
                jmp    _end_run 

            _do_reset:
                ;restart system
                mov    ax, 40h 
                mov    es, ax 
                mov    WORD PTR es:[72h], 0h 
                farjmp  0FFFFh, 0h

            _clear_screen:
                call   _delete_screen
                jmp    _end_run

            _help_users:
                call    _print_all_commands 
                jmp     _end_run

            _show_time:
                call   _print_current_time 
            
            _end_run:
                mov    ax, 2h 

            _end_compilation:
                pop     bx
                ret 

        _compilation_work ENDP

    ;-------------------------------------

    ;-------------------------------------
    ; Visualizza un mesaggio di errore
    ; inerente all'erore verificatosi.

        _print_err_msg PROC NEAR 

            push    bx 

            mov     bx, OFFSET error_occured
            cmp     BYTE PTR [bx], 1h 
            je      _gener_err 

            mov     bx, OFFSET syn_error
            cmp     BYTE PTR [bx], 1h 
            je      _syn_error_msg

            mov     bx, OFFSET max_innest
            cmp     BYTE PTR [bx], 1h 
            je      _sem_mi_error_msg

            mov     bx, OFFSET exp_error
            cmp     BYTE PTR [bx], 1h 
            je      _sem_exp_error_msg

            mov     bx, OFFSET multi_line_err
            cmp     BYTE PTR [bx], 1h 
            je      _sem_mu_error_msg

            mov     bx, OFFSET line_number_err
            cmp     BYTE PTR [bx], 1h 
            je      _sem_ln_error_msg

            mov     bx, OFFSET zero_division
            cmp     BYTE PTR [bx], 1h
            je      _divided_by_0

            mov     bx, OFFSET type_of_error
            cmp     BYTE PTR [bx], 1h 
            je      _gen_ty_error_msg 

            cmp     BYTE PTR [bx], 3h 
            je      _gen_rd_error_msg 

            cmp     BYTE PTR [bx], 4h 
            je      _gen_lg_error_msg 

            cmp     BYTE PTR [bx], 2h 
            je      _gen_rt_error_msg 

            cmp     BYTE PTR [bx], 5h 
            je      _gen_tls_error_msg 

            mov     bx, OFFSET gen_error
            cmp     BYTE PTR [bx], 1h 
            je      _ngen_error 

            ;standard semantic error 
            mov     bx, OFFSET sem_err_std
            jmp     _write_error_msg

            _gen_rt_error_msg:
                ;run time gen_err 
                mov    bx, OFFSET gen_err_msg1
                jmp    _write_error_msg

            _syn_error_msg:
                mov    bx, OFFSET syn_err_msg
                jmp    _write_error_msg 

            _sem_mi_error_msg:
                mov    bx, OFFSET sem_err_msg0
                jmp    _write_error_msg

            _sem_exp_error_msg:
                mov    bx, OFFSET sem_err_msg1
                jmp    _write_error_msg

            _sem_mu_error_msg:
                mov    bx, OFFSET sem_err_msg2
                jmp    _write_error_msg

            _sem_ln_error_msg:
                mov    bx, OFFSET sem_err_msg3
                jmp    _write_error_msg

            _gen_ty_error_msg:
                mov    bx, OFFSET gen_err_msg0
                jmp    _write_error_msg

            _gen_rd_error_msg:
                mov    bx, OFFSET gen_err_msg2
                jmp    _write_error_msg

            _gen_tls_error_msg:
                mov    bx, OFFSET gen_err_msg4
                jmp    _write_error_msg

            _gen_lg_error_msg:
                mov    bx, OFFSET gen_err_msg3
                jmp    _write_error_msg

            _divided_by_0:
                mov    bx, OFFSET div_err_msg 
                jmp    _write_error_msg

            _ngen_error:
                mov    bx, OFFSET gen_err_msg5
                jmp    _write_error_msg

            _gener_err:
                mov    bx, OFFSET generic_err

            _write_error_msg:
                push    WORD PTR [bx]
                call    _draw_char 
                add     sp, 2h 
                inc     bx 
                cmp     BYTE PTR [bx], 0h 
                jne     _write_error_msg

            mov    ax, 0Dh 
            push   ax
            call   _draw_char 
            add    sp, 2h 
            pop    bx 
            ret 

        _print_err_msg ENDP  

    ;-------------------------------------

    ;-------------------------------------
    ; Pone a zero i flag di errore 
    ; impostati dai vari moduli dell'
    ; interprete.

        _clear_flags PROC NEAR 

            push    bx 

            mov     bx, OFFSET error_occured
            mov     BYTE PTR [bx], 0h 

            mov     bx, OFFSET syn_error
            mov     BYTE PTR [bx], 0h 

            mov     bx, OFFSET gen_error
            mov     BYTE PTR [bx], 0h 

            mov     bx, OFFSET type_of_error
            mov     BYTE PTR [bx], 0h 

            mov     bx, OFFSET sem_error
            mov     BYTE PTR [bx], 0h 

            mov     bx, OFFSET max_innest
            mov     BYTE PTR [bx], 0h 

            mov     bx, OFFSET line_number_err
            mov     BYTE PTR [bx], 0h 

            mov     bx, OFFSET multi_line_err
            mov     BYTE PTR [bx], 0h

            mov     bx, OFFSET exp_error
            mov     BYTE PTR [bx], 0h

            mov     bx, OFFSET zero_division
            mov     BYTE PTR [bx], 0h 

            pop     bx 
            ret 

        _clear_flags ENDP   

    ;-------------------------------------

    ;-------------------------------------
    ; Ripistina lo stato dell'interprete
    ; per effettuare il cambio  di 
    ; modalità.

        _change_work_mode PROC NEAR 

            push    bx

            mov     bx, OFFSET current_ili
            mov     WORD PTR [bx], 0h 

            mov     bx, OFFSET current_ovt
            mov     WORD PTR [bx], 0h 

            mov     bx, OFFSET current_vip
            mov     WORD PTR [bx], OFFSET_USR_CODE+2h

            mov     bx, OFFSET num_of_var
            mov     BYTE PTR [bx], 0h 

            mov     bx, OFFSET num_of_linfo
            mov     BYTE PTR [bx], 0h 

            mov     bx, OFFSET stack_offset
            mov     WORD PTR [bx], 0h 

            mov     bx, OFFSET last_line_value 
            mov     BYTE PTR [bx], 0h 

            mov     bx, OFFSET ins_pres
            mov     BYTE PTR [bx], 0h 

            call    _clear_flags 

            pop     bx 
            ret 
            
        _change_work_mode ENDP 

    ;-------------------------------------

    ;-------------------------------------
    ; Visualizza il contenuto della 
    ; tabella delle variabili.

    _write_var_table PROC NEAR 

        push    ax 
        push    bx
        push    cx
        push    si 
        push    di

        mov     si, OFFSET num_of_var
        mov     cl, BYTE PTR [si]
        xor     ch, ch
        mov     si, OFFSET var_table
        mov     di, OFFSET hex_conv_table 

        test    cx, cx 
        jz      _end_write 
        jmp     _read_data_on_vat

        _control:
            loop    _read_data_on_vat
            jmp     _end_write 

        _read_data_on_vat:
            push    cx 
            mov     cx, 3h  
            xor     bx, bx

            _read_name_var:
                mov     al, BYTE PTR [si]
                test    al, al 
                jz      _next_char 

                push    ax
                call    _draw_char
                add     sp, 2h 
                inc     bx

            _next_char: 
                inc     si 
                loop    _read_name_var 

            mov     ax, 3Ah 
            push    ax 
            call    _draw_char
            add     sp ,2h

            mov     cx, 3h 
            sub     cx, bx

            _write_space:
                mov     ax, 20h 
                push    ax
                call    _draw_char
                add     sp, 2h 
                loop    _write_space  

            mov     bx, WORD PTR [si]
            sub     bx, 5h
            mov     ax, 0F000h 
            mov     cx, 0Ch 

            _read_offset_var:
                push    bx
                and     bx, ax 
                shr     bx, cl 
                push    WORD PTR [bx+di]
                call    _draw_char
                add     sp, 2h
                push    cx 
                mov     cl, 4h 
                shr     ax, cl 
                pop     cx 
                pop     bx
                sub     cx, 4h 
                cmp     cx, 0h 
                jge     _read_offset_var

            mov     ax, 20h 
            push    ax 
            call    _draw_char
            add     sp, 2h 

            mov     ax, 0B3h 
            push    ax
            call    _draw_char
            add     sp, 2h

            mov     ax, 20h 
            push    ax 
            call    _draw_char
            add     sp, 2h 
                
            add     si, 2h 
            cmp     BYTE PTR [si], INT_TYPE
            je      _write_int 
            cmp     BYTE PTR [si], FOR_TYPE 
            je      _write_int 

            ;is string 
            mov     bx, OFFSET str_type_s
            jmp     _write_type 

            _write_int:
                mov    bx, OFFSET int_type_s

            _write_type:
                push    WORD PTR [bx]
                call    _draw_char
                add     sp, 2h 
                inc     bx 
                cmp     BYTE PTR [bx], 0h 
                jne     _write_type

            mov     ax, 0Dh 
            push    ax 
            call    _draw_char
            add     sp, 2h

            add     si, 2h

            pop     cx 
            jmp     _control 
    
    _end_write:
        pop    di 
        pop    si 
        pop    cx
        pop    bx 
        pop    ax 
        ret 

    _write_var_table ENDP 

    ;-------------------------------------

    ;-------------------------------------
    ; Visualizza l'ora corrente.

    _print_current_time PROC NEAR 

        push    ax 
        push    bx 
        push    cx 
        push    dx 

        ;read hours 
        mov     al, 4h 
        out     70h, al  
        in      al, 71h 

        xor     cx, cx
        mov     bl, 0Ah 

        _take_h_digit:
            xor    ah, ah
            div    bl 

            mov    dl, ah 
            xor    dh, dh 
            add    dx, 30h 
            push   dx 
            inc    cx 

            test   al, al 
            jnz    _take_h_digit

        _print_h_digit:
            call   _draw_char
            add    sp, 2h 
            loop   _print_h_digit

        mov     ax, 3Ah 
        push    ax 
        call    _draw_char
        add     sp, 2h

        ;read  minutes
        mov     al, 2h 
        out     70h, al  
        in      al, 71h 

        xor     cx, cx
        mov     bl, 0Ah 

        _take_m_digit:
            xor    ah, ah
            div    bl 

            mov    dl, ah 
            xor    dh, dh 
            add    dx, 30h 
            push   dx 
            inc    cx 

            test   al, al 
            jnz    _take_m_digit

        _print_m_digit:
            call   _draw_char
            add    sp, 2h 
            loop   _print_m_digit

        mov     ax, 3Ah 
        push    ax 
        call    _draw_char
        add     sp, 2h

        ;read second
        xor     al, al  
        out     70h, al  
        in      al, 71h 

        xor     cx, cx
        mov     bl, 0Ah 

        _take_s_digit:
            xor    ah, ah
            div    bl 

            mov    dl, ah 
            xor    dh, dh 
            add    dx, 30h 
            push   dx 
            inc    cx 

            test   al, al 
            jnz    _take_s_digit

        _print_s_digit:
            call   _draw_char
            add    sp, 2h 
            loop   _print_s_digit

        mov     ax, 0Dh 
        push    ax
        call    _draw_char
        add     sp, 2h 

        pop     dx 
        pop     cx 
        pop     bx 
        pop     ax 
        ret  

    _print_current_time ENDP 

    ;-------------------------------------

    ;-------------------------------------
    ; Visualizza le informazioni riguardo
    ; i comandi

    _print_all_commands PROC NEAR 

        push    bx 

        mov     bx, OFFSET help_msg

        _write_help:
            cmp    BYTE PTR [bx], 0h 
            je     _end_write_help 

            mov    al, BYTE PTR [bx]
            xor    ah, ah 
            push   ax
            call   _draw_char
            add    sp, 2h 
            inc    bx
            jmp    _write_help

        _end_write_help:
            pop   bx 
            ret 

    _print_all_commands ENDP 

    ;-------------------------------------

    ;-------------------------------------
    ; Esegue il programma BASIC, e 
    ; memorizza lo stato del processore
    ; prima dell'esecuzione del programma.

    _execute_program PROC NEAR 

        push    ax
        push    bx 

        call    _complete_program
        
        mov     bx, OFFSET saved_ss
        mov     WORD PTR [bx], ss 
        mov     bx, OFFSET saved_sp
        mov     WORD PTR [bx], sp

        mov     ax, SEGMENT_USR_STACK
        mov     ss, ax 
        mov     sp, OFFSET_USR_STACK

        farcall    SEGMENT_USR_CODE, OFFSET_USR_CODE

        mov     ax, 0B800h 
        mov     es, ax
        mov     bx, OFFSET saved_ss
        mov     ss, WORD PTR [bx]
        mov     bx, OFFSET saved_sp 
        mov     sp, WORD PTR [bx]

        pop     bx 
        pop     ax 
        ret 

    _execute_program ENDP 

    ;-------------------------------------

    ;-------------------------------------
    ; Inserisce le istruzioni necessarie
    ; per l'esecuzione del programma.

    _complete_program PROC NEAR 

        push    bx 
        push    es 

        mov     ax, SEGMENT_USR_CODE
        mov     es, ax 

        mov     bx, OFFSET_USR_CODE
        mov     WORD PTR es:[bx], 0E589h ;mov   bp, sp 

        mov     bx, OFFSET ins_pres
        cmp     BYTE PTR [bx], 1h 
        je      _store_return_

        mov     bx, OFFSET_USR_CODE+2h 
        jmp     _continue_complete

    _store_return_: 
        mov     bx, OFFSET where_store_int
        mov     bx, WORD PTR [bx]

    _continue_complete:
        push    bx 
        mov     bx, OFFSET stack_offset
        mov     ax, WORD PTR [bx]
        neg     ax 
        sub     ax, 5h 
        pop     bx 
        mov     BYTE PTR es:[bx], 0BCh ;mov   sp, imm16
        mov     WORD PTR es:[bx+1h], ax
        mov     WORD PTR es:[bx+3h], 24CDh ;int   24h 

        pop     es 
        pop     bx 
        ret 

    _complete_program ENDP 

    ;-------------------------------------

_text ENDS 