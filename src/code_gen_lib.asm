;----------------------------------------------------------------
; Il seguente modulo contiene le principali procedure utilizzate
; dal generatore di codice
;----------------------------------------------------------------

;-------------------------------------------------------
; Code generation Macro

    NAME_STORE         EQU   0h 
    OFFSET_STORE       EQU   1h 
    TYPE_STORE         EQU   2h
    DIM_STORE          EQU   3h 
    VARI_STORE         EQU   4h 

    STRING_TYPE        EQU   0h 
    INT_TYPE           EQU   1h 

    NONE               EQU   0h
    WHILE_TYPE         EQU   1h 
    FOR_TYPE           EQU   2h 

    WHILE_SEARCH       EQU   1h 
    FOR_SEARCH         EQU   2h 
    INS_TO_CONS        EQU   3h 
    CONS_TO_INS        EQU   4h

    VAR_STRING         EQU   2h 
    STRING_CHAR        EQU   3h

    EQU_CON            EQU   0h 
    BOR_CON            EQU   1h 
    BORE_CON           EQU   2h 
    GRT_CON            EQU   3h 
    GRTE_CON           EQU   4h 
    NEQU_CON           EQU   5h

;-------------------------------------------------------

_text SEGMENT PARA PUBLIC

    type_of_error          db     0h 

    ;----------------------------------------------------------
    ; Memorizza le informazione nella tabella delle variabili
    ; ed effettua il controllo dei tipi.
    ;
    ; valore di ritorno = 
    ;       0.in caso di sucesso.
    ;       1.in caso di errore.

        _create_var PROC NEAR 

            push    cx 
            push    dx
            push    si 
            push    di 

            xor     cx, cx

            cmp     BYTE PTR [si], FOR_ID 
            je      _is_for_var

            token_in_al   BYTE PTR [si]
            cmp     al, IDENTIFIER_TOKEN
            jne     _is_declaration_ins 

            push    WORD PTR [si+2h]
            call    _search_var_in_vat
            add     sp, 2h 

            test    ax, 0FFh 
            jz      _like_decl 
            jmp     _type_control 

            _is_declaration_ins:
                inc     si 
                push    WORD PTR [si+2h]
                call    _search_var_in_vat
                add     sp, 2h 

                test    ax, 0FFh 
                jnz     _red_error

            _like_decl:
                mov     al, BYTE PTR [si+1h]
                push    ax 
                push    WORD PTR [si+2h]
                mov     ax, NAME_STORE 
                push    ax 
                call    _store_element_in_vat
                add     sp, 6h 

                cmp     BYTE PTR [si+4h], BRCKL_ID
                je      _is_string 
                cmp     BYTE PTR [si+4h], EOT
                je      _is_value

                add     si, 5h 
                cmp     BYTE PTR [si], ANGBRCKL_ID
                je      _is_exp 
                cmp     BYTE PTR [si], SUB_ID 
                jne     _l0 

                inc     si 

            _l0:
                token_in_al   BYTE PTR [si]
                cmp     al, VALUE_TOKEN
                je      _is_value 
                cmp     al, INSTRUCTION_TOKEN
                je      _is_value
                cmp     al, STRING_TOKEN
                je      _error

                ;is identifier 
                push    WORD PTR [si+2h]
                call    _search_var_in_vat
                add     sp, 2h 

                test    ax, 0FFh 
                jz      _is_value

                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]

                cmp     BYTE PTR [di+5h], INT_TYPE
                je      _is_value
                jmp     _error_type

                _is_string:
                    add     si, 5h
                    mov     dx, WORD PTR [si+1h]
                    test    dh, dh  
                    jnz     _string_too_long 
                    
                    add     si, 5h
                    token_in_al  BYTE PTR [si]
                    cmp     al, STRING_TOKEN
                    je      _is_string_

                    push    WORD PTR [si+2h]
                    call    _search_var_in_vat
                    add     sp, 2h 
                    
                    test    ax, 0FFh 
                    jz      _error

                    ;string id 
                    mov     di, OFFSET roffset_ovt
                    mov     di, WORD PTR [di]
                    mov     al, BYTE PTR [di+6h]

                    cmp     al, dl
                    jne     _error 
                    mov     cl, al 
                    jmp     _store_string 

                    _is_string_:
                        mov     si, WORD PTR [si+2h]
                        xor     cx, cx
                        
                        _loop_string:
                            cmp    BYTE PTR [si], 22h
                            je     _stop_string 
                            cmp    BYTE PTR [si], 27h 
                            je     _stop_string
                            inc    cl 
                            inc    si 
                            jmp    _loop_string 

                        _stop_string:
                            cmp    cx, dx 
                            jne    _error_lenght

                    _store_string:
                        xor     ch, ch
                        mov     di, OFFSET stack_offset
                        inc     cx
                        add     WORD PTR [di], cx 
                        push    WORD PTR [di]
                        mov     ax, OFFSET_STORE 
                        push    ax 
                        call    _store_element_in_vat
                        add     sp, 4h

                        mov     ax, STRING_TYPE
                        push    ax 
                        mov     ax, TYPE_STORE
                        push    ax 
                        call    _store_element_in_vat
                        add     sp, 4h 

                        dec     cx
                        push    cx 
                        mov     ax, DIM_STORE
                        push    ax 
                        call    _store_element_in_vat
                        add     sp, 4h
                        mov     di, OFFSET num_of_var
                        inc     BYTE PTR [di]
                        jmp     _no_err 

                _is_exp:
                    inc    si 
                
                    _loop_:
                        token_in_al   BYTE PTR [si]
                        cmp     al, VALUE_TOKEN
                        je      _check_value_exp
                        cmp     al, IDENTIFIER_TOKEN
                        jne     _next_exp   

                        ;is identifier 
                        cmp     BYTE PTR [si+4h], OFF_ID 
                        je      _error_type
                        cmp     BYTE PTR [si+4h], NOT_ID 
                        je      _error_type 

                        push    WORD PTR [si+2h]
                        call    _search_var_in_vat
                        add     sp, 2h

                        add    si, 3h 

                        test    ax, 0FFh 
                        jz      _next_exp

                        mov     di, OFFSET roffset_ovt
                        mov     di, WORD PTR [di]
                        cmp     BYTE PTR [di+5h], STRING_TYPE
                        je      _error_type
                        jmp     _next_exp

                    _check_value_exp:
                        cmp    BYTE PTR [si+3h], OFF_ID 
                        je     _error_type
                        cmp    BYTE PTR [si+3h], NOT_ID 
                        je     _error_type 
                        add    si, 2h

                    _next_exp:
                        inc    si 
                        cmp    BYTE PTR [si], ANGBRCKR_ID
                        jne    _loop_ 
                        cmp    cx, 0FFh 
                        je     _no_err 

                _is_value: 
                    mov     di, OFFSET stack_offset
                    add     WORD PTR [di], 2h 
                    push    WORD PTR [di]
                    mov     ax, OFFSET_STORE 
                    push    ax 
                    call    _store_element_in_vat
                    add     sp, 4h

                    mov     ax, INT_TYPE
                    push    ax 
                    mov     ax, TYPE_STORE
                    push    ax 
                    call    _store_element_in_vat
                    add     sp, 4h 

                    mov     ax, 0h 
                    push    ax 
                    mov     ax, TYPE_STORE
                    push    ax 
                    call    _store_element_in_vat
                    add     sp, 4h 
                    mov     di, OFFSET num_of_var
                    inc     BYTE PTR [di]
                    jmp     _no_err 

            _type_control:
                cmp     BYTE PTR [si+4h], BRCKL_ID 
                je      _error 
                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]
                mov     al, BYTE PTR [di+5h]
                mov     dl, BYTE PTR [di+6h]

                cmp     al, FOR_TYPE 
                jne     _continue_control 

                mov     al, INT_TYPE 

            _continue_control:
                add     si, 5h 
                mov     cx, 0FFh 
                cmp     BYTE PTR [si], ANGBRCKL_ID
                je      _is_exp
                push    ax
                cmp     BYTE PTR [si], SUB_ID 
                je      _is_value_type
                token_in_al    BYTE PTR [si]
                cmp     al, VALUE_TOKEN
                je      _is_value_type 
                cmp     al, IDENTIFIER_TOKEN
                je      _is_identifier_type 
                cmp     al, INSTRUCTION_TOKEN
                je      _is_value_type
                
                ;is string 
                pop     ax 
                cmp     al, STRING_TYPE
                jne     _error_type 

                xor     dh, dh 
                mov     si, WORD PTR [si+2h]

                _loop_control:
                    cmp    BYTE PTR [si], 22h 
                    je     _end_control 
                    cmp    BYTE PTR [si], 27h 
                    je     _end_control 
                    inc    si 
                    inc    dh 
                    jmp    _loop_control 

                _end_control:
                    cmp    dl, dh 
                    jne    _error_lenght
                    jmp    _no_err 

                _is_value_type:
                    pop    ax
                    cmp    al, INT_TYPE
                    je     _no_err 
                    jmp    _error_type

                _is_identifier_type:
                    push    WORD PTR [si+2h]
                    call    _search_var_in_vat
                    add     sp, 2h 

                    test    ax, 0FFh 
                    pop     ax 
                    jz      _is_value_type

                    mov     di, OFFSET roffset_ovt
                    mov     di, WORD PTR [di]
                    cmp     al, BYTE PTR [di+5h]
                    jne     _error_type

                    cmp     al, INT_TYPE 
                    je      _no_err

                    cmp     dl, BYTE PTR [di+6h]
                    jne     _error_lenght
                    jmp     _no_err 

            _is_for_var:
                xor     dx, dx
                inc     si 
                push    WORD PTR [si+2h]
                call    _search_var_in_vat
                add     sp, 2h 

                test    ax, ax 
                jnz     _no_store_same 

                mov     al, BYTE PTR [si+1h]
                push    ax 
                push    WORD PTR [si+2h]
                mov     ax, NAME_STORE  
                push    ax 
                call    _store_element_in_vat
                add     sp, 6h 

                mov     di, OFFSET stack_offset
                add     WORD PTR [di], 2h 
                push    WORD PTR [di]
                mov     ax, OFFSET_STORE 
                push    ax 
                call    _store_element_in_vat
                add     sp, 4h

            _no_store_same1:
                mov     ax, FOR_TYPE 
                push    ax 
                mov     ax, TYPE_STORE 
                push    ax 
                call    _store_element_in_vat 
                add     sp, 4h 

                _loop_for:
                    token_in_al   BYTE PTR [si]
                    cmp    al, VALUE_TOKEN
                    je     _skip_3 
                    cmp    al, STRING_TOKEN
                    je     _skip_4
                    cmp    al, IDENTIFIER_TOKEN
                    jne    _continue_loop_for

                _skip_4:
                    add    si, 4h 
                    jmp    _continue_loop_for

                _skip_3:
                    add    si, 3h

                _continue_loop_for:
                    cmp    BYTE PTR [si], EOT 
                    je     _loop_for_end 
                    inc    si 
                    jmp    _loop_for 

                _loop_for_end:
                    sub    si, 3h
                    mov    ax, WORD PTR [si] 

                push    ax 
                mov     ax, TYPE_STORE
                push    ax 
                call    _store_element_in_vat
                add     sp, 4h  
                test    dx, dx 
                jz      _continue_for_store 

                mov     bx, OFFSET current_ovt
                pop     WORD PTR [bx]

            _continue_for_store:
                mov     di, OFFSET num_of_var
                inc     BYTE PTR [di]
                jmp     _no_err 

            _no_store_same:
                mov     bx, OFFSET roffset_ovt
                mov     ax, WORD PTR [bx]
                sub     ax, OFFSET var_table
                add     ax, 5h
                mov     bx, OFFSET current_ovt
                push    WORD PTR [bx]
                mov     WORD PTR [bx], ax 
                mov     dx, 1h 
                jmp     _no_store_same1

            _error_type:
                mov     di, OFFSET type_of_error
                mov     BYTE PTR [di], 1h 
                jmp     _error 

            _red_error:
                mov    di, OFFSET type_of_error
                mov    BYTE PTR [di], 3h
                jmp    _error

            _error_lenght:
                mov    di, OFFSET type_of_error
                mov    BYTE PTR [di], 4h 
                jmp    _error

            _string_too_long:
                mov    di, OFFSET type_of_error 
                mov    BYTE PTR [di], 5h 

            _error:
                mov    ax, 1h 
                jmp    _end_var 

            _no_err:
                xor    ax, ax 

            _end_var:
                pop    di 
                pop    si 
                pop    dx
                pop    cx 
                ret 

        _create_var ENDP

    ;----------------------------------------------------------

    ;----------------------------------------------------------
    ; Memorizza un elemente all'interno della var table
    ;
    ; parametri = elemento da memorizzare e tipo di questo.

        _store_element_in_vat PROC NEAR 

            push    bp 

            mov     bp, sp 

            push    ax 
            push    cx 
            push    si 
            push    di 
            push    es 

            mov     ax, BASIC_SEG 
            mov     es, ax 

            mov     di, OFFSET current_ovt
            push    di 
            mov     di, WORD PTR [di]
            add     di, OFFSET var_table 

            cmp     BYTE PTR [bp+4h], OFFSET_STORE
            je      _store_offset 
            cmp     BYTE PTR [bp+4h], TYPE_STORE
            je      _store_type 
            cmp     BYTE PTR [bp+4h], DIM_STORE 
            je      _store_type 
            cmp     BYTE PTR [bp+4h], VARI_STORE 
            je      _store_type 

            ;store the name 
            xor     ch, ch 
            mov     cl, BYTE PTR [bp+8h]
            mov     si, WORD PTR [bp+6h]
            rep     movsb 
            mov     BYTE PTR [di], 0h 
            mov     ax, 3h 
            jmp     _inc_offset

            _store_offset:
                mov     ax, WORD PTR [bp+6h]
                
                neg     ax

                mov     WORD PTR [di], ax 
                mov     ax, 2h 
                jmp     _inc_offset

            _store_type:
                mov     al, BYTE PTR [bp+6h]
                mov     BYTE PTR [di], al 
                mov     ax, 1h  
                jmp     _inc_offset

            _inc_offset:
                pop     di 
                add     WORD PTR [di], ax 

            _end_store:
                pop    es 
                pop    di 
                pop    si 
                pop    cx 
                pop    ax 
                pop    bp 
                ret 

        _store_element_in_vat ENDP
    
    ;----------------------------------------------------------

    ;----------------------------------------------------------
    ; Controlla se una varibile ├¿ presente all'interno della 
    ; var_table
    ;
    ; parametri = offset variabile.
    ;
    ; valore di ritorno in AX = 0. no presente / 1. presente

        _search_var_in_vat PROC NEAR 

            push    bp 

            mov     bp, sp 

            push    cx 
            push    si 
            push    di 

            mov     si, OFFSET num_of_var
            mov     cx, WORD PTR [si]

            mov     di, OFFSET var_table
            mov     si, WORD PTR [bp+4h]

            test    cx, cx
            jz      _no_equ 

            _search_string:
                push    di 
                push    cx 
                push    si 
                mov     cx, 2h 

                _loop:
                    mov    al, BYTE PTR [si]
                    cmp    BYTE PTR [di], al 
                    jne    _possible_no_equ 
                    inc    si 
                    inc    di 
                    loop   _loop 
                    jmp    _find_it

                    _possible_no_equ:
                        cmp    BYTE PTR [si], 41h
                        jb     _continue 
                        cmp    BYTE PTR [si], 5Ah 
                        ja     _continue 
                        jmp    _next_it 

                    _continue:
                        cmp    BYTE PTR [di], 0h 
                        je     _find_it 

            _next_it:
                pop     si 
                pop     cx 
                add     sp, 2h
                add     di, 7h 
                loop    _search_string

            _no_equ:
                xor    ax, ax 
                jmp    _end  

            _find_it:
                add     sp, 4h
                pop     di 
                mov     ax, 1h 
                mov     si, OFFSET roffset_ovt
                mov     WORD PTR [si], di 

            _end:
                pop    di 
                pop    si 
                pop    cx 
                pop    bp 
                ret 

        _search_var_in_vat ENDP 

    ;----------------------------------------------------------

    ;----------------------------------------------------------
    ; Completa le istruzioni di salto 
    ;
    ; Parametri:
    ;     - Tipo di controllo da effettuare 

        _complete_jmp_ins PROC NEAR 

            push    bp 

            mov     bp, sp 

            push    ax
            push    bx
            push    cx 
            push    si 
            push    di 
        
            mov     si, OFFSET num_of_linfo
            mov     cl, BYTE PTR [si]  
            xor     ch, ch
            mov     si, OFFSET current_ili
            mov     si, WORD PTR [si]
            sub     si, 6h 
            add     si, OFFSET line_info 

            cmp      WORD PTR [bp+4h], WHILE_SEARCH
            je       _is_while 
            cmp      WORD PTR [bp+4h], FOR_SEARCH 
            je       _is_for 

            ;any instruction or if and goto instruction
            cmp      WORD PTR [bp+4h], INS_TO_CONS
            je       _some_want_jump_here 

            ;check if line is present
            mov      al, BYTE PTR [si+4h] 

            _loop:
                cmp     al, BYTE PTR [si] 
                je      _find_line 
                sub     si, 6h 
                loop    _loop 
                jmp     _end 

            _find_line: 
                push    cx 
                push    di 
                mov     cx, di 
                mov     di, WORD PTR [si+1h]
                sub     cx, di 
                pop     di 
                neg     cx 
                mov     WORD PTR es:[di-2h], cx 
                pop     cx 
                loop    _loop 
                jmp     _end  

            _some_want_jump_here: 
                mov     al, BYTE PTR [si]
                mov     bx, WORD PTR [si+1h]
                dec     cx 

                _loop1:
                    jz      _end 
                    sub     si, 6h 
                    cmp     al, BYTE PTR [si+4h]
                    je      _find_line1
                    dec     cx 
                    jmp     _loop1  

                _find_line1:
                    push    bx
                    mov     di, WORD PTR [si+7h]
                    sub     bx, di 
                    mov     WORD PTR es:[di-2h], bx 
                    dec     cx 
                    pop     bx
                    jmp     _loop1 

            _is_while:
                xor    bx, bx
                mov    al, WHILE_TYPE
                jmp    _iterative_ins

            _is_for:
                mov    bx, 1h 
                mov    al, FOR_TYPE

            _iterative_ins: 
                cmp     BYTE PTR [si+3h], al 
                je      _find_it 
                sub     si, 6h 
                loop    _iterative_ins

            _find_it:
                mov     BYTE PTR [si+3h], 0h 
                push    di 
                mov     cx, di 
                mov     di, WORD PTR [si+7h]
                sub     cx, di 
                test    bx, bx 
                jz      _no_sub 

                mov     ax, WORD PTR [si+0Bh]
                xor     ah, ah 
                sub     di, ax
                add     cx, ax

            _no_sub:
                mov     WORD PTR es:[di-2h], cx 
                pop     di 
                push    di 
                mov     cx, di 
                mov     di, WORD PTR [si+1h]
                sub     cx, di 
                pop     di
                neg     cx 
                mov     WORD PTR es:[di-2h], cx

            _end:
                pop     di 
                pop     si 
                pop     cx 
                pop     bx 
                pop     ax 
                pop     bp
                ret 

        _complete_jmp_ins ENDP 

    ;----------------------------------------------------------

    ;----------------------------------------------------------
    ; Genera le istruzioni in linguaggio macchina per le 
    ; istruzioni per il controllo del flusso logico del
    ; programma.
    ;
    ; Prametri:
    ;    -Tipo di istruzione NB. necessario solo con while 
    
        _gen_code_for_cond PROC NEAR 

            push    bp 

            mov     bp, sp 
 
            push    bx 
            push    cx
            push    si 
            
            token_in_al   BYTE PTR [si]
            cmp     al, VALUE_TOKEN
            je      _is_value 
            cmp     al, IDENTIFIER_TOKEN
            je      _is_identifier 

            ;is string 
            push    si 
            mov     si, WORD PTR [si+2h]
            xor     cx, cx 
            xor     dx, dx

            mov     bx, OFFSET stack_offset
            mov     ax, WORD PTR [bx]
            
            neg     ax 
            sub     ax, 5h 

            mov     BYTE PTR es:[di], 0BCh ;mov   sp, imm16 
            mov     WORD PTR es:[di+1h], ax 
            add     di, 3h

            _go_end_string:
                cmp     BYTE PTR [si+1h], 22h 
                je      _end_go 
                cmp     BYTE PTR [si+1h], 27h 
                je      _end_go 
                inc     si 
                jmp     _go_end_string

            _end_go:
                mov     WORD PTR es:[di], 0C031h ;xor   ax, ax 
                mov     BYTE PTR es:[di+2h], 50h ;push  ax 
                add     di, 3h
                add     cx, 2h 

                _store_string_on_stack:
                    cmp     BYTE PTR [si], 22h 
                    je      _end_sstore 
                    cmp     BYTE PTR [si], 27h 
                    je      _end_sstore 
                    mov     al, BYTE PTR [si]
                    xor     ah, ah 
                    mov     BYTE PTR es:[di], 0B8h  ;mov   ax, imm16 
                    mov     WORD PTR es:[di+1h], ax 
                    mov     BYTE PTR es:[di+3h], 50h ;push   ax 
                    add     di, 4h 
                    add     cx, 2h
                    dec     si 
                    jmp     _store_string_on_stack

                _end_sstore:
                    mov     BYTE PTR es:[di], 0B8h  ;mov   ax, STRING_TYPE 
                    mov     WORD PTR es:[di+1h], STRING_CHAR
                    mov     BYTE PTR es:[di+3h], 50h   ;push  ax 
                    add     di, 4h
                    add     cx, 2h 
                    cmp     dx, 1h 
                    je      _active_int

                pop      si 
                add      si, 4h 
                push     si 
                inc      si 

                token_in_al    BYTE PTR [si]
                cmp     al, VALUE_TOKEN
                je      _second_cmp_is_value
                cmp     al, STRING_TOKEN
                je      _string_char 

                ;is string identifier
                push    WORD PTR [si+2h]
                call    _search_var_in_vat
                add     sp, 2h 

                test    ax, 0FFh 
                jz      _type_error

                push    di 
                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]
                mov     bx, WORD PTR [di+3h]
                cmp     BYTE PTR [di+5h], STRING_TYPE 
                pop     di 
                jne     _type_error

                sub     bx, 5h

                mov     BYTE PTR es:[di], 0B8h ;mov  ax, OFFSET var
                mov     WORD PTR es:[di+1h], bx 
                mov     BYTE PTR es:[di+3h], 50h ;push  ax
                add     di, 4h
                mov     BYTE PTR es:[di], 0B8h ;mov  ax, TYPE_STRING
                mov     WORD PTR es:[di+1h], VAR_STRING  
                mov     BYTE PTR es:[di+3h], 50h ;push  ax
                add     di, 4h   
                add     cx, 4h
                jmp     _active_int     

                _string_char:
                    mov     si, WORD PTR [si+2h]
                    mov     dx, 1h
                    jmp     _go_end_string
                    
            _is_value:
                add     si, 3h 
                push    si 
                inc     si 

                sub     si, 4h 
                mov     ax, WORD PTR [si+1h]
                mov     BYTE PTR es:[di], 0B8h ;mov  ax, imm16
                mov     WORD PTR es:[di+1h], ax 
                add     si, 4h 
                add     di, 3h

                token_in_al   BYTE PTR [si]
                cmp     al, STRING_TOKEN
                je      _second_op_cmp_is_s
                cmp     al, VALUE_TOKEN
                je      _seco_op_is_value 

                ;second operand is identifier
                push    WORD PTR [si+2h]
                call    _search_var_in_vat
                add     sp, 2h 

                test    ax, 0FFh 
                je      _var_no_pres

                push    di 
                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]
                mov     bx, WORD PTR [di+3h]
                cmp     BYTE PTR [di+5h], STRING_TYPE
                pop     di
                je      _second_op_cmp_is_sid
                jmp     _jmp_here 

            _second_op_cmp_is_s:
                mov     BYTE PTR es:[di], 50h ;push  ax 
                mov     BYTE PTR es:[di+1h], 0B8h 
                mov     WORD PTR es:[di+2h], INT_TYPE ;mov   ax, TYPE_PAR 
                mov     BYTE PTR es:[di+4h], 50h; push  ax 
                add     cx, 4h 
                add     di, 5h
                mov     dx, 1h 
                jmp     _string_char

            _second_op_cmp_is_sid:
                mov     BYTE PTR es:[di], 50h ;push  ax 
                mov     BYTE PTR es:[di+1h], 0B8h 
                mov     WORD PTR es:[di+2h], INT_TYPE ;mov   ax, TYPE_PAR 
                mov     BYTE PTR es:[di+4h], 50h; push  ax 
                add     cx, 4h 
                add     di, 5h
                jmp    _load_string_data

            _jmp_here:
                mov     WORD PTR es:[di], 863Bh ;cmp  ax, WORD PTR [bp-offset]
                mov     WORD PTR es:[di+2h], bx                 
                add     di, 4h 
                jmp     _create_jmp_ins 

                _var_no_pres:
                    xor    ax, ax 
                    jmp    _seco_op_is_value_

                _seco_op_is_value:
                    mov     ax, WORD PTR [si+1h]

                _seco_op_is_value_:
                    mov     BYTE PTR es:[di], 3Dh  ;cmp   ax, imm16 
                    mov     WORD PTR es:[di+1h], ax                 
                    add     di, 3h 
                    jmp     _create_jmp_ins                 

            _is_identifier:
                push    WORD PTR [si+2h]
                call    _search_var_in_vat 
                add     sp, 2h 

                add     si, 4h 
                push    si 
                inc     si 

                test    ax, 0FFh 
                jz      _id_no_pres

                push    di 
                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]
                mov     bx, WORD PTR [di+3h]
                mov     al, BYTE PTR [di+5h]
                pop     di 
                cmp     al, STRING_TYPE
                je      _is_string_id 

                ;is integer identifier 
                token_in_al    BYTE PTR [si]
                cmp     al, STRING_TOKEN
                je      _type_error 
                cmp     al, VALUE_TOKEN
                je      _second_op_is_value 
                
                ;second operand is identifier
                push    WORD PTR [si+2h]
                call    _search_var_in_vat
                add     sp, 2h 

                test    ax, 0FFh 
                jz      _second_op_var_no_pres 

                push    di 
                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]
                mov     cx, WORD PTR [di+3h]
                cmp     BYTE PTR [di+5h], STRING_TYPE 
                pop     di
                je      _type_error 

                mov     WORD PTR es:[di], 868Bh ;mov  ax, WORD PTR [bp-offset]
                mov     WORD PTR es:[di+2h], cx 
                mov     WORD PTR es:[di+4h], 8639h ;cmp  WORD PTR [bp-offset], ax 
                mov     WORD PTR es:[di+6h], bx 
                add     di, 8h 
                jmp     _create_jmp_ins

                _second_op_var_no_pres:
                    xor    ax, ax 
                    jmp    _second_op_is_value_

                _second_op_is_value:
                    mov     ax, WORD PTR [si+1h]

                _second_op_is_value_:
                    mov     WORD PTR es:[di], 0BE81h ;cmp  WORD PTR[bp-offset], imm16
                    mov     WORD PTR es:[di+2h], bx 
                    mov     WORD PTR es:[di+4h], ax 
                    add     di, 6h 
                    jmp     _create_jmp_ins 

                _is_string_id:
                    push    bx 
                    push    ax
                    mov     bx, OFFSET stack_offset
                    mov     ax, WORD PTR [bx]
                    neg     ax 
                    sub     ax, 5h 

                    mov     BYTE PTR es:[di], 0BCh ;mov  sp, imm16
                    mov     WORD PTR es:[di+1h], ax 
                    add     di, 3h
                    pop     ax
                    pop     bx

                    sub     bx, 5h
 
                    mov     BYTE PTR es:[di], 0B8h  ;mov ax, OFFSET var1
                    mov     WORD PTR es:[di+1h], bx 
                    mov     BYTE PTR es:[di+3h], 50h  ;push  ax
                    mov     BYTE PTR es:[di+4h], 0B8h  ;mov  ax, TYPE_PAR
                    mov     WORD PTR es:[di+5h], VAR_STRING  
                    mov     BYTE PTR es:[di+7h], 50h      ;push  ax
                    mov     cx, 4h 
                    add     di, 8h

                    token_in_al   BYTE PTR [si]
                    cmp     al, VALUE_TOKEN
                    je      _second_cmp_is_value
                    cmp     al, STRING_TOKEN
                    je      _string_char

                    ;second op is id
                    push    WORD PTR [si+2h]
                    call    _search_var_in_vat 
                    add     sp, 2h 

                    test    ax, 0FFh 
                    jz      _type_error

                    push    di
                    mov     di, OFFSET roffset_ovt
                    mov     di, WORD PTR [di]
                    mov     bx, WORD PTR [di+3h]
                    cmp     BYTE PTR [di+5h], STRING_TYPE
                    pop     di 
                    jne     _type_error
                    
                    sub     bx, 5h

                _load_string_data:
                    mov     BYTE PTR es:[di], 0B8h   ;mov ax, OFFSET var1
                    mov     WORD PTR es:[di+1h], bx 
                    mov     BYTE PTR es:[di+3h], 50h ;push  ax
                    mov     BYTE PTR es:[di+4h], 0B8h  ;mov  ax, TYPE_PAR
                    mov     WORD PTR es:[di+5h], VAR_STRING  
                    mov     BYTE PTR es:[di+7h], 50h      ;push  ax 
                    add     cx, 4h 
                    add     di, 8h
                    jmp     _active_int

                _second_cmp_is_value:
                    mov     ax, WORD PTR [si+1h]
                    mov     BYTE PTR es:[di], 0B8h ;mov  ax, imm16
                    mov     WORD PTR es:[di+1h], ax 
                    mov     BYTE PTR es:[di+3h], 50h; push  ax 
                    mov     BYTE PTR es:[di+4h], 0B8h ;mov  ax, TYPE_PAR
                    mov     WORD PTR es:[di+5h], INT_TYPE 
                    mov     BYTE PTR es:[di+7h], 50h; push  ax    
                    add     di, 8h 
                    add     cx, 4h 
                    jmp     _active_int 

                _id_no_pres:
                    token_in_al   BYTE PTR [si]
                    cmp     al, STRING_TOKEN
                    je      _type_error

                    mov    WORD PTR es:[di], 0C031h ;xor  ax, ax 
                    add    di, 2h

                    cmp     al, VALUE_TOKEN
                    je      _is_value_sec_op

                    push    WORD PTR [si+2h]
                    call    _search_var_in_vat 
                    add     sp, 2h 

                    test    ax, 0FFh 
                    jz      _var_no_pres

                    push    di 
                    mov     di, OFFSET roffset_ovt
                    mov     di, WORD PTR [di]
                    mov     bx, WORD PTR [di+3h]
                    cmp     BYTE PTR [di+6h], STRING_TOKEN
                    pop     di 
                    je      _type_error
                    jmp     _jmp_here

                    _is_value_sec_op:
                        mov    ax, WORD PTR [si+1h]  
                        jmp    _seco_op_is_value_

                _create_jmp_ins:
                    pop    si 

                    cmp    BYTE PTR [si], EQU_ID 
                    je     _gen_equ_jmp
                    cmp    BYTE PTR [si], NEQU_ID 
                    je     _gen_nequ_jmp
                    cmp    BYTE PTR [si], BOR_ID 
                    je     _gen_bor_jmp
                    cmp    BYTE PTR [si], BORE_ID 
                    je     _gen_bore_jmp
                    cmp    BYTE PTR [si], GRT_ID 
                    je     _gen_grt_jmp
                    
                    ;generate grte's instruction
                    cmp    BYTE PTR [bp+4h], WHILE_ID 
                    je     _gen_bor_ins_

                    _gen_grte_ins_:
                        mov    WORD PTR es:[di], 8D0Fh 
                        add    di, 4h 
                        jmp    _end_gen_cond 

                    _gen_equ_jmp:
                        cmp    BYTE PTR [bp+4h], WHILE_ID
                        je     _gen_nequ_ins_ 

                    _gen_equ_ins_:
                        mov    WORD PTR es:[di], 840Fh 
                        add    di, 4h 
                        jmp    _end_gen_cond 

                    _gen_nequ_jmp:
                        cmp    BYTE PTR [bp+4h], WHILE_ID 
                        je     _gen_equ_ins_

                    _gen_nequ_ins_:
                        mov    WORD PTR es:[di], 850Fh 
                        add    di, 4h 
                        jmp    _end_gen_cond 

                    _gen_bor_jmp:
                        cmp    BYTE PTR [bp+4h], WHILE_ID 
                        je     _gen_grte_ins_

                    _gen_bor_ins_:
                        mov    WORD PTR es:[di], 8C0Fh 
                        add    di, 4h 
                        jmp    _end_gen_cond 

                    _gen_bore_jmp:
                        cmp    BYTE PTR [bp+4h], WHILE_ID 
                        je     _gen_grt_ins_

                    _gen_bore_ins_:
                        mov    WORD PTR es:[di], 8E0Fh 
                        add    di, 4h 
                        jmp    _end_gen_cond 

                    _gen_grt_jmp:
                        cmp    BYTE PTR [bp+4h], WHILE_ID 
                        je     _gen_bore_ins_

                    _gen_grt_ins_:
                        mov    WORD PTR es:[di], 8F0Fh 
                        add    di, 4h 
                        jmp    _end_gen_cond 

                _active_int:
                    pop     si 

                    cmp    BYTE PTR [si], EQU_ID 
                    je     _gen_equ_sub
                    cmp    BYTE PTR [si], NEQU_ID 
                    je     _gen_nequ_sub
                    cmp    BYTE PTR [si], BOR_ID 
                    je     _gen_bor_sub
                    cmp    BYTE PTR [si], BORE_ID 
                    je     _gen_bore_sub
                    cmp    BYTE PTR [si], GRT_ID 
                    je     _gen_grt_sub

                    ;gen grte sub
                    mov    ax, GRTE_CON
                    jmp    _gen_int 

                    _gen_equ_sub:
                        mov    ax, EQU_CON
                        jmp    _gen_int 

                    _gen_nequ_sub:
                        mov    ax, NEQU_CON
                        jmp    _gen_int 

                    _gen_bor_sub:
                        mov    ax, BOR_CON
                        jmp    _gen_int 

                    _gen_bore_sub:
                        mov    ax, BORE_CON
                        jmp    _gen_int 

                    _gen_grt_sub:
                        mov    ax, GRT_CON

                _gen_int:
                    mov    BYTE PTR es:[di], 0B8h ;mov  ax, TYPE_CON 
                    mov    WORD PTR es:[di+1h], ax 
                    mov    BYTE PTR es:[di+3h], 50h ;push  ax 
                    add    di, 4h 
                    add    cx, 2h
                    
                    mov    WORD PTR es:[di], 23CDh ;int  23h 
                    mov    WORD PTR es:[di+2h], 0C481h 
                    mov    WORD PTR es:[di+4h], cx  ;add  sp, imm16
                    mov    WORD PTR es:[di+6h], 0C085h ;test  ax, ax
                    add    di, 8h 

                    cmp    WORD PTR [bp+4h], WHILE_ID 
                    je     _invert_cond 

                    mov    WORD PTR es:[di], 850Fh ;jnz xx
                    add    di, 4h
                    jmp    _end_gen_cond 

                    _invert_cond:  
                        mov    WORD PTR es:[di], 840Fh ;jz xx
                        add    di, 4h
                        jmp    _end_gen_cond 

                _type_error:
                    add    sp, 2h 
                    mov    ax, 1h 
                    mov    bx, OFFSET type_of_error
                    mov    BYTE PTR [bx], 1h
                    jmp    _end_cond 

                _end_gen_cond:
                    xor    ax, ax

                _end_cond:
                    pop    si 
                    pop    cx 
                    pop    bx 
                    pop    bp 
                    ret 

        _gen_code_for_cond ENDP

    ;----------------------------------------------------------

_text ENDS 
