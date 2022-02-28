;-----------------------------------------------------------------------
; Il seguente modulo effettua l'analisi lessicale dei caratteri
; acquisiti da tastiera e genera i relativi token che saranno
; poi analizzati dal Parser, che effettuerà l'analisi semantica.
; L'interprete definisce un alfabeto memorizzato in un buffer
; dichiarato nel modulo "lexer.inc", contente tutte le keyword
; del linguaggio. Per effettuare l'analisi sintattica confronta
; ogni keyword con il contenuto dell buffer di input cercando 
; una corrispondenza, se presente genera il realtivo token,
; e analizza la prossima parola, fino all'incrontro del carattere 
; invio(ASCII 0Dh), altrimenti la considera come un identificatore
; (variabile), una stringa("CIAO") o un valore con segno su 16bit,
; ed esegue l'analisi sintattica di quest'ultimo, se coretta 
; sintatticamente analizza la prossima parola.
;
; I token sono o seguenti:
;                          __4bit__  _____4bit_____
;    -Innstruction token = token_id, instruction_id => 1byte
;    -Operator token = token_id, operator_id => 1byte
;    -Separator token = token_id, separator_id => 1byte
;                    ___1B___  ____1byte____  __2B__
;    -String token = token_id, lenght_string, offset => 4byte
;    -Identifier token = token_id, lenght_var, off_var => 4byte
;                   ___1B___  __2B_
;    -Value token = token_id, value => 3byte
;
; PRINT "HELLO, WORLD" => 88h | B0h | 0Ch | OFFSET | E0h => 6byte
;-----------------------------------------------------------

;-----------------------------------------------------------
; Macro lexer
    
    MAX_DIGIT_NUM       EQU     05h 
    MAX_DIM_STRING      EQU     28h
    MAX_DIM_VAR         EQU     02h
    MAX_16BIT_VALUE     EQU     7FFFh 
    MAXN_16BIT_VALUE    EQU     8000h
    EOT                 EQU     0E0h

    next_string MACRO 

        LOCAL _continue, _end 

        _continue:
            token_in_al  BYTE PTR [di]
            cmp     al, INSTRUCTION_TOKEN
            jae     _end  

            inc     di 
            jmp     _continue

        _end:
            inc     di 

    ENDM

    token_in_al MACRO value

        push    cx 
        mov     cl, 4h 
        mov     al, value 
        shr     al, cl 
        pop     cx

    ENDM

;-----------------------------------------------------------

INCLUDE  lexer.inc   

_text SEGMENT PARA PUBLIC  

    token_buffer     db     51h DUP(0h)  ;where the token are stored
    last_token       db     0h           ;it store the last token
    offset_tb        dw     0h           ;it store current offset in token buffer
    out_of_range     db     0h           ;set to one if the maximum value that can be represented on 16 bits is exceeded
    syn_error        db     0h           ;set to one if a syntax error is encountered
    token_per        db     0h 
    no_min_gen       db     0h  

    ;------------------------------------------------ 
    ; Genera i corrispettivi token partendo dalle
    ; stringhe memorizzate nel buffer di input.
    ;
    ; Valori di ritorno:
    ;   0 = nessun errore.
    ;   1 = errore.

        _tokenize_input PROC NEAR 

            push    ax
            push    bx 
            push    cx
            push    si 
            push    di 
            push    es

            mov     ax, BASIC_SEG
            mov     es, ax

            mov     bx, OFFSET last_token
            mov     si, OFFSET input_buffer
            mov     di, OFFSET alphabet_buffer

            mov     cx, 2Ch 

            cmp     BYTE PTR [si], 0Dh 
            je      _store_EOT

            _check_in_alphabet:
                cmp     BYTE PTR [si], 0Dh 
                je      _store_EOT 
                cmp     BYTE PTR [si], 20h 
                jne     _continue_check
                inc     si 
                jmp     _check_in_alphabet

            _continue_check:
                push    si 

                _check_string:
                    cmpsb 
                    jz     _check_string

                ;isolate the most significative nible(the token)
                token_in_al   BYTE PTR [di-1h]
                ;is it a token?
                cmp     al, INSTRUCTION_TOKEN
                jae     _convert_to_token  

                pop     si 
                next_string
                loop    _continue_check

            ;not in alphabet, possible identifier or value
            _is_string:
                call    _check_last_token

                cmp     ax, 1h 
                je      _syn_error_

                push    dx
                push    cx
                push    ax 
                mov     ax, 6h 
                jmp     _store 

            _convert_to_token:
                add     sp, 2h 

                dec     si 
                mov     al, BYTE PTR [di-1h]

                ;create token for string
                cmp     al, DQTS_ID
                je      _ignore_quotes
                cmp     al, SQTS_ID
                je      _ignore_quotes

                ;check if it is a comparison operator
                cmp     al, ASS_ID 
                je      _possible_equal 
                cmp     al, BOR_ID 
                je      _possible_be 
                cmp     al, GRT_ID 
                je      _possible_ge
                cmp     al, NOT_ID
                je      _possible_nequ

                cmp     al, MACHC_ID
                je      _store_token_per
                cmp     al, INP_ID 
                je      _store_token_per

                cmp     al, ANGBRCKL_ID
                je      _set_no_min_flag
                cmp     al, SUB_ID 
                je      _dont_gen_it 

            _continue:
                ;store the last token
                mov     BYTE PTR [bx], al

                push    ax 
                mov     ax, 2h 

            _store:
                ;store token in buffer_token
                call    _store_token
                add     sp, ax

                ;end of input?
                cmp    BYTE PTR [si], 0Dh  
                je     _store_EOT 

            _ignore_sub:
                ;next iteration
                mov    di, OFFSET alphabet_buffer
                mov    cx, 2Ch 
                jmp    _check_in_alphabet

            _ignore_quotes:
                mov      BYTE PTR [bx], al 
                jmp      _is_string 

            ;check if it is a comparison operator
            _possible_equal:
                cmp      BYTE PTR [si], 3Dh
                jne      _continue
                inc      si
                mov      al, EQU_ID  
                jmp      _continue

            ;check if it is a comparison operator
            _possible_be:
                cmp      BYTE PTR [si], 3Dh
                jne      _continue
                inc      si
                mov      al, BORE_ID
                jmp      _continue

            ;check if it is a comparison operator
            _possible_ge:
                cmp      BYTE PTR [si], 3Dh
                jne      _continue
                inc      si
                mov      al, GRTE_ID
                jmp      _continue

            _possible_nequ:
                cmp      BYTE PTR [si], 3Dh 
                jne      _continue 
                inc      si 
                mov      al, NEQU_ID
                jmp      _continue

            _store_token_per:
                push    bx 
                mov     bx, OFFSET token_per 
                mov     BYTE PTR [bx], al 
                pop     bx 
                jmp     _continue 

            _dont_gen_it:
                push    bx 
                mov     bx, OFFSET no_min_gen
                cmp     BYTE PTR [bx], 1h 
                pop     bx 
                jne     _continue
                cmp     BYTE PTR [bx], ANGBRCKL_ID
                je      _no_gen 
                cmp     BYTE PTR [bx], BRCKL_ID
                je      _no_gen 
                cmp     BYTE PTR [bx], MUL_ID
                je      _no_gen 
                cmp     BYTE PTR [bx], DIV_ID 
                je      _no_gen 
                cmp     BYTE PTR [bx], MOD_ID
                je      _no_gen 
                cmp     BYTE PTR [bx], OR_ID
                je      _no_gen 
                cmp     BYTE PTR [bx], AND_ID 
                je      _no_gen 
                cmp     BYTE PTR [bx], NOT_ID 
                je      _no_gen 
                
                mov     al, ADD_ID
                push    ax 
                call    _store_token
                add     sp, 2h 
                
                _no_gen:
                    mov     BYTE PTR [bx], SUB_ID 
                    jmp     _ignore_sub

            _set_no_min_flag:
                push    bx 
                mov     bx, OFFSET no_min_gen
                mov     BYTE PTR [bx], 1h 
                pop     bx 
                jmp     _continue

            _store_EOT:
                xor    ax, ax
                push   ax 
                call   _store_token 
                pop    ax
                mov    si, OFFSET offset_tb
                mov    WORD PTR [si], 0h
                jmp    _end

            ;error
            _syn_error_:
                mov    bx, OFFSET  syn_error 
                mov    BYTE PTR [bx], 1h 

            _end: 
                mov    bx, OFFSET no_min_gen
                mov    BYTE PTR [bx], 0h 
                pop    es
                pop    di 
                pop    si 
                pop    cx 
                pop    bx 
                pop    ax 
                ret

        _tokenize_input ENDP
    
    ;------------------------------------------------

    ;------------------------------------------------
    ; Memorizza il token generato dalla procedura
    ; "_tokenize_input" nell buffer dei token.

        _store_token PROC NEAR 

            push    bp 

            mov     bp, sp 

            push    ax 
            push    bx 

            mov     bx, OFFSET offset_tb
            push    bx
            mov     bx, WORD PTR [bx]
            add     bx, OFFSET token_buffer

            push    WORD PTR [bp+4h]
            ;store most significative of BYTE PTR [bp+4h] nible in al
            token_in_al   BYTE PTR [bp+4h]

            test    al, 0FFh 
            jz      _end_store 

            ;check the token type
            cmp     al, STRING_TOKEN
            je      _store_literal
            cmp     al, VALUE_TOKEN
            je      _store_value
            cmp     al, IDENTIFIER_TOKEN
            je      _store_literal

            ;normal store 
            pop     ax
            mov     BYTE PTR [bx], al
            mov     ax, 1h 
            jmp     _end 

            _store_literal:
                pop     ax 
                mov     BYTE PTR [bx], al 
                mov     al, BYTE PTR [bp+6h]
                mov     BYTE PTR [bx+1h], al 
                mov     ax, WORD PTR [bp+8h]
                mov     WORD PTR [bx+2h], ax
                mov     ax, 4h 
                jmp     _end

            _store_value:
                pop     ax 
                mov     BYTE PTR [bx], al 
                mov     ax, WORD PTR [bp+6h]
                mov     WORD PTR [bx+1h], ax 
                mov     ax, 3h 
                jmp     _end

            _end_store:
                add    sp, 2h 
                mov    BYTE PTR [bx], EOT 
                xor    ax, ax 

            _end:
                ;increment token_buffer's offset
                pop     bx
                add     WORD PTR [bx], ax

            pop    bx 
            pop    ax 
            pop    bp 
            ret   

        _store_token ENDP 

    ;------------------------------------------------

    _check_last_token PROC NEAR

        mov     al, BYTE PTR [bx]

        cmp     al, DQTS_ID
        je      _is_string 
        cmp     al, SQTS_ID
        je      _is_string 

        cmp     BYTE PTR [si], 30h 
        jb      _is_identifier
        cmp     BYTE PTR [si], 39h 
        ja      _is_identifier

        ;it's value
        mov     cl, 4h
        mov     al, VALUE_TOKEN
        shl     al, cl 
        xor     ah, ah 
        xor     cx, cx

        _check_lenght_value:
            cmp     BYTE PTR [si], 30h 
            jb      _check_sign
            cmp     BYTE PTR [si], 39h 
            ja      _check_sign 

            push    ax 
            mov     ax, 0Ah 
            mul     cx
            mov     cx, ax
            mov     al, BYTE PTR [si]
            sub     al, 30h 
            xor     ah, ah
            add     cx, ax
            pop     ax 

            inc     ah
            inc     si 

            cmp     ah, MAX_DIGIT_NUM
            jbe     _check_lenght_value
            jmp     _verror

        _check_sign:
            push    ax
            mov     al, BYTE PTR [bx]
            cmp     al, 92h
            pop     ax 
            jne     _inrange

            ;negative number 
            neg     cx

            cmp     cx, MAXN_16BIT_VALUE
            jb      _range_error 
            jmp     _end 

            _inrange:
                cmp     BYTE PTR [bx], WRITE_ID 
                je      _end 
                cmp     BYTE PTR [bx], READ_ID 
                je      _end 
                cmp     BYTE PTR [bx], OUTP_ID 
                je      _end
                push    bx 
                mov     bx, OFFSET token_per 
                test    BYTE PTR [bx], 0FFh
                mov     BYTE PTR [bx], 0h 
                pop     bx  
                jnz     _end                
                cmp     cx, MAX_16BIT_VALUE
                jbe     _end

            _range_error:
                push    bx 
                mov     bx, OFFSET out_of_range
                mov     BYTE PTR [bx], 1h 
                pop     bx
                jmp     _end

        _is_string:
            mov     cl, 4h
            mov     al, STRING_TOKEN
            shl     al, cl 
            mov     dx, si 
            xor     cl, cl

            _check_lenght_string:
                cmp     BYTE PTR [si], 22h 
                je      _send 
                cmp     BYTE PTR [si], 27h 
                je      _send 

                inc     cl
                inc     si 
                cmp     cl, MAX_DIM_STRING
                jbe     _check_lenght_string 
                jmp     _verror

        _send:
            inc     si 
            jmp     _end

        _is_identifier:
            mov     cl, 4h 
            mov     al, IDENTIFIER_TOKEN
            shl     al, cl 
            xor     cl, cl 
            mov     dx, si

            _check_var_lenght:
                test    cl, cl 
                jz      _check_it  

                cmp     BYTE PTR [si], 20h 
                je      _end 
                cmp     BYTE PTR [si], 41h 
                jb      _end 
                cmp     BYTE PTR [si], 5Ah 
                ja      _end 
                inc     cl 
                inc     si 
                jmp     _check_lenght

                _check_it:
                    cmp    BYTE PTR [si], 41h 
                    jb     _verror
                    cmp    BYTE PTR [si], 5Ah 
                    ja     _verror 
                    inc    cl 
                    inc    si 
                
                _check_lenght:
                    cmp     cl, 2h 
                    jbe     _check_var_lenght

        _verror:
            mov     ax, 1h 

        _end:
            mov     BYTE PTR [bx], al
            ret

    _check_last_token ENDP
    
_text ENDS  