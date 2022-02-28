;-----------------------------------------------------------------------
; Il seguente modulo effettua l'analisi semantica della stringa di 
; input dopo la coretta trasformazione in token da parte del lexer.
; L'analissi semantica viene effettua con una serie di confronti 
; per ogni classe di istruzioni (ho cercato di migliorare il tutto,
; minimizzando il numero di confronti, ma la diversità tra le varie
; istruzioni non me l'ha consentito).
;
; Data l'istruzinoe PRINT "HELLO, WORLD"
; Il lexer generà i relativi token:
;   88h, 0B0h, 0C0h, 00h, 00h, 0E0h
; 
; Il parser si aspetterà di trovare i token nel seguente ordine
; una qualsiasi variazione, ovviamente, genererebbe un errore semantico
;-----------------------------------------------------------------------

MULTI_LINE_MODE     EQU   0h 
SINGLE_LINE_MODE    EQU   1h 
MAX_COND_INNEST     EQU   0Ah

_text SEGMENT PARA PUBLIC

    mode_interpreter       db     SINGLE_LINE_MODE ;interpreter operation mode
    actual_cond_innestf    db     0h               ;count number of for ins
    actual_cond_innestw    db     0h               ;count number of while ins
    max_innest             db     0h               ;set to one if the maximum number of grafts is reached 
    exp_error              db     0h               ;set to one if a error in expression is encountered
    sem_error              db     0h               ;set to one if a semantic error is encountered
    last_line_value        db     0h               ;store last line number
    multi_line_err         db     0h 
    line_number_err        db     0h 

    ;-------------------------------------------------
    ; Procedura principale del Parser, si limita
    ; a controllare se il primo token è quello di
    ; un instruzione o di un valore, in funzione
    ; della modalità dell'editor(single o multi line
    ; mode), ed effettua la chiamata a una procdeura
    ; secondaria che svolge la vera e propria analisi
    ; semantica, in caso di errore pone sem_err a 1
    ;
    ; Valori di ritorno:
    ;   -0: nessun errore
    ;   -1: sem_error = 1 in caso di errore

        _parsing_token PROC NEAR 

            push    bx  
            push    si

            mov    si, OFFSET token_buffer
            mov    bx, OFFSET mode_interpreter

            cmp    BYTE PTR [si], EOT 
            je     _pend
            cmp    BYTE PTR [si], RUN_ID 
            je     _continue
            cmp    BYTE PTR [si], SHVT_ID 
            je     _continue
            cmp    BYTE PTR [si], TIME_ID 
            je     _continue
            cmp    BYTE PTR [si], RESET_ID
            je     _continue
            cmp    BYTE PTR [si], CLEAR_ID
            je     _continue
            cmp    BYTE PTR [si], HELP_ID 
            je     _continue

            cmp    BYTE PTR [bx], SINGLE_LINE_MODE
            je     _single_line_mode

            ;multi_line_mode 
            token_in_al  BYTE PTR [si]
            cmp    al, VALUE_TOKEN
            jne    _sem_err 
            push   bx 
            mov    bx, OFFSET last_line_value
            mov    ax, WORD PTR [si+1h]
            test   ah, ah 
            jnz    _dealloc 
            cmp    al, BYTE PTR [bx]
            jbe    _dealloc 
            mov    BYTE PTR [bx], al 
            pop    bx
            add    si, 3h 

            _single_line_mode:  
                token_in_al  BYTE PTR [si]
                cmp    al, INSTRUCTION_TOKEN
                je     _continue 
                cmp    al, IDENTIFIER_TOKEN
                je     _continue
                cmp    BYTE PTR [si], WEND_ID 
                je     _continue 
                cmp    BYTE PTR [si], NEXT_ID 
                je     _continue 
                jmp    _sem_err 
                
            _continue:
                call    semantic_analyzer
                test    ax, ax 
                jz      _pend 
                jmp     _sem_err

            _dealloc:
                pop    bx
                mov    bx, OFFSET line_number_err
                mov    BYTE PTR [bx], 1h 

            _sem_err:
                mov     bx, OFFSET sem_error 
                mov     BYTE PTR [bx], 1h

            _pend:
                pop    si
                pop    bx 
                ret 

        _parsing_token ENDP 

    ;-------------------------------------------------

    ;-------------------------------------------------
    ; Procedura che effettua l'anaslisi semantica
    ; dei token.
    ; 
    ; Valori di ritorno:
    ;   -0: nessun errore.
    ;   -1: errore

        semantic_analyzer PROC NEAR 

            push    bx 

            cmp     BYTE PTR [si], RUN_ID 
            je      _command_line
            cmp     BYTE PTR [si], SHVT_ID 
            je      _command_line 
            cmp     BYTE PTR [si], TIME_ID
            je      _command_line
            cmp     BYTE PTR [si], RESET_ID 
            je      _command_line
            cmp     BYTE PTR [si], CLEAR_ID 
            je      _command_line
            cmp     BYTE PTR [si], HELP_ID 
            je      _command_line

            token_in_al  BYTE PTR [si]
            cmp     al, IDENTIFIER_TOKEN
            je      _check_identifier

            mov     al, BYTE PTR [si]
            inc     si 
            cmp     al, DECL_ID 
            je      _check_declaration
            cmp     al, IF_ID 
            je      _check_selcond
            cmp     al, WHILE_ID 
            je      _check_itcond
            cmp     al, FOR_ID 
            je      _check_fcond
            cmp     al, GOTO_ID
            je      _check_gins
            cmp     al, READ_ID 
            je      _check_wins 
            cmp     al, WRITE_ID 
            je      _check_wins
            cmp     al, INP_ID 
            je      _check_io_ins 
            cmp     al, OUTP_ID 
            je      _check_io_ins 
            cmp     al, ABS_ID 
            je      _check_sins
            cmp     al, INPUT_ID 
            je      _check_input
            cmp     al, PRINT_ID 
            je      _check_print
            cmp     al, NEXT_ID 
            je      _dec_innestf
            cmp     al, WEND_ID 
            je      _dec_innestw
            
            ;check machc's semantic correctness
            ;----------------------------------------------
            _machcode_check:
                token_in_al  BYTE PTR [si]
                cmp    al, VALUE_TOKEN
                jne    _error0 
                add    si, 3h 
                cmp    BYTE PTR [si], EOT 
                je     _no_err 
                cmp    BYTE PTR [si], COMMA_ID
                jne    _error0 
                inc    si 
                jmp    _machcode_check
            ;----------------------------------------------

            ;check declarations' semantic correctness
            ;----------------------------------------------
            _check_identifier: 
                cmp     BYTE PTR [si+4h], ASS_ID 
                jne     _check_identifier_

                token_in_al   BYTE PTR [si+5h]
                cmp     al, STRING_TOKEN
                jne     _check_identifier_

                add     si, 9h 
                jmp     _no_err

            _check_identifier_:
                add     si, 4h
                cmp     BYTE PTR [si], BRCKL_ID
                je      _is_string
                cmp     BYTE PTR [si], ASS_ID
                jne     _error0
                inc     si 
                cmp     BYTE PTR [si], ANGBRCKL_ID
                je      _exp
                token_in_al  BYTE PTR [si]
                cmp     al, SEPARATOR_TOKEN
                je      _error0
                cmp     al, STRING_TOKEN
                je      _error0
                cmp     al, OPERATOR_TOKEN
                je      _who_op
                cmp     al, INSTRUCTION_TOKEN
                je      _who_instruction
                
                cmp     al, IDENTIFIER_TOKEN
                je      _add_4__

                add    si, 3h 
                jmp    _no_err 

                _add_4__:
                    add    si, 4h
                    jmp     _no_err  

            _is_string:
                inc     si 
                token_in_al   BYTE PTR [si]
                cmp     al, VALUE_TOKEN
                jne     _error0
                add     si, 3h 
                cmp     BYTE PTR [si], BRCKR_ID 
                jne     _error0 
                inc     si 
                cmp     BYTE PTR [si], ASS_ID 
                jne     _error0
                inc     si 
                token_in_al  BYTE PTR [si]
                cmp     al, STRING_TOKEN
                je      _no_err_  
                cmp     al, IDENTIFIER_TOKEN
                je      _no_err_
                jmp     _error0 

                _no_err_:
                    add    si, 4h 
                    jmp    _no_err 

            _check_declaration:
                token_in_al  BYTE PTR [si]
                cmp     al, IDENTIFIER_TOKEN
                jne     _error0
                jmp     _check_identifier_

            _who_instruction:
                cmp     BYTE PTR [si], READ_ID 
                je      _ins_ok 
                cmp     BYTE PTR [si], ABS_ID 
                je      _ins_ok 
                jmp     _error0

            _who_op:
                cmp    BYTE PTR [si], SUB_ID
                jne    _error0
                inc    si 
                token_in_al  BYTE PTR [si]
                cmp    al, VALUE_TOKEN
                jne    _error0
                add    si, 3h 
                jmp    _no_err

            _ins_ok:
                call    semantic_analyzer
                cmp     ax, 1h 
                je      _error0 
                jmp     _no_err
            
            _exp:
                inc     si
                call    _check_sem_exp   
                cmp     ax, 1h 
                jne     _no_err
                jmp     _error0 
            ;----------------------------------------------

            ;check conditions' semantic correctness
            ;----------------------------------------------
            ; IF cond
            _check_selcond:
                mov    bx, OFFSET mode_interpreter
                cmp    BYTE PTR [bx], SINGLE_LINE_MODE
                je     _cannot_use 
                call   _check_cond 
                cmp    ax, 1h 
                je     _error0 

                cmp    BYTE PTR [si], THEN_ID 
                jne    _error0
                inc    si 
                token_in_al  BYTE PTR [si]
                cmp    al, VALUE_TOKEN
                jne    _error0
                add    si, 3h
                jmp    _no_err

            ; While cond
            _check_itcond:
                mov     bx, OFFSET mode_interpreter
                cmp     BYTE PTR [bx], SINGLE_LINE_MODE 
                je      _cannot_use 
                call    _check_cond 
                cmp     ax, 1h 
                je      _error0  
                cmp     BYTE PTR [si], DBP_ID
                jne     _error0
                inc     si 
                jmp     _inc_innestw

            ;For cond
            _check_fcond:
                mov    bx, OFFSET mode_interpreter
                cmp    BYTE PTR [bx], SINGLE_LINE_MODE 
                je     _cannot_use 
                token_in_al  BYTE PTR [si]
                cmp     al, IDENTIFIER_TOKEN
                jne     _error0
                add     si, 4h 
                cmp     BYTE PTR [si], ASS_ID
                jne     _error0
                inc     si 
                cmp     BYTE PTR [si], SUB_ID 
                je      _minus_value
                token_in_al  BYTE PTR [si]
                cmp     al, IDENTIFIER_TOKEN
                je      _ok_for0
                cmp     al, VALUE_TOKEN
                je      _ok_for1 
                jmp     _error0 

                _minus_value:
                    inc    si 
                    token_in_al  BYTE PTR [si]
                    cmp    al, VALUE_TOKEN
                    jne    _error0
                
                _ok_for1:
                    add   si, 3h 
                    jmp   _continuef

                _ok_for0:
                    add   si, 4h 

                _continuef:
                    cmp    BYTE PTR [si], TO_ID
                    jne    _error0 
                    inc    si 
                    cmp    BYTE PTR  [si], SUB_ID 
                    je     _minusf_value
                    token_in_al   BYTE PTR [si]
                    cmp    al, VALUE_TOKEN
                    je      _ok_for0_
                    cmp    al, IDENTIFIER_TOKEN
                    je     _ok_for1_
                    jmp    _error0

                _minusf_value:
                    inc    si
                    token_in_al  BYTE PTR [si]
                    cmp    al, VALUE_TOKEN
                    jne    _error0

                _ok_for0_:
                    add    si, 3h 
                    jmp    _continuef0
    
                _ok_for1_:
                    add    si, 4h 
                
                _continuef0:
                    cmp    BYTE PTR [si], STEP_ID 
                    jne    _error0 
                    inc    si
                    cmp    BYTE PTR [si], SUB_ID    
                    jne    _no_sub 

                    inc    si 

                    _no_sub:
                        token_in_al   BYTE PTR [si]
                        cmp     al, VALUE_TOKEN
                        jne     _error0

                _continuef1:
                    add    si, 3h
                    cmp    BYTE PTR [si], DBP_ID
                    jne    _error0
                    inc    si 
                    jmp    _inc_innestf
            ;----------------------------------------------

            ;check goto's semantic correcntess
            ;----------------------------------------------
            _check_gins:
                mov    bx, OFFSET mode_interpreter
                cmp    BYTE PTR [bx], SINGLE_LINE_MODE
                je     _cannot_use 
                token_in_al  BYTE PTR [si]
                cmp    al, VALUE_TOKEN
                jne    _error0
                add    si, 3h 
                jmp    _no_err 
            ;----------------------------------------------

            ;check simple instructions' semantic correctness
            ;----------------------------------------------
            _check_sins:
                cmp    BYTE PTR [si], SUB_ID
                je     _inc_ 
                jmp    _cont

                _inc_:
                    cmp     BYTE PTR [si-1h], ABS_ID 
                    jne     _error0
                    inc     si 

                _cont:
                    token_in_al    BYTE PTR [si]
                    cmp    al, VALUE_TOKEN
                    je     _no_err_s3
                    cmp    al, IDENTIFIER_TOKEN
                    je     _no_err_s4

                    _no_err_s3:
                        add    si, 3h 
                        jmp    _no_err 

                    _no_err_s4:
                        add    si, 4h 
                        jmp    _no_err 
                        
                    jmp    _error0
            ;----------------------------------------------

            ;check write instruction's semantic
            ;---------------------------------------------- 
            _check_wins:
                xor    dx, dx
                cmp    BYTE PTR [si-1h], READ_ID 
                jne    _continue_wins 

                mov    dx, 1h

            _continue_wins:
                token_in_al  BYTE PTR [si]
                cmp    al, VALUE_TOKEN
                je     _value
                cmp    al, IDENTIFIER_TOKEN
                je     _identifier 
                jmp    _error0

                _value:
                    add    si, 3h 
                    jmp    _con 

                _identifier:
                    add    si, 4h 

                _con:
                    cmp    BYTE PTR [si], COMMA_ID
                    jne    _error0 
                    inc    si 

                token_in_al  BYTE PTR [si]
                cmp    al, VALUE_TOKEN
                je     _value1
                cmp    al, IDENTIFIER_TOKEN
                je     _identifier1 
                jmp    _error0

                _value1:
                    add    si, 3h 
                    jmp    _con1 

                _identifier1:
                    add    si, 4h 

                _con1:
                    test    dx, dx 
                    jnz     _no_err
                    cmp    BYTE PTR [si], COMMA_ID
                    jne    _error0
                    inc    si    
                    token_in_al    BYTE PTR [si]
                    cmp    al, VALUE_TOKEN
                    je     _no_err_w3
                    cmp    al, IDENTIFIER_TOKEN
                    je     _no_err_w4
                    jmp    _error0

                    _no_err_w3:
                        add    si, 3h 
                        jmp    _no_err 

                    _no_err_w4:
                        add    si, 4h 
                        jmp    _no_err 

            ;----------------------------------------------

            ;check outp semantic's instruction
            ;----------------------------------------------
            _check_io_ins:
                mov     bl, BYTE PTR [si-1h]
                mov     cx, 2h 

                _loopio:
                    token_in_al   BYTE PTR [si] 
                    cmp     al, IDENTIFIER_TOKEN
                    je      _a4 
                    cmp     al, VALUE_TOKEN 
                    je      _a3
                    cmp     cx, 1h 
                    jne     _error0
                    cmp     bl, OUTP_ID 
                    jne     _error0 
                    cmp     BYTE PTR [si], SUB_ID
                    jne     _error0
                    inc     si 
                    jmp     _loopio 

                    _a4:
                        add    si, 4h 
                        jmp    _l0 

                    _a3:
                        cmp    cx, 1h 
                        je     _conio
                    
                        cmp    bl, INP_ID 
                        je     _error0

                    _conio:
                        add    si, 3h 

                    _l0:
                        cmp    BYTE PTR [si], COMMA_ID
                        jne    _error0
                        inc    si 

                    loop    _loopio
                
                token_in_al   BYTE PTR [si]
                cmp     al, VALUE_TOKEN
                jne     _error0 
                add     si, 3h 
                jmp     _no_err
            ;----------------------------------------------

            ;check input's semantic correctness
            ;----------------------------------------------
            _check_input:
                xor    bx, bx 

                _loopi:
                    push    bx 
                    shr     bx, 1h 
                    jc      _is_commai 

                    ;value
                    pop     bx 
                    token_in_al   BYTE PTR [si]
                    cmp     al, IDENTIFIER_TOKEN
                    jne     _error0
                    inc     bx
                    add     si, 4h
                    jmp     _loopi

                _is_commai:
                    pop    bx
                    cmp    BYTE PTR [si], EOT 
                    je     _no_err 
                    cmp    BYTE PTR [si], COMMA_ID
                    jne    _error0
                    inc    si 
                    inc    bx 
                    jmp    _loopi
                    
            ;----------------------------------------------

            ;check print's semantic correctness
            ;----------------------------------------------
            _check_print:
                xor    bx, bx 

                _loopp:
                    push    bx 
                    shr     bx, 1h 
                    jc      _is_commap 

                    ;value
                    pop     bx 
                    inc     bx
                    cmp     BYTE PTR [si], SUB_ID 
                    je      _negative
                    token_in_al   BYTE PTR [si]
                    cmp     al, IDENTIFIER_TOKEN
                    je      _nextp4
                    cmp     al, VALUE_TOKEN
                    je      _nextp3
                    cmp     al, STRING_TOKEN
                    je      _nextp4 
                    jmp     _error0

                    _negative:
                        inc    si 
                        token_in_al  BYTE PTR [si]
                        cmp    al, VALUE_TOKEN
                        jne    _error0 

                    _nextp3:    
                        add    si, 3h 
                        jmp    _loopp 

                    _nextp4:
                        add    si, 4h 
                        jmp    _loopp 

                _is_commap:
                    pop    bx
                    cmp    BYTE PTR [si], EOT 
                    je     _no_err 
                    cmp    BYTE PTR [si], COMMA_ID
                    jne    _error0
                    inc    si 
                    inc    bx 
                    jmp    _loopp
            ;----------------------------------------------

            ;increment number of while's innest
            _inc_innestw:
                mov     bx, OFFSET actual_cond_innestw
                inc     BYTE PTR [bx]
                cmp     BYTE PTR [bx], MAX_COND_INNEST
                ja      _max_innest
                jmp     _no_err 

            ;decrement number of while's innest
            _dec_innestw:
                mov    bx, OFFSET actual_cond_innestw
                test   BYTE PTR [bx], 0FFh 
                jz     _error0 
                dec    BYTE PTR [bx]
                jmp    _no_err 

            ;increment number of for's innest
            _inc_innestf:
                mov     bx, OFFSET actual_cond_innestf
                inc     BYTE PTR [bx]
                cmp     BYTE PTR [bx], MAX_COND_INNEST
                ja      _max_innest
                jmp     _no_err    

            ;decrement number of for's innest
            _dec_innestf:
                mov    bx, OFFSET actual_cond_innestf
                test   BYTE PTR [bx], 0FFh 
                jz     _error0 
                dec    BYTE PTR [bx]
                jmp    _no_err 

            _max_innest:
                mov    bx, OFFSET max_innest 
                mov    BYTE PTR [bx], 1h 
                jmp    _error0 

            _command_line:
                inc    si 

            _no_err:
                xor   ax, ax
                cmp   BYTE PTR [si], EOT 
                je    _end 
                jmp   _error0

            _cannot_use:
                mov    bx, OFFSET multi_line_err
                mov    BYTE PTR [bx], 1h 

            _error0:
                mov    ax, 1h 

            _end:
                pop    bx
                ret

        semantic_analyzer ENDP 

    ;-------------------------------------------------

    ;---------------------------------------------------
    ; Controlla la coretezza semantica di un operazione
    ; di confronto:
    ;
    ;    m op m  |   v op v   |  m  op v  |  v op m
    ;
    ; Valori di ritorno:
    ;     -0: nessun errore.
    ;     -1: errore semantico

        _check_cond PROC NEAR 

            token_in_al  BYTE PTR [si]
            cmp    al, STRING_TOKEN
            je     _add4
            cmp    al, IDENTIFIER_TOKEN
            je     _add4
            cmp    al, VALUE_TOKEN
            je     _add3 
            cmp    BYTE PTR [si], SUB_ID
            je     _nextc
            jmp    _cerror

            _nextc:
                inc    si 
                token_in_al   BYTE PTR [si]
                cmp    al, VALUE_TOKEN
                jne    _cerror 
                jmp    _add3

            _add4:
                add    si, 4h 
                jmp    _continue 

            _add3:
                add    si, 3h 

            _continue:
                cmp    BYTE PTR [si], EQU_ID 
                je     _ok_cond 
                cmp    BYTE PTR [si], BOR_ID
                je     _ok_cond 
                cmp    BYTE PTR [si], GRT_ID
                je     _ok_cond 
                cmp    BYTE PTR [si], BORE_ID
                je     _ok_cond 
                cmp    BYTE PTR [si], GRTE_ID
                je     _ok_cond 
                cmp    BYTE PTR [si], NEQU_ID
                je     _ok_cond
                jmp    _cerror

            _ok_cond:
                inc    si
                token_in_al  BYTE PTR [si]
                cmp    al, STRING_TOKEN
                je     _add4_ 
                cmp    al, IDENTIFIER_TOKEN
                je     _add4_ 
                cmp    al, VALUE_TOKEN
                je     _add3_
                jmp    _cerror

            _add4_:
                add   si, 4h 
                jmp   _end 

            _add3_:
                add   si, 3h 
                jmp   _end 

            _cerror:
                mov   ax, 1h

            _end:
                ret

        _check_cond ENDP  

    ;---------------------------------------------------

    ;---------------------------------------------------
    ; Effettua l'analisi semantica di un espressione.
    ;
    ; Valori di ritorno:
    ;     -0: nessun errore.
    ;     -1: errore semantico nell'espressione
    ;         exp_err = 1.

        _check_sem_exp PROC NEAR 

            push    cx
            push    dx 

            xor     cx, cx 
            xor     dx, dx
            
            _check_exp:
                cmp     BYTE PTR [si], NOT_ID 
                je      _ignore_him
                cmp     BYTE PTR [si], OFF_ID 
                je      _check_if_id 
                cmp     BYTE PTR [si], BRCKL_ID 
                je      _increment_nrbrck 
                cmp     BYTE PTR [si], BRCKR_ID 
                je      _decrement_nbrck
                push    cx 
                shr     cx, 1h  
                jc      _operator 

                ;value
                pop     cx 
                token_in_al   BYTE PTR [si]
                cmp     al, VALUE_TOKEN
                je      _add3
                cmp     al, IDENTIFIER_TOKEN
                je      _add4 
                jmp     _exp_err 

                _add3:
                    add   si, 3h 
                    jmp   _continue 

                _add4:
                    add   si, 4h 
                    cmp   BYTE PTR [si], OFF_ID 
                    je    _exp_err 
                    cmp   BYTE PTR [si], NOT_ID 
                    je    _exp_err 

                _continue:
                    inc    cx 
                    jmp    _check_exp

                _operator:
                    pop     cx 
                    cmp     BYTE PTR [si], ANGBRCKR_ID
                    je      _exp_nerr
                    token_in_al   BYTE PTR [si]
                    cmp     al, OPERATOR_TOKEN
                    jne     _exp_err
                    inc     cx
                    inc     si
                    jmp     _check_exp

                _increment_nrbrck:
                    inc    dx 
                    inc    si
                    mov    ax, cx 
                    jmp    _check_exp 

                _decrement_nbrck:  
                    dec    dx 
                    inc    si
                    cmp    ax, cx 
                    je     _exp_err
                    jmp    _check_exp

                _ignore_him:
                    inc     si 
                    jmp     _check_exp  

                _check_if_id:
                    inc     si 
                    token_in_al   BYTE PTR [si]
                    cmp     al, IDENTIFIER_TOKEN
                    je      _check_exp

                _exp_err:
                    mov     ax, 1h
                    push    bx
                    mov     bx, OFFSET exp_error 
                    mov     BYTE PTR [bx], al 
                    pop     bx
                    jmp     _exp_end

                _exp_nerr:
                    test   dx, 0FFh 
                    jnz    _exp_err
                    xor    ax, ax
                    inc    si 

                _exp_end:
                    pop    dx 
                    pop    cx 
                    ret 

        _check_sem_exp ENDP 

    ;---------------------------------------------------

_text ENDS 