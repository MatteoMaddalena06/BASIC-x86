;--------------------------------------------------------------------------------
; Il seguente modulo si occupa di generare le corrispettive 
; istruzioni in linguaggio macchina per ogni singola istruzione
; dell'interprete/compilatore BASIC.
;
; Tutte le informazione riguardo alle variabili verrano memorizzate
; all'interno della var_table così strutturata
;
;  _3byte__  _2byte__ __1B__ ____1byte____
;  nome_var | offset | type | dec_inc/dim |
;
; I nomi all'interno della var table vengono terminati dal carattere
; terminatore "0", l'offset è in funzione del (bp) durante l'esecuzione
; del programma, il tipo indica il tipo della variabile stessa 
; assegnato in maniera automatica da parte del compilatore in funzione
; di ciò che si vuole memorizzare.
;
; DECL A = 10  => int type    DECL A(4) = "CIAO" => string
;
; Il controllo sul tipo non sarà effettuato solamente se la 
; la variabile non è stata ancora definita in caso contrario
; verrà confrontato il tipo della variabile per controllare 
; se l'assegnazione è coretta.
;
; A(4) = "CIAO" OK.   con A come stringa.
;
; A = 5 NO OK.  con A dichiarata come stringa
;
; Il campo dec_inc/dim contiene la dimensione della variabile stringa, nel caso
; lo sia, o il numero di unità cui deve essere incrementata/decrementata
; la variabile del ciclo for se il tipo della variabile è FOR_TYPE.
;
; In funzione della modalità operativa dell'interprete/compilatore verrà 
; compilata ad ogni istruzione la tabbella line_info contente le informazioni
; riguardo alla linea corrente e le passate, il singolo elemento è così
; strutturato:
;
; ___1byte___   _______2byte_______ _______1byte________  ____1byte___ __1B__
; line_number | OFFSET_INSTRUCTION | type_of_instruction | line_to_jmp | sub |
;
; La tabella avendo una dimensione di 1536bytes e visto che ogni singolo 
; elemento ha dimensione 5byte,fa si che il massimo di righe cui un programma 
; può estendersi è di 256.
;--------------------------------------------------------------------------------

INCLUDE code_gen_lib.asm
INCLUDE expression.asm

;--------------------------------------------------------------
; Code generator's macro    

    SEGMENT_USR_CODE   EQU   106Fh 
    OFFSET_USR_CODE    EQU   0Fh
    SEGMENT_USR_STACK  EQU   206Fh
    OFFSET_USR_STACK   EQU   0FFFFh
    
    IO_INTT        EQU    0h 
    IO_STT_ID      EQU    1h
    IO_STT         EQU    2h 

;-------------------------------------------------------------- 

_text SEGMENT PARA PUBLIC

    var_table         db     2BCh DUP(0h)       ;where stored the information about variable
    line_info         db     600h DUP(0h)       ;where stored information about single line
    current_vip       dw     OFFSET_USR_CODE+2h ;offset of instructions' buffer
    current_ovt       dw     0h                 ;offset of variables' table
    current_ili       dw     0h                 ;offset of line informations' table
    stack_offset      dw     0h                 ;offset to sub at bp
    roffset_ovt       dw     0h                 ;store the index of var in var table   
    num_of_var        db     0h                 ;count number of variable
    num_of_linfo      db     0h 
    second_call       db     0h      
    where_store_int   dw     0h 

    gen_error         db     0h

    _generate_instruction PROC NEAR  

        push    bx 
        push    es 

        mov     ax, BASIC_SEG
        mov     es, ax 

        mov     bx, OFFSET mode_interpreter
        
        mov     al, BYTE PTR [bx]
        xor     ah, ah 

        push    ax
        call    _analyze_and_create 
        add     sp, 2h
        test    ax, 0FFh 
        jz      _no_err 

        mov    bx, OFFSET gen_error 
        mov    BYTE PTR [bx], 1h
        jmp    _end 

        _no_err:
            mov    bx, OFFSET gen_error
            mov    BYTE PTR [bx], 0h 
             
        _end:
            pop    es 
            pop    bx
            ret 
 
    _generate_instruction ENDP 

    _analyze_and_create PROC NEAR 

        push    bp 

        mov     bp, sp 

        push    dx
        push    cx
        push    bx
        push    es 

        xor     cx, cx
        mov     ax, SEGMENT_USR_CODE
        mov     es, ax 

        mov     bx, OFFSET second_call

        test    BYTE PTR [bx], 0FFh 
        jnz     continue 

        mov     si, OFFSET token_buffer 
        mov     di, OFFSET current_vip
        mov     di, WORD PTR [di]
        cmp     BYTE PTR [si], EOT 
        je      _an_no_err

        cmp     BYTE PTR [bp+4h], MULTI_LINE_MODE
        jne     continue 

        add     si, 3h

    continue:
        token_in_al   BYTE PTR [si]
        cmp     al, IDENTIFIER_TOKEN
        je      _decl_ins 
        
        mov     al, BYTE PTR [si]
        
        cmp     al, FOR_ID 
        je      _for_ins 

        inc     si 

        cmp     al, DECL_ID 
        je      _decl_ins   
        cmp     al, IF_ID   
        je      _if_ins 
        cmp     al, WHILE_ID 
        je      _while_ins 
        cmp     al, GOTO_ID 
        je      _goto_ins 
        cmp     al, PRINT_ID 
        je      _print_ins 
        cmp     al, INPUT_ID 
        je      _input_ins 
        cmp     al, WRITE_ID 
        je      _write_ins 
        cmp     al, READ_ID 
        je      _read_ins 
        cmp     al, INP_ID 
        je      _inp_ins 
        cmp     al, OUTP_ID 
        je      _outp_ins 
        cmp     al, ABS_ID 
        je      _abs_ins
        cmp     al, WEND_ID 
        je      _end_of_while_block
        cmp     al, NEXT_ID 
        je      _end_of_for_block

        ;generate code for machc instruction
        ;------------------------------------------------------
        _create_instuction_for_machc:
            push   di
        
        _loop_machc:
            mov     al, BYTE PTR [si+1h]
            mov     BYTE PTR es:[di], al 
            add     si, 3h 
            inc     di 
            cmp     BYTE PTR [si], EOT 
            je      _end_machc
            inc     si 
            jmp     _loop_machc    

        _end_machc:   
            mov     cx, di 
            pop     di 
            cmp     BYTE PTR [bp+4h], MULTI_LINE_MODE
            jne     _an_no_err 

            mov     al, NONE 
            mov     ah, 0h 
            jmp     _multi_line_mode 

        ;------------------------------------------------------

        ;Generate code for decl instruction
        ;------------------------------------------------------
        _decl_ins:
            push    di 
            push    si 
            cmp     al, DECL_ID 
            jne     _continue

            dec     si 

            _continue:
                call   _create_var
                pop    si  
                test   ax, 0FFh 
                jnz    _error_gen

            push    WORD PTR [si+2h]
            call    _search_var_in_vat
            add     sp, 2h 

            mov     bx, OFFSET roffset_ovt
            mov     bx, WORD PTR [bx]

            cmp    BYTE PTR [si+4h], BRCKL_ID 
            je     _is_string 

            add    si, 5h 
            cmp    BYTE PTR [si], SUB_ID 
            jne    _l0

            inc    si 

        _l0:
            cmp    BYTE PTR [si], ANGBRCKL_ID 
            je     _is_exp 
            token_in_al  BYTE PTR [si]
            cmp    al, VALUE_TOKEN 
            je     _is_value 
            cmp    al, INSTRUCTION_TOKEN
            je     _is_instruction 
            cmp    al, STRING_TOKEN
            je     _String 

            ;is identifier
            push    WORD PTR [si+2h]
            call    _search_var_in_vat
            add     sp, 2h 

            test    ax, 0FFh 
            jz      _no_present

            push    bx 
            mov     bx, OFFSET roffset_ovt
            mov     bx, WORD PTR [bx]
            cmp     BYTE PTR [bx+5h], INT_TYPE  
            jne     _identifier_type_string 

            mov     ax, WORD PTR [bx+3h]
            pop     bx
            mov     WORD PTR es:[di], 868Bh ;mov  ax, WORD PTR [bp-offset]
            mov     WORD PTR es:[di+2h], ax 
            mov     WORD PTR es:[di+4h], 8689h 
            mov     ax, WORD PTR [bx+3h]
            mov     WORD PTR es:[di+6h], ax ;mov  WORD PTR [bp-offset], ax 
            add     di, 8h 
            jmp     _end_decl_ins 

            _identifier_type_string:
                mov     si, WORD PTR [bx+3h]
                pop     bx 
                mov     cl, BYTE PTR [bx+6h]
                mov     bx, WORD PTR [bx+3h]
                xor     ch, ch

                _loop:
                    mov    WORD PTR es:[di], 868Ah ;mov  al, BYTE PTR [bp-offset]
                    mov    WORD PTR es:[di+2h], si 
                    mov    WORD PTR es:[di+4h], 8688h ;mov  BYTE PTR [bp-offset], al
                    mov    WORD PTR es:[di+6h], bx
                    add    di, 8h 
                    inc    si 
                    inc    bx 
                    loop   _loop                  

                    mov    WORD PTR es:[di], 86C6h ;mov  BYTE PTR [bp-offset], 0h 
                    mov    WORD PTR es:[di+2h], bx 
                    mov    BYTE PTR es:[di+4h], 0h 
                    add    di, 5h
                    jmp    _end_decl_ins  

            _no_present:
                mov     WORD PTR es:[di], 86C7h ;mov  WORD PTR [bp-offset], 0h 
                mov     ax, WORD PTR [bx+3h]
                mov     WORD PTR es:[di+2h], ax 
                mov     WORD PTR es:[di+4h], 0h
                add     di, 6h 
                jmp     _end_decl_ins 
 
            _is_exp:   
                inc     si

                call    _exp_to_polish

                test    ax, ax 
                jnz     _error_gen 
                
                call    _gen_exp_ins 

                mov     WORD PTR es:[di], 8689h ;mov  WORD PTR [bp-offset], ax
                mov     ax, WORD PTR [bx+3h]
                mov     WORD PTR es:[di+2h], ax 
                add     di, 4h
                jmp     _end_decl_ins

            _is_value:  
                mov    WORD PTR es:[di], 86C7h ;mov  WORD PTR [bp-offset], imm16 
                mov    ax, WORD PTR [bx+3h]
                mov    WORD PTR es:[di+2h], ax 
                mov    ax, WORD PTR [si+1h]
                mov    WORD PTR es:[di+4h], ax 
                add    di, 6h
                jmp    _end_decl_ins 

            _is_instruction:
                push    bx 
                mov     bx, OFFSET mode_interpreter
                mov     al, BYTE PTR [bx]
                xor     ah, ah 
                mov     bx, OFFSET second_call
                mov     WORD PTR [bx], 1h 
                pop     bx 
                push    ax 
                call    _analyze_and_create ;instruction result -> ax
                add     sp, 2h 
                mov     WORD PTR es:[di], 8689h ;mov  WORD PTR [bp-offset], ax 
                mov     ax, WORD PTR [bx+3h]
                mov     WORD PTR es:[di+2h], ax
                add     di, 4h 
                mov     bx, OFFSET second_call
                mov     WORD PTR [bx], 0h 
                jmp     _end_decl_ins 

            _is_string:
                add    si, 0Ah 
                token_in_al  BYTE PTR [si]
                cmp     al, IDENTIFIER_TOKEN
                je      _is_string_id

            _String:
                ;is string 
                mov     si, WORD PTR [si+2h]
                mov     bx, WORD PTR [bx+3h]

                _loop1:
                    cmp    BYTE PTR [si], 22h 
                    je     _stop1
                    cmp    BYTE PTR [si], 27h 
                    je     _stop1
                    mov    al, BYTE PTR [si]
                    mov    WORD PTR es:[di], 86C6h ;mov BYTE PTR [bp-offset], imm8
                    mov    WORD PTR es:[di+2h], bx 
                    mov    BYTE PTR es:[di+4h], al 
                    inc    bx 
                    inc    si 
                    add    di, 5h
                    jmp    _loop1

                _stop1:
                    mov    WORD PTR es:[di], 86C6h ;mov BYTE PTR [bp-offset], 0h
                    mov    WORD PTR es:[di+2h], bx 
                    mov    BYTE PTR es:[di+4h], 0h
                    add    di, 5h 
                    jmp    _end_decl_ins 

                _is_string_id:
                    push    WORD PTR [si+2h]
                    call    _search_var_in_vat
                    add     sp, 2h 

                    xor     ch, ch
                    mov     cl, BYTE PTR [bx+6h]
                    mov     bx, WORD PTR [bx+3h]
                    push    bx
                    mov     bx, OFFSET roffset_ovt
                    mov     si, WORD PTR [bx]
                    mov     si, WORD PTR [si+3h]
                    pop     bx
        
                    _loop2:
                        mov    WORD PTR es:[di], 868Ah ;mov   al, BYTE PTR [bp-offset]
                        mov    WORD PTR es:[di+2h], si
                        mov    WORD PTR es:[di+4h], 8688h ;mov  BYTE PTR [bp-offset], al
                        mov    WORD PTR es:[di+6h], bx                         
                        inc    si 
                        inc    bx 
                        add    di, 8h 
                        loop   _loop2 

                    mov    WORD PTR es:[di], 86C6h ;mov  BYTE PTR [bp-offset], 0h 
                    mov    WORD PTR es:[di+2h], bx 
                    mov    BYTE PTR es:[di+4h], 0h 
                    add    di, 5h 

            _end_decl_ins:
                mov    cx, di 
                pop    di 
                cmp    BYTE PTR [bp+4h], MULTI_LINE_MODE
                jne    _an_no_err

                mov    al, NONE 
                mov    ah, 0h 
                jmp    _multi_line_mode

        ;------------------------------------------------------

        ;Generate code for if instruction
        ;------------------------------------------------------   
        _if_ins:
            push    di 
            mov     ax, NONE 
            push    ax
            call    _gen_code_for_cond
            add     sp, 2h 

            test    ax, 0FFh 
            jnz     _error_gen

            mov     cx, di 
            pop     di 

            mov     bx, OFFSET current_ili
            push    bx
            mov     bx, WORD PTR [bx]
            add     bx, OFFSET line_info

            mov     al, BYTE PTR [si-3h]
            mov     BYTE PTR [bx], al 
            mov     WORD PTR [bx+1h], di 
            mov     BYTE PTR [bx+3h], NONE 
            
            _search_line_to_jmp:
                cmp     BYTE PTR [si], EOT  
                je      _find_it 
                inc     si 
                jmp     _search_line_to_jmp

            _find_it:
                sub     si, 3h 
                mov     al, BYTE PTR [si+1h]
                mov     BYTE PTR [bx+4h], al 

            mov     BYTE PTR [bx+5h], 0h 

            pop     bx 
            add     WORD PTR [bx], 6h 
            mov     bx, OFFSET current_vip
            mov     WORD PTR [bx], cx 
            mov     bx, OFFSET num_of_linfo
            inc     BYTE PTR [bx]

            mov     di, cx

            mov     ax, INS_TO_CONS 
            push    ax 
            call    _complete_jmp_ins
            add     sp, 2h 

            mov     ax, CONS_TO_INS
            push    ax 
            call    _complete_jmp_ins
            add     sp, 2h 
            jmp     _an_no_err 
        ;------------------------------------------------------ 

        ;Generate for's code instruction
        ;------------------------------------------------------    
        _for_ins:
            sub     sp, 2h
            call    _create_var 

            test    ax, 0FFh  
            jnz     _error_gen 

            push    di 
            mov     di, OFFSET current_ovt
            mov     di, WORD PTR [di]
            add     di, OFFSET var_table
            sub     di, 7h 
            mov     bx, WORD PTR [di+3h]
            pop     di 

            add     si, 6h
            token_in_al   BYTE PTR [si]
            cmp     al, VALUE_TOKEN
            je      _is_for_value 

            ;is variable 
            push    WORD PTR [si+2h]
            call    _search_var_in_vat
            add     sp, 2h 
            
            add     si, 5h 

            test    ax, 0FFh 
            jz      _like_value 

            ;var is present 
            push    di 
            mov     di, OFFSET roffset_ovt
            mov     di, WORD PTR [di]
            mov     dx, WORD PTR [di+3h]
            cmp     BYTE PTR [di+5h], STRING_TOKEN
            pop     di 
            je      _error_gen

            mov     WORD PTR es:[di], 868Bh ;mov ax, WORD PTR [bp-offset] 
            mov     WORD PTR es:[di+2h], dx
            mov     WORD PTR es:[di+4h], 8689h ;mov WORD PTR [bp-offset], ax
            mov     WORD PTR es:[di+6h], bx 
            add     di, 8h 
            mov     dx, 8h 
            jmp     _continue_for

            _like_value:
                xor     ax, ax
                mov     WORD PTR es:[di], 86C7h ;mov  WORD PTR [bp-offset], imm16 
                mov     WORD PTR es:[di+2h], bx 
                jmp     _l1

            _is_for_value:
                mov     WORD PTR es:[di], 86C7h ;mov  WORD PTR [bp-offset], imm16 
                mov     WORD PTR es:[di+2h], bx 
                mov     ax, WORD PTR [si+1h]
                add     si, 4h

            _l1:
                mov     WORD PTR es:[di+4h], ax 
                add     di, 6h
                mov     dx, 6h 

            _continue_for: 
                token_in_al    BYTE PTR [si]
                cmp     al, VALUE_TOKEN
                je      _is_value_for 

                ;is variable 
                push    WORD PTR [si+2h]
                call    _search_var_in_vat
                add     sp, 2h 

                add     si, 5h 

                test    ax, 0FFh 
                jnz     _var_for_pres 

                xor     ax, ax 
                mov     cx, di 
                mov     WORD PTR es:[di], 0BE81h ;cmp  WORD PTR [bp-offset], imm16
                mov     WORD PTR es:[di+2h], bx
                jmp     _l1_

                _is_value_for:
                    mov     cx, di 
                    mov     WORD PTR es:[di], 0BE81h ;cmp  WORD PTR [bp-offset], imm16
                    mov     WORD PTR es:[di+2h], bx
                    mov     ax, WORD PTR [si+1h]
                    add     si, 4h 

                _l1_:
                    mov     WORD PTR es:[di+4h], ax 
                    add     di, 6h
                    jmp     _next_ins_to_for 

                _var_for_pres:
                    push    di 
                    mov     di, OFFSET roffset_ovt
                    mov     di, WORD PTR [di]
                    mov     ax, WORD PTR [di+3h]
                    cmp     BYTE PTR [di+5h], STRING_TOKEN
                    pop     di 
                    je      _error_gen

                    mov     cx, di 
                    mov     WORD PTR es:[di], 868Bh ;mov ax, WORD PTR [bp-offset]
                    mov     WORD PTR es:[di+2h], ax
                    add     di, 4h 
                    mov     WORD PTR es:[di], 8639h ;cmp WORD PTR [bp-offset], ax
                    mov     WORD PTR es:[di+2h], bx 
                    add     di, 4h  
                    
            _next_ins_to_for:
                cmp     WORD PTR [si+1h], 0h 
                jl      _is_less 

                ;is great 
                mov     WORD PTR es:[di], 8F0Fh  ;jg xx
                add     di, 4h 
                jmp     _end_for_ins 

                _is_less:   
                    mov    WORD PTR es:[di], 8C0Fh ;jl xx
                    add    di, 4h 

            _end_for_ins:
                add     sp, 2h
                mov     bx, OFFSET current_ili
                push    bx
                mov     bx, WORD PTR [bx]
                add     bx, OFFSET line_info

                push    bx 
                mov     bx, OFFSET token_buffer
                mov     al, BYTE PTR [bx+1h]
                pop     bx 
                mov     BYTE PTR [bx], al 
                mov     WORD PTR [bx+1h], cx
                mov     BYTE PTR [bx+3h], FOR_TYPE
                mov     BYTE PTR [bx+4h], 0h 
                mov     BYTE PTR [bx+5h], dl  

                pop     bx 
                add     WORD PTR [bx], 6h 
                mov     bx, OFFSET current_vip
                mov     WORD PTR [bx], di 
                mov     bx, OFFSET num_of_linfo
                inc     BYTE PTR [bx]

                mov     ax, INS_TO_CONS 
                push    ax 
                call    _complete_jmp_ins
                add     sp, 2h 
                jmp     _an_no_err
        ;------------------------------------------------------ 

        ;Generate code for NEXT instruction
        ;------------------------------------------------------ 
        _end_of_for_block:
            push    di 
            mov     bx, OFFSET num_of_var
            mov     cl, BYTE PTR [bx]
            xor     ah, ah 

            mov     bx, OFFSET current_ovt
            mov     bx, WORD PTR [bx]
            add     bx, OFFSET var_table
            sub     bx, 7h 

            _for_loop:
                cmp     BYTE PTR [bx+5h], FOR_TYPE
                je      _end_for_loop 
                sub     bx, 7h 
                loop    _for_loop 

            _end_for_loop:
                mov     BYTE PTR [bx+5h], INT_TYPE
                mov     WORD PTR es:[di], 8681h ;add  WORD PTR [bp-offset], imm16
                mov     ax, WORD PTR [bx+3h]
                mov     WORD PTR es:[di+2h], ax 
                mov     al, BYTE PTR [bx+6h]
                cbw     
                mov     WORD PTR es:[di+4h], ax 
                add     di, 6h

            mov    BYTE PTR es:[di], 0E9h ;jmp  xxx 
            add    di, 3h

            mov     cx, di 
            pop     di 

            mov     bx, OFFSET current_ili
            push    bx
            mov     bx, WORD PTR [bx]
            add     bx, OFFSET line_info

            mov     al, BYTE PTR [si-3h]
            mov     BYTE PTR [bx], al 
            mov     WORD PTR [bx+1h], di 
            mov     BYTE PTR [bx+3h], NONE 
            mov     BYTE PTR [bx+4h], 0h
            mov     BYTE PTR [bx+5h], 0h  

            pop     bx 
            add     WORD PTR [bx], 6h 
            mov     bx, OFFSET current_vip
            mov     WORD PTR [bx], cx 
            mov     bx, OFFSET num_of_linfo
            inc     BYTE PTR [bx]

            mov     di, cx

            mov     ax, INS_TO_CONS 
            push    ax 
            call    _complete_jmp_ins
            add     sp, 2h 
            
            mov     ax, FOR_SEARCH
            push    ax 
            call    _complete_jmp_ins
            add     sp, 2h 
            jmp     _an_no_err   
        ;------------------------------------------------------ 

        ;Generate code for while instruction
        ;------------------------------------------------------
        _while_ins:
            push    di 
            mov     ax, WHILE_ID 
            push    ax 
            call    _gen_code_for_cond 
            add     sp, 2h 

            mov     cx, di 
            pop     di 

            mov     bx, OFFSET current_ili
            push    bx
            mov     bx, WORD PTR [bx]
            add     bx, OFFSET line_info

            mov     al, BYTE PTR [si-3h]
            mov     BYTE PTR [bx], al 
            mov     WORD PTR [bx+1h], di 
            mov     BYTE PTR [bx+3h], WHILE_TYPE
            mov     BYTE PTR [bx+4h], 0h 
            mov     BYTE PTR [bx+5h], 0h 

            pop     bx 
            add     WORD PTR [bx], 6h 
            mov     bx, OFFSET current_vip
            mov     WORD PTR [bx], cx 
            mov     bx, OFFSET num_of_linfo
            inc     BYTE PTR [bx]

            mov     di, cx

            mov     ax, INS_TO_CONS 
            push    ax 
            call    _complete_jmp_ins
            add     sp, 2h 
            jmp     _an_no_err
        ;------------------------------------------------------

        ;Generate code for WEND instruction
        ;------------------------------------------------------
        _end_of_while_block:
            push    di 
            mov     BYTE PTR es:[di], 0E9h ;jmp  xxx 
            add     di, 3h

            mov     cx, di 
            pop     di 

            mov     bx, OFFSET current_ili
            push    bx 
            mov     bx, WORD PTR [bx]
            add     bx, OFFSET line_info 

            mov     al, BYTE PTR [si-3h]
            mov     BYTE PTR [bx], al 
            mov     WORD PTR [bx+1h], di 
            mov     BYTE PTR [bx+3h], NONE 
            mov     BYTE PTR [bx+4h], 0h 
            mov     BYTE PTR [bx+5h], 0h 

            pop     bx 
            add     WORD PTR [bx], 6h 
            mov     bx, OFFSET current_vip
            mov     WORD PTR [bx], cx 
            mov     bx, OFFSET num_of_linfo
            inc     BYTE PTR [bx]

            mov     di, cx

            mov     ax, WHILE_SEARCH
            push    ax 
            call    _complete_jmp_ins
            add     sp, 2h 
            jmp     _an_no_err 
        ;------------------------------------------------------
   
        ;Generate code for goto instruction
        ;------------------------------------------------------
        _goto_ins:
            push    di 
            mov     BYTE PTR es:[di], 0E9h ;jmp  xxx 
            add     di, 3h
            
            mov     cx, di 
            pop     di 

            mov     bx, OFFSET current_ili
            push    bx
            mov     bx, WORD PTR [bx]
            add     bx, OFFSET line_info

            mov     al, BYTE PTR [si-3h]
            mov     BYTE PTR [bx], al 
            mov     WORD PTR [bx+1h], di 
            mov     BYTE PTR [bx+3h], NONE 
            mov     al, BYTE PTR [si+1h]
            mov     BYTE PTR [bx+4h], al
            mov     BYTE PTR [bx+5h], 0h 

            pop     bx 
            add     WORD PTR [bx], 6h 
            mov     bx, OFFSET current_vip
            mov     WORD PTR [bx], cx 
            mov     bx, OFFSET num_of_linfo
            inc     BYTE PTR [bx]

            mov     di, cx

            mov     ax, INS_TO_CONS 
            push    ax 
            call    _complete_jmp_ins
            add     sp, 2h 
            
            mov     ax, CONS_TO_INS  
            push    ax 
            call    _complete_jmp_ins
            add     sp, 2h 
            jmp     _an_no_err
        ;------------------------------------------------------

        ;Generate code for print instruction
        ;------------------------------------------------------
        _print_ins:
            xor     dx, dx
            xor     cx, cx
            push    di 

            mov     bx, OFFSET stack_offset
            mov     ax, WORD PTR [bx]
    
            neg     ax
            sub     ax, 5h
            push    ax

        _print_next:
            mov     BYTE PTR es:[di], 0BCh ;mov  sp, imm16
            mov     WORD PTR es:[di+1h], ax 
            add     di, 3h

        _loop_print:
            xor     bx, bx
            token_in_al   BYTE PTR [si]
            cmp     al, IDENTIFIER_TOKEN
            je      _is_id_print  
            cmp     al, STRING_TOKEN
            je      _is_string_print 
            
            ;is value to print 
            mov     ax, WORD PTR [si+1h]
            add     si, 3h

            _value_print:
                mov     BYTE PTR es:[di], 0B8h ;mov    ax, INT_TYPE 
                mov     WORD PTR es:[di+1h], IO_INTT 
                mov     BYTE PTR es:[di+3h], 50h ;push   ax
                add     di, 4h 
                add     cx, 2h
                mov     BYTE PTR es:[di], 0B8h  ;mov   ax, value 
                mov     WORD PTR es:[di+1h], ax 
                mov     BYTE PTR es:[di+3h], 50h ;push   ax
                add     cx, 2h
                add     di, 4h 
                inc     dx
                jmp     _next_print

            _is_id_print:
                push    WORD PTR [si+2h]
                call    _search_var_in_vat
                add     sp, 2h 

                add     si, 4h

                test    ax, 0FFh 
                jz      _id_no_pres

                push    di 
                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]
                mov     ax, WORD PTR [di+3h]
                cmp     BYTE PTR [di+5h], STRING_TYPE 
                pop     di 
                je      _is_string_id_ 
                
                mov     BYTE PTR es:[di], 0B8h ;mov    ax, INT_TYPE 
                mov     WORD PTR es:[di+1h], IO_INTT 
                mov     BYTE PTR es:[di+3h], 50h ;push   ax
                add     di, 4h 
                add     cx, 2h
                mov     WORD PTR es:[di], 0B6FFh ;push  WORD PTR [bp-offset]  
                mov     WORD PTR es:[di+2h], ax 
                add     cx, 2h
                add     di, 4h
                inc     dx 
                jmp     _next_print

                _id_no_pres:
                    xor    ax, ax 
                    jmp    _value_print 

            _is_string_id_:
                sub     ax, 5h 
                mov     BYTE PTR es:[di], 0B8h
                mov     WORD PTR es:[di+1h], IO_STT_ID ;mov   ax, STRING_TYPE_ID 
                mov     BYTE PTR es:[di+3h], 50h ;push   ax  
                add     di, 4h 
                mov     BYTE PTR es:[di], 0B8h ;mov   ax, offset_var 
                mov     WORD PTR es:[di+1h], ax
                mov     BYTE PTR es:[di+3h], 50h ;push   ax
                add     cx, 4h 
                add     di, 4h 
                inc     dx
                jmp     _next_print

            _is_string_print:
                push    si 
                mov     si, WORD PTR [si+2h]

                mov     BYTE PTR es:[di], 0B8h   
                mov     WORD PTR es:[di+1h], IO_STT ;mov  ax, STRING_TYPE
                mov     BYTE PTR es:[di+3h], 50h  ;push   ax 
                add     di, 4h

                _string_loop:
                    cmp     BYTE PTR [si], 22h 
                    je      _end_gen_ins
                    cmp     BYTE PTR [si], 27h 
                    je      _end_gen_ins 
                    xor     ah, ah 
                    mov     al, BYTE PTR [si]
                    mov     BYTE PTR es:[di], 0B8h ;mov   ax, imm16
                    mov     WORD PTR es:[di+1h], ax 
                    mov     BYTE PTR es:[di+3h], 50h ;push  ax 
                    add     di, 4h 
                    inc     si 
                    add     cx, 2h 
                    jmp     _string_loop
            
                _end_gen_ins: 
                    mov     WORD PTR es:[di], 0C031h ;xor    ax, ax 
                    mov     BYTE PTR es:[di+2h], 50h ;push   ax
                    add     di, 3h 
                    add     cx, 2h
                    pop     si 
                    add     si, 4h 
                    inc     dx 

            _next_print:
                cmp    BYTE PTR [si], EOT 
                je     _end_print_ins 
                inc    si 
                jmp    _loop_print 

            _end_print_ins:
                mov     BYTE PTR es:[di], 0B8h ;mov   ax, num_el  
                mov     WORD PTR es:[di+1h], dx 
                mov     BYTE PTR es:[di+3h], 50h ;push ax
                add     di, 4h 
                add     cx, 2h
                pop     ax 
                sub     ax, 2h 
                mov     BYTE PTR es:[di], 0B8h  ;mov  ax, start_offset
                mov     WORD PTR es:[di+1h], ax 
                mov     BYTE PTR es:[di+3h], 50h ;push   ax 
                add     di, 4h 
                add     cx, 2h 
                mov     WORD PTR es:[di], 22CDh ;int   22h
                mov     WORD PTR es:[di+2h], 0C481h 
                mov     WORD PTR es:[di+4h], cx ;add  sp, imm16
                add     di, 6h 

                mov     cx, di 
                pop     di 
                cmp     BYTE PTR [bp+4h], MULTI_LINE_MODE 
                jne     _an_no_err 

                mov    al, NONE 
                mov    ah, 0h 
                jmp    _multi_line_mode 
        ;------------------------------------------------------

        ;Generate code for input instruction
        ;------------------------------------------------------
        _input_ins: 
            xor     dx, dx
            xor     cx, cx
            push    di

            mov     bx, OFFSET stack_offset
            mov     ax, WORD PTR [bx]

            neg     ax 
            sub     ax, 5h 
    
            push    ax
            mov     BYTE PTR es:[di], 0BCh  ;mov   sp, imm16
            mov     WORD PTR es:[di+1h], ax 
            add     di, 3h

        _loop_input: 
            push    WORD PTR [si+2h]
            call    _search_var_in_vat 
            add     sp, 2h 

            test    ax, 0FFh 
            jz      _create_input_var

            mov     bx, OFFSET roffset_ovt
            mov     bx, WORD PTR [bx]
            mov     ax, WORD PTR [bx+3h]
            mov     bl, BYTE PTR [bx+5h]
            sub     ax, 5h

            cmp     bl, INT_TYPE
            je      _input_value 

            ;string input 
            mov     bx, IO_STT
            jmp     _store_offset

            _input_value:
                mov      bx, IO_INTT

            _store_offset:
                mov      BYTE PTR es:[di], 0B8h ;mov   ax, TYPE_INPUT
                mov      WORD PTR es:[di+1h], bx
                mov      BYTE PTR es:[di+3h], 50h ;push  ax 
                mov      BYTE PTR es:[di+4h], 0B8h ;mov   ax, offset var
                mov      WORD PTR es:[di+5h], ax 
                mov      BYTE PTR es:[di+7h], 50h ;push  ax 

            _next_it_input: 
                add    cx, 4h
                add    di, 8h 
                add    si, 4h 
                inc    dx
                cmp    BYTE PTR [si], EOT 
                je     _input_ins_end 
                inc    si 
                jmp    _loop_input 

            _input_ins_end:
                mov    BYTE PTR es:[di], 0B8h  ;mov   ax, num_el
                mov    WORD PTR es:[di+1h], dx
                mov    BYTE PTR es:[di+3h], 50h ;push  ax
                add    di, 4h
                add    cx, 2h
                pop    ax 
                sub    ax, 2h 
                mov    BYTE PTR es:[di], 0B8h  ;mov   ax, start_offset
                mov    WORD PTR es:[di+1h], ax
                mov    BYTE PTR es:[di+3h], 50h ;push  ax
                add    di, 4h 
                add    cx, 2h
                mov    WORD PTR es:[di], 21CDh ;int  21h
                mov    WORD PTR es:[di+2h], 0C481h 
                mov    WORD PTR es:[di+4h], cx ;add  sp, imm16
                add    di, 6h 

                mov    cx, di 
                pop    di 
                cmp    BYTE PTR [bp+4h], MULTI_LINE_MODE
                jne    _an_no_err 
                    
                mov    al, NONE 
                mov    ah, 0h 
                jmp    _multi_line_mode  

            _create_input_var:
                call   _create_var
                jmp    _loop_input 
        ;------------------------------------------------------

        ;Generate code for write instruction
        ;------------------------------------------------------
        _write_ins:
            push    di 

            token_in_al    BYTE PTR [si]
            cmp     al, IDENTIFIER_TOKEN
            je      _p1_write_id 

            mov     ax, WORD PTR [si+1h] 
            add     si, 4h

        _is_wvalue:
            mov     BYTE PTR es:[di], 0B8h ;mov   ax, segment 
            mov     WORD PTR es:[di+1h], ax  
            mov     WORD PTR es:[di+3h], 0C08Eh ;mov   es, ax
            add     di, 5h 
            jmp     _go_to_p2

            _p1_write_id:
                push    WORD PTR [si+2h]
                call    _search_var_in_vat
                add     sp, 2h 

                add     si, 5h

                test    ax, ax 
                jz      _rzero_segment

                push    di 
                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]
                mov     ax, WORD PTR [di+3h]
                cmp     BYTE PTR [di+5h], STRING_TYPE
                pop     di 
                je      _error_type 
                mov     WORD PTR es:[di], 868Eh ;mov   es, WORD PTR [bp-offset]
                mov     WORD PTR es:[di+2h], ax
                add     di, 4h 
                jmp     _go_to_p2

            _rzero_segment:
                xor     ax, ax 
                jmp     _is_wvalue

            _go_to_p2:
                token_in_al     BYTE PTR [si]
                cmp     al, IDENTIFIER_TOKEN
                je      _p2_write_id 

                mov     ax, WORD PTR [si+1h]
                add     si, 4h

            _is_roffset_value:
                mov     BYTE PTR es:[di], 0BEh ;mov   si, offset
                mov     WORD PTR es:[di+1h], ax  
                add     di, 3h 
                jmp     _go_to_p3

            _p2_write_id:   
                push    WORD PTR [si+2h]
                call    _search_var_in_vat
                add     sp, 2h 

                add     si, 5h 
                test    ax, ax 
                je      _rzero_offset 

                push    di 
                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]
                cmp     BYTE PTR [di+5h], STRING_TYPE
                je      _error_type
                mov     ax, WORD PTR [di+3h]
                pop     di 

                mov     WORD PTR es:[di], 0B68Bh ;mov  si, WORD PTR [bp-offset]
                mov     WORD PTR es:[di+2h], ax 
                add     di, 4h
                jmp     _go_to_p3 

            _rzero_offset:
                xor     ax, ax 
                jmp     _is_roffset_value

            _go_to_p3:
                token_in_al    BYTE PTR [si]
                cmp     al, IDENTIFIER_TOKEN
                je      _p3_write_id 

                ;p3 is value
                mov     ax, WORD PTR [si+1h]

                test    ah, ah 
                jnz     _error_gen

                add     si, 3h 
            
            _p3_is_value:
                mov     BYTE PTR es:[di], 26h 
                mov     WORD PTR es:[di+1h], 04C6h ;mov  BYTE PTR es:[si], imm8
                mov     BYTE PTR es:[di+3h], al 
                add     di, 4h 
                jmp     _end_write_ins 

            _p3_write_id:
                push    WORD PTR [si+2h]
                call    _search_var_in_vat
                add     sp, 2h 

                add     si, 4h 

                test    ax, ax 
                jz      _p3_is_zero 

                push    di 
                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]
                mov     ax, WORD PTR [di+3h]
                cmp     BYTE PTR [di+5h], STRING_TYPE
                pop     di 
                je      _error_gen

                mov     WORD PTR es:[di], 868Ah ;mov   al, BYTE PTR [bp-offset]
                mov     WORD PTR es:[di+2h], ax
                mov     BYTE PTR es:[di+4h], 26h 
                mov     WORD PTR es:[di+5h], 0488h ;mov   BYTE PTR es:[si], al 
                add     di, 7h 
                jmp     _end_write_ins

            _p3_is_zero:
                xor    ax, ax 
                jmp    _p3_is_value

        _end_write_ins:
            mov     cx, di 
            pop     di 
            cmp     BYTE PTR [bp+4h], MULTI_LINE_MODE
            jne     _an_no_err

            mov    al, NONE 
            mov    ah, 0h 
            jmp    _multi_line_mode

        ;------------------------------------------------------

        ;Generate code for read insruction
        ;------------------------------------------------------
        _read_ins:
            push    di 
            token_in_al   BYTE PTR [si]
            cmp     al, IDENTIFIER_TOKEN
            je      _read_id

            mov     ax, WORD PTR [si+1h]
            add     si, 4h 

        _is_rvalue:
            mov     BYTE PTR es:[di], 0B8h ;mov   ax, segment 
            mov     WORD PTR es:[di+1h], ax  
            mov     WORD PTR es:[di+3h], 0C08Eh ;mov   es, ax
            add     di, 5h 
            jmp     _continue_read 

            _read_id:
                push    WORD PTR [si+2h]
                call    _search_var_in_vat
                add     sp, 2h 

                add     si, 5h 

                test    ax, ax 
                jz      _zero_segment 

                push    di 
                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]
                mov     ax, WORD PTR [di+3h]
                cmp     BYTE PTR [di+5h], STRING_TYPE
                pop     di 
                je      _error_type 
                mov     WORD PTR es:[di], 868Eh ;mov   es, WORD PTR [bp-offset]
                mov     WORD PTR es:[di+2h], ax
                add     di, 4h 
                jmp     _continue_Read 

            _zero_segment:
                xor     ax, ax 
                jmp     _is_rvalue 

            _continue_read: 
                token_in_al     BYTE PTR [si]
                cmp     al, IDENTIFIER_TOKEN
                je      _read_id2 

                mov     ax, WORD PTR [si+1h]
                add     si, 3h

            _is_value_offset:
                mov     BYTE PTR es:[di], 26h 
                mov     BYTE PTR es:[di+1h], 0A0h ;mov   al, BYTE PTR es:[offset]
                mov     WORD PTR es:[di+2h], ax 
                add     di, 4h 
                jmp     _end_read_ins  

            _read_id2:
                push    WORD PTR [si+2h]
                call    _search_var_in_vat
                add     sp, 2h 

                add     si, 4h

                test    ax, ax 
                jz      _zero_offset 

                push    di 
                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]
                mov     ax, WORD PTR [di+3h]
                cmp     BYTE PTR [di+5h], STRING_TYPE 
                pop     di 
                je      _error_type

                mov     WORD PTR es:[di], 0B68Bh ;mov   si, WORD PTR [bp-offset] 
                mov     WORD PTR es:[di+2h], ax 
                mov     BYTE PTR es:[di+4h], 26h 
                mov     WORD PTR es:[di+5h], 048Ah ;mov  al, BYTE PTR es:[si]
                add     di, 7h 
                jmp     _end_read_ins 

            _zero_offset:
                xor    ax, ax 
                jmp    _is_value_offset

            _end_read_ins:
                mov     WORD PTR es:[di], 0E430h ;xor    ah, ah 
                add     di, 2h 

                mov     cx, di 
                pop     di 
                cmp     BYTE PTR [bp+4h], MULTI_LINE_MODE
                jne     _an_no_err

                mov    al, NONE 
                mov    ah, 0h 
                jmp    _multi_line_mode

        ;------------------------------------------------------

        ;Generate code for inp instruction
        ;------------------------------------------------------
        _inp_ins: 
            push     di 
            push     WORD PTR [si+2h]
            call     _search_var_in_vat
            add      sp, 2h 

            test     ax, 0FFh 
            jz       _inp_create_var 

            mov      bx, OFFSET roffset_ovt
            mov      bx, WORD PTR [bx]
            cmp      BYTE PTR [bx+5h], STRING_TYPE
            je       _error_type
            push     bx
            
        _continue_inp:
            add      si, 5h 
            token_in_al   BYTE PTR [si]
            cmp      al, IDENTIFIER_TOKEN
            je       _inp_id 

            ;is value 
            mov      BYTE PTR es:[di], 0BAh   ;mov dx, imm16
            mov      ax, WORD PTR [si+1h]
            mov      WORD PTR es:[di+1h], ax
            add      di, 3h  
            add      si, 4h 
            jmp      _next_arg_inp

            _inp_id: 
                push     WORD PTR [si+2h]
                call     _search_var_in_vat
                add      sp, 2h 

                add      si, 5h 

                test     ax, 0FFh 
                jz       _inp_no_pres 

                ;pres 
                push     di 
                mov      di, OFFSET roffset_ovt
                mov      di, WORD PTR [di]
                cmp      BYTE PTR [di+5h], STRING_TYPE
                je       _error_type
                mov      ax, WORD PTR [di+3h]
                pop      di 
                mov      WORD PTR es:[di], 968Bh ;mov   dx, WORD PTR [bp-offset]
                mov      WORD PTR es:[di+2h], ax 
                add      di,  4h 
                jmp       _next_arg_inp
                
            _inp_no_pres:
                mov     BYTE PTR es:[di], 0BAh  ;mov   dx, 0h
                mov     WORD PTR es:[di+1h], 0h 
                add     di, 3h 

        _next_arg_inp:
            cmp     WORD PTR [si+1h], 8h 
            je      _inp_8bit 
            cmp     WORD PTR [si+1h], 10h 
            je      _inp_16bit
            jmp     _error_gen

            _inp_8bit:
                mov     BYTE PTR es:[di], 0ECh  ;in  al, dx
                mov     WORD PTR es:[di+1h], 0E430h ;xor  ah, ah 
                add     di, 3h 
                jmp     _store_value

            _inp_16bit:            
                mov     BYTE PTR [si], 0EDh ;in  ax, dx
                inc     di 

        _store_value:
            pop     bx 
            mov     ax, WORD PTR [bx+3h]  
            mov     WORD PTR es:[di], 8689h ;mov  WORD PTR [bp-offset], ax
            mov     WORD PTR es:[di+2h], ax 
            add     di, 4h

            mov     cx, di 
            pop     di 
            cmp     BYTE PTR [bp+4h], MULTI_LINE_MODE
            jne     _an_no_err
         
            mov    al, NONE 
            mov    ah, 0h 
            jmp    _multi_line_mode

        _inp_create_var:
            add     sp, 2h 
            call    _create_var 
            jmp     _inp_ins 
        ;------------------------------------------------------

        ;Generate code for outp instruction
        ;------------------------------------------------------
        _outp_ins:
            push     di 
            token_in_al    BYTE PTR [si]
            cmp      al, IDENTIFIER_TOKEN
            je       _out_id

            ;value 
            mov      BYTE PTR es:[di], 0BAh  ;mov    dx, imm16
            mov      ax, WORD PTR [si+1h]
            mov      WORD PTR es:[di+1h], ax 
            add      di, 3h 
            add      si, 4h
            jmp      _outp_next_arg

            _out_id:
                push    WORD PTR [si+2h]
                call    _search_var_in_vat
                add     sp, 2h 

                add     si, 5h 

                test    ax, 0FFh 
                jz      _no_pres 

                ;pres 
                push    di
                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]
                cmp     BYTE PTR [di+5h], STRING_TYPE
                je      _error_type
                mov     ax, WORD PTR [di+3h]
                pop     di 
                mov     WORD PTR es:[di], 968Bh ;mov    dx, WORD PTR [bp-offset]
                mov     WORD PTR es:[di+2h], ax 
                add     di, 4h
                jmp     _outp_next_arg 

                _no_pres:
                    mov     BYTE PTR es:[di], 0BAh   ;mov    dx, 0h 
                    mov     WORD PTR es:[di+1h], 0h 
                    add     di, 3h

            _outp_next_arg:
                token_in_al   BYTE PTR [si]
                cmp      al, IDENTIFIER_TOKEN
                je       __is_id__

                ;is value
                cmp      WORD PTR [si+5h], 8h 
                je       _8bit_outp 
                cmp      WORD PTR [si+5h], 10h 
                je       _16bit_outp
                jmp      _error_gen 

                _8bit_outp:
                    mov      BYTE PTR es:[di], 0B0h   ;mov   al, imm8
                    cmp      BYTE PTR [si], SUB_ID 
                    je       _skip_sign
                    jmp      _next_outp  

                    _skip_sign:
                        inc     si 
                    
                    _next_outp:
                        mov      al, BYTE PTR [si+1h]
                        mov      BYTE PTR es:[di+1h], al 
                        mov      al, 0EEh 
                        add      di, 2h
                        jmp      _end_outp

                _16bit_outp:
                    mov      BYTE PTR es:[di], 0B8h  ;mov   ax, imm16
                    cmp      BYTE PTR [si], SUB_ID 
                    je       _skip_sign1
                    jmp      _next_outp1

                    _skip_sign1:
                        inc     si 

                    _next_outp1:
                        mov      ax, WORD PTR [si+1h]
                        mov      WORD PTR es:[di+1h], ax 
                        mov      al, 0EFh
                        add      di, 3h
                        jmp      _end_outp
                
                __is_id__:
                    push     WORD PTR [si+2h]
                    call     _search_var_in_vat
                    add      sp, 2h 

                    test     ax, 0FFh 
                    jz       _outp_no_pres

                    ;var pres 
                    push     di 
                    mov      di, OFFSET roffset_ovt
                    mov      di, WORD PTR [di]
                    cmp      BYTE PTR [di+5h], STRING_TYPE
                    je       _error_type
                    mov      ax, WORD PTR [di+3h]
                    pop      di 
                    cmp      WORD PTR [si+6h], 8h 
                    je       _8bit_outp_id 
                    cmp      WORD PTR [si+6h], 10h 
                    je       _16bit_outp_id
                    jmp      _error_gen 

                    _8bit_outp_id:
                        mov     WORD PTR es:[di], 868Ah ;mov  al, BYTE PTR [bp-offset]
                        mov     WORD PTR es:[di+2h], ax 
                        mov     al, 0EEh 
                        add     di, 4h
                        jmp     _end_outp 
                        
                    _16bit_outp_id:
                        mov     WORD PTR es:[di], 868Bh ;mov   ax, WORD PTR [bp-offset]
                        mov     WORD PTR es:[di+2h], ax 
                        mov     al, 0EFh 
                        add     di, 4h
                        jmp     _end_outp 

                    _outp_no_pres:
                        mov     WORD PTR es:[di], 00B0h ;mov    al, 0h 
                        mov     al, 0EEh
                        add     di, 2h

            _end_outp:
                mov      BYTE PTR es:[di], al ;out  dx, ax || out  dx, al
                inc      di
 
                mov      cx, di 
                pop      di 
                cmp      BYTE PTR [bp+4h], MULTI_LINE_MODE
                jne      _an_no_err

                ;multiline mode
                mov    al, NONE 
                mov    ah, 0h 
                jmp    _multi_line_mode 
        ;------------------------------------------------------

        ;Generate code for ABS instruction
        ;------------------------------------------------------
        _abs_ins: 
            mov     bx, OFFSET stack_offset
            mov     ax, WORD PTR [bx]
            
            neg     ax 
            sub     ax, 5h 
            
            mov     BYTE PTR es:[di], 0BCh ;mov   sp, imm16
            mov     WORD PTR es:[di+1h], ax 
 
            push    di 
            token_in_al  BYTE PTR [si]
            cmp     al, IDENTIFIER_TOKEN
            je      _id_abs_create

            ;value abs create 
            mov     BYTE PTR es:[di], 0B8h  ; mov    ax, im16
            cmp     BYTE PTR [si], SUB_ID 
            jne     _next_abs

            inc     si 

            _next_abs:
                mov     ax, WORD PTR [si+1h]
                mov     WORD PTR es:[di+1h], ax 
                add     di, 3h 
        
        _continue_abs:
            mov     WORD PTR es:[di], 20CDh ;int   20h
            add     di, 2h
            jmp     _end_abs 

            _id_abs_create:
                push    WORD PTR [si+2h]
                call    _search_var_in_vat
                add     sp, 2h 

                test    ax, 0FFh 
                jz      _var_not_pres

                ;var pres 
                push    di 
                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]
                cmp     BYTE PTR [di+5h], STRING_TYPE
                je      _error_type
                mov     ax, WORD PTR [di+3h]
                pop     di 
                mov     WORD PTR es:[di], 468Bh ; mov  ax, WORD PTR [bp-offset]
                mov     WORD PTR es:[di+2h], ax 
                add     di, 4h 
                jmp     _continue_abs 

            _var_not_pres:
                mov      BYTE PTR es:[di], 0B8h ;mov   ax, 0h
                mov      WORD PTR es:[di+1], 0h 
                add      di, 3h  
                jmp      _continue_abs 

        _end_abs:
            mov     cx, di 
            pop     di 
            cmp     BYTE PTR [bp+4h], MULTI_LINE_MODE
            jne     _an_no_err

            mov    al, NONE 
            mov    ah, 0h  
        ;------------------------------------------------------    

        _multi_line_mode:
            mov    bx, OFFSET second_call
            test   BYTE PTR [bx], 0FFh 
            jnz    _an_no_err 

            mov    bx, OFFSET current_vip
            mov    WORD PTR [bx], cx 
            push   cx

            mov    bx, OFFSET token_buffer
            mov    cl, BYTE PTR [bx+1h]

            mov    si, OFFSET current_ili
            push   si 
            mov    si, WORD PTR [si]
            add    si, OFFSET line_info

            mov    BYTE PTR [si], cl 
            mov    WORD PTR [si+1h], di 
            mov    BYTE PTR [si+3h], al 
            mov    BYTE PTR [si+4h], ah
            mov    BYTE PTR [si+5h], 0h 

            pop    si 
            add    WORD PTR [si], 6h
            mov    si, OFFSET num_of_linfo
            inc    BYTE PTR [si]

            pop    cx 
            mov    di, cx 

            mov    ax, INS_TO_CONS
            push   ax 
            call   _complete_jmp_ins
            add    sp, 2h  
            jmp    _an_no_err

        _error_type:
            mov    bx, OFFSET type_of_error 
            mov    BYTE PTR [bx], 1h 

        _error_gen:
            add     sp, 2h
            mov     ax, 1h 
            jmp     _end_analyze

        _an_no_err:
            mov     bx, OFFSET where_store_int 
            mov     WORD PTR [bx], cx 
            mov     di, cx
            xor     ax, ax 

        _end_analyze:
            pop    es 
            pop    bx
            pop    cx
            pop    dx
            pop    bp 
            ret

    _analyze_and_create ENDP 

_text ENDS 
