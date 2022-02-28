;------------------------------------------------------------------------
; Il seguente modulo si occupa della gestione delle espressioni 
; matematiche, generando la corripsettiva espressione in notazione 
; polacca inversa e valutando quest'ultima, si otterà un valore 
; immediato in caso di istruzioni formate da soli valori, e una 
; serie di istruzioni altrimenti.
; Le variabili facente parte di un espressione devono essere di tipo
; intero, in caso contraio verrà restituito un errore.
; Un espressione può essere utilizzata solo durante la dichiarazione 
; o l'assegnazione di una variabile.
;
;  DECL A = [3+7] Ok.    WRITE [3+2], OFF, A No.
;
; La seguenti istruzioni sono quindi riscrivibili in questo modo:
;
;   DECL A = [3+7]    ->   WRITE B, OFF, A
;   DECL B = [3+2]
;
; Ovviamente questa scelta comporta un uso più massiccio delle 
; variabili e una maggiore difficoltà di programmazione, ma ehi
; stiamo parlando di un interprete/compilatore BASIC, nessuno utilizza 
; più questo linguaggio.
;
; Le istruzioni generate fanno un uso massiccio dello stack, per 
; esempio ecco le istruzioni generate per la seguente espressione:
;
;    (A+6)/(B+7) ->    mov    sp, OFFSET B ->  AX = result
;                      push   A   
;                      push   6
;                      pop    bx 
;                      pop    ax 
;                      add    ax, bx 
;                      push   ax
;                      push   B
;                      push   7
;                      pop    bx
;                      pop    ax
;                      add    ax, bx
;                      push   ax
;                      pop    bx 
;                      pop    ax
;                      xor    dx, dx
;                      div    bx
;                      push   ax
;                      pop    ax                      
; 
; Tutto ciò è dovuto a un più semplice sviluppo da parte mia del 
; generatore stesso, in questo modo ogni operazione viene effettuata
; con il registro AX e BX, occasionalmente DX, e le semplici e ripetute
; istruzioni per la gestione dello stack.
; Quindi le espressioni vengono gestite con un paradigma Stack oriented.
;------------------------------------------------------------------------

;----------------------------------------
; Macro expression

    PRIORITY_L0     EQU    0h 
    PRIORITY_L1     EQU    1h

    END_EXP         EQU    0E0h 

;----------------------------------------

shift_to_token MACRO token 

    push    cx 
    mov     cl, 4h 
    shl     token, cl 
    pop     cx 

ENDM

_text SEGMENT PARA PUBLIC 

    exp_buffer                db    49h DUP(0h) 
    _gen_exp_error            db    0h 

    _exp_to_polish PROC NEAR  

        push    bp 

        mov     bp, sp 
        sub     sp, 3h

        push    bx 
        push    cx
        push    di 

        mov     di, OFFSET exp_buffer
        mov     WORD PTR [bp-2h], sp 
        mov     BYTE PTR [bp-3h], 0h 

        _convert_to_polish:
            cmp    BYTE PTR [si], ANGBRCKR_ID
            je     _end_convertion

            cmp    BYTE PTR [si], OFF_ID 
            je     _get_offset 
            cmp    BYTE PTR [si], NOT_ID 
            je     _try_simplify

            token_in_al   BYTE PTR [si]
            cmp    al, SEPARATOR_TOKEN
            je     _brck 
            cmp    al, OPERATOR_TOKEN
            je     _is_operator 

            ;value or identifier 
            cmp    al, VALUE_TOKEN
            je     _is_value 

            push   WORD PTR [si+2h]
            call   _search_var_in_vat
            add    sp, 2h 

            test   ax, 0FFh 
            jz     _like_value 

            mov     al, IDENTIFIER_TOKEN
            shift_to_token  al
            mov     BYTE PTR [di], al
            push    di
            mov     di, OFFSET roffset_ovt 
            mov     di, WORD PTR [di]
            mov     ax, WORD PTR [di+3h]
            pop     di 
            mov     WORD PTR [di+1h], ax 
            add     di, 3h 
            add     si, 4h
            jmp     _convert_to_polish

            _like_value:
                xor    ax, ax 
                mov    cx, 4h
                jmp    _is_value_

            _is_value:
                mov     cx, 3h
                mov     ax, WORD PTR [si+1h]

            _is_value_:
                mov     WORD PTR [di+1h], ax
                mov     al, VALUE_TOKEN
                shift_to_token  al
                mov     BYTE PTR [di], al
                add     di, 3h 
                add     si, cx
                jmp     _convert_to_polish 

            _brck:
                cmp     BYTE PTR [si], BRCKR_ID 
                je      _pop_operand_in_brck 

                mov     al, BYTE PTR [si]
                xor     ah, ah 
                push    ax 
                inc     si 
                inc     BYTE PTR [bp-3h]
                jmp     _convert_to_polish

                _pop_operand_in_brck:
                    pop     ax 
                    dec     BYTE PTR [bp-3h]
                    cmp     ax, BRCKL_ID
                    je      _inc_si

                    mov     BYTE PTR [di], al 
                    inc     di
                    mov     cx, 0FFh  
                    jmp     _complete_op

                _inc_si:
                    inc    si 
                    jmp    _convert_to_polish

            _is_operator:
                test   BYTE PTR [bp-3h], 0FFh
                jnz    _continue_op

                inc     BYTE PTR [bp-3h]
                mov     al, BYTE PTR [si]
                xor     ah, ah 
                push    ax
                inc     si 
                jmp     _convert_to_polish 

            _continue_op: 
                pop     cx 
                cmp     cx, BRCKL_ID 
                je      _continue_push 
        
                cmp    BYTE PTR [si], MUL_ID 
                jb     _priority_1

                xor    al, al 
                jmp    _continue 

                _priority_1:
                    mov    al, PRIORITY_L1

                _continue:
                    cmp     cx, MUL_ID 
                    jae     _pop_operand 
 
                    cmp     al, PRIORITY_L1 
                    je      _pop_operand
 
                _continue_push:
                    push    cx 
                    mov     al, BYTE PTR [si]
                    xor     ah, ah 
                    push    ax 
                    inc     si 
                    inc     BYTE PTR [bp-3h]
                    jmp     _convert_to_polish 

                _pop_operand:
                    mov     BYTE PTR [di], cl 
                    inc     di 
                    mov     al, BYTE PTR [si]
                    xor     ah, ah 
                    push    ax 
                    inc     si 
                    xor     cx, cx
                    jmp     _complete_op  

            _get_offset:
                push    WORD PTR [si+3h]
                call    _search_var_in_vat
                add     sp, 2h 

                push    di 
                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]
                mov     ax, WORD PTR [di+3h]
                pop     di 
                sub     ax, 5h
                mov     cx, 5h 
                jmp     _store 

            _try_simplify:
                token_in_al    BYTE PTR [si+1]
                cmp     al, VALUE_TOKEN
                je      _simplify_it 

                ;cannot simplify
                push    WORD PTR [si+3h]
                call    _search_var_in_vat
                add     sp, 2h 

                test    ax, 0FFh 
                jz      _store_0

                push    di 
                mov     di, OFFSET roffset_ovt
                mov     di, WORD PTR [di]
                mov     ax, WORD PTR [di+3h]
                pop     di 
                mov     cl, BYTE PTR [si]
                mov     BYTE PTR [di+3h], cl
                mov     WORD PTR [di+1h], ax 
                mov     al, IDENTIFIER_TOKEN
                shift_to_token  al 
                mov     BYTE PTR [di], al 
                add     di, 4h
                add     si, 5h
                jmp     _convert_to_polish

                _simplify_it:
                    mov    ax, WORD PTR [si+2h]
                    not    ax 
                    mov    cx, 4h
                    jmp    _store 

            _store_0:
                mov     ax, 0FFFFh 
                mov     cx, 5h

            _store:
                mov     WORD PTR [di+1h], ax 
                mov     al, VALUE_TOKEN
                shift_to_token  al 
                mov     BYTE PTR [di], al 
                add     di, 3h 
                add     si, cx
                jmp     _convert_to_polish

            _complete_op:
                token_in_al    BYTE PTR [di-4h]
                cmp     al, VALUE_TOKEN
                jne     _convert_to_polish
                token_in_al    BYTE PTR [di-7h]
                cmp     al, VALUE_TOKEN
                jne     _convert_to_polish

                mov     al, BYTE PTR [di-1h]
                cmp     al, ADD_ID 
                je      _do_add 
                cmp     al, MOD_ID 
                je      _do_mod 
                cmp     al, DIV_ID 
                je      _do_div 
                cmp     al, AND_ID 
                je      _do_and 
                cmp     al, MUL_ID 
                je      _do_mul 

                ;do or
                mov    ax, WORD PTR [di-6h]
                or     ax, WORD PTR [di-3h]
                sub    di, 7h 
                mov    WORD PTR [di+1h], ax
                add    di, 3h 
                jmp    _complete_op_con

                _do_add:
                    mov    ax, WORD PTR [di-6h]
                    add    ax, WORD PTR [di-3h]
                    sub    di, 7h 
                    mov    WORD PTR [di+1h], ax
                    add    di, 3h 
                    jmp    _complete_op_con

                _do_mod: 
                    mov    ax, WORD PTR [di-6h]
                    cmp    WORD PTR [di-3h], 0h 
                    je     _division_error 
                    cwd
                    idiv    WORD PTR [di-3h]
                    sub    di, 7h 
                    mov    WORD PTR [di+1h], dx
                    add    di, 3h 
                    jmp    _complete_op_con

                _do_div:
                    mov    ax, WORD PTR [di-6h]
                    cmp    WORD PTR [di-3h], 0h 
                    je     _division_error 
                    cwd 
                    idiv    WORD PTR [di-3h]
                    sub    di, 7h 
                    mov    WORD PTR [di+1h], ax 
                    add    di, 3h
                    jmp    _complete_op_con 

                _do_and:
                    mov    ax, WORD PTR [di-6h]
                    and    ax, WORD PTR [di-3h]
                    sub    di, 7h 
                    mov    WORD PTR [di+1h], ax
                    add    di, 3h 
                    jmp    _complete_op_con

                _do_mul:
                    mov    ax, WORD PTR [di-6h]
                    imul    WORD PTR [di-3h]
                    sub    di, 7h 
                    mov    WORD PTR [di+1h], ax
                    add    di, 3h 
                 
                _complete_op_con:
                    cmp    cx, 0FFh 
                    je     _pop_operand_in_brck
                    cmp    cx, 0FEh 
                    je     _end_convertion
                    jmp    _convert_to_polish

        _division_error:
            mov    bx, OFFSET exp_error
            mov    BYTE PTR [bx], 1h 
            mov    ax, 1h 
            jmp    _done_work

        _end_convertion:
            xor    ax, ax
            cmp    sp, WORD PTR [bp-2h]
            je     _done_work 

            pop    ax 
            xor    ah, ah 
            mov    BYTE PTR [di], al 
            inc    di 
            mov    cx, 0FEh 
            jmp    _complete_op

        _done_work:
            mov    BYTE PTR [di], END_EXP
            pop    di 
            pop    cx 
            pop    bx 
            add    sp, 3h 
            pop    bp 
            ret 

    _exp_to_polish ENDP
 
    _gen_exp_ins PROC NEAR 

        push    ax 
        push    si 

        mov     si, OFFSET stack_offset
        mov     ax, WORD PTR [si]
        neg     ax 
        sub     ax, 5h
        mov     si, OFFSET exp_buffer

        push    ax
        token_in_al   BYTE PTR [si]
        cmp    al, VALUE_TOKEN
        jne    _continue_gen_ins 

        cmp    BYTE PTR [si+3h], END_EXP
        jne    _continue_gen_ins

        add    sp, 2h
        mov    BYTE PTR es:[di], 0B8h ;mov   ax, imm16
        mov    ax, WORD PTR [si+1h]
        mov    WORD PTR es:[di+1h], ax
        add    di, 3h
        jmp    _end_gen_ins_exp_

        _continue_gen_ins:
            pop     ax
            mov     BYTE PTR es:[di], 0BCh ;mov  sp, imm16
            mov     WORD PTR es:[di+1h], ax 
            add     di, 3h

        _gen_ins_exp:
            cmp    BYTE PTR [si], END_EXP 
            je     _end_gen_ins_exp 

            token_in_al   BYTE PTR [si]
            cmp    al, VALUE_TOKEN
            je     _store_value 
            cmp    al, IDENTIFIER_TOKEN
            je     _store_id 

            ;is   operator 
            cmp    BYTE PTR [si], NOT_ID 
            je     _do_not 
            
            mov    WORD PTR es:[di], 585Bh ;pop  bx / pop  ax
            add    di, 2h

            cmp    BYTE PTR [si], ADD_ID 
            je     _do_add 
            cmp    BYTE PTR [si], OR_ID 
            je     _do_or 
            cmp    BYTE PTR [si], AND_ID 
            je     _do_and 
            cmp    BYTE PTR [si], DIV_ID 
            je     _do_div 
            cmp    BYTE PTR [si], MUL_ID 
            je     _do_mul 

            ;mod instruction 
            mov    BYTE PTR es:[di], 99h ;cwd
            mov    WORD PTR es:[di+1h], 0FBF7h ;idiv   bx
            mov    BYTE PTR es:[di+3h], 52h ;push  dx
            add    di, 4h
            inc    si 
            jmp    _gen_ins_exp

            _do_add:
                mov    ax, 0D801h ;add  ax, bx
                jmp    _continue 

            _do_or:
                mov    ax, 0D809h ;or   ax, bx 
                jmp    _continue 

            _do_and:
                mov    ax, 0D821h ;and   ax, bx 
                jmp    _continue

            _do_div:
                mov    BYTE PTR es:[di], 99h ; cwd
                inc    di 
                mov    ax, 0FBF7h ;idiv   bx
                jmp    _continue

            _do_mul: 
                mov    ax, 0EBF7h ;mul  bx

            _continue:
                mov    WORD PTR es:[di], ax 
                mov    BYTE PTR es:[di+2h], 50h ;push  ax
                add    di, 3h 
                inc    si 
                jmp    _gen_ins_exp 

            _do_not:
                mov    BYTE PTR es:[di], 58h ;pop   ax
                mov    WORD PTR es:[di+1h], 0D0F7h ;not  ax 
                mov    BYTE PTR es:[di+3h], 50h ;push  ax 
                inc    si 
                add    di, 4h
                jmp    _gen_ins_exp 

            _store_value:
                mov    ax, WORD PTR [si+1h]
                mov    BYTE PTR es:[di], 0B8h ;mov  ax, imm16 
                mov    WORD PTR es:[di+1h], ax 
                mov    BYTE PTR es:[di+3h], 50h ;push  ax 
                add    di, 4h
                add    si, 3h 
                jmp    _gen_ins_exp 

            _store_id:   
                mov    ax, WORD PTR [si+1h]
                mov    WORD PTR es:[di], 0B6FFh ;push  WORD PTR [bp-offset] 
                mov    WORD PTR es:[di+2h], ax 
                add    di, 4h 
                add    si, 3h 
                jmp    _gen_ins_exp 

        _end_gen_ins_exp:
            mov    BYTE PTR es:[di], 58h ;pop  ax
            inc    di 
            
        _end_gen_ins_exp_:    
            pop    si 
            pop    ax 
            ret 

    _gen_exp_ins ENDP 

_text ENDS  