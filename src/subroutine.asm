_text SEGMENT PARA PUBLIC 

    zero_division     db     0h 
    op2_dim           dw     0h
    op2_inc           db     0h  
    op1_dim           dw     0h
    op1_inc           db     0h 

    _mount_subroutine PROC NEAR 

        push    ax 
        push    es 

        xor     ax, ax 
        mov     es, ax 

        mov     WORD PTR es:[88h], _print_subroutine 
        mov     WORD PTR es:[8Ah], BASIC_SEG

        mov     WORD PTR es:[84h], _input_subroutine 
        mov     WORD PTR es:[86h], BASIC_SEG    

        mov     WORD PTR es:[0h], _illegal_math_op
        mov     WORD PTR es:[2h], BASIC_SEG    

        mov     WORD PTR es:[80h], _abs_subroutine 
        mov     WORD PTR es:[82h], BASIC_SEG  

        mov     WORD PTR es:[8Ch], _string_cmp_subroutine 
        mov     WORD PTR es:[8Eh], BASIC_SEG    

        mov     WORD PTR es:[90h], _leave_subroutine 
        mov     WORD PTR es:[92h], BASIC_SEG

        pop     es 
        pop     ax 
        ret    

    _mount_subroutine ENDP 

_print_subroutine:
    push    bp 

    mov     bp, sp 

    push    ax 
    push    bx
    push    cx
    push    dx

    mov     cx, WORD PTR [bp+0Ah]
    mov     bp, WORD PTR [bp+8h]
    jmp     _output_parameters

    _devation_jmp:
        loop    _output_parameters
        jmp     _end_print_sub

    _output_parameters:
        cmp     BYTE PTR [bp], IO_INTT
        je      _print_integer 
        cmp     BYTE PTR [bp], IO_STT_ID 
        je      _print_id_string 

        ;print string 
        mov     ax, 0FFFEh 
        xor     dx, dx
        sub     bp, 2h   

        _print_string:
            cmp     BYTE PTR [bp], 0h 
            je      _end_print_string 

            push    WORD PTR [bp]
            call    _draw_char
            add     sp, 2h 
            add     bp, ax  
            jmp     _print_string

        _end_print_string:
            test    dx, dx 
            jnz     _continue_string_id
            sub     bp, 2h
            jmp     _next_parameters

        _print_integer:
            push    cx 
            xor     cx, cx
            mov     bx, 0Ah 
            mov     ax, WORD PTR [bp-2h]

            cmp     ax, MAX_16BIT_VALUE
            jbe     _divide_number

            neg     ax 

            mov     dx, 2Dh 
            push    dx
            call    _draw_char
            add     sp, 2h

            _divide_number:
                xor     dx, dx
                div     bx 

                add     dx, 30h 
                push    dx 
                inc     cx 

                test    ax, ax 
                jnz     _divide_number
            
            _print_number:
                call    _draw_char
                add     sp, 2h
                loop    _print_number 

            sub     bp, 4h
            pop     cx 
            mov     ax, 20h 
            push    ax 
            call    _draw_char
            add     sp, 2h
            jmp     _next_parameters 

        _print_id_string:
            push    bp 
            mov     bp, WORD PTR [bp-2h]
            mov     dx, 1h
            mov     ax, 1h  
            jmp     _print_string

            _continue_string_id:
                pop     bp 
                sub     bp, 4h 

    _next_parameters:
        jmp     _devation_jmp 

    _end_print_sub:
        mov     ax, 0Dh 
        push    ax 
        call    _draw_char
        add     sp, 2h
        pop     dx 
        pop     cx 
        pop     bx 
        pop     ax 
        pop     bp 
        iret 

_input_subroutine:
    push    bp 

    mov     bp, sp 

    push    ax 
    push    cx 

    mov     cx, WORD PTR [bp+0Ah]
    mov     bp, WORD PTR [bp+8h]
    jmp     _input_parameters

    _next_input_parameters:
        loop    _input_parameters
        jmp     _end_subroutine 

    _input_parameters:
        push    cx 
        mov     ah, 03h 
        xor     bh, bh 
        int     10h
        pop     cx 

        mov     ah, 2h 
        xor     bh, bh 
        sub     dl, 2h 
        int     10h 

        mov     ax, 2Dh 
        push    ax 
        call    _draw_char
        add     sp, 2h
        mov     ax, 3Eh 
        push    ax 
        call    _draw_char
        add     sp, 2h

        cmp     BYTE PTR [bp], IO_INTT
        je      _input_integers 

        ;input  string 
        push    bp 
        mov     bp, WORD PTR [bp-2h]
    
        _take_string_input:
            cmp     BYTE PTR [bp], 0h 
            je      _end_string_input
            
            xor     ah, ah 
            int     16h 
            push    ax 
            call    _draw_char
            add     sp, 2h 
            mov     BYTE PTR [bp], al 
            inc     bp 
            jmp     _take_string_input

        _end_string_input:
            mov     ax, 0Dh 
            push    ax 
            call    _draw_char
            add     sp, 2h 
            pop     bp 
            sub     bp, 4h 
            jmp     _next_input_parameters

        _input_integers:
            xor     bx, bx
            push    cx

        _retake:
            xor     ah, ah 
            int     16h 

            push    ax
            call    _draw_char
            add     sp, 2h

            cmp     al, 0Dh 
            je      _end_integer_input 

            cmp     al, 2Dh 
            je      _sign_value 
            cmp     al, 8h 
            je      _retake

            cmp     al, 30h
            jb      _type_error_input 
            cmp     al, 39h 
            ja      _type_error_input

            xor     ah, ah 
            sub     al, 30h 
            push    ax 
            mov     ax, bx 
            mov     bx, 0Ah 
            mul     bx 
            mov     bx, ax 
            pop     ax 
            add     bx, ax 
            xor     ch, ch 
            mov     cl, 4h 
            jmp     _take_integer_input 

            _sign_value:
                mov     ch, 1h 
                mov     cl, 5h

            _take_integer_input:
                xor     ah, ah 
                int     16h 

                push    ax 
                call    _draw_char
                add     sp, 2h

                cmp     al, 0Dh 
                je      _end_integer_input 
                cmp     al, 8h 
                je      _delete_digit

                cmp     al, 30h
                jb      _type_error_input 
                cmp     al, 39h 
                ja      _type_error_input

                xor     ah, ah 
                sub     al, 30h 
                push    ax 
                mov     ax, bx 
                mov     bx, 0Ah 
                mul     bx 
                mov     bx, ax 
                pop     ax 
                add     bx, ax 
                dec     cl 
                jnz     _take_integer_input

                mov     ax, 0Dh 
                push    ax 
                call    _draw_char
                add     sp, 2h

        _end_integer_input:
            test   ch, ch
            jz     _no_neg 

            neg    bx 

            _no_neg:
                push   bp 
                mov    bp, WORD PTR [bp-2h]
                mov    WORD PTR [bp], bx 
                pop    bp 
                sub    bp, 4h 
                pop    cx
                jmp    _next_input_parameters

        _type_error_input:
            add    sp, 2h
            mov    ax, 0Dh 
            push   ax 
            call   _draw_char
            add    sp, 2h
            mov    bx, OFFSET type_of_error
            mov    BYTE PTR [bx], 2h 
            call   _print_err_msg
            call   _clear_flags
            jmp    _input_parameters

        _delete_digit:
            xor     dx, dx 
            mov     ax, bx 
            mov     bx, 0Ah 
            div     bx 
            mov     bx, ax 
            inc     cl
            jmp     _take_integer_input

    _end_subroutine:
        pop    cx 
        pop    ax 
        pop    bp 
        iret 

_string_cmp_subroutine:
    push    bp 

    mov     bp, sp 

    push    bx 
    push    cx 
    push    dx 
    push    si 

    push    bp 
    mov     al, BYTE PTR [bp+8h]
    add     bp, 0Ah 
    mov     bx, OFFSET op2_dim 
    xor     si, si 

    _get_lenght_par__:
        cmp     BYTE PTR [bp], INT_TYPE
        je      _is_int_cmp__ 
        cmp     BYTE PTR [bp], STRING_CHAR
        je      _is_string_cmp__ 

        ;string id 
        push    bp 
        mov     bp, WORD PTR [bp+2h]
        mov     cx, 1h 
        mov     dx, 1h 
        jmp     _lenght_string_char__

        _was_string_id__:
            pop     bp 
            add     bp, 4h 
            jmp     _next_par__

        _is_int_cmp__:
            mov    ah, BYTE PTR [bp+2h]
            mov    BYTE PTR [bx+si], ah
            add    bp, 4h 
            jmp    _next_par__ 

        _is_string_cmp__:
            add    bp, 2h 
            xor    dx, dx 
            mov    cx, 2h 

            _lenght_string_char__:
                cmp     BYTE PTR [bp], 0h 
                je      _end_lenght_stc 

                add     bp, cx 
                inc     BYTE PTR [bx+si]
                jmp     _lenght_string_char__

            _end_lenght_stc:
                test     dx, dx 
                jnz      _was_string_id__
                add      bp, 2h  

    _next_par__:
        test     si, si 
        jnz      _end_get_lenght__ 
        add      si, 3h 
        jmp      _get_lenght_par__

    _end_get_lenght__:
        mov      ah, BYTE PTR [bx]

        cmp      al, EQU_CON
        je       _check_equ__  
        cmp      al, NEQU_CON 
        je       _check_nequ__  
        cmp      al, BOR_CON 
        je       _check_bor__ 
        cmp      al, BORE_CON 
        je       _check_bore__ 
        cmp      al, GRT_CON 
        je       _check_grt__ 

        ;check grte 
        cmp      BYTE PTR [bx+3h], ah 
        jae      _cond_sat__ 
        jmp      _cond_no_sat__ 

        _check_bor__:
            cmp      BYTE PTR [bx+3h], ah 
            jb       _cond_sat__ 
            jmp      _cond_no_sat__ 

        _check_bore__:
            cmp      BYTE PTR [bx+3h], ah 
            jbe      _cond_sat__ 
            jmp      _cond_no_sat__ 

        _check_grt__: 
            cmp      BYTE PTR [bx+3h], ah 
            ja       _cond_sat__ 
            jmp      _cond_no_sat__

        _check_equ__:
            cmp      BYTE PTR [bx+3h], ah 
            je       _check_equality__ 
            jmp      _cond_no_sat__ 

        _check_nequ__:
            cmp      BYTE PTR [bx+3h], ah 
            jne      _cond_sat__ 

    _check_equality__:
        pop      bp 
        push     bp 
        add      bp, 0Ah
        mov      bx, OFFSET op2_dim
        xor      dx, dx

    _check_equ_loop__: 
        cmp      BYTE PTR [bp], INT_TYPE
        je       _cond_sat_equ__ 
        cmp      BYTE PTR [bp], STRING_CHAR
        je       _string_eq__ 

        ;string id 
        add      bp, 2h 
        mov      cx, WORD PTR [bp]
        mov      WORD PTR [bx], cx
        mov      BYTE PTR [bx+2h], 1h
        jmp      _next_op_equ__ 

        _string_eq__:
            add    bp, 2h 
            mov    WORD PTR [bx], bp 
            mov    BYTE PTR [bx+2h], 2h

            _next_op_string:
                cmp     BYTE PTR [bp], 0h 
                je      _next_op_equ__ 
                add     bp, 2h 
                jmp     _next_op_string

        _next_op_equ__: 
            add    bp, 2h
            test   dx, dx  
            jnz    _control_equ__  
            add    bx, 3h  
            mov    dx, 1h
            jmp    _check_equ_loop__
            
    _control_equ__:
        mov      bx, OFFSET op2_dim
        mov      cl, BYTE PTR [bx+2h]
        mov      ch, BYTE PTR [bx+5h]
        mov      bp, WORD PTR [bx]
        mov      bx, WORD PTR [bx+3h]

        _check_if_equal__:
            mov     ah, BYTE PTR ss:[bx]
            cmp     BYTE PTR [bp], ah 
            jne     _no_equ__ 

            test    ah, ah  
            jz      _equ_cmp__  

            push    cx 
            xor     ch, ch 
            add     bp, cx 
            pop     cx 
            push    cx
            mov     cl, 8h 
            shr     cx, cl 
            xor     ch, ch  
            add     bx, cx 
            pop     cx  
            jmp     _check_if_equal__

        _no_equ__:
            xor    dx, dx 
            jmp    _check_cond_equ__

        _equ_cmp__:
            mov    dx, 1h 

    _check_cond_equ__:
        cmp     al, EQU_CON 
        je      _check_equal__ 

        test    dx, dx 
        jz      _cond_sat__ 
        jmp     _cond_no_sat__ 

        _check_equal__:
            test    dx, dx 
            jz     _cond_no_sat__
            jmp    _cond_sat__ 
        
    _cond_sat_equ__:
        cmp      al, EQU_CON
        jne      _cond_no_sat__

    _cond_sat__:
        pop     bp 
        mov     ax, 1h  
        jmp     _end_cmp__

    _cond_no_sat__:
        pop     bp 
        xor     ax, ax 

    _end_cmp__:
        mov    bx, OFFSET op2_dim
        mov    WORD PTR [bx], 0h 
        mov    WORD PTR [bx+2h], 0h 
        mov    WORD PTR [bx+4h], 0h
        pop    si 
        pop    dx 
        pop    cx 
        pop    bx 
        pop    bp 
        iret 

_illegal_math_op:
    mov     ax, BASIC_SEG
    mov     ds, ax
    mov     bx, OFFSET zero_division
    mov     BYTE PTR [bx], 1h
    call    _print_err_msg
    int     24h 

_abs_subroutine:
    cmp     ax, MAX_16BIT_VALUE
    jbe     _no_abs

    neg     ax 

    _no_abs:
        iret 

_leave_subroutine:
    mov     sp, bp 
    retf 
 
_text ENDS 