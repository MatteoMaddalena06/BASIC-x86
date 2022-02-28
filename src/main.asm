BASIC_SEG    EQU    70h 

farjmp MACRO segm, off 

    db  0EAh 
    dw  off 
    dw  segm 

ENDM

farcall MACRO segm, off 

    db   9Ah 
    dw   off
    dw   segm    

ENDM 

INCLUDE lexer.asm
INCLUDE parser.asm
INCLUDE code_gen.asm 
INCLUDE graphics.asm
INCLUDE utils.asm
INCLUDE subroutine.asm

_text SEGMENT PARA PUBLIC 

    input_buffer       db    48h DUP(0h)
    error_occured      db    0h 
    saved_sp           dw    0h 
    saved_ss           dw    0h 

_start:
    mov    ax, BASIC_SEG        
    mov    ds, ax 
    mov    ss, ax 
    mov    sp, 0FFFFh

    mov    ax, 0B800h 
    mov    es, ax

    _set_time_format
    call   _draw_shell

    _next__:
        call   _mount_subroutine 
        mov     bx, OFFSET input_buffer
    
    _input: 
       call   _take_input
       jc     _input

    call     _compilation_work

    mov      bx, OFFSET mode_interpreter
    cmp      BYTE PTR [bx], SINGLE_LINE_MODE 
    jne      _multi_line 

    cmp      ax, 1h 
    je       _error_occured
    
    cmp      ax, 2h 
    je       _next__

    mov     bx, OFFSET token_buffer
    cmp     BYTE PTR [bx], 0E0h 
    je      _next__ 

    call     _execute_program
    jmp      _next__ 

    _error_occured:
        call    _print_err_msg
        call    _clear_flags
        jmp     _next__ 

    _multi_line:
        cmp     ax, 1h 
        jne     _next__

        call    _clear_flags
        mov     bx, OFFSET error_occured
        mov     BYTE PTR [bx], 1h
        jmp     _next__ 

_text ENDS 
    END _start