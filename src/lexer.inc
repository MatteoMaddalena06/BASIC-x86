;------------------------------------------------------------------------
; Il seguente modulo definisce le principali macro utilizzate dal lexer
; Ad ogni singola keyword è assegnato un valore.
; Il nible più significativo è il token, il meno significativo 
; identifica la keyword
;
; Es => INP = 8Ch  (8h) = instruction token (0Ch) = instruction's ID
;
; È anche dichiarato un buffer contente i simboli dell'alfabeto definito
; dall'interprete. Il lexer effettuerà il confronto tra questo buffer
; e il buffer contenente l'istruzione per la ricerca di un eventuale 
; corrispondenza, in caso di successo genera il realtivo token, 
; altrimenti controllerà la corettezza sintattica della stringa 
; prendendola in considerazione come se fosse una stringa("bla bla"), 
; un identificatore(variabile) o un valore numerico con segno.
;------------------------------------------------------------------------

;--------------------------------------------------------------
; Lexer's Macro

    INSTRUCTION_TOKEN     EQU     8h  ;(token ID)
    OPERATOR_TOKEN        EQU     9h  ;(token ID)
    SEPARATOR_TOKEN       EQU     0Ah ;(token ID)
    STRING_TOKEN          EQU     0Bh ;(token ID)
    VALUE_TOKEN           EQU     0Ch ;(token ID)
    IDENTIFIER_TOKEN      EQU     0Dh ;(token ID)

    DECL_ID       EQU      80h ;(instruction)
    IF_ID         EQU      82h ;(instruction)
    FOR_ID        EQU      83h ;(instruction)
    WHILE_ID      EQU      84h ;(instruction)
    GOTO_ID       EQU      85h ;(instruction)
    PRINT_ID      EQU      88h ;(instruction)
    INPUT_ID      EQU      89h ;(instruction)
    WRITE_ID      EQU      8Ah ;(instruction) 
    READ_ID       EQU      8Bh ;(instruction) 
    INP_ID        EQU      8Ch ;(instruction) 
    OUTP_ID       EQU      8Dh ;(instruction) 
    ABS_ID        EQU      8Eh ;(instruction) 
    MACHC_ID      EQU      8Fh ;(instruction)

    THEN_ID       EQU      0A0h ;(separator)
    TO_ID         EQU      0A1h ;(separator)
    NEXT_ID       EQU      0A2h ;(separator)
    WEND_ID       EQU      0A3h ;(separator)
    STEP_ID       EQU      0A4h ;(separator)
    BRCKL_ID      EQU      0A5h ;(separator)
    BRCKR_ID      EQU      0A6h ;(separator)
    DQTS_ID       EQU      0A7h ;(separator)
    SQTS_ID       EQU      0A8h ;(separator)
    DBP_ID        EQU      0A9h ;(separator)
    COMMA_ID      EQU      0AAh ;(separator)
    ANGBRCKL_ID   EQU      0ABh ;(separator)
    ANGBRCKR_ID   EQU      0ACh ;(separator)

    ASS_ID        EQU      90h ;(operator)
    ADD_ID        EQU      91h ;(operator)
    SUB_ID        EQU      92h ;(operator)
    MUL_ID        EQU      9Ch ;(operator)
    DIV_ID        EQU      9Dh ;(operator)
    MOD_ID        EQU      9Eh ;(operator)
    OFF_ID        EQU      96h ;(operator)
    EQU_ID        EQU      97h ;(operator)
    BOR_ID        EQU      98h ;(operator)
    GRT_ID        EQU      99h ;(operator)
    BORE_ID       EQU      9Ah ;(operator)
    GRTE_ID       EQU      9Bh ;(operator) 
    AND_ID        EQU      93h ;(operator)
    OR_ID         EQU      94h ;(operator)
    NOT_ID        EQU      95h ;(operator)
    NEQU_ID       EQU      9Fh ;(operator)

    IGNORE_TOKEN    EQU    0ADh ;(special)
    RUN_ID          EQU    0AEh ;(cmd line token)
    SHVT_ID         EQU    0AFh ;(cmd line token)
    TIME_ID         EQU    0E1h ;(cmd line token)
    RESET_ID        EQU    0E2h ;(cmd line token)
    CLEAR_ID        EQU    0E3h ;(cmd line token)
    HELP_ID         EQU    0E4h ;(cmd line token)
;-------------------------------------------------------------

_text SEGMENT PARA PUBLIC 

    farjmp    BASIC_SEG, _start

    alphabet_buffer     db       "RUN", RUN_ID 
                        db       "SHVT", SHVT_ID 
                        db       "TIME", TIME_ID 
                        db       "RESET", RESET_ID 
                        db       "CLS", CLEAR_ID 
                        db       "HELP", HELP_ID  
                        db       "DECL", DECL_ID
                        db       "IF", IF_ID
                        db       "FOR", FOR_ID
                        db       "WHILE", WHILE_ID
                        db       "GOTO", GOTO_ID
                        db       "PRINT", PRINT_ID
                        db       "INPUT", INPUT_ID
                        db       "WRITE", WRITE_ID
                        db       "READ", READ_ID
                        db       "INP", INP_ID
                        db       "OUTP", OUTP_ID
                        db       "ABS", ABS_ID
                        db       "THEN", THEN_ID
                        db       "TO", TO_ID 
                        db       "NEXT", NEXT_ID 
                        db       "WEND", WEND_ID 
                        db       "STEP", STEP_ID 
                        db       "MACHC", MACHC_ID
                        db       "(", BRCKL_ID
                        db       ")", BRCKR_ID 
                        db       22h, DQTS_ID
                        db       27h, SQTS_ID                             
                        db       ":", DBP_ID
                        db       ",", COMMA_ID
                        db       "[", ANGBRCKL_ID
                        db       "]", ANGBRCKR_ID
                        db       "=", ASS_ID
                        db       "<", BOR_ID
                        db       ">", GRT_ID
                        db       "+", ADD_ID
                        db       "-", SUB_ID
                        db       "%", MOD_ID
                        db       "/", DIV_ID
                        db       "*", MUL_ID
                        db       "!", NOT_ID
                        db       "|", OR_ID 
                        db       "&", AND_ID
                        db       "$", OFF_ID
_text ENDS 