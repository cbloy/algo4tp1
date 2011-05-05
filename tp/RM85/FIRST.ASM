       PAGE    58,132
;****************************************************************
;*		    FIRST.ASM					*
;*								*
;*   Program written by: Liant Software Corp.	Date: 86.09.04	*
;*   Description:   Sample program to illustrate machine	*
;*		  language library construction for		*
;*		  RM/COBOL-85.					*
;*								*
;****************************************************************
;
CALLING_PARAMETERS     STRUC
PARAMETER_COUNT        DW     1 DUP (?)
STRING_VALUE_ADDRESS   DD     1 DUP (?)
STRING_VALUE_SIZE      DW     2 DUP (?)
STRING_VALUE_RESERVE   DB     8 DUP (?)  ; Type, digit count, scale
;					 ;  and picture address are
;					 ;  unused in this subprogram
STRING_LENGTH_ADDRESS  DD     1 DUP (?)
STRING_LENGTH_SIZE     DW     2 DUP (?)
STRING_LENGTH_RESERVE  DB     8 DUP (?)  ; Type, digit count, scale
;					 ;  and picture address are
;					 ;  unused in this subprogram
CALLING_PARAMETERS     ENDS
;
;
LIB_DIRECTORY  SEGMENT	byte public  ; Build directory entry for
L1L    DB  L1A - L1L - 1	     ; FIRST-STRING
       DB  'FIRST-STRING'
L1A    DD  FIRST_STRING
LIB_DIRECTORY	       ENDS
;
CODE_SEGMENT   SEGMENT	byte public
      ASSUME CS:CODE_SEGMENT
;
;
;      Local data area
;
STRING1    DB  'First String'
LNGTH	   EQU $ - STRING1
;
;
;
FIRST_STRING	PROC far   ; Allows for far return
       NOP		   ; Allow room for debug INT 3
;
       CALL    VALIDATE_AND_CONVERT_PARAMETERS
       JC      FAILURE
       MOV     CX, LNGTH
       CMP     ES:[BX].STRING_VALUE_SIZE, CX
       JB      FAILURE	   ; If string-value too short for string
;
       PUSH    CS
       POP     DS
       MOV     SI, offset STRING1
       CALL    MOVE_STRING
;
       XOR     AX,AX	   ; Clear AX for return code
       RET
;
FAILURE:
       MOV     AX, 2	   ; Return non-zero error code
       RET
;
FIRST_STRING	   ENDP
;
;
       PUBLIC  MOVE_STRING
MOVE_STRING    PROC near
       PUSH    DS
       PUSH    SI
       LDS     SI, ES:[BX].STRING_LENGTH_ADDRESS
       MOV     [SI], CH    ; BINARY values are stored high-order byte
       MOV     [SI+1], CL  ; in the lower (lowest) addresses position
       POP     SI
       POP     DS
;
       MOV     DX, ES:[BX].STRING_VALUE_SIZE
       SUB     DX, CX	   ; DX contains the count of spaces to fill
       LES     DI, ES:[BX].STRING_VALUE_ADDRESS
       REP     MOVSB
       MOV     CX, DX
       MOV     AL, ' '
       REP     STOSB
       RET
MOVE_STRING    ENDP
;
;
       PUBLIC  VALIDATE_AND_CONVERT_PARAMETERS
VALIDATE_AND_CONVERT_PARAMETERS  PROC near
       CMP     CX, 2	   ; Check parameter count
       JNE     FAIL_VAL    ; If argument count is not 2
       CMP     ES:[BX].STRING_VALUE_SIZE + 2, 0
       JNE     FAIL_VAL    ; If string-value too long
       CMP     ES:[BX].STRING_VALUE_SIZE, 65280
       JA      FAIL_VAL    ; If string-value too long
       CMP     ES:[BX].STRING_LENGTH_SIZE + 2, 0
       JNE     FAIL_VAL    ; If not 2-byte BINARY
       CMP     ES:[BX].STRING_LENGTH_SIZE, 2
       JNE     FAIL_VAL    ; If not 2-byte BINARY
       LEA     DI, [BX].STRING_VALUE_ADDRESS
       CALL    CONVERT_ARGUMENT_ADDRESS
       LEA     DI, [BX].STRING_LENGTH_ADDRESS
       CALL    CONVERT_ARGUMENT_ADDRESS
       CLC
       RET
;
FAIL_VAL:
       STC
       RET
VALIDATE_AND_CONVERT_PARAMETERS  ENDP
;
;
CONVERT_ARGUMENT_ADDRESS	       PROC near
       MOV     DX, ES:[DI + 2]
       MOV     AX, ES:[DI]
       MOV     DH, DL
       MOV     DL, AH
       SUB     AH, AH
       SHL     DX, 1
       SHL     DX, 1
       SHL     DX, 1
       SHL     DX, 1
       STOSW
       XCHG    AX, DX
       STOSW
       RET
CONVERT_ARGUMENT_ADDRESS	       ENDP
;
CODE_SEGMENT   ENDS
;
;
       END
