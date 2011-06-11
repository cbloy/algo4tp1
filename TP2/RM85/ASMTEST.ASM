       PAGE    ,132
;****************************************************************
;*		    ASMTEST.ASM 				*
;*								*
;*   Program written by: Liant Software Corp.	Date: 86.09.04	*
;*   Description: Sample program to illustrate machine		*
;*		  language linkage to RM/COBOL-85. The runtime	*
;*		  would be configured to automatically load	*
;*		  this module at start-up. Initial will copy	*
;*		  the passed command line arguments to a	*
;*		  local data space, and its work is finished.	*
;*		  The RM/COBOL-85 program, LNKTEST, will later	*
;*		  call ASMTEST and have the saved command line	*
;*		  arguments returned to the program LNKTEST.	*
;*								*
;*   Parameters passed to ASMTEST:				*
;*		  Register BX points to  an argument list	*
;*		  that has the following structure:		*
;*		  ES:[BX] -> DW Count of arguments		*
;*			     DD Linear address of STRING-VALUE	*
;*			     DD Length of STRING-VALUE		*
;*			     DB Data type of STRING-VALUE	*
;*			     DB Reserved			*
;*			     DB Digit count of STRING-VALUE	*
;*			     DB Scale of STRING-VALUE		*
;*			     DD Picture address of STRING-VALUE *
;*			     DD Linear address of STRING-LENGTH *
;*			     DD Length of STRING-LENGTH 	*
;*			     DB Data type of STRING-LENGTH	*
;*			     DB Reserved			*
;*			     DB Digit count of STRING-LENGTH	*
;*			     DB Scale of STRING-LENGTH		*
;*			     DD Picture address of STRING-LENGTH*
;*		  These two variables are in the calling	*
;*		  RM/COBOL-85 program. The linear addresses	*
;*		  must be converted to segment:offset form.	*
;*								*
;*   ASMTEST  will move the string from its local space here	*
;*	      to the RM/COBOL-85 variable STRING-VALUE, and the *
;*	      length of this string to STRING-LENGTH. A 	*
;*	      return value is placed in register AL. Zero	*
;*	      implies all is well, non-zero denotes error.	*
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
CGROUP GROUP CODE
CODE SEGMENT BYTE PUBLIC 'CODE'          ; This program has only one segment.
      ASSUME CS:CGROUP
;
;      Local data area
;
STRING	   DB  'This is the string'
LNGTH	   EQU $ - STRING
;
;
;      Code segment
;
ASMTEST    PROC    FAR	   ; Allows for far return
;			   ; Registers ES:BX point to the argument count
       NOP		   ; Allow room for debug INT 3
       CMP     CX, 2
       JNE     FAILURE	   ; If argument count is not 2
       CMP     ES:[BX].STRING_VALUE_SIZE + 2, 0
       JNE     FAILURE	   ; If string-value too long for asmtst
       CMP     ES:[BX].STRING_VALUE_SIZE, 65280
       JA      FAILURE	   ; If string-value too long for asmtst
       CMP     ES:[BX].STRING_VALUE_SIZE, LNGTH
       JB      FAILURE	   ; If string-value too short for string
       CMP     ES:[BX].STRING_LENGTH_SIZE + 2, 0
       JNE     FAILURE	   ; If not 2-byte BINARY
       CMP     ES:[BX].STRING_LENGTH_SIZE, 2
       JNE     FAILURE	   ; If not 2-byte BINARY
       LEA     DI, [BX].STRING_VALUE_ADDRESS
       CALL    CONVERT_ARGUMENT_ADDRESS
       LEA     DI, [BX].STRING_LENGTH_ADDRESS
       CALL    CONVERT_ARGUMENT_ADDRESS
;
       LDS     SI, ES:[BX].STRING_LENGTH_ADDRESS
       MOV     CX, LNGTH
       MOV     [SI], CH    ; BINARY values are stored high-order byte
       MOV     [SI+1], CL  ; in the lower (lowest) addresses position
;
       PUSH    CS
       POP     DS
       MOV     SI, offset STRING
       MOV     DX, ES:[BX].STRING_VALUE_SIZE
       SUB     DX, CX	   ; DX contains the count of spaces to fill
       LES     DI, ES:[BX].STRING_VALUE_ADDRESS
       REP     MOVSB
       MOV     CX, DX
       MOV     AL, ' '
       REP     STOSB
;
       XOR     AX,AX	   ; Clear AX for return code
       RET
;
FAILURE:
       MOV     AX, 2	   ; Return non-zero error code
       RET
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
ASMTEST  ENDP
;
CODE   ENDS
       END     ASMTEST
