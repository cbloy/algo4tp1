       PAGE    58,132
;****************************************************************
;*		    SECOND.ASM					*
;*								*
;*   Program written by: Liant Software Corp.	Date: 86.09.04	*
;*   Description: Sample program to illustrate machine		*
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
       EXTRN   MOVE_STRING:near
       EXTRN   VALIDATE_AND_CONVERT_PARAMETERS:near
;
;
LIB_DIRECTORY SEGMENT byte public    ; Build directory entry for
L2L    DB  L2A - L2L - 1	     ; SECOND-STRING
       DB  'SECOND-STRING'
L2A    DD  SECOND_STRING
LIB_DIRECTORY	       ENDS
;
CODE_SEGMENT  SEGMENT byte public
      ASSUME CS:CODE_SEGMENT
;
; Local data
STRING1    DB  'Second string-initial'
LNGTH1	   EQU $ - STRING1
STRING2    DB  'Second string-last used'
LNGTH2	   EQU $ - STRING2
;
SECOND_STRING  PROC far    ; Register CX contains the argument count
       NOP		   ; Allow room for debug INT 3
;
       PUSH    AX
       CALL    VALIDATE_AND_CONVERT_PARAMETERS
       POP     AX
       JC      FAILURE
       MOV     CX, LNGTH1  ; Set parameters for initial entry
       MOV     SI, offset STRING1
       TEST    AX, AX
       JZ      SS1	   ; If initial entry
       MOV     CX, LNGTH2  ; Set parameters for last used entry
       MOV     SI, offset STRING2
SS1:   CMP     ES:[BX].STRING_VALUE_SIZE, CX
       JB      FAILURE	   ; If string-value too short for string
;
       PUSH    CS
       POP     DS
       CALL    MOVE_STRING
;
       XOR     AX,AX	   ; Clear AX for return code
       RET
;
FAILURE:
       MOV     AL, 2
       RET
;
SECOND_STRING	    ENDP
CODE_SEGMENT	   ENDS
;
       END
