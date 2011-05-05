       PAGE    58,132
;****************************************************************
;*                  LIBEND.ASM                                  *
;*                                                              *
;*   Program written by: Liant Software Corp.	Date: 86.09.04	*
;*   Description: Sample program to illustrate machine          *
;*                language library construction for             *
;*                RM/COBOL-85.                                  *
;*                                                              *
;****************************************************************
;
LIB_DIRECTORY  SEGMENT  byte public  ; Build machine language library
       DB  0                         ; terminator
LIB_DIRECTORY          ENDS
;
       END
