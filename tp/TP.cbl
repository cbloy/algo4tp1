       IDENTIFICATION DIVISION.
	   PROGRAM-ID. "TRABAJO PRACTICO NRO. 1".
      * 
       ENVIRONMENT DIVISION.
	   CONFIGURATION SECTION.
       SOURCE-COMPUTER. RM-COBOL-85.
       OBJECT-COMPUTER. RM-COBOL-85.
       SPECIAL-NAMES.
                 DECIMAL-POINT IS COMMA.
      *      	  
      * INPUT-OUTPUT SECTION.
      * FILE-CONTROL.
	   
	   DATA DIVISION.
      * FILE SECTION.
	   
	   WORKING-STORAGE SECTION.
      ************		   
      *  CLAVES  *
      ************
	   01  CLAVE-ANTERIOR.
	       05  PATENTE            PIC X(6).
		   05  FECHA              PIC 9(8).
	   
       01  CLAVE-ACTUAL.
	       05  PATENTE            PIC X(6).
		   05  FECHA              PIC 9(8).	   
		   
      **************		   
      *  LISTADOS  *
      **************
       01  FECHA.
   		   03  FECHA-AA   				PIC 9(02).
		   03  FECHA-MM					PIC 9(02).
		   03  FECHA-DD					PIC 9(02).
		
	   01  ENCABEZADO-HOJA.
		   03 FILLER					PIC X(06)
										VALUE 'Fecha '.
		   03 ENC-FECHA-DD				PIC 99.
		   03 FILLER					PIC X 
										VALUE '/'.
		   03 ENC-FECHA-MM				PIC 99.
		   03 FILLER					PIC X 
										VALUE '/'.
		   03 FILLER 					PIC X(02) 
										VALUE '20'.
	       03 ENC-FECHA-AA				PIC 99.
		   03 FILLER					PIC X(57).
		   03 FILLER					PIC X(5) 
										VALUE 'Hoja '.
		   03 ENC-N-HOJA				PIC 99.		   
		
       PROCEDURE DIVISION.