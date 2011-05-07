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

      ************		   
      *  TABLAS  *
      ************
	   01  TABLA-AUTOS.
	       05  TABLA-AUT                OCCURS 300 TIMES.
		       09  TABLA-AUTOS-REG.
			       11  AUTO-PATENTE     PIC X(6).
				   11  AUTO-DESC        PIC X(30).
				   11  AUTO-MARCA       PIC X(20).
				   11  AUTO-COLOR       PIC X(10).
				   11  AUTO-TAMANIO     PIC X.
				   11  AUTO-IMPORTE     PIC 9(4)V99.
				   
       01  TABLA-ESTAD.
	       05  ESTAD-MARCAS             OCCURS 100 TIMES.
		       09  ESTAD-MARCA          PIC X(10).
			   09  ESTAD-MESES          OCCURS 12 TIMES.
			       11  ESTAD-MES        PIC 9(3).
				   
       PROCEDURE DIVISION.