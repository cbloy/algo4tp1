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
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AUTOS
               ASSIGN TO "..\AUTOS.TXT" 
		       ORGANIZATION IS LINE SEQUENTIAL
      	       ACCESS MODE IS SEQUENTIAL
		       FILE STATUS IS FS-AUTOS.
		   
	   
	   DATA DIVISION.
	   
       FILE SECTION.
       FD AUTOS.
	   01  AUT-REG.
	       05  AUT-PATENTE        PIC X(6).
		   05  AUT-DESC           PIC X(30).
		   05  AUT-MARCA          PIC X(20).
		   05  AUT-COLOR          PIC X(10).
		   05  AUT-TAMANIO        PIC X.
		   05  AUT-IMPORTE        PIC 9(4)V99.
	   
	   WORKING-STORAGE SECTION.
	   
      *****************
      *  FILE STATUS  *	  
      *****************
	   01  FS-AUTOS                     PIC X(02).
	       88  FS-AUTOS-OK              VALUE '00'.
           88  FS-AUTOS-FIN             VALUE '10'.
 
      * PARA CHEQUEO DE FILE STATUS
       01  FILE-STATUS.
          05  FS                       PIC X(02).
          05  FS-NOMBRE                PIC X(08).
          05  FS-FUNCION               PIC X(05).	  
		   
      ************		   
      *  CLAVES  *
      ************
	   01  CLAVE-ANTERIOR.
	       05  PATENTE                  PIC X(6).
		   05  FECHA                    PIC 9(8).
	   
       01  CLAVE-ACTUAL.
	       05  PATENTE                  PIC X(6).
		   05  FECHA                    PIC 9(8).	   
		   
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
	   PGM.		
           DISPLAY "INICIA EL PROGRAMA".
           PERFORM 1000-INICIO.
		   PERFORM 8400-LEER-AUTOS.
		   DISPLAY AUT-REG
		   PERFORM 8400-LEER-AUTOS.
		   DISPLAY AUT-REG
		   PERFORM 8400-LEER-AUTOS.
		   DISPLAY AUT-REG
           DISPLAY "FINALIZA EL PROGRAMA". 
		   STOP RUN.
		   
       1000-INICIO.
           PERFORM 1100-ABRIR-ARCHIVOS.
	  
       1100-ABRIR-ARCHIVOS.
		   OPEN INPUT  AUTOS.
           MOVE FS-AUTOS      TO FS.
           MOVE "AUTOS   "    TO FS-NOMBRE.
           MOVE "ABRIR"       TO FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.
       
	   8400-LEER-AUTOS.
           READ AUTOS AT END SET FS-AUTOS-FIN TO TRUE.
		   
           IF NOT FS-AUTOS-OK AND NOT FS-AUTOS-FIN
               DISPLAY 'ERROR AL INTENTAR LEER AUTOS'
               PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.
       8900-CHECK-FILE-STATUS.
           IF FS NOT EQUAL "00"
              DISPLAY "CANCELACION POR ERROR"
              DISPLAY "EN ARCHIVO: " FS-NOMBRE
              DISPLAY "FILESTATUS: " FS
              DISPLAY "AL INTENTAR: " FS-FUNCION
              PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.	
		   
       9000-FINAL.
           CLOSE AUTOS.
           
       9999-CANCELAR-PROGRAMA.
           PERFORM 9000-FINAL.
           DISPLAY "SALIDA POR CANCELACION DE PROGRAMA".
		   STOP RUN.		   