       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDX2RECH.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
	      DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   
		SELECT  RECH-IDX	ASSIGN TO "..\RECHIDX.DAT" 
				ORGANIZATION IS INDEXED
				ACCESS MODE IS SEQUENTIAL
				RECORD KEY IS RECH-IDX-CLAVE
				FILE STATUS IS FS-RECH-IDX.

		SELECT RECH ASSIGN TO "..\RECHIDX.TXT"
				ORGANIZATION IS LINE SEQUENTIAL
				FILE STATUS IS FS-RECH.
	   DATA DIVISION.
       FILE SECTION.

       FD RECH-IDX.
	   01 RECH-IDX-REG.
	       03  RECH-IDX-CLAVE.
              05  RECH-IDX-PATENTE                      PIC X(06).
              05  RECH-IDX-FECHA.  
			      07  RECH-IDX-FECHA-AA                 PIC 9(04).
				  07  RECH-IDX-FECHA-MM                 PIC 9(02).
				  07  RECH-IDX-FECHA-DD                 PIC 9(02).
		   03  RECH-IDX-TIPO-DOC                        PIC X.
		   03  RECH-IDX-NRO-DOC                         PIC X(20).
		   03  RECH-IDX-IMPORTE                         PIC 9(4)V99.	  
		
       FD RECH.
	   01 RECH-REG.
	       03  RECH-CLAVE.
              05  RECH-PATENTE                      PIC X(06).
              05  RECH-FECHA.  
			      07  RECH-FECHA-AA                 PIC 9(04).
				  07  RECH-FECHA-MM                 PIC 9(02).
				  07  RECH-FECHA-DD                 PIC 9(02).
		   03  RECH-TIPO-DOC                        PIC X.
		   03  RECH-NRO-DOC                         PIC X(20).
		   03  RECH-IMPORTE                         PIC 9(4)V99.
		
       WORKING-STORAGE SECTION.

       01  FS-RECH								PIC X(02).
           88  FS-RECH-OK						VALUE '00'.
           88  FS-RECH-FIN						VALUE '10'.
		
	   01  FS-RECH-IDX							PIC X(02).
           88  FS-RECH-IDX-OK           			VALUE '00'.
           88  FS-RECH-IDX-FIN          			VALUE '10'.
		   88  FS-RECH-IDX-NO-EXIST				VALUE '23'.	

	   01  WS-FILE-STATUS.
           05  WS-FS                    		PIC X(02).
           05  WS-FS-NOMBRE             		PIC X(08).
           05  WS-FS-FUNCION            		PIC X(05).	   
		
       PROCEDURE DIVISION.

       PGM.
		DISPLAY 'INICIO IDX2RECH'.
		
		PERFORM 100-INICIO.		
		PERFORM 300-LEER-RECH-IDX.		
		PERFORM 200-PROCESO UNTIL FS-RECH-IDX-FIN.

		DISPLAY 'FIN IDX2RECH'.
		
		STOP RUN.
		
	   100-INICIO.
	   		OPEN INPUT RECH-IDX.
			MOVE FS-RECH-IDX  	TO  WS-FS.
			MOVE "RECH-IDX"      TO  WS-FS-NOMBRE.
			MOVE "ABRIR"        TO  WS-FS-FUNCION.
			PERFORM 900-CHECK-FILE-STATUS.

			OPEN OUTPUT RECH.
			MOVE FS-RECH			TO  WS-FS.
			MOVE "RECH"		   	TO  WS-FS-NOMBRE.
			MOVE "ABRIR"      	TO  WS-FS-FUNCION.
			PERFORM 900-CHECK-FILE-STATUS.
	   
	   200-PROCESO.
			MOVE RECH-IDX-REG TO RECH-REG.
			WRITE RECH-REG.
			
			IF NOT FS-RECH-OK
				DISPLAY 'ERROR AL GUARDAR EL REGISTRO'
				GO 999-CANCELAR-PROGRAMA
			END-IF.
			
			PERFORM 300-LEER-RECH-IDX.
			
       300-LEER-RECH-IDX.
			READ RECH-IDX AT END SET FS-RECH-IDX-FIN  TO TRUE.

			IF NOT FS-RECH-IDX-OK AND NOT FS-RECH-IDX-FIN THEN
				DISPLAY "FS: " FS-RECH-IDX
				DISPLAY 'ERROR AL INTETAR LEER RECH-IDX'
				GO 999-CANCELAR-PROGRAMA
			END-IF.
		
		
       900-CHECK-FILE-STATUS.
           IF WS-FS NOT EQUAL "00"
              DISPLAY "CANCELACION POR ERROR"
              DISPLAY "EN ARCHIVO: " WS-FS-NOMBRE
              DISPLAY "FILSTATUS: " WS-FS
              DISPLAY "AL INTENTAR: " WS-FS-FUNCION
              GO 999-CANCELAR-PROGRAMA
           END-IF.
		   
       998-FIN.
			CLOSE RECH.
			CLOSE RECH-IDX.
		   
       999-CANCELAR-PROGRAMA.
		   PERFORM 998-FIN.
		   DISPLAY "SALIDA POR CANCELACION DE PROGRAMA".