       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALQ2IDX.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
	      DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   
		SELECT  ALQ-IDX	ASSIGN TO "..\ALQIDX.DAT" 
				ORGANIZATION IS INDEXED
				ACCESS MODE IS RANDOM
				RECORD KEY IS ALQ-IDX-CLAVE
				FILE STATUS IS FS-ALQ-IDX.

		SELECT ALQ ASSIGN TO "..\ALQ.TXT"
				ORGANIZATION IS LINE SEQUENTIAL
				FILE STATUS IS FS-ALQ.
       
	   DATA DIVISION.
       FILE SECTION.

       FD  ALQ-IDX. 
       01  ALQ-IDX-REG.
		   03 ALQ-IDX-CLAVE.
			   05 ALQ-IDX-PATENTE				PIC X(06).
			   05 ALQ-IDX-FECHA.
			   	  07  ALQ-IDX-FECHA-AA     		PIC 9(04).
				  07  ALQ-IDX-FECHA-MM     		PIC 9(02).
				  07  ALQ-IDX-FECHA-DD     		PIC 9(02).
		   03  ALQ-IDX-TIPO-DOC            		PIC X.
		   03  ALQ-IDX-NRO-DOC             		PIC X(20).
		   03  ALQ-IDX-IMPORTE            		PIC 9(04)V99.
		   03  ALQ-IDX-CHOFER			   		PIC X(07).	
		   03  ALQ-IDX-ESTADO                 	PIC X.				  
		
       FD  ALQ.
	   01  ALQ-REG.
           03 ALQ-CLAVE.
			   05 ALQ-PATENTE					PIC X(06).
			   05 ALQ-FECHA.
			   	  07  ALQ-FECHA-AA  	   		PIC 9(04).
				  07  ALQ-FECHA-MM     			PIC 9(02).
				  07  ALQ-FECHA-DD     			PIC 9(02).
		   03  ALQ-TIPO-DOC                     PIC X.
		   03  ALQ-NRO-DOC                      PIC X(20).
		   03  ALQ-IMPORTE                      PIC 9(04)V99.
		   03  ALQ-CHOFER						PIC X(07).	
		   03  ALQ-ESTADO                       PIC X.
       
       WORKING-STORAGE SECTION.

       01  FS-ALQ								PIC X(02).
           88  FS-ALQ-OK						VALUE '00'.
           88  FS-ALQ-FIN						VALUE '10'.
		
	   01  FS-ALQ-IDX							PIC X(02).
           88  FS-ALQ-IDX-OK           			VALUE '00'.
           88  FS-ALQ-IDX-FIN          			VALUE '10'.
		   88  FS-ALQ-IDX-NO-EXIST				VALUE '23'.	

	   01  WS-FILE-STATUS.
           05  WS-FS                    		PIC X(02).
           05  WS-FS-NOMBRE             		PIC X(08).
           05  WS-FS-FUNCION            		PIC X(05).
		   
	   77 NVOREGISTRO							PIC X VALUE 'N'.	   
		

       PROCEDURE DIVISION.

       PGM.
		
		PERFORM 100-INICIO.
		
		PERFORM 300-LEER-ALQ.
		
		PERFORM 200-PROCESO UNTIL FS-ALQ-FIN.
		
		STOP RUN.
       
	   100-INICIO.
	   		OPEN OUTPUT ALQ-IDX.
			MOVE FS-ALQ-IDX  	TO  WS-FS.
			MOVE "ALQ-IDX"      TO  WS-FS-NOMBRE.
			MOVE "ABRIR"        TO  WS-FS-FUNCION.
			PERFORM 900-CHECK-FILE-STATUS.
			DISPLAY 'ALQ IDX ABIERTO'.		

			OPEN INPUT ALQ.
			MOVE FS-ALQ			TO  WS-FS.
			MOVE "ALQ"		   	TO  WS-FS-NOMBRE.
			MOVE "ABRIR"      	TO  WS-FS-FUNCION.
			PERFORM 900-CHECK-FILE-STATUS.
			DISPLAY 'ALQ ABIERTO'.		
			
       200-PROCESO.
			DISPLAY "PROCESO.".
			MOVE ALQ-REG TO ALQ-IDX-REG.
			
			DISPLAY ALQ-IDX-REG.		
			WRITE ALQ-IDX-REG.
			
			IF NOT FS-ALQ-IDX-OK
				DISPLAY 'ERROR AL GUARDAR EL REGISTRO'
				GO 999-CANCELAR-PROGRAMA
			END-IF.
			
			PERFORM 300-LEER-ALQ.
			

	   300-LEER-ALQ.
	   		
			DISPLAY 'LEER-ALQ'.
			READ ALQ AT END SET FS-ALQ-FIN  TO TRUE.
			
			IF NOT FS-ALQ-OK AND NOT FS-ALQ-FIN
				DISPLAY "FILE STATUS: " FS-ALQ
				DISPLAY 'ERROR AL INTENTAR LEER ALQ'
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
			CLOSE ALQ.
			CLOSE ALQ-IDX.
		   
       999-CANCELAR-PROGRAMA.
		   PERFORM 998-FIN.
		   DISPLAY "SALIDA POR CANCELACION DE PROGRAMA".
