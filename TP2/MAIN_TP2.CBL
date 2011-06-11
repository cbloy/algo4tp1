       IDENTIFICATION DIVISION.
	   PROGRAM-ID. "TRABAJO PRACTICO NRO. 2".
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
           SELECT ALQ-IDX
               ASSIGN TO ".\ALQIDX.DAT" 
		       ORGANIZATION IS INDEXED
      	       ACCESS MODE IS SEQUENTIAL
			   RECORD KEY IS ALQ-IDX-CLAVE
		       FILE STATUS IS FS-ALQ-IDX.

		   SELECT  CHO-IDX	ASSIGN TO ".\CHOIDX.DAT" 
				ORGANIZATION IS INDEXED
				ACCESS MODE IS DYNAMIC
				RECORD KEY IS CHO-IDX-CLAVE
				FILE STATUS IS FS-CHO-IDX.
				
		   	   
		   SELECT  CLI-IDX	ASSIGN TO ".\CLIIDX.DAT" 
				ORGANIZATION IS INDEXED
				ACCESS MODE IS RANDOM
				RECORD KEY IS CLI-IDX-CLAVE
				ALTERNATE RECORD KEY IS CLI-IDX-CLAVE-ALT
				FILE STATUS IS FS-CLI-IDX.
			
		   SELECT  RECH-IDX	ASSIGN TO ".\RECHIDX.DAT" 
				ORGANIZATION IS INDEXED
				ACCESS MODE IS RANDOM
				RECORD KEY IS RECH-IDX-CLAVE
				FILE STATUS IS FS-RECH-IDX.

			
		   SELECT APRO ASSIGN TO DISK.
			   
	   DATA DIVISION.
	   
       FILE SECTION.
	   FD ALQ-IDX.
	   01  ALQ-IDX-REG.
	       03  ALQ-IDX-CLAVE.
              05  ALQ-IDX-PATENTE                      PIC X(06).
              05  ALQ-IDX-FECHA.  
			      07  ALQ-IDX-FECHA-AA                 PIC 9(04).
				  07  ALQ-IDX-FECHA-MM                 PIC 9(02).
				  07  ALQ-IDX-FECHA-DD                 PIC 9(02).
		   03  ALQ-IDX-TIPO-DOC                        PIC X.
		   03  ALQ-IDX-NRO-DOC                         PIC X(20).
		   03  ALQ-IDX-IMPORTE                         PIC 9(4)V99.
		   03  ALQ-IDX-CHOFER                          PIC X(7).
		   03  ALQ-IDX-ESTADO                          PIC X.
		
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
		
	   SD APRO.
	   01 APRO-REG.
	      03 APRO-CLAVE.
		     05 APRO-CHOFER                         PIC X(7).
			 05 APRO-FECHA.
			      07  APRO-FECHA-AA                 PIC 9(04).
				  07  APRO-FECHA-MM                 PIC 9(02).
				  07  APRO-FECHA-DD                 PIC 9(02).
			 05 APRO-CLI-NUMERO                     PIC X(8).
		  03 APRO-CHO-TURNO                         PIC X.
		  03 APRO-CLI-TIPO-DOC                      PIC X.
		  03 APRO-CLI-NRO-DOC                       PIC X(20).
		  03 APRO-CLI-DIRECCION                     PIC X(30).
		  
	   FD  CLI-IDX. 
       01  CLI-IDX-REG.
		   03 CLI-IDX-CLAVE.
			   05 CLI-IDX-NUMERO				PIC X(08).
		   03 CLI-IDX-FEC-ALTA.
			   05  CLI-IDX-FEC-ALTA-AA	    	PIC 9(04).
			   05  CLI-IDX-FEC-ALTA-MM 			PIC 9(02).
			   05  CLI-IDX-FEC-ALTA-DD 			PIC 9(02).
		   03  CLI-IDX-TELEFONO            		PIC X(20).
		   03  CLI-IDX-DIRECCION           		PIC X(30).
		   03  CLI-IDX-CLAVE-ALT.
               05  CLI-IDX-DOCUMENTO            PIC X(20).
			   
			   
	   FD  CHO-IDX. 
       01  CHO-IDX-REG.
		   03 CHO-IDX-CLAVE.
			   05 CHO-IDX-LEGAJO				PIC X(07).
			   05 CHO-IDX-FECHA-DESDE.
			   	  07  CHO-IDX-FECHA-DESDE-AA    PIC 9(04).
				  07  CHO-IDX-FECHA-DESDE-MM 	PIC 9(02).
				  07  CHO-IDX-FECHA-DESDE-DD 	PIC 9(02).
		   03 CHO-IDX-FECHA-HASTA.
			   05  CHO-IDX-FECHA-HASTA-AA    	PIC 9(04).
			   05  CHO-IDX-FECHA-HASTA-MM 		PIC 9(02).
			   05  CHO-IDX-FECHA-HASTA-DD 		PIC 9(02).
		   03  CHO-IDX-TURNO            		PIC X.
		   
	   WORKING-STORAGE SECTION.
	   
	   01  WS-MENOR.
           05  WS-CLAVE-MENOR.
              10  CLAVE-MENOR-PATENTE                      PIC X(06).
              10  CLAVE-MENOR-FECHA                        PIC 9(08).

       01  WS-ANT.
           05  WS-CLAVE-ANT.
              10  CLAVE-ANT-PATENTE                      PIC X(06).
              10  CLAVE-ANT-FECHA                        PIC 9(08).
			  
	   01  WS-PAT-ANT                                    PIC X(6).
	   
	   01  FS-ALQ-IDX							PIC X(02).
           88  FS-ALQ-IDX-OK           			VALUE '00'.
           88  FS-ALQ-IDX-FIN          			VALUE '10'.
		   88  FS-ALQ-IDX-NO-EXIST				VALUE '23'.
		
	   01  FS-CHO-IDX							PIC X(02).
           88  FS-CHO-IDX-OK           			VALUE '00'.
           88  FS-CHO-IDX-FIN          			VALUE '10'.
		   88  FS-CHO-IDX-NO-EXIST				VALUE '23'.
	   		
	   01  FS-RECH-IDX							PIC X(02).
           88  FS-RECH-IDX-OK           			VALUE '00'.
           88  FS-RECH-IDX-FIN          			VALUE '10'.
		   88  FS-RECH-IDX-NO-EXIST				VALUE '23'.

	   01  FS-CLI-IDX							PIC X(02).
           88  FS-CLI-IDX-OK           			VALUE '00'.
           88  FS-CLI-IDX-FIN          			VALUE '10'.
		   88  FS-CLI-IDX-NO-EXIST				VALUE '23'.		   
	   
      * PARA CHEQUEO DE FILE STATUS
       01  WS-FILE-STATUS.
          05  WS-FS                       PIC X(02).
          05  WS-FS-NOMBRE                PIC X(08).
          05  WS-FS-FUNCION               PIC X(05).	 
	   
	   01 PARAM.
		   03 P-OP                     PIC X.
		   03 P-RETORNO                PIC X(2).
		   03 P-TIPO-IDEN              PIC 9.
		   03 P-CLI-NUM                PIC X(8).
		   03 P-CLI-DOC                PIC X(20).
		   03 P-CLI-REG.
		      07 P-CLI-IDX-CLAVE.
			     10 P-CLI-IDX-NUMERO				PIC X(08).
			  07 P-CLI-IDX-FEC-ALTA.
				  10  P-CLI-IDX-FEC-ALTA-AA	    	PIC 9(04).
				  10  P-CLI-IDX-FEC-ALTA-MM 			PIC 9(02).
				  10  P-CLI-IDX-FEC-ALTA-DD 			PIC 9(02).
			  07  P-CLI-IDX-TELEFONO            		PIC X(20).
			  07  P-CLI-IDX-DIRECCION           		PIC X(30).
			  07  P-CLI-IDX-CLAVE-ALT.
				  10  P-CLI-IDX-DOCUMENTO            PIC X(20).
			  

		   
		   
       PROCEDURE DIVISION.
      **************************************************************
      *               PROGRAMA PRINCIPAL                           *
      **************************************************************	   
	   PGM.		
		   DISPLAY "INICIA EL PROGRAMA".
		   SORT APRO
		   ON ASCENDING KEY APRO-CLAVE
		   INPUT  PROCEDURE IS 1000-PROCESO-INPUT
		   OUTPUT PROCEDURE IS 5000-PROCESO-OUTPUT.
	       DISPLAY "FINALIZA EL PROGRAMA". 
		   STOP RUN.   

       1000-PROCESO-INPUT.
	       DISPLAY "INPUT".
		   PERFORM 1100-INICIO-IN.
		   PERFORM 8000-LEER-ALQ.
		   PERFORM 1200-PROCESAR-ALQ UNTIL FS-ALQ-IDX-FIN.
           PERFORM 1300-FIN-IN.
		   
	   
	   1100-INICIO-IN.
	      DISPLAY "INICIALIZANDO ARCHIVOS".
		  OPEN INPUT CHO-IDX.
		  MOVE FS-CHO-IDX  TO  WS-FS.
		  MOVE "CHO-IDX"   TO  WS-FS-NOMBRE.
		  MOVE "ABRIR"      TO  WS-FS-FUNCION.
		  PERFORM 8900-CHECK-FILE-STATUS.
		  DISPLAY 'CHO IDX ABIERTO'.	
		  
		  OPEN OUTPUT RECH-IDX
		  MOVE FS-RECH-IDX  TO  WS-FS.
		  MOVE "RECH-IDX"   TO  WS-FS-NOMBRE.
		  MOVE "ABRIR"      TO  WS-FS-FUNCION.
		  PERFORM 8900-CHECK-FILE-STATUS.
		  DISPLAY 'RECHAZADOS IDX ABIERTO'.	
		  
		  OPEN I-O ALQ-IDX.
		  MOVE FS-ALQ-IDX  TO  WS-FS.
		  MOVE "ALQ-IDX"     TO  WS-FS-NOMBRE.
		  MOVE "ABRIR"        TO  WS-FS-FUNCION.
		  PERFORM 8900-CHECK-FILE-STATUS.
		  DISPLAY 'ALQ-IDX ABIERTO'.	

		  MOVE  'OPEN' TO P-OP.
		  DISPLAY "CALL CLIENTE-SUB OPEN".
		  CALL  "SUBTP2" USING PARAM.
		  MOVE P-RETORNO TO WS-FS.
		  MOVE "CLIENTE"	TO WS-FS-NOMBRE.
		  MOVE "ABRIR" TO WS-FS-FUNCION.
		  DISPLAY 'CLIENTE ABIERTO'.
		  
	   1200-PROCESAR-ALQ.	   
		IF ALQ-IDX-ESTADO EQUAL 'P' THEN 
	         PERFORM 1210-PROCESAR-ALQ-REG
		  END-IF.
		  PERFORM 8000-LEER-ALQ.
		  
	   1210-PROCESAR-ALQ-REG.
		     MOVE ALQ-IDX-CHOFER TO CHO-IDX-LEGAJO.
			 MOVE 00000000 TO CHO-IDX-FECHA-DESDE.
			 
  			 START CHO-IDX 
					KEY IS NOT < CHO-IDX-CLAVE
   			 END-START.

			 IF FS-CHO-IDX EQUAL '00' THEN
				DISPLAY "CICLO READ INDEX"
				PERFORM 8001-LEER-CHO UNTIL
				  FS-CHO-IDX-FIN OR
                  ALQ-IDX-FECHA > CHO-IDX-FECHA-HASTA OR
				  CHO-IDX-LEGAJO NOT EQUAL ALQ-IDX-CHOFER OR
                  ( CHO-IDX-FECHA-DESDE NOT > ALQ-IDX-FECHA AND
				    CHO-IDX-FECHA-HASTA NOT < ALQ-IDX-FECHA)
				
				IF CHO-IDX-LEGAJO EQUAL ALQ-IDX-CHOFER AND
                   CHO-IDX-FECHA-DESDE NOT > ALQ-IDX-FECHA AND
				   CHO-IDX-FECHA-HASTA NOT < ALQ-IDX-FECHA THEN
				  PERFORM 1220-APROBAR-ALQUILER
				ELSE
				  PERFORM 1230-RECHAZAR-ALQUILER
				END-IF
			 END-IF.
			 PERFORM 8000-LEER-ALQ.			 
			 
	   1220-APROBAR-ALQUILER.
	     DISPLAY "APROBAR".
		 MOVE 'T' TO ALQ-IDX-ESTADO.
		 REWRITE ALQ-IDX-REG.
		 MOVE "READ" TO P-OP.
		 MOVE ALQ-IDX-NRO-DOC TO P-CLI-DOC.
		 MOVE 2 TO P-TIPO-IDEN.
		 CALL  "SUBTP2" USING PARAM.
		 MOVE ALQ-IDX-FECHA TO APRO-FECHA.
		 MOVE ALQ-IDX-CHOFER TO APRO-CHOFER.
		 MOVE P-CLI-IDX-NUMERO TO APRO-CLI-NUMERO.
		 MOVE CHO-IDX-TURNO TO APRO-CHO-TURNO.
		 MOVE ALQ-IDX-TIPO-DOC TO APRO-CLI-TIPO-DOC.
		 MOVE ALQ-IDX-NRO-DOC  TO APRO-CLI-NRO-DOC.
		 MOVE P-CLI-IDX-DIRECCION TO APRO-CLI-DIRECCION.
		 RELEASE APRO-REG.
		 
		 
	   1230-RECHAZAR-ALQUILER.
	     DISPLAY "APROBAR".
		 MOVE 'T' TO ALQ-IDX-ESTADO.
		 REWRITE ALQ-IDX-REG.
		 MOVE ALQ-IDX-PATENTE TO RECH-IDX-PATENTE.
		 MOVE ALQ-IDX-FECHA TO RECH-IDX-FECHA.
		 MOVE ALQ-IDX-TIPO-DOC TO RECH-IDX-TIPO-DOC.
		 MOVE ALQ-IDX-NRO-DOC TO RECH-IDX-NRO-DOC.
		 MOVE ALQ-IDX-IMPORTE TO RECH-IDX-IMPORTE.
		 WRITE RECH-IDX-REG.
		  
       1300-FIN-IN.
			CLOSE CHO-IDX.
			CLOSE ALQ-IDX.
			CLOSE RECH-IDX.
			MOVE "CLOSE" TO P-OP.
			DISPLAY "CALL CLIENTE-SUB CLOSE".
			CALL  "SUBTP2" USING PARAM.
	   
	   5000-PROCESO-OUTPUT.
	       DISPLAY "OUTPUT".
  
	   8000-LEER-ALQ.
	       DISPLAY "LEO ALQ.".
      		READ ALQ-IDX AT END 
					MOVE HIGH-VALUES TO ALQ-IDX-CLAVE 
					SET FS-ALQ-IDX-FIN  TO TRUE
      		END-READ.

           IF NOT FS-ALQ-IDX-OK AND NOT FS-ALQ-IDX-FIN
               DISPLAY 'ERROR AL INTENTAR LEER ALQ-IDX'
               GO 9999-CANCELAR-PROGRAMA
           END-IF.
		  
	   8001-LEER-CHO.
		DISPLAY "LEER-CHO".
      	    READ CHO-IDX NEXT RECORD 
      				AT END SET FS-CHO-IDX-FIN TO TRUE
			END-READ.
	   
      ********************************************************
      * FIN DE LECTURA DE ARCHIVOS
      ********************************************************	  
		   
       8900-CHECK-FILE-STATUS.
           IF WS-FS NOT EQUAL "00"
              DISPLAY "CANCELACION POR ERROR"
              DISPLAY "EN ARCHIVO: " WS-FS-NOMBRE
              DISPLAY "FILESTATUS: " WS-FS
              DISPLAY "AL INTENTAR: " WS-FS-FUNCION
              GO 9999-CANCELAR-PROGRAMA
           END-IF.
		   
		   
	   9000-FINAL.
			CLOSE CHO-IDX.
	   
       9999-CANCELAR-PROGRAMA.
           PERFORM 9000-FINAL.
           DISPLAY "SALIDA POR CANCELACION DE PROGRAMA".
		   STOP RUN.