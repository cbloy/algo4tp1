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
		   
		   SELECT SOL1
               ASSIGN TO "..\SOL1.TXT"
               ORGANIZATION IS LINE SEQUENTIAL 
               FILE STATUS IS FS-SOL1.
			   
		   SELECT SOL2
               ASSIGN TO "..\SOL2.TXT"
               ORGANIZATION IS LINE SEQUENTIAL 
               FILE STATUS IS FS-SOL2.

		   SELECT SOL3
               ASSIGN TO "..\SOL3.TXT"
               ORGANIZATION IS LINE SEQUENTIAL 
               FILE STATUS IS FS-SOL3.			   

		   SELECT ALQ
               ASSIGN TO "..\ALQ.TXT"
               ORGANIZATION IS LINE SEQUENTIAL 
               FILE STATUS IS FS-ALQ.

		   SELECT RECH
               ASSIGN TO "..\RECHAZOS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL 
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-RECH.
			   
		   SELECT ESTAD
               ASSIGN TO "..\ESTADIST.TXT"
               ORGANIZATION IS LINE SEQUENTIAL 
               FILE STATUS IS FS-ESTAD.

		   SELECT   LISTADO	
		       ASSIGN TO "LISTADO.TXT" 
			   ORGANIZATION IS LINE SEQUENTIAL.	

			   
	   DATA DIVISION.
	   
       FILE SECTION.
       FD SOL1.
       01  SOL1-REG.
           05  SOL1-CLAVE.
              10  SOL1-PATENTE                      PIC X(06).
              10  SOL1-FECHA                        PIC 9(08).
		   05  SOL1-TIPO-DOC                        PIC X.
		   05  SOL1-NRO-DOC                         PIC X(20).

       FD SOL2.
       01  SOL2-REG.
           05  SOL2-CLAVE.
              10  SOL2-PATENTE                      PIC X(06).
              10  SOL2-FECHA                        PIC 9(08).
		   05  SOL2-TIPO-DOC                        PIC X.
		   05  SOL2-NRO-DOC                         PIC X(20).

       FD SOL3.
       01  SOL3-REG.
           05  SOL3-CLAVE.
              10  SOL3-PATENTE                      PIC X(06).
              10  SOL3-FECHA                        PIC 9(08).
		   05  SOL3-TIPO-DOC                        PIC X.
		   05  SOL3-NRO-DOC                         PIC X(20).
		   
       FD AUTOS.
	   01  AUT-REG.
	       05  AUT-PATENTE        PIC X(6).
		   05  AUT-DESC           PIC X(30).
		   05  AUT-MARCA          PIC X(20).
		   05  AUT-COLOR          PIC X(10).
		   05  AUT-TAMANIO        PIC X.
		   05  AUT-IMPORTE        PIC 9(4)V99.
	   
       FD ALQ.
	   01  ALQ-REG.
	       05  ALQ-CLAVE.
              10  ALQ-PATENTE                      PIC X(06).
              10  ALQ-FECHA                        PIC 9(08).
		   05  ALQ-TIPO-DOC                        PIC X.
		   05  ALQ-NRO-DOC                         PIC X(20).
		   05  ALQ-IMPORTE                         PIC 9(4)V99.
		   
       FD RECH.
       01  RECH-REG.
           05  RECH-CLAVE.
              10  RECH-PATENTE                      PIC X(06).
              10  RECH-FECHA                        PIC 9(08).
		   05  RECH-TIPO-DOC                        PIC X.
		   05  RECH-NRO-DOC                         PIC X(20).
		   05  RECH-MOTIVO                          PIC 9.
		   05  RECH-AGENCIA                         PIC 9.
		   
	   FD ESTAD.
	   01 LINEA-ESTADISTICA						    PIC X(100).
	   
	   FD LISTADO.
	   01 LINEA											PIC X(80).
	   
	   WORKING-STORAGE SECTION.
	   
	   01  WS-MENOR.
           05  WS-CLAVE-MENOR.
              10  CLAVE-MENOR-PATENTE                      PIC X(06).
              10  CLAVE-MENOR-FECHA                        PIC 9(08).

       01  WS-ANT.
           05  WS-CLAVE-ANT.
              10  CLAVE-ANT-PATENTE                      PIC X(06).
              10  CLAVE-ANT-FECHA                        PIC 9(08).
			  
      *****************
      *  FILE STATUS  *	  
      *****************
	   01  FS-AUTOS                     PIC X(02).
	       88  FS-AUTOS-OK              VALUE '00'.
           88  FS-AUTOS-FIN             VALUE '10'.
		   
       01  FS-SOL1                                       PIC X(02).
           88  FS-SOL1-OK                                VALUE '00'.
           88  FS-SOL1-FIN                               VALUE '10'.
		   
       01  FS-SOL2                                       PIC X(02).
           88  FS-SOL2-OK                                VALUE '00'.
           88  FS-SOL2-FIN                               VALUE '10'.
		   
       01  FS-SOL3                                       PIC X(02).
           88  FS-SOL3-OK                                VALUE '00'.
           88  FS-SOL3-FIN                               VALUE '10'.
		   
	   01  FS-ALQ                     PIC X(02).
	       88  FS-ALQ-OK              VALUE '00'.
           88  FS-ALQ-FIN             VALUE '10'.
		   
	   01  FS-RECH                     PIC X(02).
	       88  FS-RECH-OK              VALUE '00'.
           88  FS-RECH-FIN             VALUE '10'.
		   
	   01  FS-ESTAD                     PIC X(02).
	       88  FS-ESTAD-OK              VALUE '00'.
           88  FS-ESTAD-FIN             VALUE '10'.
		   
		   
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
       01  TOTAL-PAT-IMPORTE            PIC 9(7)V99.
	   01  TOTAL-PAT-DIAS               PIC 999.
	   01  TOTAL-GRAL-IMPORTE           PIC 9(7)V99.
	   01  MOTIVO-RECHAZO               PIC X.
	   01  CONT-LINEAS                  PIC 99.
	   01  EXISTE-AUTO                  PIC X.
	   01  PATENTE-ANTERIOR             PIC X(6).
	   01  PATENTE-MENOR                PIC X(6).
	   
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
	       05  TABLA-AUT                OCCURS 300 TIMES
      *		                                ASCENDING KEY AUT-PATENTE
								        INDEXED BY IN-AUT.
		       09  TABLA-AUT-REG.
			       11  AUT-PATENTE     PIC X(6).
				   11  AUT-DESC        PIC X(30).
				   11  AUT-MARCA       PIC X(20).
				   11  AUT-COLOR       PIC X(10).
				   11  AUT-TAMANIO     PIC X.
				   11  AUT-IMPORTE     PIC 9(4)V99.
				   
       01  TABLA-ESTAD.
	       05  ESTAD-MARCAS             OCCURS 100 TIMES.
		       09  ESTAD-MARCA          PIC X(20).
			   09  ESTAD-MESES          OCCURS 12 TIMES.
			       11  ESTAD-MES        PIC 9(3).
			   09  ESTAD-TOTAL          PIC 9(4).
	   

       01  IND-I 										PIC 9(3).	   
       01  IND-J 										PIC 9(2).
	   
       PROCEDURE DIVISION.
	   PGM.		
           DISPLAY "INICIA EL PROGRAMA".
           PERFORM 1000-INICIO.
		   PERFORM 8300-LEER-ALQ.
		   PERFORM 8000-LEER-SOL1.
		   PERFORM 8100-LEER-SOL2.
		   PERFORM 8200-LEER-SOL3.
		   PERFORM 2100-DETER-CLAVE-MENOR.
		   MOVE WS-MENOR TO WS-ANT.
		   PERFORM 5000-BUSCAR-PATENTE-EN-AUTOS.
		   PERFORM 6000-PROCESAR
		      UNTIL FS-SOL1-FIN
			  AND FS-SOL2-FIN
			  AND FS-SOL3-FIN
			  AND FS-ALQ-FIN.
		   PERFORM 7000-IMPRIMIR-TOTAL-GRAL.
		   PERFORM 7100-IMPRIMIR-POR-MARCA.
		   DISPLAY "FINALIZA EL PROGRAMA". 
		   STOP RUN.
	   
	   TMP-IMPRIMIR-TABLA-AUT.
	       DISPLAY TABLA-AUT-REG(IND-I). 
		   
       1000-INICIO.
           PERFORM 1100-ABRIR-ARCHIVOS.
		   PERFORM 1200-CARGAR-TABLAS.
		   PERFORM 1300-INICIALIZAR-VARIABLES.
	  
      **************************************************************
      *               APERTURAS DE ARCHIVOS                        *
      **************************************************************
       1100-ABRIR-ARCHIVOS.
	       PERFORM 1101-ABRIR-ARCHIVO-AUTOS.
	  	   PERFORM 1102-ABRIR-ARCHIVO-SOLICITUD1.	  	   
		   PERFORM 1103-ABRIR-ARCHIVO-SOLICITUD2.	  	   
		   PERFORM 1104-ABRIR-ARCHIVO-SOLICITUD3.
		   PERFORM 1105-ABRIR-ARCHIVO-ALQUILERES.
		   PERFORM 1106-ABRIR-ARCHIVO-RECHAZOS.
		   PERFORM 1107-ABRIR-ARCHIVO-ESTAD.
		   PERFORM 1108-ABRIR-ARCHIVO-LISTADO.
	  
       1101-ABRIR-ARCHIVO-AUTOS.
		   OPEN INPUT  AUTOS.
           MOVE FS-AUTOS      TO FS.
           MOVE "AUTOS   "    TO FS-NOMBRE.
           MOVE "ABRIR"       TO FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.
		   
	   1102-ABRIR-ARCHIVO-SOLICITUD1.
	       OPEN INPUT  SOL1.
           MOVE FS-SOL1         TO FS.
           MOVE "SOL1   " TO FS-NOMBRE.
           MOVE "ABRIR"         TO FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.
		   
	   1103-ABRIR-ARCHIVO-SOLICITUD2.
	       OPEN INPUT  SOL2.
           MOVE FS-SOL2         TO FS.
           MOVE "SOL2   " TO FS-NOMBRE.
           MOVE "ABRIR"         TO FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.
		   
	   1104-ABRIR-ARCHIVO-SOLICITUD3.
	       OPEN INPUT  SOL3.
           MOVE FS-SOL3         TO FS.
           MOVE "SOL3   " TO FS-NOMBRE.
           MOVE "ABRIR"         TO FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.
		   
	   1105-ABRIR-ARCHIVO-ALQUILERES.
	   	   OPEN INPUT  ALQ.
           MOVE FS-ALQ         TO FS.
           MOVE "ALQ   "       TO FS-NOMBRE.
           MOVE "ABRIR"        TO FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.

	   1106-ABRIR-ARCHIVO-RECHAZOS.
	       OPEN OUTPUT  RECH.
           MOVE FS-RECH         TO FS.
           MOVE "RECH   "       TO FS-NOMBRE.
           MOVE "ABRIR"         TO FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.
	   
	   1107-ABRIR-ARCHIVO-ESTAD.
	   	   OPEN OUTPUT  ESTAD.
           MOVE FS-ESTAD         TO FS.
           MOVE "ESTAD   "       TO FS-NOMBRE.
           MOVE "ABRIR"          TO FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.
		  
	   1108-ABRIR-ARCHIVO-LISTADO.
		   OPEN OUTPUT LISTADO.
		   
      **************************************************************
      *       HASTA ACA APERTURAS DE ARCHIVOS                      *
      **************************************************************
	  
      **************************************************************
      *       LEO ARCHIVOS                                         *
      **************************************************************
	  
	   8000-LEER-SOL1.
           READ SOL1 AT END 
					 MOVE HIGH-VALUES TO SOL1-CLAVE
					 SET FS-SOL1-FIN  TO TRUE
           END-READ.

           IF NOT FS-SOL1-OK AND NOT FS-SOL1-FIN
               DISPLAY 'ERROR AL INTENTAR LEER SOL1'
               PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.

       8100-LEER-SOL2.
           READ SOL2 AT END 
                     MOVE HIGH-VALUES TO SOL2-CLAVE
                     SET FS-SOL2-FIN  TO TRUE
           END-READ.

           IF NOT FS-SOL2-OK AND NOT FS-SOL2-FIN
               DISPLAY 'ERROR AL INTENTAR LEER SOL2'
               PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.
       
       8200-LEER-SOL3.
           READ SOL3 AT END 
                     MOVE HIGH-VALUES TO SOL3-CLAVE
                     SET FS-SOL3-FIN  TO TRUE
           END-READ.

           IF NOT FS-SOL3-OK AND NOT FS-SOL3-FIN
               DISPLAY 'ERROR AL INTENTAR LEER SOL3'
               PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.
		   
	   8300-LEER-ALQ.
	       DISPLAY "LEO ALQ.".
	       READ ALQ AT END 
                     MOVE HIGH-VALUES TO ALQ-CLAVE
                     SET FS-ALQ-FIN  TO TRUE
           END-READ.

           IF NOT FS-ALQ-OK AND NOT FS-ALQ-FIN
               DISPLAY 'ERROR AL INTENTAR LEER ALQUILER'
               PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.
	   
      **************************************************************
      *      HASTA ACA LEO ARCHIVOS                                *
      **************************************************************		   
 
      ***************************************************************
      * 	INICIALIZO LAS VARIABLES								*
      ***************************************************************
       1300-INICIALIZAR-VARIABLES.
           MOVE HIGH-VALUES TO CLAVE-ANTERIOR.
		   MOVE HIGH-VALUES TO CLAVE-ACTUAL.
		   MOVE ZERO TO TOTAL-PAT-IMPORTE.
		   MOVE ZERO TO TOTAL-PAT-DIAS.
		   MOVE ZERO TO TOTAL-GRAL-IMPORTE.
		   MOVE ZERO TO ENC-N-HOJA.
		   MOVE ZERO TO CONT-LINEAS.		   
		   MOVE 'X' TO MOTIVO-RECHAZO.		   
		   MOVE 'X' TO EXISTE-AUTO.
		   MOVE 'X' TO PATENTE-ANTERIOR.
		   MOVE 'X' TO PATENTE-MENOR.
		   
		   
      ***************************************************************
      * 	HASTA ACA INICIALIZO LAS VARIABLES						*
      ***************************************************************
	  
	  5000-BUSCAR-PATENTE-EN-AUTOS.
	       DISPLAY "ENTRE AL BUSCAR DE PATENTE".
		   
	  6000-PROCESAR.
	       DISPLAY "ENTRE AL PROCESAR".
           IF EXISTE-AUTO = '1' THEN
		       PERFORM 7200-IMPRIMIR-ENCABEZADO
		       MOVE 10 TO CONT-LINEAS
		   END-IF.
	       MOVE ZERO TO TOTAL-PAT-IMPORTE.
	       MOVE ZERO TO TOTAL-PAT-DIAS.
		   PERFORM 6100-PROCESAR-PAT
		      UNTIL (FS-SOL1-FIN
			  AND FS-SOL2-FIN
			  AND FS-SOL3-FIN
			  AND FS-ALQ-FIN)
			  OR CLAVE-MENOR-PATENTE NOT EQUAL CLAVE-ANT-PATENTE.
	       IF EXISTE-AUTO = '1' THEN
		       PERFORM 7300-IMPRIMIR-PIE
		   END-IF.

		   
		   
	  6100-PROCESAR-PAT.
	       DISPLAY "ENTRE AL PROCESAR PATENTE".
		   PERFORM 6200-POSIBLE-ALQ.
		   PERFORM 6300-POSIBLE-SOL1.
		   PERFORM 6400-POSIBLE-SOL2.
		   PERFORM 6500-POSIBLE-SOL3.
		   MOVE WS-MENOR TO WS-ANT.
		   PERFORM 2100-DETER-CLAVE-MENOR.
	  
	  6200-POSIBLE-ALQ.
	       IF ALQ-CLAVE EQUAL WS-CLAVE-MENOR THEN
		        PERFORM 3000-PROCESAR-ALQUILERES
				PERFORM 8300-LEER-ALQ
			END-IF.
	  
	  6300-POSIBLE-SOL1.
	      IF SOL1-CLAVE EQUAL WS-CLAVE-MENOR THEN
		        PERFORM 4000-PROCESAR-SOL1
				PERFORM 8000-LEER-SOL1
		  END-IF.
	  
	  6400-POSIBLE-SOL2.
	      IF SOL2-CLAVE EQUAL WS-CLAVE-MENOR THEN
		        PERFORM 4100-PROCESAR-SOL2
				PERFORM 8100-LEER-SOL2
		  END-IF.

		  
	  6500-POSIBLE-SOL3.
	      IF SOL3-CLAVE EQUAL WS-CLAVE-MENOR THEN
		        PERFORM 4200-PROCESAR-SOL3
				PERFORM 8200-LEER-SOL3
		  END-IF.


	  7000-IMPRIMIR-TOTAL-GRAL.
	       DISPLAY "ENTRE AL IMPRIMIR GRAL".
	  
	  7100-IMPRIMIR-POR-MARCA.
	       DISPLAY "ENTRE AL IMPRIMIR POR MARCA".
	  
	  7200-IMPRIMIR-ENCABEZADO.
	       DISPLAY "ENCABEZADO".
	  
	  7300-IMPRIMIR-PIE.
	       DISPLAY "PIE".
		   IF CONT-LINEAS = 60 THEN
		      DISPLAY "IMPRIMIR SALTO DE PAGINA"
			  PERFORM 7200-IMPRIMIR-ENCABEZADO
			  MOVE 10 TO CONT-LINEAS
			  ADD 1 TO ENC-N-HOJA
		   END-IF.
		   DISPLAY "IMPRMIO TOTALES".
	  
	  3000-PROCESAR-ALQUILERES.
	       DISPLAY "PROCESAR ALQ".
	  
	  4000-PROCESAR-SOL1.
	       DISPLAY "PROCESAR SOL1".
		   
	  4100-PROCESAR-SOL2.
	       DISPLAY "PROCESAR SOL2".
		   
	  4200-PROCESAR-SOL3.
	       DISPLAY "PROCESAR SOL3".
	  

      **************************************************************
      *                    DETERMINARES                            *
      **************************************************************
	   2100-DETER-CLAVE-MENOR.
	   
           MOVE ALQ-CLAVE TO WS-CLAVE-MENOR.
		   DISPLAY  ALQ-CLAVE.		   
		   DISPLAY  SOL1-CLAVE.
		   DISPLAY  SOL2-CLAVE.
		   DISPLAY  SOL3-CLAVE.
		   DISPLAY 'EL MENOR ES ALQ'

           IF WS-CLAVE-MENOR GREATER THAN SOL1-CLAVE
                MOVE SOL1-CLAVE TO WS-CLAVE-MENOR
                DISPLAY 'EL MENOR ES SOL1'
		   END-IF.
           
           IF WS-CLAVE-MENOR GREATER THAN SOL2-CLAVE
                MOVE SOL2-CLAVE TO WS-CLAVE-MENOR
                DISPLAY 'EL MENOR ES SOL2'
		   END-IF.
		   
           IF WS-CLAVE-MENOR GREATER THAN SOL3-CLAVE
			 MOVE SOL3-CLAVE  TO WS-CLAVE-MENOR
				DISPLAY 'EL MENOR ES SOL3'
           END-IF.
		   
           DISPLAY 'CLAVE MENOR: ' WS-CLAVE-MENOR.
	  
	   8400-LEER-AUTOS.
           READ AUTOS AT END SET FS-AUTOS-FIN TO TRUE.
		   
           IF NOT FS-AUTOS-OK AND NOT FS-AUTOS-FIN
               DISPLAY 'ERROR AL INTENTAR LEER AUTOS'
               PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.
		   
	   1200-CARGAR-TABLAS.
		   PERFORM 1300-CARGAR-TABLA-AUTOS
				  VARYING IN-AUT FROM 1 BY 1
                  UNTIL FS-AUTOS-FIN 
				  OR IN-AUT > 300.
		   MOVE 1 TO IND-I.		  
		
	   1300-CARGAR-TABLA-AUTOS.
           PERFORM 8400-LEER-AUTOS.
           MOVE AUT-REG TO TABLA-AUT-REG(IN-AUT).

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
		   CLOSE SOL1.
           CLOSE SOL2.
           CLOSE SOL3.
           CLOSE ALQ.
           CLOSE RECH.
		   CLOSE ESTAD.
		   CLOSE LISTADO.
           
       9999-CANCELAR-PROGRAMA.
           PERFORM 9000-FINAL.
           DISPLAY "SALIDA POR CANCELACION DE PROGRAMA".
		   STOP RUN.		   