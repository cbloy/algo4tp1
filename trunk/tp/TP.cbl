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
		       ASSIGN TO "..\LISTADO.TXT" 
			   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT  ALQ-ACT
               ASSIGN TO "..\ALQACT.TXT"
			   ORGANIZATION IS LINE SEQUENTIAL
			   FILE STATUS IS FS-ALQ-ACT.

			   
	   DATA DIVISION.
	   
       FILE SECTION.
       FD SOL1.
       01  SOL1-REG.
           05  SOL1-CLAVE.
              10  SOL1-PATENTE                      PIC X(06).
              10  SOL1-FECHA.  
			      15  SOL1-FECHA-AA                 PIC 9(04).
				  15  SOL1-FECHA-MM                 PIC 9(02).
				  15  SOL1-FECHA-DD                 PIC 9(02).
		   05  SOL1-TIPO-DOC                        PIC X.
		   05  SOL1-NRO-DOC                         PIC X(20).

       FD SOL2.
       01  SOL2-REG.
           05  SOL2-CLAVE.
              10  SOL2-PATENTE                      PIC X(06).
              10  SOL2-FECHA.  
			      15  SOL2-FECHA-AA                 PIC 9(04).
				  15  SOL2-FECHA-MM                 PIC 9(02).
				  15  SOL2-FECHA-DD                 PIC 9(02).
		   05  SOL2-TIPO-DOC                        PIC X.
		   05  SOL2-NRO-DOC                         PIC X(20).

       FD SOL3.
       01  SOL3-REG.
           05  SOL3-CLAVE.
              10  SOL3-PATENTE                      PIC X(06).
              10  SOL3-FECHA.  
			      15  SOL3-FECHA-AA                 PIC 9(04).
				  15  SOL3-FECHA-MM                 PIC 9(02).
				  15  SOL3-FECHA-DD                 PIC 9(02).
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
              10  ALQ-FECHA.  
			      15  ALQ-FECHA-AA                 PIC 9(04).
				  15  ALQ-FECHA-MM                 PIC 9(02).
				  15  ALQ-FECHA-DD                 PIC 9(02).
		   05  ALQ-TIPO-DOC                        PIC X.
		   05  ALQ-NRO-DOC                         PIC X(20).
		   05  ALQ-IMPORTE                         PIC 9(4)V99.
		   
		   
	   FD ALQ-ACT.
	   01  ALQ-ACT-REG.
	       05  ALQ-ACT-CLAVE.
              10  ALQ-ACT-PATENTE                      PIC X(06).
              10  ALQ-ACT-FECHA                        PIC 9(08).
		   05  ALQ-ACT-TIPO-DOC                        PIC X.
		   05  ALQ-ACT-NRO-DOC                         PIC X(20).
		   05  ALQ-ACT-IMPORTE                         PIC 9(4)V99.
		   
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
	   
	   01 LINEA-AUX						PIC X(80).
	   
	   01 NRO-AGENCIA-IMPRIMIR          PIC X.
	   
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
		   
       01  FS-ALQ-RECH                  PIC X(02).
	       88  FS-ALQ-RECH-OK           VALUE '00'.           
		   88  FS-ALQ-RECH-FIN          VALUE '10'.
		   
	   01 FS-ALQ-ACT					PIC	X(02).
		   88 FS-ALQ-ACT-OK			VALUE '00'.
		   88 FS-ALQ-ACT-FIN			VALUE '10'.
			
      * PARA CHEQUEO DE FILE STATUS
       01  FILE-STATUS.
          05  FS                       PIC X(02).
          05  FS-NOMBRE                PIC X(08).
          05  FS-FUNCION               PIC X(05).	  
		   
      	   
		   
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
	       05  TABLA-AUT                OCCURS 300 TIMES.
		       09  TABLA-AUT-REG.
			       11  T-AUT-PATENTE     PIC X(6).
				   11  T-AUT-DESC        PIC X(30).
				   11  T-AUT-MARCA       PIC X(20).
				   11  T-AUT-COLOR       PIC X(10).
				   11  T-AUT-TAMANIO     PIC X.
				   11  T-AUT-IMPORTE     PIC 9(4)V99.
				   
       01  TABLA-ESTAD.
	       05  ESTAD-MARCAS             OCCURS 100 TIMES.
		       09  ESTAD-MARCA          PIC X(20).
			   09  ESTAD-MESES          OCCURS 12 TIMES.
			       11  ESTAD-MES        PIC 9(3).
			   09  ESTAD-TOTAL          PIC 9(4).
	   

       01  IND-I 										PIC 9(3).	   
       01  IND-J 										PIC 9(2).
	   01  IND-I2 										PIC 9(3).
	   01  IND-MAR 										PIC 9(3).
	   01  IND-MES                                      PIC 9(2).
	   01  MARCA-ENCONTRADO                             PIC X.
	   
       PROCEDURE DIVISION.
      **************************************************************
      *               PROGRAMA PRINCIPAL                           *
      **************************************************************	   
	   PGM.		
			DISPLAY "INICIA EL PROGRAMA".
	       PERFORM 1000-INICIO.
		
		   PERFORM 8300-LEER-ALQ.
		   PERFORM 8000-LEER-SOL1.
		   PERFORM 8100-LEER-SOL2.       
	       PERFORM 8200-LEER-SOL3.	
		   
		   PERFORM 2100-DETER-CLAVE-MENOR.
		   		   		   
		   PERFORM 6000-PROCESAR

      * Lo deje comentado porque es lo que pincha el while infinito		   
      *	           UNTIL FS-SOL1-FIN
      *	      AND FS-SOL2-FIN
      *		  AND FS-SOL3-FIN
      *		       AND FS-ALQ-FIN.
      
		   PERFORM 7000-IMPRIMIR-TOTAL-GRAL.
		   PERFORM 7100-IMPRIMIR-POR-MARCA.
	   			
		   DISPLAY "FINALIZA EL PROGRAMA". 
		   PERFORM 9000-FINAL.
		   STOP RUN.   
      **************************************************************
      *               RUTINAS                                      *
      **************************************************************
	  
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
		   PERFORM 1105-ABRIR-ARCHIVO-ALQ-ACT.
	  
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

	  1105-ABRIR-ARCHIVO-ALQ-ACT.
	   	   OPEN OUTPUT  ALQ-ACT.
           MOVE FS-ALQ-ACT     TO FS.
           MOVE "ALQ-ACT"   TO FS-NOMBRE.
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
           DISPLAY "LEO SOL1.".
           READ SOL1 AT END 
					 MOVE HIGH-VALUES TO SOL1-CLAVE
					 SET FS-SOL1-FIN  TO TRUE
           END-READ.

           IF NOT FS-SOL1-OK AND NOT FS-SOL1-FIN
			   DISPLAY 'ERROR AL INTENTAR LEER SOL1'
			   PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.

       8100-LEER-SOL2.
	       DISPLAY "LEO SOL2.".
           READ SOL2 AT END 
                     MOVE HIGH-VALUES TO SOL2-CLAVE
                     SET FS-SOL2-FIN  TO TRUE
           END-READ.

           IF NOT FS-SOL2-OK AND NOT FS-SOL2-FIN
               DISPLAY 'ERROR AL INTENTAR LEER SOL2'
               PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.
       
       8200-LEER-SOL3.
	       DISPLAY "LEO SOL3.".
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
	  
	   6000-PROCESAR.
	       DISPLAY "ENTRE AL PROCESAR".
		   
		   MOVE WS-MENOR TO WS-ANT.
      
      * Si encuentra el auto por patente en la tabla de autos: 
      *     Guarda en AUT-REG y EXISTE-AUTO = '1'
      * Sino EXISTE-AUTO = '0'
		   PERFORM 5000-BUSCAR-PATENTE-EN-AUTOS.
		   
           IF EXISTE-AUTO = '1' THEN
		       PERFORM 7200-IMPRIMIR-ENCABEZADO
		       MOVE 10 TO CONT-LINEAS
		   END-IF.
	       MOVE ZERO TO TOTAL-PAT-IMPORTE.
	       MOVE ZERO TO TOTAL-PAT-DIAS.
		   
		   MOVE WS-MENOR TO WS-ANT.
		   
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
		   PERFORM 6500-POSIBLE-SOL3.
		   PERFORM 6400-POSIBLE-SOL2.
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

		  
	   3000-PROCESAR-ALQUILERES.
	       MOVE  ALQ-REG TO ALQ-ACT-REG.
		   WRITE ALQ-ACT-REG.		  
		   MOVE CORRESPONDING  WS-CLAVE-MENOR TO WS-CLAVE-ANT.
	  
	   4000-PROCESAR-SOL1.
           DISPLAY "PROCESAR SOL1".
		   IF WS-MENOR EQUAL WS-ANT THEN
		       DISPLAY "ES IGUAL AL ANT"
		       MOVE SOL1-PATENTE TO RECH-PATENTE
			   MOVE SOL1-FECHA   TO RECH-FECHA
			   MOVE SOL1-TIPO-DOC TO RECH-TIPO-DOC
			   MOVE SOL1-NRO-DOC  TO RECH-NRO-DOC
			   MOVE 1 TO RECH-MOTIVO
			   MOVE 1 TO RECH-AGENCIA
			   DISPLAY "ESCRIBO EN RECH"
			   WRITE RECH-REG
		   ELSE IF EXISTE-AUTO = '0' THEN
		       MOVE SOL1-PATENTE TO RECH-PATENTE
			   MOVE SOL1-FECHA   TO RECH-FECHA
			   MOVE SOL1-TIPO-DOC TO RECH-TIPO-DOC
			   MOVE SOL1-NRO-DOC  TO RECH-NRO-DOC
			   MOVE 2 TO RECH-MOTIVO
			   MOVE 1 TO RECH-AGENCIA
			   DISPLAY "ESCRIBO EN RECH"
			   WRITE RECH-REG
			   MOVE HIGH-VALUES TO WS-ANT
		   ELSE
		       ADD AUT-IMPORTE TO TOTAL-PAT-IMPORTE
			   ADD AUT-IMPORTE TO TOTAL-GRAL-IMPORTE
			   ADD 1 TO TOTAL-PAT-DIAS
			   PERFORM 4001-GUARDAR-SOL1-ALQ-ACT
			   MOVE 1 TO NRO-AGENCIA-IMPRIMIR
			   PERFORM 7400-IMPRIMIR-APROBADO
			   MOVE WS-MENOR TO WS-ANT
	           
		       MOVE 'N' TO MARCA-ENCONTRADO
		   
               PERFORM 1500-BUSCAR-TABLA-ESTAD 
			           VARYING IND-I2 FROM 1 BY 1
			           UNTIL IND-I2 > 100 
      			   	   OR MARCA-ENCONTRADO = 'S'  
			   SUBTRACT 1 FROM IND-I2

			   ADD 1 TO ESTAD-MES (IND-I2, SOL1-FECHA-MM)
			   ADD 1 TO ESTAD-TOTAL (IND-I2)
		   END-IF.
		   
	   4001-GUARDAR-SOL1-ALQ-ACT.
		 MOVE SOL1-PATENTE TO ALQ-ACT-PATENTE.
	     MOVE SOL1-FECHA   TO ALQ-ACT-FECHA.
	     MOVE SOL1-NRO-DOC  TO ALQ-ACT-NRO-DOC.
		 MOVE SOL1-TIPO-DOC TO ALQ-ACT-TIPO-DOC.
		 MOVE AUT-IMPORTE TO ALQ-ACT-IMPORTE.
		   
	   4100-PROCESAR-SOL2.
	       DISPLAY "PROCESAR SOL2".
		   
		   IF WS-MENOR EQUAL WS-ANT THEN
		       DISPLAY "ES IGUAL AL ANT"
		       MOVE SOL2-PATENTE TO RECH-PATENTE
			   MOVE SOL2-FECHA   TO RECH-FECHA
			   MOVE SOL2-TIPO-DOC TO RECH-TIPO-DOC
			   MOVE SOL2-NRO-DOC  TO RECH-NRO-DOC
			   MOVE 1 TO RECH-MOTIVO
			   MOVE 2 TO RECH-AGENCIA
			   DISPLAY "ESCRIBO EN RECH"
			   WRITE RECH-REG
		   ELSE IF EXISTE-AUTO = '0' THEN
		       MOVE SOL2-PATENTE TO RECH-PATENTE
			   MOVE SOL2-FECHA   TO RECH-FECHA
			   MOVE SOL2-TIPO-DOC TO RECH-TIPO-DOC
			   MOVE SOL2-NRO-DOC  TO RECH-NRO-DOC
			   MOVE 2 TO RECH-MOTIVO
			   MOVE 2 TO RECH-AGENCIA
			   DISPLAY "ESCRIBO EN RECH"
			   WRITE RECH-REG
			   MOVE HIGH-VALUES TO WS-ANT
		   ELSE
		       ADD AUT-IMPORTE TO TOTAL-PAT-IMPORTE
			   ADD AUT-IMPORTE TO TOTAL-GRAL-IMPORTE
			   ADD 1 TO TOTAL-PAT-DIAS
			   PERFORM 4101-GUARDAR-SOL2-ALQ-ACT
			   MOVE 2 TO NRO-AGENCIA-IMPRIMIR
			   PERFORM 7400-IMPRIMIR-APROBADO
			   MOVE WS-MENOR TO WS-ANT
	           
		       MOVE 'N' TO MARCA-ENCONTRADO
		   
               PERFORM 1500-BUSCAR-TABLA-ESTAD 
			           VARYING IND-I2 FROM 1 BY 1
			           UNTIL IND-I2 > 100 
      			   	   OR MARCA-ENCONTRADO = 'S'  
			   SUBTRACT 1 FROM IND-I2

			   ADD 1 TO ESTAD-MES (IND-I2, SOL2-FECHA-MM)
			   ADD 1 TO ESTAD-TOTAL (IND-I2)
		   END-IF.
	
	   4101-GUARDAR-SOL2-ALQ-ACT.
		   MOVE SOL2-PATENTE TO ALQ-ACT-PATENTE.
	       MOVE SOL2-FECHA   TO ALQ-ACT-FECHA.
	       MOVE SOL2-NRO-DOC  TO ALQ-ACT-NRO-DOC.
		   MOVE SOL2-TIPO-DOC TO ALQ-ACT-TIPO-DOC.
		   MOVE AUT-IMPORTE TO ALQ-ACT-IMPORTE.	
	   
	   4200-PROCESAR-SOL3.
	       DISPLAY "PROCESAR SOL3".
		   
		   IF WS-MENOR EQUAL WS-ANT THEN
		       DISPLAY "ES IGUAL AL ANT"
		       MOVE SOL3-PATENTE TO RECH-PATENTE
			   MOVE SOL3-FECHA   TO RECH-FECHA
			   MOVE SOL3-TIPO-DOC TO RECH-TIPO-DOC
			   MOVE SOL3-NRO-DOC  TO RECH-NRO-DOC
			   MOVE 1 TO RECH-MOTIVO
			   MOVE 3 TO RECH-AGENCIA
			   DISPLAY "ESCRIBO EN RECH"
			   WRITE RECH-REG
		   ELSE IF EXISTE-AUTO = '0' THEN
		       MOVE SOL3-PATENTE TO RECH-PATENTE
			   MOVE SOL3-FECHA   TO RECH-FECHA
			   MOVE SOL3-TIPO-DOC TO RECH-TIPO-DOC
			   MOVE SOL3-NRO-DOC  TO RECH-NRO-DOC
			   MOVE 2 TO RECH-MOTIVO
			   MOVE 3 TO RECH-AGENCIA
			   DISPLAY "ESCRIBO EN RECH"
			   WRITE RECH-REG
			   MOVE HIGH-VALUES TO WS-ANT
		   ELSE
		       ADD AUT-IMPORTE TO TOTAL-PAT-IMPORTE
			   ADD AUT-IMPORTE TO TOTAL-GRAL-IMPORTE
			   ADD 1 TO TOTAL-PAT-DIAS
			   PERFORM 4201-GUARDAR-SOL3-ALQ-ACT
			   MOVE 3 TO NRO-AGENCIA-IMPRIMIR
			   PERFORM 7400-IMPRIMIR-APROBADO
			   MOVE WS-MENOR TO WS-ANT
	           
		       MOVE 'N' TO MARCA-ENCONTRADO
		   
               PERFORM 1500-BUSCAR-TABLA-ESTAD 
			           VARYING IND-I2 FROM 1 BY 1
			           UNTIL IND-I2 > 100 
      			   	   OR MARCA-ENCONTRADO = 'S'  
			   SUBTRACT 1 FROM IND-I2

			   ADD 1 TO ESTAD-MES (IND-I2, SOL3-FECHA-MM)
			   ADD 1 TO ESTAD-TOTAL (IND-I2)
		   END-IF.
	
	   4201-GUARDAR-SOL3-ALQ-ACT.
		   MOVE SOL3-PATENTE TO ALQ-ACT-PATENTE.
	       MOVE SOL3-FECHA   TO ALQ-ACT-FECHA.
	       MOVE SOL3-NRO-DOC  TO ALQ-ACT-NRO-DOC.
		   MOVE SOL3-TIPO-DOC TO ALQ-ACT-TIPO-DOC.
		   MOVE AUT-IMPORTE TO ALQ-ACT-IMPORTE.
	  
      **************************************************************
      *                    DETERMINARES                            *
      **************************************************************
	   2100-DETER-CLAVE-MENOR.
	   
           MOVE ALQ-CLAVE TO WS-CLAVE-MENOR.
		   DISPLAY  ALQ-CLAVE.		   
		   DISPLAY  SOL1-CLAVE.
		   DISPLAY  SOL2-CLAVE.
		   DISPLAY  SOL3-CLAVE.
		   DISPLAY 'EL MENOR ES ALQ'.

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
	       MOVE 1 TO IND-MAR.
		   MOVE 1 TO IND-I.
		   PERFORM 1300-CARGAR-TABLA-AUTOS
				  VARYING IND-I FROM 1 BY 1
                  UNTIL FS-AUTOS-FIN 
				  OR IND-I > 300.
		   		
	   1300-CARGAR-TABLA-AUTOS.
           PERFORM 8400-LEER-AUTOS.
           MOVE AUT-REG TO TABLA-AUT-REG(IND-I).
		   
           PERFORM 1400-CARGAR-TABLA-ESTAD.
		   
		   
       
	   1400-CARGAR-TABLA-ESTAD.
		   MOVE 'N' TO MARCA-ENCONTRADO.
		   
           PERFORM 1500-BUSCAR-TABLA-ESTAD 
		                VARYING IND-I2 FROM 1 BY 1
		                UNTIL IND-I2 > 100 
      					OR MARCA-ENCONTRADO = 'S'. 
		   
           IF MARCA-ENCONTRADO EQUAL 'N' THEN
		        
      	        MOVE AUT-MARCA TO ESTAD-MARCA(IND-MAR)
				
				MOVE 1 TO IND-MES
				PERFORM 1401-CARGAR-ESTAD-MESES-ZERO
				        VARYING IND-MES FROM 1 BY 1
						UNTIL IND-MES > 12
			    
				MOVE ZERO TO ESTAD-TOTAL(IND-MAR)
				
		        ADD 1 TO IND-MAR
           END-IF.
		   
       1401-CARGAR-ESTAD-MESES-ZERO.
	       MOVE ZERO TO ESTAD-MES(IND-MAR, IND-MES).
		   	   
   	   1500-BUSCAR-TABLA-ESTAD.
           IF ESTAD-MARCA(IND-I2) EQUAL AUT-MARCA
      		   MOVE 'S' TO MARCA-ENCONTRADO
     	   END-IF.

       
	   5000-BUSCAR-PATENTE-EN-AUTOS.
	       MOVE '0' TO EXISTE-AUTO
	       PERFORM 5001-RECORRER-TABLA-AUTOS
	               VARYING IND-I FROM 1 BY 1
			       UNTIL IND-I > 300
			       OR EXISTE-AUTO = '1'.
			  
	   5001-RECORRER-TABLA-AUTOS.
           IF T-AUT-PATENTE(IND-I) EQUAL CLAVE-MENOR-PATENTE
		       MOVE '1' TO EXISTE-AUTO
			   MOVE TABLA-AUT-REG(IND-I) TO AUT-REG
		   END-IF.
	       

      **************************************************************
      *                    IMPRIMIR                                *
      **************************************************************
	  
	   
	   7000-IMPRIMIR-TOTAL-GRAL.
	       DISPLAY "ENTRE AL IMPRIMIR GRAL".
		   STRING 'Totales general            ' TOTAL-GRAL-IMPORTE
		             DELIMITED BY SIZE INTO LINEA.
		   PERFORM 7500-IMPRIMIR-LINEA.
	  
	   7100-IMPRIMIR-POR-MARCA.
	       DISPLAY "ENTRE AL IMPRIMIR POR MARCA".
	  
	   7200-IMPRIMIR-ENCABEZADO.
	       DISPLAY "ENCABEZADO".
		   STRING  '    Patente: ' AUT-PATENTE
		           '          Descripcion: ' AUT-DESC
					DELIMITED BY SIZE INTO LINEA.
		   PERFORM 7500-IMPRIMIR-LINEA.
		   STRING  '         Marca: ' AUT-MARCA
			       DELIMITED BY SIZE INTO LINEA.
		   PERFORM 7500-IMPRIMIR-LINEA.		   
		   STRING  '         Color: ' AUT-COLOR
			       DELIMITED BY SIZE INTO LINEA.
		   PERFORM 7500-IMPRIMIR-LINEA.		   
		   STRING  '         Tamanio: ' AUT-TAMANIO
			       DELIMITED BY SIZE INTO LINEA.
		   PERFORM 7500-IMPRIMIR-LINEA.
		   PERFORM 7501-IMPRIMIR-LINEA-VACIA.
		   STRING  '            Fecha         Tipo Doc           '
		           '            Nro Document       Agencia       '
			       DELIMITED BY SIZE INTO LINEA.
		   PERFORM 7500-IMPRIMIR-LINEA.
		   STRING  '-------------------------------------------'
		           '-------------------------------------------'
			       DELIMITED BY SIZE INTO LINEA.
		   PERFORM 7500-IMPRIMIR-LINEA.
	  
	   7300-IMPRIMIR-PIE.
	       DISPLAY "PIE".
		   STRING 'Totales por patente            '
		          '                  Cantidad de dias' TOTAL-PAT-DIAS
		          '   Importe' TOTAL-PAT-IMPORTE
		             DELIMITED BY SIZE INTO LINEA.
		   PERFORM 7500-IMPRIMIR-LINEA.
		   
	   7400-IMPRIMIR-APROBADO.
	      DISPLAY "IMPRIMIENDO APROBADOS".
		  STRING '                '  ALQ-ACT-PATENTE
		         '                '  ALQ-ACT-FECHA
		         '                '  ALQ-ACT-TIPO-DOC
		         '                '  ALQ-ACT-NRO-DOC
		         '                '  NRO-AGENCIA-IMPRIMIR
		             DELIMITED BY SIZE INTO LINEA.
		  PERFORM 7500-IMPRIMIR-LINEA.
		  
		   
		   
	   7500-IMPRIMIR-LINEA.
			IF CONT-LINEAS EQUAL 60
				PERFORM 7502-IMPRIMIR-SALTO-PAGINA.
			WRITE LINEA.
			ADD 1 TO CONT-LINEAS.
			MOVE SPACES TO LINEA.
			
	   7501-IMPRIMIR-LINEA-VACIA.
			MOVE SPACES TO LINEA.
			WRITE LINEA.	   
			ADD 1 TO CONT-LINEAS.
			
	   7502-IMPRIMIR-SALTO-PAGINA.
			MOVE LINEA TO LINEA-AUX.
			PERFORM 7503-ARMAR-ENCABEZADO-PAGINA.
           	WRITE LINEA.
			PERFORM 7501-IMPRIMIR-LINEA-VACIA.			
			MOVE LINEA-AUX TO LINEA.

	   7503-ARMAR-ENCABEZADO-PAGINA.
			PERFORM 7504-ARMAR-FECHA.
			ADD 1 TO ENC-N-HOJA.
			MOVE ZEROES TO CONT-LINEAS.
			MOVE ENCABEZADO-HOJA TO LINEA.

	   7504-ARMAR-FECHA.
			ACCEPT FECHA FROM DATE.
			MOVE FECHA-DD TO ENC-FECHA-DD.
			MOVE FECHA-MM TO ENC-FECHA-MM.
			MOVE FECHA-AA TO ENC-FECHA-AA.		
		   
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
		   CLOSE ALQ-ACT.
           
       9999-CANCELAR-PROGRAMA.
           PERFORM 9000-FINAL.
           DISPLAY "SALIDA POR CANCELACION DE PROGRAMA".
		   STOP RUN.