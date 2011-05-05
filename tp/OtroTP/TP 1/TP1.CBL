       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP1-PROCESAMIENTO-VENTA-PASAJ.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                 DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AG1
               ASSIGN TO "AG1.TXT"
               ORGANIZATION IS LINE SEQUENTIAL 
               FILE STATUS IS FS-AG1.

           SELECT AG2
               ASSIGN TO "AG2.TXT"
               ORGANIZATION IS LINE SEQUENTIAL 
               FILE STATUS IS FS-AG2.

           SELECT AG3
               ASSIGN TO "AG3.TXT"
               ORGANIZATION IS LINE SEQUENTIAL 
               FILE STATUS IS FS-AG3.

           SELECT VIAJES
               ASSIGN TO "VIAJES.TXT"
               ORGANIZATION IS LINE SEQUENTIAL 
               FILE STATUS IS FS-VIAJES.

           SELECT MICROS
               ASSIGN TO "MICROS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL 
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-MICROS.

           SELECT TRAMOS
               ASSIGN TO "TRAMOS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL 
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-TRAMOS.
			   
		   SELECT RECHAZOS
               ASSIGN TO "RECHAZOS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL 
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-RECHAZOS.
			   
		   SELECT ESTAD
               ASSIGN TO "ESTADISTICAS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL 
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-ESTAD.

		   SELECT   LISTADO	
		       ASSIGN TO "VENTAS.TXT" 
			   ORGANIZATION IS LINE SEQUENTIAL.			   

       DATA DIVISION.

       FILE SECTION.

       FD AG1.
	  01  AG1-REG.
		  05  AG1-CLAVE.
			  07 AG1-CLAVE-M.	
				  10  AG1-COD-MICRO                     PIC 9(06).
				  10  AG1-COD-TRAMO                     PIC 9(04).
				  10  AG1-FECHA                         PIC X(10).
				  10  AG1-HORA-PART                     PIC X(05).
		      07  AG1-NUM-VENTA                         PIC 9(06).
		  05  AG1-VTA-IMPORTE                           PIC 9(04)V99.


       FD AG2.
       01  AG2-REG.
		  05  AG2-CLAVE.
			  07 AG2-CLAVE-M.
				  10  AG2-COD-MICRO                     PIC 9(06).
				  10  AG2-COD-TRAMO                     PIC 9(04).
				  10  AG2-FECHA                         PIC X(10).
				  10  AG2-HORA-PART                     PIC X(05).
			  07  AG2-NUM-VENTA                         PIC 9(06).
		  05  AG2-VTA-IMPORTE                           PIC 9(04)V99.
  
  
       FD AG3.
       01  AG3-REG.
		   05  AG3-CLAVE.
			  07 AG3-CLAVE-M.	
				  10  AG3-COD-MICRO                     PIC 9(06).
				  10  AG3-COD-TRAMO                     PIC 9(04).
				  10  AG3-FECHA                         PIC X(10).
				  10  AG3-HORA-PART                     PIC X(05).
		      07  AG3-NUM-VENTA                         PIC 9(06).
		  05  AG3-VTA-IMPORTE                           PIC 9(04)V99.
              
 
       FD VIAJES.
       01  VIAJES-REG.
           05  VIAJES-CLAVE.
              10  VIAJES-COD-MICRO                      PIC 9(06).
              10  VIAJES-COD-TRAMO                      PIC 9(04).
			  10  VIAJES-FECHA                          PIC X(10).
			  10  VIAJES-HORA-PART                      PIC X(05).
           05  VIAJES-CANT-PASAJ                        PIC 9(02).
      
	   FD MICROS.
       01  MICROS-REG.
           05  MIC-COD-MICRO                            PIC 9(06).
           05  MIC-DESC                                 PIC X(30).
		   
       FD TRAMOS.
       01  TRAMOS-REG.
           05  TRA-COD-TRAMO                            PIC 9(04).
           05  TRA-DESC                                 PIC X(30).
		   
       FD RECHAZOS.
       01  RECHAZOS-REG.
		   05  RECH-COD-MICRO                           PIC 9(06).
		   05  RECH-COD-TRAMO                           PIC 9(04).
 		   05  RECH-FECHA                               PIC X(10).
		   05  RECH-HORA-PART                           PIC X(05).
		   05  RECH-NRO-VENTA                           PIC 9(06).
		   05  RECH-IMPORTE                             PIC 9(04)V99.
		   05  RECH-MOTIVO                              PIC X(20).
           05  RECH-AGENCIA                             PIC 9(01).
                              
	   FD ESTAD.
	   01 LINEA-ESTADISTICA								PIC X(100).
	   
	   FD LISTADO.
	   01 LINEA											PIC X(80).
	   
       WORKING-STORAGE SECTION.
	   
      **************************************************************
      *                     FILE STATUS                            *
      **************************************************************
       01  FS-AG1                                       PIC X(02).
           88  FS-AG1-OK                                VALUE '00'.
           88  FS-AG1-FIN                               VALUE '10'.

       01  FS-AG2                                       PIC X(02).
           88  FS-AG2-OK                                VALUE '00'.
           88  FS-AG2-FIN                               VALUE '10'.

       01  FS-AG3                                       PIC X(02).
           88  FS-AG3-OK                                VALUE '00'.
           88  FS-AG3-FIN                               VALUE '10'.

       01  FS-VIAJES                                    PIC X(02).
           88  FS-VIAJES-OK                             VALUE '00'.
           88  FS-VIAJES-FIN                            VALUE '10'.

       01  FS-MICROS                                    PIC X(02).
           88  FS-MICROS-OK                             VALUE '00'.
           88  FS-MICROS-FIN                            VALUE '10'.

       01  FS-TRAMOS                                    PIC X(02).
           88  FS-TRAMOS-OK                             VALUE '00'.
           88  FS-TRAMOS-FIN                            VALUE '10'.
		   
	   01  FS-RECHAZOS                                  PIC X(02).
           88  FS-RECHAZOS-OK                           VALUE '00'.
           88  FS-RECHAZOS-FIN                          VALUE '10'.
		   
	   01  FS-ESTAD                                     PIC X(02).
           88  FS-ESTAD-OK                              VALUE '00'.
           88  FS-ESTAD-FIN                             VALUE '10'.

      * PARA CHEQUEO DE FILE STATUS
       01  WS-FILE-STATUS.
           05  WS-FS                                    PIC X(02).
           05  WS-FS-NOMBRE                             PIC X(08).
           05  WS-FS-FUNCION                            PIC X(05).

      **************************************************************
      *                      CONTADORES                            *
      **************************************************************
		   
	   01  WS-TOTAL-SOLICITADO                          PIC 9(6).
	   01  WS-TOTALGRAL-VENTAS                          PIC 9(6).
       01  WS-TOTALGRAL-IMPORTE                         PIC 9(7)V99.
	   
	   01  WS-MENOR.
           05  WS-CLAVE-MENOR.
			  07  WS-CLAVE-COD-MICRO                	PIC 9(06).
			  07  WS-CLAVE-COD-TRAMO                	PIC 9(04).
			  07  WS-CLAVE-FECHA                    	PIC X(10).
			  07  WS-CLAVE-HORA-PART                	PIC X(05).
			  
	   01  WS-VARIABLES.
		   05  WS-MAX-PASAJ                             PIC 9(02).
	       05  WS-EXISTE-VIAJE                          PIC X(01).
		   05  WS-CANT-VENTAS-POR-VIAJE                 PIC 9(02).
		   05  WS-IMPORTE-POR-VIAJE                     PIC 9(04)V99.
		   05  WS-IMPORTE-POR-VIAJE-VISUAL              PIC ZZZ9,99.
		   05  WS-VTA-IMP-VISUAL                        PIC ZZZ9,99.

      **************************************************************
      *                      LISTADOS                              *
      **************************************************************

       01	FECHA.
   		   03 FECHA-AA   				PIC 9(02).
		   03 FECHA-MM					PIC 9(02).
		   03 FECHA-DD					PIC 9(02).
			   
       01	ENCABEZADO-HOJA.
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
		   
	   01 LINEA-AUX						PIC X(80).
	   01 CONTADOR-LINEAS				PIC 9(03) VALUE ZEROES.
	   
	   
	   01  WS-LINEA-ESTADISTICA.
		   03 ESTAD-TRAMO-NOMBRE						PIC X(10).
		   03 ESTAD-TRAMO-PORCENTAJES.
				05 ESTAD-PORCENTAJE-HORA 	OCCURS	4 TIMES. 
				   07 FILLER							PIC X(04).
				   07 NOEXISTE-EDIT						PIC 999,99.
				   07 FILLER 							PIC X(01)
														VALUE '%'.
				   07 FILLER							PIC X(02).
				   07 SOBREVTA-EDIT						PIC 999,99.
				   07 FILLER 							PIC X(01)
														VALUE '%'.
		   03 FILLER 									PIC X(04).		   
		   03 ESTAD-PORCENTAJE-TOTAL					PIC 999,99.
	   
      **************************************************************
      *                      TABLAS                                *
      **************************************************************
 
       01  WS-MICROS-TABLA.
           05  WS-MIC-TABLA     OCCURS    300 TIMES
								ASCENDING KEY WS-MIC-CLAVE
								INDEXED BY IN-MIC.
               10  WS-MICROS-REG.
                   15  WS-MIC-CLAVE                     PIC 9(06).
                   15  WS-MIC-DESC                      PIC X(30).
				   
	   01  WS-TRAMOS-TABLA.
           05  WS-TRA-TABLA     OCCURS    100 TIMES
								ASCENDING KEY WS-TRA-CLAVE
								INDEXED BY IN-TRA.
               10  WS-TRAMOS-REG.
                   15  WS-TRA-CLAVE                     PIC 9(04).
                   15  WS-TRA-DESC                      PIC X(30).

	   01 WS-ESTADISTICA.
           03  WS-TRAMOS       	OCCURS    100 TIMES.
  			   05  WS-TRAMOS-COD                     	PIC 9(04).
               05  WS-HORAS		OCCURS	  4 TIMES.
                   07  SOBREVTA							PIC 9(4).
                   07  NOEXISTE                      	PIC 9(4).
		   
      **************************************************************
      *                      ESTADISTICAS                          *
      **************************************************************
	
       01  IND-I 										PIC 9(3).
       01  IND-J 										PIC 9(1).
       01  WS-ENCONTRADO 								PIC X(1).
       01  WS-ACTUALIZO 								PIC X(1).
	   01  WS-COD-TRA-BUSCAR							PIC 9(4).
	   01  WS-RESULTADO									PIC 9(4)V99.
	   01  WS-PORCENTAJE-SOBREVTA						PIC 9(3)V99.
	   01  WS-PORCENTAJE-NOEXISTE						PIC 9(3)V99.
	   01  WS-PORCENTAJE-TOTAL-TRAMO					PIC 9(3)V99.
	   
       PROCEDURE DIVISION.
        
       PGM.		
           DISPLAY "INICIA EL PROGRAMA".
           PERFORM 1000-INICIO.
           PERFORM 2000-PROC-PASAJ-VEND UNTIL 
					FS-AG1-FIN AND
					FS-AG2-FIN AND
					FS-AG3-FIN.
		   PERFORM GENERAR-ESTADISTICA.	
           PERFORM 5000-IMPRIMIR-TOTAL-GRAL.					
           PERFORM 9000-FINAL.
           DISPLAY "FINALIZA EL PROGRAMA". 
		   STOP RUN.
		   
       1000-INICIO.
		   MOVE ZEROES TO WS-TOTALGRAL-VENTAS.
           MOVE ZEROES TO WS-TOTALGRAL-IMPORTE.
		   MOVE ZEROES TO WS-TOTAL-SOLICITADO.

           PERFORM 1100-ABRIR-ARCHIVOS.
		   PERFORM 8000-LEER-AG1.
           PERFORM 8100-LEER-AG2.
           PERFORM 8200-LEER-AG3.
		   PERFORM 8300-LEER-VIAJES.
           PERFORM 1200-CARGAR-TABLAS.

      **************************************************************
      *               APERTURAS DE ARCHIVOS                        *
      **************************************************************
       1100-ABRIR-ARCHIVOS.
		   OPEN INPUT  VIAJES.
           MOVE FS-VIAJES      TO  WS-FS.
           MOVE "CONSUL  "     TO  WS-FS-NOMBRE.
           MOVE "ABRIR"        TO  WS-FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.
	   
           OPEN INPUT AG1.
           MOVE FS-AG1         TO  WS-FS.
           MOVE "AG1     "     TO  WS-FS-NOMBRE.
           MOVE "ABRIR"        TO  WS-FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.

           OPEN INPUT AG2.
           MOVE FS-AG2         TO  WS-FS.
           MOVE "AG2     "     TO  WS-FS-NOMBRE.
           MOVE "ABRIR"        TO  WS-FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.

           OPEN INPUT AG3.
           MOVE FS-AG3         TO  WS-FS.
           MOVE "AG3     "     TO  WS-FS-NOMBRE.
           MOVE "ABRIR"        TO  WS-FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.
		   
           OPEN INPUT MICROS.
           MOVE FS-MICROS         TO  WS-FS.
           MOVE "MICROS     "     TO  WS-FS-NOMBRE.
           MOVE "ABRIR"        TO  WS-FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.
		   
		   OPEN INPUT TRAMOS.
           MOVE FS-TRAMOS         TO  WS-FS.
           MOVE "TRAMOS     "     TO  WS-FS-NOMBRE.
           MOVE "ABRIR"        TO  WS-FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.
      
           OPEN OUTPUT RECHAZOS.
           MOVE FS-RECHAZOS    TO  WS-FS.
           MOVE "RECHAZOS"     TO  WS-FS-NOMBRE.
           MOVE "ABRIR"        TO  WS-FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.
      
           OPEN OUTPUT ESTAD.
           MOVE FS-ESTAD       TO  WS-FS.
           MOVE "ESTAD"        TO  WS-FS-NOMBRE.
           MOVE "ABRIR"        TO  WS-FS-FUNCION.
           PERFORM 8900-CHECK-FILE-STATUS.
		   
		   OPEN OUTPUT LISTADO.
		   
      **************************************************************
      *                LECTURAS DE ARCHIVOS                        *
      **************************************************************

       8000-LEER-AG1.
           READ AG1 AT END 
					 MOVE HIGH-VALUES TO AG1-CLAVE
					 SET FS-AG1-FIN  TO TRUE
           END-READ.

           IF NOT FS-AG1-OK AND NOT FS-AG1-FIN
               DISPLAY 'ERROR AL INTENTAR LEER AG1'
               PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.

       8100-LEER-AG2.
           READ AG2 AT END 
                     MOVE HIGH-VALUES TO AG2-CLAVE
                     SET FS-AG2-FIN  TO TRUE
           END-READ.

           IF NOT FS-AG2-OK AND NOT FS-AG2-FIN
               DISPLAY 'ERROR AL INTENTAR LEER AG2'
               PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.
       
       8200-LEER-AG3.
           READ AG3 AT END 
                     MOVE HIGH-VALUES TO AG3-CLAVE
                     SET FS-AG3-FIN  TO TRUE
           END-READ.

           IF NOT FS-AG3-OK AND NOT FS-AG3-FIN
               DISPLAY 'ERROR AL INTENTAR LEER AG3'
               PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.

       8300-LEER-VIAJES.
           READ VIAJES AT END SET FS-VIAJES-FIN TO TRUE.
		   DISPLAY 'LEE VIAJE' VIAJES-CLAVE.
           IF NOT FS-VIAJES-OK AND NOT FS-VIAJES-FIN
               DISPLAY 'ERROR AL INTENTAR LEER VIAJES'
               PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.

	   8400-LEER-MICROS.
           READ MICROS AT END SET FS-MICROS-FIN TO TRUE.

           IF NOT FS-MICROS-OK AND NOT FS-MICROS-FIN
               DISPLAY 'ERROR AL INTENTAR LEER MICROS'
               PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.
	  
	   8500-LEER-TRAMOS.
           READ TRAMOS AT END SET FS-TRAMOS-FIN TO TRUE.

           IF NOT FS-TRAMOS-OK AND NOT FS-TRAMOS-FIN
               DISPLAY 'ERROR AL INTENTAR LEER TRAMOS'
               PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.
		   
      **************************************************************
      *                CARGA DE TABLAS                             *
      **************************************************************

	   1200-CARGAR-TABLAS.
		   PERFORM INICIALIZAR-ESTADISTICA.		
		   PERFORM 1300-CARGAR-TABLA-MICROS
				  VARYING IN-MIC FROM 1 BY 1
                  UNTIL FS-MICROS-FIN 
				  OR IN-MIC > 300.
		   MOVE 1 TO IND-I.		  
		   PERFORM 1400-CARGAR-TABLA-TRAMOS
				  VARYING IN-TRA FROM 1 BY 1
                  UNTIL FS-TRAMOS-FIN
				  OR IN-TRA > 100.
		
	   1300-CARGAR-TABLA-MICROS.
           PERFORM 8400-LEER-MICROS.
           MOVE MICROS-REG TO  WS-MICROS-REG(IN-MIC).

	   1400-CARGAR-TABLA-TRAMOS.
           PERFORM 8500-LEER-TRAMOS.
           MOVE TRAMOS-REG TO  WS-TRAMOS-REG(IN-TRA).
		   MOVE TRA-COD-TRAMO TO  WS-TRAMOS-COD(IND-I).
		   ADD 1 TO IND-I.
		   
	   INICIALIZAR-ESTADISTICA.
		   MOVE 1 TO IND-I.
		   PERFORM INIT-ESTADISTICA-IND1 UNTIL IND-I > 100.

	   INIT-ESTADISTICA-IND1.
		   MOVE 1 TO IND-J.
		   MOVE 0000 TO WS-TRAMOS-COD(IND-I).
		   PERFORM INIT-ESTADISTICA-IND2 UNTIL IND-J > 4.
		   ADD 1 TO IND-I.
		   
	   INIT-ESTADISTICA-IND2.
		   MOVE 0 TO SOBREVTA(IND-I, IND-J).
		   MOVE 0 TO NOEXISTE(IND-I, IND-J).
		   ADD 1 TO IND-J.

      **************************************************************
      *                 PROCESAR-PASAJES-VENDIDOS                  *
      **************************************************************
	   2000-PROC-PASAJ-VEND.
		   PERFORM 2100-DETER-CLAVE-MENOR.
		   
           PERFORM 2200-INIT-DATOS-VIAJES.
		   
		   PERFORM 3000-POSIBLE-CABECERA.
		   
		   PERFORM 6000-POSIBLE-AGENCIA1.
		   PERFORM 7000-POSIBLE-AGENCIA3.
		   PERFORM 6500-POSIBLE-AGENCIA2.
			   
		   PERFORM 7500-POSIBLE-PIE.
		   
      **************************************************************
      *                    DETERMINARES                            *
      **************************************************************
	   2100-DETER-CLAVE-MENOR.
	   
           MOVE AG1-CLAVE-M TO WS-CLAVE-MENOR.
           
           IF WS-CLAVE-MENOR GREATER THAN AG2-CLAVE-M
                MOVE AG2-CLAVE-M TO WS-CLAVE-MENOR
                DISPLAY 'EL MENOR ES AG2'
		   END-IF.
		   
           IF WS-CLAVE-MENOR GREATER THAN AG3-CLAVE-M
			 MOVE AG3-CLAVE-M  TO WS-CLAVE-MENOR
				DISPLAY 'EL MENOR ES AG3'
			ELSE
				DISPLAY 'EL MENOR ES AG1'
           END-IF.
		   
           DISPLAY 'CLAVE MENOR: ' WS-CLAVE-MENOR.
		   
      **************************************************************
      *                 INIT DATOS VIAJE                           *
      **************************************************************
		   
	   2200-INIT-DATOS-VIAJES.
		   MOVE "N"  TO WS-EXISTE-VIAJE.
		   
		   PERFORM 	8300-LEER-VIAJES UNTIL FS-VIAJES-FIN OR 
				(VIAJES-CLAVE GREATER OR EQUAL WS-CLAVE-MENOR).
			
		   PERFORM 2700-VERIFICAR-CLAVES.
	   
	   2700-VERIFICAR-CLAVES.
		   DISPLAY 'VIAJES-CL:' VIAJES-CLAVE.
		   DISPLAY 'WS-VIAJES-CL:' WS-CLAVE-MENOR.
	       IF VIAJES-CLAVE EQUAL WS-CLAVE-MENOR THEN
				DISPLAY 'EXISTE EL VIAJE'
				MOVE VIAJES-CANT-PASAJ TO WS-MAX-PASAJ
				MOVE "S" TO WS-EXISTE-VIAJE
				MOVE 0 TO WS-CANT-VENTAS-POR-VIAJE
				MOVE 0 TO WS-IMPORTE-POR-VIAJE
		   ELSE IF VIAJES-CLAVE LESS THAN WS-CLAVE-MENOR THEN
		        MOVE "N" TO WS-EXISTE-VIAJE
		   END-IF.

      **************************************************************
      *                IMPRIMIR TABLA PARA CABECERA                *
      **************************************************************
	   3000-POSIBLE-CABECERA.
		   
		   IF WS-EXISTE-VIAJE = 'S' THEN
		        PERFORM 3200-IMPRIMIR-CABECERA.
	   
	   3200-IMPRIMIR-CABECERA.
	        DISPLAY '3200-IMPRIMIR-CABECERA'.
			PERFORM ARMAR-ENCABEZADO-PAGINA.
			PERFORM IMPRIMIR-LINEA.
			PERFORM IMPRIMIR-LINEA-VACIA.
			PERFORM IMPRIMIR-TITULO.			
			PERFORM IMPRIMIR-DATOS-MICRO.
			PERFORM IMPRIMIR-DATOS-TRAMO.
			PERFORM IMPRIMIR-FECHA-HORA.	   
			PERFORM IMPRIMIR-ENC-DETALLE.
			
	   IMPRIMIR-TITULO.			
			STRING  '                  Listado de '
				    'pasajes vendidos y aprobados'
					DELIMITED BY SIZE INTO LINEA.
			PERFORM IMPRIMIR-LINEA.
			PERFORM IMPRIMIR-LINEA-VACIA.
			
	   IMPRIMIR-DATOS-MICRO.
		    SET IN-MIC TO 1.
		    SEARCH 	WS-MIC-TABLA 
				AT END
					DISPLAY 'CODIGO DE MICRO INEXISTENTE'
					PERFORM  9999-CANCELAR-PROGRAMA
				WHEN WS-MIC-CLAVE (IN-MIC) EQUAL WS-CLAVE-COD-MICRO
					STRING '  Código micro: ' WS-CLAVE-COD-MICRO
						   '  Detalle micro:  ' WS-MIC-DESC(IN-MIC)
							DELIMITED BY SIZE INTO LINEA
					PERFORM IMPRIMIR-LINEA
	 		END-SEARCH.
				   
	   
	   IMPRIMIR-DATOS-TRAMO.
			SET IN-TRA TO 1.
			SEARCH WS-TRA-TABLA 
				AT END 
					DISPLAY 'CODIGO DE TRAMO INEXISTENTE'
					PERFORM 9999-CANCELAR-PROGRAMA
				WHEN WS-TRA-CLAVE (IN-TRA) EQUAL WS-CLAVE-COD-TRAMO
					STRING '  Código tramo: ' WS-CLAVE-COD-TRAMO
						   '  Detalle tramo: ' WS-TRA-DESC(IN-TRA) 
							DELIMITED BY SIZE INTO LINEA
					PERFORM IMPRIMIR-LINEA
			END-SEARCH.
	   
       IMPRIMIR-FECHA-HORA.	   
			STRING '  Fecha: ' WS-CLAVE-FECHA
				   '  Hora: ' WS-CLAVE-HORA-PART
					DELIMITED BY SIZE INTO LINEA.
			PERFORM IMPRIMIR-LINEA.			
			PERFORM IMPRIMIR-LINEA-VACIA.

	   IMPRIMIR-ENC-DETALLE.	
			STRING '                    NRO-VENTA'
				   '       IMPORTE  '
				   '     AGENCIA '
				  DELIMITED BY SIZE	INTO LINEA.
		    PERFORM IMPRIMIR-LINEA.
			STRING '----------------------------------------'
				   '----------------------------------------'
				   DELIMITED BY SIZE INTO LINEA.	
			PERFORM IMPRIMIR-LINEA.
			
      **************************************************************
      *                PROCESA POSIBLES AGENCIAS                   *
      **************************************************************
	   6000-POSIBLE-AGENCIA1.
	       DISPLAY '6000-POSIBLE-AGENCIA1'.
		   PERFORM 6100-PROCESAR-VENTAS
			   UNTIL FS-AG1-FIN 
			   OR AG1-CLAVE-M NOT EQUAL WS-CLAVE-MENOR.
    
   	   6100-PROCESAR-VENTAS.
		   ADD 1 TO WS-TOTAL-SOLICITADO.
           PERFORM 6200-PROCESO-VENTA.
           PERFORM 8000-LEER-AG1.

       6200-PROCESO-VENTA.

           IF WS-EXISTE-VIAJE = "S" AND WS-MAX-PASAJ > 0 THEN
			   DISPLAY 'VENTA AG1'
			   SUBTRACT 1 FROM WS-MAX-PASAJ
               ADD 1 TO WS-CANT-VENTAS-POR-VIAJE
               ADD AG1-VTA-IMPORTE TO WS-IMPORTE-POR-VIAJE
               PERFORM 6300-IMPRIMIR-DETALLE
           ELSE
			   MOVE AG1-COD-MICRO    TO RECH-COD-MICRO
			   MOVE AG1-COD-TRAMO    TO RECH-COD-TRAMO
			   MOVE AG1-FECHA        TO RECH-FECHA
			   MOVE AG1-HORA-PART    TO RECH-HORA-PART
			   MOVE AG1-NUM-VENTA    TO RECH-NRO-VENTA
			   MOVE AG1-VTA-IMPORTE  TO RECH-IMPORTE
			   MOVE  1               TO RECH-AGENCIA
		   
               PERFORM CLASIF-RECHAZO
           END-IF.
 
       6300-IMPRIMIR-DETALLE.
			MOVE AG1-VTA-IMPORTE TO WS-VTA-IMP-VISUAL
	   
			STRING 	'                      ' 
					AG1-NUM-VENTA
					'         ' WS-VTA-IMP-VISUAL
					'          1' 
				  DELIMITED BY SIZE	INTO LINEA.
		    PERFORM IMPRIMIR-LINEA.

      * ************************************************************
	   6500-POSIBLE-AGENCIA2.
	       DISPLAY '6500-POSIBLE-AGENCIA2'.
		   PERFORM 6600-PROCESAR-VENTAS
			   UNTIL FS-AG2-FIN 
			   OR AG2-CLAVE-M NOT EQUAL WS-CLAVE-MENOR.
		   
   	   6600-PROCESAR-VENTAS.
	       ADD 1 TO WS-TOTAL-SOLICITADO.
           PERFORM 6700-PROCESO-VENTA.
           PERFORM 8100-LEER-AG2.
 
       6700-PROCESO-VENTA.

           IF WS-EXISTE-VIAJE = "S" AND WS-MAX-PASAJ > 0 THEN
           
			   SUBTRACT 1 FROM WS-MAX-PASAJ
               ADD 1 TO WS-CANT-VENTAS-POR-VIAJE
               ADD AG2-VTA-IMPORTE TO WS-IMPORTE-POR-VIAJE
               PERFORM 6800-IMPRIMIR-DETALLE
           ELSE
		   
			   MOVE AG2-COD-MICRO    TO RECH-COD-MICRO
			   MOVE AG2-COD-TRAMO    TO RECH-COD-TRAMO
			   MOVE AG2-FECHA        TO RECH-FECHA
			   MOVE AG2-HORA-PART    TO RECH-HORA-PART
			   MOVE AG2-NUM-VENTA    TO RECH-NRO-VENTA
			   MOVE AG2-VTA-IMPORTE  TO RECH-IMPORTE
			   MOVE  2               TO RECH-AGENCIA
		   
               PERFORM CLASIF-RECHAZO
           END-IF.

       6800-IMPRIMIR-DETALLE.
			MOVE AG2-VTA-IMPORTE TO WS-VTA-IMP-VISUAL
			STRING 	'                      '
					AG2-NUM-VENTA
					'         ' WS-VTA-IMP-VISUAL
					'          2' 
				  DELIMITED BY SIZE	INTO LINEA.
		    PERFORM IMPRIMIR-LINEA.

	   
      * ************************************************************
	  7000-POSIBLE-AGENCIA3.
	  
	    DISPLAY '7000-POSIBLE-AGENCIA3'.
	  
		PERFORM 7100-PROCESAR-VENTAS
		   UNTIL FS-AG3-FIN 
		   OR AG3-CLAVE-M NOT EQUAL WS-CLAVE-MENOR.
		   
	   7000-POSIBLE-AGENCIA3-EXIT.
	   
   	   7100-PROCESAR-VENTAS.
               DISPLAY '7100-PROCESAR-VENTAS'.
	       ADD 1 TO WS-TOTAL-SOLICITADO.
           PERFORM 7200-PROCESO-VENTA.
           PERFORM 8200-LEER-AG3.
 
       7200-PROCESO-VENTA.
	  
           IF WS-EXISTE-VIAJE = "S" AND WS-MAX-PASAJ > 0 THEN
           
			   SUBTRACT 1 FROM WS-MAX-PASAJ
               ADD 1 TO WS-CANT-VENTAS-POR-VIAJE
               ADD AG3-VTA-IMPORTE TO WS-IMPORTE-POR-VIAJE
               PERFORM 7300-IMPRIMIR-DETALLE
           ELSE
		   
			   MOVE AG3-COD-MICRO    TO RECH-COD-MICRO
			   MOVE AG3-COD-TRAMO    TO RECH-COD-TRAMO
			   MOVE AG3-FECHA        TO RECH-FECHA
			   MOVE AG3-HORA-PART    TO RECH-HORA-PART
			   MOVE AG3-NUM-VENTA    TO RECH-NRO-VENTA
			   MOVE AG3-VTA-IMPORTE  TO RECH-IMPORTE
			   MOVE  3               TO RECH-AGENCIA
		   
               PERFORM CLASIF-RECHAZO
           END-IF.
 
       7300-IMPRIMIR-DETALLE.
			MOVE AG3-VTA-IMPORTE TO WS-VTA-IMP-VISUAL
			STRING 	'                      '
					AG3-NUM-VENTA
					'         ' WS-VTA-IMP-VISUAL
					'          3' 
				  DELIMITED BY SIZE	INTO LINEA.
		    PERFORM IMPRIMIR-LINEA.

      **************************************************************
      *                CLASIFICAR RECHAZO                          *
      **************************************************************
	  
       CLASIF-RECHAZO.
      *     DISPLAY 'CLASIF-RECHAZO'.
		   
           PERFORM GUARDAR-TABLA-ESTADISTICA.

           IF WS-EXISTE-VIAJE EQUAL "N" THEN
               MOVE " VIAJE INEXISTENTE" TO RECH-MOTIVO
           ELSE
			   DISPLAY 'RECHAZO SOBREVENTA'
               MOVE " SOBREVENTA" TO RECH-MOTIVO
           END-IF
		   
           PERFORM GRABAR-RECHAZO.

       GRABAR-RECHAZO.
           WRITE RECHAZOS-REG.
           IF NOT FS-RECHAZOS-OK
              DISPLAY 'ERROR AL GRABAR RECHAZO'
              DISPLAY ' RETURN CODE : ' FS-RECHAZOS
              PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.

      **************************************************************
      *             GUARDAR TABLA ESTADISTICA                      *
      **************************************************************
      *  
       GUARDAR-TABLA-ESTADISTICA.
			PERFORM BUSCAR-TRAMO-ESTADISTICA.
			IF WS-ENCONTRADO EQUAL 'N' THEN
			  PERFORM AGREGAR-TRAMO-ESTADISTICA
			END-IF.
			PERFORM ACTUALIZAR-ESTADISTICA.
	
       BUSCAR-TRAMO-ESTADISTICA.		
           MOVE 1 TO IND-I.
		   MOVE 'N' TO WS-ENCONTRADO.
		   MOVE RECH-COD-TRAMO TO WS-COD-TRA-BUSCAR.
           PERFORM BUSCAR-TRAMO UNTIL IND-I > 100 
					OR WS-ENCONTRADO = 'S'. 
        
       BUSCAR-TRAMO.
		   IF (WS-TRAMOS-COD(IND-I) EQUAL WS-COD-TRA-BUSCAR) THEN
			   MOVE 'S' TO WS-ENCONTRADO
		   ELSE
			   ADD 1 TO IND-I
		   END-IF.

       AGREGAR-TRAMO-ESTADISTICA.		
           MOVE 1 TO IND-I.
		   MOVE 'N' TO WS-ENCONTRADO.
		   MOVE 0000 TO WS-COD-TRA-BUSCAR.
           PERFORM BUSCAR-TRAMO UNTIL IND-I > 100 
					OR WS-ENCONTRADO = 'S'. 
		   MOVE RECH-COD-TRAMO TO WS-TRAMOS-COD(IND-I).

		   
	   ACTUALIZAR-ESTADISTICA.
           MOVE 1 TO IND-J.
		   MOVE 'N' TO WS-ACTUALIZO.
           PERFORM GUAR-EST-IND-J UNTIL IND-J > 4 
					OR WS-ACTUALIZO = 'S'.
	   
       GUAR-EST-IND-J.
			
		   IF IND-J = 1 AND RECH-HORA-PART >= '00:01' 
			  AND RECH-HORA-PART <= '06:00' THEN
				IF WS-EXISTE-VIAJE = 'N'
				  ADD 1 TO NOEXISTE(IND-I, IND-J)
                                  DISPLAY 'SUME UN RECHAZO J1'
				  MOVE 'S' TO WS-ACTUALIZO
				ELSE
				  ADD 1 TO SOBREVTA(IND-I, IND-J) 
		   END-IF.
		   IF IND-J = 2 AND RECH-HORA-PART >= '06:01' 
			  AND RECH-HORA-PART <= '12:00' THEN
				IF WS-EXISTE-VIAJE = 'N'
				  ADD 1 TO NOEXISTE(IND-I, IND-J)
                                  DISPLAY 'SUME UN RECHAZO J2'
				  MOVE 'S' TO WS-ACTUALIZO
				ELSE
				  ADD 1 TO SOBREVTA(IND-I, IND-J) 
		   END-IF.
		   IF IND-J = 3 AND RECH-HORA-PART >= '12:01' 
			  AND RECH-HORA-PART <= '18:00' THEN
				IF WS-EXISTE-VIAJE = 'N'
				  ADD 1 TO NOEXISTE(IND-I, IND-J)
                                  DISPLAY 'SUME UN RECHAZO J3'
				  MOVE 'S' TO WS-ACTUALIZO
				ELSE
				  ADD 1 TO SOBREVTA(IND-I, IND-J) 
		   END-IF.
		   IF IND-J = 4 AND RECH-HORA-PART >= '18:01' 
			  AND RECH-HORA-PART <= '00:00' THEN
				IF WS-EXISTE-VIAJE = 'N'
				  ADD 1 TO NOEXISTE(IND-I, IND-J)
                                  DISPLAY 'SUME UN RECHAZO J4'
				  MOVE 'S' TO WS-ACTUALIZO
				ELSE
				  ADD 1 TO SOBREVTA(IND-I, IND-J) 
		   END-IF.
			
		   ADD 1 TO IND-J.
		
      * FIN DEL GUAR-EST-IND-J

      **************************************************************
      *                PROCESA POSIBLE PIE                         *
      **************************************************************
	   7500-POSIBLE-PIE.
		   IF WS-EXISTE-VIAJE = 'S' 
		        AND WS-MAX-PASAJ GREATER OR EQUAL 0 THEN
		        PERFORM 7600-IMPRIMIR-TOTAL-VIAJE
				ADD WS-CANT-VENTAS-POR-VIAJE TO WS-TOTALGRAL-VENTAS
				ADD WS-IMPORTE-POR-VIAJE TO WS-TOTALGRAL-IMPORTE
		   END-IF.
		   
	   
	   7600-IMPRIMIR-TOTAL-VIAJE.
			MOVE SPACES TO LINEA.
			WRITE LINEA.
			MOVE WS-IMPORTE-POR-VIAJE TO 
				WS-IMPORTE-POR-VIAJE-VISUAL
			STRING '      Totales por viaje      '
				  'Cantidad de ventas ' WS-CANT-VENTAS-POR-VIAJE
				  '      Importe ' WS-IMPORTE-POR-VIAJE-VISUAL
				DELIMITED SIZE INTO LINEA.
			WRITE LINEA.
			MOVE SPACES TO LINEA.
			WRITE LINEA.
	  
      **************************************************************
      *                CALCULO ESTADISTICA                         *
      **************************************************************
	   
	   GENERAR-ESTADISTICA.
                   DISPLAY 'TOTAL SOLICITADO' WS-TOTAL-SOLICITADO
		   PERFORM GUARDAR-ENCABEZADO-ESTADIST.
		   MOVE 1 TO IND-I.
		   PERFORM GENERAR-ESTADISTICA-TRAMO UNTIL IND-I > 100.
        
	   GENERAR-ESTADISTICA-TRAMO.
		   MOVE ZEROES TO WS-PORCENTAJE-TOTAL-TRAMO.
		   MOVE 1 TO IND-J.
		   PERFORM GENERAR-ESTADISTICA-HORAS UNTIL IND-J > 4.
		   PERFORM GUARDAR-TRAMOS-HORAS.
		   ADD 1 TO IND-I.

	   GENERAR-ESTADISTICA-HORAS.
		   PERFORM INICIALIZAR-TOTALES.
		   COMPUTE WS-RESULTADO = 
					SOBREVTA(IND-I, IND-J) * 100.
		   COMPUTE WS-RESULTADO = 
				(WS-RESULTADO / WS-TOTAL-SOLICITADO).
		   ADD WS-RESULTADO TO WS-PORCENTAJE-SOBREVTA.
		   ADD WS-RESULTADO TO WS-PORCENTAJE-TOTAL-TRAMO. 
		   MOVE WS-PORCENTAJE-SOBREVTA TO SOBREVTA-EDIT(IND-J).
		   IF( SOBREVTA (IND-I, IND-J) > 0) THEN
				DISPLAY 'HAY SOBREVTA'
				DISPLAY WS-RESULTADO
		   END-IF.
		
		  
		   COMPUTE WS-RESULTADO = 
					NOEXISTE(IND-I, IND-J) * 100.
		   COMPUTE WS-RESULTADO = 
					(WS-RESULTADO / WS-TOTAL-SOLICITADO).
		   ADD WS-RESULTADO TO WS-PORCENTAJE-NOEXISTE.
		   ADD WS-RESULTADO TO WS-PORCENTAJE-TOTAL-TRAMO. 
		   MOVE WS-PORCENTAJE-NOEXISTE TO NOEXISTE-EDIT(IND-J).
                   IF( NOEXISTE(IND-I, IND-J) > 0) THEN
				DISPLAY 'HAY NOEXIT-VIAJE'
				DISPLAY WS-RESULTADO
		   END-IF.		  
		   ADD 1 TO IND-J.
	   
	   GUARDAR-TRAMOS-HORAS.
		  IF WS-PORCENTAJE-TOTAL-TRAMO NOT EQUAL 0 THEN
			  MOVE SPACES TO LINEA-ESTADISTICA	
			  STRING 'TRAMO ' WS-TRAMOS-COD(IND-I)
					  DELIMITED BY SIZE INTO ESTAD-TRAMO-NOMBRE
			  MOVE WS-PORCENTAJE-TOTAL-TRAMO 
					TO ESTAD-PORCENTAJE-TOTAL
			  MOVE WS-LINEA-ESTADISTICA TO LINEA-ESTADISTICA
			  WRITE LINEA-ESTADISTICA
		  END-IF.
		  
	   GUARDAR-ENCABEZADO-ESTADIST.  
		  STRING '       ESTADISTICA DE RECHAZOS por no existencia'
				 'de pasajes/ supero maximo de pasajes'	
				 DELIMITED BY SIZE INTO LINEA-ESTADISTICA.
		  WRITE LINEA-ESTADISTICA.
		  
		  MOVE SPACES TO LINEA-ESTADISTICA.
		  WRITE LINEA-ESTADISTICA.
		  WRITE LINEA-ESTADISTICA.
		  
		  STRING '		       00:01-06:00 Hs    06:01-12:00 Hs'
				 '    12:01-18:00 Hs    18:01-00:00 Hs   %Tramo'
				DELIMITED BY SIZE INTO LINEA-ESTADISTICA.
		  WRITE LINEA-ESTADISTICA.

		  MOVE SPACES TO LINEA-ESTADISTICA.
		  WRITE LINEA-ESTADISTICA.
		  
	   INICIALIZAR-TOTALES.
		   MOVE ZEROES TO WS-RESULTADO.
		   MOVE ZEROES TO WS-PORCENTAJE-SOBREVTA.
		   MOVE ZEROES TO WS-PORCENTAJE-NOEXISTE.

      **************************************************************
      *                IMPRIMIR TOTALES GENERALES                  *
      **************************************************************	  
	   5000-IMPRIMIR-TOTAL-GRAL.
			MOVE SPACES TO LINEA.
			MOVE WS-TOTALGRAL-IMPORTE TO 
				WS-IMPORTE-POR-VIAJE-VISUAL
			STRING '   Totales generales    '
				  'Cantidad de ventas ' WS-TOTALGRAL-VENTAS
				  '     Importe ' WS-IMPORTE-POR-VIAJE-VISUAL
				DELIMITED SIZE INTO LINEA.
			WRITE LINEA.
			MOVE SPACES TO LINEA.
			WRITE LINEA.
					   
      **************************************************************
      *                      AUXILIARES                            *
      **************************************************************
	  
	   IMPRIMIR-LINEA.
			IF CONTADOR-LINEAS EQUAL 60
				PERFORM IMPRIMIR-SALTO-PAGINA.
			WRITE LINEA.
			ADD 1 TO CONTADOR-LINEAS.
			MOVE SPACES TO LINEA.
			
	   IMPRIMIR-LINEA-VACIA.
			MOVE SPACES TO LINEA.
			WRITE LINEA.	   
			ADD 1 TO CONTADOR-LINEAS.
			
	   IMPRIMIR-SALTO-PAGINA.
			MOVE LINEA TO LINEA-AUX.
			PERFORM ARMAR-ENCABEZADO-PAGINA.
           	WRITE LINEA.
			PERFORM IMPRIMIR-LINEA-VACIA.			
			MOVE LINEA-AUX TO LINEA.

	   ARMAR-ENCABEZADO-PAGINA.
			PERFORM ARMAR-FECHA.
			ADD 1 TO ENC-N-HOJA.
			MOVE ZEROES TO CONTADOR-LINEAS.
			MOVE ENCABEZADO-HOJA TO LINEA.

	   ARMAR-FECHA.
			ACCEPT FECHA FROM DATE.
			MOVE FECHA-DD TO ENC-FECHA-DD.
			MOVE FECHA-MM TO ENC-FECHA-MM.
			MOVE FECHA-AA TO ENC-FECHA-AA.				
			
       8900-CHECK-FILE-STATUS.
           IF WS-FS NOT EQUAL "00"
              DISPLAY "CANCELACION POR ERROR"
              DISPLAY "EN ARCHIVO: " WS-FS-NOMBRE
              DISPLAY "FILESTATUS: " WS-FS
              DISPLAY "AL INTENTAR: " WS-FS-FUNCION
              PERFORM 9999-CANCELAR-PROGRAMA
           END-IF.

       9000-FINAL.
           CLOSE AG1.
           CLOSE AG2.
           CLOSE AG3.
           CLOSE VIAJES.
           CLOSE RECHAZOS.
		   CLOSE ESTAD.
		   CLOSE LISTADO.

       9999-CANCELAR-PROGRAMA.
           PERFORM 9000-FINAL.
           DISPLAY "SALIDA POR CANCELACION DE PROGRAMA".
		   STOP RUN.
