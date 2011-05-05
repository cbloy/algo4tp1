echo off
cls
echo Extrae del archivo %1 y lo graba en %2
echo de tal forma que quede en formato txt
echo y luego llevarlo a otro cobol o base de datos
echo ----------
echo Agustin Zabala
echo --------------
if %1x==x goto sininput
if %2x==x goto sinoutput

:ok
   echo Lee %1 y graba %2
   echo -------------------------------------
   runcobol recover2 l=recover2 k a='%1,%2,NOSUB'
   if errorlevel 1 goto errose
   echo -------------------------------------
   type %2
   goto fin

:errose
   echo -------------------------------------
   echo O la entrada no existe o no es uno valido para el rm-cobol
   goto salir

:sininput
   echo Falta la entrada (indexado)
   goto fin

:sinoutput
   echo falta la salida (ser  ascii)
   goto fin

:fin
