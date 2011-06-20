ECHO OFF
cd rm85
echo ----------------------------
echo compilo TP
echo ----------------------------
pause
rmcobol ..\tp.cbl
pause
echo ----------------------------
echo Ejecuto TP
echo ----------------------------
pause
runcobol ..\tp.cob
cd..