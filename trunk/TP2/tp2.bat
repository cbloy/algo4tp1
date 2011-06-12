ECHO OFF
cd rm85
echo ----------------------------
echo compilo MAIN_TP2
echo ----------------------------
pause
rmcobol ..\main_tp2.cbl
echo ----------------------------
echo Armo archivos IDX
echo ----------------------------
pause
rmcobol  ..\tp2aux\alq2idx.cbl
runcobol ..\tp2aux\alq2idx.cob
rmcobol  ..\tp2aux\cho2idx.cbl
runcobol ..\tp2aux\cho2idx.cob
rmcobol  ..\tp2aux\cli2idx.cbl
runcobol ..\tp2aux\cli2idx.cob
echo ----------------------------
echo Ejecuto MAIN_TP2
echo ----------------------------
pause
runcobol ..\main_tp2.cob
pause
echo ----------------------------
echo Ejecuto IDX2RECH
echo ----------------------------
pause
rmcobol  ..\tp2aux\idx2rech.cbl
runcobol ..\tp2aux\idx2rech.cob
pause
echo ----------------------------
echo Ejecuto IDX2ALQ
echo ----------------------------
pause
rmcobol  ..\tp2aux\idx2alq.cbl
runcobol ..\tp2aux\idx2alq.cob
cd..