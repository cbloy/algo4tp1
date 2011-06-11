REM This batch file illustrates the step necessary
REM to create assembly language subprograms which
REM are callable from RM/COBOL-85.
REM
masm asmtest,asmtest,,;
link asmtest,asmtest,,,,
runcobol lnktest
