REM This batch file illustrates the steps necessary
REM to create assembly language libraries which
REM are usable from RM/COBOL-85.
REM
masm libstart,libstart,,;
masm first,first,,;
masm second,second,,;
masm libend,libend,,;
link libstart+first+second+libend,asmlib,,,,
runcobol libtest l=asmlib
