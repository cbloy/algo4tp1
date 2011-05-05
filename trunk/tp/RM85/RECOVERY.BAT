@ECHO OFF
CLS
IF "%1" == "" GOTO ERR1
IF "%2" == "" GOTO ERR1
IF NOT EXIST %1 GOTO ERR2
ECHO Attempt to recover '%1' in place:
PAUSE
RECOVER1 %1 %2
IF ERRORLEVEL 2 GOTO ERR3
IF ERRORLEVEL 1 GOTO REC2
CLS
ECHO Index file '%1' recovered in place.  Records with invalid duplicate keys
ECHO  can be found in '%2'.
GOTO ENDIT
:REC2
ECHO Attempt to extract data records from '%1' (file structure will
ECHO  be retrieved from the original index file):
PAUSE
RUNCOBOL RECOVER2 L=RECOVER2 K A='%1,%3,NOSUB'
IF NOT ERRORLEVEL 1 GOTO GOOD2
ECHO Attempt to extract data records from '%1' (file structure must
ECHO  be supplied by user):
PAUSE
RUNCOBOL RECOVER2 L=RECOVER2 K A='%1,%3,SUB'
IF ERRORLEVEL 1 GOTO ERR4
:GOOD2
CLS
ECHO The data recovery file specified for RECOVER2 contains the data records
ECHO  from index file '%1'.
ECHO See RM/COBOL-85 User's Guide, Appendix I, Utilities.
GOTO ENDIT
:ERR1
ECHO Usage: 	RECOVERY index-file-name drop-file-name [data-recovery-file-name]
GOTO ENDIT
:ERR2
ECHO File '%1' does not exist - execution terminated
GOTO ENDIT
:ERR3
CLS
ECHO Recovery process canceled by operator.
GOTO ENDIT
:ERR4
ECHO Recovery failed.  A portion of the data records from index file '%1' may
ECHO  exist in the data recovery file specified for RECOVER2.
ECHO See RM/COBOL-85 User's Guide, Appendix I, Utilities.
:ENDIT
