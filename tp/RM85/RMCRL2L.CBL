       identification division.
       program-id.     "rmcrl2l".
      *
      *    title:  rmcrl2l.cbl
      *        rm/cobol-85 relative file conversion
      *
      *    copyright (c) 1985-1990 by ryan mcfarland corp.  all rights reserved.
      *
      *    this file is supplied by ryan mcfarland corporation as an example onl
      *    ryan mcfarland corporation has no liability either expressed or impli
      *    as to the completeness or validity of this program.
      *
      * ==============
      *
      *    this is the relative file conversion example program
      *    to be used to convert:
      *          rm/cobol 2.n ----> rm/cobol-85
      *          record size greater than 512 bytes.
      *
      *    this is an example program that must be edited, compiled
      *    using the rm/cobol-85 rmcobol command, and executed using
      *    the rm/cobol-85 runcobol command.
      *
      *    1. edit the maximum record size value at the ^^^^ points
      *       indicated below.
      *
      *    2. compile the edited source using the rm/cobol-85 rmcobol
      *       command.
      *
      *    3. use the dos set command to indicated the input (2.n) and
      *       output (85) relative files' dos pathname, filename and
      *       extension as follows:
      *          set input=<input file's pathname\filename.ext>
      *          set output=<output file's pathname\filename.ext>
      *
      *    4. execute the object program produced by step 2 to perform
      *       the file conversion.
      *
      *    5. use the dos set command to remove the input and output
      *       files' pathname, filename and extension from the dos
      *       environment table as follows:
      *          set input=
      *          set output=
      *
      * ===========
      *
      *  version identification:
      *    $revision:   5.0  $
      *    $date:   15 sep 1990  1:43:32  $
      *    $author:   bill  $
      *    $logfile:   u:\c85\dev\util\vcs\rmcrl2l.cbv  $
      *
      *  version history:
      *    $log:   u:\c85\dev\util\vcs\rmcrl2l.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:43:32   bill
      *    no change.
      *
      *       rev 1.6   29 aug 1990 14:38:52   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.6   29 aug 1990 14:17:32   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.5   16 aug 1990  8:55:48   bill
      *    change version number to 5.00.
      *
      *       rev 1.4   28 mar 1990  9:59:16   anthony
      *    added copyright and pvcs keywords.
      *    modified title displayed to be standard.
      *
       environment division.
       configuration section.
       source-computer.    rmc.
       object-computer.    rmc.
       input-output section.
       file-control.
           select out-file assign to random "output"
           organization is relative access is random
           relative key is relkey.

           select in-file assign to random "input"
           organization is binary sequential
           file status is in-status.
       i-o-control.
           same record area for in-file, out-file.

       data division.
       file section.

       fd  out-file.
       01  out-record                  pic x(1000).
      * >>> change to maximum record size    ^^^^

       fd  in-file.
       01  in-record.
           02                                     pic x(0512).
               88  deleted-record      value low-values.
           02                                     pic x(0488).
      * >>> change to (maximum record size minus 512)   ^^^^

       working-storage section.
       01  number-of-records           pic 9(10) value is zero.

       01  deleted-records             pic 9(10) value is zero.

       01  response-indicator          pic x.
           88  affirmative-response    values "y", "y".
           88  negative-response       values "n", "n".

       01  relkey                      pic 9(10).

       01  in-status                   pic x(2).
           88  end-of-input-file       values "04", "10", "46", "97".

       procedure division.
       declaratives.
       in-file-error section.
           use after standard error procedure on in-file.
       a.
           if not end-of-input-file then
               display "input file error status ", in-status,
                       " caused premature termination." line 20
               exit program
               stop run
           end-if.
       end declaratives.

       main section.
       start-up.
           display "rm/cobol 2.n to rm/cobol-85 relative file conversion
      -            " - version 5.00" erase line 2
           display " you must edit and compile this program and set the"
                       line 5.
           display " appropriate environment variables before running"
           display " this program.  have you done this? (y/n) " line 7.
           set negative-response to true.
           accept response-indicator update position 0 tab.
           if not (affirmative-response or negative-response) then
               go to start-up
           end-if.
           if not affirmative-response then
               display "program terminated by negative response."
               exit program
               stop run
           end-if.

           set negative-response to true.
           open output out-file with lock.
           open input in-file with lock.
           move 1 to relkey.
           display "files opened."               line 9.
           display "valid data records found:"   line 10.
           display "deleted records noted:"      line 12.
           display "record number:"              line 14.

           read in-file.
           perform until end-of-input-file
               if deleted-record
                   add 1 to deleted-records
                   display deleted-records line 12 position 28
               else
                   write out-record
                   add 1 to number-of-records
                   display number-of-records line 10 position 28
               end-if
               display relkey line 14 position 28
               add 1 to relkey
               read in-file end-read
           end-perform.

           close out-file.
           close in-file.
           display "files closed.  conversion complete." line 20.
           exit program.
           stop run.
       end program "rmcrl2l".
