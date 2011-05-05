       identification division.
       program-id. "libtest".
      *
      * title:  libtest.cbl
      *         rm/cobol-85 assembly language library test
      *
      * the information contained herein is proprietary to ryan mcfarland
      *    corporation, and provided for maintenance purposes only.
      *    no other use of this material is authorized or permitted without
      *    specific authorization, in writing, from ryan mcfarland corporation.
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:06  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\libtest.cbv  $
      *
      *    module history:
      *     $log:   u:\c85\dev\verify\vcs\libtest.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:06   bill
      *    no change.
      *
      *       rev 1.3   29 aug 1990 14:51:20   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.2   28 aug 1990 13:43:32   bill
      *    include copyright notice.
      *
      *       rev 1.1   15 aug 1990  9:42:50   bill
      *    improve spelling of program name.
      *    change displayed text to mixed case.
      *
      *       rev 1.0   14 aug 1990 18:09:24   bill
      *    initial revision.
      *
       author. ryan mcfarland corp.
       date-written. 26 july 1985.
       environment division.
       configuration section.
       source-computer. rmcobol-85.
       object-computer. rmcobol-85.
       data division.
       working-storage section.
       77  character-string    pic     x(40) value zeros.
       77  string-length       pic     99 binary.
       procedure division.
       main-proc.
           perform initialization.

           call "first-string" using character-string, string-length.
           display "parameters passed from first routine >",
                   character-string, size string-length,
                   "< length of ",
                   string-length, convert.

           call "first-string" using character-string, string-length.
           display "parameters passed from first routine >",
                   character-string, size string-length,
                   "< length of ",
                   string-length, convert.

           cancel "first-string".
           call "first-string" using character-string, string-length.
           display "parameters passed from first routine >",
                   character-string, size string-length,
                   "< length of ",
                   string-length, convert.


           call "second-string" using character-string, string-length.
           display "parameters passed from second routine >",
                   character-string, size string-length,
                   "< length of ",
                   string-length, convert.

           call "second-string" using character-string, string-length.
           display "parameters passed from second routine >",
                   character-string, size string-length,
                   "< length of ",
                   string-length, convert.

           cancel "second-string".
           call "second-string" using character-string, string-length.
           display "parameters passed from second routine >",
                   character-string, size string-length,
                   "< length of ",
                   string-length, convert.


           go to stop-para.


       initialization.
           display "    testing assembly language call"
            line 5 position 5 erase.


       stop-para.
           exit program.
           stop run.
