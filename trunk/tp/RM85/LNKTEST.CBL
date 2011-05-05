       identification division.
       program-id. "lnktest".
      *
      * title:  lnktest.cbl
      *         rm/cobol-85 assembly language call test
      *
      * the information contained herein is proprietary to ryan mcfarland
      *    corporation, and provided for maintenance purposes only.
      *    no other use of this material is authorized or permitted without
      *    specific authorization, in writing, from ryan mcfarland corporation.
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:24  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\lnktest.cbv  $
      *
      *    module history:
      *     $log:   u:\c85\dev\verify\vcs\lnktest.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:24   bill
      *    no change.
      *
      *       rev 1.4   29 aug 1990 14:51:36   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.3   28 aug 1990 13:43:44   bill
      *    include copyright notice.
      *
      *       rev 1.2   15 aug 1990 15:35:52   bill
      *    change spelling of asmtest.
      *
      *       rev 1.1   15 aug 1990  9:53:40   bill
      *    improve spelling of program name.
      *    change displayed text to mixed case.
      *
      *       rev 1.0   14 aug 1990 18:08:04   bill
      *    initial revision.
      *
       author. ryan mcfarland corp.
       date-written. 18 july 1985.
       environment division.
       configuration section.
       source-computer. rmcobol-85.
       object-computer. rmcobol-85.
       data division.
       working-storage section.
       77  character-string    pic     x(20) value zeros.
       77  string-length       pic     99 binary.
       procedure division.
       main-proc.
           perform initialization.
           call "asmtest" using character-string, string-length.
           display "parameters passed from asm routine >",
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
