       identification division.
       program-id.  "nuctest".
      *
      *    title:  nuctest.cbl
      *        rm/cobol-85 nucleus test
      *
      * the information contained herein is proprietary to ryan mcfarland
      *    corporation, and provided for maintenance purposes only.
      *    no other use of this material is authorized or permitted without
      *    specific authorization, in writing, from ryan mcfarland corporation.
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:04  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\nuctest.cbv  $
      *
      *    module history:
      *     $log:   u:\c85\dev\verify\vcs\nuctest.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:04   bill
      *    no change.
      *
      *       rev 1.2   29 aug 1990 14:51:52   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.1   14 aug 1990 15:57:00   bill
      *    improve spelling of program name.
      *
       environment division.
       configuration section.
       source-computer.  rmcobol-85.
       object-computer.  rmcobol-85.
      *
       data division.
       working-storage section.
       01  title               pic x(60) value
           "rm/cobol-85 verify nucleus functions - version 5.00".

       01  num-1               pic 999.
       01  num-2               pic 999.
       01  group-a.
           03  sub-a1          pic 9.
           03  sub-a2          pic 9.
           03  sub-a3          pic 9.
       01  group-b         usage comp-3.
           03  sub-b1          pic s9(2)v99.
           03  sub-b2          pic s9(5)v99.
           03  sub-b3          pic s9(4).
           03  sub-b4          pic 9(4).
       01  correct             pic x(4)    value "pass".
       01  incorrect           pic x(4)    value "fail".
       01  return-key          pic x.
       01  calc-value          pic 999.
       01  edited-value            pic 9(9)v9(9).
       01  counter             pic 9.
      /
      *
       procedure division.
       init-para.
           move 100 to num-1.
           move 200 to num-2.
           move 005 to group-a.
           move .00 to sub-b1.
           move 42.00 to sub-b2.
           move 5.00 to sub-b3.
           move 1 to sub-b4.
           move zero to counter.
      *
       para-1.
           display title, low, line 1, erase.
           display "this section tests runtime nucleus functions."
                       low, line 4, position 16.
           display "operation", low, line 8, position 2.
           display "display           calculated           pass/fail"
                       low, line 8, position 15.
      *
       test-1.
           compute calc-value = (sub-a3 * 60) - num-1.
           display "compute", low, line 10, position 3.
           display calc-value, low, line 10, position 36.
           display num-2, low, line 10, position 17.
           if calc-value = num-2 then
               display correct, low, line 10, position 57
           else
               display incorrect, low, line 10, position 57
               add 1 to counter
           end-if.
      *
       test-2.
           compute calc-value rounded = (num-1 / 6) / sub-a3.
           add 2 to calc-value.
           display "compute", low, line 11, position 3.
           display calc-value, low, line 11, position 36.
           display group-a, low, line 11, position 17.
           if calc-value = group-a then
               display correct, low, line 11, position 57
           else
               display incorrect, low, line 11, position 57
               add 1 to counter
           end-if.
      *
       test-3.
           display "comp-edit", low, line 12, position 2.
           compute edited-value = ((100 - sub-b1) / 100 *
               sub-b2 * sub-b3 / sub-b4)
               on size error go to error-message.
           display num-1, low, line 12, position 17.
           display num-1, low, line 12, position 36.
           display correct, low, line 12, position 57.
      /
      *
       test-4.
           move num-1 to calc-value.
           multiply sub-a3 by num-1.
           move num-1 to num-2.
           divide 5 into num-2.
           move calc-value to num-1.
           display "calc-move", low, line 13, position 2.
           display num-1, low, line 13, position 17.
           display num-2, low, line 13, position 36.
           if num-1 = num-2 then
               display correct, low, line 13, position 57
           else
               display incorrect, low, line 13, position 57
               add 1 to counter
           end-if.
      *
       test-5.
           add 11 to num-2.
           move num-2 to group-a.
           add sub-a1, sub-a2, sub-a3 to num-1.
           divide 3 into num-1 rounded.
           add 77 to num-1.
           display "calc-move", low, line 14, position 2.
           display num-1, low, line 14, position 17.
           display num-2, low, line 14, position 36.
           if num-1 = num-2 then
               display correct, low, line 14, position 57
           else
               display incorrect, low, line 14, position 57
               add 1 to counter
           end-if.
      *
       test-6.
           subtract sub-a2 from num-1.
           compute calc-value = (num-1 / 10) - sub-a3.
           divide 10 into num-2 rounded.
           subtract sub-b4 from num-2.
           display "rounded", low, line 15, position 3.
           display num-2, low, line 15, position 17.
           display calc-value, low, line 15, position 36.
           if num-2 = calc-value then
               display correct, low, line 15, position 57
           else
               display incorrect, low, line 15, position 57
               add 1 to counter
           end-if.
      /
      *
       test-7.
           display "over-flow", low, line 16, position 2.
           compute calc-value = (101 * (sub-b4 + 9)) / sub-b4
               on size error
                   display "overflow", low, line 16, position 15
                   display "overflow", low, line 16, position 34
                   display correct, low, line 16, position 57
                   go to test-8.
           display "should've had overflow", low, line 16, position 26.
           display incorrect, low, line 16, position 57.
           add 1 to counter.
      *
       test-8.
           move 2 to calc-value.
           subtract 1 from num-2.
           perform test-8-loop num-2 times.
           multiply 57 by num-2.
           display "perform", low, line 17, position 3.
           display num-2, low, line 17, position 17.
           display calc-value, low, line 17, position 36.
           if calc-value = num-2 then
               display correct, low, line 17, position 57
           else
               display incorrect, low, line 17, position 57
               add 1 to counter
           end-if.
      *
       test-9.
           compute calc-value rounded = (num-2 - 13) / 450.
           move 001 to group-a.
           display "rounded", low, line 18, position 3.
           display group-a, low, line 18, position 17.
           display calc-value, low, line 18, position 36.
           if calc-value = sub-b4 then
               display correct, low, line 18, position 57
           else
               display incorrect, low, line 18, position 57
               add 1 to counter
           end-if.
      /
      *
       display-results.
           display counter, low, line 20, position 40.
           if counter not = 1 then display " errors encountered",
           low, position 0.
           if counter = 1 then display "error encountered",
           low, position 0.
           display "press any key to continue ...",
                       blink, high, line 24.
           accept return-key, position 0, low.
       exit-para.
           exit program.
       stop-para.
           stop run.
      *
       error-message.
           display "size error !", low, line 12, position 26.
           display incorrect, low, line 12, position 57.
           add 1 to counter.
           go to test-4.
      *
       test-8-loop.
           multiply 2 by calc-value.
           subtract sub-a3 from calc-value.
