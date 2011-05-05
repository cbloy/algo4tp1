       identification division.
       program-id.  "verify".
      *
      *    title:  verify.cbl
      *        rm/cobol-85 verify test suite driver
      *
      * the information contained herein is proprietary to ryan mcfarland
      *    corporation, and provided for maintenance purposes only.
      *    no other use of this material is authorized or permitted without
      *    specific authorization, in writing, from ryan mcfarland corporation.
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:14  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\verify.cbv  $
      *
      *    module history:
      *     $log:   u:\c85\dev\verify\vcs\verify.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:14   bill
      *    no change.
      *
      *       rev 1.2   29 aug 1990 14:53:28   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.1   14 aug 1990 17:52:52   bill
      *    convert menu to mixed case.
      *    add option to run window test.
      *
       environment division.
       configuration section.
       source-computer.  rmcobol-85.
       object-computer.  rmcobol-85.
      *
      *
      *
       data division.
      *
       working-storage section.
       01  title               pic x(60) value
           "rm/cobol-85 verification program - version 5.00".

       01  function-number     pic 9.
       01  return-key          pic x.
       01  filler value
           "vdttest filetestnuctest prntest sorttestwintest ".
           02  test-name       pic x(8) occurs 6.
      /
       procedure division.
       main-start.
           display "the rm/cobol-85 verification suite",
                       high, line 1, position 20, erase.

           display "the rm/cobol-85 verification suite tests the followi
      -            "ng aspects of rm/cobol-85:", line 3, low.
           display "  -  the terminal configuration",    low, line 5.
           display "  -  the i-o subsystem",             low, line 7.
           display "  -  the nucleus",                   low, line 9.
           display "  -  the printer configuration",     low, line 11.
           display "  -  the sort/merge subsystem",      low, line 13.
           display "  -  the windows subsystem (if enabled)",
                                low, line 15.

           display "the test selection menus are designed to be self-exp
      -            "lanatory.  where a", line 18, low.

           display "specific field or type of data is required, a prompt
      -            " will appear on the", line 19, low.

           display "screen so indicating.", low, line 20.

           display " press any key to continue...",
                       high, line 22, position 20.
           accept return-key, position 0.
      /
       display-main-menu.
           display title, line 1, erase, low.
           display " ", low.
           display "   1) screen configuration test", low.
           display "   2) file system test", low.
           display "   3) nucleus test", low.
           display "   4) printer test", low.
           display "   5) sort/merge test", low.
           display "   6) windows test", low.
           display "   7) exit program", low.
           display " ", low.
           display "      enter desired test number       ", low.

       accept-main-menu.
           accept function-number line 11 position 37 tab, convert.
           if function-number greater than 0 and less than 7 then
               call test-name (function-number)
               go display-main-menu
           end-if.
           if function-number not equal 7 then
               display "  " line 11 position 37
               go to accept-main-menu
           end-if.
           exit program.
           stop run.
