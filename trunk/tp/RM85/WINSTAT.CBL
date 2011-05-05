       identification division.
       program-id. "winstat".
      *
      * title:  winstat.cbl
      *         rm/cobol-85 test windows status codes
      *
      * the information contained herein is proprietary to ryan mcfarland
      * corporation, and provided for maintenance purposes only.
      * no other use of this material is authorized or permitted without
      * specific authorization, in writing, from ryan mcfarland corporation.
      *
      *author: rah.
      *date written: 3/01/90
      *program description.
      *this program tests window status codes.
      *
      *input-file - none
      *operator-response - enter selection number from terminal.
      *output-file - none
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:20  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\winstat.cbv  $
      *
      * module history:
      *     $log:   u:\c85\dev\verify\vcs\winstat.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:20   bill
      *    no change.
      *
      *       rev 1.2   13 sep 1990 16:30:12   donny
      *    create windows according to dimensions passed from wintest.
      *
      *       rev 1.1   29 aug 1990 14:54:56   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.0   15 aug 1990 14:41:30   bill
      *    initial revision.
      *
       environment division.
       data division.

       working-storage section.
       77  x                            pic x.

       copy "win.cpy".

       linkage section.
       01 screen-num-rows               pic 999.
       01 screen-num-cols               pic 999.

       procedure division using screen-num-rows, screen-num-cols.
       begin-main.
      *clear screen.
           display space line 1 erase eos.

      *define and create window.
           compute wcb-num-rows = screen-num-rows - 3.
           compute wcb-num-cols = screen-num-cols - 2.
           move "s" to wcb-location-reference.
           move "y" to wcb-border-switch.
           move  0  to wcb-border-type.
           move "*" to wcb-border-char.
           move "t" to wcb-title-location.
           move "c" to wcb-title-position.
           move  " windows:  rm/cobol-85  window status codes "
                   to wcb-title.
           move 44 to wcb-title-length.

      *display normal window status = 0.
           display wcb high erase line 2 position 2
                   control "window-create".
           accept window-status from exception status.
           display "normal window status"        line 10 position 30,
                   "window status = "            line 13 position 30,
                   window-status                 line 13 position 46,
                   "status should = 000"         line 14 position 30,
                   "press <return> to continue " line 22 position 27
           accept  x line 22 position 55 no beep.
           display wcb control "window-remove".

      *display window title will not fit on screen.
           display space line 1 erase eos.
           move "n" to wcb-border-switch.
           move "t" to wcb-title-location.
           display wcb high line 1 position 2 control "window-create".
           accept window-status from exception status.
           display "title will not fit on screen" line 10 position 26,
                   "window status = "             line 13 position 30,
                   window-status                  line 13 position 46,
                   "status should = 301"          line 14 position 30,
                   "press <return> to continue "  line 22 position 27
           accept  x line 22 position 55 no beep.
           display wcb control "window-remove".

      *display window border will not fit on screen.
           display space line 1 erase eos.
           move "y" to wcb-border-switch.
           move  0  to wcb-border-type.
           move "*" to wcb-border-char.
           move "t" to wcb-title-location.
           display wcb high line 4 position 2 control "window-create".
           accept window-status from exception status.
           display "border will not fit on screen" line 10 position 26,
                   "window status = "              line 13 position 31,
                   window-status                   line 13 position 47,
                   "status should = 301"           line 14 position 31,
                   "press <return> to continue "   line 22 position 27
           accept  x line 22 position 55 no beep.
           display wcb control "window-remove".

      *display window title is too long for window.
           display space line 1 erase eos.
           compute wcb-num-rows = screen-num-rows - 3.
           move 41 to wcb-num-cols.
           display wcb high line 2 position 20 control "window-create".
           accept window-status from exception status.
           display "title is too long for window" line 7  position 8,
                   "window status = "             line 12 position 12,
                   window-status                  line 12 position 28,
                   "status should = 302"          line 13 position 12,
                   "press <return> to continue "  line 22 position 8.
           accept  x line 21 position 35 no beep.
           display wcb control "window-remove".

      *display window will not fit on screen.
           display space line 1 erase eos.
           move screen-num-rows to wcb-num-rows.
           move screen-num-cols to wcb-num-cols.
           display wcb high line 2 position 2 control "window-create".
           accept window-status from exception status.
           display "window will not fit on screen" line 10 position 26,
                   "window status = "              line 13 position 31,
                   window-status                   line 13 position 47,
                   "status should = 303"           line 14 position 31,
                   "press <return> to continue "   line 22 position 27
           accept  x line 22 position 55 no beep.
           display wcb control "window-remove".

      *display no windows active.
           display space line 1 erase eos.
           display wcb control "window-remove".
           accept window-status from exception status.
           display "no windows are active "      line 10 position 30,
                   "window status = "            line 13 position 31,
                   window-status                 line 13 position 47,
                   "status should = 304"         line 14 position 31,
                   "press <return> to continue " line 22 position 27
           accept  x line 22 position 55 no beep.
           display space line 1 erase eos.
           exit program.
           stop run.
       end program "winstat".
