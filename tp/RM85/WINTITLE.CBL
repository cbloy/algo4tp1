       identification division.
       program-id. "wintitle".
      *
      * title:  wintitle.cbl
      *         rm/cobol-85 test windows titles
      *
      * the information contained herein is proprietary to ryan mcfarland
      * corporation, and provided for maintenance purposes only.
      * no other use of this material is authorized or permitted without
      * specific authorization, in writing, from ryan mcfarland corporation.
      *
      *author: rah.
      *date written: 2/23/90
      *program description.
      *this program tests window titles fill characters.
      *
      *input-file - none
      *operator-response - enter selection number from terminal.
      *output-file - none
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:08  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\wintitle.cbv  $
      *
      * module history:
      *     $log:   u:\c85\dev\verify\vcs\wintitle.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:08   bill
      *    no change.
      *
      *       rev 1.2   13 sep 1990 16:30:42   donny
      *    create windows according to dimensions passed from wintest.
      *
      *       rev 1.1   29 aug 1990 14:55:38   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.0   15 aug 1990 14:41:16   bill
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
      *define and create window.
           compute wcb-num-rows = screen-num-rows - 3.
           compute wcb-num-cols = screen-num-cols - 2.
           move "s" to wcb-location-reference.
           move "y" to wcb-border-switch.
           move  0  to wcb-border-type.
           move "*" to wcb-border-char.
           move "t" to wcb-title-location.
           move "c" to wcb-title-position.
           move  " windows:  rm/cobol-85  window titles "
                   to wcb-title.
           move 38 to wcb-title-length.

      *display top center title.
           display wcb high erase  line  2 position  2
                                   control "window-create".
           accept window-status from exception status.
           display "window status = "              line  3 position 58,
                   window-status                   line  3 position 74,
                   "the title for this window    " line 12 position 25,
                   "should begin top center      " line 13 position 25,
                   "press <return> to continue "
                        line wcb-num-rows position 25.
           accept  x line wcb-num-rows position 55 no beep.
           display wcb control "window-remove".

      *display top left title.
           move "l" to wcb-title-position.
           display wcb high line 2 position 2 control "window-create".
           accept window-status from exception status.
           display "window status = "              line  3 position 58,
                   window-status                   line  3 position 74,
                   "the title for this window    " line 12 position 25,
                   "should begin top left        " line 13 position 25,
                   "press <return> to continue "
                        line wcb-num-rows position 25.
           accept  x line wcb-num-rows position 55 no beep.
           display wcb control "window-remove".

      *display top right title.
           move "r" to wcb-title-position.
           display wcb high line 2 position 2 control "window-create".
           accept window-status from exception status.
           display "window status = "              line  3 position 58,
                   window-status                   line  3 position 74,
                   "the title for this window    " line 12 position 25,
                   "should begin top right       " line 13 position 25,
                   "press <return> to continue "
                        line wcb-num-rows position 25.
           accept  x line wcb-num-rows position 55 no beep.
           display wcb control "window-remove".

      *display bottom center title.
           move "b" to wcb-title-location.
           move "c" to wcb-title-position.
           display wcb high line 2 position 2 control "window-create".
           accept window-status from exception status.
           display "window status = "              line  3 position 58,
                   window-status                   line  3 position 74,
                   "the title for this window    " line 12 position 25,
                   "should begin bottom center   " line 13 position 25,
                   "press <return> to continue "
                        line wcb-num-rows position 25.
           accept  x line wcb-num-rows position 55 no beep.
           display wcb control "window-remove".

      *display bottom left title.
           move "l" to wcb-title-position.
           display wcb high line 2 position 2 control "window-create".
           accept window-status from exception status.
           display "window status = "              line  3 position 58,
                   window-status                   line  3 position 74,
                   "the title for this window  "   line 12 position 25,
                   "should begin bottom left   "   line 13 position 25,
                   "press <return> to continue "
                        line wcb-num-rows position 25.
           accept  x line wcb-num-rows position 55 no beep.
           display wcb control "window-remove".

      *display bottom right title.
           move "r" to wcb-title-position.
           display wcb high line 2 position 2 control "window-create".
           accept window-status from exception status.
           display "window status = "              line  3 position 58,
                   window-status                   line  3 position 74,
                   "the title for this window  "   line 12 position 25,
                   "should begin bottom right  "   line 13 position 25,
                   "press <return> to continue "
                        line wcb-num-rows position 25.
           accept  x line wcb-num-rows position 55 no beep.
           display wcb control "window-remove".

      *display no title.
           move " " to wcb-title-location.
           move " " to wcb-title-position.
           move  0  to wcb-title-length.
           move spaces to wcb-title.
           display wcb high line 2 position 2 control "window-create".
           accept window-status from exception status.
           display "window status = "              line  3 position 58,
                   window-status                   line  3 position 74,
                   "there should be no title   "   line 12 position 25,
                   "    for this window        "   line 13 position 25,
                   "press <return> to continue "
                        line wcb-num-rows position 25.
           accept  x line wcb-num-rows position 55 no beep.
           display wcb control "window-remove".

      *display fill char.
           move "t" to wcb-title-location.
           move "c" to wcb-title-position.
           move  0  to wcb-title-length.
           move  " windows:  rm/cobol-85  fill characters "
                   to wcb-title.
           move 42 to wcb-title-length.
           move "+" to wcb-fill-char.
           move "y" to wcb-fill-switch.
           display wcb high line 2 position 2 control "window-create".
           accept window-status from exception status.
           display "window status = "              line  3 position 58,
                   window-status                   line  3 position 74,
                   "  the title for this window  " line 12 position 25,
                   "  should begin top center    " line 13 position 25,
                   "     fill character is +     " line 14 position 25,
                   "press <return> to continue "
                        line wcb-num-rows position 25.
           accept  x line wcb-num-rows position 55 no beep.
           display wcb control "window-remove".

           exit program.
           stop run.
       end program "wintitle".
