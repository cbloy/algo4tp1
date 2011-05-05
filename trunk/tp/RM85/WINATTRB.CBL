       identification division.
       program-id. "winattrb".
      *
      * title:  winattrb.cbl
      *         rm/cobol-85 test window attributes
      *
      * the information contained herein is proprietary to ryan mcfarland
      * corporation, and provided for maintenance purposes only.
      * no other use of this material is authorized or permitted without
      * specific authorization, in writing, from ryan mcfarland corporation.
      *
      *author: rah.
      *date written: 2/23/90
      *program description.
      *this program test attributes displayed with windows.
      *
      *input-file - none
      *operator-response - enter selection number from terminal.
      *output-file - none
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:22  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\winattrb.cbv  $
      *
      * module history:
      *     $log:   u:\c85\dev\verify\vcs\winattrb.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:22   bill
      *    no change.
      *
      *       rev 1.2   13 sep 1990 16:27:00   donny
      *    create windows according to dimensions passed from wintest.
      *
      *       rev 1.1   29 aug 1990 14:53:46   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.0   15 aug 1990 14:43:20   bill
      *    initial revision.
      *
       environment division.
       data division.

       working-storage section.
       77  x                            pic x.

       copy "win.cpy".

       77  nested-index                 pic 99 binary value 0.
       01  nested-windows.
           03  nested-wcb               pic x(80) occurs 9.

       linkage section.
       01 screen-num-rows               pic 999.
       01 screen-num-cols               pic 999.

       screen section.
       01  menu-screen.
           05  blank screen.
           05  line 3 col 25 highlight
               "window status = ".
           05  line 3 col 43 highlight
               pic 999 using window-status.

       01  return-screen.
           05  line 21 col 17 highlight
               "press <return> for next test ".
           05  line 21 col 51  pic x using x.
      /
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
           move  " windows:  rm/cobol-85  window attributes "
                   to wcb-title.
           move 42 to wcb-title-length.
           move 0 to nested-index.

      *display beep window.
           add 1 to nested-index.
           move wcb to nested-wcb (nested-index).
           display nested-wcb (nested-index) beep line 2 position 2
                   control "window-create".
           accept window-status from exception status.
           display menu-screen.
           display "the border for this window    " line  5 position 25,
                   "should begin on line 1 col 1  " line  6 position 25,
                   "         beep = on            " line  7 position 25.
           display return-screen.
           accept  return-screen.

      *display high window.
           add 1 to nested-index.
           move wcb to nested-wcb (nested-index).
           display nested-wcb (nested-index) high line 2 position 2
                   control "window-create".
           accept window-status from exception status.
           display menu-screen.
           display "the border for this window    " line  5 position 25,
                   "should begin on line 1 col 1  " line  6 position 25,
                   "       border = high          " line  7 position 25.
           display return-screen.
           accept  return-screen.

      *display high reverse window.
           subtract 2 from wcb-num-rows.
           subtract 2 from wcb-num-cols.
           add 1 to nested-index.
           move wcb to nested-wcb (nested-index).
           display nested-wcb (nested-index) line 3 position 3
                   high reverse control "window-create".
           accept window-status from exception status.
           display menu-screen.
           display "the border for this window    " line  5 position 25,
                   "should begin on line 2 col 2  " line  6 position 25,
                   "   border = high/reverse      " line  7 position 25.
           display return-screen.
           accept  return-screen.

      *display high blink window.
           subtract 2 from wcb-num-rows.
           subtract 2 from wcb-num-cols.
           add 1 to nested-index.
           move wcb to nested-wcb (nested-index).
           display nested-wcb (nested-index) line 4 position 4
                   high blink control "window-create".
           accept window-status from exception status.
           display menu-screen.
           display "the border for this window    " line  5 position 24,
                   "should begin on line 3 col 3  " line  6 position 24,
                   "    border = high/blink       " line  7 position 24.
           display return-screen.
           accept  return-screen.

      *display high blink reverse window.
           subtract 2 from wcb-num-rows.
           subtract 2 from wcb-num-cols.
           add 1 to nested-index.
           move wcb to nested-wcb (nested-index).
           display nested-wcb (nested-index) line 5 position 5
                   high blink reverse control "window-create".
           accept window-status from exception status.
           display menu-screen.
           display "the border for this window    " line  5 position 23,
                   "should begin on line 4 col 4  " line  6 position 23,
                   "border = high/blink/reverse   " line  7 position 23.
           display return-screen.
           accept  return-screen.

      *display low window.
           subtract 2 from wcb-num-rows.
           subtract 2 from wcb-num-cols.
           add 1 to nested-index.
           move wcb to nested-wcb (nested-index).
           display nested-wcb (nested-index) low line 6 position 6
                   control "window-create".
           accept window-status from exception status.
           display menu-screen.
           display "the border for this window    " line  5 position 22,
                   "should begin on line 5 col 5  " line  6 position 22,
                   "        border = low          " line  7 position 22.
           display return-screen.
           accept  return-screen.

      *display low blink window.
           subtract 2 from wcb-num-rows.
           subtract 2 from wcb-num-cols.
           add 1 to nested-index.
           move wcb to nested-wcb (nested-index).
           display nested-wcb (nested-index) line 7 position 7
                   low blink control "window-create".
           accept window-status from exception status.
           display menu-screen.
           display "the border for this window    " line  5 position 21,
                   "should begin on line 6 col 6  " line  6 position 21,
                   "     border = low/blink       " line  7 position 21.
           display return-screen.
           accept  return-screen.

      *display low/blink window.
           subtract 2 from wcb-num-rows.
           subtract 2 from wcb-num-cols.
           add 1 to nested-index.
           move wcb to nested-wcb (nested-index).
           display nested-wcb (nested-index) line 8 position 8
                   low blink reverse control "window-create".
           accept window-status from exception status.
           display menu-screen.
           display "the border for this window    " line  5 position 20,
                   "should begin on line 7 col 7  " line  6 position 20,
                   " border = low/blink/reverse   " line  7 position 20.
           display return-screen.
           accept  return-screen.

      *display fill char window.
           subtract 2 from wcb-num-rows.
           subtract 2 from wcb-num-cols.
           move "y" to wcb-fill-switch.
           move "%" to wcb-fill-char.
           add 1 to nested-index.
           move wcb to nested-wcb (nested-index).
           display nested-wcb (nested-index) low line 9 position 9
                   control "window-create".
           accept window-status from exception status.
           display "the border for this window    " line  2 position 19,
                   "should begin on line 8 col 8  " line  3 position 19,
                   "        border = low          " line  4 position 19,
                   "     fill character is %      " line  5 position 19.
           display return-screen.
           accept  return-screen.

      *remove windows.
           perform until nested-index < 1
               display nested-wcb (nested-index) control "window-remove"
               subtract 1 from nested-index
           end-perform.
           exit program.
           stop run.
       end program "winattrb".
