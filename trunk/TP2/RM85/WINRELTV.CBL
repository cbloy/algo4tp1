       identification division.
       program-id. "winreltv".
      *
      * title:  winreltv.cbl
      *         rm/cobol-85 test windows relative options
      *
      * the information contained herein is proprietary to ryan mcfarland
      * corporation, and provided for maintenance purposes only.
      * no other use of this material is authorized or permitted without
      * specific authorization, in writing, from ryan mcfarland corporation.
      *
      *author: rah.
      *date written: 2/28/90
      *program description.
      *this program tests screen or window relative of overlayed windows.
      *
      *input-file - none
      *operator-response - enter selection number from terminal.
      *output-file - none
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:18  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\winreltv.cbv  $
      *
      * module history:
      *     $log:   u:\c85\dev\verify\vcs\winreltv.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:18   bill
      *    no change.
      *
      *       rev 1.3   13 sep 1990 16:29:44   donny
      *    create windows according to dimensions passed from wintest.
      *
      *       rev 1.2   29 aug 1990 14:54:40   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.1   23 aug 1990 10:36:16   randy
      *    use array of wcbs
      *    shift windows to the right to show line numbers, and change sizes.
      *
      *       rev 1.0   15 aug 1990 14:41:46   bill
      *    initial revision.
      *
       environment division.
       data division.

       working-storage section.
       77  x                            pic x.
       77  i                            pic 999.
       01  ruler                        pic x(150)
                                        value all "1234567890".

       01  relative-windows.
           02  relative-wcb             pic x(80) occurs 3 times.

       copy "win.cpy".

       linkage section.
       01 screen-num-rows               pic 999.
       01 screen-num-cols               pic 999.

       screen section.

       01  status-screen.
           05 blank screen.
           05  line 3 col 27 highlight
               "window status = ".
           05  line 3 col 45 highlight
               pic 999 using window-status.

       01  return-screen.
           05  line 21 col 15 highlight
                 "press <return> for next test ".
           05  line 21 col 53  pic x using x.
      /
       procedure division using screen-num-rows, screen-num-cols.
       begin-main.
           perform display-rulers.

      *define and create window.
           compute wcb-num-rows = screen-num-rows - 4.
           compute wcb-num-cols = screen-num-cols - 11.
           move "s" to wcb-location-reference.
           move "y" to wcb-border-switch.
           move  0  to wcb-border-type.
           move "*" to wcb-border-char.
           move "t" to wcb-title-location.
           move "c" to wcb-title-position.
           move  " windows:  rm/cobol-85  relative location "
               to wcb-title.
           move 42 to wcb-title-length.

      *display screen relative window.
           move wcb to relative-wcb (1).
           display relative-wcb (1) line 3 position 11
               control "window-create".
           accept window-status from exception status.
           display status-screen.
           display "the border for this window    " line  5 position 35,
                   "should begin on line 2 col 10 " line  6 position 35,
                   "       border = high          " line  7 position 35,
                   "      screen relative         " line  8 position 35.
           display ruler line 1, position 1 size, wcb-num-cols.
           display return-screen.
           accept  return-screen.

      *display window relative window.
           move "w" to wcb-location-reference.
           subtract 6 from wcb-num-rows.
           subtract 16 from wcb-num-cols.
           move wcb to relative-wcb (2).
           display relative-wcb (2) reverse line 6 position 16
                       control "window-create".
           accept window-status from exception status.
           display status-screen.
           display "the border for this window    " line  5 position 20,
                   "should begin on line 5 col 15 " line  6 position 20,
                   "       border = reverse       " line  7 position 20,
                   "       window relative        " line  8 position 20.
           display ruler line 1, position 1, size wcb-num-cols.
           display return-screen.
           accept  return-screen.

      *display screen relative window.
           move "s" to wcb-location-reference.
           compute wcb-num-rows = screen-num-rows - 12.
           compute wcb-num-cols = screen-num-cols - 22.
           move wcb to relative-wcb (3).
           display relative-wcb (3) low line 5 position 15
                       control "window-create".
           accept window-status from exception status.
           display status-screen.
           display "the border for this window    " line  5 position 25,
                   "should begin on line 4 col 14 " line  6 position 25,
                   "       border = low           " line  7 position 25,
                   "      screen relative         " line  8 position 25.
           display ruler line 1, position 1 size, wcb-num-cols.

      *remove windows.
           perform remove-window
               varying i from 3 by -1 until i is equal to 0.
           display space line 1 erase eos.
           exit program.
           stop run.

       display-rulers.
           display space erase.
           perform varying i from 1 by 1 until i > screen-num-rows
             display "line " line i position 1
                     i       position 0
           end-perform.
           display ruler line screen-num-rows position 1
                size screen-num-cols.

       remove-window.
           display return-screen.
           accept  return-screen.
           display relative-wcb (i) control "window-remove".
