       identification division.
       program-id. "winborder".
      *
      * title:  winbordr.cbl
      *         rm/cobol-85 test window borders
      *
      * the information contained herein is proprietary to ryan mcfarland
      * corporation, and provided for maintenance purposes only.
      * no other use of this material is authorized or permitted without
      * specific authorization, in writing, from ryan mcfarland corporation.
      *
      *author: rah.
      *date written: 2/27/90
      *program description.
      *this program tests the 9 different border types.
      *
      *input-file - none
      *operator-response - enter selection number from terminal.
      *output-file - none
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:22  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\winbordr.cbv  $
      *
      *    module history:
      *     $log:   u:\c85\dev\verify\vcs\winbordr.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:22   bill
      *    no change.
      *
      *       rev 1.2   13 sep 1990 16:28:40   donny
      *    create windows according to dimensions passed from wintest.
      *
      *       rev 1.1   29 aug 1990 14:54:06   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.0   15 aug 1990 14:43:02   bill
      *    initial revision.
      *
       environment division.
       data division.

       working-storage section.
       77  x                            pic x.
       77  l                            pic 99.
       77  p                            pic 99.
       77  i                            pic 99 binary.

       01  border-windows.
           03  border-wcb               pic x(80) occurs 4.

       copy "win.cpy".

       linkage section.
       01 screen-num-rows               pic 999.
       01 screen-num-cols               pic 999.

       screen section.
       01  menu-screen.
           05  blank screen.
           05  line 3 col 21 highlight
               "window status = ".
           05  line 3 col 38 highlight
               pic 999 using window-status.

       01  return-screen.
           05  line 9  col 17 highlight
               "press <return> for next test ".
           05  line 9  col 54  pic x using x.
      /
       procedure division using screen-num-rows, screen-num-cols.
       begin-main.
           display space line 1 position 1 erase eos.
      *define and create window.
           move 11 to wcb-num-rows.
           move 60 to wcb-num-cols.
           move "s" to wcb-location-reference.
           move "y" to wcb-border-switch.
           move "t" to wcb-title-location.
           move "c" to wcb-title-position.
           move  " windows:  rm/cobol-85  border types "
                   to wcb-title.
           move 37 to wcb-title-length.
           move  9 to l.
           move 10 to p.
           move 0 to wcb-border-type.
           perform display-borders varying i from 1 by 1
                   until i > 4.
           perform remove-windows varying i from 4 by -1
                   until i < 1.
           exit program.
           stop run.

      *display window wcb-border-type 0 to 4.
       display-borders.
           move wcb to border-wcb (i).
           display border-wcb (i) line l position p
                   control "window-create".
           accept window-status from exception status.
           display menu-screen.
           display "the border type for this window" line 5 position 13,
                   " = "                             line 5 position 0,
                   wcb-border-type                   line 5 position 0.
           display return-screen.
           accept  return-screen.
           add 1 to wcb-border-type.

      *remove windows.
       remove-windows.
           display border-wcb (i) control "window-remove".
