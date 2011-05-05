       identification division.
       program-id. "wincolor".
      *
      * title:  wincolor.cbl
      *         rm/cobol-85 test window colors
      *
      * the information contained herein is proprietary to ryan mcfarland
      * corporation, and provided for maintenance purposes only.
      * no other use of this material is authorized or permitted without
      * specific authorization, in writing, from ryan mcfarland corporation.
      *
      *author: rah.
      *date written: 3/01/90
      *program description.
      *this program test color attributes displayed with windows.
      *
      *input-file - none
      *operator-response - enter selection number from terminal.
      *output-file - none
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:16  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\wincolor.cbv  $
      *
      * module history:
      *     $log:   u:\c85\dev\verify\vcs\wincolor.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:16   bill
      *    no change.
      *
      *       rev 1.2   13 sep 1990 16:29:14   donny
      *    create windows according to dimensions passed from wintest.
      *
      *       rev 1.1   29 aug 1990 14:54:22   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.0   15 aug 1990 14:41:58   bill
      *    initial revision.
      *
       environment division.
       data division.

       working-storage section.
       77  x                            pic x.
       77  s                            pic 99 binary value 0.
       77  a                            pic 99 binary value 0.
       77  l                            pic 999.
       77  p                            pic 999.

       copy "win.cpy".

       01  color-table.
          03  filler                    pic x(109) value
              "fcolor=blue   ,bcolor=green  ".
          03  filler                    pic x(109) value
              "fcolor=green  ,bcolor=cyan   ".
          03  filler                    pic x(109) value
              "fcolor=cyan   ,bcolor=red    ".
          03  filler                    pic x(109) value
              "fcolor=red    ,bcolor=magenta".
          03  filler                    pic x(109) value
              "fcolor=magenta,bcolor=brown  ".
          03  filler                    pic x(109) value
              "fcolor=brown  ,bcolor=white  ".
          03  filler                    pic x(109) value
              "fcolor=white  ,bcolor=black  ".
          03  filler                    pic x(109) value
              "fcolor=black  ,bcolor=blue   ".
       01  color-tab redefines color-table.
          03  color-values occurs 8 times.
              05  color-control         pic x(29).
              05  color-wcb             pic x(80).

       01  attrb-table.
           03  filler                   pic x(8)  value "high   ,".
           03  filler                   pic x(8)  value "low    ,".
           03  filler                   pic x(8)  value "off    ,".
           03  filler                   pic x(8)  value "reverse,".
           03  filler                   pic x(8)  value "blink  ,".
           03  filler                   pic x(8)  value "beep   ,".
           03  filler                   pic x(8)  value "erase  ,".
       01  attrb-tab redefines attrb-table.
           03  attrb                    occurs 7 times pic x(8).

       01  win-control                  pic x(80) value spaces.

       linkage section.
       01 screen-num-rows               pic 999.
       01 screen-num-cols               pic 999.

       screen section.
       01  status-screen.
           05 blank screen.
           05  line 2 col 2 highlight
               " status = ".
           05  line 2 col 14 highlight
               pic 999 using window-status.
      /
       procedure division using screen-num-rows, screen-num-cols.
       begin-main.
           display space line 1 erase eos.
      *define and create window.
           compute wcb-num-rows = ( screen-num-rows - 4 ) / 2.
           compute wcb-num-cols = ( screen-num-cols - 8 ) / 4.
           move "s" to wcb-location-reference.
           move "y" to wcb-border-switch.
           move  0  to wcb-border-type.
           move "*" to wcb-border-char.
           move "t" to wcb-title-location.
           move "c" to wcb-title-position.
           move  " window color "
                   to wcb-title.
           move 14 to wcb-title-length.

      *display 8 window color combinations with 7 attributes.
           perform varying a from 1 by 1 until a > 7
               move 2 to l, p
               perform varying s from 1 by 1 until s > 8
                   perform display-windows
                   compute p = p + wcb-num-cols + 2
                   if s = 4 then
                       compute l = l + wcb-num-rows + 2
                       move 2 to p
                   end-if
               end-perform
               accept x line wcb-num-rows position wcb-num-cols
                       no beep
               perform remove-windows varying s
                       from 8 by -1 until s < 1
           end-perform.

      *display same window with 7 different attributes.
           move 1 to s.
           move spaces to win-control.
           move 2 to l, p.
           move 1 to a.
           perform varying s from 1 by 1 until s > 7
               perform display-windows
               add 1 to a
               compute p = p + wcb-num-cols + 2
               if s = 4 then
                   compute l = l + wcb-num-rows + 2
                   move 2 to p
               end-if
           end-perform.
           accept x line wcb-num-rows position wcb-num-cols no beep
           perform remove-windows varying s
                   from 7 by -1 until s < 1.
           exit program.
           stop run.
      /
      *display window colors.
       display-windows.
           string "window-create,", attrb(a), color-control(s)
                   delimited by size into win-control.
           move wcb to color-wcb (s).
           display color-wcb (s) line l position p
                   control win-control.
           accept window-status from exception status.
           display status-screen.
           display color-control(s) (1:14)  line 4 position 3,
                   color-control(s) (16:14) line 5 position 3,
                   "attrib="                line 6 position 3,
                   attrb(a) (1:7)           line 6 position 0.

      *display window attributes.
       display-attribs.
           string "window-create,", attrb(a),
                  "fcolor=red,", "bcolor=blue"
                  delimited by size into win-control.
           move wcb to color-wcb (a).
           display color-wcb (a) line l position p
                   control win-control.
           accept window-status from exception status.
           display status-screen.
           display "fcolor=red"             line 4 position 3,
                   "bcolor=blue"            line 5 position 3,
                   "attrib="                line 6 position 3,
                   attrb(a) (1:7)           line 6 position 0.

      *remove windows.
       remove-windows.
           display color-wcb (s) control "window-remove".
