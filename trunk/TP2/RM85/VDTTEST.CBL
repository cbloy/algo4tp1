      * abcdefg
      * abcdefg
       identification division.
       program-id.  "vdttest".
      *
      * title: vdttest.cbl
      *     rm/cobol-85 dummy mcs module
      *
      * the information contained herein is proprietary to ryan mcfarland
      * corporation, and provided for maintenance purposes only.
      * no other use of this material is authorized or permitted without
      * specific authorization, in writing, from ryan mcfarland corporation.
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:28  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\vdttest.cbv  $
      *
      *    module history:
      *     $log:   u:\c85\dev\verify\vcs\vdttest.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:28   bill
      *    no change.
      *
      *       rev 1.3   29 aug 1990 14:53:02   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.2   15 aug 1990 10:59:46   bill
      *    change displayed text to mixed case.
      *
      *       rev 1.1   14 aug 1990 17:39:42   bill
      *    improve spelling of program name.
      *    remove color test from unix version.
      *
       environment division.
       configuration section.
       source-computer.  rmcobol-85.
       object-computer.  rmcobol-85.
      /
       data division.
       working-storage section.
       01  title        pic x(60) value
          "rm/cobol-85 verify terminal i/o functions - version 5.00".

       01  command-number          pic 99    value zero.
       01  last-command            pic 99    value zero.
       01  last-comedit            pic zz    value zero.
       01  exc-num                 pic 99    value zero.
       01  one-char                pic x     value space.
       01  r                       pic 99    value 24.
       01  c                       pic 999   value 79.
       01  c-display               pic zz9   value zero.
       01  o                       pic 999   value zero.
       01  n                       pic 999   value zero.
       01  name                    pic x(10) value spaces.
       01  line-no                 pic 99    value zero.
       01  line-no-edit            pic z9    value zero.
       01  date-fld                pic 9(6)  value zero.
       01  day-fld                 pic 9(5)  value zero.
       01  time-fld                pic 9(8)  value zero.
       01  operand                 pic s9(9)v9(9)  value zero.
       01  result                  pic -(9)9.9(9)  value zero.
       01  col-acc                 pic x(150)      value spaces.

       01  control-value.
           02                          pic x(4).
               88  intensity-high      value "high".
               88  intensity-low       value "low".
               88  intensity-off       value "off".
           02                          pic x(12).
               88  reverse-video       value ", reverse" false spaces.
               88  no-reverse-video    value ", no reverse".
           02                          pic x(10).
               88  blinking            value ", blink".
               88  no-blinking         value ", no blink".
           02  color-control.
               03                      pic x(11).
                   88  fcolor-equals   value ", fcolor = "
                                       false spaces.
               03  foreground-color    pic x(7).
                   88  foreground-is-black value "black".
               03                      pic x(11).
                   88  bcolor-equals   value ", bcolor = "
                                       false spaces.
               03  background-color    pic x(7).
               03                      pic x(11).
                   88  border-equals   value ", border = "
                                       false spaces.
               03  border-color        pic x(7).

       01  i                           pic 99 binary.

       01  color-table.
           02  pic x(7) value "black  ".
           02  pic x(7) value "blue   ".
           02  pic x(7) value "green  ".
           02  pic x(7) value "cyan   ".
           02  pic x(7) value "red    ".
           02  pic x(7) value "magenta".
           02  pic x(7) value "brown  ".
           02  pic x(7) value "white  ".
       01  redefines color-table.
           02  color-name              pic x(7) occurs 8.

       01  intensity                   pic x.
           88  low-intensity-desired   values "n", "n".
           88  high-intensity-desired  values "y", "y", space.
       01  reverse-vid                 pic x.
           88  reverse-video-desired   values "y", "y".
           88  reverse-video-undesired values "n", "n", space.

       01  standard-control.
           02  pic x(13) value "border=black,".
           02  substandard-control     pic x(47) value
                   "low,fcolor=white,bcolor=black".

       01  main-menu-values.
           02  pic x(51) value " 1.  accept with and without prompt".
           02  pic x(51) value
               " 2.  accept and display value with and without beep".
           02  pic x(51) value " 3.  accept with and without tab".
           02  pic x(51) value
               " 4.  accept and display value with default values".
           02  pic x(51) value " 5.  check screen dimensions".
           02  pic x(51) value " 6.  accept with secure".
           02  pic x(51) value
               " 7.  accept and display value with reverse".
           02  pic x(51) value " 8.  accept date and time".
           02  pic x(51) value " 9.  accept high and low intensity".
           02  pic x(51) value "10.  accept with blink and reverse".
           02  pic x(51) value "11.  accept with on exception clause".
           02  pic x(51) value "12.  accept with update".
           02  pic x(51) value "13.  erase to end of line".
           02  pic x(51) value "14.  erase to end of screen".
          02  pic x(51) value "15.  change colors".
          02  pic x(51) value "16.  exit program".
       01  redefines main-menu-values.
          02  main-menu-line          pic x(51) occurs 16.
      /
      *
      *
       procedure division.
       enter-command.
           display title line 1 erase control standard-control.
           perform varying i from 1 by 1 until i > 16
               add i, 2 giving line-no
               display main-menu-line (i)
                   line line-no, position 3, control standard-control
           end-perform.
           display "enter command number" low, position 12, line 20
           display "last command = " low, position 15,
                   command-number, convert, low.
           display "<enter> goes to next test." low, position 12.
           move command-number to last-command.
      /

       accept-command-number.
           accept command-number line 20 position 34
                   no beep convert tab on exception exc-num
               display spaces size 2 line 20 position 34 beep
               go to accept-command-number.
           if command-number equal zero
               add 1, last-command giving last-command, command-number.

       vector-on-command-number.
           if command-number > 0 and < 16
               display main-menu-line (command-number),
                       line 1, position 10, erase, high.
           go to test-1-para, test-2-para, test-3-para, test-4-para,
                 test-5-para, test-6-para, test-7-para, test-8-para,
                 test-9-para, test-10-para, test-11-para, test-12-para,
            test-13-para, test-14-para, test-15-para, exit-para
                                       depending on command-number.
           display spaces size 2 line 20 position 34 beep.
           go to accept-command-number.

       return-to-menu.
           move space to one-char.

           display 'press "enter" to return to menu.  < >'
                       line 21 position 26 control substandard-control
                   'enter an "r" to repeat this test.'
                       line 22 position 26 control substandard-control.

           accept one-char line 21 position 61 tab no beep
                        control substandard-control.

           if one-char = space go to enter-command.
           if one-char not equal "r" and "r" go to return-to-menu.
           go vector-on-command-number.
      /
      *
       test-1-para.
           display "a.  accept with no prompt" line 3.
           display "    enter your name:  " line 4.
           accept name line 4 position 23 no beep tab.
           display name line 5 position 23.
      *
           display "b.  accept with default prompt character" line 7.
           display "    enter your name:  " line 8.
           accept name line 8 position 23 no beep tab prompt.
           display name line 9 position 23.
      *
           display
              'c.  accept with user-specified prompt character: "h"'
                      line 11.
           display "    enter your name:  " line 12.
           accept name line 12 position 23 no beep tab prompt "h"
           display name line 13 position 23.
      *
           go return-to-menu.
      /
      *
       test-2-para.
           display 'a. accept with beep (press "enter")'  line 3.
           accept one-char position 0 no beep.
           display "enter your name (you should have heard a beep):  "
                       line 4 position 4.
           accept name position 0 tab prompt.
      *
           display 'b. accept without beep (press "enter")' line 6.
           accept one-char position 0 no beep.
           display
                "enter your name (you should not have heard a beep):  "
                       line 7 position 4.
           accept name position 0 tab prompt no beep.
      *
           display 'c. display with beep (press "enter")'
                       line 9.
           accept one-char position 0 no beep.
           display "this display should have beeped."
                       line 10 position 4 beep.
      *
           display 'd. display without beep (press "enter")'
                       line 12.
           accept one-char position 0 no beep.
           display "this display should not have beeped."
                       line 13 position 4.
      *
           go return-to-menu.
      /
      *
       test-3-para.
           display "automatic tab is where input is terminated when"
              line 3 position 5.
           display "end of the input field is encountered."
              line 4 position 5.
           display
               "enter a string of 10 chars.  an automatic tab should"
                       line 6 position 5.
           display "occur after the last character has been typed."
                       line 7 position 5.
           accept name line 10 position 15 prompt no beep echo.
           display name line 11 position 15.
      *
           display
               "enter a string of 10 chars.  an automatic tab should"
                       line 13 position 5.
           display "not occur after the last character is typed."
                       line 14 position 5.
           display "you must press enter to exit from the field"
           line 15 position 5.
           accept name line 17 position 15
                       no beep prompt tab echo.
           display name line 18 position 15.
      *
           go return-to-menu.
      /
      *
       test-4-para.
           move 4 to line-no.
           subtract 4 from r giving o.
           subtract 3 from c giving n.
           perform display-line-no o times.
           display all "1234567890" line 3 size 30.
           display "this line should start on line 4 position 1."
           display "this line should start on line 5 position 15."
                       position 15.
           display "this line should start on line 6 position 1."
                       line 6 position 1.
           display "this line should start on line 7 position 1."
                       line 7.
           display "this line should appear on line 24 position 10."
                       line 24 position 10.
           display
           'this should be line 12  ****press "enter" to continue: '
                       line 12.
           accept one-char position 0 no beep tab.
      *
           display "this line should appear on line 13 position 5."
                       position 5.
           display "accept and display default test (position 0) "
                       line 15 position 5.
           display "enter field " position 0.
           accept name position 0 prompt no beep tab.
           display " accept again " position 0.
           accept name position 0 prompt no beep tab.
           display " one more time " position 0.
           accept name position 0 prompt no beep tab.
      *
           go return-to-menu.
      /
      *
       test-5-para.
           display "how many rows are there on your crt? "
                       line 3 position 5.
           accept r position 0 tab no beep convert update.
           if r = 0 move 24 to r.
           display "how many columns? "  line 5 position 5.
           accept c position 0 tab no beep convert update.
           if c = 0 move 79 to c.
           display "you should see an asterisk in each corner"
                   line 7 position 5.
           display "of screen (after pressing ""enter"")"
                   line 8 position 5.
           accept one-char position 0 no beep.
           display "*" line 1 position 1 erase
              "*" line r position 1
                   "*" line 1 position c
                   "*" line r position c.
      *
           display 'enter "0" to display line nos, otherwise "enter"'
               line 3 position 5.
           accept one-char position 0 no beep tab.
           if one-char not = "0" go to col-display.
           move 1 to line-no.
           subtract 2 from c giving n.
           perform display-line-no r times.
      *
       col-display.
           display 'enter "1" to display all "1234567890", '
                           line 5 position 5
                   'otherwise "enter"'
           accept one-char position 0 no beep tab.
           if one-char not = "1" go to enter-line.
           display all "1234567890" size c line 1 position 1.
           display all "1234567890" size c line r position 1.
      *
       enter-line.
           display "enter an " line 7 position 5
                   c, convert
                   " character field to test accept limits"
                       position 0.
           accept col-acc line 9 position 1 size c no beep prompt tab.
           display col-acc line 10 size c reverse.
      *
           go return-to-menu.
      *
       display-line-no.
           display line-no, line line-no, position 1, convert,
                   line-no, line line-no, position n, convert.
           add 1 to line-no.
      /
      *
       test-6-para.
           display "enter your code name: " line 5 position 5.
           accept name position 0 prompt no beep tab off.
           display "  your code name was: " line 7 position 5.
           display name position 0.
      *
           go return-to-menu.
      /
      *
       test-7-para.
           display "display with extra spaces    "
                       line 5 position 5 reverse.
           display "display high and reverse     "  position 5
                reverse high line 7.
           display "display low and reverse      " position 5
                line 9 low reverse.
      *
           display "accept in reverse low:            "
                line 11 position 5.
           accept name position 0 reverse low  no beep tab.
           display "accept in reverse high:           "
                line 13 position 5.
           accept name position 0 reverse high no beep tab.
      *
           display "accept with prompt character ""-""  "
                line 15 position 5.
           accept name position 0 prompt "-"
                reverse no beep tab.
      *
           display "accept with default prompt:       "
                line 17 position 5.
           accept name position 0 prompt reverse no beep tab.
      *
           go return-to-menu.
      /
      *
       test-8-para.
           accept date-fld from date.
           accept day-fld from day.
           accept time-fld from time.
           display " " line 2.
           display "    today's date is   (yymmdd): ", date-fld.
           display " ".
           display "    the julian form is (yyddd): ", day-fld.
           display " ".
           display "    the time is     (hhmmsscc): ", time-fld.
      *
           go return-to-menu.
      /
      *
       test-9-para.
           display "enter numeric data for operand" line 3
                    position 1 high.
           display "display low, accept low" line 4 position 50.
           display "display low, accept high" line 6 position 50.
           display "with default positioning" line 8 position 50.
      *
           display "operand   = " line 4 low.
           accept operand line 4 position 13 size 9
                                       tab no beep prompt convert low.
           move operand to result.
           display result line 4 position 13 low.
      *
           display "operand   = " line 6 low.
           accept operand line 6 position 13 size 9
                                       tab no beep prompt convert high.
           move operand to result.
           display result line 6 position 13 low.
      *
           display "operand   = " line 8 low.
           accept operand line 8 position 13 size 9
                                       tab no beep prompt convert high.
           move operand to result.
           display result position 0 low.
      *
           go return-to-menu.
      /
      *
       test-10-para.
           display " display with extra spaces    "
                       line 3 position 5 blink reverse.
           display "blink and reverse video" line 3 position 43.
           display " display with extra spaces    "
                             line 5 position 5 low blink.
           display "low and blink" line 5 position 43.
           display " display with extra spaces    "
                           line 7 position 5 high blink.
           display "high and blink" line 7 position 43.
           display " display with extra spaces    "
                 line 9 position 5 high blink reverse.
           display "high and blink and reverse" line 9
                 position 43.
           display " we are testing all the "
                       line 11 position 5 low reverse.
           display "low and reverse video" line 11 position 43.
           display " display options "
                       line 13 position 5 low blink reverse.
           display "low and reverse and blink" line 13 position 43.
      *
           display "accept with blink:                     "
                   line 16 position 5.
           accept name position 0 no beep tab blink.
      *
           display "low and reverse with prompt character ""-""  "
               line 18 position 5.
           accept
               name position 0 no beep tab prompt "-" low reverse.
      *
           display "blink and reverse with default prompt:     "
               line 20  position 5.
           accept name position 0 no beep tab prompt blink reverse.
      *
           go return-to-menu.
      /
      *
       test-11-para.
           move zeros to operand, result.
           display
               "enter a number and terminate with field termination key"
                       line 4 position 5.
           display
           "field termination keys will show nonzero termination code"
                    line 5 position 5.
           display
               '(nonnumeric entry will show convert error "98")'
                       line 6 position 5.
           display spaces size 40 line 12 position 20.
           accept operand line 8 position 25 size 9
                       prompt convert no beep tab
                       on exception exc-num
                               display
                                       "*** exception branch taken ***"
                                       line 12 position 29 beep.
           move operand to result.
           display "as it was accepted:" line 9 position 5.
           display result line 9 position 24.
           display "termination code <  >" line 13 position 33.
           display exc-num line 13 position 51, convert.
      *
           go return-to-menu.
      /
      *
       test-12-para.
           display "update initial field, then <enter>"
                       line 3 position 5.
           move "init-field" to name.
           accept name line 5 position 20 update no beep tab.
           display
              "update current field, then <erase-right-key (alt-e)>"
                       line 7 position 5
           accept name line 9 position 20 update no beep tab.
           display "(trailing characters should be blank)"
                       line 11 position 5.
           display name line 13 position 20.
      *
           go return-to-menu.
      /
      *
       test-13-para.
           display "(screen dimensions taken from test 5.)"
                       line 3 position 15.
      *
           display "accept with erase to end-of-line."
                           line 5 position 5.
           display "this should erase from position 5 to end of line"
                   line 6 position 5.
           display all "1234567890" size c line 9.
           display all "1234567890" size c line 10.
           display "  this line must remain unchanged.  "
                       line 10 position 3.
           display "press ""enter"" to enter your name."
                       line 7 position 5.
           accept one-char position 0 no beep.
           accept name line 9 position 5 prompt
                                       no beep erase eol tab.
      *
           display
              "display with erase to end-of-line." line 12 position 5.
           display all "1234567890" size c line 16.
           display all "1234567890" size c line 17.
           display "  this line must remain unchanged.  "
                       line 17 position 3.
           display
           "press ""enter"" to erase from position 5 to end of line."
                       line 14 position 5.
           accept one-char position 0 no beep.
           display name line 16 position 5 erase eol.
      *
           go return-to-menu.
      /
      *
       test-14-para.
           subtract 1 from c.
           display "(screen dimensions taken from test 5.)"
                       line 3 position 15.
      *
           display
            "accept with erase to end-of-screen." line 5 position 5.
           display "this should erase to end-of-screen" line 6
                 position 5.
           display all "1234567890" size c line 9.
           move 10 to n.
           perform this-line.
           display "press ""enter"" to enter your name"
                       line 7 position 5.
           accept one-char position 0 no beep.
           accept name line 9 position 5
                                       no beep prompt erase eos tab.
      *
           display "display with erase to end-of-screen"
                       line 12 position 5.
           display all "1234567890" size c line 16.
           move 17 to n.
           perform this-line.
           display "press ""enter"" to display erase to end-of-screen"
                       line 14 position 5.
           accept one-char position 0 no beep.
           display name line 16 position 5 erase eos.
      *
           add 1 to c.
           go return-to-menu.
      *
       this-line.
           display all "1234567890" size c line n.
           display " this line must be erased. " line n position 3.
           add 1 to n.
           if n not greater than r go to this-line.
      /
      *
       test-15-para.
           display "foreground" line 13 position 20 high.
           display "valid color          if              if"
                       line 14 position 2.
           display "   names        high intensity     reverse"
                       line 15 position 2.

           move spaces to control-value.
           set fcolor-equals, bcolor-equals, border-equals to true.
           move "white" to background-color.
           perform varying i from 1 by 1 until i > 8
               add i, 15 giving line-no
               move color-name (i) to foreground-color, border-color
               set intensity-low to true
               display color-name (i) line line-no position 2
                   size 10 control control-value
               set reverse-video to true
               display color-name (i) line line-no position 37
                   size 10 control control-value
               set no-reverse-video to true
               set intensity-high to true
               display color-name (i) line line-no position 18
                   size 16 control control-value
               move "black" to background-color
           end-perform.

           move spaces to control-value.
           set fcolor-equals, bcolor-equals, border-equals to true.
           move space to intensity.
           perform until intensity-high or intensity-low
               display "high intensity (default y) y/n?: "
                       line 3, control standard-control
               accept intensity
                       position 0 tab update control standard-control
               if low-intensity-desired set intensity-low to true
               else if high-intensity-desired set intensity-high to true
                    end-if
               end-if
           end-perform.

           move space to reverse-vid.
           perform until reverse-video or no-reverse-video
               display "reverse (default n)  y/n?: "
                       line 4, control standard-control
               accept reverse-vid
                       position 0 tab update control standard-control
               if reverse-video-desired set reverse-video to true
               else if reverse-video-undesired
                       set no-reverse-video to true
                    end-if
               end-if
           end-perform.

           move "black" to background-color, border-color.
           move "white" to foreground-color.
           display "enter foreground color name (default white): "
                   control standard-control.
           accept foreground-color position 0 tab update
                   control standard-control.
           display "enter background color name (default black): "
                   control standard-control.
           accept background-color position 0 tab update
                   control standard-control.
           display "enter border color name (default black): "
                   control standard-control.
           accept border-color position 0 tab update
                   control standard-control.
           if foreground-color = spaces move "white" to foreground-color
           end-if.
           if background-color = spaces move "black" to background-color
           end-if.
           if border-color = spaces move "black" to border-color
           end-if.

           display control-value line 1 erase control control-value.
      *
           go return-to-menu.
      *
      *
       exit-para.
           exit program.
           stop run.
