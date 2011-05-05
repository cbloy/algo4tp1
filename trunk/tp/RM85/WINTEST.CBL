       identification division.
       program-id. "wintest".
      *
      * title:  wintest.cbl
      *         rm/cobol-85 windows test driver
      *
      * the information contained herein is proprietary to ryan mcfarland
      * corporation, and provided for maintenance purposes only.
      * no other use of this material is authorized or permitted without
      * specific authorization, in writing, from ryan mcfarland corporation.
      *
      *author: rah.
      *date written: 2/22/90
      *program description.
      *this is a main menu program that calls eight subroutines
      *based on the selection made.  it tests the windows.
      *
      *input-file - none
      *operator-response - enter selection number from terminal.
      *output-file - none
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:26  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\wintest.cbv  $
      *
      * module history:
      *     $log:   u:\c85\dev\verify\vcs\wintest.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:26   bill
      *    no change.
      *
      *       rev 1.4   13 sep 1990 16:15:56   donny
      *    prompt for screen size, create menu window accordingly, pass dimensio
      *      to other window programs.
      *
      *       rev 1.3   29 aug 1990 14:55:18   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.2   23 aug 1990 10:34:32   randy
      *    change menu for xenix exit program option to 6.
      *    erase screen before displaying menu.
      *
      *       rev 1.1   15 aug 1990 17:24:40   bill
      *    display error message if window manager not available.
      *
      *       rev 1.0   15 aug 1990 14:43:40   bill
      *    initial revision.
      *
       environment division.
       data division.

       working-storage section.
       01  title               pic x(60) value
           "rm/cobol-85 verification program - version 5.00".

       01  screen-num-rows              pic 999.
       01  screen-num-cols              pic 999.
       77  x                            pic x.

       01  escape-key   global          pic 99 value 0.
       01  select-num   global          pic 99 value 1.
       01  select-hold                  pic 99 value 0.

       copy "win.cpy".

       screen section.
       01  menu-screen.
           05 blank screen.
           05  line 3 col 10
               "window screen ".
           05  line 3 col 43
               "window status = ".
           05  line 3 col 61
               pic 999 using window-status.
           05  line 8 col 5
               " 1.  display window attributes ".
           05  line 9 col 5
               " 2.  display title positions ".
           05  line 10 col 5
               " 3.  display border types ".
           05  line 11 col 5
               " 4.  display relative positions".
           05  line 12 col 5
               " 5.  display status codes".
           05  line 13 col 5
               " 6.  display colors".
           05  line 14 col 5
               " 7.  exit program".
           05  line 18 col 15
               "select command number".
           05  line 19 col 15
               "   last command = ".
           05  line 21 col 15
               "<enter> goes to next test".

       01  select-screen  col 37 line 18  pic 9 using select-num.
       01  disp-last-scrn col 37 line 19  pic 9 from select-hold.

       01  screen-size-screen.
           05  blank screen.
           05  line 2 col 5
               "how many rows are there on your crt? ".
           05  pic 999 to screen-num-rows.
           05  line 3 col 5
               "how many columns are there on your crt? ".
           05  pic 999 to screen-num-cols.

       01  no-window-mgr-screen.
           05  blank screen.
           05  line 5 col 15
               "window manager is not available.".
           05  pic x to x.
      /
       procedure division.
       begin-main.
           display screen-size-screen.
           accept screen-size-screen.

       display-menu.
           display space erase.
      *define and create window.
           compute wcb-num-rows = screen-num-rows - 2.
           compute wcb-num-cols = screen-num-cols - 2.
           move "s" to wcb-location-reference.
           move "y" to wcb-border-switch.
           move  0  to wcb-border-type.
           move "*" to wcb-border-char.
           move "t" to wcb-title-location.
           move "c" to wcb-title-position.
           move  title to wcb-title.
           move 48 to wcb-title-length.
           display wcb high erase
               line 2 position 2 control "window-create".
           accept window-status from exception status.
           if window-status = 305
               display no-window-mgr-screen
               accept no-window-mgr-screen
               go end-program.

      *display the section menu for screen tests.
           display menu-screen.

      *display the number of the last test executed.
           display disp-last-scrn.
           display select-screen.
           accept select-screen.
       goto-para.
           display wcb control "window-remove".
           go to command-1, command-2, command-3, command-4, command-5,
               command-6, command-7,
               depending on select-num.
           go to display-menu.
       command-1.
           call "winattrb" using screen-num-rows, screen-num-cols.
           cancel "winattrb".
           go to check-status.
       command-2.
           call "wintitle" using screen-num-rows, screen-num-cols.
           cancel "wintitle".
           go to check-status.
       command-3.
           call "winbordr" using screen-num-rows, screen-num-cols.
           cancel "winbordr".
           go to check-status.
       command-4.
           call "winreltv" using screen-num-rows, screen-num-cols.
           cancel "winreltv".
           go to check-status.
       command-5.
           call "winstat" using screen-num-rows, screen-num-cols.
           cancel "winstat".
           go to check-status.
       command-6.
           call "wincolor" using screen-num-rows, screen-num-cols.
           cancel "wincolor".
           go to check-status.
       command-7.
           go to end-window-test.

      *determine how program terminated and appropriately stop
      *the run, begin again or rerun the last test.
       check-status.
           accept escape-key from escape key.
           evaluate escape-key
               when 13 move select-num to select-hold
                   add 1 to select-num
                   go to display-menu
               when 01 go to goto-para
               when 27 go to end-window-test
               when other move select-num to select-hold
                   add 1 to select-num
                   go to display-menu.

       end-window-test.
           display wcb control "window-remove".
       end-program.
           exit program.
           stop run.
