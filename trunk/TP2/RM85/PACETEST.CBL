       identification division.
       program-id.  "pacetest".
      *
      *    title:  pacetest.cbl
      *        rm/cobol-85 multi-user test
      *
      * the information contained herein is proprietary to ryan mcfarland
      *    corporation, and provided for maintenance purposes only.
      *    no other use of this material is authorized or permitted without
      *    specific authorization, in writing, from ryan mcfarland corporation.
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:12  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\pacetest.cbv  $
      *
      *    module history:
      *     $log:   u:\c85\dev\verify\vcs\pacetest.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:12   bill
      *    no change.
      *
      *       rev 1.2   29 aug 1990 14:52:08   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.1   14 aug 1990 16:10:10   bill
      *    change filenames to lower case for unix.
      *
       environment division.
       configuration section.
       source-computer.  rmc.
       object-computer.  rmc.
      *
       input-output section.
       file-control.
           select index-file
               assign to random "pinxfl"
               organization indexed
               access mode dynamic
               record key is i-key
               file status is i-status.

           select reg-file
               assign to random "pinx"
               organization indexed
               access mode random
               record key is r-key
               file status is r-status.
      *
       data division.
      *
       file section.

       fd index-file
           label record is standard.
       01 index-record.
           03 i-key    pic x(5).
           03 i-data   pic x(20).

       fd reg-file
           label record is standard.
       01 reg-record.
           03 r-key    pic x.
           03 r-num    pic 9(5).
      *
       working-storage section.

       77 i-status     pic xx.
       77 r-status     pic xx.
       77 reckount     pic 99999.
       77 keykount     pic 99999.
       77 kount        pic 99999.
       77 dumnum       pic 99.
       77 menunum      pic 99.
       77 term-id      pic 99.
       01 ws-record.
           03 ws-key   pic 9(5).
           03 filler   pic x(16)  value  "   terminal id= ".
           03 ws-id    pic 99.
           03 filler   pic xx value spaces.
      *
      *
       procedure division.
      *
       begining.
           move 0 to menunum.
       menu.
           display "****    pacetest  menu    ****"
                   line 3 position 24 erase.
           if menunum less than 1
              display
              "1.  initialization (only one of you run this)."
                   line 5 position 14.
           if menunum less than 2
              display "2.  simultaneous extension (both of you)."
                   line 7 position 14.
           if menunum less than 3
              display
              "3.  verification listing (only one need run this)."
                   line 9 position 14.
           display "4.  start ""pacetest"" all over."
                   line 11 position 14.
           display "5.  exit ""pacetest""." line 13 position 14.
           display "< >  press the number of the desired section."
                   line 16 position 13.
           accept dumnum line 16 position 14 size 1 convert no beep.
           if dumnum greater than 5 go to menu.
           if dumnum equal to menunum or
              dumnum less than menunum go to menu.
           move dumnum to menunum.
           go to initializ,
                 simextend,
                 veralist,
                 begining,
                 exiting   depending on menunum.
      *
      *
       initializ.
           display "*    pacetest  initialization    *"
                   line 3 position 23 erase.
           display
           "now initializing ""pinxfl"" and ""pinx"" files."
           line 8 position 20.
           open output reg-file.
           move 0 to r-num.
           move "x" to r-key.
           write reg-record.
           open output index-file.
           close index-file.
           close reg-file.
           display
           "initialization complete, press ""return"" < >."
           line 13 position 19.
           accept dumnum position 60 line 13 size 1 no beep.
           go to menu.
      *
      *
       simextend.
           move zero to kount, term-id.
           display "**    pacetest  simultaneous  extension    **"
                   line 3 position 18 erase.
           display
           "enter a number for terminal-id. (like 1 or 2) <  >"
                   line 6 position 15.
           display
           "(both of you press ""return"" simultaneously)"
                   line 7 position 16.
           accept term-id line 6 position 62
                          no beep tab convert echo.
           move term-id to ws-id.
           display "now extending ""pinxfl"" with a key value of : "
                   line 10 position 15.
           open i-o reg-file.
           open i-o index-file.
           perform loop until kount equal to 100.
           close reg-file.
           close index-file.
           display "extension has finished, press ""return"" < >."
                   line 13 position 20.
           accept dumnum line 13 position 60 size 1 no beep.
           go to menu.
       loop.
           move "x" to r-key.
           read reg-file key is r-key
                         invalid key stop "reg-key-bad".
           add 1 to r-num.
           move r-num to ws-key.
           rewrite reg-record invalid key stop "reg-rewrite-bad".
           display ws-key line 10 position 60.
           add 1 to kount.
           write index-record from ws-record
               invalid key stop "index-key-bad".
      *
      *
       veralist.
           display "***    pacetest  verification  listing    ***"
                   line 1 position 17 erase.
           open input index-file.
           move 0 to i-key, reckount, keykount, kount.
           perform read-loop until kount = 999.
           display "press ""return"" to return to menu. < >"
                   line 23 position 22.
           accept dumnum line 23 position 57 size 1 no beep.
           go to menu.
       read-loop.
           add 1 to reckount.
           read index-file next record at end
                                       close index-file
                                       move 999 to kount.
           if kount equal 999 go to done.
           move i-key to keykount.
           add 1 to kount.
           move kount to dumnum.
           add 2 to dumnum.
           display index-record line dumnum position 25.
           if reckount not equal to keykount
              display "*** error ***" line dumnum position 59
                      blink, beep.
           if dumnum equal 22 move 0 to kount.
           if kount equal 0
              display "press ""return"" to continue. < >"
                      line 24 position 25
              accept dumnum size 1 line 24 position 54 no beep
              display
              "***    pacetest  verification  listing    ***"
              line 1 position 17 erase.
       done.
           display
           """pinxfl"" exhausted. press ""return"" to exit. < >"
           line 24 position 16.
           accept dumnum size 1 line 24 position 61 no beep.
           go to menu.
      *
      *
       exiting.
           exit program.
       stoping.
           stop run.
       end program.
