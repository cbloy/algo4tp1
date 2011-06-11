       identification division.
       program-id.  "filetest".
      *
      *    title:  filetest.cbl
      *        rm/cobol-85 file system test
      *
      * the information contained herein is proprietary to ryan mcfarland
      *    corporation, and provided for maintenance purposes only.
      *    no other use of this material is authorized or permitted without
      *    specific authorization, in writing, from ryan mcfarland corporation.
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:10  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\filetest.cbv  $
      *
      *    module history:
      *     $log:   u:\c85\dev\verify\vcs\filetest.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:10   bill
      *    no change.
      *
      *       rev 1.4   29 aug 1990 14:51:04   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.3   15 aug 1990  9:29:36   bill
      *    add delete file statements to clean up after running test.
      *
      *       rev 1.2   14 aug 1990 15:42:56   bill
      *    correct spelling of program name.
      *    remove sccs id from working-storage section.
      *    change external file names to lower case for unix.
      *    add stop run.
      *
      *       rev 1.1   14 aug 1990 14:57:28   bill
      *    add read previous.
      *    change program name to filetest to match unix.
      *
       environment division.

       configuration section.

       source-computer.  rmcobol-85.
       object-computer.  rmcobol-85.

       input-output section.

       file-control.

           select seq-file
              assign to random, "seqfl".

           select rel-file
               assign to random, "relfl"
               organization is relative,
               access mode is dynamic,
               relative key is rel-key.

           select inx-file
               assign to random, "inxfl"
               organization is indexed,
               access mode is dynamic,
               record key is inx-key,
               alternate record key is alt-key.
      /
       data division.

       file section.

       fd  seq-file                    block contains 8 records
                                       data record is seq-record.
       01  seq-record                  pic x(30).

       fd  rel-file                    block contains 8 records
                                       data record is rel-record.
       01  rel-record                  pic x(30).

       fd  inx-file                    block contains 8 records
                                       data record is inx-record.
       01  inx-record.
           02  inx-key                 pic x(3).
           02                          pic x(6).
           02  alt-key                 pic x(3).
           02  inx-dat                 pic x(3).
           02                          pic x(15).

       working-storage section.

       01  title                       pic x(60) value
           "rm/cobol-85 verify file i/o operations - version 5.00".

       01  rel-key                     pic 999.

       01  key-num                     pic 999.

       01  return-key                  pic x.

       01  function-number             pic 9.

       01  counter                     pic 999.

       01  alt-count                   pic 999.
      /
       procedure division.

       section-1 section 1.

       display-menu.
           display title, high, line 1, erase.

           display "1. sequential file test", low, line 4 position 10.
           display "2. relative file test",   low,        position 10.
           display "3. indexed file test",    low,        position 10.
           display "4. exit program",         low,        position 10.

           display "each of these tests will write 100 records",
                                              low, line 10,
                   " and then read the records.", low.

       accept-function-number.
           display "enter test number", low, line 16, position 40.
           accept function-number, line 16, position 66, tab.
           display spaces, erase eol, line 16, position 40.
           go to para-3,
                 para-2,
                 para-1,
                 exit-para depending on function-number.
           go to accept-function-number.

       display-menu-loop.
           display "type any key to continue....", high,
                   line 22 position 40.
           accept return-key position 0.
           go to display-menu.

       reset-counter.
           move zero to counter.
      /
       section-2 section 2.

       write-inx-record.
           add 1 to counter.
           compute alt-count = 101 - counter.
           move counter to inx-dat, inx-key.
           move alt-count to alt-key.
           display "writing by prime key with alternate. record ",
                       low, line 12 position 10,
                   counter, low, convert.
           write inx-record.

       read-by-inx-key.
           add 1 to key-num.
           move key-num to inx-key.
           display "reading by prime key. record ",
                       low, line 13 position 10,
                   key-num, low, convert.
           read inx-file record key is inx-key
               invalid key add 1 to counter.
           if inx-dat not = key-num add 1 to counter.

       read-by-alt-key.
           move key-num to alt-key.
           compute alt-count = 101 - key-num.
           display "reading by alternate key. record ",
                       low, line 14 position 10,
                   alt-count, low, convert.
           read inx-file record key is alt-key
               invalid key add 1 to counter.
           if inx-dat not = alt-count add 1 to counter.
           subtract 1 from key-num.

       read-prev-inx-record.
           subtract 1 from key-num.
           display "reading previous by prime key. record ",
                       low, line 15 position 10,
                   key-num, low, convert.
           read inx-file previous record
               at end add 1 to counter.
           if inx-dat not = key-num add 1 to counter.
      /
       write-rel-record.
           move spaces to rel-record.
           move counter to rel-key.
           move counter to rel-record.
           display "writing relative record ",
                       low, line 12, position 10,
                   counter, low, convert.
           write rel-record.
           add 2 to counter.

       read-rel-record.
           add 1 to key-num.
           move key-num to rel-key.
           display "reading relative record "
                       low, line 13, position 10,
                   key-num, low, convert.
           read rel-file record
               invalid key add 1 to counter.
           if rel-record not = rel-key add 1 to counter.

       read-prev-rel-record.
           subtract 1 from key-num.
           display "reading previous relative record ",
                       low, line 14 position 10,
                   key-num, low, convert.
           read rel-file previous record
               at end add 1 to counter.
           if rel-record not = rel-key add 1 to counter.

       write-seq-record.
           add 1 to key-num.
           move key-num to seq-record.
           display "writing sequential record ",
                       low, line 12, position 10,
                   key-num, low, convert.
           write seq-record.

       read-seq-record.
           add 1 to key-num.
           display "reading sequential record "
                       low, line 13, position 10,
                   key-num, low, convert.
           read seq-file record.
           if seq-record not = key-num
               add 1 to counter.
      /
       section-51 section 4.

       para-1.
           perform reset-counter.
           open output inx-file with lock.
           perform write-inx-record 100 times.
           close inx-file.
           open input inx-file with lock.
           perform reset-counter.
           move 0 to key-num.
           perform read-by-inx-key 100 times.
           perform read-by-alt-key 100 times.
           move 101 to inx-key, key-num.
           start inx-file key not greater than inx-key
               invalid key
                   display "invalid start", high line 18 position 10.
           perform read-prev-inx-record 100 times.
           close inx-file.
           delete file inx-file.
           if counter = 0
               display "passed!", high, line 17, position 10
           else
               display "failed!", high, blink, line 17, position 10.
           go to display-menu-loop.

       section-52 section 5.

       para-2.
           open output rel-file with lock.
           move 1 to counter.
           perform write-rel-record 50 times.
           move 2 to counter.
           perform write-rel-record 50 times.
           perform reset-counter.
           close rel-file.
           open input rel-file with lock.
           move 0 to key-num.
           perform read-rel-record 100 times.
           move 101 to key-num, rel-key.
           start rel-file key not greater than rel-key
               invalid key
                   display "invalid start", high line 18 position 10.
           perform read-prev-rel-record 100 times.
           close rel-file.
           delete file rel-file.
           if counter = 0
               display "passed!", high, line 16, position 10
           else
               display "failed!", high, blink, line 16, position 10.
           go to display-menu-loop.
      /
       section-53 section 6.

       para-3.
           open output seq-file with lock.
           move 0 to key-num.
           perform write-seq-record 100 times.
           perform reset-counter.
           close seq-file.
           open input seq-file with lock.
           move 0 to key-num.
           perform read-seq-record 100 times.
           close seq-file.
           delete file seq-file.
           if counter = 0
               display "passed!", high, line 15, position 10
           else
               display "failed!", high, blink, line 15, position 10.
           go to display-menu-loop.

       exit-para.
           exit program.
           stop run.
