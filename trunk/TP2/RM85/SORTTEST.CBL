       identification division.
       program-id.  "sorttest".
      *
      *    title:  sorttest.cbl
      *        rm/cobol-85 sort test
      *
      * the information contained herein is proprietary to ryan mcfarland
      *    corporation, and provided for maintenance purposes only.
      *    no other use of this material is authorized or permitted without
      *    specific authorization, in writing, from ryan mcfarland corporation.
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:30  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\sorttest.cbv  $
      *
      *    module history:
      *     $log:   u:\c85\dev\verify\vcs\sorttest.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:30   bill
      *    no change.
      *
      *       rev 1.3   30 aug 1990  9:57:50   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.2   24 aug 1990 13:32:54   bill
      *    add delete file statements to remove temporary output files.
      *
      *       rev 1.1   14 aug 1990 16:36:14   bill
      *    correct spelling of program name.
      *    do not use line 25 of display.
      *
       environment division.
       configuration section.
       source-computer.  rmcobol-85.
       object-computer.  rmcobol-85.
       input-output section.
       file-control.
           select sort-file assign to disk "sortwork".
           select merge-1 assign to disk "merge1fl".
           select merge-2 assign to disk "merge2fl".
      /
       data division.
       file section.
       sd  sort-file.
       01  sort-record.
           02  record-number            pic 99.
           02                           pic x(04).
           02  name                     pic x(10).
           02                           pic x(4).
           02  zip                      pic 9(5).
           02                           pic x(04).
           02  file-number              pic 9.

       fd  merge-1
           block contains 8 records
           data record is merge1-out-record.
       01  merge1-out-record            pic x(30).

       fd  merge-2
           block contains 8 records
           data record is merge2-out-record.
       01  merge2-out-record            pic x(30).
      /
       working-storage section.
       01  title                 pic x(60) value
           "rm/cobol-85 verify sort/merge functions - version 5.00".

       01  data-area.
           02  data-filler.
               03  pic x(15) value "smith     01023".
               03  pic x(15) value "jones     90274".
               03  pic x(15) value "smith     90024".
               03  pic x(15) value "brown     02345".
               03  pic x(15) value "wong      01456".
               03  pic x(15) value "jones     45729".
               03  pic x(15) value "smith     78945".
               03  pic x(15) value "adams     38217".
               03  pic x(15) value "smith     38217".
               03  pic x(15) value "morris    86721".
               03  pic x(15) value "smith     01023".
               03  pic x(15) value "roberts   46575".
               03  pic x(15) value "young     72365".
               03  pic x(15) value "smith     58724".
               03  pic x(15) value "stevenson 24345".
               03  pic x(15) value "david     89431".
               03  pic x(15) value "smith     68752".
               03  pic x(15) value "jackson   31547".
           02  test-data  redefines data-filler occurs 18 times.
               03  test-name           pic x(10).
               03  test-zip            pic 9(5).

       01  count-wk                    pic 99 binary value zeros.

       01                              pic x(1).
           88  sort-at-end             value "y" when false "n".

       01                              pic x(1).
           88  alternating-flag        value "y" when false "n".

       01  temp-x                      pic x.

       01  crt-line                    pic 99 binary.
      /
       procedure division.
       main-pgm.
           display title, low, line 1, erase.

           display "this is a demonstration of the sort/merge feature.",
                       low, line 4, position 10.
           display "press any key to start sort demonstration ...",
                       low, line 24, blink.
           accept temp-x, low, position 0.

           display "sort by zip and name in ascending order and ",
                       low, line 1, erase,
                   "record number in descending order.", low,

                   "these are the unsorted records",
                       low, line 2, position 1,
                   "these are the sorted records",
                       low, line 2, position 41,
                   "rec #   name         zip",
                       low, line 3, position 1,
                   "rec #   name         zip",
                       low, line 3, position 41.

           sort sort-file
                   on ascending key zip, name
                   on descending key record-number
                   input procedure is input-sort
                   output procedure is output-sort.


           display "press any key to start merge demonstration ...",
                       low, line 24, blink.
           accept temp-x, low, position 0.
      /
           display "merge by zip and name in ascending order and ",
                       low, line 1, erase
                   "record number in descending order", low.

           display "this is a list of file number 1", low, line 2.
           display "rec #   name         zip  file #", low, line 3.
           display "this is a list of file number 2", low, line 14.
           display "rec #   name         zip  file #", low, line 15.
           display "this is a list of the merged files",
                       low, line 2, position 41,
                   "rec #   name         zip  file #",
                       low, line 3, position 41.

           open input merge-1 with lock.
           move 4 to crt-line.
           set sort-at-end to false.
           perform until sort-at-end
               read merge-1 at end set sort-at-end to true end-read
               if not sort-at-end
                   display merge1-out-record, low, line crt-line
                   add 1 to crt-line
               end-if
           end-perform.
           close merge-1.

           open input merge-2 with lock.
           move 16 to crt-line.
           set sort-at-end to false.
           perform until sort-at-end
               read merge-2 at end set sort-at-end to true end-read
               if not sort-at-end
                   display merge2-out-record, low, line crt-line
                   add 1 to crt-line
               end-if
           end-perform.
           close merge-2.

           merge sort-file
                   on ascending key zip, name
                   on descending key record-number
                   using merge-1 merge-2
                   output procedure output-merge.

           display "end of demonstration.  ", high, line 23, position 50,
                   "press any key to continue ...", high,
                       line 24, position 50.
           accept temp-x, low, position 0.

           delete file merge-1.
           delete file merge-2.
           exit program.
           stop run.
      /

       input-sort.
           move spaces to sort-record.
           perform varying count-wk from 1 by 1 until count-wk > 18
               move count-wk to record-number
               move test-name (count-wk) to name
               move test-zip  (count-wk) to zip
               add 4, count-wk giving crt-line
               display sort-record, low, line crt-line
               release sort-record
           end-perform.


       output-sort.
           open output merge-1 with lock.
           open output merge-2 with lock.
           set sort-at-end to false.
           set alternating-flag to false.
           move 5 to crt-line.
           perform until sort-at-end
               return sort-file record at end set sort-at-end to true
               end-return
               if not sort-at-end
                   display sort-record
                               low, line crt-line, position 41
                   add 1 to crt-line
                   if alternating-flag
                       move 1 to file-number
                       write merge1-out-record from sort-record
                       set alternating-flag to false
                   else
                       move 2 to file-number
                       write merge2-out-record from sort-record
                       set alternating-flag to true
                   end-if
               end-if
           end-perform.
           close merge-1 merge-2.
      /

       output-merge.
           set sort-at-end to false.
           move 4 to crt-line.
           perform until sort-at-end
               return sort-file record at end set sort-at-end to true
               end-return
               if not sort-at-end
                   display sort-record
                               low, line crt-line, position 41
                   add 1 to crt-line
               end-if
           end-perform.
