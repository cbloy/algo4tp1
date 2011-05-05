       identification division.
       program-id.  "rmifcb".
      *----------------------------------------------------------------
      *
      *    title:  rmifcb.cbl
      *        rm/cobol-85 index file conversion
      *
      * copyright (c) 1985-1990 by ryan mcfarland corp.  all rights reserved.
      *
      *    this file is supplied by ryan mcfarland corporation as an example onl
      *    ryan mcfarland corporation has no liability either expressed or impli
      *    as to the completeness or validity of this program.
      *
      *  version identification:
      *    $revision:   5.0  $
      *    $date:   15 sep 1990  1:42:58  $
      *    $author:   bill  $
      *    $logfile:   u:\c85\dev\util\vcs\rmifcb.cbv  $
      *
      *  version history:
      *    $log:   u:\c85\dev\util\vcs\rmifcb.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:42:58   bill
      *    no change.
      *
      *       rev 1.7   29 aug 1990 14:21:20   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.6   16 aug 1990  8:58:24   bill
      *    change version number to 5.00.
      *
      *       rev 1.5   28 mar 1990 11:10:56   anthony
      *    changed copyright to be consistent with other programs.
      *    added pvcs keywords.
      *    changed copy file names to lower case for unix.
      *    updated title displayed to be standard.
      *    updated version number.
      *
       author.  ryan mcfarland corp.
       date-written.   11 september 1985.
      *                17 september 1985  cleanup.
      *                19 september 1985  add customization commentary.
      *                21 may 1986        enhanced error diagnostics.
      *                26 june 1989       fixed large file conversion.
       date-compiled.
       security.  none.
      *
      *    for use with rmifca
      *    this program will perform the work of converting rm/cobol
      *      version 2.n and version 1.n index files to
      *      rm/cobol-85 indexed files.
      *    rmifca must be executed prior to compiling and executing
      *      this program.  the copybooks placed on files rmcpy1.cbl
      *      and rmcpy2.cbl are unique for each file description.
      *    rmifcb can be customized to provide data conversion (such as
      *      that needed to convert from separate to combined signs) on
      *      a file-by-file basis.  the appropriate points in rmifcb at
      *      which such customization code should be added are indicated
      *      by comment lines below.  (remember: do not customize the
      *      original copy of this source; make a copy.  the
      *      customization applies only to one file description.)
      *      the copybooks produced by rmifca will probably need to be
      *      customized as well to provide a new record description.
      *      alternatively, you may provide yet another complete indexed
      *      file description for your new file and change the
      *      paragraphs which open, write and close the new-inx-file to
      *      operate on your new file.
      /
       environment division.
       configuration section.
       source-computer.    rmcobol-85.
       object-computer.    rmcobol-85.
       input-output section.
       file-control.
           select inx-file-1 assign to random inx-name,
               organization is binary sequential,
               file status is index-status.
           select inx-file-2 assign to random inx-name,
               organization is binary sequential,
               file status is index-status.
           select data-file assign to random data-file-name
               organization is binary sequential,
               file status is data-file-status.
           select scratch-file assign to random "scrtch.fil"
               organization is binary sequential,
               file status is scratch-status.
      *  select statement for the index structure follows by copybook
      *
           copy "rmcpy1".
      /
       data division.
       file section.
       fd  inx-file-1.
       01  inx-record-1.
           03   inx-bytes-1    pic x occurs 32  times indexed by
                       inx-bytes-index-1.
       fd  inx-file-2.
       01  inx-record-2.
           03   inx-bytes-2    pic x occurs 512 times indexed by
                       inx-bytes-index-2.
      *  fd descriptions for the index and temporary and scratch files
      *  follow, by copybook construction.  working-storage section
      *  is also in the copybook with a single data definition for
      *  the data size
      /
           copy "rmcpy2".
      *
       01  first-header-au.
           02                       pic x(24).
           02  next-free-au         pic 9(8) binary.
           02                       pic x(4).
       77  function-number          pic  9.
       77  ver-1-record-size        pic 9(10).
       77  ver-1-aus-in-record      pic 9(5) comp-1.
       77  new-file-format          pic  x.
           88  new-single           value "1".
       77  old-or-new               pic  x.
           88  new-file             value "y".

       01  months                   pic x(36) value
           "janfebmaraprmayjunjulaugsepoctnovdec".
       01  month-names              redefines months.
           03  month-name           pic x(3) occurs 12 times.
       01  julian.
           03  j-year               pic 99.
           03  j-day                pic 999.
       01  accept-daytime.
           03  accept-date.
               05  year             pic 99.
               05  mo               pic 99.
               05  jour             pic 99.
           03  accept-time.
               05  hour             pic 99.
               05  min              pic 99.
               05  sec              pic 99.
               05                   pic 99.
       01  display-daytime.
           03  display-date.
               05  jour             pic 99.
               05                   pic x value " ".
               05  mo-name          pic xxx.
               05                   pic xxx value " 19".
               05  year             pic 99.
           03  display-time.
               05  hour             pic 99.
               05                   pic x value ":".
               05  min              pic 99.
               05                   pic x value ":".
               05  sec              pic 99.
               05                   pic 99.
       01  display-et.
               05  ethours-d        pic zzz9.
               05                   pic x value ":".
               05  etmin            pic 99 value 0.
               05                   pic x value ":".
               05  etsec            pic 99 value 0.
       77  ethours                  pic 9(4) value 0.
       77  start-seconds            pic 9(10).
       77  now-seconds              pic 9(10).
       77  process-seconds          pic 9(10).
       77  dead-time-start          pic 9(10).
       77  dead-time                pic 9(10) value 0.
       77  bytes-processed          pic 9(10) value 0.
       77  sample-bytes             pic 9(10) value 0.
       77  bytes-per-minute         pic 9(10) value 5120.
       77  index-status             pic  xx value "00".
       77  data-file-status         pic  xx value "00".
       77  scratch-status           pic  xx value "00".
       77  new-inx-status           pic  xx value "00".
       77  return-key               pic  x  value "x".
       77  expanded-status          pic  x(4)   value "0000".
       77  inx-key-num              pic  9(10)  value 1.
       77  data-key-num             pic  9(10)  value 1.
       77  inx-name                 pic  x(65)  value spaces.
       77  data-file-name           pic  x(65)  value spaces.
       77  new-inx-name             pic  x(65)  value spaces.
       77  version-ind              pic  x      value "n".
           88  ver-1-type           value "y".
           88  ver-error            value "e".
       77  dual-single-ind          pic  x      value "n".
           88  single-file          value "y".
           88  dual-single-error    value "e".
       77  flipped-flag             pic  x      value "n".
           88  flipped                          value "y".
       77  gap-ind                  pic  x      value "n".
           88  gap-found                        value "y".
       77  empty-ind                pic  x      value "n".
           88  empty-record                     value "y".
       77  search-ind               pic  x      value "n".
           88  multiple-of-512                  value "y".
       77  fill-ind                 pic  x      value "n".
           88  filling-temp-file                value "y".
       77  test-delete              pic  x(512).
       01  in-rec redefines test-delete.
           03  in-au-map            pic x(32)   occurs 16 times.
      /
       77  fix-comp-1               pic s9(5)   value 0.
       77  sum-two-comp-1s          pic 9(10)   value 0.
       77  skip-records             pic 9(10)   value 0.
       01  code-word-bytes.
           03  code-byte            pic x       occurs 2 times.
       01  comp1-code redefines code-word-bytes.
           03  comp1-bytes          pic 9(5)    comp-1.
           88  free-block           value -2.
           88  node-type            value -3.
           88  deleted-node         value -4.
       01  node-count-bytes.
           03  node-byte            pic x   occurs 2 times.
       01  node-comp-1 redefines node-count-bytes.
           03  node-count           pic 9(5) comp-1.
       77  records-read             pic  9(10)   value 0.
       77  records-written          pic  9(10)   value 0.
       77  records-dropped          pic  9(10)   value 0.
       77  ten-digits               pic  z,zzz,zzz,zz9.
       77  in-counter               pic  999     value 0.
       77  out-counter              pic  9(5)    value 1.
       77  max-temp-size            pic  9(10)   value 4096.
       77  temp-rec-count           pic  9(10).
       77  next-temp-read           pic  9(10).
       77  next-temp-write          pic  9(10).
       77  i                        pic  9(10)   value 0.
       77  j                        pic  9(10)   value 0.
       77  filter-factor            pic  9v9 value 0.0.
           88  filter-not-set       value 0.0.
      *
      *    variables for version conversion
      *
       77  remaining-aus-in-record  pic 9(5)    comp-1.
       77  current-au               pic 9(10)   value 0.
       77  last-au                  pic 9(10).
       77  sector-au                pic 9.
       77  flag-save                pic 9(5)    comp-1.
       77  x-rec                    pic x.
       01  x-rec-len-1.
           03  a-rec                pic x.
           03  b-rec                pic x.
       01  x-rec-len redefines x-rec-len-1 pic 9(5) comp-1.
       77  k                        pic 9(4).
       77  l                        pic 9.
       77  r                        pic 9(5).
       77  temp-word                pic s9(5).
       77  eof-flag                 pic x value "n".
           88  eof-on-input               value "y".
       01  bit-values               pic 9(16) value 4096025600160001.
       01  bit-value-table          redefines bit-values.
           03  bit-value            pic 9(4) occurs 4 times.
       01  hex-values               pic x(16) value "0123456789abcdef".
       01  hex-value-table          redefines hex-values.
           03  hex-value            pic x occurs 16 times.
       01  word-disp.
           03  hex-digit            pic x occurs 6 times.
       01  ascii-values.
           03                       pic x(48)   value
           "................................ !""#$%&'()*+,-./".
           03                       pic x(49)   value
           "0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz(\)^_`".
           03                       pic x(31)   value
           "abcdefghijklmnopqrstuvwxyz{|}~.".
       01  ascii-value-table redefines ascii-values.
           03  ascii-value          pic x occurs 128 times.
      * the flag values are twos-complement 16-bit numbers,
      * one entry each for flipped or non-flipped bytes.
      *
       01  in-au                    pic x(32).
       01  in-au-words              redefines in-au.
           03  in-au-word           pic 9(5)  comp-1 occurs 16 times.
       01  look-au                  redefines in-au.
           03  flag                 pic 9(5)  comp-1.
               88  type-del-record  value 0.
               88  type-dup         value -2.
               88  type-dup-f       value -257.
               88  type-node        value -3.
               88  type-node-f      value -513.
               88  type-del-node    value -4.
               88  type-del-node-f  value -769.
               88  type-gap         value -5.
               88  type-gap-f       value -1025.
           03                       pic x(30).
      /
       procedure division.
       declaratives.
       index-error section 49.
           use after standard error procedure on inx-file-1, inx-file-2.
       index-par.
      d        call "c$rerr" using expanded-status.
      d        display "    index file error # " line 2 position 55
      d        expanded-status.
           if index-status = "10" move "y" to eof-flag
           else                   move "n" to eof-flag end-if.
       index-exit.
           exit.

       data-file-error section 49.
           use after standard error procedure on data-file.
      ddata-par.
      d        call "c$rerr" using expanded-status.
      d        display "     data file error # " line 2 position 55
      d        expanded-status.
       data-file-errors.
           exit.

       new-inx-error section 49.
           use after standard error procedure on new-inx-file.
      dnew-par.
      d        call "c$rerr" using expanded-status.
      d        display "  new inx file error # " line 2 position 55
      d        expanded-status.
       new-inx-errors.
           exit.

       scratch-error section 49.
           use after standard error procedure on scratch-file.
      dscratch-par.
      d        call "c$rerr" using expanded-status.
      d        display "  scratch file error # " line 2 position 55
      d        expanded-status.
       scratch-errors.
           exit.
       end declaratives.
      /
       main section 00.
       main-para.
           perform start-up.
           go to
               dual-file,
               single-file,
               version-conversion
           depending on function-number.
       fctn-exit.
           perform close-scratch.
           perform show-time.
           compute process-seconds = now-seconds - start-seconds -
                           dead-time.
           if process-seconds < 1 move 60 to process-seconds.
           move 1 to filter-factor.
           perform show-stats.
           display "final processing rate,   " line 14 position 43.
           perform ask-continue.
       exit-para.
           exit program.
       stop-para.
           stop run.
      /
       ask-continue.
           display "type any key to continue" line 24 position 55.
           accept return-key position 0.
       ask-go.
           perform get-date-time.
           move now-seconds to dead-time-start.
           display 'type "y" (or "y") to proceed, any other key will '
                       line 24 " exit ".
           accept return-key tab position 0.
           perform get-date-time.
           compute dead-time = dead-time +
                   (now-seconds - dead-time-start).
           if return-key not = "y" and return-key not = "y" then
               display "function cancelled"
               go to exit-para
           end-if.
           display spaces size 79 line 24.
      /
      *
      *   i-o routines
      *
       open-scratch-output.
           open output scratch-file with lock.
       write-scratch.
           write scratch-record.
       close-scratch.
           close scratch-file.
       open-data-input.
           open input data-file with lock.
           perform check-open-data-file.
       read-data.
           read data-file record.
           perform check-data-time.
       delete-data.
           exit.
      * delete data-file record.
       close-data.
           close data-file.
       open-inx-input.
           if ver-1-type then
               open input inx-file-1 with lock
           else
               open input inx-file-2 with lock
           end-if.
           perform check-open-inx-file.
       read-inx.
           if ver-1-type then
               read inx-file-1
           else
               read inx-file-2
           end-if.
           perform check-inx-time.
       close-inx.
           if ver-1-type then
               close inx-file-1
           else
               close inx-file-2
           end-if.
       open-new-output.
           open output new-inx-file with lock.
           perform check-open-new-inx-file.
       write-new.
           write new-inx-record.
       close-new.
           close new-inx-file.
      /
       check-open-inx-file.
           if index-status not = "00" then
               call "c$rerr" using expanded-status
               display spaces, size 79, line 24
                       "open error on the index file, status = "
                           line 24 position 1 expanded-status
               go to fatal-error
           end-if.
       check-open-data-file.
           if data-file-status not = "00" then
               call "c$rerr" using expanded-status
               display spaces, size 79, line 24
                       "open error on the data file" line 24 position 1
                       ", status = " expanded-status
               go to fatal-error
           end-if.
       check-open-new-inx-file.
           if new-inx-status not = "00" then
               display spaces, size 79, line 24
                       "open error" line 24 position 1
                       " on the new file, status = " expanded-status
                   go to fatal-error
           end-if.
       display-records-found.
           add 1 to records-read.
           move records-read to ten-digits.
           display ten-digits line 12 position 18.
       common-write.
           perform write-record thru write-end.
       write-record.
           perform write-new.
           if new-inx-status = "00" or new-inx-status = "02" then
               go to write-successful.
           if new-inx-status = "22" then
               perform common-dup-error
               go to write-end.
           display spaces size 79 line 24.
           display "unrecognized error # " line 24, new-inx-status,
                   " on the output file".
           go to error-exit-common.
       write-successful.
           add 1 to records-written.
           move records-written to ten-digits.
           display ten-digits line 15 position 18.
       write-end.
           exit.
       common-dup-error.
           if function-number = 1 then
               move data-record to scratch-record
           else
               move new-inx-record to scratch-record
           end-if.
           perform write-scratch.
           if scratch-status not = "00" then
               call "c$rerr" using expanded-status
               display spaces, size 79, line 24
                       "error on scrtch.fil" line 24 position 1
                       ", status = " expanded-status
               go to error-exit-common
           end-if.
           add 1 to records-dropped.
           move records-dropped to ten-digits.
           display ten-digits line 18 position 18.
       error-exit-common.
           perform close-new.
           if function-number = 1 then
               perform close-data
           else
               perform close-inx
           end-if.
           go to fatal-error.
       check-data-time.
           add data-size to bytes-processed.
           add data-size to sample-bytes.
           if sample-bytes > bytes-per-minute perform show-time.
       check-inx-time.
           if ver-1-type then
               add 32  to bytes-processed, sample-bytes
           else
               add 512 to bytes-processed, sample-bytes
           end-if.
           if sample-bytes > bytes-per-minute perform show-time.
       show-time.
           perform get-date-time.
           compute process-seconds = now-seconds - start-seconds -
                           dead-time.
           if process-seconds not > 0 move 60 to process-seconds.
      *   filter factor is zero at the beginning, then becomes 20 % at 2 minutes
           if filter-not-set if process-seconds > 120 move .2
               to filter-factor.
           perform show-stats.
           move 0 to sample-bytes.
       show-stats.
           display display-date line 9 position 60 " " display-time.
           perform compute-et.
           display display-et line 10 position 70.
           compute bytes-per-minute = ( ( 1 - filter-factor ) *
                bytes-per-minute ) + (  filter-factor *
                ( bytes-processed / ( process-seconds / 60 ) ) ).
           if bytes-per-minute < 1 move 5120 to bytes-per-minute.
           move bytes-processed to ten-digits.
           display ten-digits line 12 position 67.
           move bytes-per-minute to ten-digits
               display ten-digits line 15 position 67.
           compute process-seconds = 60000000 / bytes-per-minute.
           perform compute-et.
           display display-et line 18 position 70.
       compute-et.
           compute ethours = process-seconds / 3600.
           compute etmin = ( process-seconds - (ethours * 3600) ) / 60.
           compute etsec =   process-seconds - (ethours * 3600) -
                       (etmin * 60).
           move ethours to ethours-d.
       get-date-time.
           accept accept-date from date accept accept-time from time.
           accept julian from day.
           if mo of accept-date < 1 or mo of accept-date > 12
               move 4 to mo of accept-date.
           if jour of accept-date < 1 or jour of accept-date > 31
               move 1 to jour of accept-date.
           if year of accept-date < 1 move 84 to year of accept-date.
           if j-year < 1 move 84 to j-year.
           if j-day < 1 or j-day > 366 move 92 to j-day.
           if hour of accept-time > 23 move 12 to hour of accept-time.
           if min of accept-time > 59 move 0 to min of accept-time.
           if sec of accept-time > 59 move 0 to sec of accept-time.
           move corresponding accept-date to display-date.
           move corresponding accept-time to display-time.
           move month-name ( mo of accept-date ) to mo-name.
           compute now-seconds = ( j-day - 1 ) * 86400 +
               hour of accept-time * 3600 +
               min of accept-time * 60 +
               sec of accept-time.
      /
       start-up.
           perform title.
           display spaces size 79 line 24.
           move restart-inx-name to inx-name
           move restart-data-name to data-file-name
           move restart-new-inx to new-inx-name
           move restart-version to version-ind
           move restart-dual-sin to dual-single-ind
           move restart-flipped to flipped-flag
           move restart-old-new to old-or-new
           move "y" to old-or-new
           move new-file-type to new-file-format.
           move data-size to ver-1-record-size.
           move 0 to function-number.
           if ver-1-type move 3 to function-number
               compute ver-1-aus-in-record
               =  ( data-size + 33 ) / 32.
           if single-file and not ver-1-type move 2 to function-number.
           if ( not single-file ) and ( not ver-1-type )
               move 1 to function-number.
           if function-number = 0 then
               display "illegal parameters" line 23 position 25
                       ", must stop."
               go to fatal-error
           end-if.
           perform open-scratch-output.
           if scratch-status not = "00"
               display spaces size 79 line 24 position 1
                       "open error on scrtch.fil ?" line 24 position 1
                       " status = " scratch-status
               go to fatal-error
           end-if.
           perform get-date-time.
           move now-seconds to start-seconds.
           display "processing started: " line 8 position 40
                   display-date
                   " "
                   display-time.
           display "statistics refreshed: " line 9 position 38
                   display-date
                   " "
                   display-time.
           display "processing time:" line 10 position 43
                   display-et line 10 position 70.
           move bytes-processed to ten-digits.
           display "bytes processed:" line 12 position 43
                   ten-digits line 12 position 67.
           move bytes-per-minute to ten-digits.
           display "filtered processing rate," line 14 position 43
                   "(bytes per minute):" line 15 position 43
                   ten-digits line 15 position 67.
           display "estimated elapsed time to" line 17 position 43
                   "process a 1 megabyte file:" line 18 position 43
                   display-et line 18 position 70.
           move 0 to records-read, records-written, records-dropped,
               ten-digits.
           display "data records read" line 11
                   "successfully"  line 12 position 1
                   ten-digits line 12 position 18
                   "records written"  line 14 position 1
                   "with no errors" line 15 position 1
                   ten-digits line 15 position 18
                   "records dropped to" line 17 position 1
                   "scrtch.fil" line 18 position 1
                   ten-digits line 18 position 18
                   "note: it takes a few minutes of processing time"
                       line 21 position 1
                   " before the statistics are valid".
       title.
           display "rm/cobol 1.n and 2.n to rm/cobol-85 indexed file"
                   line 1 erase " conversion - version 5.00".
           display "step b: convert indexed file data." line 2.
       fatal-error.
           perform close-scratch.
           display "fatal error" line 23.
           perform ask-continue.
           go to exit-para.
      /
       dual section 51.
       dual-file.
           perform dual-new thru dual-new-exit.
           display "file conversion completed" line 23.
           move 0 to function-number.
           go to fctn-exit.
       dual-new.
           display "ready to convert version 2.n dual file format"
                       line 3 position 1
                   "file name: " line 4 position 1
                   restart-inx-name.
           display "data file: " line 5 restart-data-name.
           display "creating the new file:" line 6.
           display "file name: " line 7 new-inx-name.
           perform ask-go.
           perform open-data-input.
           perform open-new-output.
       dual-read.
           perform read-data.
           if data-file-status = "10" or data-file-status = "30" then
               go to dual-new-exit
           end-if.
           if data-size > 512 then
               move data-record to test-delete
               if test-delete = low-values then go to dual-read
               end-if
           end-if.
           if data-size < 513 and data-record = low-values then
                go to dual-read
           end-if.
           perform display-records-found.
           move data-record to new-inx-record.
      * ^^^^^ replace previous line if you wish to customize ^^^^^
      * ^^^^^ the conversion process for 2.n dual file format^^^^^
           perform common-write.
           go to dual-read.
       dual-new-exit.
           perform close-data. perform close-new.
      /
       single section 52.
       single-file.
           perform single-new thru single-new-exit.
           display "file conversion completed" line 23.
           move 0 to function-number.
           go to fctn-exit.
       single-new.
           display "ready to convert version 2.n single file format"
                       line 3 position 1
                   "file name: " line 4 position 1
                   restart-inx-name.
           display "creating the new file:" line 6.
           display "file name: " line 7 new-inx-name.
           perform ask-go.
           perform open-inx-input.
           perform open-new-output.
           perform find-data-start.
           move 1 to out-counter. move 0 to in-counter.
      *  read in the header and the 512 bytes following that.
           perform read-inx.
           if eof-on-input go to single-new-exit.
           perform read-inx.
           if eof-on-input go to single-new-exit.
           perform read-inx-record skip-records times.
           if eof-on-input go to single-new-exit.
           if not (index-status = "00" or "o4" or "97")
               call "c$rerr" using expanded-status
               display spaces, size 79, line 24
                       "error on reading from" line 24 position 1
                       " the input file, status = " expanded-status
               go to error-exit-common
           end-if.
      *  loop between scan-something and scan-end
       scan-something.
           move low-values to out-buffer.
           perform get-input-code.
      * if this is a node of some sort, skip it by returning to scan-end
           if free-block or node-type or deleted-node then
               next sentence
           else move "n" to gap-ind move 0 to in-counter
               move 1 to out-counter
               perform process-record thru
               process-record-exit until gap-found
           end-if.
       scan-end.
           perform read-inx-record.
           if not eof-on-input go to scan-something.
       single-new-exit.
           perform close-inx.
           perform close-new.
       process-record.
           if eof-on-input then
               move "y" to gap-ind
               go to process-record-exit
           end-if.
           if in-counter = 512 then
               move 0 to in-counter
               perform read-inx-record
               if out-counter = 1 or out-buffer = low-values then
                   perform get-input-code
                   perform get-node-count
                   if (free-block or node-type or deleted-node) and
                      (node-count not < 0 and node-count not > 128) then
                       move "y" to gap-ind
                       go to process-record-exit
                   end-if
               end-if
           end-if.
           add 1 to in-counter.
           if eof-on-input
               perform flush-last-output
               go to process-record-exit
           end-if.
           move inx-bytes-2 (in-counter) to
               out-buffer-byte (out-counter).
           if out-counter = 512 then
               perform deleted-rec-check thru del-rec-check-exit
               if empty-record
                   go to process-record-exit
               end-if
           end-if.
           if out-counter = data-size then
               perform scan-out-rec thru scan-out-exit
               move 0 to out-counter
               move low-values to out-buffer
           end-if.
           add 1 to out-counter.
       process-record-exit.
           exit.
      /
       flush-last-output.
           move "y" to gap-ind.
           if out-counter = data-size then
               perform scan-out-rec thru scan-out-exit
           end-if.
       scan-out-rec.
      *  check an output record when the output buffer is full,
           move "n" to empty-ind.
           if data-size > 512 then
               move out-buffer to test-delete
               if test-delete = low-values then move "y" to empty-ind
               end-if
           end-if.
           if data-size < 513 and out-buffer = low-values then
               move "y" to empty-ind.
           if empty-record perform deleted-record
               go to scan-out-exit.
           perform record-found.
       scan-out-exit.
           exit.
       deleted-rec-check.
           move new-inx-record to test-delete.
           move "n" to empty-ind.
           if test-delete = low-values move "y" to empty-ind
                perform deleted-record.
       del-rec-check-exit.
           exit.
      /
       deleted-record.
      *  a deleted record has been detected, either by finding 512 zeroes
      *  or an output record full of zeroes.
      *    for the case of 512 zeroes, move to the next record
           if data-size > out-counter
               perform move-to-next-record.
      *  starting on a new output record
           move low-values to out-buffer.
           move 1 to out-counter.
       move-to-next-record.
      *  move up by the difference of data-size and out-counter
      *  out-counter will be 512 here always
           compute i = data-size - out-counter.
           add in-counter to i.
      *  if in-counter goes over 512, move up to that particular record
           divide i by 512 giving i remainder j.
      *  if trying to move to the first byte of a block, let the main loop
      *  read in that block so it can check for node information.
           if j = 0 and i > 0 subtract 1 from i move 512 to j.
           perform read-inx-record i times.
           move j to in-counter.
       record-found.
           perform display-records-found.
      * ^^^^^ add code here if you wish to customize the     ^^^^^
      * ^^^^^ conversion process for 2.n single file format. ^^^^^
      * ^^^^^ the data for the record are in the record area ^^^^^
      * ^^^^^ for new-inx-file.                              ^^^^^
           perform common-write.
      /
       get-input-code.
           if flipped move inx-bytes-2 ( 1 ) to code-byte ( 2 )
               move inx-bytes-2 ( 2 ) to code-byte ( 1 )
           else
               move inx-bytes-2 ( 1 ) to code-byte ( 1 )
               move inx-bytes-2 ( 2 ) to code-byte ( 2 ).
       get-node-count.
           if flipped move inx-bytes-2 ( 3 ) to node-byte ( 2 )
               move inx-bytes-2 ( 4 ) to node-byte ( 1 )
           else
               move inx-bytes-2 ( 3 ) to node-byte ( 1 )
               move inx-bytes-2 ( 4 ) to node-byte ( 2 ).
       read-inx-record.
           if not eof-on-input perform read-inx.
       find-data-start.
           compute skip-records = node-size / 512.
           if skip-records > 0 subtract 1 from skip-records.
       compute-fix-comp-1.
           move x-rec-len to fix-comp-1.
           if fix-comp-1 < 0 add 65536 to fix-comp-1.
      /
       version section 53.
       version-conversion.
           display
           "ready to convert the version 1.n rm/cobol index file,"
               line 3.
           display "file name: " line 4 restart-inx-name.
           display "creating the new file:" line 6.
           display "file name: " line 7 new-inx-name.
           perform ask-go.
           perform open-new-output.
           perform open-inx-input.
           move 8 to sector-au.
           perform get-rec-au varying i from 1 by 1 until
               i > ver-1-aus-in-header.
           move out-au(1) to first-header-au.
           move spaces to out-rec.
           move data-size to x-rec-len
           perform flip-bytes move x-rec-len to flag-save.
           subtract 1 from ver-1-aus-in-record giving
                    remaining-aus-in-record.
           move 0 to current-au.
           if flipped
               move next-free-au (1:1) to x-rec
               move next-free-au (2:1) to next-free-au (1:1)
               move x-rec              to next-free-au (2:1)
               move next-free-au (3:1) to x-rec
               move next-free-au (4:1) to next-free-au (3:1)
               move x-rec              to next-free-au (4:1)
           end-if.
           subtract 1 from next-free-au giving last-au.
           perform scan-aus until ( current-au not less than last-au )
               or eof-on-input.
           perform close-new.  perform close-inx.
           display "conversion complete" line 23.
           move 0 to function-number.
           go to fctn-exit.
       scan-aus.
           perform get-au.
           if eof-on-input next sentence
           else if type-node and not flipped perform ignore-node,
           else if type-node-f and flipped perform ignore-node,
           else if type-del-node and not flipped perform ignore-node,
           else if type-del-node-f and flipped perform ignore-node,
           else if type-dup and not flipped next sentence
           else if type-dup-f and flipped   next sentence
           else if type-gap and not flipped perform ignore-gap,
           else if type-gap-f and flipped perform ignore-gap,
           else if not type-del-record perform display-records-found,
                       perform process-record,
           else perform ignore-record.
      *
       ignore-node.
           perform get-au 15 times.
      *
       ignore-gap.
           perform get-au until sector-au = 8.
      *
       ignore-record.
           perform get-au remaining-aus-in-record times.
      /
       process-record.
           if not eof-on-input then
               move flag to x-rec-len
               perform flip-bytes.
           if not eof-on-input
               if x-rec-len > data-size or x-rec-len < 0
                   perform no-hit thru no-hit-exit
                   go to process-record.
           if not eof-on-input move in-au to out-au (1)
           perform get-rec-au varying i from 2 by 1
               until ( i greater than ver-1-aus-in-record )
                   or eof-on-input.
           if not eof-on-input perform ver-1-write.
       flip-bytes.
           if flipped
               move b-rec to x-rec
               move a-rec to b-rec
               move x-rec to a-rec.
      *
       get-rec-au.
           perform get-au.
           if not eof-on-input move in-au to out-au (i).
      *
       get-au.
           if sector-au = 8 move 0 to sector-au.
           add 1 to sector-au.
           if not eof-on-input
               perform read-inx
               if not eof-on-input
                   move inx-record-1 to in-rec, in-au
                   add 1 to current-au.
      *
       ver-1-write.
           move actual-data to new-inx-record.
      * ^^^^^ replace previous line if you wish to customize ^^^^^
      * ^^^^^ the conversion process for 1.n file format     ^^^^^
           perform common-write.
      *
       no-hit.
           perform show-time.
           perform get-date-time.
           move now-seconds to dead-time-start.
       no-hit-repeat.
           perform clear-display-area.
           display "an unrecognized area of this file has been" line 20
               position 1 " encountered.".
           display "can you assist in finding the next data record? "
               line 21.
           accept return-key position 0 tab.
           if return-key = "y" or return-key = "y"
               perform view-data thru check-data-exit
               go to no-hit-exit.
           if return-key = "n" or return-key = "n"
               go to error-exit-common.
           go to no-hit-repeat.
       no-hit-exit.
           perform clear-display-area
           perform get-date-time.
           compute dead-time = dead-time +
                   (now-seconds - dead-time-start).
       view-data.
           if eof-on-input go to check-data-exit.
           move 0 to i.
           display space line 20 erase eos.
           perform hex-display 8 times.
           move 0 to i.
           display quote line 20 position 53.
           perform ascii-display 8 times.
           display quote position 0.
           display space line 21.
           perform hex-display 8 times.
           move 8 to i.
           display quote line 21 position 53.
           perform ascii-display 8 times.
           display quote position 0.
      /
       check-data.
           display "is this the start of a data record?" line 23
                   " (the data record must start at the third".
           display
               "byte.  the first two bytes should be the record size".
           if flipped
               display ", reversed.) " position 0
           else
               display ".) " position 0.
           accept return-key position 0 tab.
           if return-key = "y" or return-key = "y"
               move flag-save to flag
               go to check-data-exit.
           if return-key = "n" or return-key = "n"
               perform get-au
               go to view-data.
           go to check-data.
       check-data-exit.
           display spaces size 79 line 23
           display spaces size 79 line 24.
       clear-display-area.
           display spaces size 79 line 20
           display spaces size 79 line 21
           display spaces size 79 line 22
           display spaces size 79 line 23.
       hex-display.
           add 1 to i.
           move in-au-word (i) to temp-word.
           if temp-word < 0 add 65536 to temp-word.
           move 0 to j.
           move 0 to l.
           perform hex-convert 4 times.
           display word-disp position 0.
       hex-convert.
           add 1 to j.
           add 1 to l.
           if l = 3 add 1 to l.
           divide temp-word by bit-value (j)
               giving k remainder r.
           move r to temp-word.
           add 1 to k.
           move hex-value (k) to hex-digit (l).
       ascii-display.
           add 1 to i.
           move in-au-word (i) to temp-word.
           if temp-word < 0 add 65536 to temp-word.
           divide temp-word by 256 giving k remainder r.
           add 1 to k.
           add 1 to r.
           if k > 128 or k < 1 move 1 to k.
           if r > 128 or r < 1 move 1 to r.
           display ascii-value (k) position 0.
           display ascii-value (r) position 0.
       end program.
