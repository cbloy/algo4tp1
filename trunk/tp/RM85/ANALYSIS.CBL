       identification division.
       program-id. "analysis".

      *
      *    title:  analysis.cbl
      *        rm/cobol-85 analyze runtime instrumentation
      *
      * copyright (c) 1985-1990 by ryan mcfarland corp.  all rights reserved.
      *
      *    this file is supplied by ryan mcfarland corporation as an example
      *    only.  ryan mcfarland corporation has no liability either expressed
      *    or implied as to the completeness or validity of this program.
      *
      *====
      *  this is the analysis program described in your
      *  rm/cobol-85 user's guide under instrumentation.
      *  it provides a model of how the data gathered
      *  by runtime instumentation can be used to produce
      *  a report showing execution counts for a given
      *  cobol program.  while it is intended to be complete,
      *  you may use this source as a starting point
      *  for what you might consider to be a more effective
      *  analysis program.  however, only the delivered
      *  version of analysis will be supported by ryan-mcfarland.
      *
      *  analysis: ver. 1.00  05-29-86.
      *            ver. 2.00  09-03-86.
      *            ver. 3.00  08-15-88.
      *====
      *
      *  version identification:
      *    $revision:   5.0  $
      *    $date:   15 sep 1990  1:43:28  $
      *    $author:   bill  $
      *    $logfile:   u:\c85\dev\util\vcs\analysis.cbv  $
      *
      *  version history:
      *    $log:   u:\c85\dev\util\vcs\analysis.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:43:28   bill
      *    no change.
      *
      *       rev 1.7   29 aug 1990 14:06:22   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.6   13 apr 1990  8:56:14   anthony
      *    use the first letter of the input file to determine the case of the
      *    extension.
      *
      *       rev 1.5   12 apr 1990 17:17:04   anthony
      *    forgot a period.
      *
      *       rev 1.4   12 apr 1990 16:53:08   anthony
      *    remove f1=end from prompt.
      *    look for lower and upper case .lst for unix
      *    allow / as well as \ at the end of the path for the .hst file.
      *    use / instead of \ to delimit pathnames.
      *    always make the .cnt file name upper case.
      *
      *       rev 1.3   28 mar 1990  8:32:26   anthony
      *    added copyright message and pvcs identifier strings.
      *
       environment division.
       configuration section.

       source-computer.  rmcobol-85.

       object-computer.  rmcobol-85
           program collating sequence is upper-lower.

       special-names.
           switch-1 on status is suppress-listing;
           switch-2 on status is suppress-procedures;
           switch-3 on status is suppress-nonprocedure-lines;
           switch-4 on status is suppress-total-update;
           switch-5 on status is merge-from-count;

           alphabet upper-lower is
               1 thru 65, 'a' also 'a', 'b' also 'b', 'c' also 'c',
                          'd' also 'd', 'e' also 'e', 'f' also 'f',
                          'g' also 'g', 'h' also 'h', 'i' also 'i',
                          'j' also 'j', 'k' also 'k', 'l' also 'l',
                          'm' also 'm', 'n' also 'n', 'o' also 'o',
                          'p' also 'p', 'q' also 'q', 'r' also 'r',
                          's' also 's', 't' also 't', 'u' also 'u',
                          'v' also 'v', 'w' also 'w', 'x' also 'x',
                          'y' also 'y', 'z' also 'z',
               92 thru 97, 124 thru 128.

      /
       input-output section.
       file-control.

           select listing-file
               assign input, listing-filename
               organization line sequential
               file status listing-file-stat.

           select merged-file
               assign print, merged-filename
               organization line sequential.

           select verb-count-file
               assign input, verb-count-filename
               organization binary sequential
               file status verb-count-file-stat.

           select total-count-file
               assign random, total-count-filename
               organization binary sequential
               file status total-count-file-stat.
      /
       data division.
       file section.


       fd  listing-file                label records omitted.

       01  listing-line.
           02  listing-line-number     pic z(5).
           02  redefines listing-line-number pic x(5).
               88  listing-header-found value " line".
           02                          pic x(13).
           02  listing-source-image.
               03                      pic x(6).
               03                      pic x(1).
                   88  not-a-comment-line  value space.
               03  area-a-and-b        pic x(65).
               03                      pic x(42).

       01  listing-line-tests.
           02                          pic x(4).
               88  page-ejector        value "rm/c".
           02  line-number-low-digit   pic x(1).



       fd  verb-count-file             label records omitted.

       01  verb-count-record.
           02  verb-entry              occurs 2.
               03  verb-type           pic x(1).
               03  execution-count     pic 9(8) binary.


       fd  total-count-file            label records omitted.

       01  total-count-record.
           02  verb-entry              occurs 2.
               03  verb-type           pic x(1).
               03  execution-count     pic 9(8) binary.

      /
       fd  merged-file                 label records omitted.

       01  merged-record.
           02  merge-line-count        pic z(6)b occurs 2.
           02  merge-line-number       pic z(5)b.
           02  merge-source-image      pic x(112).

       01  merged-header-line.
           02  merge-count-headers     pic x(14).
           02  merge-line-header       pic x(6).

       01  merged-verb-line.
           02  merge-verb-count        pic zzz,zzzb.
           02  merge-verb-percent      pic zz9.
           02  merge-verb-name         pic bbx(10)bb.
           02  merge-history-count     pic zzz,zzz,zzzb.
           02  merge-history-percent   pic zz9.

       01  merged-procedure-line.
           02  merge-procedure-count   pic zzz,zzzb.
           02  merge-procedure-percent pic zz9.99.
           02  merge-procedure-line    pic z(5)9bb.
           02  merge-procedure-name    pic x(110).
      /
       working-storage section.

       01  verb-table-value.
           02  pic x(10) value "accept".
           02  pic x(10) value "add".
           02  pic x(10) value "alter".
           02  pic x(10) value "call".
           02  pic x(10) value "cancel".
           02  pic x(10) value "close".
           02  pic x(10) value "compute".
           02  pic x(10) value "continue".
           02  pic x(10) value "delete".
           02  pic x(10) value "disable".
           02  pic x(10) value "display".
           02  pic x(10) value "divide".
           02  pic x(10) value "enable".
           02  pic x(10) value "enter".
           02  pic x(10) value "evaluate".
           02  pic x(10) value "exit".
           02  pic x(10) value "go".
           02  pic x(10) value "goback".
           02  pic x(10) value "if".
           02  pic x(10) value "initialize".
           02  pic x(10) value "inspect".
           02  pic x(10) value "merge".
           02  pic x(10) value "move".
           02  pic x(10) value "multiply".
           02  pic x(10) value "open".
           02  pic x(10) value "perform".
           02  pic x(10) value "purge".
           02  pic x(10) value "read".
           02  pic x(10) value "receive".
           02  pic x(10) value "release".
           02  pic x(10) value "return".
           02  pic x(10) value "rewrite".
           02  pic x(10) value "search".
           02  pic x(10) value "send".
           02  pic x(10) value "set".
           02  pic x(10) value "sort".
           02  pic x(10) value "start".
           02  pic x(10) value "stop".
           02  pic x(10) value "string".
           02  pic x(10) value "subtract".
           02  pic x(10) value "unlock".
           02  pic x(10) value "unstring".
           02  pic x(10) value "write".
       01  verb-table                  redefines verb-table-value.
           02  verb-name               pic x(10)
                                       occurs 43 times
                                       ascending key verb-name
                                       indexed by verb-name-index.

       01  verb-counts.
           02  verb-count              pic 9(8) binary
                                       occurs 43 times
                                       indexed by verb-count-index.

       01  verb-count-hold             pic 9(8) binary.

       01  verb-name-hold              pic x(10).

       01  listing-filename            pic x(40).

       01  merged-filename             pic x(40).

       01  filename.
           02                          pic x(8).
           02  filename-crunch         pic x(12).

       01  verb-count-filename         pic x(40).

       01  total-count-filename        pic x(40).

       01  listing-file-status.
           02  listing-file-stat       pic x(2).
               88  listing-file-okay   value zeros.
               88  end-of-listing-file value "10".
           02                          pic x(2).
       01  listing-file-stat-nsu redefines listing-file-status
               pic 9(4).

       01  verb-count-file-status.
           02  verb-count-file-stat    pic x(2).
               88  verb-count-file-okay value zeros.
           02                          pic x(2).
       01  verb-count-file-stat-nsu redefines verb-count-file-status
               pic 9(4).

       01  total-count-file-status.
           02  total-count-file-stat   pic x(2).
               88  total-count-file-okay value zeros.
               88  total-count-file-not-found value "35".
           02                          pic x(2).
       01  total-count-file-stat-nsu redefines total-count-file-status
               pic 9(4).

       01  display-file-status         pic 99,99.

       01                              pic x(1).
           88  verb-count-file-open    value "y" false "n".

       01                              pic x(1).
           88  scanning-for-id         value "y" false "n".

       01                              pic x(1).
           88  procedure-region        value "y" false "n".

       01  listing-line-pointer        pic 99 binary.

       01  verb-candidate              value spaces.
           02  literal-delimiter       pic x(1).
               88  literal-found       values '"' "'".
           02                          pic x(39).

       01  literal-holder              pic x(1).

       01  line-verb-counts.
           02  line-verb-count         pic 9(8) binary occurs 2.

       01  line-of-listing             pic 9(8) binary value 0.

       01  i                           pic 9(4) binary.

       01  j                           pic 9(4) binary.

       01  current-paragraph           pic 9(3) binary.

       01  paragraph-table.
           02  paragraph-entry         occurs 500.
               03                      pic x(1).
                   88  section-entry   value "x" false "p".
               03  paragraph-name      pic x(30).
               03  paragraph-line      pic 9(8) binary.
               03  paragraph-count     pic 9(8) binary.

       01  paragraph-entry-hold.
           02  left-justify-hold-1 pic x(10).
           02  left-justify-hold-2 pic x(10).
           02                          pic x(19).

       01  verb-count-file-header.
           02  verb-count-header-entry pic x(10) occurs 6
                           indexed by verb-count-header-index.
       01  redefines verb-count-file-header.
           02  c-verb-count-program-id pic x(30).
           02  c-first-procedure-line  pic 9(8) binary.
           02  c-end-procedure-line    pic 9(8) binary.
           02  c-total-verb-count      pic 9(8) binary.
           02  c-pitched-verb-count    pic 9(8) binary.
           02                          pic x(14).

       01  total-count-file-header.
           02  total-count-header-entry pic x(10) occurs 6
                           indexed by total-count-header-index.
       01  redefines total-count-file-header.
           02  t-verb-count-program-id pic x(30).
           02  t-first-procedure-line  pic 9(8) binary.
           02  t-end-procedure-line    pic 9(8) binary.
           02  t-total-verb-count      pic 9(8) binary.
           02  t-pitched-verb-count    pic 9(8) binary.
           02                          pic x(14).

       01  identification-line         pic 9(8) binary.

       01  procedure-line              pic 9(8) binary.

       01  expected-procedure-line     pic 9(8) binary.

       01  totalulator                 pic 9(8) binary.

       01  total-statement-count       pic 9(8) binary value 0.

       01  total-procedure-count       pic 9(8) binary value 0.

       01  lines-containing-statements pic 9(8) binary value 0.

       01  lines-containing-procedures pic 9(8) binary value 0.

       01  procedure-report-line       pic 9(2) binary.

       01  edit-number                 pic zz,zzz,zz9.
       01  edit-number-alpha           redefines edit-number
                                       pic x(10).

       01  left-justified-number       pic x(10).

       01  field-terminator            pic 9(4) binary.


       linkage section.
       01  main-argument.
           02  argument-size           pic 9(4) binary.
           02  histogram-path.
               03    argument-char     pic x(1) occurs 0 to 100
                                       depending on argument-size.
      /
       procedure division using main-argument.
       declaratives.
       the-listing-file section.
           use after error procedure on listing-file.
       a.
           call "c$rerr" using listing-file-status.
           move listing-file-stat-nsu to display-file-status.
           if not end-of-listing-file
               display "i-o error ", display-file-status,
                       " on listing file ", listing-filename.

       the-verb-count-file section.
           use after error procedure on verb-count-file.
       a.
           call "c$rerr" using verb-count-file-status.
           move verb-count-file-stat-nsu to display-file-status.
           display "i-o error ", display-file-status,
                   " on data collection file ", verb-count-filename.

       the-total-count-file section.
            use after error procedure on total-count-file.
       a.
           call "c$rerr" using total-count-file-status.
           move total-count-file-stat-nsu to display-file-status.
           if not total-count-file-not-found
               display "i-o error ", display-file-status,
                   " on total count file ", total-count-filename.

       end declaratives.
      /
       command-processing section.
       accept-filename.
           display "listing file: ".
           accept filename, position 0, prompt, tab, echo.
           if filename = spaces stop run.
           move spaces to listing-filename.
           if filename(1:1) is alphabetic-upper
               string
                   filename delimited by space,
                   ".lst" delimited by size
                   into listing-filename
           else
               string
                   filename delimited by space,
                   ".lst" delimited by size
                   into listing-filename
           end-if.
           open input listing-file.
           if not listing-file-okay go accept-filename.

       make-hst-filename.
           move spaces to merged-filename

           if argument-size = 0
             string filename delimited by space,
                    ".hst" delimited by size
               into merged-filename
           else if argument-char (argument-size) = ":" or "\" or "/"
             string histogram-path delimited by size,
                    filename delimited by space,
                    ".hst" delimited by size
               into merged-filename
           else
             string histogram-path, '/' delimited by size,
                    filename delimited by space,
                    ".hst" delimited by size
               into merged-filename
           end-if end-if.
           if filename(1:1) is alphabetic-lower
               inspect merged-filename replacing all ".hst" by ".hst".
           open output merged-file.
           perform the-main until not listing-file-okay.
           close listing-file merged-file.
           if verb-count-file-open close verb-count-file.
           set verb-count-file-open to false.
           go accept-filename.
      /
       the-main section.
       scan-for-program-id.
           perform varying verb-count-index
                   from 1 by 1 until verb-count-index > 43
               move 0 to verb-count (verb-count-index)
           end-perform.
           move 0 to current-paragraph, total-statement-count,
               total-procedure-count, lines-containing-statements,
               lines-containing-procedures.
           set scanning-for-id to true.
           set procedure-region to false.
           move low-values to verb-candidate.
           perform get-listing-source-line.

           perform until not listing-file-okay or
                       verb-candidate equal "identification" or "id"
               perform get-next-nonliteral-word
               if verb-candidate not = "identification"
                   perform print-nonprocedure-line
                   perform get-listing-source-line
               end-if
           end-perform.

           if not listing-file-okay go exit-section.
           set scanning-for-id to false.
           move listing-line-number to identification-line.

           perform until not listing-file-okay or
                       verb-candidate equal "program-id"
               perform get-next-nonliteral-word
               if verb-candidate not = "program-id"
                   perform print-nonprocedure-line
                   perform get-listing-source-line
               end-if
           end-perform.

           if not listing-file-okay go exit-section.

           move spaces to verb-candidate.
           perform until verb-candidate not = spaces or
                   not listing-file-okay
               unstring area-a-and-b delimited by all space
                   into verb-candidate
                   pointer listing-line-pointer
               if listing-file-okay and verb-candidate = spaces
                   perform print-nonprocedure-line
                   perform get-listing-source-line
               end-if
           end-perform.

           if not listing-file-okay
               display "premature eof on listing file; status=",
                       display-file-status
               go exit-section.

           if literal-found
               move literal-delimiter to literal-holder
               inspect verb-candidate
                   replacing all literal-holder by space
               move 2 to i
           else
               move 1 to i
           end-if.
           inspect verb-candidate replacing first "." by space.
           unstring verb-candidate delimited by space into filename
               with pointer i.
           inspect filename converting
               "abcdefghijklmnopqrstuvwxyz" to
               "abcdefghijklmnopqrstuvwxyz".
           move spaces to filename-crunch, verb-count-filename.
           string
               filename delimited by space, ".cnt" delimited by size
               into verb-count-filename.
           move spaces to filename-crunch, total-count-filename.
           string
               filename delimited by space, ".tot" delimited by size
               into total-count-filename.

           if not suppress-total-update
               perform merge-count-files.

           if not merge-from-count
               move total-count-filename to verb-count-filename.

           set verb-count-file-open to false.
           open input verb-count-file.
           if not verb-count-file-okay
               perform print-nonprocedure-line
               display "skipping ", verb-candidate
               go scan-for-program-id.
           set verb-count-file-open to true.

           perform varying verb-count-header-index from 1 by 1
                   until verb-count-header-index > 6 or
                       not verb-count-file-okay
               read verb-count-file
               if verb-count-file-okay
                   move verb-count-record to verb-count-header-entry
                       (verb-count-header-index)
               end-if
           end-perform.

           perform until not listing-file-okay or
                       line-of-listing = c-first-procedure-line
               perform print-nonprocedure-line
               perform get-listing-source-line
           end-perform.
           set procedure-region to true.

           move c-total-verb-count to line-verb-count (1).
           move c-pitched-verb-count to line-verb-count (2).
           perform print-merged-line.
           perform get-listing-source-line.

           move c-first-procedure-line to expected-procedure-line.
           perform process-procedure-line
               until   not listing-file-okay or
                       not verb-count-file-okay.
           if verb-count-file-open close verb-count-file.
           set verb-count-file-open to false.

           perform print-statistics.

           go scan-for-program-id.

       exit-section.
           exit.
      /
       process-procedure-line section.
       a.
           read verb-count-file at end go drive-listing-to-completion.
           if not verb-count-file-okay go drive-listing-to-completion.

           add 1 to expected-procedure-line.
           if verb-count-record equal low-values go unreferenced-line.

           perform until not listing-file-okay or
                   line-of-listing not < expected-procedure-line
               perform print-merged-line
               perform get-listing-source-line
           end-perform.
           if not listing-file-okay
               display "premature eof on listing file; status="
                       display-file-status
               go exit-section.

           if line-of-listing not = expected-procedure-line
               display "line ", expected-procedure-line, convert
                       " is missing.".

           move execution-count of verb-count-record (1) to
               line-verb-count (1), totalulator.
           move execution-count of verb-count-record (2) to
               line-verb-count (2).
           move 1 to listing-line-pointer.
           inspect area-a-and-b
               tallying listing-line-pointer for leading spaces.

           if verb-type of verb-count-record (1) equal "x"
               add 1 to lines-containing-procedures
               add totalulator to total-procedure-count
               perform section-reference
           else if verb-type of verb-count-record (1) equal "p"
               add 1 to lines-containing-procedures
               add totalulator to total-procedure-count
               perform paragraph-reference
           else
               add 1 to lines-containing-statements
               perform statement-reference.

           if execution-count of verb-count-record (2) not equal 0
               move execution-count of verb-count-record (2)
                   to totalulator
               perform statement-reference
               if verb-type of verb-count-record (1) equal "x" or "p"
                   add 1 to lines-containing-statements.

           go exit-section.
      /
       unreferenced-line.
           go exit-section.

       section-reference.
           if current-paragraph < 500
               perform paragraph-reference
               if verb-candidate not equal spaces
                   set section-entry (current-paragraph) to true.

       paragraph-reference.
           perform get-next-nonliteral-word.
           if verb-candidate not equal spaces and
                       current-paragraph < 500
               add 1 to current-paragraph
               move verb-candidate to paragraph-name (current-paragraph)
               move totalulator to paragraph-count (current-paragraph)
               move line-of-listing to
                       paragraph-line (current-paragraph)
               set section-entry (current-paragraph) to false
           end-if.

       statement-reference.
           perform get-next-nonliteral-word.
           if verb-candidate not equal spaces
               search all verb-name
                   at end go to statement-reference
                   when verb-name (verb-name-index) = verb-candidate
                       set verb-count-index to verb-name-index
                       add totalulator to verb-count (verb-count-index)
                       add totalulator to total-statement-count
               end-search
           end-if.

       drive-listing-to-completion.
           perform print-merged-line.
           if line-of-listing < expected-procedure-line and
                       listing-file-okay
               perform get-listing-source-line
               go drive-listing-to-completion.

       exit-section.
           exit.
      /
       print-statistics section.
       a.
           move spaces to merged-record.
           string "summary statistics for "
                   c-verb-count-program-id delimited by size
               into merged-record.
           write merged-record after page.

           move spaces to merged-record.
           compute edit-number =
               c-end-procedure-line - identification-line + 1.
           perform left-justify-number.
           move left-justified-number to left-justify-hold-1.
           compute edit-number =
               c-end-procedure-line - c-first-procedure-line + 1.
           perform left-justify-number.
           string "the program contains " delimited by size
               left-justify-hold-1 delimited by space
               " lines, of which " delimited by size
               left-justified-number delimited by space
               " are procedure division lines." delimited by size
               into merged-record.
           write merged-record after 2 lines.

           move spaces to merged-record.
           move lines-containing-statements to edit-number.
           perform left-justify-number.
           string "the procedure division has " delimited by size
               left-justified-number delimited by space
               " lines which had cobol verbs executed at least once,"
                       delimited by size
               into merged-record.
           write merged-record after 2 lines.
           move spaces to merged-record.
           move lines-containing-procedures to edit-number.
           perform left-justify-number.
           string "and " delimited by size
               left-justified-number delimited by space
               " lines which had procedure-names "
               "executed at least once." delimited by size
               into merged-record.
           write merged-record after 1 lines.
      /
           move spaces to merged-record.
           move c-total-verb-count to edit-number.
           perform left-justify-number.
           move left-justified-number to left-justify-hold-1.
           move total-statement-count to edit-number.
           perform left-justify-number.
           move left-justified-number to left-justify-hold-2.
           move total-procedure-count to edit-number.
           perform left-justify-number.
           string "there were " delimited by size
               left-justify-hold-1 delimited by space
               " verb executions counted, of which " delimited by size
               left-justify-hold-2 delimited by space
               " were cobol statements and " delimited by size
               left-justified-number delimited by space
               " were procedures." delimited by size
               into merged-record.
           write merged-record after 2 lines.

           if c-pitched-verb-count not = 0
               move spaces to merged-record
               move c-pitched-verb-count to edit-number
               perform left-justify-number
               string "there were " delimited by size
                   left-justified-number delimited by space
                   " counts unattributed." delimited by size
                   into merged-record
               write merged-record after 2 lines.

      /
           write merged-record
                   from "verb execution counts ordered by verb"
                   after 3 lines.
           write merged-record
                   from "--total  --  verb"
                   after 2 lines.
           write merged-record
                   from "  count   %  name"
                   after 1 line.
           write merged-record from space after 1 line.

           perform varying verb-name-index from 1 by 1
                       until verb-name-index > 43
               set verb-count-index to verb-name-index
               move spaces to merged-verb-line
               move verb-name (verb-name-index) to merge-verb-name
               if verb-count (verb-count-index) not = 0
                   move verb-count (verb-count-index) to
                           merge-verb-count
                   compute merge-verb-percent rounded =
                           (verb-count (verb-count-index) * 100)
                           / total-statement-count
               end-if
               write merged-verb-line after 1 line
           end-perform.

           perform varying j from 1 by 1 until j = 43
               perform varying i from j by 1 until i > 43
                   if verb-count (j) < verb-count (i)
                       move verb-count (j) to verb-count-hold
                       move verb-count (i) to verb-count (j)
                       move verb-count-hold to verb-count (i)
                       move verb-name (j) to verb-name-hold
                       move verb-name (i) to verb-name (j)
                       move verb-name-hold to verb-name (i)
                   end-if
               end-perform
           end-perform.

           write merged-record
                   from "verb execution counts ordered by count"
                   after page.
           write merged-record from "--total  --  verb"
                   after 2 lines.
           write merged-record from "  count   %  name"
                   after 1 line.
           write merged-record from space after 1 line.

           perform varying verb-count-index from 1 by 1
                       until verb-count-index > 43 or
                           verb-count (verb-count-index) = 0
               set verb-name-index to verb-count-index
               move verb-count (verb-count-index) to merge-verb-count
               move verb-name (verb-name-index) to merge-verb-name
               compute merge-verb-percent rounded =
                       (verb-count (verb-count-index) * 100)
                       / total-statement-count
               write merged-verb-line after 1 line
           end-perform.

           perform varying j from 1 by 1 until j = 43
               perform varying i from j by 1 until i > 43
                   if verb-name (j) > verb-name (i)
                       move verb-count (j) to verb-count-hold
                       move verb-count (i) to verb-count (j)
                       move verb-count-hold to verb-count (i)
                       move verb-name (j) to verb-name-hold
                       move verb-name (i) to verb-name (j)
                       move verb-name-hold to verb-name (i)
                   end-if
               end-perform
           end-perform.

           perform varying j from 1 by 1
                       until j = current-paragraph
               perform varying i from j by 1
                       until i > current-paragraph
                   if paragraph-count (j) < paragraph-count (i)
                       move paragraph-entry (j) to paragraph-entry-hold
                       move paragraph-entry (i) to paragraph-entry (j)
                       move paragraph-entry-hold to paragraph-entry (i)
                   end-if
               end-perform
           end-perform.

           if suppress-procedures go exit-section.

           perform procedure-report-heading.

           perform varying i from 1 by 1 until i > current-paragraph
               move paragraph-count (i) to merge-procedure-count
               move paragraph-line  (i) to merge-procedure-line
               compute merge-procedure-percent rounded =
                   (paragraph-count (i) * 100) / total-procedure-count
               if section-entry (i)
                   move spaces to merge-procedure-name
                   string paragraph-name (i) delimited by space
                       " section" delimited by size
                       into merge-procedure-name
                   end-string
               else
                   move paragraph-name (i) to merge-procedure-name
               end-if
               if procedure-report-line > 55
                   perform procedure-report-heading
               end-if
               write merged-procedure-line after 1 line
               add 1 to procedure-report-line
           end-perform.

       exit-section.
           exit.
      /
       merge-count-files section.
       a.
           open input verb-count-file.
           if not verb-count-file-okay
               go exit-section.

           perform varying verb-count-header-index from 1 by 1
                   until verb-count-header-index > 6 or
                       not verb-count-file-okay
               read verb-count-file
               if verb-count-file-okay
                   move verb-count-record to verb-count-header-entry
                       (verb-count-header-index)
               end-if
           end-perform.

           open i-o total-count-file.
           if not total-count-file-okay
               if total-count-file-not-found
                   perform copy-verb-count
               else
                   close verb-count-file
               end-if
               go exit-section
           end-if.

           perform varying total-count-header-index from 1 by 1
                   until total-count-header-index > 6 or
                       not total-count-file-okay
               read total-count-file
               if total-count-file-okay
                   move total-count-record to total-count-header-entry
                       (total-count-header-index)
               end-if
           end-perform.

           if c-verb-count-program-id not = t-verb-count-program-id  or
              c-first-procedure-line  not = t-first-procedure-line   or
              c-end-procedure-line    not = t-end-procedure-line
               display " file mismatch - data collection file differ's
      -        "from total count file! "
               display " skipping update... total count file"
               go close-and-exit-section
           end-if

           add c-total-verb-count to t-total-verb-count.
           add c-pitched-verb-count to t-pitched-verb-count.

           close total-count-file.
           open i-o total-count-file.

           perform varying total-count-header-index from 1 by 1
                   until total-count-header-index > 6 or
                       not total-count-file-okay
               read total-count-file
               if total-count-file-okay
                   move
                   total-count-header-entry (total-count-header-index)
                   to
                   total-count-record
               rewrite total-count-record
               end-if
           end-perform.

           perform varying i from 1 by 1 until i >
                   c-end-procedure-line - c-first-procedure-line
               read verb-count-file
               read total-count-file
               add execution-count of verb-count-file (1) to
                   execution-count of total-count-file (1)
               add execution-count of verb-count-file (2) to
                   execution-count of total-count-file (2)
               rewrite total-count-record
               if not total-count-file-okay
                   display display-file-status,
                       " error rewriting total count file."
               end-if
           end-perform.

       close-and-exit-section.
           close verb-count-file total-count-file.

       exit-section.
           exit.

      /
       copy-verb-count section.
       a.
           open output total-count-file.
           if not total-count-file-okay
               display display-file-status,
                       " error opening total count file."
               close verb-count-file
               go exit-section.

           perform varying total-count-header-index from 1 by 1
                   until total-count-header-index > 6 or
                       not total-count-file-okay
               set verb-count-header-index to total-count-header-index
               move verb-count-header-entry (verb-count-header-index)
                       to total-count-record
               write total-count-record
           end-perform.

           perform varying i from 1 by 1 until i >
                   c-end-procedure-line - c-first-procedure-line
               read verb-count-file
               write total-count-record from verb-count-record
               if not total-count-file-okay
                   display display-file-status,
                       " error writing total count file."
               end-if
           end-perform.

       close-and-exit-section.
           close verb-count-file total-count-file.

       exit-section.
           exit.
      /
       miscellaneous section.
       get-listing-source-line.
           perform read-listing-line.
           if listing-file-okay
               if line-number-low-digit not numeric
                   if procedure-region perform print-merged-line
                   else                perform print-nonprocedure-line
                   end-if
                   go get-listing-source-line.
           move listing-line-number to line-of-listing.
           move 1 to listing-line-pointer.
           if not-a-comment-line
               inspect area-a-and-b
                   tallying listing-line-pointer for leading spaces
           else move 99 to listing-line-pointer.

       get-next-nonliteral-word.
           move spaces to verb-candidate.
           unstring area-a-and-b delimited by all space
               into verb-candidate pointer listing-line-pointer.
           if literal-found
               move literal-delimiter to literal-holder
               move 0 to i
               inspect verb-candidate tallying i for all literal-holder
               if i not = 2
                   unstring area-a-and-b delimited by literal-holder
                       into verb-candidate pointer listing-line-pointer
               end-if
               go get-next-nonliteral-word
           end-if.
           if verb-candidate equal "." go get-next-nonliteral-word.
           inspect verb-candidate replacing first "." by space.

       left-justify-number.
           move 1 to i.
           inspect edit-number-alpha tallying i for leading spaces.
           move spaces to left-justified-number.
           unstring edit-number-alpha delimited by all space
               into left-justified-number with pointer i.

       procedure-report-heading.
           write merged-record from
               "  count     %  line  paragraph/section"
                   after page.
           write merged-record from space after 1 line.
           move 2 to procedure-report-line.

       read-listing-line.
           move spaces to listing-line.
           read listing-file.
      /
       print-merged-line.
           if not suppress-listing
               move line-verb-count (1) to merge-line-count (1)
               move line-verb-count (2) to merge-line-count (2)

               if (line-number-low-digit numeric or
                       listing-header-found)
                   move listing-source-image to merge-source-image
                   if listing-header-found
                       move "count1 count2" to merge-count-headers
                       move " line" to merge-line-header
                   else move line-of-listing to merge-line-number
                   end-if
               else move listing-line to merged-record
               end-if

               if page-ejector write merged-record after page
               else            write merged-record after 1 line
               end-if

           end-if.
           move 0 to line-verb-count (1), line-verb-count (2).

       print-nonprocedure-line.
           if not suppress-nonprocedure-lines
               perform print-merged-line
           else
               move 0 to line-verb-count (1), line-verb-count (2).
