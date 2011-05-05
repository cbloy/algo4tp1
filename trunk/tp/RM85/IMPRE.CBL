       identification division.
       program-id.  "prntest".
      *
      *    title:  prntest.cbl
      *        rm/cobol-85 printer test
      *
      * the information contained herein is proprietary to ryan mcfarland
      *    corporation, and provided for maintenance purposes only.
      *    no other use of this material is authorized or permitted without
      *    specific authorization, in writing, from ryan mcfarland corporation.
      *
      * version identification:
      *   $revision:   5.0  $
      *   $date:   15 sep 1990  1:44:14  $
      *   $author:   bill  $
      *   $logfile:   u:\c85\dev\verify\vcs\prntest.cbv  $
      *
      *    module history:
      *     $log:   u:\c85\dev\verify\vcs\prntest.cbv  $
      *
      *       rev 5.0   15 sep 1990  1:44:14   bill
      *    no change.
      *
      *       rev 1.2   29 aug 1990 14:52:24   bill
      *    change program name to lower case for unix program library.
      *
      *       rev 1.1   14 aug 1990 16:24:36   bill
      *    improve spelling of program name.
      *
       environment division.
       configuration section.
       source-computer.  rmcobol-85.
       object-computer.  rmcobol-85.
       input-output section.
       file-control.
           select pfile assign to print, "printer".
      *
       data division.
       file section.
       fd  pfile.
       01  fd-record       pic x(80).
      *
       working-storage section.
       01  title                     pic x(60) value
           "rm/cobol-85 verify printer operation - version 5.00".

       01  ws-new-page               pic x(80) value "new page".
       01  ws-next-line              pic x(80) value
               "          next line".
       01  ws-five-lines             pic x(80) value
               "                    5 lines".
       01  ws-same-line              pic x(80) value
               "                              same line".
       01  ws-new-page-b             pic x(80) value
               "                                      new page next".
       01  ws-next-line-b            pic x(80) value
               "          line next".
       01  ws-five-lines-b           pic x(80) value
               "                    5 next".
       01  return-key                pic x.
      /
       procedure division.
       para-1.
           display title, low, line 1, erase.
           open output pfile.
           display "this section tests printer configuration."
                       low, line 10, position 20.
           display "make sure that the printer is attached and online.",
                       low, line 12, position 20.

           DISPLAY "PRUEBA DE IMPRESION"   LINE 15 POSITION 30


           write fd-record from ws-new-page
               after advancing page.
           write fd-record from ws-same-line
               after advancing 1 lines.
           write fd-record from ws-five-lines
               after advancing 5 lines.

               CLOSE pfile.
               STOP RUN.


       para-2.
           write fd-record from ws-new-page
               after advancing page.
           write fd-record from ws-same-line
               after advancing zero lines.
           write fd-record from ws-five-lines
               after advancing 5 lines.
           write fd-record from ws-same-line after advancing 0 lines.
           write fd-record from ws-next-line.
           write fd-record from ws-next-line.
           write fd-record from ws-new-page-b
               before advancing page.
           write fd-record from ws-same-line after advancing 0 lines.
           write fd-record from ws-five-lines-b
               before advancing 5 lines.
           write fd-record from ws-same-line after advancing 0 lines.
           write fd-record from ws-next-line-b
               before advancing 1 line.
           write fd-record from ws-next-line-b
               before advancing 1 line.
       program-end.
           close pfile.
