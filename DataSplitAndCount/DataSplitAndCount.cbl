       identification division.
       program-id. Edit.

       environment division.
       input-output section.
       file-control.

           select input-file
               assign to "../../../data/project6.dat"
               organization is line sequential.

           select valid-file
               assign to "../../../data/valid.dat"
               organization is line sequential.

           select invalid-file
               assign to "../../../data/invalid.dat"
               organization is line sequential.

           select report-file
               assign to "../../../data/report.out"
               organization is line sequential.

     
      *
       data division.
       file section.
      *
       fd input-file
           data record is input-line
           record contains 36 characters.

       01 input-line.
          05 TransactionCode                pic X.
             88 Transactioncode-88-valid
                value 'S', 'R', 'L'.
          05 TransactionAmount              pic 9(5)V99.
          05 PaymentType                    pic XX.
             88 Paymentype-88-valid
                value 'CA', 'CR', 'DB'.
          05 StoreNumber                    pic 99.
             88 Storenumber-valid
                value 01, 02, 03, 04, 05, 12.
          05 InvoiceNumber.
             10 alphabets-invoicenumber     pic x(2).
                88 xx-invoicenumber-88-valid
                value 'AB','AC','AD','AE',
                      'BA','BC','BD','BE',
                      'CA','CB','CD','CE',
                      'DA','DB','DC','DE',
                      'EA','EB','EC','ED'.
             10 dash                        pic x(1) value '-'.
             10 number-invoicenumber        pic 9(6).
          05 SKUCode                        pic X(15).


       fd valid-file       
          data record is valid-line
          record contains 36 characters.
             
       01 valid-line                        pic x(36).

       fd invalid-file
          data record is invalid-line
          record contains 36 characters.
             
       01 invalid-line                      pic x(36).
       
       fd report-file
       data record is report-line
       record contains 120 characters.
             
       01 report-line                       pic x(60).

       working-storage section.

       01 ws-errors-in-data-file.
           05 ws-error-in-transactioncode     pic x(28) 
               value "INVALID TRANSACTION CODE".
           05 ws-error-in-transactionamount   pic x(33) 
               value "INVALID TRANSACTION AMOUNT".
           05 ws-error-in-paymenttype         pic x(43) 
               value "INVALID PAYMENT TYPE".
           05 ws-error-in-store-number        pic x(20) 
               value "INVALID STORE NUMBER".
           05 ws-error-in-invoice-code        pic x(36) 
               value  "First two characters cannot be same".
           05 ws-error-in-code-range          pic x(47) 
               value "First two characters need to be A, B, C, D or E".
           05 ws-error-in-dash                pic x(48) 
               value " '-' should be at third position in invoice code".
           05 ws-error-in-rangeofinvoice      pic x(46) 
               value "Invoice number must be between 900000 & 100000".
           05 ws-error-in-typeofinvoice       pic x(29) 
               value "Invoice number is not numeric".
           05 ws-error-in-SKUCodeempty        pic x(36) 
               value "SKU code cannot be empty.".

       01 report-line-1.
           05 filler                          pic x(22) value spaces.
           05 filler                          pic x(19)
               value "EDIT PROGRAM RESULT".
           05 filler                          pic x(19) value spaces.
         
       01 ws-record-with-error.
           05 filler                          pic x(9)
               value "Record  :".
           05 ws-record-num-data              pic 9(3).
           05 filler                          pic x(10).
           05 ws-original-record              pic x(36).
           05 filler                          pic x(6).

       01 output-line.
           05 filler                          pic x(15)
               value "Valid records: ".
           05 filler                          pic x(1).
           05 valid-records                   pic 9(3).
           05 filler                          pic x(4).
           05 filler                          pic x(17)
               value "Invalid records: ".
           05 filler                          pic x(1).
           05 invalid-records                 pic 999.
           05 filler                          pic x(30).

       01 ws-summary-heading.
           05 filler                        pic x(34)
               value "---------------------------SUMMARY".
           05 filler                        pic x(26)
               value "--------------------------".

       01 ws-page-heading.
           05 filler                        pic x(22) value spaces.
           05 filler                        pic x(36)
               value "INVALID RECORDS               PAGE ".
           05 ws-page-count                 pic 99 value 0.



       77 ws-eof-flag                       pic x value 'n'.
       77 ws-record-count                   pic 999 value 0.
       77 ws-erros-per-rec                  pic 99.
       77 ws-valid-entry                    pic 999.
       77 ws-invalid-entry                  pic 999.
       77 ws-one                            pic 9 value 1.
       77 ws-one-lakh                       pic 9(6) value 100000.
       77 ws-nine-lakh                      pic 9(6) value 900000.
      *77 ws-page-count                     pic 99 value 0.
       77 ws-zero                           pic 9 value 0.
       77 ws-lines-per-page                 pic 99 value 9.
       77 ws-line-count                     pic 99 value 0.


       01 ws-blank-line                     pic x(60) value spaces.

       01 ws-invoice-code-separate.
         05 ws-invoice-1                    pic x.
         05 ws-invoice-2                    pic x.

       procedure division.
       000-main.
      
      * Open files
           open input  input-file.
           open output valid-file,
                       invalid-file,
                       report-file.
           

      * Initial read of input file
           read input-file
               at end
               move 'y' to ws-eof-flag.
      
      * write report heading
           write report-line from report-line-1.
           move spaces                      to report-line.

      * Process each input record and read in next record
      
           perform 100-process-pages
               varying ws-page-count       from 1 by 1
               until ws-eof-flag = 'y'.

           perform 400-print-summary.

      * Close files and end program    
           close input-file,
                 valid-file,
                 invalid-file,
                 report-file.
      *
           goback.
      *
       100-process-pages.

      * Process page headings in perform loop
           perform 200-print-headings.

           
           perform 300-process-lines 
      *        varying ws-line-count from 1 by 1       
               until (   ws-line-count > ws-lines-per-page  
                      OR ws-eof-flag   = "y"). 
              
       200-print-headings.
      *    add 1                            to ws-page-count.
           move spaces                      to report-line.
           move 0                           to ws-line-count.
      *
      * Write the report heading with a blank line before heading
      * and blank line after heading
      *
           if (ws-page-count > 1) then
               write report-line
                   after advancing page
               write report-line
               write report-line from ws-page-heading 
              
               write report-line from ws-blank-line
           else
               write report-line
               write report-line from ws-page-heading 
               write report-line from ws-blank-line
           end-if.

       300-process-lines.
     
           add 1                            to ws-record-count.
           move ws-record-count             to ws-record-num-data.
      *    move input-line                  to ws-raw-data.
           move 0                           to ws-erros-per-rec.
           
           
      
           

      *Perform data validation
           if not Transactioncode-88-valid then
               add ws-one                   to ws-erros-per-rec
               write report-line from ws-error-in-transactioncode
           end-if.

           if TransactionAmount is not numeric then
               add ws-one                   to ws-erros-per-rec
               write report-line from ws-error-in-transactionamount
           end-if.

           if not Paymentype-88-valid then
               add ws-one                   to ws-erros-per-rec
               write report-line from ws-error-in-paymenttype
           end-if.
               
           if not Storenumber-valid then
               add ws-one                   to ws-erros-per-rec
               write report-line from ws-error-in-store-number
           end-if.

           if not xx-invoicenumber-88-valid then
               add ws-one                   to ws-erros-per-rec
               write report-line from ws-error-in-invoice-code
           end-if.

           if dash is not = "-" then
               add ws-one                   to ws-erros-per-rec
               write report-line from ws-error-in-dash
           end-if.

           if number-invoicenumber is not numeric
               add ws-one                   to ws-erros-per-rec
               write report-line from ws-error-in-typeofinvoice
           else
           end-if.

           if not (number-invoicenumber > ws-one-lakh and 
           number-invoicenumber < ws-nine-lakh)
               add ws-one                   to ws-erros-per-rec
               write report-line from ws-error-in-rangeofinvoice
           else
           end-if.

           if (SKUCode = spaces) then
               add ws-one                   to ws-erros-per-rec
               write report-line from ws-error-in-SKUCodeempty
           else
           end-if.


           if ws-erros-per-rec > ws-zero then
               add ws-one                   to ws-invalid-entry
               add 1                        to ws-line-count
               move ws-record-count         to ws-record-num-data
               move input-line              to ws-original-record
               write report-line from ws-record-with-error
               write report-line from ws-blank-line
               write invalid-line from input-line
           else
               add ws-one                   to ws-valid-entry
               write valid-line from input-line
           end-if.

          read input-file
               at end
                   move 'y' to ws-eof-flag.
           
      
       400-print-summary.
           move ws-valid-entry              to valid-records.
           move ws-invalid-entry            to invalid-records.
           write report-line from ws-blank-line.
           write report-line from ws-summary-heading.
           write report-line from ws-blank-line.
           write report-line from output-line.


       end program Edit.