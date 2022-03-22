       identification division.
       program-id. Edit.

       environment division.
       input-output section.
       file-control.

           select input-file
               assign to "../../../data/project6.dat"
               organization is line sequential.

           select valid-file
               assign to "../../../data/valid.out"
               organization is line sequential.

           select invalid-file
               assign to "../../../data/invalid.out"
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
         05 TransactionCode pic X.
           88 Transactioncode-88-valid
                value 'S', 'R', 'L'.
         05 TransactionAmount pic 9(5)V99.
         05 PaymentType pic XX.
           88 Paymentype-valid
                value 'CA', 'CB', 'DB'.
         05 StoreNumber pic 99.
           88 Storenumber-valid
                value 01, 02, 03, 04, 05, 12.
         05 InvoiceNumber.
           10 alphabets-invoicenumber pic x(2).
             88 xx-invoicenumber-valid
                value 'AB', 'AC', 'AD', 'AE', 'BA', 'BC', 'BD', 'BE',
                'CA', 'CB', 'CD', 'CE', 'DA', 'DB', 'DC', 'DE', 'EA',
                'EB', 'EC', 'ED'.
           10 dash pic x(1) value '-'.
           10 number-invoicenumber pic 9(6).
         05 SKUCode pic X(15).

       fd valid-file
          data record is valid-line
          record contains 36 characters.

       01 valid-line pic x(23).

       fd invalid-file
          data record is invalid-line
          record contains 36 characters.

       01 invalid-line pic x(23).

       fd report-file
       data record is report-line
       record contains 120 characters.

       01 report-line pic x(120).

       working-storage section.

       01 ws-errors-in-data-file.
         05 ws-error-in-transactioncode pic x(28) value
                                        "Transaction code is invalid.".
         05 ws-error-in-transactionamount pic x(33) value
                                    "Transaction amount is not numeric".
         05 ws-error-in-paymenttype pic x(43) value
                          "Payment type is not valid i.e, CA, CR or DB".
         05 ws-error-in-store-number pic x(55) value
              "Store number is not valid (must be from 01 to 05 or 12)".
         05 ws-error-in-invoice-code pic x(36) value
                                  "First two characters cannot be same".
         05 ws-error-in-code-range pic x(47) value
                      "First two characters need to be A, B, C, D or E".
         05 ws-error-in-dash pic x(48) value
                     " '-' should be at third position in invoice code".
         05 ws-error-in-rangeofinvoice pic x(46) value
                       "Invoice number must be between 900000 & 100000".
         05 ws-error-in-typeofinvoice pic x(29) value
                                      "Invoice number is not numeric".
         05 ws-error-in-SKUCodeempty pic x(36) value
                                     "SKU code cannot be empty.".

       01 report-line-1.
         05 filler pic x(25) value spaces.
         05 filler pic x(19) value "EDIT PROGRAM RESULT".
         05 filler pic x(29) value spaces.

       01 ws-record-with-error.
         05 filler pic x(9) value "Record  :".
         05 ws-record-num-data pic 9(3).
         05 filler pic x(10).
         05 ws-original-record pic x(36).
         05 filler pic x(6).

       01 output-line.
         05 filler pic x(15) value "Valid records: ".
         05 filler pic x(1).
         05 valid-records pic 9(3).
         05 filler pic x(4).
         05 filler pic x(17) value "Invalid records: ".
         05 filler pic x(1).
         05 invalid-records pic 99.
         05 filler pic x(30).

       77 ws-eof-flag pic x value 'n'.
       77 ws-input-number-check pic 999.
       77 ws-error-number-count pic 99.
       77 ws-valid-entry pic 999.
       77 ws-invalid-entry pic 999.
       77 ws-one pic 9 value 1.
       77 ws-one-lakh pic 9(6) value 100000.
       77 ws-nine-lakh pic 9(6) value 900000.
       77 ws-page-count pic 99 value 0.
       procedure division.

           open input input-file.
           open output valid-file,
             invalid-file,
             report-file.

           write report-line from report-line-1.
           move spaces to report-line.

           read input-file
               at end
                   move 'y' to ws-eof-flag.

           perform 100-process-files
             varying ws-page-count from 1 by 1
             until ws-eof-flag = 'y'.

           move ws-valid-entry to valid-records.
           move ws-invalid-entry to invalid-records.
           write report-line from output-line.

           close input-file, valid-file, invalid-file, report-file.
           goback.

       100-process-files.

           perform 300-validation until ws-eof-flag = 'y'.

       300-validation.
           move spaces to report-line.

           add ws-one to ws-input-number-check.
           if not (Transactioncode-88-valid) then
               add ws-one to ws-error-number-count
               write report-line from ws-error-in-transactioncode
           end-if.

           if not (TransactionAmount is numeric) then
               add ws-one to ws-error-number-count
               write report-line from ws-error-in-transactionamount
           end-if.

           if not (Paymentype-valid) then
               add ws-one to ws-error-number-count
               write report-line from ws-error-in-paymenttype
           end-if.

           if not (Storenumber-valid) then
               add ws-one to ws-error-number-count
               write report-line from ws-error-in-store-number
           end-if.

           if not (xx-invoicenumber-valid) then
               add ws-one to ws-error-number-count
               write report-line from ws-error-in-invoice-code
           end-if.

           if not (dash = "-") then
               add ws-one to ws-error-number-count
               write report-line from ws-error-in-dash
           end-if.

           if not (number-invoicenumber is numeric)
               add ws-one to ws-error-number-count
               write report-line from ws-error-in-typeofinvoice
           end-if.

           if not (InvoiceNumber > ws-one-lakh and InvoiceNumber <
             ws-nine-lakh)
               add ws-one to ws-error-number-count
               write report-line from ws-error-in-rangeofinvoice
           end-if.

           if (SKUCode = space) then
               add ws-one to ws-error-number-count
               write report-line from ws-error-in-SKUCodeempty
           end-if.

           if (ws-error-number-count = 0) then
               add ws-one to ws-valid-entry
               write valid-line from input-line
           else
               add ws-one to ws-invalid-entry
               move ws-input-number-check to ws-record-num-data
               write report-line from ws-record-with-error
               write invalid-line from input-line
           end-if.
  -
           move zeroes to ws-error-number-count.

           goback.

       end program Edit.