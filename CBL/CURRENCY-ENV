       IDENTIFICATION DIVISION.
       PROGRAM-ID. CURRENCY-ENV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-AMOUNT             PIC 9(6)V99 VALUE 9876.54.
       01  WS-AMOUNT-FMT         PIC Z(6)9.99.
       01  WS-CURRENCY-SYMBOL    PIC X(8) VALUE SPACE.
       01  WS-OUTPUT-STR         PIC X(40).
       01  WS-ENV-VALUE          PIC X(32).

       PROCEDURE DIVISION.
           *> Read currency symbol from environment
           ACCEPT WS-ENV-VALUE FROM ENVIRONMENT "CURRENCY"

           IF FUNCTION TRIM(WS-ENV-VALUE) = SPACES
              MOVE "$" TO WS-CURRENCY-SYMBOL
           ELSE
              MOVE FUNCTION TRIM(WS-ENV-VALUE) TO WS-CURRENCY-SYMBOL
           END-IF

           MOVE WS-AMOUNT TO WS-AMOUNT-FMT

           STRING FUNCTION TRIM(WS-CURRENCY-SYMBOL TRAILING)
                  " "
                  WS-AMOUNT-FMT
                  DELIMITED BY SIZE
                  INTO WS-OUTPUT-STR
           END-STRING

           DISPLAY "Amount: " WS-OUTPUT-STR
           GOBACK.
