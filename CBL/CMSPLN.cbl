       IDENTIFICATION DIVISION.
       PROGRAM-ID. CMSPLN.
       AUTHOR. John Doe.
       COPY SCRNIO.
      *-----------------------------------------------------
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DATE-OF-SALE.
          03 YEAR-OF-SALE      PIC 9(2)  VALUE ZEROS.
          03 MONTH-OF-SALE     PIC 9(2)  VALUE ZEROS.
          03 DAY-OF-SALE       PIC 9(2)  VALUE ZEROS.
       01 NUM-FIELDS           PIC 9     VALUE ZERO.
          88 ONLY-LAST-NAME              VALUE 1.
       01 CATEGORY-OF-SALE     PIC X(4)  VALUE SPACES.
          88 CAT-ANTI                    VALUE 'ANTI'.
          88 CAT-CRAF                    VALUE 'CRAF'.
          88 CAT-HOLI                    VALUE 'HOLI'.
          88 CAT-JEWL                    VALUE 'JEWL'.
          88 CAT-MISC                    VALUE 'MISC'.
          88 CAT-XMAS                    VALUE 'XMAS'.
       01 SWITCHES.
          03 SW-SALE           PIC X     VALUE SPACE.
             88 SALE-ITEM                VALUE 'Y'.
      *---------------------BEGIN-PAN2SCR-------------------
       COPY WSSCRN.
       01 SC-NAME              PIC X(40) VALUE SPACES.
       01 SC-LAST-NAME         PIC X(30) VALUE SPACES.
       01 SC-FIRST-NAME        PIC X(30) VALUE SPACES.
       01 SC-DATE-OF-SALE      PIC 9(6) VALUE ZEROS.
       01 SC-FULL-PRICE        PIC 9(4)V9(2) VALUE ZEROS.
       01 SC-DISCOUNT-PERCENT  PIC 9(2) VALUE ZEROS.
       01 SC-SALE-PRICE        PIC 9(4)V9(2) VALUE ZEROS.
       01 SC-GROUP-ID          PIC X VALUE SPACES.
       01 SC-COMMISSION-PLAN   PIC X VALUE SPACES.
       01 SC-PERCENT           PIC 9(2) VALUE ZEROS.
       01 SC-COMMISSION        PIC 9(4)V9(2) VALUE ZEROS.
      *-----------------------------------------------------
       SCREEN SECTION.
       01 CMSPLN-SCREEN
          BLANK SCREEN, AUTO,
          FOREGROUND-COLOR IS 7,
          BACKGROUND-COLOR IS 1.
          03 LINE 02 COLUMN 39 VALUE 'Commission Plan Entry'
                               HIGHLIGHT.
          03 LINE 05 COLUMN 14 VALUE 'Name:'.
          03 LINE 05 COLUMN 20 PIC X(40)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-NAME.
          03 LINE 06 COLUMN 09 VALUE 'Last Name:'.
          03 LINE 06 COLUMN 20 PIC X(30)
                               REVERSE-VIDEO
                               FROM SC-LAST-NAME.
          03 LINE 07 COLUMN 08 VALUE 'First Name:'.
          03 LINE 07 COLUMN 20 PIC X(30)
                               REVERSE-VIDEO
                               FROM SC-FIRST-NAME.
          03 LINE 09 COLUMN 06 VALUE 'Date of Sale:'.
          03 LINE 09 COLUMN 20 PIC 9(2)/9(2)/9(2)
                               REVERSE-VIDEO
                               FROM SC-DATE-OF-SALE.
          03 LINE 10 COLUMN 08 VALUE 'Full Price:'.
          03 LINE 10 COLUMN 20 PIC Z,ZZZ.9(2)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-FULL-PRICE.
          03 LINE 12 COLUMN 02 VALUE 'Discount Percent:'.
          03 LINE 12 COLUMN 20 PIC 9(2)
                               REVERSE-VIDEO
                               FROM SC-DISCOUNT-PERCENT.
          03 LINE 12 COLUMN 26 VALUE '%'.
          03 LINE 13 COLUMN 08 VALUE 'Sale Price:'.
          03 LINE 13 COLUMN 20 PIC Z,ZZZ.9(2)
                               REVERSE-VIDEO
                               FROM SC-SALE-PRICE.
          03 LINE 15 COLUMN 10 VALUE 'Group ID:'.
          03 LINE 15 COLUMN 20 PIC X
                               REVERSE-VIDEO
                               FROM SC-GROUP-ID.
          03 LINE 16 COLUMN 03 VALUE 'Commission Plan:'.
          03 LINE 16 COLUMN 20 PIC X
                               REVERSE-VIDEO
                               FROM SC-COMMISSION-PLAN.
          03 LINE 17 COLUMN 11 VALUE 'Percent:'.
          03 LINE 17 COLUMN 20 PIC 9(2)
                               REVERSE-VIDEO
                               FROM SC-PERCENT.
          03 LINE 17 COLUMN 26 VALUE '%'.
          03 LINE 18 COLUMN 08 VALUE 'Commission:'.
          03 LINE 18 COLUMN 20 PIC Z,ZZZ.9(2)
                               REVERSE-VIDEO
                               FROM SC-COMMISSION.
      *----------------------END-PAN2SCR--------------------
       PROCEDURE DIVISION.
           ACCEPT DATE-OF-SALE FROM DATE
           MOVE DATE-OF-SALE TO SC-DATE-OF-SALE
           SET SALE-ITEM TO TRUE

           DISPLAY CMSPLN-SCREEN
           ACCEPT CMSPLN-SCREEN

           PERFORM CALC-DISCOUNT
           PERFORM CALC-COMMISSION
           PERFORM SPLIT-NAME

           DISPLAY CMSPLN-SCREEN
           STOP RUN
           .
      *-----------------------------------------------------
       CALC-COMMISSION.
           EVALUATE TRUE ALSO TRUE
               WHEN SC-SALE-PRICE >= 1000
                       ALSO SC-SALE-PRICE * .5 > 750.00
                   MOVE 'A' TO SC-COMMISSION-PLAN
                   MOVE 750.00 TO SC-COMMISSION
               WHEN SC-SALE-PRICE >= 1000 ALSO ANY
                   MOVE 'A' TO SC-COMMISSION-PLAN
                   MOVE 50 TO SC-PERCENT
                   COMPUTE SC-COMMISSION = SC-SALE-PRICE * .5
               WHEN SC-SALE-PRICE >= 500
                        ALSO SC-SALE-PRICE * .25 > 150.00
                   MOVE 'B' TO SC-COMMISSION-PLAN
                   MOVE 150.00 TO SC-COMMISSION
               WHEN SC-SALE-PRICE >= 500 ALSO ANY
                   MOVE 'B' TO SC-COMMISSION-PLAN
                   MOVE 25 TO SC-PERCENT
                   COMPUTE SC-COMMISSION ROUNDED = SC-SALE-PRICE * .25
               WHEN SC-SALE-PRICE >= 250
                        ALSO SC-SALE-PRICE * .10 > 30.00
                   MOVE 'C' TO SC-COMMISSION-PLAN
                   MOVE 30.00 TO SC-COMMISSION
               WHEN SC-SALE-PRICE >= 250 ALSO ANY
                   MOVE 'C' TO SC-COMMISSION-PLAN
                   MOVE 10 TO SC-PERCENT
                   COMPUTE SC-COMMISSION ROUNDED = SC-SALE-PRICE * .10
               WHEN OTHER
                   MOVE 'O' TO SC-COMMISSION-PLAN
                   MOVE  5  TO SC-PERCENT
                   COMPUTE SC-COMMISSION ROUNDED = SC-SALE-PRICE * .05
                   IF SC-COMMISSION < 1.00
                       MOVE 1.00 TO SC-COMMISSION
                   END-IF
                   IF SC-COMMISSION > SC-SALE-PRICE
                       COMPUTE SC-COMMISSION = SC-SALE-PRICE * .75
                   END-IF
           END-EVALUATE
           .
      *-----------------------------------------------------
       SPLIT-NAME.
           UNSTRING SC-NAME DELIMITED BY ALL SPACE
               INTO SC-FIRST-NAME, SC-LAST-NAME
               TALLYING IN NUM-FIELDS
           END-UNSTRING
           IF ONLY-LAST-NAME
               MOVE SC-FIRST-NAME TO SC-LAST-NAME
               INITIALIZE SC-FIRST-NAME
           END-IF
           EVALUATE SC-LAST-NAME(1:1)
                   ALSO SC-FIRST-NAME(1:1)
               WHEN 'A' THRU 'F' ALSO 'A' THRU 'F'
                   MOVE 1 TO SC-GROUP-ID
               WHEN 'A' THRU 'F' ALSO 'G' THRU 'N'
                   MOVE 2 TO SC-GROUP-ID
               WHEN 'A' THRU 'F' ALSO ANY
                   MOVE 3 TO SC-GROUP-ID
               WHEN 'G' THRU 'N' ALSO 'A' THRU 'F'
                   MOVE 4 TO SC-GROUP-ID
               WHEN 'G' THRU 'N' ALSO 'G' THRU 'N'
                   MOVE 5 TO SC-GROUP-ID
               WHEN 'G' THRU 'N' ALSO ANY
                   MOVE 6 TO SC-GROUP-ID
               WHEN ANY ALSO 'A' THRU 'F'
                   MOVE 7 TO SC-GROUP-ID
               WHEN ANY ALSO 'G' THRU 'N'
                   MOVE 8 TO SC-GROUP-ID
               WHEN OTHER
                   MOVE 9 TO SC-GROUP-ID
           END-EVALUATE
           .
      *-----------------------------------------------------
       CALC-DISCOUNT.
           EVALUATE SALE-ITEM ALSO MONTH-OF-SALE ALSO TRUE
               WHEN TRUE ALSO  1 THRU  3 ALSO CAT-ANTI
               WHEN TRUE ALSO  1 THRU  3 ALSO CAT-JEWL
               WHEN TRUE ALSO  1 THRU  3 ALSO CAT-MISC
               WHEN TRUE ALSO  4 THRU  6 ALSO CAT-XMAS
               WHEN TRUE ALSO  4 THRU  6 ALSO CAT-CRAF
               WHEN TRUE ALSO 10 THRU 12 ALSO CAT-ANTI
                   MOVE 50 TO SC-DISCOUNT-PERCENT
                   COMPUTE SC-SALE-PRICE = SC-FULL-PRICE * .5
               WHEN TRUE ALSO  1 THRU  3 ALSO CAT-XMAS
               WHEN TRUE ALSO  1 THRU  3 ALSO CAT-CRAF
                   MOVE 75 TO SC-DISCOUNT-PERCENT
                   COMPUTE SC-SALE-PRICE = SC-FULL-PRICE * .25
               WHEN TRUE ALSO  4 THRU  6 ALSO CAT-ANTI
               WHEN TRUE ALSO  4 THRU  6 ALSO CAT-JEWL
               WHEN TRUE ALSO  4 THRU  6 ALSO CAT-MISC
               WHEN TRUE ALSO  6 THRU  9 ALSO ANY
                   MOVE 25 TO SC-DISCOUNT-PERCENT
                   COMPUTE SC-SALE-PRICE = SC-FULL-PRICE * .75
               WHEN TRUE ALSO  1 THRU  3 ALSO ANY
               WHEN TRUE ALSO  4 THRU  6 ALSO ANY
               WHEN TRUE ALSO 10 THRU 12 ALSO ANY
                   MOVE 10 TO SC-DISCOUNT-PERCENT
                   COMPUTE SC-SALE-PRICE = SC-FULL-PRICE * .9
               WHEN OTHER
                   MOVE SC-FULL-PRICE TO SC-SALE-PRICE
           END-EVALUATE
           .

