       IDENTIFICATION DIVISION.
       PROGRAM-ID. CMSPLN.
       AUTHOR. John Doe.
       COPY SCRNIO.
      *-----------------------------------------------------
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM-FIELDS           PIC 9     VALUE ZERO.
          88 ONLY-LAST-NAME              VALUE 1.
      *---------------------BEGIN-PAN2SCR-------------------
       COPY WSSCRN.
       01 SC-NAME              PIC X(40) VALUE SPACES.
       01 SC-LAST-NAME         PIC X(30) VALUE SPACES.
       01 SC-FIRST-NAME        PIC X(30) VALUE SPACES.
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
          03 LINE 05 COLUMN 13 VALUE 'Name:'.
          03 LINE 05 COLUMN 19 PIC X(40)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-NAME.
          03 LINE 06 COLUMN 08 VALUE 'Last Name:'.
          03 LINE 06 COLUMN 19 PIC X(30)
                               REVERSE-VIDEO
                               FROM SC-LAST-NAME.
          03 LINE 07 COLUMN 07 VALUE 'First Name:'.
          03 LINE 07 COLUMN 19 PIC X(30)
                               REVERSE-VIDEO
                               FROM SC-FIRST-NAME.
          03 LINE 08 COLUMN 07 VALUE 'Sale Price:'.
          03 LINE 08 COLUMN 19 PIC Z,ZZZ.9(2)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-SALE-PRICE.
          03 LINE 09 COLUMN 09 VALUE 'Group ID:'.
          03 LINE 09 COLUMN 19 PIC X
                               REVERSE-VIDEO
                               FROM SC-GROUP-ID.
          03 LINE 10 COLUMN 02 VALUE 'Commission Plan:'.
          03 LINE 10 COLUMN 19 PIC X
                               REVERSE-VIDEO
                               FROM SC-COMMISSION-PLAN.
          03 LINE 11 COLUMN 10 VALUE 'Percent:'.
          03 LINE 11 COLUMN 19 PIC 9(2)
                               REVERSE-VIDEO
                               FROM SC-PERCENT.
          03 LINE 11 COLUMN 25 VALUE '%'.
          03 LINE 12 COLUMN 07 VALUE 'Commission:'.
          03 LINE 12 COLUMN 19 PIC Z,ZZZ.9(2)
                               REVERSE-VIDEO
                               FROM SC-COMMISSION.
      *----------------------END-PAN2SCR--------------------
       PROCEDURE DIVISION.
           DISPLAY CMSPLN-SCREEN
           ACCEPT CMSPLN-SCREEN
      *-----------------------------------------------------
           EVALUATE TRUE
               WHEN SC-SALE-PRICE >= 1000
                   MOVE 'A' TO SC-COMMISSION-PLAN
                   MOVE 50 TO SC-PERCENT
               WHEN SC-SALE-PRICE >= 500
                   MOVE 'B' TO SC-COMMISSION-PLAN
                   MOVE 25 TO SC-PERCENT
               WHEN SC-SALE-PRICE >= 250
                   MOVE 'C' TO SC-COMMISSION-PLAN
                   MOVE 25 TO SC-PERCENT
               WHEN OTHER
                   MOVE 'O' TO SC-COMMISSION-PLAN
                   MOVE 5 TO SC-PERCENT
           END-EVALUATE
           COMPUTE SC-COMMISSION ROUNDED =
                   SC-SALE-PRICE * SC-PERCENT / 100
      *-----------------------------------------------------
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
      *-----------------------------------------------------
           DISPLAY CMSPLN-SCREEN
           STOP RUN
           .
