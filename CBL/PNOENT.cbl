       IDENTIFICATION DIVISION.
       PROGRAM-ID. PNOENT.
       AUTHOR. John Doe.
       COPY SCRNIO.
      *-----------------------------------------------------
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY WSSCRN.
       01 FORMATTED-NUMBER     PIC X(14) VALUE '(XXX) XXX-XXXX'.
       01 FORMATTED-ALTERNATE  PIC X(08) VALUE 'XXX-XXXX'.
       01 WS-STRING            PIC X(20) VALUE SPACES.
      *---------------------BEGIN-PAN2SCR-------------------
       01 SC-ENTER-PHONE-NUMBER PIC X(10) VALUE SPACES.
       01 SC-LAST-NAME         PIC X(20) VALUE SPACES.
       01 SC-FIRST-NAME        PIC X(20) VALUE SPACES.
       01 SC-OUTPUT-FIELDS.
           03 SC-EDITED-PHONE-NUMBER PIC X(14) VALUE SPACES.
           03 SC-COMBINED-NAME     PIC X(40) VALUE SPACES.
           03 SC-MESSAGE           PIC X(37) VALUE SPACES.
      *-----------------------------------------------------
       SCREEN SECTION.
       01 PNOENT-SCREEN
          BLANK SCREEN, AUTO,
          FOREGROUND-COLOR IS 7,
          BACKGROUND-COLOR IS 1.
          03 LINE 03 COLUMN 18 VALUE 'Phone Number Entry'
                               HIGHLIGHT.
          03 LINE 05 COLUMN 05 VALUE 'Enter Phone Number:'.
          03 LINE 05 COLUMN 25 PIC X(10)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-ENTER-PHONE-NUMBER.
          03 LINE 06 COLUMN 14 VALUE 'Last Name:'.
          03 LINE 06 COLUMN 25 PIC X(20)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-LAST-NAME.
          03 LINE 07 COLUMN 13 VALUE 'First Name:'.
          03 LINE 07 COLUMN 25 PIC X(20)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-FIRST-NAME.
          03 LINE 09 COLUMN 04 VALUE 'Edited Phone Number:'.
          03 LINE 09 COLUMN 25 PIC X(14)
                               REVERSE-VIDEO
                               FROM SC-EDITED-PHONE-NUMBER.
          03 LINE 10 COLUMN 10 VALUE 'Combined Name:'.
          03 LINE 10 COLUMN 25 PIC X(40)
                               REVERSE-VIDEO
                               FROM SC-COMBINED-NAME.
          03 LINE 22 COLUMN 16 VALUE 'Message:'.
          03 LINE 22 COLUMN 25 PIC X(37)
                               FOREGROUND-COLOR 4
                               FROM SC-MESSAGE.
          03 LINE 24 COLUMN 01 VALUE 'F1 - Exit'.
      *----------------------END-PAN2SCR--------------------
       PROCEDURE DIVISION.
           PERFORM UNTIL F1-PRESSED
               DISPLAY PNOENT-SCREEN
               ACCEPT PNOENT-SCREEN
               IF NOT F1-PRESSED
                   PERFORM INIT-FIELDS
                   PERFORM PROCESS-PHONE-NUMBER
                   PERFORM PROCESS-NAME
                   DISPLAY PNOENT-SCREEN
               END-IF
           END-PERFORM
           GOBACK
           .
       INIT-FIELDS.
           MOVE '(XXX) XXX-XXXX' TO FORMATTED-NUMBER
           MOVE 'XXX-XXXX' TO FORMATTED-ALTERNATE
           INITIALIZE SC-OUTPUT-FIELDS
           .
       PROCESS-NAME.
           MOVE FUNCTION TRIM(SC-LAST-NAME) TO WS-STRING
           MOVE WS-STRING TO SC-LAST-NAME

           MOVE FUNCTION TRIM(SC-FIRST-NAME) TO WS-STRING
           MOVE WS-STRING TO SC-FIRST-NAME

           STRING SC-LAST-NAME DELIMITED BY SPACE
                  ', '
                  SC-FIRST-NAME DELIMITED BY SIZE
                  INTO SC-COMBINED-NAME
           .
       PROCESS-PHONE-NUMBER.
      * initialize fields
           IF FUNCTION TRIM(SC-ENTER-PHONE-NUMBER) IS NOT NUMERIC
               MOVE 'ONLY ENTER DIGITS' TO SC-MESSAGE
               EXIT PARAGRAPH
           END-IF
      * trim phone-number
           MOVE FUNCTION TRIM(SC-ENTER-PHONE-NUMBER) TO WS-STRING
           MOVE WS-STRING TO SC-ENTER-PHONE-NUMBER
      * check value
           IF FUNCTION NUMVAL(SC-ENTER-PHONE-NUMBER) > 9999999 THEN
               INSPECT FORMATTED-NUMBER REPLACING
                   FIRST 'XXX'
                       BY SC-ENTER-PHONE-NUMBER(1:3)
                   FIRST 'XXX'
                       BY SC-ENTER-PHONE-NUMBER(4:3)
                   FIRST 'XXXX'
                       BY SC-ENTER-PHONE-NUMBER(7:4)
               MOVE FORMATTED-NUMBER
                   TO SC-EDITED-PHONE-NUMBER
           ELSE
               INSPECT FORMATTED-ALTERNATE REPLACING
                   FIRST 'XXX'
                       BY SC-ENTER-PHONE-NUMBER(1:3)
                   FIRST 'XXXX'
                       BY SC-ENTER-PHONE-NUMBER(4:4)
               MOVE FORMATTED-ALTERNATE
                   TO SC-EDITED-PHONE-NUMBER
           END-IF
           .
