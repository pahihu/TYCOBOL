       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLES.
       AUTHOR. John Doe.
       COPY SCRNIO.
      *-----------------------------------------------------
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY WSSCRN.
       COPY MONTHS.
       COPY STATESAB.
      *-----------------------------------------------------
       01 DATE-FIELD.
          03 MONTH-PORTION      PIC 9(2)  VALUE ZEROS.
          03 DAY-PORTION        PIC 9(2)  VALUE ZEROS.
          03 YEAR-PORTION       PIC 9(4)  VALUE ZEROS.
       01 UPPER-STATE           PIC X(20) VALUE SPACES.
       01 NUMERIC-STATE-IDX     PIC 9(2)  VALUE ZEROS.
      *---------------------BEGIN-PAN2SCR-------------------
       01 SC-ENTER-DATE        PIC 9(8) VALUE ZEROS.
       01 SC-STATE             PIC X(2) VALUE SPACES.
       01 SC-OUTPUT-FIELDS.
           03 SC-DATE              PIC 9(8) VALUE ZEROS.
           03 SC-EDITED-DATE       PIC X(20) VALUE SPACES.
           03 SC-STATE-INDEX       PIC 9(2) VALUE ZEROS.
           03 SC-CAPITAL           PIC X(15) VALUE SPACES.
           03 SC-STATE-NAME        PIC X(20) VALUE SPACES.
           03 SC-MESSAGE           PIC X(40) VALUE SPACES.
      *-----------------------------------------------------
       SCREEN SECTION.
       01 TABLES-SCREEN
          BLANK SCREEN, AUTO,
          FOREGROUND-COLOR IS 7,
          BACKGROUND-COLOR IS 1.
          03 LINE 03 COLUMN 37 VALUE 'Date Entry'
                               HIGHLIGHT.
          03 LINE 08 COLUMN 04 VALUE 'Enter Date:'.
          03 LINE 08 COLUMN 16 PIC 9(8)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-ENTER-DATE.
          03 LINE 08 COLUMN 38 VALUE '(MMDDYYYY)'.
          03 LINE 09 COLUMN 09 VALUE 'State:'.
          03 LINE 09 COLUMN 16 PIC X(2)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-STATE.
          03 LINE 11 COLUMN 10 VALUE 'Date:'.
          03 LINE 11 COLUMN 16 PIC 9(2)/9(2)/9(4)
                               REVERSE-VIDEO
                               FROM SC-DATE.
          03 LINE 12 COLUMN 03 VALUE 'Edited Date:'.
          03 LINE 12 COLUMN 16 PIC X(20)
                               REVERSE-VIDEO
                               FROM SC-EDITED-DATE.
          03 LINE 14 COLUMN 03 VALUE 'State Index:'.
          03 LINE 14 COLUMN 16 PIC Z9
                               REVERSE-VIDEO
                               FROM SC-STATE-INDEX.
          03 LINE 15 COLUMN 07 VALUE 'Capital:'.
          03 LINE 15 COLUMN 16 PIC X(15)
                               REVERSE-VIDEO
                               FROM SC-CAPITAL.
          03 LINE 16 COLUMN 04 VALUE 'State Name:'.
          03 LINE 16 COLUMN 16 PIC X(20)
                               REVERSE-VIDEO
                               FROM SC-STATE-NAME.
          03 LINE 22 COLUMN 07 VALUE 'Message:'.
          03 LINE 22 COLUMN 16 PIC X(40)
                               FOREGROUND-COLOR 4
                               FROM SC-MESSAGE.
          03 LINE 24 COLUMN 01 VALUE 'F1 - Exit'.
      *----------------------END-PAN2SCR--------------------
       PROCEDURE DIVISION.
           SORT STATE-TABLE-OCCURRENCES
                ON ASCENDING KEY STATE-ABBREV
                WITH DUPLICATES IN ORDER
           PERFORM UNTIL F1-PRESSED
               DISPLAY TABLES-SCREEN
               ACCEPT TABLES-SCREEN
               IF NOT F1-PRESSED
                   INITIALIZE SC-OUTPUT-FIELDS
                   PERFORM PROCESS-DATE
                   PERFORM PROCESS-STATE
                   DISPLAY TABLES-SCREEN
               END-IF
           END-PERFORM
           GOBACK
           .
       PROCESS-STATE.
           MOVE FUNCTION UPPER-CASE (SC-STATE) TO UPPER-STATE
           SET STATE-IDX TO 1
           INITIALIZE NUMERIC-STATE-IDX
           SEARCH ALL STATE-TABLE-OCCURRENCES
      * serial search only
      *        VARYING NUMERIC-STATE-IDX
               AT END 
                    MOVE 'State not found' TO SC-STATE-NAME
               WHEN STATE-ABBREV (STATE-IDX) = UPPER-STATE
                    MOVE STATE-NAME (STATE-IDX) TO SC-STATE-NAME
                    MOVE NUMERIC-STATE-IDX TO SC-STATE-INDEX
                    MOVE STATE-CAPITAL (STATE-IDX)  TO SC-CAPITAL
           END-SEARCH
           . 
       PROCESS-DATE.
           MOVE SC-ENTER-DATE TO DATE-FIELD
           IF MONTH-PORTION < 01 OR MONTH-PORTION > 12
               MOVE 'INVALID MONTH' TO SC-MESSAGE
           ELSE
               MOVE DATE-FIELD TO SC-DATE
               STRING MONTH-NAME (MONTH-PORTION) DELIMITED BY SPACE
                      SPACE
                      DATE-FIELD (3:2)
                      ','
                      DATE-FIELD (5:4) DELIMITED BY SIZE
                      INTO SC-EDITED-DATE
               END-STRING
           END-IF
           .
