       IDENTIFICATION DIVISION.
       PROGRAM-ID. NMENTRY.
       AUTHOR. John Doe.
       COPY SCRNIO.
      *-----------------------------------------------------
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER           PIC 9(2)  VALUE ZEROS.
       01 WS-NUM-SPACES        PIC 9(2)  VALUE ZEROS.
       01 FIELD-IDX            PIC 9     VALUE ZEROS.
       01 FIELD OCCURS 3 TIMES PIC X(30) VALUE SPACES.
       01 NUM-FIELDS           PIC 9     VALUE ZEROS.
          88 LAST-NAME-ONLY    VALUE 1
                               WHEN SET TO FALSE 4.
          88 FIRST-AND-LAST    VALUE 2
                               WHEN SET TO FALSE 5.
          88 FIRST-LAST-MIDDLE VALUE 3
                               WHEN SET TO FALSE 6.
      *---------------------BEGIN-PAN2SCR-------------------
       COPY WSSCRN.
       01 SC-NAME              PIC X(40) VALUE SPACES.
       01 SC-E-MAIL            PIC X(30) VALUE SPACES.
       01 SC-NUM-FIELDS        PIC 9 VALUE ZEROS.
       01 SC-LAST              PIC X(30) VALUE SPACES.
       01 SC-MIDDLE            PIC X(30) VALUE SPACES.
       01 SC-FIRST             PIC X(30) VALUE SPACES.
      *-----------------------------------------------------
       SCREEN SECTION.
       01 NMENTRY-SCREEN
          BLANK SCREEN, AUTO,
          FOREGROUND-COLOR IS 7,
          BACKGROUND-COLOR IS 1.
          03 LINE 03 COLUMN 34 VALUE 'Name and E-mail Entry'
                               HIGHLIGHT.
          03 LINE 06 COLUMN 09 VALUE 'Name:'.
          03 LINE 06 COLUMN 15 PIC X(40)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-NAME.
          03 LINE 07 COLUMN 07 VALUE 'E-mail:'.
          03 LINE 07 COLUMN 15 PIC X(30)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-E-MAIL.
          03 LINE 09 COLUMN 03 VALUE 'Num Fields:'.
          03 LINE 09 COLUMN 15 PIC 9
                               REVERSE-VIDEO
                               FROM SC-NUM-FIELDS.
          03 LINE 10 COLUMN 09 VALUE 'Last:'.
          03 LINE 10 COLUMN 15 PIC X(30)
                               REVERSE-VIDEO
                               FROM SC-LAST.
          03 LINE 11 COLUMN 07 VALUE 'Middle:'.
          03 LINE 11 COLUMN 15 PIC X(30)
                               REVERSE-VIDEO
                               FROM SC-MIDDLE.
          03 LINE 12 COLUMN 08 VALUE 'First:'.
          03 LINE 12 COLUMN 15 PIC X(30)
                               REVERSE-VIDEO
                               FROM SC-FIRST.
      *----------------------END-PAN2SCR--------------------
       PROCEDURE DIVISION.
           DISPLAY NMENTRY-SCREEN
           ACCEPT NMENTRY-SCREEN
      * --- SPLIT NAME INTO LAST, FIRST --------------------
           UNSTRING SC-NAME DELIMITED BY ALL SPACE
               INTO FIELD(1), FIELD(2), FIELD(3)
               TALLYING IN NUM-FIELDS
           END-UNSTRING
           MOVE 1 TO FIELD-IDX
           PERFORM CAMEL-CASE
           MOVE 2 TO FIELD-IDX
           PERFORM CAMEL-CASE
           MOVE 3 TO FIELD-IDX
           PERFORM CAMEL-CASE
           IF LAST-NAME-ONLY
               MOVE FIELD(1) TO SC-LAST
           END-IF
           IF FIRST-AND-LAST
               MOVE FIELD(1) TO SC-FIRST
               MOVE FIELD(2) TO SC-LAST
           END-IF
           IF FIRST-LAST-MIDDLE
               MOVE FIELD(1) TO SC-FIRST
               MOVE FIELD(2) TO SC-MIDDLE
               MOVE FIELD(3) TO SC-LAST
           END-IF
      * --- CONVERT E-MAIL TO LOWERCASE --------------------
           INSPECT SC-E-MAIL
               CONVERTING 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
               TO         'abcdefghijklmnopqrstuvwxyz'
           SET LAST-NAME-ONLY TO FALSE
           MOVE NUM-FIELDS TO SC-NUM-FIELDS
           DISPLAY NMENTRY-SCREEN
           STOP RUN
           .
      * ----------------------------------------------------
       CAMEL-CASE.
           INSPECT FIELD(FIELD-IDX)(1:1)
               CONVERTING 'abcdefghijklmnopqrstuvwxyz'
               TO         'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
           INSPECT FIELD(FIELD-IDX)(2:LENGTH OF FIELD - 1)
               CONVERTING 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
               TO         'abcdefghijklmnopqrstuvwxyz'
           .
