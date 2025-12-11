       IDENTIFICATION DIVISION.
       PROGRAM-ID. NMENTRY.
       AUTHOR. John Doe.
       COPY SCRNIO.
      *-----------------------------------------------------
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY WSSCRN.
       01 FUNCTION-KEYS REDEFINES KEYBOARD-STATUS PIC 9(4).
          88 F1-PRESSED                  VALUE 1001.
       01 WORK-VARIABLES.
          03 WS-COUNTER           PIC 9(2)  VALUE ZEROS.
          03 WS-NUM-SPACES        PIC 9(2)  VALUE ZEROS.
          03 FIELD-IDX            PIC 9     VALUE ZEROS.
          03 FIELD OCCURS 4 TIMES PIC X(30) VALUE SPACES.
          03 NUM-FIELDS           PIC 9     VALUE ZEROS.
             88 LAST-NAME-ONLY    VALUE 1
                                  WHEN SET TO FALSE 0.
             88 FIRST-AND-LAST    VALUE 2
                                  WHEN SET TO FALSE 0.
             88 FIRST-LAST-MIDDLE VALUE 3
                                  WHEN SET TO FALSE 0.
      *---------------------BEGIN-PAN2SCR-------------------
       01 SC-NAME              PIC X(40) VALUE SPACES.
       01 SC-E-MAIL            PIC X(30) VALUE SPACES.
       01 OUTPUT-FIELDS.
       03 SC-NUM-FIELDS        PIC 9 VALUE ZEROS.
       03 SC-LAST              PIC X(30) VALUE SPACES.
       03 SC-MIDDLE            PIC X(30) VALUE SPACES.
       03 SC-FIRST             PIC X(30) VALUE SPACES.
       03 SC-MESSAGE           PIC X(60) VALUE SPACES.
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
          03 LINE 19 COLUMN 06 VALUE 'Message:'.
          03 LINE 19 COLUMN 15 PIC X(60)
                               HIGHLIGHT
                               FOREGROUND-COLOR 4
                               FROM SC-MESSAGE.
      *----------------------END-PAN2SCR--------------------
       PROCEDURE DIVISION.
       NAME-ENTRY-START.
           PERFORM DISPLAY-AND-ACCEPT-SCREEN UNTIL F1-PRESSED
           GOBACK
           .
       DISPLAY-AND-ACCEPT-SCREEN.
           DISPLAY NMENTRY-SCREEN
           ACCEPT NMENTRY-SCREEN
           IF F1-PRESSED
               CONTINUE
           ELSE
               PERFORM PROCESS-DATA
               MOVE NUM-FIELDS TO SC-NUM-FIELDS
               DISPLAY NMENTRY-SCREEN
           END-IF
           .
       PROCESS-DATA.
      * --- INIT THE WORKING FIELDS
           INITIALIZE WORK-VARIABLES
           INITIALIZE OUTPUT-FIELDS
           IF SC-NAME > SPACES
               PERFORM PROCESS-NAME
           ELSE
               MOVE ALL '*' TO SC-NAME
           END-IF
           IF SC-E-MAIL > SPACES
               PERFORM PROCESS-E-MAIL
           ELSE
               MOVE ALL '*' TO SC-E-MAIL
           END-IF
           .
      * --- SPLIT NAME INTO LAST, FIRST --------------------
       PROCESS-NAME.
           UNSTRING SC-NAME DELIMITED BY ALL SPACE
               INTO FIELD(1), FIELD(2), FIELD(3), FIELD(4)
               COUNT IN WS-COUNTER
               TALLYING IN NUM-FIELDS
           END-UNSTRING
           IF 0 < NUM-FIELDS AND NUM-FIELDS < 4
               PERFORM PROCESS-VALID-NAME
           ELSE
               MOVE 'MAX. 3 NAME PARTS' TO SC-MESSAGE
           END-IF
           .
       PROCESS-VALID-NAME.
           PERFORM VARYING FIELD-IDX
                   FROM 1 BY 1
                   UNTIL FIELD-IDX > 3
               PERFORM CAMEL-CASE
           END-PERFORM
           MOVE FIELD(NUM-FIELDS) TO SC-LAST
           IF NUM-FIELDS > 1
               MOVE FIELD(1) TO SC-FIRST
           END-IF 
           IF NUM-FIELDS > 2
               MOVE FIELD(2) TO SC-MIDDLE
           END-IF
           .
      * --- CONVERT E-MAIL TO LOWERCASE --------------------
        PROCESS-E-MAIL.
           INSPECT SC-E-MAIL
               CONVERTING 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
               TO         'abcdefghijklmnopqrstuvwxyz'
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
