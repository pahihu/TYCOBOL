       IDENTIFICATION DIVISION.
       PROGRAM-ID. NMENTRY.
       AUTHOR. John Doe.
       COPY SCRNIO.
      *-----------------------------------------------------
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER           PIC 9(2)  VALUE ZEROS.
       01 WS-NUM-SPACES        PIC 9(2)  VALUE ZEROS.
      *---------------------BEGIN-PAN2SCR-------------------
       COPY WSSCRN.
       01 SC-NAME              PIC X(40) VALUE SPACES.
       01 SC-E-MAIL            PIC X(30) VALUE SPACES.
       01 SC-LAST              PIC X(30) VALUE SPACES.
       01 SC-FIRST             PIC X(30) VALUE SPACES.
      *-----------------------------------------------------
       SCREEN SECTION.
       01 NMENTRY-SCREEN
          BLANK SCREEN, AUTO,
          FOREGROUND-COLOR IS 7,
          BACKGROUND-COLOR IS 1.
          03 LINE 03 COLUMN 30 VALUE 'Name and E-mail Entry'
                               HIGHLIGHT.
          03 LINE 06 COLUMN 05 VALUE 'Name:'.
          03 LINE 06 COLUMN 11 PIC X(40)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-NAME.
          03 LINE 07 COLUMN 03 VALUE 'E-mail:'.
          03 LINE 07 COLUMN 11 PIC X(30)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-E-MAIL.
          03 LINE 09 COLUMN 05 VALUE 'Last:'.
          03 LINE 09 COLUMN 11 PIC X(30)
                               REVERSE-VIDEO
                               FROM SC-LAST.
          03 LINE 10 COLUMN 04 VALUE 'First:'.
          03 LINE 10 COLUMN 11 PIC X(30)
                               REVERSE-VIDEO
                               FROM SC-FIRST.
      *----------------------END-PAN2SCR--------------------
       PROCEDURE DIVISION.
           DISPLAY NMENTRY-SCREEN
           ACCEPT NMENTRY-SCREEN
      * --- SPLIT NAME INTO LAST, FIRST --------------------
           INSPECT SC-NAME
               TALLYING WS-COUNTER
               FOR CHARACTERS BEFORE INITIAL ','
           MOVE SC-NAME(1:WS-COUNTER) TO SC-LAST
      * --- MOVE AFTER COMMA -------------------------------
           ADD 2 TO WS-COUNTER
      * --- COUNT LEADING SPACES AFTER COMMA ---------------
           INSPECT SC-NAME(WS-COUNTER:)
               TALLYING WS-NUM-SPACES
               FOR LEADING SPACES
           MOVE SC-NAME(WS-COUNTER + WS-NUM-SPACES:)
                TO SC-FIRST
      * --- CONVERT E-MAIL TO LOWERCASE --------------------
           INSPECT SC-E-MAIL
               CONVERTING 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
               TO         'abcdefghijklmnopqrstuvwxyz'
           DISPLAY NMENTRY-SCREEN
           STOP RUN
           .
