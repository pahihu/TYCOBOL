000000*AA1ABBB+----2----+----3----+----4----+----5----+----6----+----7--SEQUENCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCR001.
       AUTHOR. Andras Pahi.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       SCREEN SECTION.
       01 MY-MAIN-SCREEN
          BLANK SCREEN, AUTO, REQUIRED,
          FOREGROUND-COLOR IS 7,
          BACKGROUND-COLOR IS 1.
          03 LINE 01 COLUMN 27 VALUE "Name and Address Entry"
             UNDERLINE.
          03 LINE 3 COLUMN 5 VALUE "Last Name " HIGHLIGHT BELL.
       PROCEDURE DIVISION.
           DISPLAY MY-MAIN-SCREEN
           STOP RUN
           .
