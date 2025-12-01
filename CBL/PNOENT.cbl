       IDENTIFICATION DIVISION.
       PROGRAM-ID. PNOENT.
       AUTHOR. John Doe.
       COPY SCRNIO.
      *-----------------------------------------------------
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FORMATTED-NUMBER PIC X(14) VALUE '(XXX) XXX-XXXX'.
       01 FORMATTED-ALTERNATE PIC X(8) VALUE 'XXX-XXXX'.
      *---------------------BEGIN-PAN2SCR-------------------
       COPY WSSCRN.
       01 SC-ENTER-PHONE-NUMBER PIC 9(10) VALUE ZEROS.
       01 SC-EDITED-PHONE-NUMBER PIC X(14) VALUE SPACES.
      *-----------------------------------------------------
       SCREEN SECTION.
       01 PNOENT-SCREEN
          BLANK SCREEN, AUTO,
          FOREGROUND-COLOR IS 7,
          BACKGROUND-COLOR IS 1.
          03 LINE 03 COLUMN 18 VALUE 'Phone Number Entry'
                               HIGHLIGHT.
          03 LINE 05 COLUMN 05 VALUE 'Enter Phone Number:'.
          03 LINE 05 COLUMN 25 PIC ZZZZZZZZZZ
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-ENTER-PHONE-NUMBER.
          03 LINE 06 COLUMN 04 VALUE 'Edited Phone Number:'.
          03 LINE 06 COLUMN 25 PIC X(14)
                               REVERSE-VIDEO
                               FROM SC-EDITED-PHONE-NUMBER.
      *----------------------END-PAN2SCR--------------------
       PROCEDURE DIVISION.
           DISPLAY PNOENT-SCREEN
           ACCEPT PNOENT-SCREEN
           IF SC-ENTER-PHONE-NUMBER > 9999999 THEN
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
                       BY SC-ENTER-PHONE-NUMBER(4:3)
                   FIRST 'XXXX'
                       BY SC-ENTER-PHONE-NUMBER(7:4)
               MOVE FORMATTED-ALTERNATE
                   TO SC-EDITED-PHONE-NUMBER
           END-IF
           DISPLAY PNOENT-SCREEN
           STOP RUN
           .
