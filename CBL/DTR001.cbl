       IDENTIFICATION DIVISION.
       PROGRAM-ID. DTR001.
       AUTHOR. Andras Pahi.
       DATE-WRITTEN. 11/22/2025.
       COPY SCRNIO.
      *-----------------------------------------------------------------
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY WSSCRN.
       01 WS-DT.
          03 DT-YEAR        PIC 9(4).
          03 DT-MONTH       PIC 9(2).
          03 DT-DAY         PIC 9(2).
          03 DT-HOUR        PIC 9(2).
          03 DT-MINUT       PIC 9(2).
          03 DT-SECOND      PIC 9(2)V99.
       01 WS-FMT-DT.
          03 DT-MONTH       PIC 9(2).
          03 DT-DAY         PIC 9(2).
          03 DT-YEAR        PIC 9(4).
      *-----------------------------------------------------------------
       01 SC-LAST-NAME         PIC X(25) VALUE SPACES.
       01 SC-MID               PIC X(10) VALUE SPACES.
       01 SC-FIRST             PIC X(15) VALUE SPACES.
       01 SC-ADDRESS-LINE-1    PIC X(50) VALUE SPACES.
       01 SC-ADDRESS-LINE-2    PIC X(50) VALUE SPACES.
       01 SC-CITY              PIC X(40) VALUE SPACES.
       01 SC-STATE-COUNTRY     PIC X(20) VALUE SPACES.
       01 SC-POSTAL-CODE       PIC X(15) VALUE SPACES.
       01 SC-HOME-TELEPHONE    PIC X(20) VALUE SPACES.
       01 SC-WORK              PIC X(20) VALUE SPACES.
       01 SC-OTHER             PIC X(20) VALUE SPACES.
       01 SC-START-DATE        PIC 9(8) VALUE ZEROS.
       01 SC-LAST-PAID-DATE    PIC 9(8) VALUE ZEROS.
       01 SC-NEXT-RENT-DUE-ON  PIC 9(8) VALUE ZEROS.
       01 SC-RENT-AMOUNT       PIC 9(4)V9(2) VALUE 50.00.
       01 SC-CONSIGNMENT-PCT   PIC 9(3) VALUE 40.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01 DTR001-SCREEN
          BLANK SCREEN, AUTO,
          FOREGROUND-COLOR IS 7,
          BACKGROUND-COLOR IS 1.
          03 LINE 02 COLUMN 31 VALUE 'Darlene''s Treasures'
                               HIGHLIGHT
                               FOREGROUND-COLOR 4.
          03 LINE 04 COLUMN 31 VALUE 'Tenant Entry Program'
                               HIGHLIGHT.
          03 LINE 06 COLUMN 01 VALUE 'Last Name'.
          03 LINE 06 COLUMN 11 PIC X(25)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-LAST-NAME.
          03 LINE 06 COLUMN 40 VALUE 'Mid.'.
          03 LINE 06 COLUMN 45 PIC X(10)
                               REVERSE-VIDEO
                               USING SC-MID.
          03 LINE 06 COLUMN 58 VALUE 'First'.
          03 LINE 06 COLUMN 64 PIC X(15)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-FIRST.
          03 LINE 08 COLUMN 06 VALUE 'Address Line 1'.
          03 LINE 08 COLUMN 21 PIC X(50)
                               REVERSE-VIDEO
                               USING SC-ADDRESS-LINE-1.
          03 LINE 09 COLUMN 06 VALUE 'Address Line 2'.
          03 LINE 09 COLUMN 21 PIC X(50)
                               REVERSE-VIDEO
                               USING SC-ADDRESS-LINE-2.
          03 LINE 10 COLUMN 16 VALUE 'City'.
          03 LINE 10 COLUMN 21 PIC X(40)
                               REVERSE-VIDEO
                               USING SC-CITY.
          03 LINE 11 COLUMN 07 VALUE 'State/Country'.
          03 LINE 11 COLUMN 21 PIC X(20)
                               REVERSE-VIDEO
                               USING SC-STATE-COUNTRY.
          03 LINE 11 COLUMN 44 VALUE 'Postal Code'.
          03 LINE 11 COLUMN 56 PIC X(15)
                               REVERSE-VIDEO
                               USING SC-POSTAL-CODE.
          03 LINE 12 COLUMN 06 VALUE 'Home Telephone'.
          03 LINE 12 COLUMN 21 PIC X(20)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-HOME-TELEPHONE.
          03 LINE 12 COLUMN 51 VALUE 'Work'.
          03 LINE 12 COLUMN 56 PIC X(20)
                               REVERSE-VIDEO
                               USING SC-WORK.
          03 LINE 13 COLUMN 15 VALUE 'Other'.
          03 LINE 13 COLUMN 21 PIC X(20)
                               REVERSE-VIDEO
                               USING SC-OTHER.
          03 LINE 15 COLUMN 10 VALUE 'Start Date'.
          03 LINE 15 COLUMN 21 PIC 9(2)/9(2)/9(4)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-START-DATE.
          03 LINE 16 COLUMN 06 VALUE 'Last Paid Date'.
          03 LINE 16 COLUMN 21 PIC 9(2)/9(2)/9(4)
                               REVERSE-VIDEO
                               USING SC-LAST-PAID-DATE.
          03 LINE 17 COLUMN 04 VALUE 'Next Rent Due on'.
          03 LINE 17 COLUMN 21 PIC 9(2)/9(2)/9(4)
                               REVERSE-VIDEO
                               USING SC-NEXT-RENT-DUE-ON.
          03 LINE 19 COLUMN 09 VALUE 'Rent Amount'.
          03 LINE 19 COLUMN 21 PIC Z,ZZZ.9(2)
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-RENT-AMOUNT.
          03 LINE 20 COLUMN 04 VALUE 'Consignment Pct.'.
          03 LINE 20 COLUMN 21 PIC ZZ9
                               REVERSE-VIDEO
                               REQUIRED
                               USING SC-CONSIGNMENT-PCT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
           MOVE FUNCTION CURRENT-DATE TO WS-DT
           MOVE CORRESPONDING WS-DT TO WS-FMT-DT
           MOVE WS-FMT-DT TO SC-START-DATE
           DISPLAY DTR001-SCREEN
           ACCEPT DTR001-SCREEN
           STOP RUN
           .
