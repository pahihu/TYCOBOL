       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPLEXP.
       AUTHOR. John Doe.
       COPY SCRNIO.
      *-----------------------------------------------------
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PTR               PIC 9(2) VALUE 1.
      *---------------------BEGIN-PAN2SCR-------------------
       COPY WSSCRN.
       01 SC-ENTER-EXPRESSION  PIC X(10) VALUE SPACES.
       01 SC-FIRST-TERM        PIC X(5) VALUE SPACES.
       01 SC-SECOND-TERM       PIC X(5) VALUE SPACES.
       01 SC-OPERATION         PIC X VALUE SPACES.
      *-----------------------------------------------------
       SCREEN SECTION.
       01 SPLEXP-SCREEN
          BLANK SCREEN, AUTO,
          FOREGROUND-COLOR IS 7,
          BACKGROUND-COLOR IS 1.
          03 LINE 03 COLUMN 34 VALUE 'Split expression'
                               HIGHLIGHT.
          03 LINE 05 COLUMN 03 VALUE 'Enter Expression:'.
          03 LINE 05 COLUMN 21 PIC X(10)
                               REVERSE-VIDEO
                               USING SC-ENTER-EXPRESSION.
          03 LINE 06 COLUMN 04 VALUE 'First Term'.
          03 LINE 06 COLUMN 15 PIC X(5)
                               REVERSE-VIDEO
                               FROM SC-FIRST-TERM.
          03 LINE 07 COLUMN 03 VALUE 'Second Term'.
          03 LINE 07 COLUMN 15 PIC X(5)
                               REVERSE-VIDEO
                               FROM SC-SECOND-TERM.
          03 LINE 08 COLUMN 05 VALUE 'Operation'.
          03 LINE 08 COLUMN 15 PIC X
                               REVERSE-VIDEO
                               FROM SC-OPERATION.
      *----------------------END-PAN2SCR--------------------
       PROCEDURE DIVISION.
           DISPLAY SPLEXP-SCREEN
           ACCEPT SPLEXP-SCREEN
           UNSTRING SC-ENTER-EXPRESSION
               DELIMITED BY '+' OR '-' OR '*' OR '/'
               INTO SC-FIRST-TERM
                    DELIMITER IN SC-OPERATION
                    COUNT IN WS-PTR
           END-UNSTRING
           ADD 2 TO WS-PTR
           UNSTRING SC-ENTER-EXPRESSION
               DELIMITED BY '='
               INTO SC-SECOND-TERM
               POINTER WS-PTR
           END-UNSTRING
           DISPLAY SPLEXP-SCREEN
           STOP RUN
           .
