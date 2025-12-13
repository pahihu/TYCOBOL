000000*AA1ABBB+----2----+----3----+----4----+----5----+----6----+----7--SEQUENCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLES0.
       AUTHOR. Andras Pahi.
       DATE-WRITTEN. 2025-12-13.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 MONTH-TABLE-AREA.
          03 MONTH-DESCRIPTIONS.
             05 FILLER          PIC X(9) VALUE 'January'.
             05 FILLER          PIC X(9) VALUE 'February'.
             05 FILLER          PIC X(9) VALUE 'March'.
             05 FILLER          PIC X(9) VALUE 'April'.
             05 FILLER          PIC X(9) VALUE 'May'.
             05 FILLER          PIC X(9) VALUE 'June'.
             05 FILLER          PIC X(9) VALUE 'July'.
             05 FILLER          PIC X(9) VALUE 'August'.
             05 FILLER          PIC X(9) VALUE 'September'.
             05 FILLER          PIC X(9) VALUE 'October'.
             05 FILLER          PIC X(9) VALUE 'November'.
             05 FILLER          PIC X(9) VALUE 'December'.
          03 MONTH-TABLE REDEFINES MONTH-DESCRIPTIONS.
             05 MONTH-NAME      PIC X(9) OCCURS 12 TIMES.
       01 WS-COUNTER            PIC 9(2) VALUE ZERO.
       PROCEDURE DIVISION.
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
                   UNTIL WS-COUNTER > 12
               DISPLAY MONTH-NAME(WS-COUNTER)
           END-PERFORM
           GOBACK
           .
       
