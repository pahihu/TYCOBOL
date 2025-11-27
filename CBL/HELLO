000000*AA1ABBB+----2----+----3----+----4----+----5----+----6----+----7--PRGRAMID
       IDENTIFICATION DIVISION.
           PROGRAM-ID. HELLO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SOURCE-COMPUTER. MACOS.
      *    SOURCE-COMPUTER. MACOS WITH DEBUGGING MODE.
           OBJECT-COMPUTER. MACOS.
           SPECIAL-NAMES.
               CURRENCY IS '$'.
               DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM PIC S9(3)V99 VALUE IS -23,45.
       PROCEDURE DIVISION.
       HELLO-START.
           DISPLAY "Hello World! " WS-NUM
      D    DISPLAY "Only in debug mode."
           STOP RUN.

