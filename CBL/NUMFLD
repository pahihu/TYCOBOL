000000*AA1ABBB+----2----+----3----+----4----+----5----+----6----+----7--SEQUENCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NUMFLD.
       AUTHOR. Andras Pahi.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 QUANTITY-ON-HAND      PIC 9(3)    VALUE 20.
       01 QUANTITY-ON-ORDER     PIC 9(2)    VALUE 15.
       01 QUANTITY-SOLD-TO-DATE PIC 9(12)   VALUE 5021.
      *
       01 COST-OF-EACH-ITEM     PIC 9(5)V9(2)   VALUE 10.00.
       01 AVERAGE-COST          PIC 9(3)V9(4)   VALUE 10.0000.
       01 OVERALL-DOLLARS       PIC 9(7)        VALUE 10.
      *
       01 NET-PROFIT            PIC S9(5)V9(2)  VALUE -10.00.
       01 MONTHLY-NET-PROFIT    PIC S9(5)V9(2)
                                    VALUE -987.65
                                    SIGN IS LEADING SEPARATE CHARACTER.
       01 QUARTERLY-NET-PROFIT  PIC S9(5)V9(2)
                                    VALUE 123.45
                                    SIGN TRAILING SEPARATE.
       01 YEARLY-NET-PROFIT     PIC S9(5)V9(2) VALUE ZEROS.
       01 YEARLY-GROSS-PROFIT   PIC S9(5)V9(2) VALUE ZEROS
                                               SIGN LEADING SEPARATE.
       01 WS-DISPLAY            PIC S9(5)V9(2)
                                        VALUE -12345.67
                                        USAGE DISPLAY.
       01 WS-BINARY             PIC S9(5)V9(2)
                                        VALUE -12345.67
                                        USAGE COMP.
       01 WS-FLOAT              VALUE -12345.67
                                        USAGE COMP-1.
       01 WS-DOUBLE             VALUE -12345.67
                                        USAGE COMP-2.
       01 WS-PACKED             PIC S9(5)V9(2)
                                        VALUE -12345.67
                                        USAGE COMP-3.
       01 WS-BINARY2            PIC S9(5)V9(2)
                                        VALUE -12345.67
                                        USAGE COMP-4.
       01 WS-NATIVE             PIC S9(5)V9(2)
                                        VALUE -12345.67
                                        USAGE COMP-5.
       PROCEDURE DIVISION.
           DISPLAY "COST OF EACH ITEM.... " COST-OF-EACH-ITEM
           DISPLAY "AVERAGE COST......... " AVERAGE-COST
           DISPLAY "OVERALL DOLLARS...... " OVERALL-DOLLARS

           DISPLAY "NET PROFIT........... " NET-PROFIT
           DISPLAY "NET PROFIT..........:=" LENGTH OF NET-PROFIT
           DISPLAY "MONTHLY NET PROFIT... " MONTHLY-NET-PROFIT
           DISPLAY "MONTHLY NET PROFIT..:=" LENGTH OF MONTHLY-NET-PROFIT
           DISPLAY "QUARTERLY NET PROFIT. " QUARTERLY-NET-PROFIT
           DISPLAY "QUARTERLY NET PROFIT:="
                                   LENGTH OF QUARTERLY-NET-PROFIT

           DISPLAY "YEARLY-NET-PROFIT.... "
                           FUNCTION HEX-OF(YEARLY-NET-PROFIT)
           DISPLAY "QUARTERLY-NET-PROFIT.... "
                           FUNCTION HEX-OF(QUARTERLY-NET-PROFIT)
           DISPLAY "YEARLY-NET-PROFIT....... "
                           FUNCTION HEX-OF(YEARLY-NET-PROFIT)
           DISPLAY "YEARLY-GROSS-PROFIT.... "
                           FUNCTION HEX-OF(YEARLY-GROSS-PROFIT)

           DISPLAY "WS-DISPLAY..... " FUNCTION HEX-OF(WS-DISPLAY)
           DISPLAY "WS-BINARY...... " FUNCTION HEX-OF(WS-BINARY)
           DISPLAY "WS-FLOAT....... " FUNCTION HEX-OF(WS-FLOAT)
           DISPLAY "WS-DOUBLE...... " FUNCTION HEX-OF(WS-DOUBLE)
           DISPLAY "WS-PACKED...... " FUNCTION HEX-OF(WS-PACKED)
           DISPLAY "WS-BINARY2..... " FUNCTION HEX-OF(WS-BINARY2)
           DISPLAY "WS-NATIVE...... " FUNCTION HEX-OF(WS-NATIVE)
           .
