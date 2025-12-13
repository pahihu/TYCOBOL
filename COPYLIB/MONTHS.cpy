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
             05 MONTH-NAME      PIC X(9) OCCURS 12 TIMES 
                                         INDEXED BY MONTH-IDX.
