        COPY STATES.
           03 State-Table Redefines State-Table-Data.
              05 State-Table-Occurrences Occurs 51 Times 
                                         Ascending Key State-Abbrev
                                         Indexed By State-Idx.
                 10 State-Abbrev         Pic XX.
                 10 State-Capital        Pic X(15).
                 10 State-Name           Pic X(20).
