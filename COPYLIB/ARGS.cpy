       01 ARGUMENTS.
          03 MODULE    PIC X(40) VALUE SPACES.
          03 ARGC      PIC 9(2) VALUE ZEROS.
          03 FILLER.
             05 ARGV   OCCURS 16 TIMES
                       PIC X(40) VALUE SPACES.
