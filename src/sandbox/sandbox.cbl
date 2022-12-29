       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. SANDBOX.

DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ScratchPad.
          02 XValue PIC 99.
             88 InvalidX VALUES 11 THRU 99.
          02 YValue PIC 99.
             88 InvalidY VALUES 11 THRU 99.

PROCEDURE DIVISION.
       PERFORM VARYING YValue FROM 1 BY 1 UNTIL InvalidY AFTER XValue FROM 1 BY 1 UNTIL InvalidX
           DISPLAY "(" XValue "," YValue")"
       END-PERFORM
STOP RUN.
