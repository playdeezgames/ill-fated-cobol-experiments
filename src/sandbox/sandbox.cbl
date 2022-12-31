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
          02 FlagValue PIC X.
             88 FlagIsSet VALUE "Y".
             88 FlagIsClear VALUE "N".

PROCEDURE DIVISION.
       SET FlagIsClear TO TRUE
       PERFORM VARYING YValue FROM 1 BY 1 UNTIL InvalidY AFTER XValue FROM 1 BY 1 UNTIL InvalidX
           DISPLAY ScratchPad
       END-PERFORM
       DISPLAY ScratchPad
STOP RUN.
