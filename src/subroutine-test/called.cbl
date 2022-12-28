       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. CALLED.
DATA DIVISION.
    working-storage section.
    LINKAGE SECTION.
    01 Imported PIC X.
PROCEDURE DIVISION USING Imported.
       DISPLAY "Yep, you called me!"
       MOVE "Y" TO Imported
EXIT PROGRAM.
