       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. CALLER.
DATA DIVISION.
    working-storage section.
    01 Stuff PIC X VALUE SPACE.
PROCEDURE DIVISION.
       CALL "CALLED" USING Stuff
       DISPLAY Stuff
STOP RUN.