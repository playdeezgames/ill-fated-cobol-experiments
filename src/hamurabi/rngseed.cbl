       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. RNGSEED.

DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Rng-Seed-Data PIC 999.

PROCEDURE DIVISION.
       COMPUTE Rng-Seed-Data = FUNCTION RANDOM(function seconds-past-midnight())
EXIT PROGRAM.
