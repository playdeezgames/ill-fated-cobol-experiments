       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. INVENTORY.

DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WorldData.
          02 LocationData.
             03 Locations OCCURS 100 TIMES.
                04 LocationTypes PIC X VALUE "P".
                   88 IsPassageway VALUE "P".
                   88 IsChamber VALUE "C".
             03 LocationId PIC 999 VALUE 1. 
                88 InvalidNextLocationId VALUES 101 THRU 999.
       01 ScratchPad.
          02 CurrentLocationId PIC 999.
          02 Dummy PIC 999.
PROCEDURE DIVISION.
       CALL "RNGSEED"
       PERFORM InitializeLocations
       PERFORM DisplayLocationType VARYING CurrentLocationId FROM 1 BY 1 UNTIL CurrentLocationId IS GREATER THAN 100
STOP RUN.

DisplayLocationType.
       DISPLAY CurrentLocationId " = " LocationTypes(CurrentLocationId)
EXIT.

InitializeLocations.
       MOVE 1 TO LocationId
       PERFORM InitializeLocation UNTIL InvalidNextLocationId
EXIT.

InitializeLocation.
       COMPUTE Dummy = FUNCTION RANDOM() * 2
       DISPLAY Dummy WITH NO ADVANCING
       EVALUATE Dummy
        WHEN 0
           SET IsPassageway(LocationId) TO TRUE
        WHEN 1
           SET IsChamber(LocationId) TO TRUE
       END-EVALUATE
       ADD 1 TO LocationId
EXIT.
