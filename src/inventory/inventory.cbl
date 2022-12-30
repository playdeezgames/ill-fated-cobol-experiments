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
                88 InvalidId VALUES 101 THRU 999.
          02 ItemData.
             03 Items OCCURS 100 TIMES.
                04 ItemTypes PIC 99 VALUE ZEROS.
                   88 NotAnItem VALUE 0.
                   88 Food VALUE 1.
                   88 Potion VALUE 2.
                04 ItemStatus PIC 999 VALUE ZEROS.
             03 ItemId PIC 999 VALUE 1.
                88 InvalidId VALUES 101 THRU 999.
       01 ScratchPad.
          02 LocationId PIC 999.
          02 Dummy PIC 999.
PROCEDURE DIVISION.
       CALL "RNGSEED"
       PERFORM InitializeLocations
       PERFORM DisplayLocationType VARYING LocationId OF ScratchPad FROM 1 BY 1 UNTIL LocationId OF ScratchPad IS GREATER THAN 100
STOP RUN.

DisplayLocationType.
       DISPLAY LocationId OF ScratchPad " = " LocationTypes(LocationId OF ScratchPad)
EXIT.

InitializeLocations.
       MOVE 1 TO LocationId OF LocationData
       PERFORM InitializeLocation UNTIL InvalidId OF LocationData
EXIT.

InitializeLocation.
       COMPUTE Dummy = FUNCTION RANDOM() * 2
       EVALUATE Dummy
        WHEN 0
           SET IsPassageway(LocationId OF LocationData) TO TRUE
        WHEN 1
           SET IsChamber(LocationId OF LocationData) TO TRUE
       END-EVALUATE
       ADD 1 TO LocationId OF LocationData
EXIT.
