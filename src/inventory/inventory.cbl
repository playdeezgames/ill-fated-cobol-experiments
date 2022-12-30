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
                04 ItemId PIC 999 VALUE ZEROS.
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
             88 GameOver VALUE 0.
          02 Dummy PIC 999.
          02 Command PIC X.

PROCEDURE DIVISION.
       CALL "RNGSEED"
       PERFORM InitializeLocations
       MOVE 1 TO LocationId Of ScratchPad
       PERFORM GameLoop UNTIL GameOver
STOP RUN.

GameLoop.
       DISPLAY "LocationId: " LocationId OF ScratchPad
       DISPLAY "ItemId: " ItemId OF Locations(LocationId OF ScratchPad)
       ACCEPT Command
       EVALUATE Command
           WHEN "q"
               SET GameOver TO TRUE
           WHEN "n"
               PERFORM MoveNorth
           WHEN "s"
               PERFORM MoveSouth
           WHEN "e"
               PERFORM MoveEast
           WHEN "w"
               PERFORM MoveWest
       END-EVALUATE.

MoveNorth.
IF LocationId OF ScratchPad IS NOT LESS THAN 11 THEN
       SUBTRACT 10 FROM LocationId OF ScratchPad
END-IF.

MoveSouth.
IF LocationId OF ScratchPad IS NOT GREATER THAN 90 THEN
       ADD 10 TO LocationId OF ScratchPad
END-IF.

MoveEast.
IF LocationId OF ScratchPad IS NOT GREATER THAN 99 THEN
       ADD 1 TO LocationId OF ScratchPad
END-IF.

MoveWest.
IF LocationId OF ScratchPad IS NOT LESS THAN 2 THEN
       SUBTRACT 1 FROM LocationId OF ScratchPad
END-IF.

InitializeLocations.
       MOVE 1 TO LocationId OF LocationData
       PERFORM InitializeLocation UNTIL InvalidId OF LocationData
       PERFORM PlaceItems.

PlaceItems.
       PERFORM PlaceFoods
       PERFORM PlacePotions.

PlaceFoods.
       

PlacePotions.

InitializeLocation.
       COMPUTE Dummy = FUNCTION RANDOM() * 2
       EVALUATE Dummy
        WHEN 0
           SET IsPassageway(LocationId OF LocationData) TO TRUE
        WHEN 1
           SET IsChamber(LocationId OF LocationData) TO TRUE
       END-EVALUATE
       MOVE 0 TO ItemId OF Locations(LocationId OF LocationData)
       ADD 1 TO LocationId OF LocationData.
