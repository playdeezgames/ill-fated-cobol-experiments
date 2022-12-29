       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. MAZEGEN.

DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MazeData.
           02 MazeColumns OCCURS 8 TIMES.
               03 MazeRows OCCURS 8 TIMES.
                   04 State PIC X.
                      88 Inside VALUE "I".
                      88 Outside VALUE "O".
                      88 Frontier VALUE "F".
                   04 Doors PIC X OCCURS 4 TIMES.
       01 ScratchPad.
           02 MazeColumn PIC 9.
           02 MazeRow PIC 9.
           02 Direction PIC 9.
           02 NextColumn PIC 9.
           02 NextRow PIC 9.
           02 MazeGenComplete PIC X.
              88 GenerationComplete VALUE "Y".
              88 GenerationIncomplete VALUE "N".
           02 DoorCandidate PIC X occurs 4 times.

PROCEDURE DIVISION.
       CALL "RNG"
       PERFORM InitializeMaze
       PERFORM GenerateMaze
       PERFORM DrawMaze
STOP RUN.

DrawMaze.
       PERFORM WriteBlankLine
       PERFORM DrawMazeRows
       PERFORM DrawMazeBottom
EXIT.

DrawMazeRows.
       perform varying MazeRow from 1 by 1 until mazerow is greater than 8
           PERFORM DrawMazeRow
       end-perform
EXIT.

DrawMazeRow.
       PERFORM DrawMazeRowTopLine
       PERFORM DrawMazeRowMiddleLine
EXIT.

DrawMazeRowTopLine.
       perform varying MazeColumn from 1 by 1 until MazeColumn is greater than 8
           PERFORM DrawWall
           PERFORM DrawNorthDoor
       end-perform
       PERFORM DrawEndWall
EXIT.

DrawNorthDoor.
       evaluate doors(MazeColumn, MazeRow, 1)
           when "Y"
               PERFORM DrawSpace
           when other 
               PERFORM DrawWall
       end-evaluate
EXIT.

DrawMazeRowMiddleLine.
       perform varying MazeColumn from 1 by 1 until MazeColumn is greater than 8
           PERFORM DrawWestDoor
           PERFORM DrawSpace
       end-perform
       PERFORM DrawEndWall
EXIT.

DrawWestDoor.
       evaluate doors(MazeColumn, MazeRow,4)
           when "Y"
               PERFORM DrawSpace
           when other 
               PERFORM DrawWall
       end-evaluate
EXIT.

WriteBlankLine.
       DISPLAY " "
EXIT.

DrawMazeBottom.
       perform varying MazeColumn from 1 by 1 until MazeColumn is greater than 8
           PERFORM DrawWall
           PERFORM DrawWall
       end-perform
       PERFORM DrawEndWall
EXIT.

DrawWall.
       DISPLAY "#" WITH NO ADVANCING
EXIT.

DrawSpace.
       DISPLAY " " WITH NO ADVANCING
EXIT.

DrawEndWall.
       DISPLAY "#"
EXIT.

InitializeMaze.
       PERFORM VARYING MazeColumn FROM 1 BY 1 UNTIL MazeColumn IS GREATER THAN 8
           PERFORM InitializeMazeRow
       END-PERFORM
EXIT.

InitializeMazeRow.
       PERFORM VARYING MazeRow FROM 1 BY 1 UNTIL MazeRow IS GREATER THAN 8
           PERFORM InitializeMazeCell
       END-PERFORM
EXIT.

InitializeMazeCell.
       PERFORM MarkCellOutside
       PERFORM ClearCellDoors
EXIT.

ClearCellDoors.
       PERFORM VARYING Direction FROM 1 BY 1 UNTIL Direction IS greater THAN 4
           PERFORM ClearCellDoor
       END-PERFORM
EXIT.

ClearCellDoor.
       MOVE "N" TO Doors(MazeColumn,MazeRow,Direction)
EXIT.

MarkCellOutside.
       SET Outside(MazeColumn,MazeRow) TO TRUE
EXIT.

GenerateMaze.
       PERFORM DetermineRandomMazeCell
       PERFORM MarkCellInside
       PERFORM MarkFrontierNeighbors
       PERFORM WITH TEST AFTER UNTIL GenerationComplete
           PERFORM GenerateMazeCell
       end-perform
EXIT.

GenerateMazeCell.
       PERFORM DetermineRandomFrontierCell
       PERFORM DetermineDoorCandidates
       PERFORM DetermineValidDoorCandidate
       PERFORM MarkCellInside
       PERFORM SetDoor
       perform DetermineNextPosition
       perform DetermineOppositeDirection
       PERFORM SetNextDoor
       PERFORM MarkFrontierNeighbors
       PERFORM DetermineMazeGenComplete
EXIT.

DetermineDoorCandidates.
       PERFORM VARYING Direction FROM 1 BY 1 UNTIL Direction IS greater than 4
           perform DetermineDoorCandidacy
       end-perform
EXIT.

DetermineValidDoorCandidate.
       perform with test after until DoorCandidate(Direction) is equal to "Y"
           compute direction = function random() * 4 + 1
       end-perform
EXIT.

SetDoor.
       MOVE "Y" TO Doors(MazeColumn, MazeRow, Direction)
EXIT.

SetNextDoor.
       MOVE "Y" TO Doors(NextColumn, NextRow, Direction)
EXIT.

DetermineRandomFrontierCell.
       PERFORM WITH TEST AFTER UNTIL State(MazeColumn, MazeRow) IS EQUAL TO "F"
           PERFORM DetermineRandomMazeCell
       END-PERFORM
EXIT.

MarkCellInside.
       MOVE "I" TO State(MazeColumn, MazeRow)
EXIT.


MarkFrontierNeighbors.
       PERFORM VARYING Direction FROM 1 BY 1 UNTIL Direction IS greater  THAN 4
           PERFORM DetermineNextPosition
           PERFORM MarkFrontierCell
       END-PERFORM
EXIT.

MarkFrontierCell.
       IF NextColumn IS GREATER THAN 0 AND NextRow IS GREATER THAN 0 AND NextColumn IS NOT GREATER THAN 8 AND NextRow IS NOT GREATER THAN 8 and state(NextColumn, NextRow) IS EQUAL TO "O" THEN
           MOVE "F" TO State(NextColumn, NextRow)
       END-IF
EXIT.

DetermineRandomMazeCell.
       COMPUTE MazeColumn = FUNCTION RANDOM() * 8 + 1
       COMPUTE MazeRow = FUNCTION RANDOM() * 8 + 1
EXIT.

DetermineOppositeDirection.
       add 2 to Direction
       if direction is greater than 4 then 
           subtract 4 from direction
       end-if
EXIT.

DetermineDoorCandidacy.
       MOVE "Y" TO DoorCandidate(Direction)
       perform DetermineNextPosition
       if NextColumn is less than 1 or nextrow is less than 1 or nextcolumn is greater than 8 or nextrow is greater than 8 then 
           MOVE "N" TO DoorCandidate(Direction)
           exit
       end-if
       if state(nextcolumn, NextRow) is not equal to "I" then 
           move "N" to DoorCandidate(Direction)
       end-if
Exit.

DetermineMazeGenComplete.
       SET GenerationComplete TO TRUE
       PERFORM VARYING MazeColumn FROM 1 BY 1 UNTIL MazeColumn IS GREATER THAN 8
           perform varying MazeRow from 1 by 1 until MazeRow IS GREATER THAN 8
               if State(MazeColumn, MazeRow) is equal to "F" then 
                   SET GenerationIncomplete TO TRUE
                   exit
               end-if
           end-perform
       end-perform
EXIT.

DetermineNextPosition.
       MOVE MazeColumn TO NextColumn
       MOVE MazeRow TO NextRow
       EVALUATE Direction
           WHEN 1
               SUBTRACT 1 FROM NextRow
           WHEN 2
               ADD 1 TO NextColumn
           WHEN 3
               ADD 1 TO NextRow
           WHEN 4
               SUBTRACT 1 FROM NextColumn
       END-EVALUATE
EXIT.
