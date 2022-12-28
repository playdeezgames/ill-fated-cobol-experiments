       IDENTIFICATION DIVISION.
       PROGRAM-ID. WANDER.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 GameData.
              02 X PIC 999 VALUE 0.
              02 Y PIC 999 VALUE 0.
              02 Facing PIC X VALUE "N".
              01 GameOver PIC X VALUE "N".

           01 ScratchPad.
              02 Command PIC X(10).
       PROCEDURE DIVISION.
       PERFORM InitializeGame
       PERFORM WITH TEST AFTER UNTIL GameOver IS EQUAL TO "Y"
           PERFORM ShowStatus
           PERFORM HandleCommand
       END-PERFORM
       STOP RUN.

       InitializeGame.
       MOVE 500 TO X
       MOVE 500 TO Y
       MOVE "N" TO Facing
       EXIT.

       ShowStatus.
       DISPLAY " "
       DISPLAY "Yer at (" X "," Y ")"
       DISPLAY "Yer facing " WITH NO ADVANCING
       EVALUATE Facing
        WHEN "N"
           DISPLAY "north."
        WHEN "E"
           DISPLAY "east."
        WHEN "S"
           DISPLAY "south."
        WHEN "W"
           DISPLAY "west."
       END-EVALUATE
       EXIT.

       HandleCommand.
       DISPLAY "Now What? " WITH NO ADVANCING
       ACCEPT Command
       EVALUATE Command
        WHEN "?"
           PERFORM ShowHelp
        WHEN "M"
        WHEN "m"
           PERFORM DoMove
        WHEN "L"
        WHEN "l"
           PERFORM TurnLeft
        WHEN "R"
        WHEN "r"
           PERFORM TurnRight
        WHEN "A"
        WHEN "a"
           PERFORM TurnAround
        WHEN "Q"
        WHEN "q"
           PERFORM QuitGame
       END-EVALUATE
       EXIT.

       ShowHelp.
       DISPLAY " "
       DISPLAY "Help:"
       DISPLAY " ? - Help"
       DISPLAY " M - Move forward"
       DISPLAY " L - Turn left"
       DISPLAY " R - Turn right"
       DISPLAY " A - Turn around"
       DISPLAY " Q - Quit"
       EXIT.

       DoMove.
       EVALUATE Facing
        WHEN "N"
           SUBTRACT 1 FROM Y
        WHEN "E"
           ADD 1 TO X
        WHEN "S"
           ADD 1 TO Y
        WHEN "W"
           SUBTRACT 1 FROM X
       END-EVALUATE
       EXIT.

       TurnLeft.
       EVALUATE Facing
        WHEN "N"
         MOVE "W" TO Facing
        WHEN "E"
         MOVE "N" TO Facing
        WHEN "S"
         MOVE "E" TO Facing
        WHEN "W"
         MOVE "S" TO Facing
       END-EVALUATE
       EXIT.

       TurnRight.
       EVALUATE Facing
        WHEN "N"
         MOVE "E" TO Facing
        WHEN "E"
         MOVE "S" TO Facing
        WHEN "S"
         MOVE "W" TO Facing
        WHEN "W"
         MOVE "N" TO Facing
       END-EVALUATE
       EXIT.

       TurnAround.
       EVALUATE Facing
        WHEN "N"
         MOVE "S" TO Facing
        WHEN "E"
         MOVE "W" TO Facing
        WHEN "S"
         MOVE "N" TO Facing
        WHEN "W"
         MOVE "E" TO Facing
       END-EVALUATE
       EXIT.

       QuitGame.
       MOVE "Y" TO GameOver
       DISPLAY " "
       DISPLAY "Thanks for playing!"
       EXIT.
