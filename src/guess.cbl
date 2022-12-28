       IDENTIFICATION DIVISION.
       PROGRAM-ID. GUESS.
       DATA DIVISION.
           working-storage section.
           01 Prng-Data.
               02 dateTimeString PIC X(16).
               02 dateTime PIC 9(16) USAGE IS COMP VALUE ZEROS.
               02 dummy PIC 999.
           
           01 Game-Data.
               02 Target PIC 999.
               02 Guess PIC 999.
               02 Guess-Count PIC 99.
               02 Done PIC 9 VALUE ZEROS.
               02 Choice PIC X(1).
       PROCEDURE DIVISION.
           perform Seed-Rng
           MOVE 0 TO done
           perform until done is equal to 1
               compute Target= FUNCTION RANDOM() * 100 + 1
               MOVE 0 TO Guess-Count
               move 0 to Guess
               perform until guess is equal to target
               display "Guess my number(1-100)" with no advancing 
               accept Guess
               compute Guess-Count = Guess-Count + 1
           if guess<1 or guess>100 then 
               display "Please choose a number between 1 and 100"
           end-if
           if guess is greater than 0 and guess is less than 101 and 
               guess is less than target then 
               display "Yer guess is too low!"
           end-if
           if guess is greater than 0 and guess is less than 101 and 
               guess is greater than target then 
               display "Yer guess is too high!"
           end-if
           if guess is greater than 0 and guess is less than 101 and 
               guess is equal to target then 
               display "Yer right!"
           end-if
           end-perform
               display "It took you " Guess-Count " guesses!"
               display "Would you like to play again? (y/n)"
                   with no advancing
               accept Choice
               if choice is equal to "n" or choice is equal to "N" then 
                   move 1 to done
               end-if
           end-perform
       STOP RUN.
       Seed-Rng.
           MOVE FUNCTION CURRENT-DATE TO dateTimeString
           MOVE FUNCTION NUMVAL(dateTimeString) TO dateTime
           COMPUTE dummy = FUNCTION RANDOM(dateTime)
       Exit.
