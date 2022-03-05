       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIZBUZ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ITERATOR             PIC 9(2) VALUE 00.
      * Modulus operator output WS-MOD-FIZZ and WS-MOD-BUZZ
       01  WS-MOD-FIZZ             PIC 9(2) VALUE 00.
       01  WS-MOD-BUZZ             PIC 9(2) VALUE 00.
       01  WS-FIZZ                 PIC X(4) VALUE 'FIZZ'.
       01  WS-BUZZ                 PIC X(4) VALUE 'BUZZ'.
       01  WS-FIZZBUZZ-OUT         PIC X(8) VALUE SPACES.
       01  WS-FIZZBUZ-CHAR-COUNT   PIC 9(2) VALUE 00.

       PROCEDURE DIVISION.
      *------------
      * Primary function that iterates over FIZZ-BUZZ-STEPS
      *------------
       DO-FIZZ-BUZZ.
           PERFORM UNTIL WS-ITERATOR = 20
            PERFORM FIZZ-BUZZ-STEPS
           END-PERFORM.
           GOBACK.

      *------------
      * Procedure for doing fizzbuzz
      *------------
       FIZZ-BUZZ-STEPS.
           PERFORM PREP-ITERATION-DATA.
           PERFORM CALC-FIZZ-BUZZ.
           PERFORM CALC-OUTPUT.

      *------------
      * Clean up and initizalize variables for each iteration
      *------------
       PREP-ITERATION-DATA.
           ADD 1 TO WS-ITERATOR.
           MOVE SPACES TO WS-FIZZBUZZ-OUT.
           MOVE 00 TO WS-FIZZBUZ-CHAR-COUNT.
           COMPUTE WS-MOD-FIZZ = FUNCTION MOD(WS-ITERATOR 3).
           COMPUTE WS-MOD-BUZZ = FUNCTION MOD(WS-ITERATOR 5).

      *------------
      * Calculate which word(s), if any, should be output
      *------------
       CALC-FIZZ-BUZZ.
      * Add fizz to output
           IF WS-MOD-FIZZ = 0 THEN
            MOVE WS-FIZZ TO WS-FIZZBUZZ-OUT
           END-IF.
      * Add buzz to output
           IF WS-MOD-BUZZ = 0 THEN
            MOVE WS-BUZZ TO WS-FIZZBUZZ-OUT
      * Add fizzbuzz to output
           IF WS-MOD-FIZZ = 0 AND WS-MOD-BUZZ = 0 THEN
            STRING
             WS-FIZZ DELIMITED BY SPACES
             WS-BUZZ DELIMITED BY SPACES
             INTO WS-FIZZBUZZ-OUT
            END-STRING
           END-IF.

      *------------
      * Calculate whether to display a number or string in output
      *------------
       CALC-OUTPUT.
           INSPECT WS-FIZZBUZZ-OUT
            TALLYING WS-FIZZBUZ-CHAR-COUNT FOR CHARACTERS
            BEFORE INITIAL SPACE.
           IF WS-FIZZBUZ-CHAR-COUNT = 00 THEN
            DISPLAY WS-ITERATOR
           ELSE
            DISPLAY WS-FIZZBUZZ-OUT
           END-IF.
