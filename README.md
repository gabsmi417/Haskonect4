# project-cis5520
Name: Susan Zhang, Gabriel Smith
PennKeys: szhang25, gabsmi

## Module organization
Haskell packages typically divide their source code into three separate places:

  - src:
    Read our code in the following order
    - Board.hs
      - includes data structures defining the game board, columns,
        players (colors), win state
      - has functions such as insert and win condition checks for game play mechanism
      - has functions for getting positions that if played, allows a player to win;
        this is for the eval function for the Minimax algorithm
      - defines many example boards, unit tests, and QuickCheck props
    - Minimax.hs
      - where our Negamax algorithm with alpha beta pruning is
      - defines the evaluation function and heuristics for short-circuiting in the tree search
      - has helper functions for prop testing, allowing the bot
        to play with a certain depth/number of steps against itself
      - defines QuickCheck props
    - Game.hs
      - allows the user to pick whether they want to play against someone
        or our AI bot
      - this is where the IO is done for our game; gets the moves that the user enter
        in the command line and either performs the move or outputs an error message
        if the move is invalid
      - also prints out the board, messages during gameplay, and winning/losing value
        outputted by the AI bot if the user chooses to play against the bot
    * note: State.hs is from lecture so no need to read
  
  - app/Main.hs: the entry point for our executable 
  
  - test/Spec.hs: can run all of our test cases

## Building, running, and testing

This project compiles with `stack build`. 
You can start the game with `stack run`.
You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.
