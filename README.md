# CSE230 Final Project
#### Group Members: Ruoqi(Rachel) Yang, Ruichun Yang, Angela Chen, Maggie Zhao

### Rules:
Hangman is a 2-player game where Player 1 thinks of a word that Player 2 tries to guess. The screen will display an underscore for each letter in the word, and Player 2 will guess letters in succession. If the guessed letter is contained in the word, the letter will appear in the correct slot(s). If the guessed letter is not the word, one body part will be added to the hangman figure. If the word is not guessed before all body parts are drawn, Player 2 loses the game. Otherwise, Player 2 wins.

### Our project:
Our goal is to build a command-line version of hangman using the brick library. We will begin by implementing a one-player version of hangman, where the computer (Player 1) draws a word from a word-base of a user-selected difficulty, and the user (Player 2) tries to guess the word. Next, we will implement a two-player version of hangman.

### Interface Design
We will start with a welcome screen where users select a difficulty level and start the game by clicking a start button.
The main game interface will consist of two sides. The left side will contain the graphical interface of a hangman figure tracking the process of the user’s mistakes so far. The right side will contain empty squares as placeholders for unknown characters and display known characters at corresponding places and in the bottom, we plan a keyboard component that allows the player to select letters. 

### Possibly adding network
Our reach goal for this project is to implement multi-user experience, enabling interactive user experience across different machines. 