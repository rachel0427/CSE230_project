# CSE230 Final Project
#### Group Members: Ruoqi(Rachel) Yang, Ruichun Yang, Angela Chen, Maggie Zhao

### How to execute the program
Since we have written a Makefile, you can build and execute the program with 
```
$ make
```

Our goal is to create a survival game. You are stranded on an island. Your goal is to survive for 10 days so that you can be rescued.

### Rules of the game:
The Time System: Each day has 5 time blocks that the player can use to allocate activities. The player will be provided with a range of activities (randomly generated from a wide variety of predefined activities) to choose from. Some activities may have probabilistic outcomes.

#### Survival Criteria: 
The results of the player's choices will affect several criterias, including the health bar, the hunger bar, and the thirst bar. The values of these bars change in accordance to the activities the player select, time elapses, and the weather. If either of the hunger or thirst bar depletes, the player’s health will drop dramatically over each time block. Once the health value reaches 0, the game ends.

#### The Weather System: 
On each game day, a type of weather will be randomly generated. Depending on the weather, the player's health, hunger, and thirst will decrease at different rates.

  Sunny day – thirst bar decreases faster

  Rainy day – hunger bar decreases faster

  Cloudy day – health bar decreases faster

#### Examples of Activities 
There are various activities for user to choose each day. Different activites consume varying units of time and have different effects on the character's state. Here are some examples:

  Picking Coconuts (Cost: 2 time blocks. Reward: +5 to hunger bar, +10 to thirst bar)

  Fishing (Cost: 2 time blocks. Reward: 1-3 fish, +5 to hunger bar per fish, +5 to health bar per fish)

  (Some activities may yield variable rewards according to a predetermined probability distribution.)

### Interface Design
The UI will display the bars of survival and a brief prompt that leads the player’s choices, 3-4 text based choices or visual choices (eg. coconut, fish,object icons) will be displayed in the center of the interface. We also plan to have some decorative arts to make the interface more realistic and engaging.

### Update for Milestone 2 (12/1)
#### Overview
Over the past weeks, we completed an initial simplified version of our game and set a good foundation for future updates. We have implemented a simple user interface that displays player status (hunger, thirst, health), weather status, and date in text format. In each in-game day, a series of options (randomly selected from a predefined pool) are displayed to the player, and the player's selection leads to a status update.  

#### Architecture

##### UI Component
We implemented user interfaces for the menu and in-game playtime, presenting information and optional operations. The in-game interface includes details such as weather conditions, date, character status, and descriptions of available activities during the game.

##### Game Logic
We implemented our main game logic in the `Game.hs` file that handles generating initial player status, activity options, and player status updates based on player input. We defined the basic characteristics of the activities (e.g. text description, activity effect) in the  `src/Activity.hs` file. It includes a pool of predefined activities that would be randomly selected and displayed to the player.

#### Challenges and Solutions
  1. Our very first challenge is coordinating our work as a group. Since we are using different operating systems and have different schedules, we find it difficult at first to handle collaboration as well as version control. Our solution was to plan our meetings ahead and try our best to allocate large chunks of time to work together as a team. To unify our development environment, we choose to work with GitHub Codespace. We also practiced personal branching strategy to maintain version control and merge upon group revision. At the same time, we split our work into two main categories - UI design, **user interaction handling** and **game logic implementation**. When both team completes their individual work, we then come together to merge our results.
  2. At the current state, we have implemented the UI with basic text-based structures. Our goal, however, includes developing a more advanced and visually pleasing UI. For example, we want to incorporate visual cues for different weather conditions, player status, as well as effects of player actions. The main challenge in this task is correctly representing and displaying the dynamic relationship between our game components. At the same time, the UI should still present choices, whether text-based or graphic-based, with clarity and easy accessibility. To address this challenge, we believe that our current version of the game has laid good foundation in representing the relationships between game components. In the future, we plan on incorporating various ASCII arts into different stages of the game play to improve player experience. 
#### Expectations
We believe we are at a good place in our development and expect to complete the game by the deadline. However, during our development, we have made some updates to our goals and designs that would benefit the overall outcome and user experience.

  1. We have decided to remove the time block feature so the player only performs 1 activity per day. During our development, we realized that adding the time block feature would add too much complexity as well as repetitiveness to this game, and removing this feature would improve clearity and player experience. 
  2. Since we have implemented the foundational logic of the game, we want to make improvements regarding visual aspects of the game. We plan to add some ASCII arts to our game interface.
  3. We also want to integrate the logic of early stopping the game when user triggers some selection, for example calling for help and succeeding or deer hunting and get hurt. This adds to the randomness of our game and makes it more playable.
  4. The weather's influence on the survival play status is yet to be implemented. We expect to incorporate the changes in our next developmental stage.
  5. We plan to add ASCII art and decorations: integrate ASCII art or other decorative elements to enhance the visual appeal and theme of the game.  They possibly include sun, clouds, rain etc.
  6. We plan to use brick's widgets, such as ProgressBar or custom widgets, to represent the health, hunger, and thirst bars dynamically. Update these widgets based on player choices, time progression, and weather conditions.
  7. We plan to utilize the library's support for colors to enhance the visual appeal. Assign distinct colors to different elements such as bars, prompts, and choices. For example, you can use green for health, blue for hunger, and yellow for thirst.
