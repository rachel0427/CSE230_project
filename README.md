# CSE230 Final Project
#### Group Members: Ruoqi(Rachel) Yang, Ruichun Yang, Angela Chen, Maggie Zhao

Our goal is to create a survival game. You are stranded on an island. Your goal is to survive for 10 days so that you can be rescued.

### Rules of the game:
The Time System: Each day has 5 time blocks that the player can use to allocate activities. The player will be provided with a range of activities (randomly generated from a wide variety of predefined activities) to choose from. Some activities may have probabilistic outcomes.

#### Survival Criteria: 
The results of the player's choices will affect several criterias, including the health bar, the hunger bar, and the thirst bar. The values of these bars change in accordance to the activities the player select, time elapses, and the weather. If either of the hunger or thirst bar depletes, the player’s health will drop dramatically over each time block. Once the health value reaches 0, the game ends.


#### The Weather System: 
On each game day, a type of weather will be randomly generated. Depending on the weather, the player's health, hunger, and thirst will decrease at different rates.

  Sunny day – thirst bar decreases faster

  Rainy day – hunger bar decreases faster

  Stormy day – health bar decreases faster

#### Examples of Activities 
There are various activities for user to choose. Different activites consume varying units of time and have different effects on the character's state. Here are some examples:

  Picking Coconuts (Cost: 2 time blocks. Reward: +5 to hunger bar, +10 to thirst bar)

  Fishing (Cost: 2 time blocks. Reward: 1-3 fish, +5 to hunger bar per fish, +5 to health bar per fish)

  (Some activities may yield variable rewards according to a predetermined probability distribution.)

### Interface Design
The UI will display the bars of survival and a brief prompt that leads the player’s choices, 3-4 text based choices or visual choices (eg. coconut, fish,object icons) will be displayed in the center of the interface. We also plan to have some decorative arts to make the interface more realistic and engaging.
