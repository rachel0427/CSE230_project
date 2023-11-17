# CSE230 Final Project
#### Group Members: Ruoqi(Rachel) Yang, Ruichun Yang, Angela Chen, Maggie Zhao

Our goal is to create a survival game. You are stranded on an island. Your goal is to survive for 10 days so that you can be rescued.

#### Rules of the game:
The Time System: Each day has 5 time blocks, and you can choose to do different activities during those time blocks. The user will be given a choice to do some activities (from a wide range of predefined options) that will benefit their survival. Some activities may have probabilistic results.

#### Survival Criteria: 
The result of the user's choices will affect several criterias, including the health bar, the hunger bar, and the thirst bar. The values of these bars change in accordance to the activities the player chooses, time elapses, and the weather. If either of the hunger or thirst bar depletes, the player’s health will drop dramatically each time block. Once the health value reaches 0, the game ends.


#### The Weather System: 
On each game day, a type of weather will be randomly generated. Depending on the weather, your health, hunger, and thirst will decrease at different rates.

  Sunny day – your thirst bar will decrease faster

  Rainy day – your hunger bar will decrease faster

  Stormy day – your health bar will decrease faster

#### Examples of Activities 
(X and Y below are Random Variables that follow some predetermined distribution)

  Picking Coconuts (Cost: 2 time blocks. Reward: +X to hunger bar, +Y to thirst bar)

  Fishing (Cost: 2 time blocks. Reward: 1-3 fish, +X to hunger bar per fish, +Y to health bar per fish)

#### Interface Design
The UI will display the bars of survival and a brief prompt that leads the user’s choices, 3-4 text based choices or visual choices (eg. coconut, fish,object icons) will be displayed in the center of the interface. We also plan to have some decorative arts to make the interface more realistic and engaging.
