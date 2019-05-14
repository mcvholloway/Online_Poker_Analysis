<h1> An Analysis of the Profitability of Online Poker Strategies </h2>
This project explores a dataset of one million hands of Texas Hold'em Poker played on a popular online poker site between September and November of 2018.
Half of the data comes from "low stakes" ($0.01/$0.02 blinds) games and the other half from "high stakes" ($1.00/$2.00 blinds). All data comes from 6-max tables, and hands feature between 3 and 6 players.
For detailed explanation of the rules of Texas Hold\'em, see  <a href="https://www.cardplayer.com/rules-of-poker/how-to-play-poker/games/texas-holdem", target="_blank">how to play Texas Hold\'em.</a>

<h2> Data Question </h2>
Poker is generally thought of as a game of skill, where some players are able to make a profit in the long-term. However, there is a great deal of randomness and luck involved in the game, making it difficult to analyze the effectiveness of strategies without a sufficiently large sample size. Finally, poker is a game of hidden information with aspects of psychology, making exact analysis from a mathematics and game theory perspective difficult.

The first goal of this project is to explore differences in playing style between profitable players and losing players. For example, do profitable players choose to raise more often than unprofitable players? A secondary aspect to this goal is to compare common poker strategy advice (for example, players should never enter a hand by just calling the big blind) to real-world results.

Second, since the dataset contains data for both very small stakes ($0.02 big blind) and moderate stakes ($2.00), another goal is to examine differences in playstyles between stakes levels. An unskilled players playing $2.00 blinds will most likely quickly go broke, which means that the overall skill level of the higher stakes players will tend to exceed that of lower stakes players. What differences are there in these two groups of players?

<h2> Shiny App Description </h2>
To allow for exploration of the data, an app has been created with R Shiny. It can be found at <a href = "https://mholloway.shinyapps.io/Poker/" target="_blank"> https://mholloway.shinyapps.io/Poker/ </a>. The body of the app is divided into five tabs.

<h3> Tab 1: Profitability Analysis </h3>

This tab addresses the question of identifying the percentage of players who were able to play profitably. Since there is so much variance in profits/losses, it is important to choose a suffiently large sample size to estimate a player's longterm win/loss rate, and this tab allows the user to set the minimum cutoff for number of hands played. As the minimum number of hands increases, the proportion of players who were profitable increases as well. One possible explanation for this is survivorship bias. First, a player would likely not be able to sustain a bankroll playing a large number of hands unprofitably, and second those individuals playing a large quanity of hands and more frequently are likely more skilled than those playing only occasionally.

<h3> Tab 2: Player Tendencies </h3>

This tab explores correlations between numerous metrics and players' average profit/loss rate as well as differences across stakes levels and between the group of profitable players and unprofitable players.

<h3> Tab 3: Positional Advantage </h3>

It is generally accepted that players who act later in a hand are at a significant advantage, having more information when it is their turn to act. The effect of position is explored in this tab.

<h3> Tab 4: Breakdown by Betting Street </h3>
This tab provides a Sankey diagram illustrating the path that all hands took from preflop to either winning or losing. It demonstrates the fraction of hands that were played to each betting street and the fraction that were played to showdown. An interesing observation from this tab is that profitable players won a smaller percentage of the hands that they played (but made up for it by winning larger pots).

<h3> Tab 5: Showdown Summary </h3>
This tab give a breakdown of the number and proportion of hands won at showdown by each type of hand, as well as the average size of the pot that occurred for each possible combination of winning and losing hand.