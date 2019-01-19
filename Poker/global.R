library(shinydashboard)
library(tidyverse)
library(tools)
library(plotly)
library(DT)
library(corrplot)

players <- readRDS('../data/players.RDS')
positions <- readRDS('data/position.RDS')
player_positions <- readRDS('data/player_position.RDS')
winning_hands <- readRDS('data/showdown.RDS')
by_street <- readRDS('data/by_street.RDS')
tactics_breakdown <- readRDS('data/tactics_breakdown.RDS')
tactics_profitability <- readRDS('data/tactics_profitability.RDS')
sd <- readRDS('data/hand_pairings.RDS')

players$Stakes <- factor(players$Stakes, levels = c('Low', 'High'))
positions$Stakes <- factor(positions$Stakes, levels = c('Low', 'High'))

library(RColorBrewer)
myColors <- brewer.pal(2,"Set1")
names(myColors) <- levels(positions$Stakes)
colScale <- scale_colour_manual(name = "Stakes",values = myColors)

sankey_values = list('low_prof' = 100*c(0.07897744, 0.68157517,0.23944740,0.03110017, 0.05492542, 0.15342181,
                           0.01653835, 0.02773253, 0.10915093, 0.01256784, 0.02076179, 0.07582130,
                           0.04198473, 0.03383657),
                     'low_unprof' = 100*c(0.07208834, 0.64186938,0.28604228,0.03401224, 0.06223925, 0.18979080,
                                          0.01938006, 0.03227579, 0.13813495, 0.01657484, 0.02476544, 0.09679468,
                                          0.04880913, 0.04798555),
                     'low_all' = 100*c(0.07372287, 0.61045945,0.31581768, 0.03740379, 0.06813551, 0.21027839,
                                       0.02174926, 0.03380247, 0.15472666, 0.02004301, 0.02655924, 0.10812441,
                                       0.05345117, 0.05467324),
                     'high_all' = 100*c(0.09864491, 0.67782713,0.22352796, 0.03068192, 0.04338712, 0.14945892,
                                        0.02055993, 0.02642569, 0.10247330, 0.01593575, 0.01830809, 0.06822946,
                                        0.03424850, 0.03398096),
                     'high_prof' = 100*c(0.09861094, 0.71118750,1-0.80979845,0.02562964, 0.03792403, 1-0.80979845-0.06355368,
                                         0.01741412, 0.02340839, 1-0.80979845-0.06355368-0.04082251, 0.01292370, 0.01640427, 1-0.80979845-0.06355368-0.04082251-0.02932797,
                                         0.03024620, 0.02625120),
                     'high_unprof' = 100*c(0.10060700, 0.68611387,1-0.78672087,0.02939804, 0.04202110, 1-0.78672087-0.07141914,
                                           0.01945674, 0.02562136, 1-0.78672087-0.07141914-0.04507810, 0.01465005, 0.01815273, 1-0.78672087-0.07141914-0.04507810-0.03280278,
                                           0.03194004, 0.03203908))

fill_matrix <- function(sd, column){
  values = c()
  k = 1
  for (i in 1:9){
    for (j in 1:9){
      if (i >= j){values = c(values, sd[[k, column]])
      k = k+1}
      else{values = c(values, 0)}
    }
  }
  mat <- matrix(values, nrow = 9)
  return (mat)
}

descriptions = c('Call_Ratio' = 'The <b>Calling Ratio</b> measures the percentage of all actions that a player took that were either checks or calls. It is defined as $$Calling\\ Ratio = \\frac{checks + calls}{checks + calls + bets + raises + folds}$$',
                 'Fold_Ratio' = 'The <b>Folding Ratio</b> measures the percentage of all actions that a player took that were folds. It is defined as $$Folding\\ Ratio = \\frac{folds}{checks + calls + bets + raises + folds}$$',
                 'Raise_Ratio' = 'The <b>Raising Ratio</b> measures the percentage of all actions that a player took that were either bets or raises. It is defined as $$Raising\\ Ratio = \\frac{bets + raises}{checks + calls + bets + raises + folds}$$',
                 'VPIP_Ratio' =
                   'The <b>VPIP Ratio</b> measures the percentage of hands that a player chose to play (not counting blinds). VPIP stands for <b>V</b>oluntary <b>P</b>ut Money <b>I</b>n <b>P</b>ot.',
        
                 'Winning_Ratio'=
                   'The <b>Winning Ratio</b> measures that percentage of hands that a player won some portion of the pot out of all hands they were dealt.',
                 "VPIP_Win_Ratio" =
                   'The <b>VPIP Winning Ratio</b> measures the percentage of hands for which a player won some portion of the pot out of hands that the player voluntarily chose to play.',
                 "All_In_Ratio" = 
                 "Going <b>all-in</b> means to put all of one\'s chips into the pot. The <b>All-In Ratio</b> measures how often a player went all-in as a percentage of hands voluntarily played.",
                 "Preflop_Aggression" = 
                   '<b>Preflop Aggression</b> measures how frequently a player entered the hand with a raise as a percentage of total hands voluntarily played. This counts by hands in which a player was the first player to raise and hands in which the player reraised an opponent\'s raise.',
                 "Three_Bet_Ratio" =
                   'A <b>three bet</b> occurs when the original preflop raiser is re-raised by another player.
                 The <b>Three Bet Plus Ratio</b> measures how frequently a player entered the hand by reraising another player\'s bet as a percentage of total hands voluntarily played.',
                 'PF_Raise_Ratio' =
                 'The <b>Preflop Raise Ratio</b> measues how frequently a player entered the hand as the first raiser.',
                 'PF_Call_Ratio' =
                 'The <b>Preflop Call Ratio</b> measues how frequently a player entered the hand by calling rather than raising as a percentage of total hands voluntarily played.',
                 'Limp' =
                 'To <b>Limp</b> means to enter a hand by calling the big blind instead of raising. Limping can only occur in an unraised pot. The <b>Limp Ratio</b> is defined as
                 $$Limp\\ Ratio = \\frac{Number\\ of\\ Limps}{Number\\ of\\ Limp\\ Opportunities}$$',
                 'CB' =
                 'A bet from a player on the flop who raised pre-flop is known as a <b>continuation bet.</b> The <b>Continuation Bet Ratio</b> is defined as
$$Continuation\\ Bet\\ Ratio = \\frac{Number\\ of\\ Continuation\\ Bets}{Number\\ of\\ Continuation\\ Bet\\ Opportunities}$$',
'Donk' =
'<b>Donk bet</b> refers to a postflop bet in which a player who is out of position bets before the preflop aggressor. The <b>Donk Bet Ratio</b> is defined as
$$Donk\\ Bet\\ Ratio = \\frac{Number\\ of\\ Donk\\ Bets}{Number\\ of\\ Donk\\ Bet\\ Opportunities}$$',
'CR' =
'A <b>Check-Raise</b> is a trapping move consisting of checking when the action\'s on you, and raising after a player behind you has bet. The <b>Check-Raise Ratio</b> is defined as
$$Check\\ Raise\\ Ratio = \\frac{Number\\ of\\ Check\\ Raises}{Number\\ of\\ Check\\ Raise\\ Opportunities}$$',
'Showdown_Ratio' = 
'The <b>showdown</b> occurs after the last round of betting. If any players remain in the hand at showdown, those players expose and compare their hands to determine the winner or winners. The <b>Showdown Ratio</b> measures how frequently a player was still in the hand at showdown as a percentage of total hands voluntarily played.',
'Showdown_Winning_Ratio' =
'The <b>Showdown Winning Ratio</b> measures how frequently a player won some portion of the pot at showdown out of all showdown hands for that player.',
'Call_Pct' = 'The <b>Calling Ratio</b> measures the percentage of the times that a player called when facing a bet. It is defined as $$Calling\\ Ratio = \\frac{calls}{calls + raises + folds}$$',
'Raise_Pct' = 'The <b>Raising Ratio</b> measures the percentage of the times that a player raised when facing a bet. It is defined as $$Raising\\ Ratio = \\frac{raises}{calls + raises + folds}$$',
'Fold_Pct' = 'The <b>Folding Ratio</b> measures the percentage of the times that a player folded when facing a bet. It is defined as $$Folding\\ Ratio = \\frac{folds}{calls + raises + folds}$$',
'Bet_Pct' = 'The <b>Betting Ratio</b> measures the percentage of the times that a player bet when checked to. It is defined as $$Betting\\ Ratio = \\frac{bets}{bets + checks}$$',
'Check_Pct' = 'The <b>Checking Ratio</b> measures the percentage of the times that a player checked when checked to. It is defined as $$Checking\\ Ratio = \\frac{checks}{bets + checks}$$'
)
