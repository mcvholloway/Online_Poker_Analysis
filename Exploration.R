library(tidyverse)
library(anonymizer)
library(digest)

hands <- read_csv('data/processed_12.csv') %>% select(-X1)

hands <- hands %>% 
  mutate_at(c('Limp', 'Call', 'Raise', 'Winner', 'VPIP', 
              'Blind', 'In_Showdown', 'All_In', 'CB_Opportunity', 'CB', 'Winner'), 
            as.logical)

by_player <- hands %>%
  #filter(num_players == 6) %>% 
  group_by(Player) %>% 
  summarize(Total_PL = sum(Amt),
            Num_Hands = n(), 
            #Limp_Ratio = sum(Limp)/sum(VPIP),
            Showdown_Ratio =sum(In_Showdown & VPIP)/sum(VPIP),
            Showdown_Winning_Ratio = sum(In_Showdown & Winner)/sum(In_Showdown),
            All_In_Ratio = sum(All_In & VPIP)/sum(VPIP),
            VPIP_Ratio = mean(VPIP),
            Winning_Ratio = mean(Winner),
            VPIP_Win_Ratio = sum(Winner & VPIP)/sum(VPIP),
            #Num_CB_Opp = sum(CB_Opportunity), 
            #Num_CB = sum(CB),
            Raise_Ratio = sum(num_raises) / sum(num_actions),
            Call_Ratio = sum(num_calls) / sum(num_actions),
            Fold_Ratio = sum(num_folds) / sum(num_actions),
            Median_PL = median(Amt)
            #Total_Actions = sum(num_actions), 
            #Total_Raises = sum(num_raises),
            #Total_Folds = sum(num_folds), 
            #Total_Calls = sum(num_calls)
            ) %>% 
  mutate(PL_Per_Hand = Total_PL / Num_Hands / .02
         #Limp_Ratio = mean(Limp), 
         #Call_Ratio = Num_Calls / Num_Hands, 
         #Raise_Ratio = Num_Raises / Num_Hands, 
         #VPIP_Ratio = Num_VPIP / Num_Hands, 
         #CB_Ratio = Num_CB/Num_CB_Opp, 
         #Raising_Ratio = Total_Raises/ Total_Actions, 
         #Calling_Ratio = Total_Calls / Total_Actions, 
         #Folding_Ratio = Total_Folds/ Total_Actions
         #Winning_Ratio = Total_Wins/Num_Hands, 
         #All_In_Ratio = Total_All_In / Num_Hands
         ) %>% 
  ungroup()

by_player <- by_player %>% mutate(Profitable = PL_Per_Hand > 0)
by_player$Stakes <- rep('Low', nrow(by_player))

preflop <- read_csv('data/preflop0102.csv')

preflop <- preflop %>% select(-X1)
preflop <- preflop %>% mutate(Position = case_when(
  Position == 'Player_1' ~ 'Button',
  (Position == 'Player_2') & (num_players == 3) ~ 'Big Blind',
  (Position == 'Player_2') ~ 'Cutoff',
  (Position == 'Player_3') & (num_players == 3) ~ 'Small Blind',
  (Position == 'Player_3') & (num_players == 4) ~ 'Big Blind',
  (Position == 'Player_3') ~ 'Hijack',
  (Position == 'Player_4') & (num_players == 4) ~ 'Small Blind',
  (Position == 'Player_4') & (num_players == 5) ~ 'Big Blind',
  (Position == 'Player_4') ~ 'Under the Gun',
  (Position == 'Player_5') & (num_players == 5) ~ 'Small Blind',
  (Position == 'Player_5') ~ 'Big Blind',
  TRUE ~ 'Small Blind'
))


preflop$Preflop_Action <- factor(preflop$Preflop_Action,
                                 levels = c('Did Not Play','Walk', 'Check', 'Fold', 'Call',
                                            'Raise', 'Three Bet', 'Four Bet', 'Five Bet',
                                            'Six Bet', 'Seven Bet', 'Eight Bet', 'Nine Bet',
                                            'At Least Ten Bet'))

preflop$Position <- factor(preflop$Position, levels = c('Small Blind', 'Big Blind',
                                                        'Under the Gun', 'Hijack',
                                                        'Cutoff', 'Button'))

preflop <- preflop %>% mutate(Action = case_when(
  Preflop_Action == 'Walk' ~ 'Walk',
  Preflop_Action == 'Check' ~ 'Check',
  Preflop_Action == 'Fold' ~ 'Fold',
  Preflop_Action == 'Call' ~ 'Call',
  Preflop_Action == 'Raise' ~ 'Raise',
  TRUE ~ 'Three_Bet_Plus'
))

preflop_by_player <- preflop %>% 
  group_by(Player) %>% 
  mutate(countT = n()) %>% 
  group_by(Player,Action) %>% 
  mutate(Pct = n() / countT) %>% 
  group_by(Player, Action) %>% summarize(Pct = mean(Pct))

by_player <- inner_join(by_player, preflop_by_player %>% spread(Action, Pct))
by_player <- by_player %>%
  mutate(Preflop_Aggression = (Raise + Three_Bet_Plus)/(Raise + Three_Bet_Plus + Call), 
         Three_Bet_Ratio = Three_Bet_Plus/(Raise + Three_Bet_Plus + Call),
         PF_Raise_Ratio = Raise/(Raise + Three_Bet_Plus + Call),
         PF_Call_Ratio = Call/(Raise + Three_Bet_Plus + Call))

tactics <- read_csv('data/tactics0102.csv')
tactics <- tactics %>% select(-X1)
tactics <- tactics %>% mutate_at(c("CB_Opportunity","CB","Donk_Opportunity","Donk", 
                                   "Limp_Opportunity", "Limp", "CR_Opportunity","CR"), 
                                 as.logical)
tactics_breakdown_high = rbind(
  tactics %>% filter(Limp_Opportunity) %>% group_by(Limp) %>% summarize(Amt = mean(Amt), Total = n()) %>% mutate(Tactic = 'Limp') %>% rename(Y_N = Limp),
  tactics %>% filter(CB_Opportunity) %>% group_by(CB) %>% summarize(Amt = mean(Amt), Total = n()) %>% mutate(Tactic = 'Continuation Bet') %>% rename(Y_N = CB),
  tactics %>% filter(Donk_Opportunity) %>% group_by(Donk) %>% summarize(Amt = mean(Amt), Total = n()) %>% mutate(Tactic = 'Donk Bet') %>% rename(Y_N = Donk),
  tactics %>% filter(CR_Opportunity) %>% group_by(CR) %>% summarize(Amt = mean(Amt), Total = n()) %>% mutate(Tactic = 'Check-Raise') %>% rename(Y_N = CR)
)

tactics_breakdown_high <- tactics_breakdown_high %>% mutate(Amt = Amt/2)
tactics_breakdown_high$Stakes <- rep('High', nrow(tactics_breakdown_high))
tactics_breakdown_low <- tactics_breakdown_low %>% mutate(Amt = Amt/.02)
tactics_breakdown_low$Stakes <- rep('Low', nrow(tactics_breakdown_low))
tactics_breakdown <- rbind(tactics_breakdown_high, tactics_breakdown_low)
tactics_breakdown <- tactics_breakdown %>% 
  group_by(Tactic, Stakes) %>% mutate(Tot = sum(Total)) %>% ungroup() %>% 
  mutate(Pct = Total / Tot)
saveRDS(tactics_breakdown, 'data/tactics_breakdown.RDS')

tactics_prof <- tactics %>% 
  inner_join(by_player %>% filter(Num_Hands >= 1000) %>% select(Player, Profitable))

tactics_breakdown_prof = rbind(
  tactics_prof %>% filter(Limp_Opportunity) %>% group_by(Profitable, Limp) %>% summarize(Amt = mean(Amt), Total = n()) %>% mutate(Tactic = 'Limp') %>% rename(Y_N = Limp),
  tactics_prof %>% filter(CB_Opportunity) %>% group_by(Profitable, CB) %>% summarize(Amt = mean(Amt), Total = n()) %>% mutate(Tactic = 'Continuation Bet') %>% rename(Y_N = CB),
  tactics_prof %>% filter(Donk_Opportunity) %>% group_by(Profitable,Donk) %>% summarize(Amt = mean(Amt), Total = n()) %>% mutate(Tactic = 'Donk Bet') %>% rename(Y_N = Donk),
  tactics_prof %>% filter(CR_Opportunity) %>% group_by(Profitable,CR) %>% summarize(Amt = mean(Amt), Total = n()) %>% mutate(Tactic = 'Check-Raise') %>% rename(Y_N = CR)
)

tactics_breakdown_prof <- tactics_breakdown_prof %>% 
  group_by(Tactic, Profitable) %>% mutate(Tot = sum(Total)) %>% ungroup() %>% 
  mutate(Pct = Total / Tot)
tactics_breakdown_prof$Stakes <- rep('Low', nrow(tactics_breakdown_prof))
tactics_breakdown_prof <- tactics_breakdown_prof %>% mutate(Amt = Amt/.02)

tb_high <- tactics_breakdown_prof
tactics_profitability <- rbind(tb_high, tactics_breakdown_prof)
saveRDS(tactics_profitability, 'data/tactics_profitability.RDS')

donk <- tactics %>% filter(Donk_Opportunity) %>% group_by(Player) %>% summarize(Donk = mean(Donk))
cb <- tactics %>% filter(CB_Opportunity) %>% group_by(Player) %>% summarize(CB = mean(CB))
limp <- tactics %>% filter(Limp_Opportunity) %>% group_by(Player) %>% summarize(Limp = mean(Limp))
cr <- tactics %>% filter(CR_Opportunity) %>% group_by(Player) %>% summarize(CR = mean(CR))

by_player <- by_player %>% left_join(donk %>% select(Player, Donk)) %>% 
  left_join(cb %>% select(Player, CB)) %>% left_join(limp %>% select(Player, Limp)) %>% 
  left_join(cr %>% select(Player, CR))

by_player <- by_player %>% select(-c(Total_PL, Call, Check, Fold, Raise, Three_Bet_Plus, Walk))

actions <- read_csv('data/actions0102.csv')
actions <- actions %>% select(-c(X1, `Unnamed: 0`))
actions <- actions %>% select(-c(X1))
actions <- actions %>% group_by(Player) %>% summarize(num_raises = sum(num_raises),
                                                num_calls = sum(num_calls),
                                                num_folds = sum(num_folds),
                                                num_bets = sum(num_bets),
                                                num_checks = sum(num_checks)) %>% 
  mutate(num_rcf = num_raises + num_calls + num_folds, num_bc = num_bets + num_checks) %>% 
  mutate(Raise_Pct = num_raises/num_rcf,
         Call_Pct = num_calls/num_rcf,
         Fold_Pct = num_folds/num_rcf,
         Bet_Pct = num_bets/num_bc,
         Check_Pct = num_checks/num_bc) %>% ungroup()

by_player <- by_player %>% select(-c(Raise_Ratio, Call_Ratio, Fold_Ratio)) %>% 
  inner_join(actions)

by_player %>% filter(Num_Hands >= 1000) %>% select(PL_Per_Hand, Call_Pct, Raise_Pct, Fold_Pct, Bet_Pct) %>% cor()

saveRDS(by_player, 'data/low_stakes_players_2.RDS')

players_high <- readRDS('data/high_stakes_players_2.RDS')
players_low <- readRDS('data/low_stakes_players_2.RDS')
saveRDS(rbind(players_high, players_low), 'data/players.RDS')

by_player %>% 
  filter(Num_Hands >= 1000) %>% select(-c(Player, Total_PL, Num_Hands)) %>% drop_na() %>% cor()

#saveRDS(by_player, 'data/high_stakes_players.RDS')
#by_player <- by_player %>% mutate(Anon_Player = anonymize(Player, .algo='crc32'))


by_position <- hands %>% group_by(num_players, Position) %>% 
   summarize(Amt = mean(Amt)/.02, VPIP = mean(VPIP), Winner = mean(Winner))
by_position$Stakes <- rep('Low', nrow(by_position))
by_position <- by_position %>% mutate(Position = case_when(
   Position == 'Player_1' ~ 'Button',
   (Position == 'Player_2') & (num_players == 3) ~ 'Big Blind',
   (Position == 'Player_2') ~ 'Cutoff',
   (Position == 'Player_3') & (num_players == 3) ~ 'Small Blind',
   (Position == 'Player_3') & (num_players == 4) ~ 'Big Blind',
   (Position == 'Player_3') ~ 'Hijack',
   (Position == 'Player_4') & (num_players == 4) ~ 'Small Blind',
   (Position == 'Player_4') & (num_players == 5) ~ 'Big Blind',
   (Position == 'Player_4') ~ 'Under the Gun',
   (Position == 'Player_5') & (num_players == 5) ~ 'Small Blind',
   (Position == 'Player_5') ~ 'Big Blind',
   TRUE ~ 'Small Blind'
 ))

by_position$Position = factor(by_position$Position, levels = c("Small Blind", "Big Blind",
                              "Under the Gun", "Hijack", "Cutoff", "Button"))
saveRDS(by_position, 'data/by_position_low.RDS')

frequent_players <- (by_player %>% filter(Num_Hands >= 1000) %>% select(Player) %>% unique())[['Player']]

player_position <- hands %>% 
  filter(Player %in% frequent_players) %>%
  mutate(Position = case_when(
    Position == 'Player_1' ~ 'Button',
    (Position == 'Player_2') & (num_players == 3) ~ 'Big Blind',
    (Position == 'Player_2') ~ 'Cutoff',
    (Position == 'Player_3') & (num_players == 3) ~ 'Small Blind',
    (Position == 'Player_3') & (num_players == 4) ~ 'Big Blind',
    (Position == 'Player_3') ~ 'Hijack',
    (Position == 'Player_4') & (num_players == 4) ~ 'Small Blind',
    (Position == 'Player_4') & (num_players == 5) ~ 'Big Blind',
    (Position == 'Player_4') ~ 'Under the Gun',
    (Position == 'Player_5') & (num_players == 5) ~ 'Small Blind',
    (Position == 'Player_5') ~ 'Big Blind',
    TRUE ~ 'Small Blind'
  )) %>% 
  group_by(Player, Position, num_players) %>% 
  summarize(VPIP = mean(VPIP), Amt = mean(Amt)/.02, Winner = mean(Winner))
  
player_position$Position <- factor(player_position$Position, levels = c('Small Blind', 'Big Blind',
                                                            'Under the Gun', 'Hijack',
                                                            'Cutoff', 'Button'))

player_position <- player_position %>% 
  inner_join(by_player %>% select(Player, Profitable))
player_position$Stakes <- rep('Low', nrow(player_position))

saveRDS(player_position, 'data/player_position_low.RDS')

ggplot(player_position,aes_string(x="Position",y="VPIP",fill="Profitable"))+
  geom_bar(stat = "summary", fun.y = "mean", position="dodge")






ggplot(preflop %>% filter(num_players == 6) %>% group_by(Position, Preflop_Action) %>% 
         summarize(Count = n())
       ,aes_string(x="Position",y="Count",fill="Preflop_Action"))+
  geom_bar(stat = "identity", position="fill")



showdown_high <- read_csv('data/showdown12.csv')
showdown_high <- showdown_high %>% select(-X1)

showdown_high$Winning_Hand <- factor(showdown_high$Winning_Hand, 
                                levels = c('High Card', 'One Pair', 'Two Pairs',
                                           'Three of a Kind', 'Straight', 'Flush',
                                           'Full House', 'Four of a Kind', 'Straight Flush'))
showdown_high$Stakes <- rep('High', nrow(showdown_high))
showdown_high <- showdown_high %>% group_by(Winning_Hand, Stakes) %>% 
  summarize(Pct = n()/nrow(showdown_high), Pot_Size = mean(Amt) / 2)


showdown_low <- read_csv('data/showdown0102.csv')
showdown_low <- showdown_low %>% select(-X1)

showdown_low$Winning_Hand <- factor(showdown_low$Winning_Hand, 
                                     levels = c('High Card', 'One Pair', 'Two Pairs',
                                                'Three of a Kind', 'Straight', 'Flush',
                                                'Full House', 'Four of a Kind', 'Straight Flush'))
showdown_low$Stakes <- rep('Low', nrow(showdown_low))
showdown_low <- showdown_low %>% group_by(Winning_Hand, Stakes) %>% 
  summarize(Pct = n()/nrow(showdown_low), Pot_Size = mean(Amt) / .02)

showdown <- rbind(showdown_high, showdown_low)
saveRDS(showdown, 'data/showdown.RDS')



showdown_winners <- read_csv('data/showdown_winners_12.csv') %>% select(-X1)
showdown_winners$Winning_Hand <- factor(showdown_winners$Winning_Hand, levels = 
                                          c('High Card', 'One Pair', 'Two Pairs',
                                            'Three of a Kind', 'Straight', 'Flush',
                                            'Full House', 'Four of a Kind', 'Straight Flush'))
showdown_winners %>% rename(Player = Winners) %>% 
  inner_join(by_player %>% filter(Num_Hands >= 1000) %>% select(Player, Profitable)) %>% 
  group_by(Profitable, Winning_Hand) %>% 
  summarize(Total = n(), Avg_Pot = mean(Amt)) %>% 
  ggplot(aes(x = Winning_Hand, y = Avg_Pot, fill = Profitable)) +
  geom_bar(stat = 'identity', position = 'dodge')

by_street <- read_csv('data/by_street12.csv')
by_street <- by_street %>% select(-X1)
by_street$last_street <- factor(by_street$last_street,
                                levels = c('Preflop', 'Flop', 'Turn', 'River', 'Showdown'))
by_street$Stakes <- rep('High', nrow(by_street))
saveRDS(by_street, 'data/by_street_high.RDS')

ggplot(by_street %>% group_by(last_street) %>% summarize(Total = n()),
       aes(x = last_street, y = Total)) + geom_bar(stat = 'identity')








preflop_profit <- preflop %>% inner_join(by_player %>% select(Player, Profitable))
preflop_profit <- preflop_profit %>% group_by(Profitable, Position) %>% 
  mutate(countT = n()) %>% 
  group_by(Profitable, Position, Action) %>% 
  mutate(Pct = n() / countT) %>% 
  group_by(Profitable, Position, Action) %>% summarize(Pct = mean(Pct))

preflop_profit$Action <- factor(preflop_profit$Action, levels = c('Walk', 'Check',
                                                                    'Fold', 'Call',
                                                                    'Raise', 'Three_Bet_Plus'))


vpip_hands <- hands %>% 
  filter(VPIP) %>% 
  group_by(Player) %>% 
  summarize(Total_Hands = n(), Total_Wins = sum(Winner)) %>% 
  mutate(Win_Pct = Total_Wins / Total_Hands) %>% 
  ungroup()

by_player <- by_player %>% 
  left_join(vpip_hands %>% select(Player, Win_Pct))

vpip_showdowns <- hands %>% filter(VPIP & In_Showdown)
vpip_showdowns <- vpip_showdowns %>% 
  group_by(Player) %>% summarize(VPIP_Showdown = mean(Winner))

by_player <- by_player %>% left_join(vpip_showdowns)

by_player %>% filter(Num_Hands >= 1000) %>% 
  ggplot(aes(x = VPIP_Win_Pct, y= PL_Per_Hand)) + geom_point()






lm.model <- lm(data = pf %>% filter(Num_Hands >= 1000) %>% drop_na(), PL_Per_Hand ~ poly(CB_Ratio,2))
summary(lm.model)


library(rsample)
library(randomForest)
library(caret)
library(miscTools)

df <- pf %>%
  filter(Num_Hands >= 500) %>% 
  select(PL_Per_Hand, Limp_Ratio, VPIP_Ratio, CB_Ratio, Raising_Ratio, Calling_Ratio, Folding_Ratio,
         Winning_Ratio, All_In_Ratio, Call, Check, Fold, Raise, Three_Bet_Plus, Preflop_Aggression, Win_Pct) %>% 
  drop_na()

df_splits <- initial_split(df, prop = 0.80)
train <- training(df_splits)
test <- testing(df_splits)

rf_fit <- train(PL_Per_Hand~.,data=train, method = 'ranger', importance = 'impurity')
rf_fit <- train(PL_Per_Hand~.,data=train, method = 'bstTree')
print(rf_fit)

test$predictions <- predict(rf_fit, test)
mae <- mean(abs(test$PL_Per_Hand - test$predictions))
mae
a <-ggplot(aes(PL_Per_Hand, predictions), data = test) +
  geom_point() +
  geom_smooth() +
  geom_line(aes(PL_Per_Hand, PL_Per_Hand), color = 'red') +
  labs(x = "True Opioid Death Rate", y = "Predicted Opioid Death Rate", 
       title = "Model Performance on Test Set", subtitle = "Random Forest Model") +
  theme(plot.title = element_text(size=18), axis.text.x=element_text(size=16), axis.title = element_text(size = 16), axis.text.y=element_text(size = 16), plot.subtitle = element_text(size = 14))# +
  annotate("text", x = 25, y = 2, label = paste("Mean Absolute Error: ", round(mae, 3)), size = 8)
a

Importance<-as.vector(varImp(rf_fit)$importance)
Variable<-(as.vector(rownames(varImp(rf_fit)$importance)))
DF<-cbind(Variable,Importance)
DF<-as.data.frame(DF)

ggplot(DF %>% top_n(10, Overall), aes(x=reorder(Variable,Overall), y=Overall,fill=Overall))+ 
  geom_bar(stat="identity", position="dodge") + coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  labs(subtitle = 'Random Forest Model') +
  ggtitle("Information Value Summary")+
  guides(fill=F) +
  scale_fill_gradient(low="blue", high="blue") 



ggplot(by_player %>% filter(Num_Hands >= 1000), aes(x = CR, y = PL_Per_Hand)) +
  geom_point()

by_player %>% filter(Num_Hands >= 1000) %>% 
  select(PL_Per_Hand, Limp, CB, Donk, CR) %>% 
  drop_na()%>% cor()

# num_streets by profitability
street_profitable <- hands %>% select(Player, num_streets, In_Showdown, Winner) %>% 
  mutate(num_streets = case_when(
    In_Showdown ~ 5,
    TRUE ~ as.numeric(num_streets)
  )) %>% inner_join(by_player %>% filter(Num_Hands >= 1000) %>% select(Player, Profitable)) %>% 
  group_by(Profitable, num_streets, Winner) %>% summarize(Total = n())

street_profitable <- street_profitable %>% 
  inner_join(street_profitable %>% group_by(Profitable) %>% 
               summarize(Group_Total = sum(Total)))

street_profitable <- street_profitable %>% mutate(Total = Total/Group_Total)

street_profitable <- street_profitable %>% 
  group_by(Profitable) %>% mutate(Cumulative_Pct = 1 - cumsum(Total))

street_profitable <- street_profitable %>% 
  inner_join(street_profitable %>% group_by(Profitable, num_streets) %>% 
  summarize(Street_Total = sum(Total)))

street_profitable <- street_profitable %>% mutate(Street_Pct = Total/Street_Total)

saveRDS(street_profitable, 'data/street_by_profitable_high.RDS')

hands_by_street  <- hands %>% select(num_streets, In_Showdown, Winner) %>% 
  mutate(num_streets = case_when(
    In_Showdown ~ 5,
    TRUE ~ as.numeric(num_streets)
  )) %>% 
  group_by(num_streets, Winner) %>% summarize(Total = n()) %>% ungroup()

hands_by_street <- hands_by_street %>% mutate(Street_Pct = Total/sum(Total))

hands_by_street <- hands_by_street %>% mutate(Cumulative_Percentage = 1 - cumsum(Street_Pct))


ggplot(by_player %>% filter(Num_Hands >= 100), aes(x=PL_Per_Hand)) + stat_ecdf() + geom_vline(xintercept = 0)
by_player %>% filter(Num_Hands >= 100, PL_Per_Hand >= 0) %>% nrow()
by_player %>% filter(Num_Hands >= 100) %>% nrow()
ggplot(by_player %>% filter(Num_Hands >= 100), aes(x=Stakes,y = PL_Per_Hand)) + geom_boxplot() + 
  geom_jitter(alpha = 0.3) + geom_hline(yintercept = 0)
data = players %>% filter(Num_Hands >= 1000) %>% mutate(Stakes = as.factor(Stakes))
data %>%
  plot_ly() %>%
  add_trace(x = ~as.numeric(Stakes) ,y = ~PL_Per_Hand, type = "box", hoverinfo = 'name+y',boxpoints = FALSE) %>%
  add_markers(x = ~jitter(as.numeric(Stakes), amount = 1/6), y = ~PL_Per_Hand,
              hoverinfo = "text", marker = list(color = 'rgb(0,0,0)', opacity = 0.5), alpha = 0.6,
              text = ~paste0("Number of Hands Played: ", Num_Hands),
              showlegend = FALSE) 
ggplot(data, aes(x=PL_Per_Hand, group = Stakes, color = Stakes)) + 
  stat_ecdf() + geom_vline(xintercept = 0)
(data %>% filter(Stakes == 'High', PL_Per_Hand > 0) %>% nrow()) / (data %>% filter(Stakes == 'High') %>% nrow())

profit <- read_csv('data/profit0102.csv')
profit <- profit %>% select(-X1)
profit <- profit %>% group_by(Player) %>% summarize(Num_Hands = n(), Total_PL = sum(Amt)) %>%
  mutate(PL_Per_Hand = Total_PL/Num_Hands / .02) %>% ungroup()

by_player <- by_player %>%  select(-PL_Per_Hand)%>% 
  inner_join(profit %>% select(Player, PL_Per_Hand))

by_player <- by_player %>% filter(Num_Hands >= 1000)

saveRDS(by_player, 'data/low_stakes_players_3.RDS')

testing <- readRDS('data/low_stakes_players_3.RDS')
saveRDS(testing %>% filter(Num_Hands >= 1000), 'data/low_stakes_players_3.RDS')

ls <- readRDS('data/low_stakes_players_3.RDS')
hs <- readRDS('data/high_stakes_players_3.RDS')
saveRDS(rbind(ls, hs), 'data/players.RDS')

test <- readRDS('data/players.RDS')

