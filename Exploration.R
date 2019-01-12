library(tidyverse)
library(anonymizer)
library(digest)

hands <- read_csv('data/processed_0102.csv') %>% select(-X1)

hands <- hands %>% 
  mutate_at(c('Limp', 'Call', 'Raise', 'Winner', 'VPIP', 
              'Blind', 'In_Showdown', 'All_In', 'CB_Opportunity', 'CB', 'Winner'), 
            as.logical)

by_player <- hands %>% group_by(Player) %>% 
  summarize(Total_PL = sum(Amt),
            Num_Hands = n(), 
            Limp_Ratio = mean(Limp),
            Showdown_Ratio = mean(In_Showdown),
            All_In_Ratio = mean(All_In),
            VPIP_Ratio = mean(VPIP),
            Winning_Ratio = mean(Winner),
            Num_CB_Opp = sum(CB_Opportunity), 
            Num_CB = sum(CB), 
            Total_Actions = sum(num_actions), 
            Total_Raises = sum(num_raises),
            Total_Folds = sum(num_folds), 
            Total_Calls = sum(num_calls)
            ) %>% 
  mutate(PL_Per_Hand = Total_PL / Num_Hands / 2, 
         #Limp_Ratio = mean(Limp), 
         #Call_Ratio = Num_Calls / Num_Hands, 
         #Raise_Ratio = Num_Raises / Num_Hands, 
         #VPIP_Ratio = Num_VPIP / Num_Hands, 
         CB_Ratio = Num_CB/Num_CB_Opp, 
         Raising_Ratio = Total_Raises/ Total_Actions, 
         Calling_Ratio = Total_Calls / Total_Actions, 
         Folding_Ratio = Total_Folds/ Total_Actions
         #Winning_Ratio = Total_Wins/Num_Hands, 
         #All_In_Ratio = Total_All_In / Num_Hands
         ) %>% 
  ungroup()

by_player <- by_player %>% mutate(Profitable = PL_Per_Hand > 0)
by_player$Stakes <- rep('Low', nrow(by_player))

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
  group_by(Player, Action) %>% 
  summarize(Count = n()) %>% 
  ungroup

preflop_by_player <- preflop %>% 
  group_by(Player) %>% 
  mutate(countT = n()) %>% 
  group_by(Player,Action) %>% 
  mutate(Pct = n() / countT) %>% 
  group_by(Player, Action) %>% summarize(Pct = mean(Pct))

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


ggplot(showdown,
       aes(x = Winning_Hand, y = Pct, fill = Stakes)) + 
  geom_bar(stat = 'identity', position = 'dodge')


by_street <- read_csv('data/by_street12.csv')
by_street <- by_street %>% select(-X1)
by_street$last_street <- factor(by_street$last_street,
                                levels = c('Preflop', 'Flop', 'Turn', 'River', 'Showdown'))
ggplot(by_street %>% group_by(last_street) %>% summarize(Total = n()),
       aes(x = last_street, y = Total)) + geom_bar(stat = 'identity')


by_player <- inner_join(by_player, preflop_by_player %>% spread(Action, Pct))
by_player <- by_player %>%
  mutate(Preflop_Aggression = (Raise + Three_Bet_Plus)/VPIP_Ratio)


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



saveRDS(by_player, 'data/low_stakes_players.RDS')



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


