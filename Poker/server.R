shinyServer(function(input, output) {
   
  labels <- list( "Calling_Ratio" = "Calling Ratio",
                      "Folding_Ratio" = "Folding Ratio",
                      "Raising_Ratio" = "Raising Ratio",
                      "VPIP_Ratio" = "VPIP Ratio",
                      "Winning_Ratio" = "Winning Ratio",
                      "All_In_Ratio" = "All-In Ratio",
                      "Preflop_Aggression" =  "Preflop Aggression",
                      "Limp_Ratio" = "Limp Ratio",
                      "Win_Pct" = "Winning VPIP Ratio",
                      "CB_Ratio" = "Continuation Bet Ratio",
                      "Showdown_Ratio" = "Showdown Ratio",
                      "VPIP_Showdown" = "Showdown Winning Ratio"
  )
  
  output$profitable_comparison <- renderPlot({
    if(input$stake == "Both"){stakes = c("Low", "High")}
    else{stakes = input$stake}
    ggplot(players %>% filter(Num_Hands >= 1000, Stakes %in% stakes),aes_string(x="Stakes",y=input$select,fill="Profitable"))+
      geom_bar(stat = "summary", fun.y = "mean", position="dodge") +
      scale_x_discrete(limits = stakes) +
      scale_fill_discrete(name="",
                          labels=c("Unprofitable", "Profitable"))+
      xlab("Stakes Level")+ylab(paste('Mean', labels[input$select])) + 
      ggtitle('Comparsion of Profitable to Unprofitable Players') +
      theme(plot.title = element_text(size = 16), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.key.size = unit(2,"line"),
            legend.position = 'right')
  })
  
  output$by_player <- renderPlot({
    if(input$stake == "Both"){stakes = c("Low", "High")}
    else{stakes = input$stake}    
    ggplot(players %>% filter(Num_Hands >= 1000, Stakes %in% stakes),aes_string(x=input$select,y='PL_Per_Hand',color='Stakes'))+
      geom_point(alpha = 0.6, size = 3) +
      xlab(labels[input$select]) + 
      ylab("Normalized Profit/Loss Per Hand (Big Blinds)") +
      ggtitle(paste('Profit/Loss Per Hand vs', labels[input$select])) +
      theme(plot.title = element_text(size = 16), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.key.size = unit(1.5,"line"))
  })
  
  output$position <- renderPlot({
    positions_labels_axis = c('Amt' = "Average Profit/Loss Per Hand (Big Blinds)",
                              'VPIP' = 'VPIP Percentage',
                              'Winner' = 'Winning Percentage')
    positions_labels_title = c('Amt' = 'Profit/Loss Per Hand',
                               'VPIP' = 'VPIP Percentage',
                               'Winner' = 'Winning Percentage')
    
    if(input$stake_tab == "Both"){stakes = c("Low", "High")}
    else{stakes = input$stake_tab}
    
    ggplot(positions %>% filter(num_players == input$players, Stakes %in% stakes),
           aes_string(x = 'Position', y = input$position_selection, fill = 'Stakes')) +
      geom_bar(stat='identity', position = 'dodge') +
      xlab("Player Position\n (Players Act in Order From Left to Right)") + 
      ylab(positions_labels_axis[input$position_selection]) +
      ggtitle(paste(paste(positions_labels_title[input$position_selection], 'vs Player Position'))) +
      theme(plot.title = element_text(size = 16), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.key.size = unit(1.5,"line"))
  })
  
  output$position_profit <- renderPlot({
    positions_labels_axis = c('Amt' = "Average Profit/Loss Per Hand (Big Blinds)",
                              'VPIP' = 'VPIP Percentage',
                              'Winner' = 'Winning Percentage')
    positions_labels_title = c('Amt' = 'Profit/Loss Per Hand',
                               'VPIP' = 'VPIP Percentage',
                               'Winner' = 'Winning Percentage')
    
    if(input$stake_tab == "Both"){stakes = c("Low", "High")}
    else{stakes = input$stake_tab}
    
    ggplot(player_positions %>% filter(num_players == input$players, Stakes %in% stakes),
           aes_string(x = 'Position', y = input$position_selection, fill = 'Profitable')) +
      geom_bar(stat = "summary", fun.y = "mean", position="dodge") +
      xlab("Player Position\n (Players Act in Order From Left to Right)") + 
      ylab(positions_labels_axis[input$position_selection]) +
      ggtitle(paste(paste(positions_labels_title[input$position_selection], 'vs Player Position'))) +
      scale_fill_discrete(name="",
                          labels=c("Unprofitable", "Profitable"))+
      theme(plot.title = element_text(size = 16), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.key.size = unit(1.5,"line"))
  })
  
  output$hands <- renderPlot({
    winning_hands$Stakes <- factor(winning_hands$Stakes, levels = c('Low', 'High'))
    ggplot(winning_hands,
           aes(x = Winning_Hand, y = Pct, fill = Stakes)) + 
      geom_bar(stat = 'identity', position = 'dodge') +
      xlab("Hand Type") + 
      ylab("Percentage") +
      ggtitle('Winning Hands by Type') +
      theme(plot.title = element_text(size = 16), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.key.size = unit(1.5,"line"))
  })
  
  output$pot_size <- renderPlot({
    winning_hands$Stakes <- factor(winning_hands$Stakes, levels = c('Low', 'High'))
    ggplot(winning_hands,
           aes(x = Winning_Hand, y = Pot_Size, fill = Stakes)) + 
      geom_bar(stat = 'identity', position = 'dodge') +
      xlab("Hand Type") + 
      ylab("Average Pot Size (Big Blinds)") +
      ggtitle('Pot Size vs Hand Type') +
      theme(plot.title = element_text(size = 16), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.key.size = unit(1.5,"line"))
  })

})
