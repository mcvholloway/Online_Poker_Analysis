shinyServer(function(input, output) {
   
  labels <- list( "Call_Pct" = "Calling Ratio",
                 "Fold_Pct" = "Folding Ratio",
                 "Raise_Pct" = "Raising Ratio",
                 "Bet_Pct" = "Betting Ratio",
                 "Check_Pct" = "Check Ratio",
                 "VPIP_Ratio" = "VPIP Ratio",
                 "Winning_Ratio" = "Winning Ratio",
                 "VPIP_Win_Ratio" = "VPIP Winning Ratio",
                 "All_In_Ratio" = "All-In Ratio",
                 "Preflop_Aggression" = "Preflop Aggression",
                 "Three_Bet_Ratio" = "Three Bet Plus Ratio",
                 "PF_Raise_Ratio" = "Preflop Raise Ratio",
                 "PF_Call_Ratio" = "Preflop Call Ratio",
                 "Limp" = "Limp Ratio",
                 "CB" = "Continuation Bet Ratio",
                 "Donk" = "Donk Bet Ratio",
                 "CR" = "Check-Raise Ratio",
                 "Showdown_Ratio" = "Showdown Ratio",
                 "Showdown_Winning_Ratio" = "Showdown Winning Ratio" 
  )
  
  output$reminder_text <- renderUI({
    withMathJax(HTML(descriptions[input$select]))
  })
  
  output$profitable_comparison <- renderPlot({
    #if(input$stake == "Both"){stakes = c("Low", "High")}
    #else{stakes = input$stake}
    stakes = c("Low", "High")
    ggplot(players %>% filter(Num_Hands >= 500, Stakes %in% stakes),
           aes_string(x="Stakes",y=input$select,fill="Profitable"))+
      geom_bar(stat = "summary", fun.y = "mean", position="dodge") +
      scale_x_discrete(limits = stakes) +
      #scale_fill_discrete(name="",
      #                    labels=c("Unprofitable", "Profitable"),
      #                    values = c('black', 'orange'))+
      scale_fill_manual("", labels=c("Unprofitable", "Profitable"),
                        values = c("FALSE" = "black", "TRUE" = "orange")) +
      xlab("Stakes Level")+ylab(paste('Mean', labels[input$select])) + 
      ggtitle('Profitable vs. Unprofitable Players') +
      labs(subtitle = 'Minimum of 500 Hands Played') +
      theme(plot.title = element_text(size = 16), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.key.size = unit(2,"line"),
            legend.position = 'bottom')
  })
  
  output$cor_low <- renderInfoBox({
    if(input$stake == "Both"){stakes = c("Low", "High")}
    else{stakes = input$stake}
    data <- players %>% filter(Num_Hands >= 500, Stakes %in% stakes)
    correl <- data %>% select("PL_Per_Hand", input$select) %>% drop_na() %>% cor()
    correl <- correl[1,2]
    valueBox(tags$p(sprintf("%.3f", round(correl,3)), style = "font-size: 90%;"),
             paste0('Correlation Between Profit/Loss and ', labels[input$select]),
             #icon = icon("pills"),
             color = "navy"
    )
  })
  
  
  output$by_player <- renderPlot({
    #if(input$stake == "Both"){stakes = c("Low", "High")}
    #else{stakes = input$stake}    
    stakes = c("Low", "High")
    ggplot(players %>% filter(Num_Hands >= 500, Stakes %in% stakes),
           aes_string(x=input$select,y='PL_Per_Hand',color='Stakes'))+
      geom_point(alpha = 0.6, size = 3) +
      labs(subtitle = 'Minimum of 500 Hands Played') +
      xlab(labels[input$select]) + 
      ylab("Normalized Profit/Loss Per Hand (Big Blinds)") +
      ggtitle(paste('Profit/Loss Per Hand vs', labels[input$select], 'by Player')) +
      theme(plot.title = element_text(size = 16), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.key.size = unit(1.5,"line")) +
      facet_grid(.~Stakes) + colScale+ theme(legend.position="none") +
      geom_smooth(method = "lm")
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
      scale_fill_manual("", labels=c("Unprofitable", "Profitable"),
                        values = c("FALSE" = "black", "TRUE" = "orange")) +
      #scale_fill_discrete(name="",
      #                    labels=c("Unprofitable", "Profitable"))+
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
  
  output$street <- renderPlotly({
    # ggplot(by_street %>% group_by(last_street, Stakes) %>% summarize(Total = n()),
    #        aes(x = last_street, y = Total, fill = Stakes)) + 
    #   geom_bar(stat = 'identity', position = 'dodge')
    
    if (input$prof_tab == 'All Players'){
      if (input$street_stakes == 'Low Stakes'){sankey_value = sankey_values['low_all']}
      else if (input$street_stakes == 'High Stakes'){sankey_value = sankey_values['high_all']}
    }
    else if (input$prof_tab == 'Profitable Players'){
      if (input$street_stakes == 'Low Stakes'){sankey_value = sankey_values['low_prof']}
      else if (input$street_stakes == 'High Stakes'){sankey_value = sankey_values['high_prof']}
    }
    else if (input$prof_tab == 'Unprofitable Players'){
      if (input$street_stakes == 'Low Stakes'){sankey_value = sankey_values['low_unprof']}
      else if (input$street_stakes == 'High Stakes'){sankey_value = sankey_values['high_unprof']}
    }
    
    sankey_value = unlist(unname(sankey_value))
    
    r <- plot_ly(
      type = "sankey",
      orientation = "h",
      valueformat = ".1f",
      valuesuffix = "%",
      height = 500,
      
      node = list(
        label = c("Preflop", "Flop", "Turn", "River", "Showdown","Win", "Lose"),
        color = c("blue", "blue", "blue", "blue", "blue", "blue", "blue"),
        pad = 15,
        thickness = 20,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      
      link = list(
        source = c(0,0,0,1,1,1,2,2,2,3,3,3,4,4),
        target = c(5,6,1,5,6,2,5,6,3,5,6,4,5,6),
        value =  sankey_value
      )
    ) %>% 
      layout(
        title = HTML(paste('<b>Percent of Hands Played to Each Street</b> <br>', input$prof_tab, '-', input$street_stakes)),
        font = list(
          size = 12
        ),
        margin = list(t = 50)
      )
    r
  })
  
  output$testing <- renderPlot({
    sd <- sd %>% filter(Stakes == input$sd_stakes)
    
    hands_matrix <- matrix(as.numeric(fill_matrix(sd, 'Num_Hands')), nrow = 9)
    rownames(hands_matrix) <- c('High Card', 'One Pair', 'Two Pairs',
                                'Three of a Kind', 'Straight', 'Flush',
                                'Full House', 'Four of a Kind', 'Straight Flush')
    colnames(hands_matrix) <- c('High Card', 'One Pair', 'Two Pairs',
                                'Three of a Kind', 'Straight', 'Flush',
                                'Full House', 'Four of a Kind', 'Straight Flush')
    pot_matrix <- matrix(as.numeric(fill_matrix(sd, 'Pot_Size')), nrow = 9)
    rownames(pot_matrix) <- c('High Card', 'One Pair', 'Two Pairs',
                              'Three of a Kind', 'Straight', 'Flush',
                              'Full House', 'Four of a Kind', 'Straight Flush')
    colnames(pot_matrix) <- c('High Card', 'One Pair', 'Two Pairs',
                              'Three of a Kind', 'Straight', 'Flush',
                              'Full House', 'Four of a Kind', 'Straight Flush')
    col1 <- colorRampPalette(c("black","white", "darkorange"))
    
    if (input$sd_type == 'Pairing Frequency'){
      if(input$sd_pct){hands_matrix <- 100*hands_matrix/sum(hands_matrix)}
    corrplot(hands_matrix^.25, type="upper", 
             tl.col="black", tl.srt=45,
             is.corr=FALSE, method = 'color', cl.pos = 'n', mar = c(0,0,1,0),
             col = col1(100))
      corrplot(hands_matrix, type="upper", 
               tl.col="black", tl.srt=45,
               is.corr=FALSE, addCoef.col = "black",
               method = 'number', col = c('#000000'),add = TRUE, bg = 'transparent', 
               cl.pos='n', mar = c(1,1,1,1), na.label = "NA", number.digits = 3)
    mtext('Winning Hand', side = 3, cex = 1.1, line = 0.7, font =2, adj = 0.53)
    mtext('Frequency of Showdown Pairings', side = 3, cex = 1.5, font = 2, 
          adj = 0.55, line = 2, col = 'darkblue')
    usr <- par( "usr" )
    text( usr[ 1 ] + 5, usr[ 4 ] - 7.5, "Losing Hand", 
          col = "black", srt = -45, cex = 1.1, font = 2)
    }
    if (input$sd_type == 'Average Pot Size'){
      if (input$sd_stakes == 'Low'){blinds = 0.02}
      else {blinds = 2}
      corrplot(pot_matrix, type="upper", 
               tl.col="black", tl.srt=45,
               is.corr=FALSE, method = 'color', cl.pos = 'n', 
               mar = c(0,0,1,0), na.label = 'NA', col = col1(100))
      corrplot(pot_matrix/blinds, type="upper", 
               tl.col="black", tl.srt=45,
               is.corr=FALSE, addCoef.col = "black",
               method = 'number', col = c('#000000'),add = TRUE, bg = 'transparent', 
               cl.pos='n', mar = c(1,1,1,1), na.label = 'NA')
      mtext('Winning Hand', side = 3, cex = 1.1, line = 0.7, font =2, adj = 0.53)
      mtext('Average Pot Size (Big Blinds)', side = 3, cex = 1.5, font = 2, 
            adj = 0.55, line = 2, col = 'darkblue')
      usr <- par( "usr" )
      text( usr[ 1 ] + 5, usr[ 4 ] - 7.5, "Losing Hand", 
            col = "black", srt = -45, cex = 1.1, font = 2)
    }
  }, height = 525, width = 525)
  
  
  output$ranking_table <- DT::renderDataTable({
    dat <- data.frame('Name' = c('Straight Flush', 'Four of a Kind', 'Full House',
                                                                  'Flush', 'Straight', 'Three of a Kind',
                                                                  'Two Pair', 'One Pair', 'High Card'),
                                                          'Description' = c('Five cards of sequential rank, all of the same suit',
                                                                            'Four cards of one rank and one card of another rank',
                                                                            'Three cards of one rank and two cards of another rank',
                                                                            'Five cards all of the same suit, not all of sequential rank',
                                                                            'Five cards of sequential rank, not all of the same suit',
                                                                            'Three cards of one rank and two cards of two other ranks',
                                                                            'Two cards of one rank, two cards of another rank and one card of a third rank',
                                                                            'Two cards of one rank and three cards of three other ranks',
                                                                            'A hand that does not fall into any other category'),
                                                       'Example' = c('<img src=\"Qs.png\" width = 40></img><img src=\"Js.png\" width = 40></img><img src=\"Ts.png\" width = 40></img><img src=\"9s.png\" width = 40></img><img src=\"8s.png\" width = 40></img>',
                                                                     '<img src=\"3s.png\" width = 40></img><img src=\"3h.png\" width = 40></img><img src=\"3c.png\" width = 40></img><img src=\"3d.png\" width = 40></img><img src=\"6c.png\" width = 40></img>',
'<img src=\"6s.png\" width = 40></img><img src=\"6h.png\" width = 40></img><img src=\"6c.png\" width = 40></img><img src=\"Kh.png\" width = 40></img><img src=\"Kc.png\" width = 40></img>',
'<img src=\"9d.png\" width = 40></img><img src=\"Jd.png\" width = 40></img><img src=\"Ad.png\" width = 40></img><img src=\"4d.png\" width = 40></img><img src=\"2d.png\" width = 40></img>',
'<img src=\"8c.png\" width = 40></img><img src=\"7h.png\" width = 40></img><img src=\"6c.png\" width = 40></img><img src=\"5h.png\" width = 40></img><img src=\"4s.png\" width = 40></img>',
'<img src=\"Th.png\" width = 40></img><img src=\"Tc.png\" width = 40></img><img src=\"Td.png\" width = 40></img><img src=\"3h.png\" width = 40></img><img src=\"7h.png\" width = 40></img>',
'<img src=\"5d.png\" width = 40></img><img src=\"5c.png\" width = 40></img><img src=\"8c.png\" width = 40></img><img src=\"8s.png\" width = 40></img><img src=\"Qc.png\" width = 40></img>',
'<img src=\"2h.png\" width = 40></img><img src=\"2c.png\" width = 40></img><img src=\"7h.png\" width = 40></img><img src=\"Tc.png\" width = 40></img><img src=\"6s.png\" width = 40></img>',
'<img src=\"Jd.png\" width = 40></img><img src=\"8s.png\" width = 40></img><img src=\"3c.png\" width = 40></img><img src=\"7d.png\" width = 40></img><img src=\"Ac.png\" width = 40></img>'
)
                                                       )
    DT::datatable(dat, rownames = FALSE, escape = FALSE,options = list(dom = 't', bSort=FALSE)) %>%  formatStyle("Example","white-space"="nowrap")
  })
  
  output$profit_box <- renderPlotly({
    data = players %>% filter(Num_Hands >= input$min_hands) %>% 
      mutate(Stakes = as.factor(Stakes)) %>% 
      mutate(Total_Profit = case_when(
        Stakes == 'High' ~ PL_Per_Hand * 2 * Num_Hands,
        Stakes == 'Low' ~ PL_Per_Hand * .02 * Num_Hands
      )) 
    data %>%
      plot_ly() %>%
      add_trace(x = ~as.numeric(Stakes) ,y = ~PL_Per_Hand, type = "box", hoverinfo = 'name+y',boxpoints = FALSE) %>%
      add_markers(x = ~jitter(as.numeric(Stakes), amount = 1/6), y = ~PL_Per_Hand,
                  hoverinfo = "text", marker = list(color = 'rgb(128,0,0)', opacity = 0.4), alpha = 0.6,
                  text = ~paste0("Number of Hands Played: ", Num_Hands,
                                 "<br>Total Profit/Loss:  ", round(Total_Profit, 2)
                                 ),
                  showlegend = FALSE) %>% 
      layout(
        showlegend = FALSE,
        xaxis = list(title = "Stakes Level", tickmode = 'array', nticks = 2, tickvals = c(1,2),
                     ticktext = c('Low', 'High'), tickfont = list(size = 14)),
        yaxis = list(title = 'Profit/Loss Per Hand (Big Blinds)', titlefont = list(size = 14), tickfont = list(size = 18)),
        titlefont = list(size = 16),
        title = 'Profit/Loss Per Hand By Player',
        margin = list(t = 50)
      )
  })
  
  output$ecdf <- renderPlot({
    data = players %>% filter(Num_Hands >= input$min_hands) 
    high_pct <- 1-(data %>% filter(Stakes == 'High', PL_Per_Hand >= 0) %>% nrow()) / (data %>% filter(Stakes == 'High') %>% nrow())
    low_pct <- 1-(data %>% filter(Stakes == 'Low', PL_Per_Hand >= 0) %>% nrow()) / (data %>% filter(Stakes == 'Low') %>% nrow())
    ggplot(data, aes(x=PL_Per_Hand, group = Stakes, color = Stakes)) + 
      stat_ecdf(size = 1.5) + geom_vline(xintercept = 0) +
      xlab("Profit/Loss Per Hand (Big Blinds)") + 
      ylab("Cumulative Percent of Players") +
      ggtitle('Profit/Loss Cumulative Distribution') +
      theme(plot.title = element_text(size = 16), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.key.size = unit(1.5,"line")) +
      geom_hline(yintercept = high_pct, col = myColors[2]) + 
      geom_hline(yintercept = low_pct, col = myColors[1]) + colScale
  })
  
  output$prof_low <- renderInfoBox({
    data = players %>% filter(Num_Hands >= input$min_hands)
    low_pct <- (data %>% filter(Stakes == 'Low', PL_Per_Hand >= 0) %>% nrow()) / (data %>% filter(Stakes == 'Low') %>% nrow())
    valueBox(tags$p(paste0(as.character(round(100*low_pct,1)),'%'), style = "font-size: 90%;"),
             paste0('of low-stakes players were profitable
                    (Minimum ', input$min_hands, ' hands)'),
             #icon = icon("pills"),
             color = "navy"
    )
  })
  
  output$prof_high <- renderInfoBox({
    data = players %>% filter(Num_Hands >= input$min_hands)
    high_pct <- (data %>% filter(Stakes == 'High', PL_Per_Hand >= 0) %>% nrow()) / (data %>% filter(Stakes == 'High') %>% nrow())
    valueBox(tags$p(paste0(as.character(round(100*high_pct,1)), '%'), style = "font-size: 90%;"),
             paste0('of high-stakes players were profitable
                    (Minimum ', input$min_hands, ' hands)'),
             #icon = icon("pills"),
             color = "navy"
    )
  })
  
})
