shinyUI(
  dashboardPage(
  
    dashboardHeader(title = 'Online Poker'),
    
    dashboardSidebar(
      sidebarMenu(id = 'tabs',
        menuItem("Player Tendencies", tabName = "tendencies", icon = icon("chart-line")),
        menuItem("Position Analysis", tabName = "position", icon = icon("chart-line")),
        menuItem("Winning Hand", tabName = 'winning_hand', icon = icon("chart-line"))
      ),
      
      conditionalPanel(condition = "input.tabs == 'tendencies'",
                       selectInput("select", h3("Select Metric"),
                                   choices = list("Calling Ratio" = "Calling_Ratio",
                                                  "Folding Ratio" = "Folding_Ratio",
                                                  "Raising Ratio" = "Raising_Ratio",
                                                  "VPIP Ratio" = "VPIP_Ratio",
                                                  "Winning Ratio" = "Winning_Ratio",
                                                  "All-In Ratio" = "All_In_Ratio",
                                                  "Preflop Aggression" = "Preflop_Aggression",
                                                  "Limp Ratio" = "Limp_Ratio",
                                                  "Winning VPIP Ratio" = "Win_Pct",
                                                  "Continuation Bet Ratio" = "CB_Ratio",
                                                  "Showdown Ratio" = "Showdown_Ratio",
                                                  "Showdown Winning Ratio" = "VPIP_Showdown"
                                   ),
                                   selected = "Calling_Ratio"),
                       radioButtons("stake", "Stakes level to show:",
                                          c("Low", "High", "Both"),
                                            selected = c("Both"))
      ),
      conditionalPanel(condition = "input.tabs == 'position'",
                       radioButtons("players", h3("Select Number of Players:"),
                                   choices = c(3,4,5,6),
                                   selected = 6),
                       radioButtons('position_selection', h3('Choose Metric:'),
                                    choices = c("Average Profit/Loss" = "Amt",
                                                "VPIP Percentage" = "VPIP",
                                                'Winning Percentage' = 'Winner')),
                       radioButtons("stake_tab", "Stakes level to show:",
                                    c("Low", "High", "Both"),
                                    selected = c("Both"))
      )
      
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = 'tendencies',
                fluidRow(
        box(height = '300px', plotOutput("by_player")),#),
      #fluidRow(
        box(height = '300px', plotOutput("profitable_comparison"))
    )
  ),
      tabItem(tabName = 'position',
              tabBox(
                width = 12,
                title = "",
                id = "tabset1", height = "250px",
                tabPanel("All Players", plotOutput('position')),
                tabPanel("Profitable Vs. Unprofitable Players", 
                           plotOutput('position_profit')))
              ),
  tabItem(tabName = 'winning_hand',
          plotOutput('hands'),
          plotOutput('pot_size'))
              )
)
)
)