shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Online Poker'),
    
    dashboardSidebar(
      sidebarMenu(id = 'tabs',
                  menuItem("Introduction", tabName = 'intro', icon = icon('chart-line')),
                  menuItem("Profitability", tabName = 'profit', icon = icon('chart-line')),
                  menuItem("Player Tendencies", tabName = "tendencies", icon = icon("chart-line")),
                  menuItem("Position Analysis", tabName = "position", icon = icon("chart-line")),
                  menuItem("Streets", tabName = 'streets', icon = icon('chart-line')),
                  menuItem("Showdown Analysis", tabName = 'winning_hand', icon = icon("chart-line"))
                  #,
                  #menuItem("Tactics", tabName = 'tactics', icon = icon('chart-line'))
      ),
      
      conditionalPanel(condition = "input.tabs == 'profit'",
                       sliderInput('min_hands', "Select minimum number of hands",
                                   min = 0, max = 5000, value = 100, step = 100)),
      conditionalPanel(condition = "input.tabs == 'tendencies'",
                       selectInput("select", h3("Select Metric"),
                                   choices = list("Calling Ratio" = "Call_Pct",
                                                  "Folding Ratio" = "Fold_Pct",
                                                  "Raising Ratio" = "Raise_Pct",
                                                  "Checking Ratio" = "Check_Pct",
                                                  "Betting Ratio" = "Bet_Pct",
                                                  "VPIP Ratio" = "VPIP_Ratio",
                                                  "Winning Ratio" = "Winning_Ratio",
                                                  "VPIP Winning Ratio" = "VPIP_Win_Ratio",
                                                  "All-In Ratio" = "All_In_Ratio",
                                                  "Preflop Aggression" = "Preflop_Aggression",
                                                  "Three Bet Plus Ratio" = "Three_Bet_Ratio",
                                                  "Preflop Raise Ratio" = "PF_Raise_Ratio",
                                                  "Preflop Call Ratio" = "PF_Call_Ratio",
                                                  "Limp Ratio" = "Limp",
                                                  "Continuation Bet Ratio" = "CB",
                                                  "Donk Bet Ratio" = "Donk",
                                                  "Check-Raise Ratio" = "CR",
                                                  "Showdown Ratio" = "Showdown_Ratio",
                                                  "Showdown Winning Ratio" = "Showdown_Winning_Ratio"
                                   ),
                                   selected = "Calling_Ratio"),
                       radioButtons("stake", "Show Correlation for Which Stakes Level:",
                                    c("Low", "High", "Both"),
                                    selected = c("Both"))
      ),
      conditionalPanel(condition = "input.tabs == 'position'",
                       selectInput("stake_tab", "Stakes level to show:",
                                   c("Low", "High", "Both"),
                                   selected = c("Both")),
                       selectInput('position_selection', h3('Choose Metric:'),
                                    choices = c("Average Profit/Loss" = "Amt",
                                                "VPIP Percentage" = "VPIP",
                                                'Winning Percentage' = 'Winner')),
                       selectInput("players", h3("Select Number of Players:"),
                                   choices = c(3,4,5,6),
                                   selected = 6)
      ),
      
      conditionalPanel(condition = "input.tabs == 'streets'",
                       radioButtons("prof_tab", "Make a selection:",
                                    c("Profitable Players", "Unprofitable Players", "All Players"),
                                    selected = c("All Players")),
                       radioButtons("street_stakes", "Choose Stakes Level:",
                                    c("Low Stakes", "High Stakes"),
                                    selected = "Low Stakes")
      ),    
      
      conditionalPanel(condition = "input.tabs == 'winning_hand'",
                       radioButtons("sd_stakes", "Choose Stakes Level:",
                                    c("Low", "High"),
                                    selected = "Low"),
                       radioButtons("sd_type", "Make a Selection:",
                                    c("Pairing Frequency", "Average Pot Size"),
                                    selected = c("Pairing Frequency")),
                       conditionalPanel(condition = "input.sd_type == 'Pairing Frequency'",
                                        checkboxInput('sd_pct', "Show as Percentages", value = FALSE))
                       
      )
                       
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'intro',
                tags$div(    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Roboto|Cabin:400,700');
      
      h2 {
        font-family: 'Roboto';
        font-weight: 500;
        line-height: 1.1;
        color: #000000;
      }

    ")),
            tags$h2("An Analysis of the Profitability of Online Poker Strategies")
            #tags$img(src = "Ace.jpg", width = "400px")
            #HTML('<center><img src="Ace.jpg" width = "400px"></center>')
                )
            
        ),
        tabItem(tabName = 'profit',
                fluidRow(
                  box(width = 6,plotlyOutput('profit_box')),
                  box(width = 6,plotOutput('ecdf'))
                ),
                fluidRow(
                  box(uiOutput('prof_low')),
                  box(uiOutput('prof_high'))
                )
                ),
        tabItem(tabName = 'tendencies',
                fluidRow(
                  box(width = 8, plotOutput("by_player")),
                  box(width = 4, plotOutput("profitable_comparison"))
                ),
                fluidRow(
                  box(width = 4,uiOutput('cor_low')),
                  box(width = 8,
                    htmlOutput('reminder_text')
                  )
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
        # tabItem(tabName = 'winning_hand',
        #         plotOutput('hands'),
        #         plotOutput('pot_size')),
        tabItem(tabName = 'streets',
                fluidRow(
                  tags$h1("Street Analysis"),
                  
                  'Hands are divided into 4 streets, or rounds of betting, named the Preflop, Flop, Turn, and River.',
                  HTML('<br>'),
                  'If any players remain in the hand after the River, the Showdown occurs, where players reveal their hands and the best hand wins the pot.'
                ),
                fluidRow(
                  plotlyOutput('street')
                  )
                ),
        tabItem(tabName = 'winning_hand',
                fluidRow(
                  box(plotOutput('testing', width = '120%')),
                  box(title = 'Hand Rankings (Highest to Lowest)',
                      DTOutput('ranking_table'))
                )
        )
      )
    )
  )
)
