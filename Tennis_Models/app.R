#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plyr)
library(plotrix)
library(ggplot2)
library(plotly)
library(Lahman)
library(RColorBrewer)
library(gganimate)
library(tidyverse)

# load in data (2015-2022) cleaned data
atp_matches_cleaned <- read.csv("C:/Users/timst/Documents/SMU/Fall_2022/STAT6341/Project/Tennis_Data_Reduced/reduced_clean_matches.csv",
                                header =T)

ui <- dashboardPage(skin='green',
                    dashboardHeader(title = 'Tennis Analytics'),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Overview", tabName = 'overview', icon = icon('star')),
                        menuItem("Viz 1", tabName = 'RF', icon = icon('tree')),
                        menuItem("Viz 2", tabName = 'demo', icon = icon('brain')),
                        menuItem("Viz 3", tabName = 'raw_data', icon = icon('cogs')),
                        menuItem("Viz 4", tabName = 'choose_plot', icon = icon('lightbulb-o'))
                      )),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName='overview',
                                h1('Overview'),
                                fluidRow( 
                                  #valueBoxOutput("champion"), 
                                 # valueBoxOutput("al_mvp"), 
                                 # valueBoxOutput("nl_mvp") 
                                ),
                                fluidRow(
                                  box(verbatimTextOutput("welcome"), width = 12)
                                ),
                                fluidRow(
                                  box(selectInput("year",
                                                  label = "Select a year",
                                                  choices = c(2015:2022),
                                                  selected = 2015), width = 4)
                                ),
                                fluidRow(
                                  box(plotlyOutput("plot_wins"), width = 12)
                                )
                        ),
                        tabItem(tabName='demo',
                                h1('First Plots'),
                                fluidRow(
                                  box(plotlyOutput("plot1"), title = 'Prob of win on 1st Serve vs Prob of Win on 2nd Serve', width = 4),
                                  box(plotlyOutput("plot2"), title = 'Median Service Points', width = 4),
                                  box(plotlyOutput("plot3"), title = 'Average Aces', width = 4)
                                )
                        ),
                        tabItem(tabName='raw_data',
                                h1('Player Data'),
                                h3('Explore the available data. Note that you can select pitching or batting data!'),
                                fluidPage(
                                  # Create a new Row in the UI for selectInputs
                                  fluidRow(
                                    column(6,
                                           selectInput("stats", "Category:", c("Batting", "Pitching"))
                                    ),
                                  ),
                                  # Create a new row for the table.
                                  DT::dataTableOutput("table")
                                )),
                        tabItem(tabName='choose_plot',
                                h1('Season Win Predictor'),
                                fluidPage(
                                  sidebarPanel(
                                    sliderInput("runs", "Runs:", min = 466, max = 1009, value = 734),
                                    sliderInput("hr", "Home Runs", min = 68, max = 307, value = 162),
                                    sliderInput("era", "ERA", min = 2.94, max = 6.38, value = 4.22),
                                    actionButton(inputId = "go", label = "Update")
                                  ),
                                  mainPanel(
                                    fluidRow(
                                      plotlyOutput('trendPlot', height = "600px")
                                    ),
                                    fluidRow(
                                      column(width=6, offset=4, div(tableOutput("values"), style = "font-size:150%"))
                                    )
                                  )
                                )
                        )
                      )
                    )
)

server <- function(input, output) {
  
  outData <- reactive({
    data.temp <- d2k.analyze
    form.temp <- input$form
    form.vars <- as.character(subset(d2k.dd, Form.Name==form.temp)[,1])
    names.temp <- colnames(data.temp)[unique(unlist(lapply(form.vars, function(x){grep(x, colnames(data.temp))})))]
    data.temp <- data.temp[,c('d2krecord_id', 'redcap_event_name', names.temp)]
    data.temp$qids_tot <- apply(d2k.analyze[,468:483], 1, function(x)qids_score(x))
    data.temp$gad_tot <- rowSums(d2k.analyze[,168:174])
    data.temp
  })
  
  output$plot_wins <- renderPlotly({
    
    Teams_temp <- Teams %>%
      filter(yearID==input$year & W>0)
    
    plot_ly(Teams_temp, x = ~teamIDBR, y = ~W, type="bar",
            text = paste("Team: ", Teams_temp$name,
                         "<br>Division Win: ", Teams_temp$DivWin,
                         "<br>League Win: ", Teams_temp$LgWin,
                         "<br>World Series Win: ", Teams_temp$WSWin),
            hoverinfo = 'text')
  })
  
  output$welcome <- renderText({
    paste("Welcome to the Dashboard for Understanding Lahman, or DUL! With the DUL, all the knowledge available in this database is at your fingertips.
          
          - On the Overview page, you can select a year and see important metrics and view the overall wine ratings over time. 
          - In the Longitudinal Plots page, you will find visuals of time trends in the data.
          - The Player Stats page allows you to view and sort player data for the selected year.
          - The Build-A-Plot page allows you to explore different visualizations of batting data for the selected year.
          
If you have any questions, please contact Charles South at csouth@smu.edu")
  })
  
  output$plot1 <- renderPlotly({
    teams90 <- Teams %>%
      filter(yearID >= 1990)
    
    prob.win <- atp_matches_cleaned %>%
      plot_ly(
        x = ~pr_win_on_1st_serve, 
        y = ~pr_win_on_2nd_serve, 
        frame = ~year, 
        #text = ~name, 
       # hoverinfo = "text",
        type = 'scatter',
        mode = 'markers'
      )
    
    prob.win2 <- prob.win %>%
      animation_opts(
        1000, easing = "linear", redraw = FALSE
      )
    
    wins.era2
  })
  
  output$plot2 <- renderPlotly({
    median.salary <- Salaries %>%
      dplyr::group_by(yearID) %>%
      dplyr:: summarize(medsal = median(salary)) %>%
      ggplot(., aes(x = yearID, y = medsal/1e6)) +
      geom_point() +
      geom_smooth() +
      theme_bw() +
      labs(x = "Year", y = "Median MLB salary (millions)")
    ggplotly(median.salary)
  })
  
  output$plot3 <- renderPlotly({
    
    Salaries <- Salaries %>% 
      mutate( salary.adj = salary*(1.03)^( max(yearID) - yearID ) )
    
    d2 <- Salaries %>%
      dplyr::group_by( teamID, yearID, lgID ) %>%
      dplyr::summarize( n.players=n(), adj.budget=sum(salary.adj) ) %>%
      merge( Teams, by=c( "yearID", "teamID", "lgID" ), all=FALSE) %>%
      filter( yearID >= 1900 )
    ave.so <- d2$SO/d2$G
    
    so.plot <- ggplot(d2, aes(yearID, ave.so, color = name )) + 
      geom_path() +
      geom_point() +
      theme_bw() +
      theme(legend.position = "none")
    ggplotly(so.plot)
  })
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    if (input$stats == "Batting") {
      data <- subset(Batting, yearID==input$year)
    }
    if (input$stats == "Pitching") {
      data <- subset(Pitching, yearID==input$year)
    }
    data
  }))
  
  output$champion <- renderValueBox({ 
    
    valueBox( 
      value = subset(Teams, yearID==input$year & WSWin=='Y')$name, 
      subtitle = "World Series Champion", 
      icon = icon("wine-glass"), 
      color = "aqua"
    ) 
  }) 
  
  output$al_mvp <- renderValueBox({ 
    
    valueBox( 
      value = subset(AwardsPlayers, yearID==input$year & lgID=='AL' & awardID=='Most Valuable Player')$playerID, 
      subtitle = "AL MVP", 
      icon = icon("brain"), 
      color = "green"
    ) 
  }) 
  
  output$nl_mvp <- renderValueBox({ 
    
    valueBox( 
      value = subset(AwardsPlayers, yearID==input$year & lgID=='NL' & awardID=='Most Valuable Player')$playerID, 
      subtitle = "NL MVP", 
      icon = icon("smile-beam"), 
      color = "red"
    ) 
  }) 
  
  output$trendPlot <- renderPlotly({
    
    Teams.90 <- subset(Teams, yearID >= 1990)
    fig <- plot_ly(Teams.90, x = ~R, y = ~HR, z = ~ERA, marker=list(size=5, color=~W),
                   text = paste("Team: ", Teams.90$name,
                                "<br>Runs: ", Teams.90$R,
                                "<br>Home Runs: ", Teams.90$HR,
                                "<br>ERA: ", Teams.90$ERA,
                                "<br>Wins: ", Teams.90$W),
                   hoverinfo = 'text')
    #        fig <- fig %>% add_markers()
    fig <- fig %>% layout(scene = list(xaxis = list(title = 'Runs'),
                                       yaxis = list(title = 'Home Runs'),
                                       zaxis = list(title = 'ERA')))
    
    fig
    
    
  })
  
  post.results <- eventReactive(input$go, {
    
    values <- data.frame(R=input$runs, HR=input$hr, ERA=input$era)
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    Teams.90 <- subset(Teams, yearID >= 1990)
    wins.model <- lm(W ~ R + HR + ERA, data=Teams.90)
    predict.wins <- predict(wins.model, newdata=post.results(), interval='predict')[1]
    predict.wins.lower <- predict(wins.model, newdata=post.results(), interval='predict')[2]
    predict.wins.upper <- predict(wins.model, newdata=post.results(), interval='predict')[3]
    
    data.frame(
      Estimate = c("Predicted Number of Wins:",
                   "Lower Limit, 95% Prediction Interval:",
                   "Upper Limit, 95% Prediction Interval:"),
      Value = c(predict.wins, predict.wins.lower, predict.wins.upper),
      stringsAsFactors = FALSE)
  })
  
}

shinyApp(ui, server)