#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# load in data (2015-2022) cleaned data
atp_matches_cleaned <- read.csv("C:/Users/timst/Documents/SMU/Fall_2022/STAT6341/Project/Tennis_Data_Reduced/reduced_clean_matches.csv",
                                header =T)


ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel("", windowTitle="Mavs Shots 21-22"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("num",
                                "Choose a number",
                                min = 0,
                                max = 1000,
                                value = 20),
                    selectInput("var",
                                label = "Choose a player",
                                choices = unique(mavs_shots$namePlayer),
                                selected = "Luka Doncic")
                  ),
                  mainPanel(
                    plotOutput("myplot")
                  )
                )
)

#-------------------------------------------------------------------------------------------------------

server <- function(input, output) {
  output$myplot <- renderPlot({
    sample.index <- sample(1:nrow(mavs_shots), input$num)
    mavs_shots_short <- subset(mavs_shots, namePlayer==input$var)[sample.index,]
    mavs_shots_short %>%
      ggplot(aes(x = locationX, y = locationY,
                 color = isShotMade)) +
      geom_point(alpha = 0.5) +
      scale_color_manual(values = c("darkorange", "darkblue")) +
      theme_bw() +
      scale_x_continuous(limits=c(min(mavs_shots$locationX), max(mavs_shots$locationX))) +
      scale_y_continuous(limits=c(min(mavs_shots$locationY), max(mavs_shots$locationY))) +
      labs(title = paste(input$var))
  })
}

shinyApp(ui = ui, server = server)