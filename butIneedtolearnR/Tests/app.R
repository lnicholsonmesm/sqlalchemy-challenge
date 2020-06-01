# My first app example!!
# Laura

#1 Attach necessary packages

library(shiny)
library(tidyverse)

#command+shift+return = all
#command+return = just the one line

#Get penguins.csv data
penguins <- read_csv("penguins.csv")

#2 Create 'ui' = "User Interface"

#google code for what you want--like radio buttons!
ui <- fluidPage(
  titlePanel("This is my awesome app title!"),
  sidebarLayout(
    sidebarPanel("here are my widgets",
                 radioButtons(inputId = "species",
                              label = "Choose penguin species:",
                              choices = c("Adelie","Gentoo", "Awesome chinstrap penguins"="Chinstrap")),
                 selectInput(inputId = "pt_color",
                             label = "Select a point color!",
                             choices = c("Favorite RED!!!" = "red",
                                         "Pretty Purple" = "purple",
                                         "That last color" = "orange" ))),
    mainPanel("here is my graph!",
              plotOutput(outputId = "penguin_plot"))
  )
)

#create server
server <- function(input, output){
  
  #created a reactive data frame:
  penguin_select <- reactive({
    penguins %>% 
      filter(sp_short == input$species)
  })
  output$penguin_plot <- renderPlot({
    ggplot(data = penguin_select(), aes(x= flipper_length_mm, y= body_mass_g))+
      geom_point(color = input$pt_color)
  })
}

#Let R know that you want to combine the ui & server into an app:
shinyApp(ui = ui, server = server)

#MUST SAVE AS app.R

#Building shiny apps is as much about parentheses about everthing else.

