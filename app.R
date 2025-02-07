library(shiny)
library(dplyr)
library(ggplot2)
library(glue)
library(DT)

thematic_shiny(
  font = "auto",
)
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly"
  ),
  titlePanel("My First Shiny App"),
  h1("Star Wars Characters"),
  h2("My app from scratch"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "taille",
        label = "Height of Characters:",
        min = 0,
        max = 250,
        value = 30
      ),
      selectInput(
        inputId = "gender",
        label = "Choisir le genre des personnages",
        choices = c("masculine", "feminine")
      ),
    ),
    mainPanel(
      textOutput("StarWarsTitle"),
      plotOutput("StarWarsPlot"),
      DTOutput("StarWarsTable")
    )
  )
)

server <- function(input, output) {
  output$StarWarsPlot <- renderPlot({
    starwars |>
      filter(height > input$taille) |>
      filter(gender == input$gender) |>
      ggplot(aes(x = height)) +
      geom_histogram(
        binwidth = 10,
        fill = "grey",
        color = "white"
      ) +
      labs(
        title = glue("Selection du genre : {input$gender}")
      )
  })
  output$StarWarsTitle <- renderText({
    nb_ligne <- starwars |>
      filter(height > input$taille) |>
      filter(gender == input$gender) |>
      nrow()

    glue("nombre de ligne : {nb_ligne}")
  })
  output$StarWarsTable <- renderDT({
    starwars |>
      filter(height > input$taille) |>
      filter(gender == input$gender)
  })
}


shinyApp(ui = ui, server = server)
