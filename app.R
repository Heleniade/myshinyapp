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
      actionButton(
        inputId = "boutton",
        label = "Cliquez moi"
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
  rv <- reactiveValues()
  observeEvent(c(input$taille,input$gender),{
    rv$starwars_filter <- starwars |>
      filter(height > input$taille) |>
      filter(gender == input$gender)
    })
  output$StarWarsPlot <- renderPlot({
    rv$starwars_filter |>
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
    nb_ligne <- rv$starwars_filter |>
      nrow()

    glue("nombre de ligne : {nb_ligne}")
  })
  output$StarWarsTable <- renderDT({
    rv$starwars_filter
  })
  observeEvent(input$boutton, {
               message("Vous avez cliqué sur le bouton")
               })
  observeEvent(input$taille, {
    showNotification("Valeur du slider changé",
                     type ="message")
  })
}


shinyApp(ui = ui, server = server)
