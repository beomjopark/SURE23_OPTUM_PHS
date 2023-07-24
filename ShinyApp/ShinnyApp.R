library(shiny)
library(rsconnect)
rsconnect::setAccountInfo(name='bdlp5f-hugo-baca',
                          token='8E15EBFA411A95DC922A43F3122CBD34',
                          secret='Wh7lRaJZV7eqScEk1U6gOWK9ibH0oAb04zk84Vyc')

rsconnect::deployApp('../test/ShinyApp/ShinnyApp.R')
ui <- fluidPage(
  "Healthcare Optimization for Preventable Stays"
)
server <- function(input, output, session) {
}
shinyApp(ui, server)