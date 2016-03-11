library(shiny)


shinyUI(fluidPage(
    textInput("InputText", "Input your text", value = ""),
    selectInput("SelectElement", "Select an element", c("option 1", "option 2", "option 3")),
    textOutput("OutputText"),
    textOutput("OutputSelect")
))

