library(shiny)




shinyServer(function(input, output) {
    output$OutputText <- renderText({print(paste("You wrote: ", input$InputText, sep = ""))})
    output$OutputSelect <- renderText({print(paste("You selected: ", input$SelectElement, sep = ""))})
})