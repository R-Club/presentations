library(shiny)




shinyServer(function(input, output) {
    merged.text <- reactive(paste("You wrote: ", input$InputText, sep = ""))
    output$OutputText <- renderText({print(merged.text())})
    output$OutputSelect <- renderText({print(paste("You selected: ", input$SelectElement, merged.text(), sep = ""))})
})