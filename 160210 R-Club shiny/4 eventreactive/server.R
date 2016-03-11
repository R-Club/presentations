library(shiny)

load("df.expr.RData")



shinyServer(function(input, output) {
    InputText <- eventReactive(input$ButtonSearch, {grep(input$InputGene, rownames(df.expr))})
    output$GeneSearchRow <- renderText({print(InputText())})
    output$GeneSearchName <- renderText({print(rownames(df.expr)[InputText()])})
    InputGraph <- eventReactive(input$ButtonGraph, {grep(input$InputGene, rownames(df.expr))})
    output$GeneExpression <- renderPlot({
        plot(x = 1:length(df.expr[1, ]), y = df.expr[InputGraph(), ])
    })
})

