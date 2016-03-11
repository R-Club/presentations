library(shiny)

load("df.expr.RData")


shinyServer(function(input, output) {
    output$Genes <- renderText({print(input$InputGenes)})
    output$GeneExpression <- renderPlot({
        plot(x = 1:length(df.expr[1, ]), y = df.expr[which(rownames(df.expr) == input$SelectGene), ])
    })
})

