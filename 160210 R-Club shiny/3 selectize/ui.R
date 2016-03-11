library(shiny)
library(shinydashboard)

load("df.expr.RData")


shinyUI(dashboardPage(
    dashboardHeader(title = "testapp"),
    dashboardSidebar(
        selectInput("SelectGene", "Select gene", rownames(df.expr)),
        selectizeInput("InputGenes", "Select genes", rownames(df.expr), multiple = TRUE)
    ),
    dashboardBody(
        fluidRow(
            box(textOutput("Genes"))
        ),
        fluidRow(
            box(title = "GeneExpression", status = "primary", solidHeader = TRUE, width = 12,
                plotOutput("GeneExpression"))
        )
    )
))

