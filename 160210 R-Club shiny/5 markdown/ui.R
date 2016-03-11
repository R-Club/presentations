library(shiny)
library(shinydashboard)

load("df.expr.RData")


shinyUI(dashboardPage(
    dashboardHeader(title = "testapp"),
    dashboardSidebar(
        textInput("InputGene", "Input gene names", value = ""),
        actionButton("ButtonSearch", "Search genes"),
        actionButton("ButtonGraph", "Draw graph")
    ),
    dashboardBody(
        fluidRow(
            box(textOutput("GeneSearchRow")),
            box(textOutput("GeneSearchName"))
        ),
        fluidRow(
            box(title = "GeneExpression", status = "primary", solidHeader = TRUE, width = 12,
                plotOutput("GeneExpression"))
        )
    )
))

