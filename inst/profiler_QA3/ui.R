
library(shiny)
library(rhandsontable)

shinyUI(fluidPage(

  titlePanel("ESM2 Profiler QA3 Tool"),
  fluidRow(
    column(2, numericInput('select_year', "Select Year", min = 2005, value = 2016)),
    column(4, selectInput('select_cruiseID', "Select Cruise ID", choices = "")),
    column(3, selectInput('select_profile', "Select Profile", choices = "")),
    column(3, selectInput('select_variable', "Select Variable", choices = ""))
  ),
  plotOutput("bottle_plot", brush = brushOpts("scan_brush", direction = "x"), width = "100%"),
  fluidRow(
    column(6, align = "left",
     wellPanel( textOutput("mark_text") )
    ),
    column(6, align = "left", actionButton("mark_btn", "Add selection", icon = icon("caret-square-o-left")))
  ),
  wellPanel(
    rHandsontableOutput("marks")
  ),
  plotOutput("regression", width = "100%"),
  tableOutput("debug")
))
