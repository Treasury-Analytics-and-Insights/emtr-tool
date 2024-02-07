#
# This is the user-interface definition of the Income Explorer Shiny web application. You can
# run the application by clicking 'Run App' above.
#
###############################
# Libraries
###############################

suppressMessages({
  library(shiny)
  library(shinythemes)
  library(plotly)
  library(rhandsontable)
  library(data.table)
})

# Define UI
shinyUI(fluidPage(
  # different themes from shinythemes R package, https://rstudio.github.io/shinythemes/
  theme = shinytheme("sandstone"), # united
  # Application title
  title = "EMTR Scenario Family Tool",
  
  navbarPage("EMTR Scenario Family Tool",
  tabPanel("EMTR Analysis",
  
  # Side menu
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # Input the hourly wage and hours
      fluidRow(
        column(6, numericInput(
          "wage1_hourly", "Hourly wage ($):",
          min = 18, max = 100,
          #TY24 minimum wage
          value = 22.70, step = .5
        )),
        # Select winter energy payment settings
        column(6, selectInput(
          "MFTC_WEP_scaling", "Winter Energy Payment:",
          c("Average week" = 1, "Winter week" = 12/5, "Summer week" = 0),
          selected = "Average week"
        ))),

      # Input accomodation cost settings
      fluidRow(
        column(6, numericInput(
          "AS_Accommodation_Costs", "Weekly Housing Cost ($):",
          min = 0, max = 1000, value = 450, step = 1 
        )),
        column(4, selectInput(
          "AS_Area", label = "AS Area:",
          choices = c(1, 2, 3, 4), selected = 2
        )),
        column(4, selectInput(
          "Acc_type", label = "Housing",
          choices = c("Renting", "Mortgage"), selected = "Renting"
        ))
      ),
      # Input the children's ages
      textInput(
        "Children_ages",
        "Age of children (e.g. '1, 4' or leave blank)",
        "0, 10"
      ),
      # Input partner status
      checkboxInput("Partnered", "Partnered", value = FALSE),
      
      # Input parter wage details, note that this is only displayed if there is a partner
      fluidRow(
        column(6, conditionalPanel(
          condition = "input.Partnered == 1",
          numericInput(
            "gross_wage2", "Partner's hourly wage ($):",
            min = 15, max = 100, value = 20, step = .5
          )
        )),
        column(6, conditionalPanel(
          condition = "input.Partnered == 1",
          numericInput(
            "hours2", "Partner's hours worked:",
            min = 0, max = 80, value = 0, step = 1
          )
        ))
      ),
      
      # Input to allow the user to choose situations of people
      # stay on benefit, or on WFF or showing the maximum amount
      # between the two options
      checkboxInput("Advanced", "Advanced", value = FALSE),
      fluidRow(
        column(6, conditionalPanel(
          condition = "input.Advanced == 1",
          selectInput(
            "WFFBEN_SQ", "WFF or Benefit",
            c("Max", "WFF", "Benefit"), selected = "Max"
          )
        ))),
      
      # Comment out download button when deploying to shinylive
      # downloadButton("downloadData", "Download Results")
    ),
    # Main panel containing plots etc.
    mainPanel(
      # Select plot to view
      tabsetPanel(
        tabPanel("EMTR", 
                 h2("Effective Marginal Tax Rate"), 
                 plotlyOutput("plot_emtr", height = "300px")),
        tabPanel("Income Composition",
                 h2("Income Composition"), 
                 plotlyOutput("plot_incomecomposition_SQ", height = "400px")),
        tabPanel("Budget Constraint",
                 h2("Budget Constraint"), 
                 plotlyOutput("plot_budgetconstraint_SQ", height = "400px"))
      ),
          
      h2(" ") # Add padding to bottom of page
    )
  )
  ),
  # Very basic overview of the tool
  tabPanel("About", 
           mainPanel(h2("About the Tool"),
                     p("This tool is designed to support policy analysts in understanding the EMTR profiles faced by a range of different scenario families in order to help inform the design of Tax and Welfare policy. "),
                     h2("Analytics & Insights - The Treasury"),
                     p("The EMTR Scenario Family tool was developed by the Analytics & Insights team in the New Zealand Treasury's Office of the Chief Economic Adviser.")
  ))
)))
