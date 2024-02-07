#
# This is the server logic of the Income Explorer Shiny web application. You can
# run the application by clicking 'Run App' above.
#

suppressMessages({
  library(shiny)
  library(openxlsx)
  library(rhandsontable)
  library(magrittr)
  library(data.table)
})

# Define server logic required to run the app
shinyServer(function(input, output, session) {
  ##########################################
  # Pop up warning on start 
  ##########################################
  # the modal dialog where the user can enter the query details.
  warning_modal <- modalDialog(
    title = "Warning", 
    paste0(
      "This software is provided as-is, for research purposes only, ",
      "with absolutely no warranty or guarantee of correctness."
    ),
    easyClose = FALSE, fade = FALSE
  )
  
  # Show the model on start up ...
  showModal(warning_modal)
  
  ##########################################
  # Loading data and calculating incomes 
  ##########################################
  
  WEEKS_IN_YEAR <- 52L
  
  # Read in the parameters from files
  reload_data <- reactive({
    if(!is.null(input$show_parameters)){
      DF <- hot_to_r(input$show_parameters)
      parameters_SQ <- parameters_from_df(DF, parameters_column = 2)
    } else {
      parameters_SQ <- get_parameters()
    }
    
    return(list(
      parameters_SQ = parameters_SQ
    ))
  })
  
  
  # calculate incomes 
  calculate_income <- reactive({
    parameters = req(reload_data())
    
    # convert inputs
    MAX_WAGE <- 50*input$wage1_hourly 
    children <- convert_ages(input$Children_ages)
    
    # accommodation type
    if (input$Acc_type == "Renting"){
      AS_Accommodation_Rent <- TRUE
    } else {
      AS_Accommodation_Rent <- FALSE
    }
    
    
    if (input$Partnered == 1){
      if (!is.na(input$hours2)) {
        partner_hours <- input$hours2
      } else {
        partner_hours <- 0
      }
      
      if (!is.na(input$gross_wage2)) {
        partner_wages <- input$gross_wage2*input$hours2
      } else {
        partner_wages <- 0
      }
    } else {
      partner_wages <- 0
      partner_hours <- 0
    }
    
    # Create helper emtr function using current inputs
    hot_emtr <- function(params) {
      emtr_df <- emtr(
        # System parameters
        params,
        # Family parameters
        input$Partnered, input$wage1_hourly, children, partner_wages, partner_hours,
        input$AS_Accommodation_Costs, AS_Accommodation_Rent, as.numeric(input$AS_Area),
        # Presentation parameters
        max_wage = MAX_WAGE, steps_per_dollar = 1L, weeks_in_year = WEEKS_IN_YEAR, 
        MFTC_WEP_scaling = as.numeric(input$MFTC_WEP_scaling)
      )
      return(emtr_df)
    }
    
    X_SQ <- hot_emtr(parameters$parameters_SQ)
    
    # MFTC is meant to make families always better off being off-benefit than staying
    # on a benefit. We let the user set whether the family stays on benefit
    # or gets IWTC when they work, with the parameter input$WFFBEN_SQ. This can be:
    # "Max" - choose the option which maximises the family income.
    # "WFF" - go off the benefit while working, and get IWTC + MFTC.
    # "Benefit" - stay on the benefit while working, and never get IWTC or MFTC
    #             (benefit abates away as earned income increases).
    # Note that these are only applicable when beneficiaries are ineligible for IWTC;
    # MFTC eligibility currently depends on IWTC eligibility in the `emtr` function.
    
    if (input$WFFBEN_SQ != "WFF") {
      # Net income without benefits but including in-work tax credits
      SQ_params_with_no_IWTC <- remove_IWTC_from_params(parameters$parameters_SQ)
      X_SQ_without_IWTC <- hot_emtr(SQ_params_with_no_IWTC)

      if (input$WFFBEN_SQ == "Max") {
        # Choose which of benefit or IWTC gives max net income
        X_SQ <- choose_IWTC_or_benefit(X_SQ, X_SQ_without_IWTC)

      } else if (input$WFFBEN_SQ == "Benefit") {
        # Net income without in-work tax credits but including benefits
        X_SQ <- X_SQ_without_IWTC
      }
    }
    
    # used to ensure that SQ and reform plots have the same axes
    max_income <- max(X_SQ$gross_wage1_annual)
    
    max_net_income <- 1.1*WEEKS_IN_YEAR*max(X_SQ$Net_Income)
    min_y_SQ <- X_SQ[, WEEKS_IN_YEAR*min(
      -(
        gross_benefit1 + gross_benefit2 - net_benefit1 - net_benefit2 +
          wage1_tax + wage2_tax + wage1_ACC_levy + wage2_ACC_levy
      )
    )]

    min_y <- 1.1*min(min_y_SQ) 
    
    return(list(
      X_SQ = X_SQ,
      max_income = max_income,
      max_net_income = max_net_income, 
      min_y = min_y
    ))
  })
  
  ########################################## 
  ### Plots
  ########################################## 
  
  # budget constraint
  output$plot_budgetconstraint_SQ<- renderPlotly({
    compElements = c("Best Start", "Winter Energy", "Accomodation Supplement", "IWTC", "FTC",
                     "MFTC", "IETC", "Net Core Benefit", "Net Wage", "Tax on Core Benefit", "Tax on Wage and ACC")
    X_results <- req(calculate_income())
    X_SQ <- X_results$X_SQ
    amounts_net_plot(
      X_SQ,
      inc_limit = X_results$max_income,
      y_min = X_results$min_y,
      y_max = X_results$max_net_income,
      display_cols = FALSE)
  })
  
  # income composition
  output$plot_incomecomposition_SQ<- renderPlotly({
    compElements = c("Best Start", "Winter Energy", "Accomodation Supplement", "IWTC", "FTC",
                     "MFTC", "IETC", "Net Core Benefit", "Net Wage", "Tax on Core Benefit", "Tax on Wage and ACC")
    X_results <- req(calculate_income())
    X_SQ <- X_results$X_SQ
    amounts_net_plot(
      X_SQ,
      inc_limit = X_results$max_income,
      y_min = X_results$min_y,
      y_max = X_results$max_net_income,
      display_cols = TRUE)
  })
  
  # emtr plots
  output$plot_emtr<- renderPlotly({
    reload_data()
    X_results <- req(calculate_income())
    
    X_SQ <- X_results$X_SQ
    
    compare_plots(X_SQ, type = "EMTR", min_rate = 0, max_rate = 1.1,
      inc_limit = X_results$max_income, title = "EMTR comparison",
      policy_name1 = 'Status Quo', policy_name2 = 'Reform',
      watermark = FALSE, weeks_in_year = WEEKS_IN_YEAR
    )
  })
  
  ########################################## 
  ### Outputs
  ##########################################
  
  # NOTE: Download not available in shinylive
  
  # Download everything 
  output$downloadData <- downloadHandler(
    filename = function() {
      "IncomeExplorerResults.xlsx"
    },
    content = function(file) {
      X_results <- calculate_income()
      parameters = reload_data()
      
      wb <- createWorkbook()
      
      # Details of the example family and input files
      details <- c(
        SQ_file = input$parameters_SQ$name,
        HourlyWage = input$wage1_hourl,
        Partnered = input$Partnered,
        Partner_HourlyWage = input$gross_wage2*(input$Partnered == 1),
        Partner_HoursWorked = input$hours2*(input$Partnered == 1),
        Accomodation_Costs = input$AS_Accommodation_Costs,
        Accomodation_Type = input$Acc_type,
        AS_Area = input$AS_Area,
        Children_Ages = input$Children_ages
      )
      
      addWorksheet(wb, 'Details')
      writeData(wb, 'Details', names(details), startCol=1)
      writeData(wb, 'Details', details, startCol=2)
      
      # Full sets of results (should probably be more selective)
      addWorksheet(wb, 'SQ')
      writeData(wb, 'SQ', X_results$X_SQ)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
})
