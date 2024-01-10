# Converts a string used to input children's ages into numeric values
convert_ages <- function(input_string){
  ages = as.numeric(strsplit(input_string, ",")[[1]])
  ages = sort(ages)
  return(ages)
}

# Plot budget contraints/income composition
# new plotting functions based off plotly R package
amounts_net_plot <- 
  function(EMTR_table, inc_limit=NULL, y_min=NULL, y_max=NULL,
           watermark=FALSE, weeks_in_year=52L,
           display_cols = TRUE) {
    
    if (display_cols == TRUE) {
      # display full income composition
      display_cols = c("Net Income", "Best Start", "Winter Energy", "Accomodation Supplement", 
                       "IWTC", "FTC", "MFTC", "IETC", "Net Core Benefit", "Net Wage", 
                       "Net Wage (Partner)", "Tax on Core Benefit", "Tax on Wage and ACC")
    } else {
      # display budget constraint
      display_cols = c("Net Income")
    }
    
    # tsy_palette <- RColorBrewer::brewer.pal(n = 12, name = "Paired")
    tsy_palette <- c( 
      rgb(0, 131, 172, maxColorValue = 255),
      rgb(0, 188, 226, maxColorValue = 255),
      rgb(103, 168, 84, maxColorValue = 255),
      rgb(188, 214, 81, maxColorValue = 255),
      rgb(241, 164, 45, maxColorValue = 255),
      rgb(239, 150, 108, maxColorValue = 255),
      rgb(0, 79, 103, maxColorValue = 255),
      rgb(0, 113, 136, maxColorValue = 255),
      rgb(62, 101, 50, maxColorValue = 255),
      rgb(122, 143, 34, maxColorValue = 255),
      rgb(13, 143, 34, maxColorValue = 255),
      rgb(122, 42, 34, maxColorValue = 255)
    )
    tsy_palette <- colorRampPalette(tsy_palette)(20)
    
    set_tsy_palette <- c("Best Start" = tsy_palette[1], 
                     "Winter Energy" = tsy_palette[2], 
                     "Accomodation Supplement" = tsy_palette[3], 
                     "IWTC" = tsy_palette[4], 
                     "FTC" = tsy_palette[5], 
                     "MFTC" = tsy_palette[6], 
                     "IETC" = tsy_palette[7], 
                     
                     "Net Core Benefit" = tsy_palette[9], 
                     "Net Wage" = tsy_palette[10], 
                     "Net Wage (Partner)" = tsy_palette[12], 
                     "Tax on Core Benefit" = tsy_palette[12], 
                     "Tax on Wage and ACC" = tsy_palette[11])
    
    X <- copy(EMTR_table)
    
    two_adults <- (X[, max(net_benefit2)]>0) # Do we need this? 
    
    wage1_hourly <- X[2, gross_wage1/hours1] # Do we need this? 
    
    if (is.null(inc_limit))
      inc_limit <- X[,max(gross_wage1_annual)]
    
    Y <- EMTR_table[,.(gross_wage1_annual,
                       gross_benefit1,
                       gross_benefit2,
                       net_benefit = net_benefit1 + net_benefit2,
                       net_wage1,
                       net_wage2,
                       benefit_tax=-(gross_benefit1+gross_benefit2-net_benefit1-net_benefit2),
                       gross_wage=gross_wage1+gross_wage2,
                       wage_tax_and_ACC=-(wage1_tax+wage2_tax+wage1_ACC_levy+wage2_ACC_levy),
                       IETC_abated=IETC_abated1+IETC_abated2,
                       
                       FTC_abated,MFTC,
                       IWTC_abated,
                       AS_Amount,
                       WinterEnergy,
                       BestStart_Total, 
                       Net_Income)]
    
    Y[, ':=' (gross_benefit1 = NULL,
              gross_benefit2 = NULL )]
    
    Y <- Y[, lapply(.SD, function(x) x*weeks_in_year), by = .(gross_wage1_annual)]
    
    p <- 
      plot_ly(Y) %>%
      add_trace(x=~hours1, y=~0, line=list(width = 0), xaxis="x2", 
                data=X, showlegend=FALSE, inherit=FALSE, 
                hoverinfo = "none", type = "scatter", mode = "lines") %>%
      layout(xaxis2 = list(overlaying = "x", nticks = 10, side = "top",
                           title = "Hours/week", automargin=TRUE, size=8,
                           showline = TRUE),
             xaxis = list(title = "Annual gross wage income ($)", 
                          tickformat = "$", 
                          automargin=TRUE,
                          zeroline = TRUE,
                          showline = TRUE,
                          mirror=TRUE),
             yaxis = list (title = "Income ($)", tickformat = "$", 
                           automargin=TRUE,
                           zeroline = TRUE,
                           showline = TRUE,
                           mirror=TRUE
                           # range = c(0, max(1, data1_for_plot$value) + 0.03)
                           ), # TEST THIS
             legend = list(x = 100, y = 0.5),
             hovermode = "compare") 
    
    if("Tax on Wage and ACC" %in% display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~wage_tax_and_ACC, name = 'Tax on Wage and ACC', 
                           fillcolor = set_tsy_palette[13], stackgroup = 'one',
                           hovertemplate = paste("Tax on Wage and ACC: %{y:$,.0f}<extra></extra>"))  
    
    if("Tax on Core Benefit" %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none', 
                           y = ~benefit_tax, name = "Tax on Core Benefit", 
                           fillcolor = set_tsy_palette[12], stackgroup = 'one',
                           hovertemplate = paste("Tax on Core Benefit: %{y:$,.0f}<extra></extra>"))  
    
    if("Net Wage (Partner)" %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~net_wage2, name = 'Net Wage (Partner)', stackgroup = 'two', 
                           fillcolor = set_tsy_palette[11],
                           hovertemplate = paste("Net Wage (Partner): %{y:$,.0f}<extra></extra>"))  
    
    if("Net Wage" %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~net_wage1, name = 'Net Wage', stackgroup = 'two',
                           fillcolor = set_tsy_palette[10],
                           hovertemplate = paste(
                             "Annual gross wage income:\n %{x:$,.2f} \n",
                             "Net Wage: %{y:$,.0f}<extra></extra>"))  
    
    if("Net Core Benefit" %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~net_benefit, name = 'Net Core Benefit',
                           fillcolor = set_tsy_palette[9], stackgroup = 'two',
                           hovertemplate = paste("Net Core Benefit: %{y:$,.0f}<extra></extra>"))  
    
    
    
    if("IETC" %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~IETC_abated, name = 'IETC',
                           fillcolor = set_tsy_palette[7], stackgroup = 'two',
                           hovertemplate = paste("IETC: %{y:$,.0f}<extra></extra>"))  
    
    if("MFTC" %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~MFTC, name = 'MFTC', 
                           fillcolor = set_tsy_palette[6], stackgroup = 'two',
                           hovertemplate = paste("MFTC: %{y:$,.0f}<extra></extra>"))  
    
    if("FTC" %in% display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~FTC_abated, name = 'FTC',
                           fillcolor = set_tsy_palette[5], stackgroup = 'two',
                           hovertemplate = paste("FTC: %{y:$,.0f}<extra></extra>"))  
    
    if("IWTC" %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~IWTC_abated, name = 'IWTC',
                           fillcolor = set_tsy_palette[4], stackgroup = 'two',
                           hovertemplate = paste("IWTC: %{y:$,.0f}<extra></extra>"))  
    
    if("Accomodation Supplement" %in%  display_cols)
      p <- p %>%   add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                             y = ~AS_Amount,
                             name = 'Accomodation Supplement',
                             fillcolor = set_tsy_palette[3], stackgroup = 'two',
                             hovertemplate = paste("Accomodation Supplement: %{y:$,.0f}<extra></extra>"))  
    
    if("Winter Energy"  %in%  display_cols )
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~WinterEnergy, name = 'Winter Energy',
                           fillcolor = set_tsy_palette[2], stackgroup = 'two',
                           hovertemplate = paste("Winter Energy: %{y:$,.0f}<extra></extra>"))  
    
    if("Best Start"  %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~BestStart_Total, 
                           name = 'Best Start', fillcolor = set_tsy_palette[1], 
                           stackgroup = 'two',
                           hovertemplate = paste("Best Start: %{y:$,.0f}<extra></extra>"))   
    
    
    #Adding a line for Net Income
    if("Net Income"  %in%  display_cols)
      p <- 
      p %>% add_lines(data = Y, x = ~gross_wage1_annual, 
                      y = ~Net_Income, name = 'Net Income', color = I("black"),
                      hovertemplate = paste("Net Income: %{y:$,.0f}<extra></extra>")) 
    
    return(p)
    
  }

# Plot effective Marginal tax rates
compare_plots <- function(data1,  
                          type = "EMTR",
                          min_rate=0, max_rate=1.1,
                          inc_limit=NULL, title=NULL,
                          policy_name1 = 'Status Quo',
                          policy_name2 = 'Policy 1',
                          watermark=FALSE,
                          weeks_in_year=52L
) {
  
  type <- match.arg(type)  
  
  data1_for_plot <- copy(data1)
  
  if(type == "EMTR"){
    setnames(data1_for_plot, "EMTR", "value1")
    y_axis_title <- "Effective Marginal Tax Rate"
  }
  
  
  data1_for_plot[, value1 := pmax(pmin(value1, max_rate), min_rate)]
  data1_for_plot <- 
    data1_for_plot[, .(gross_wage1, gross_wage1_annual, value1)]
  
  data1_for_plot %<>% melt(id.vars=c('gross_wage1','gross_wage1_annual'),
                           variable.name='Scenario')
  
  data1_for_plot[Scenario=="value1", Scenario:=policy_name1]
  
  data1_for_plot %>% dcast(gross_wage1 + gross_wage1_annual ~ Scenario) %>%
    plot_ly(x = ~gross_wage1_annual, y = ~`Status Quo`, name = policy_name1, 
            mode = "lines", type = 'scatter',
            line = list(color = "#56B4E9", width = 3),
            hovertemplate =  paste0(
              "Annual gross wage income:\n %{x:$,.2f}\n ",
              policy_name1, ": %{y:.2%} <extra></extra>")) %>% 
    add_trace(x=~hours1, y=~0, line=list(width = 0), xaxis="x2", 
              data=data1, showlegend=FALSE, inherit=FALSE,
              hoverinfo="none", type = "scatter", mode = "lines") %>%
    layout(xaxis2 = list(overlaying = "x", nticks = 10, side = "top",
                         title = "Hours/week", automargin=TRUE, size=8,
                         showline = TRUE),
           xaxis = list(title = "Annual gross wage income ($)", 
                        tickformat =  "$,",
                        automargin=TRUE,
                        showline = TRUE,
                        mirror=TRUE,
                        margin = list(b = 0)),
           yaxis = list (title = y_axis_title,
                         # texttemplate='%{y:.2%}',
                         tickformat = ".0%",
                         automargin=TRUE,
                         showline = TRUE,
                         mirror=TRUE,
                         range = c(0, max(1, data1_for_plot$value) + 0.03)
                         ),
           legend = list(x = 100, y = 0.5),
           hovermode = "compare") 
}

remove_IWTC_from_params <- function(input_params) {
  output_params <- copy(input_params)
  output_params$FamilyAssistance_IWTC_Rates_UpTo3Children <- 0
  output_params$FamilyAssistance_IWTC_Rates_SubsequentChildren <- 0
  return(output_params)
}

choose_IWTC_or_benefit <- function(X, X_without_IWTC) {
  # Merge max
  SQ_net_income_comparison <- cbind(
    X[, .(With_IWTC = Net_Income)],
    X_without_IWTC[, .(Without_IWTC = Net_Income)]
  )
  SQ_net_income_comparison[, row_ID := 1:.N]
  With_IWTC_indices <- SQ_net_income_comparison[With_IWTC >= Without_IWTC, row_ID]
  Without_IWTC_indices <- SQ_net_income_comparison[Without_IWTC > With_IWTC, row_ID]
  
  With_IWTC <- X[With_IWTC_indices]
  Without_IWTC <- X_without_IWTC[Without_IWTC_indices]
  
  X <- rbind(With_IWTC, Without_IWTC)
  setorderv(X, "hours1")
  
  X[, EMTR := 1 - 1L*(shift(Net_Income,1L,type="lead")-Net_Income)]
  X[, EMTR := zoo::na.locf(EMTR)]
  
  return(X)
}

# plotly - income composition
plot_income_decomposition <- function(comebined_data, scenario_name, set_tsy_palette) {
  plot_ly(comebined_data[Scenario == scenario_name &
                           hours1 <= 50,]) %>%
    add_trace(x=~hours1, y=~0, line=list(width = 0), xaxis="x2",
              showlegend=FALSE, inherit=FALSE, 
              hoverinfo = "none", type = "scatter", mode = "lines") %>%
    layout(xaxis2 = list(overlaying = "x", nticks = 10, side = "top",
                         title = "Hours/week", automargin=TRUE, size=8,
                         showline = TRUE),
           xaxis = list(title = "Annual gross wage income ($)", 
                        tickformat = "$", 
                        automargin=TRUE,
                        zeroline = TRUE,
                        showline = TRUE,
                        mirror=TRUE),
           yaxis = list (title = "Income ($)", tickformat = "$", 
                         automargin=TRUE,
                         zeroline = TRUE,
                         showline = TRUE,
                         mirror=TRUE),
           legend = list(x = 100, y = 0.5),
           hovermode = "compare")  %>% 
    add_trace(x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
              y = ~wage_tax_and_ACC, name = 'Tax on Wage and ACC', 
              fillcolor = set_tsy_palette[13], stackgroup = 'one',
              hovertemplate = paste("Tax on Wage and ACC: %{y:$,.0f}<extra></extra>"))  %>%
    add_trace(x = ~gross_wage1_annual, type = 'scatter', mode = 'none', 
              y = ~benefit_tax, name = "Tax on Core Benefit", 
              fillcolor = set_tsy_palette[12], stackgroup = 'one',
              hovertemplate = paste("Tax on Core Benefit: %{y:$,.0f}<extra></extra>"))  %>%
    add_trace(x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
              y = ~net_wage2, name = 'Net Wage (Partner)', stackgroup = 'two', 
              fillcolor = set_tsy_palette[11],
              hovertemplate = paste("Net Wage (Partner): %{y:$,.0f}<extra></extra>")) %>% 
    add_trace(x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
              y = ~net_wage1, name = 'Net Wage', stackgroup = 'two',
              fillcolor = set_tsy_palette[10],
              hovertemplate = paste("Net Wage: %{y:$,.0f}<extra></extra>")) %>% 
    add_trace(x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
              y = ~net_benefit, name = 'Net Core Benefit',
              fillcolor = set_tsy_palette[9], stackgroup = 'two',
              hovertemplate = paste("Net Core Benefit: %{y:$,.0f}<extra></extra>"))  %>% 
    
    add_trace(x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
              y = ~IETC_abated, name = 'IETC',
              fillcolor = set_tsy_palette[7], stackgroup = 'two',
              hovertemplate = paste("IETC: %{y:$,.0f}<extra></extra>")) %>% 
    add_trace(x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
              y = ~MFTC, name = 'MFTC', 
              fillcolor = set_tsy_palette[6], stackgroup = 'two',
              hovertemplate = paste("MFTC: %{y:$,.0f}<extra></extra>"))  %>%
    add_trace( x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
               y = ~FTC_abated, name = 'FTC',
               fillcolor = set_tsy_palette[5], stackgroup = 'two',
               hovertemplate = paste("FTC: %{y:$,.0f}<extra></extra>"))  %>%
    add_trace( x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
               y = ~IWTC_abated, name = 'IWTC',
               fillcolor = set_tsy_palette[4], stackgroup = 'two',
               hovertemplate = paste("IWTC: %{y:$,.0f}<extra></extra>")) %>% 
    add_trace( x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
               y = ~AS_Amount,
               name = 'Accommodation Supplement',
               fillcolor = set_tsy_palette[3], stackgroup = 'two',
               hovertemplate = paste("Accommodation Supplement: %{y:$,.0f}<extra></extra>")) %>%
    add_trace( x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
               y = ~WinterEnergy, name = 'Winter Energy',
               fillcolor = set_tsy_palette[2], stackgroup = 'two',
               hovertemplate = paste("Winter Energy: %{y:$,.0f}<extra></extra>")) %>%
    add_trace( x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
               y = ~BestStart_Total, 
               name = 'Best Start', fillcolor = set_tsy_palette[1], 
               stackgroup = 'two',
               hovertemplate = paste("Best Start: %{y:$,.0f}<extra></extra>")) 
}

