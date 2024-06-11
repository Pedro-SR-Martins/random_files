# Required libraries
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

library(shiny)
library(tidyverse)
library(easystats)
library(boot)

# Function definition
bootstrap_bca_correlations <- function(data, R = 1000) {
  if (!requireNamespace("pacman", quietly = TRUE)) {
    install.packages("pacman")
  }
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    install.packages("tidyverse")
  }
  if (!requireNamespace("easystats", quietly = TRUE)) {
    install.packages("easystats")
  }
  if (!requireNamespace("boot", quietly = TRUE)) {
    install.packages("boot")
  }
  
  pacman::p_load(tidyverse, easystats, boot)
  rs_fun <- function(data, indices) {
    d <- data[indices, ]
    rs <- d %>%
      correlation() %>% .$r
    names(rs) <- d %>%
      correlation() %>%
      data.frame() %>%
      mutate(corxy = paste(Parameter1, Parameter2)) %>%
      .$corxy
    rs
  }
  
  bootstrapped_rs <- boot(data = as.data.frame(data),
                          R = R,
                          statistic = rs_fun)
  
  booted_rs <- matrix(ncol = 3, nrow = NROW(bootstrapped_rs$t0))
  
  for (i in 1:NROW(bootstrapped_rs$t0)) {
    tmp <- boot::boot.ci(bootstrapped_rs,
                         type = "bca", index = i)
    tmp2 <- c(tmp$t0, tmp$bca[, 4:5])
    booted_rs[i, ] <- tmp2
  }
  rownames(booted_rs) <- names(bootstrapped_rs$t0)
  colnames(booted_rs) <- c("r", "Lower", "upper")
  BOOT_RS <- booted_rs %>%
    data.frame() %>%
    rownames_to_column(var = "Variable.pair") %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
  
  # Create a correlation matrix with confidence intervals
  correlation_matrix <- matrix(NA, nrow = ncol(data), ncol = ncol(data))
  dimnames(correlation_matrix) <- list(colnames(data), colnames(data))
  
  for (i in 1:NROW(BOOT_RS)) {
    var_pair <- strsplit(BOOT_RS$Variable.pair[i], " ")[[1]]
    cor_val <- BOOT_RS$r[i]
    conf_int <- paste(BOOT_RS$Lower[i], BOOT_RS$upper[i], sep = ", ")
    
    row_idx <- match(var_pair[1], colnames(correlation_matrix))
    col_idx <- match(var_pair[2], colnames(correlation_matrix))
    
    correlation_matrix[row_idx, col_idx] <- paste(cor_val, "[", conf_int, "]", sep = " ")
    correlation_matrix[col_idx, row_idx] <- paste(cor_val, "[", conf_int, "]", sep = " ")
  }
  
  
  
  return(list(
    bootstrapped_correlations = BOOT_RS,
    correlation_matrix = correlation_matrix
  ))
}


# UI
ui <- fluidPage(
  titlePanel("Bootstrap BCA Correlations"),
  sidebarLayout(
    sidebarPanel(
      # Data input
      textAreaInput("data_input", "Paste your dataframe here (space-separated values):", 
                    width = "100%", height = "200px"),
      # Number of bootstraps input
      numericInput("num_bootstraps", "Number of Bootstraps:", value = 1000),
      
      # Output choice
      radioButtons("output_choice", "Choose Output:",
                   choices = c("Bootstrapped Correlations", "Correlation Matrix"),
                   selected = "Bootstrapped Correlations"),
      
      # Submit button
      actionButton("submit_btn", "Submit")
    ),
    mainPanel(
      # Output display
      tableOutput("output_display")
    )
  )
)

# Server
server <- function(input, output) {
  # Function to read the user input and convert it to a dataframe
  data_df <- reactive({
    req(input$data_input)
    data <- read.table(text = input$data_input, header = TRUE)
    return(data)
  })
  
  # Function to update the output based on user choices
  output_data <- eventReactive(input$submit_btn, {
    data <- data_df()
    R <- input$num_bootstraps
    choice <- input$output_choice
    
    result <- bootstrap_bca_correlations(data, R)
    
    if (choice == "Bootstrapped Correlations") {
      output_data <- result$bootstrapped_correlations
    } else {
      # Display correlation matrix without rownames
      output_data <- as.data.frame(result$correlation_matrix)
      rownames(output_data) <- NULL
    }
    
    return(output_data)
  })
  
  # Display the output
  output$output_display <- renderTable({
    output_data()
  })
}

# Run the Shiny app
shinyApp(ui, server)
