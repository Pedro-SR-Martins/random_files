# Load necessary libraries
library(shiny)
library(DT)

# Bootstrap correlation function
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

# Define the UI
ui <- fluidPage(
  titlePanel("Bootstrap Correlation Analysis"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("data_input", "Paste your dataframe here (space-separated values):", 
                    width = "100%", height = "200px"),
      br(),
      numericInput("num_bootstraps", "Number of Bootstraps:", value = 1000),
      actionButton("run_analysis", "Run Analysis"),
      downloadButton("download_results", "Download Results")
    ),
    mainPanel(
      DTOutput("bootstrapped_correlations_table"),
      DTOutput("correlation_matrix_table")
    )
  )
)

# Define the server
server <- function(input, output) {
  # Function to read the user input and convert it to a dataframe
  data_df <- reactive({
    req(input$data_input)
    data <- read.table(text = input$data_input, header = TRUE, sep = " ")
    return(data)
  })
  
  # Function to run the analysis and update the tables
  observeEvent(input$run_analysis, {
    data <- data_df()
    num_bootstraps <- input$num_bootstraps
    analysis_result <- bootstrap_bca_correlations(data, R = num_bootstraps)
    
    # Update the bootstrapped_correlations_table
    output$bootstrapped_correlations_table <- renderDT({
      datatable(analysis_result$bootstrapped_correlations)
    })
    
    # Update the correlation_matrix_table
    output$correlation_matrix_table <- renderDT({
      datatable(analysis_result$correlation_matrix, rownames = TRUE)
    })
  })
  # Function to download the analysis results as CSV files
  observeEvent(input$download_results, {
    data <- data_df()
    num_bootstraps <- input$num_bootstraps
    analysis_result <- bootstrap_bca_correlations(data, R = num_bootstraps)
    write.csv(analysis_result$bootstrapped_correlations, "bootstrapped_correlations.csv", row.names = FALSE)
    write.csv(analysis_result$correlation_matrix, "correlation_matrix.csv", row.names = TRUE)
  })
}

# Run the Shiny app
shinyApp(ui, server)
