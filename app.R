rm(list = ls())


# Load necessary libraries
library(shiny)
library(readxl)
library(dplyr)
library(meta)
library(stringr)


# Define UI
ui <- fluidPage(
  titlePanel("Meta-Analysis: Binary and Continuous Outcomes"),
  
  tags$style(HTML("
    .radio-buttons {
      font-size: 16px;
      font-weight: bold;
      margin-bottom: 20px;
    }
    .radio-buttons label {
      display: block;
      margin-bottom: 10px;
    }
    .radio-buttons .shiny-input-radiogroup {
      display: flex;
      flex-direction: column;
    }
    .radio-buttons .shiny-input-radiogroup label {
      margin-bottom: 10px;
    }
    .group-checkboxes {
      display: flex;
      flex-wrap: wrap;
    }
    .group-checkboxes .checkbox-group {
      margin-right: 20px;
      margin-bottom: 10px;
    }
  ")),
  
  sidebarLayout(
    sidebarPanel(
      h4("Data Upload"),
      fileInput("file", "Upload Excel File (.xlsx)",
                accept = c(".xlsx")),
      
      conditionalPanel(
        condition = "!output.dataUploaded",
        div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
            h5("Required Data Format:", style = "color: #007bff;"),
            p("Choose your outcome type and upload the corresponding Excel format:", style = "font-weight: bold;"),
            
            h6("For Binary Outcomes (Risk Ratios, Odds Ratios):"),
            tags$ul(
              tags$li(strong("author:"), " Unique study identifier (e.g., 'Smith_2023_1')"),
              tags$li(strong("t_incident:"), " Number of events in treatment group"),
              tags$li(strong("t_noincident:"), " Number of non-events in treatment group"),
              tags$li(strong("c_incident:"), " Number of events in control group"),
              tags$li(strong("c_noincident:"), " Number of non-events in control group")
            ),
            
            h6("For Continuous Outcomes (Standardized Mean Differences):"),
            tags$ul(
              tags$li(strong("author:"), " Unique study identifier (e.g., 'Smith_2023_1')"),
              tags$li(strong("t_mean:"), " Mean value in treatment group"),
              tags$li(strong("t_sd:"), " Standard deviation in treatment group"),
              tags$li(strong("t_n:"), " Sample size in treatment group"),
              tags$li(strong("c_mean:"), " Mean value in control group"),
              tags$li(strong("c_sd:"), " Standard deviation in control group"),
              tags$li(strong("c_n:"), " Sample size in control group")
            ),
            
            p(style = "font-weight: bold; margin-top: 15px;", "Additional columns for advanced analyses:"),
            tags$ul(
              tags$li(strong("name:"), " Study title (optional but recommended)"),
              tags$li(strong("cluster:"), " Cluster identifier for multi-arm studies"),
              tags$li(strong("year:"), " Publication year for meta-regression"),
              tags$li(strong("study_type:"), " Study design or category for subgroup analysis"),
              tags$li(strong("intervention_type:"), " Type of intervention or treatment method"),
              tags$li(strong("population_A, population_B:"), " Population characteristics (0/1)"),
              tags$li("Any other categorical or continuous variables for subgroup analyses")
            ),
            
            div(style = "background-color: #fff3cd; padding: 8px; border-left: 4px solid #ffc107; margin: 10px 0;",
                h6("Data Processing Notes:", style = "color: #856404;"),
                tags$ul(style = "font-size: 13px;",
                        tags$li("Each row represents one study arm or comparison"),
                        tags$li("Multi-arm studies should have identical cluster identifiers"),
                        tags$li("All numeric columns must contain valid, non-negative values"),
                        tags$li("Missing or invalid data rows will be automatically excluded"),
                        tags$li("The app will detect your outcome type based on available columns")
                )
            )
        )
      ),
      
      conditionalPanel(
        condition = "output.dataUploaded",
        div(class = "radio-buttons",
            radioButtons("outcome_type", "Outcome Type:",
                         choices = list("Binary Outcomes (Events/No Events)" = "binary", 
                                        "Continuous Outcomes (Means/SDs)" = "continuous"),
                         selected = "binary")
        ),
        
        conditionalPanel(
          condition = "input.outcome_type == 'binary'",
          div(class = "radio-buttons",
              radioButtons("measure", "Effect Measure:",
                           choices = list("Risk Ratio" = "RR", "Odds Ratio" = "OR"),
                           selected = "RR")
          )
        ),
        
        conditionalPanel(
          condition = "input.outcome_type == 'continuous'",
          div(class = "radio-buttons",
              radioButtons("smd_method", "Standardized Mean Difference:",
                           choices = list("Cohen's d" = "SMD", 
                                          "Hedges' g" = "SMDH",
                                          "Glass's Δ" = "GLASS"),
                           selected = "SMD")
          )
        ),
        checkboxGroupInput("selected_interventions", "Select Interventions:", choices = NULL),
        h4("Assign Studies to Groups"),
        uiOutput("group_assignment"),
        h4("Statistical Model Options"),
        checkboxInput("multi_arm", "Apply Three-Level Model for Multi-Arm Studies", value = FALSE)
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.file == null",
        div(style = "text-align: center; padding: 50px;",
            h3("Meta-Analysis: Binary and Continuous Outcomes", style = "color: #007bff;"),
            p("Upload your Excel file to conduct meta-analysis of binary or continuous outcome data.", style = "font-size: 18px;"),
            br(),
            h4("Sample Data Structures:"),
            div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; text-align: left;",
                p(strong("Binary outcomes (2×2 contingency table data):")),
                tags$table(class = "table table-sm", style = "font-size: 12px; margin-bottom: 15px;",
                           tags$thead(
                             tags$tr(
                               tags$th("author"),
                               tags$th("name"),
                               tags$th("t_incident"),
                               tags$th("t_noincident"),
                               tags$th("c_incident"),
                               tags$th("c_noincident")
                             )
                           ),
                           tags$tbody(
                             tags$tr(
                               tags$td("Smith_2023_1"),
                               tags$td("Study Title Here"),
                               tags$td("25"),
                               tags$td("75"),
                               tags$td("15"),
                               tags$td("85")
                             )
                           )
                ),
                p(strong("Continuous outcomes (means and standard deviations):")),
                tags$table(class = "table table-sm", style = "font-size: 12px;",
                           tags$thead(
                             tags$tr(
                               tags$th("author"),
                               tags$th("name"),
                               tags$th("t_mean"),
                               tags$th("t_sd"),
                               tags$th("t_n"),
                               tags$th("c_mean"),
                               tags$th("c_sd"),
                               tags$th("c_n")
                             )
                           ),
                           tags$tbody(
                             tags$tr(
                               tags$td("Jones_2024_1"),
                               tags$td("Another Study"),
                               tags$td("12.5"),
                               tags$td("2.1"),
                               tags$td("50"),
                               tags$td("10.2"),
                               tags$td("2.3"),
                               tags$td("48")
                             )
                           )
                )
            )
        )
      ),
      
      conditionalPanel(
        condition = "output.dataUploaded",
        tabsetPanel(
          tabPanel("Data Preview",
                   h3("Uploaded Data Summary"),
                   verbatimTextOutput("data_summary"),
                   h4("First 10 Studies"),
                   tableOutput("data_preview")
          ),
          tabPanel("Main Analysis",
                   h3("Study Names"),
                   tableOutput("study_desc"),
                   h3("Meta-Analysis Summary"),
                   verbatimTextOutput("meta_summary"),
                   h3("Forest Plot"),
                   plotOutput("forest_plot", height = "600px")
          ),
          tabPanel("Sensitivity Analysis",
                   h3("Meta-Analysis Summary (Haenszel-Mantel)"),
                   verbatimTextOutput("fixed_meta_summary"),
                   h3("Forest Plot (Haenszel-Mantel)"),
                   plotOutput("fixed_forest_plot", height = "600px")
          ),
          tabPanel("Publication Bias",
                   h3("Funnel Plot"),
                   plotOutput("funnel_plot", height = "600px"),
                   h3("Egger's Test for Funnel Plot Asymmetry"),
                   verbatimTextOutput("eggers_test"),
                   h3("Trim and Fill Analysis"),
                   verbatimTextOutput("trim_fill_summary"),
                   plotOutput("trim_fill_plot", height = "600px")
          ),
          tabPanel("Subgroup Analysis",
                   h3("Subgroup Analysis Results"),
                   verbatimTextOutput("subgroup_meta_summary"),
                   plotOutput("subgroup_forest_plot", height = "800px"),  # Increased height for better visibility
                   conditionalPanel(
                     condition = "output.subgroup_error_message !== null",
                     verbatimTextOutput("subgroup_error_message")
                   )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  data <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    }
    
    # Read uploaded file
    ext <- tools::file_ext(input$file$datapath)
    if (ext != "xlsx") {
      showNotification("Please upload an Excel (.xlsx) file.", type = "error")
      return(NULL)
    }
    
    tryCatch({
      uploaded_data <- read_xlsx(input$file$datapath)
      
      # Show basic info about uploaded data
      showNotification(paste("Uploaded data with", nrow(uploaded_data), "rows and", ncol(uploaded_data), "columns"), 
                       type = "default", duration = 3)
      
      # Detect data type based on available columns
      binary_cols <- c("t_incident", "t_noincident", "c_incident", "c_noincident")
      continuous_cols <- c("t_mean", "t_sd", "t_n", "c_mean", "c_sd", "c_n")
      
      has_binary <- all(binary_cols %in% names(uploaded_data))
      has_continuous <- all(continuous_cols %in% names(uploaded_data))
      
      if (!has_binary && !has_continuous) {
        showNotification("Data must contain either binary outcome columns (t_incident, t_noincident, c_incident, c_noincident) OR continuous outcome columns (t_mean, t_sd, t_n, c_mean, c_sd, c_n)", 
                         type = "error", duration = 10)
        return(NULL)
      }
      
      # Check for required columns
      required_cols <- c("author")
      if (has_binary) {
        required_cols <- c(required_cols, binary_cols)
      }
      if (has_continuous) {
        required_cols <- c(required_cols, continuous_cols)
      }
      
      missing_cols <- setdiff(required_cols, names(uploaded_data))
      
      if (length(missing_cols) > 0) {
        showNotification(paste("Missing required columns:", paste(missing_cols, collapse = ", ")), 
                         type = "error", duration = 10)
        return(NULL)
      }
      
      # Data cleaning and validation
      if (has_binary) {
        cleaned_data <- uploaded_data %>%
          filter(
            !is.na(author),
            !is.na(t_incident) & is.numeric(t_incident) & t_incident >= 0,
            !is.na(t_noincident) & is.numeric(t_noincident) & t_noincident >= 0,
            !is.na(c_incident) & is.numeric(c_incident) & c_incident >= 0,
            !is.na(c_noincident) & is.numeric(c_noincident) & c_noincident >= 0
          ) %>%
          mutate(
            t_incident = as.integer(t_incident),
            t_noincident = as.integer(t_noincident),
            c_incident = as.integer(c_incident),
            c_noincident = as.integer(c_noincident),
            data_type = "binary"
          )
      } else {
        cleaned_data <- uploaded_data %>%
          filter(
            !is.na(author),
            !is.na(t_mean) & is.numeric(t_mean),
            !is.na(t_sd) & is.numeric(t_sd) & t_sd >= 0,
            !is.na(t_n) & is.numeric(t_n) & t_n > 0,
            !is.na(c_mean) & is.numeric(c_mean),
            !is.na(c_sd) & is.numeric(c_sd) & c_sd >= 0,
            !is.na(c_n) & is.numeric(c_n) & c_n > 0
          ) %>%
          mutate(
            t_n = as.integer(t_n),
            c_n = as.integer(c_n),
            data_type = "continuous"
          )
      }
      
      # Check if we lost too many rows
      rows_removed = nrow(uploaded_data) - nrow(cleaned_data)
      if (rows_removed > 0) {
        showNotification(paste("Removed", rows_removed, "rows with missing or invalid data"), 
                         type = "warning", duration = 5)
      }
      
      if (nrow(cleaned_data) == 0) {
        showNotification("No valid data rows found. Please check your data format.", 
                         type = "error", duration = 10)
        return(NULL)
      }
      
      # Create cluster_name column (for multi-arm correction)
      if (!"cluster" %in% names(cleaned_data)) {
        cleaned_data <- cleaned_data %>%
          mutate(cluster_name = str_remove(author, "_\\d+$"))
      } else {
        cleaned_data <- cleaned_data %>%
          mutate(cluster_name = ifelse(is.na(cluster) | cluster == "", 
                                       str_remove(author, "_\\d+$"), 
                                       cluster))
      }
      
      # Additional validation for binary data
      if (has_binary) {
        invalid_studies <- cleaned_data %>%
          filter((t_incident + t_noincident) == 0 | (c_incident + c_noincident) == 0)
        
        if (nrow(invalid_studies) > 0) {
          showNotification(paste("Found", nrow(invalid_studies), "studies with zero participants in treatment or control group"), 
                           type = "warning", duration = 5)
          cleaned_data <- cleaned_data %>%
            filter((t_incident + t_noincident) > 0 & (c_incident + c_noincident) > 0)
        }
      }
      
      data_type_msg <- if(has_binary) "binary outcome" else "continuous outcome"
      showNotification(paste("Data processed successfully!", nrow(cleaned_data), "valid", data_type_msg, "studies ready for analysis"), 
                       type = "default", duration = 5)
      return(cleaned_data)
      
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error", duration = 10)
      return(NULL)
    })
  })
  
  # Output to control conditional panels
  output$dataUploaded <- reactive({
    return(!is.null(data()))
  })
  outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)
  
  observe({
    req(data())
    choices <- unique(data()$author)
    display_choices <- gsub("_", " ", choices)
    updateCheckboxGroupInput(session, "selected_interventions",
                             choices = setNames(choices, display_choices),
                             selected = choices)
    
    output$group_assignment <- renderUI({
      req(data())
      tagList(
        fluidRow(
          column(4, strong("Study")),
          column(2, strong("Group 1")),
          column(2, strong("Group 2")),
          column(2, strong("Group 3")),
          column(2, strong("Group 4"))
        ),
        lapply(choices, function(choice) {
          fluidRow(
            column(4, choice),
            column(2, checkboxInput(paste0("group1_", choice), NULL)),
            column(2, checkboxInput(paste0("group2_", choice), NULL)),
            column(2, checkboxInput(paste0("group3_", choice), NULL)),
            column(2, checkboxInput(paste0("group4_", choice), NULL))
          )
        })
      )
    })
  })
  
  filtered_data <- reactive({
    req(data(), input$selected_interventions)
    data() %>% filter(author %in% input$selected_interventions)
  })
  
  unique_studies <- reactive({
    req(filtered_data())
    filtered_data() %>% 
      group_by(name) %>% 
      summarise(Interventions_k = n()) %>%
      rename(Name = name)
  })
  
  meta_analysis <- reactive({
    req(filtered_data())
    
    if (filtered_data()$data_type[1] == "binary") {
      # Binary outcome meta-analysis
      if (input$multi_arm) {
        metabin(event.e = filtered_data()$t_incident, n.e = filtered_data()$t_incident + filtered_data()$t_noincident,
                event.c = filtered_data()$c_incident, n.c = filtered_data()$c_incident + filtered_data()$c_noincident,
                data = filtered_data(), sm = input$measure, studlab = filtered_data()$author, 
                cluster = filtered_data()$cluster_name)
      } else {
        metabin(event.e = filtered_data()$t_incident, n.e = filtered_data()$t_incident + filtered_data()$t_noincident,
                event.c = filtered_data()$c_incident, n.c = filtered_data()$c_incident + filtered_data()$c_noincident,
                data = filtered_data(), sm = input$measure, method = "Inverse", hakn = TRUE, studlab = filtered_data()$author)
      }
    } else {
      # Continuous outcome meta-analysis
      sm_method <- if (input$outcome_type == "continuous") input$smd_method else "SMD"
      if (input$multi_arm) {
        metacont(n.e = filtered_data()$t_n, mean.e = filtered_data()$t_mean, sd.e = filtered_data()$t_sd,
                 n.c = filtered_data()$c_n, mean.c = filtered_data()$c_mean, sd.c = filtered_data()$c_sd,
                 data = filtered_data(), sm = sm_method, studlab = filtered_data()$author,
                 cluster = filtered_data()$cluster_name)
      } else {
        metacont(n.e = filtered_data()$t_n, mean.e = filtered_data()$t_mean, sd.e = filtered_data()$t_sd,
                 n.c = filtered_data()$c_n, mean.c = filtered_data()$c_mean, sd.c = filtered_data()$c_sd,
                 data = filtered_data(), sm = sm_method, method = "Inverse", hakn = TRUE, studlab = filtered_data()$author)
      }
    }
  })
  
  fixed_effect_meta_analysis <- reactive({
    req(filtered_data())
    
    if (filtered_data()$data_type[1] == "binary") {
      if (input$multi_arm) {
        metabin(event.e = filtered_data()$t_incident, n.e = filtered_data()$t_incident + filtered_data()$t_noincident,
                event.c = filtered_data()$c_incident, n.c = filtered_data()$c_incident + filtered_data()$c_noincident,
                data = filtered_data(), sm = input$measure, method = "MH", fixed = TRUE, studlab = filtered_data()$author,
                cluster = filtered_data()$cluster_name)
      } else {
        metabin(event.e = filtered_data()$t_incident, n.e = filtered_data()$t_incident + filtered_data()$t_noincident,
                event.c = filtered_data()$c_incident, n.c = filtered_data()$c_incident + filtered_data()$c_noincident,
                data = filtered_data(), sm = input$measure, method = "MH", fixed = TRUE, studlab = filtered_data()$author)
      }
    } else {
      sm_method <- if (input$outcome_type == "continuous") input$smd_method else "SMD"
      if (input$multi_arm) {
        metacont(n.e = filtered_data()$t_n, mean.e = filtered_data()$t_mean, sd.e = filtered_data()$t_sd,
                 n.c = filtered_data()$c_n, mean.c = filtered_data()$c_mean, sd.c = filtered_data()$c_sd,
                 data = filtered_data(), sm = sm_method, fixed = TRUE, studlab = filtered_data()$author,
                 cluster = filtered_data()$cluster_name)
      } else {
        metacont(n.e = filtered_data()$t_n, mean.e = filtered_data()$t_mean, sd.e = filtered_data()$t_sd,
                 n.c = filtered_data()$c_n, mean.c = filtered_data()$c_mean, sd.c = filtered_data()$c_sd,
                 data = filtered_data(), sm = sm_method, fixed = TRUE, studlab = filtered_data()$author)
      }
    }
  })
  
  subgroup_data <- reactive({
    req(filtered_data(), data())
    filtered_data() %>% mutate(group = case_when(
      filtered_data()$author %in% unlist(lapply(unique(data()$author), function(author) if(input[[paste0("group1_", author)]]) author else NULL)) ~ "Group 1",
      filtered_data()$author %in% unlist(lapply(unique(data()$author), function(author) if(input[[paste0("group2_", author)]]) author else NULL)) ~ "Group 2",
      filtered_data()$author %in% unlist(lapply(unique(data()$author), function(author) if(input[[paste0("group3_", author)]]) author else NULL)) ~ "Group 3",
      filtered_data()$author %in% unlist(lapply(unique(data()$author), function(author) if(input[[paste0("group4_", author)]]) author else NULL)) ~ "Group 4",
      TRUE ~ "Not assigned"
    ))
  })
  
  subgroup_meta_analysis <- reactive({
    req(subgroup_data())
    assigned_groups <- subgroup_data() %>% filter(group != "Not assigned")
    if (nrow(assigned_groups) == 0) {
      return(NULL)
    }
    if (input$multi_arm) {
      metabin(event.e = assigned_groups$t_incident, n.e = assigned_groups$t_incident + assigned_groups$t_noincident,
              event.c = assigned_groups$c_incident, n.c = assigned_groups$c_incident + assigned_groups$c_noincident,
              data = assigned_groups, sm = input$measure, subgroup = assigned_groups$group, 
              studlab = assigned_groups$author, cluster = assigned_groups$cluster_name)
    } else {
      metabin(event.e = assigned_groups$t_incident, n.e = assigned_groups$t_incident + assigned_groups$t_noincident,
              event.c = assigned_groups$c_incident, n.c = assigned_groups$c_incident + assigned_groups$c_noincident,
              data = assigned_groups, sm = input$measure, method = "Inverse", hakn = TRUE, subgroup = assigned_groups$group, 
              studlab = assigned_groups$author)
    }
  })
  
  eggers_test <- reactive({
    req(meta_analysis())
    metabias(meta_analysis(), method.bias = "linreg", plotit = FALSE)
  })
  
  trim_and_fill <- reactive({
    req(meta_analysis())
    trimfill(meta_analysis())
  })
  
  output$data_summary <- renderPrint({
    req(data())
    cat("Dataset Summary\n")
    cat("===============\n")
    cat("Total studies:", nrow(data()), "\n")
    cat("Unique clusters:", length(unique(data()$cluster_name)), "\n")
    cat("Columns available:", paste(names(data()), collapse = ", "), "\n\n")
    
    # Show basic statistics
    cat("Treatment group:\n")
    cat("  Total events:", sum(data()$t_incident, na.rm = TRUE), "\n")
    cat("  Total participants:", sum(data()$t_incident + data()$t_noincident, na.rm = TRUE), "\n")
    
    cat("\nControl group:\n")
    cat("  Total events:", sum(data()$c_incident, na.rm = TRUE), "\n")
    cat("  Total participants:", sum(data()$c_incident + data()$c_noincident, na.rm = TRUE), "\n")
  })
  
  output$data_preview <- renderTable({
    req(data())
    if (data()$data_type[1] == "binary") {
      preview_data <- data() %>%
        select(author, 
               t_incident, t_noincident, 
               c_incident, c_noincident, 
               cluster_name) %>%
        mutate(
          t_total = t_incident + t_noincident,
          c_total = c_incident + c_noincident
        ) %>%
        head(10)
    } else {
      preview_data <- data() %>%
        select(author, 
               t_mean, t_sd, t_n,
               c_mean, c_sd, c_n,
               cluster_name) %>%
        head(10)
    }
    preview_data
  }, digits = 2)
  
  output$study_desc <- renderTable({
    req(unique_studies())
    unique_studies()
  })
  
  output$meta_summary <- renderPrint({
    req(meta_analysis())
    summary(meta_analysis())
  })
  
  output$forest_plot <- renderPlot({
    req(meta_analysis())
    data_type <- filtered_data()$data_type[1]
    
    if (data_type == "binary") {
      x_label <- if (input$measure == "RR") "Risk Ratio" else "Odds Ratio"
    } else {
      x_label <- switch(input$smd_method,
                        "SMD" = "Cohen's d",
                        "SMDH" = "Hedges' g", 
                        "GLASS" = "Glass's Δ",
                        "Standardized Mean Difference")
    }
    
    forest(meta_analysis(), xlab = x_label, col.study = "blue", col.square = "darkblue", col.diamond = "blue")
  }, height = 600)
  
  output$fixed_meta_summary <- renderPrint({
    req(fixed_effect_meta_analysis())
    summary(fixed_effect_meta_analysis())
  })
  
  output$fixed_forest_plot <- renderPlot({
    req(fixed_effect_meta_analysis())
    data_type <- filtered_data()$data_type[1]
    
    if (data_type == "binary") {
      x_label <- if (input$measure == "RR") "Risk Ratio" else "Odds Ratio"
    } else {
      x_label <- switch(input$smd_method,
                        "SMD" = "Cohen's d",
                        "SMDH" = "Hedges' g", 
                        "GLASS" = "Glass's Δ",
                        "Standardized Mean Difference")
    }
    
    forest(fixed_effect_meta_analysis(), xlab = x_label, col.study = "red", col.square = "darkred", col.diamond = "red")
  }, height = 600)
  
  output$funnel_plot <- renderPlot({
    req(meta_analysis())
    data_type <- filtered_data()$data_type[1]
    
    if (data_type == "binary") {
      sm_value <- input$measure
      x_label <- if (input$measure == "RR") "Risk Ratio" else "Odds Ratio"
    } else {
      sm_value <- input$smd_method
      x_label <- switch(input$smd_method,
                        "SMD" = "Cohen's d",
                        "SMDH" = "Hedges' g", 
                        "GLASS" = "Glass's Δ",
                        "Standardized Mean Difference")
    }
    
    funnel(meta_analysis(), sm = sm_value, xlab = x_label)
  }, height = 600)
  
  output$eggers_test <- renderPrint({
    req(eggers_test())
    test <- eggers_test()
    cat("Egger's Test for Funnel Plot Asymmetry\n")
    cat("=====================================\n")
    cat("Intercept: ", test$intercept, "\n")
    cat("Standard Error of Intercept: ", test$se.intercept, "\n")
    cat("P-Value: ", test$p.value, "\n")
  })
  
  output$trim_fill_summary <- renderPrint({
    req(trim_and_fill())
    summary(trim_and_fill())
  })
  
  output$trim_fill_plot <- renderPlot({
    req(trim_and_fill())
    data_type <- filtered_data()$data_type[1]
    
    if (data_type == "binary") {
      x_label <- if (input$measure == "RR") "Risk Ratio" else "Odds Ratio"
    } else {
      x_label <- switch(input$smd_method,
                        "SMD" = "Cohen's d",
                        "SMDH" = "Hedges' g", 
                        "GLASS" = "Glass's Δ",
                        "Standardized Mean Difference")
    }
    
    forest(trim_and_fill(), xlab = x_label, col.study = "green", col.square = "darkgreen", col.diamond = "green")
  }, height = 600)
  
  output$subgroup_meta_summary <- renderPrint({
    subgroup_analysis <- subgroup_meta_analysis()
    if (is.null(subgroup_analysis)) {
      cat("Assign studies to groups to conduct subgroup meta-analysis.")
    } else {
      summary(subgroup_analysis)
    }
  })
  
  output$subgroup_forest_plot <- renderPlot({
    subgroup_analysis <- subgroup_meta_analysis()
    if (!is.null(subgroup_analysis)) {
      data_type <- subgroup_data()$data_type[1]
      
      if (data_type == "binary") {
        x_label <- if (input$measure == "RR") "Risk Ratio" else "Odds Ratio"
      } else {
        x_label <- switch(input$smd_method,
                          "SMD" = "Cohen's d",
                          "SMDH" = "Hedges' g", 
                          "GLASS" = "Glass's Δ",
                          "Standardized Mean Difference")
      }
      
      forest(subgroup_analysis, xlab = x_label, col.study = "purple", col.square = "purple4", col.diamond = "purple4")
    }
  }, height = 800)
  
  output$subgroup_error_message <- renderPrint({
    subgroup_analysis <- subgroup_meta_analysis()
    if (is.null(subgroup_analysis)) {
      cat("Assign studies to groups to conduct subgroup meta-analysis.")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)