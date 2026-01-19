library(shiny)
library(dplyr)
library(writexl)
library(readr)

# Increase max upload size to 30MB
options(shiny.maxRequestSize = 30*1024^2)

# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("dPCR Rain Calculator (Between Clusters)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Upload Data"),
      fileInput("pos_file", "Analyzed Positives (.csv)", accept = ".csv"),
      fileInput("neg_file", "Analyzed Negatives (.csv)", accept = ".csv"),
      
      hr(),
      h4("2. Filter Settings"),
      # CHANGED: Filter by Well instead of Assay
      selectInput("well_select", "Select Well(s)", choices = NULL, multiple = TRUE),
      checkboxInput("exclude_ntc", "Exclude NTCs (Sample description 2)", value = TRUE),
      
      hr(),
      h4("3. Downloads"),
      downloadButton("download_summary", "Download Summary Table"),
      br(), br(),
      downloadButton("download_detailed", "Download Detailed Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Table", 
                 p("Statistics (Mean ± SD) based on selected wells:"),
                 tableOutput("summary_table")),
        tabPanel("Detailed Data", 
                 p("Preview (First 20 rows):"),
                 tableOutput("detailed_table"))
      )
    )
  )
)

# -------------------------------------------------------------------------
# SERVER
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Constants (Column Names) ---
  WELL_COL     <- "Well"
  SAMPLE1_COL  <- "Sample description 1"
  SAMPLE2_COL  <- "Sample description 2"
  TARGET_COL   <- "Target"
  POS_COL      <- "Positives"
  TOTAL_COL    <- "Accepted Droplets"
  
  # --- Reactive: Read Data ---
  raw_data <- reactive({
    req(input$pos_file, input$neg_file)
    
    # Read CSVs
    pos_df <- read.csv(input$pos_file$datapath, check.names = FALSE, stringsAsFactors = FALSE)
    neg_df <- read.csv(input$neg_file$datapath, check.names = FALSE, stringsAsFactors = FALSE)
    
    list(pos = pos_df, neg = neg_df)
  })
  
  # --- Observer: Update Well Choices ---
  observe({
    data <- raw_data()
    if (!is.null(data)) {
      # Extract unique Wells
      # We use mixedsort or standard sort to keep them in order (A01, A02...)
      wells <- unique(data$pos[[WELL_COL]])
      wells <- sort(wells)
      
      # Update the UI
      # Default: Select ALL wells initially
      updateSelectInput(session, "well_select", choices = wells, selected = wells)
    }
  })
  
  # --- Reactive: Process and Merge Data ---
  processed_data <- reactive({
    data <- raw_data()
    req(data)
    
    pos_df <- data$pos
    neg_df <- data$neg
    
    # 1. Filter by Well (User Selection)
    req(input$well_select)
    pos_df <- pos_df %>% filter(.data[[WELL_COL]] %in% input$well_select)
    neg_df <- neg_df %>% filter(.data[[WELL_COL]] %in% input$well_select)
    
    # 2. Filter out NTCs (User Checkbox)
    if (input$exclude_ntc) {
      pos_df <- pos_df %>% filter(!grepl("NTC", .data[[SAMPLE2_COL]], ignore.case = TRUE))
      neg_df <- neg_df %>% filter(!grepl("NTC", .data[[SAMPLE2_COL]], ignore.case = TRUE))
    }
    
    # 3. Rename columns for merging logic
    pos_clean <- pos_df %>%
      select(all_of(c(WELL_COL, SAMPLE1_COL, SAMPLE2_COL, TARGET_COL, POS_COL))) %>%
      rename(Pos_PosGate = all_of(POS_COL))
    
    neg_clean <- neg_df %>%
      select(all_of(c(WELL_COL, SAMPLE1_COL, SAMPLE2_COL, TARGET_COL, POS_COL, TOTAL_COL))) %>%
      rename(Pos_NegGate = all_of(POS_COL))
    
    # 4. Merge (Inner Join)
    merged <- inner_join(
      neg_clean, 
      pos_clean, 
      by = c(WELL_COL, SAMPLE1_COL, SAMPLE2_COL, TARGET_COL)
    )
    
    # 5. Calculate Rain
    # Formula: (NegGate - PosGate) / NegGate
    merged <- merged %>%
      mutate(Rain = (Pos_NegGate - Pos_PosGate) / Pos_NegGate) %>%
      filter(Pos_NegGate > 0) %>%
      arrange(.data[[SAMPLE1_COL]], .data[[SAMPLE2_COL]], .data[[WELL_COL]], .data[[TARGET_COL]])
    
    return(merged)
  })
  
  # --- Reactive: Summary Stats ---
  summary_stats <- reactive({
    df <- processed_data()
    req(df)
    
    if (nrow(df) == 0) return(NULL)
    
    # Calculate Mean/SD grouped by Assay and Target
    stats <- df %>%
      group_by(.data[[SAMPLE1_COL]], .data[[TARGET_COL]]) %>%
      summarise(
        Mean_Rain = mean(Rain, na.rm = TRUE),
        SD_Rain = sd(Rain, na.rm = TRUE),
        Count = n(), # Useful to see how many wells contributed
        .groups = "drop"
      ) %>%
      mutate(
        `Rain (mean ± SD)` = paste0(
          sprintf("%.1f", Mean_Rain * 100), "% ± ",
          sprintf("%.1f", SD_Rain * 100), "%"
        )
      ) %>%
      select(`Sample description 1` = .data[[SAMPLE1_COL]], Target = .data[[TARGET_COL]], `Rain (mean ± SD)`)
    
    # Add Grand Total Row
    grand_mean <- mean(df$Rain, na.rm = TRUE)
    grand_sd <- sd(df$Rain, na.rm = TRUE)
    
    total_row <- data.frame(
      `Sample description 1` = "Grand Total",
      Target = "All",
      `Rain (mean ± SD)` = paste0(sprintf("%.1f", grand_mean * 100), "% ± ", sprintf("%.1f", grand_sd * 100), "%"),
      check.names = FALSE
    )
    
    final_stats <- bind_rows(stats, total_row)
    
    return(final_stats)
  })
  
  # --- Outputs ---
  
  output$summary_table <- renderTable({
    summary_stats()
  })
  
  output$detailed_table <- renderTable({
    head(processed_data(), 20)
  })
  
  # --- Download Handlers ---
  
  output$download_summary <- downloadHandler(
    filename = function() {
      paste0("Rain_Summary_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(summary_stats(), file)
    }
  )
  
  output$download_detailed <- downloadHandler(
    filename = function() {
      paste0("Rain_Detailed_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(processed_data(), file)
    }
  )
}

shinyApp(ui = ui, server = server)