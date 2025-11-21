library(shiny)
library(data.table)
library(RCurl)
library(randomForest)
library(plotly)
library(readr)
library(bslib)    
library(shinyjs)
library(rlang)
library(dplyr)

library(xgboost)
library(Matrix)
library(caret)
library(jsonlite)
<<<<<<< Updated upstream

=======
library(rstudioapi)
library(shinyBS)
library(thematic) 

setwd(dirname(getActiveDocumentContext()$path))
>>>>>>> Stashed changes

# --- Source Modules ---
source("modules/mod_dynamic_plot.R")
source("modules/mod_prediction_panel.R")

# --- CORRECTED: Resource path now correctly points to your image folder ---
shiny::addResourcePath(prefix = 'detailed_images', directoryPath = 'detailed_images')

# Activates automatic plot adjustment (ggplot/base)
thematic_shiny(font = "auto")

<<<<<<< Updated upstream
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage("Auto Market",
             selected = "Prediction Panel", # Start on the prediction panel for easier testing
             tabPanel("Dynamic Plot", mod_dynamic_plot_ui("plot")),
             tabPanel("Prediction Panel", mod_prediction_panel_ui("predict"))
  )
=======
# --- 1. DEFINITION OF COLOR PALETTE (16 Colors) ---
color_palette <- c(
  "Standard (Theme Default)" = "default",
  "Black" = "#000000",
  "White" = "#FFFFFF",
  "Blue (Primary)" = "#007bff",
  "Indigo" = "#6610f2",
  "Purple" = "#6f42c1",
  "Pink" = "#e83e8c",
  "Red (Danger)" = "#dc3545",
  "Orange" = "#fd7e14",
  "Yellow (Warning)" = "#ffc107",
  "Green (Success)" = "#28a745",
  "Teal" = "#20c997",
  "Cyan (Info)" = "#17a2b8",
  "Gray" = "#6c757d",
  "Dark Gray" = "#343a40",
  "Light Gray" = "#f8f9fa",
  "Navy" = "#001f3f"
>>>>>>> Stashed changes
)

ui <- navbarPage(
  title = "Auto Market",
  
  # Start Theme (Bootstrap 5 is required for bslib)
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  
  selected = "Prediction Panel", 
  
  tabPanel("Dynamic Plot", mod_dynamic_plot_ui("plot")),
  tabPanel("Prediction Panel", mod_prediction_panel_ui("predict")),
  # --- SETTINGS TAB WITH COLOR SELECTION ---
  tabPanel("Settings",
           fluidRow(
             column(4,
                    wellPanel(
                      h4("Global Design Settings"),
                      
                      # 1. Base Theme Selection
                      selectInput("theme_selector", "1. Base Theme:",
                                  choices = c("Light Standard" = "cerulean",
                                              "Dark & Cool" = "darkly",
                                              "Modern & Flat" = "flatly",
                                              "Grey & Professional" = "sandstone",
                                              "High Contrast" = "cyborg",
                                              "Orange & Black" = "solar"),
                                  selected = "cerulean"
                      ),
                      hr(),
                      h5("2. Override Colors"),
                      helpText("Select specific colors to override the theme defaults."),
                      
                      # 2. Primary Color (Buttons, Bars, Links)
                      selectInput("col_primary", "Primary Color (Bars & Buttons):",
                                  choices = color_palette, selected = "default"),
                      
                      # 3. Body/Text Color
                      selectInput("col_text", "Text Color (Body):",
                                  choices = color_palette, selected = "default"),
                      
                      # 4. Background Color
                      selectInput("col_bg", "Background Color:",
                                  choices = color_palette, selected = "default"),
                      
                      # 5. Border Color (Box Frames)
                      selectInput("col_border", "Border Color:",
                                  choices = color_palette, selected = "default")
                    )
             ),
             column(8,
                    h4("Preview Area"),
                    p("Change the settings on the left to see how the app changes."),
                    actionButton("dummy_btn", "Primary Button Example", class = "btn-primary"),
                    br(), br(),
                    div(style = "border: 1px solid; padding: 20px; border-radius: 5px;", 
                        "This box shows the current border and text color settings.")
             )
           )
  )
)

server <- function(input, output, session) {
  
  data_file_path <- "detailed_car_sales_data_train.csv"
  
  if (!file.exists(data_file_path)) {
    # Fallback if file is missing
    shared_data <- reactiveVal(data.frame(
      manufacturer = c("Audi", "BMW"), 
      price = c(20000, 30000),
      year_of_manufacture = c(2015, 2018)
    ))
  } else {
    shared_data <- reactiveVal(read_csv(data_file_path))
  }
  
  # --- THEME UPDATE LOGIC ---
  observe({
    
    # 1. Create the base theme
    current_theme <- bs_theme(version = 5, preset = input$theme_selector)
    
    # 2. Collect color overrides
    my_primary <- if(input$col_primary != "default") input$col_primary else NULL
    my_fg      <- if(input$col_text != "default")    input$col_text    else NULL
    my_bg      <- if(input$col_bg != "default")      input$col_bg      else NULL
    my_border  <- if(input$col_border != "default")  input$col_border  else NULL
    
    # --- FIX START: Handle missing FG or BG ---
    # bslib requires both FG and BG to be present if one of them is changed.
    
    # If Background is set but Text is not -> Default Text to Black
    if (!is.null(my_bg) && is.null(my_fg)) {
      my_fg <- "#000000" 
    }
    
    # If Text is set but Background is not -> Default Background to White
    if (!is.null(my_fg) && is.null(my_bg)) {
      my_bg <- "#FFFFFF"
    }
    # --- FIX END ---
    
    # 3. Update the theme with the colors
    current_theme <- bs_theme_update(
      current_theme,
      primary = my_primary,
      fg = my_fg,
      bg = my_bg,
      "border-color" = my_border
    )
    
    # 4. Apply the theme
    session$setCurrentTheme(current_theme)
  })
  
  mod_dynamic_plot_server("plot", shared_data)
  mod_prediction_panel_server("predict", shared_data)
}

shinyApp(ui, server)