# Helper function for UI tooltips (unchanged)
labelWithTooltip <- function(labelText, tooltipText) {
  tags$label(
    labelText,
    tags$span(
      class = "tooltip-container",
      shiny::icon("info-circle", style = "margin-left: 5px; color: var(--bs-primary); cursor: help;"), # Adjusted color
      tags$span(class = "tooltip-text", tooltipText)
    )
  )
}

mod_dynamic_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML(paste0("
        /* --- General Layout & Panel Styling (Unchanged) --- */
        #", ns("plot_container"), " {
          height: calc(100vh - 80px); display: flex; flex-direction: row;
          align-items: stretch; padding: 20px; gap: 20px;
        }
        /* UPDATED: Use CSS variables for backgrounds and text color */
        #", ns("plot_sidebar"), ", #", ns("plot_main_panel"), " {
          background-color: var(--bs-card-bg, rgba(255, 255, 255, 0.9)); 
          color: var(--bs-body-color, #333);
          border: 1px solid var(--bs-border-color, #ccc);
          border-radius: 10px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.15); padding: 25px; overflow-y: auto;
        }
        #", ns("generate_plot"), " { margin-top: 20px; width: 100%; }

        /* --- 2. ADD THE CSS FOR THE CUSTOM TOOLTIPS --- */
        .tooltip-container {
          position: relative;
          display: inline-block;
        }
        .tooltip-text {
          visibility: hidden; width: 180px; 
          background-color: var(--bs-body-color, #333); /* Adaptive dark/light */
          color: var(--bs-body-bg, #fff);              /* Adaptive text */
          text-align: center; border-radius: 6px; padding: 5px 10px;
          position: absolute; z-index: 10; bottom: 125%; left: 50%;
          margin-left: -90px; opacity: 0; transition: opacity 0.3s; font-weight: normal;
        }
        .tooltip-container:hover .tooltip-text {
          visibility: visible;
          opacity: 1;
        }
      ")))
    ),
    
    div(
      id = ns("plot_container"),
      
      column(
        width = 4,
        id = ns("plot_sidebar"),
        
        selectInput(ns("plot_type"), 
                    label = labelWithTooltip("Select Plot Type", "Choose the type of chart to display."),
                    choices = c("Scatter Plot", "Bar Chart"),
                    width = "100%"), 
        
        selectInput(ns("x_col"), 
                    label = labelWithTooltip("Select X Column (Category)", "For bar charts, this is the categorical axis. For other plots, this is the numeric x-axis."), 
                    choices = NULL,
                    width = "100%"), 
        
        selectInput(ns("y_col"), 
                    label = labelWithTooltip("Select Y Column (Value)", "This is the numeric value axis for all plot types."), 
                    choices = NULL,
                    width = "100%"), 
        
        actionButton(ns("generate_plot"), "Generate Plot", class = "btn-primary")
      ),
      
      column(
        width = 8,
        id = ns("plot_main_panel"),
        plotlyOutput(ns("dynamic_plot"), height = "100%")
      )
    )
  )
}

mod_dynamic_plot_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    
<<<<<<< Updated upstream
    # This observer for updating dropdowns is correct and does not cause the issue.
=======
    # 1. Input Updates (Unchanged)
>>>>>>> Stashed changes
    observe({
      df <- shared_data()
      req(df, input$plot_type)
      
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
      
      if (input$plot_type == "Bar Chart") {
        updateSelectInput(session, "x_col", choices = categorical_cols)
        updateSelectInput(session, "y_col", choices = numeric_cols)
      } else {
        updateSelectInput(session, "x_col", choices = numeric_cols)
        updateSelectInput(session, "y_col", choices = numeric_cols)
      }
    })
    
    # 1. This reactiveVal will store our final plot. It starts NULL.
    plot_to_render <- reactiveVal(NULL)
    
<<<<<<< Updated upstream
    # 2. This observeEvent listens ONLY to the button click. This is the sole trigger.
    observeEvent(input$generate_plot, {
      
      # 3. Immediately isolate all required input values into local variables.
      # This is the most crucial step. The rest of the code will only use
      # these non-reactive variables, completely breaking the reactive link.
=======
    # 2. Main Plot Logic
    # Trigger on Button Click OR Theme Change
    observeEvent(c(input$generate_plot, bslib::bs_current_theme()), {
      
      # Don't run if user hasn't clicked generate at least once
      req(input$generate_plot > 0)
      
>>>>>>> Stashed changes
      current_plot_type <- isolate(input$plot_type)
      current_x_col <- isolate(input$x_col)
      current_y_col <- isolate(input$y_col)
      df <- isolate(shared_data())
      
      # Ensure we actually have valid values before proceeding.
      req(df, current_x_col, current_y_col, current_plot_type)
      
<<<<<<< Updated upstream
      # 4. Perform all plotting logic using ONLY the local variables.
=======
      # --- DYNAMIC COLOR FETCHING ---
      # We ask bslib for the current theme's 'primary' color and text color
      # If bslib can't find them, we set fallback defaults
      theme_vars <- bslib::bs_get_variables(
        bslib::bs_current_theme(), 
        c("primary", "body-color", "border-color")
      )
      
      # Use fetched colors or fallbacks
      accent_color <- if (!is.null(theme_vars[["primary"]])) theme_vars[["primary"]] else "#007bff"
      text_color   <- if (!is.null(theme_vars[["body-color"]])) theme_vars[["body-color"]] else "#333333"
      grid_color   <- if (!is.null(theme_vars[["border-color"]])) theme_vars[["border-color"]] else "rgba(128, 128, 128, 0.3)"
      
>>>>>>> Stashed changes
      p <- switch(
        current_plot_type,
        "Scatter Plot" = {
          plot_ly(df, x = ~get(current_x_col), y = ~get(current_y_col),
                  type = 'scatter', mode = 'markers',
                  # Apply dynamic accent color
                  marker = list(color = accent_color, size = 8))
        },
        "Bar Chart" = {
          df_agg <- df %>%
            group_by(!!sym(current_x_col)) %>%
            summarise(agg_y = mean(!!sym(current_y_col), na.rm = TRUE), .groups = 'drop') %>%
            rename(y_val = agg_y, x_cat = !!sym(current_x_col))
          
          plot_ly(df_agg, x = ~x_cat, y = ~y_val,
                  type = 'bar', 
                  # Apply dynamic accent color
                  marker = list(color = accent_color))
        }
      )
      
<<<<<<< Updated upstream
      # Also use the local, non-reactive variables for the layout.
=======
      # Apply dynamic Layout colors
>>>>>>> Stashed changes
      p <- p %>% layout(
        paper_bgcolor = 'rgba(0,0,0,0)', # Transparent so it shows the card background
        plot_bgcolor = 'rgba(0,0,0,0)',
        xaxis = list(title = current_x_col, color = text_color, gridcolor = grid_color),
        yaxis = list(title = current_y_col, color = text_color, gridcolor = grid_color),
        font = list(color = text_color)
      )
      
      # 5. Store the resulting "sanitized" plot in our reactiveVal.
      plot_to_render(p)
      
<<<<<<< Updated upstream
    }, ignoreInit = TRUE) # End of observeEvent
    
    
    # 6. The output rendering logic ONLY depends on our reactiveVal.
    # It has no knowledge of the input dropdowns or the button.
=======
    }) # removed ignoreInit so theme updates trigger it
    
>>>>>>> Stashed changes
    output$dynamic_plot <- renderPlotly({
      
      # If the reactiveVal is NULL (button never clicked), show the placeholder.
      if (is.null(plot_to_render())) {
        # Get current text color for the empty state message
        theme_vars <- bslib::bs_get_variables(bslib::bs_current_theme(), "body-color")
        txt_col <- if (!is.null(theme_vars[["body-color"]])) theme_vars[["body-color"]] else "#555"
        
        return(
          plot_ly() %>%
            layout(
              paper_bgcolor = 'rgba(0,0,0,0)',
              plot_bgcolor = 'rgba(0,0,0,0)',
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              annotations = list(
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                text = "Please select your options and click 'Generate Plot'",
                showarrow = FALSE, font = list(size = 16, color = txt_col)
              )
            )
        )
      }
      
      # Otherwise, render the plot that is stored inside the reactiveVal.
      plot_to_render()
    })
    
  })
}