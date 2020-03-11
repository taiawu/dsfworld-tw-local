# 4_analyze/app.R
library(quantmod) # contains the findValleys function, which maybe we should just extract and put verbatim in a source file instead of loading this whole thing...?
library(minpack.lm) # contains the nlsLM function, which we use for our fitting
library(modelr) # used in both the data modeling and the analysis model fitting 
library(SciViews) # contains the ln function used in the data modeling
library(signal) # contains the savistky golay filter (savgolfilt), used to generate the first derivative data in both data modeling and analysis model fitting  
named_mods <- c("s1_pred", "s1_d_pred", "s2_pred", "s2_d_pred") %>%
    set_names("Model 1", "Model 2", "Model 3", "Model 4")

library(shinyBS) # drop-down panels
library(tidyverse) #  handling data structures and plotting

source("support_scripts/upload_formatters.R")
source("support_scripts/layout_handling.R")
source("support_scripts/plotting.R")
source("support_scripts/analysis.R")

library(shinyalert) # pop-up error messages
library(shinycssloaders) # spinning plot loading icon
library(rhandsontable) # user-interactive tables 
library(shiny) # for shiny web-apps 

ui <- navbarPage("",
                 #tabPanel(p("to data analysis", style = "font-family: 'Avenir Next'; font-size: 20px; color: grey",align = "right")),
                 # Data Analysis --------------------------------------------------------------------------------
                 tabPanel(p("to data analysis", style = "font-family: 'Avenir Next'; font-size: 20px; color: grey",align = "center"), value = "data_analysis_mother", # end tab panel (tabset, div, main still remaining)
                          tabsetPanel(id = "inTabset_analysis", # tabset for all analysis sub-tabs
# analyze and visualize ui --------------------------- 
                                      tabPanel(p("2 | analyze and visualize", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"), value = "analysis_tab",
                                               tags$head( # det the slider aesthetic
                                                   tags$style(
                                                       ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: grey; border-color: transparent;}",
                                                       ".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: grey; border-color: transparent;}"
                                                   )
                                               ),
                                               sidebarLayout(
                                                   sidebarPanel(
                                                       bsCollapse(id = "plot_aes", open = "Panel 2",
                                                                  bsCollapsePanel(p("Set plate layout and replicates", style = "font-family: 'Avenir Next'; font-size: 16px; color: black",align = "center"), 
                                                                                  bsCollapsePanel(p("Method 1 - upload layout", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"), 
                                                                                                  p("Our favorite approach to DSF data analysis.", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                                  p("Use the template below to create a layout file for your experiment. Each plate in the layout file defines a new experimental variable (e.g. compound, pH, concentration), with the varible name provided in the first column of the layout file. You can define any number of variables by adding additional plates to the layout file. Using this method, data can be visualized by user-defined variables (e.g. color by concentration).", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                                  p("Layouts are connected to data by well name, so your data must have a 'well' column to use this feature.", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                                  p("For more information, see the instructions tab.", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                                  downloadButton("sample_layout_file", p("Download layout template", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center")), #
                                                                                                  p("...", style = "font-family: 'Avenir Next'; font-size: 12px; color: white",align = "center"),
                                                                                                  fileInput("layout_file", p("Upload your csv layout file", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                                            accept = c(
                                                                                                                "text/csv",
                                                                                                                "text/comma-separated-values,text/plain",
                                                                                                                ".csv")
                                                                                                  )),
                                                                                  p("...", style = "font-family: 'Avenir Next'; font-size: 15px; color: white",align = "center"),
                                                                                  p("Method 2 - edit manually", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"),
                                                                                  rHandsontableOutput("r_table"),
                                                                                  p("...", style = "font-family: 'Avenir Next'; font-size: 15px; color: white",align = "center"),
                                                                                  uiOutput("handson_update_button"),
                                                                                  #actionButton("submit_handson_names", p("Update names", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center")),
                                                                                  style = "default")
                                                       ),
                                                       bsCollapse(id = "plot_aes", open = "Panel 2",
                                                                  bsCollapsePanel(p("Make plots", style = "font-family: 'Avenir Next'; font-size: 16px; color: black", align = "center"),
                                                                                  # update_plot
                                                                                  uiOutput("trigger_df_1"),
                                                                                  uiOutput("update_plot"),
                                                                                  radioButtons("facet", "Make sub-plots",
                                                                                               c("Single plot" = "none",
                                                                                                 "Subset by one variable" = "wrap",
                                                                                                 "Subset by two variables" = "grid")),
                                                                                  uiOutput("wrap_by"), # reactive selector for the graph?
                                                                                  uiOutput("grid_rows"),
                                                                                  uiOutput("grid_cols"),
                                                                                  uiOutput("color_by"),
                                                                                  checkboxInput("use_linetypes", "Vary line types", FALSE),
                                                                                  uiOutput("linetype_by"),
                                                                                  uiOutput("mean_or_each"),
                                                                                  radioButtons("mean_or_each", "Show mean?",
                                                                                               c("Each replicate" = "each",
                                                                                                 "Mean" = "mean")),
                                                                                  
                                                                                  radioButtons("fix_free", "Equalize y axes?",
                                                                                               c("Equal" = "fixed",
                                                                                                 "Independent" = "free")),
                                                                                  bsCollapsePanel(h5("Edit plot labels"),
                                                                                                  textInput("plot_title", "Plot title", "Raw RFU Data"),
                                                                                                  textInput("legend_title", "Legend title", "Condition"),
                                                                                                  uiOutput("linetype_title"),
                                                                                                  textInput("y_title", "y-axis title", "RFU"),
                                                                                                  textInput("x_title", "x-axis title", "Temperature (ºC)"),
                                                                                                  numericInput("text_size", "Plot text size", 10, min = 4, max = 20),
                                                                                                  checkboxInput("hide_legend", "Hide legend", FALSE)),
                                                                                  style = "default")
                                                       ),
# tm determination ui --------------------------- 
                                                       bsCollapse(id = "tm_table", open = "Panel 2",
                                                                  bsCollapsePanel(p("Find apparent Tms", style = "font-family: 'Avenir Next'; font-size: 16px; color: black",align = "center"),
                                                                                  bsCollapsePanel(p("By dRFU", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                                                                  checkboxInput("show_Tm_dRFU", "Show Tm' on plot", FALSE),
                                                                                                  checkboxInput("show_Tm_dRFU_colors", "Apply colors  to Tm'", FALSE),
                                                                                                  DT::dataTableOutput("tm_table_render"), #style = "height:400px;"
                                                                                                  style = "default"
                                                                                  ),
                                                                                  
                                                                                  bsCollapsePanel(p("By sigmoid fitting", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                                                                  p("Select the models you would like to fit to your data below.", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                                  splitLayout(cellWidths = c("25%", "25%", "25%", "25%"), 
                                                                                                              p("Model 1", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center"),
                                                                                                              p("Model 2", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center"),
                                                                                                              p("Model 3", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center"),
                                                                                                              p("Model 4", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center")
                                                                                                  ),
                                                                                                  
                                                                                                  splitLayout(cellWidths = c("25%", "25%", "25%", "25%"), 
                                                                                                              bsButton("s1", label = tags$img(src = "s1_v1.png",
                                                                                                                                              width = "100%"),
                                                                                                                       block = TRUE, type = "toggle", value = TRUE), 
                                                                                                              bsButton("s1_d", label = tags$img(src = "s1_id_v1.png",
                                                                                                                                                width = "100%"),
                                                                                                                       block = TRUE, type = "toggle", value = FALSE),
                                                                                                              bsButton("s2", label = tags$img(src = "s2_v1.png",
                                                                                                                                              width = "100%"),
                                                                                                                       block = TRUE, type = "toggle", value = FALSE), 
                                                                                                              bsButton("s2_d", label = tags$img(src = "s2_id_v1.png",
                                                                                                                                                width = "100%"),
                                                                                                                       block = TRUE, type = "toggle", value = FALSE)
                                                                                                  ),
                                                                                                  
                                                                                                  bsTooltip("s1", "Model 1: One sigmoid with decay",
                                                                                                            "right", options = list(container = "body")),
                                                                                                  bsTooltip("s1_d", "Model 2: One sigmoid with decay and starting-temperature fluorescence",
                                                                                                            "right", options = list(container = "body")),
                                                                                                  bsTooltip("s2", "Model 3: Two sigmoids with decays",
                                                                                                            "right", options = list(container = "body")),
                                                                                                  bsTooltip("s2_d", "Model 4: Two sigmoids with decays and starting-temperature fluorescence",
                                                                                                            "right", options = list(container = "body")),
                                                                                                  p(" ", style = "font-family: 'Avenir Next'; font-size: 8px; color: black",align = "center"),
                                                                                                
                                                                                                  DT::dataTableOutput("tm_table_render_models"), #style = "height:400px;"
                                                                                                  p(" ", style = "font-family: 'Avenir Next'; font-size: 8px; color: black",align = "center"),
                                                                                                  bsCollapsePanel(p("Add Tm' and fits to plots", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                                                 
                                                                                                                  uiOutput("choose_model_tm"),
                                                                                                                  checkboxInput("show_Tm_mods", "Show Tm' on plots", FALSE),
                                                                                                                  checkboxInput("show_fit", "Show fits", FALSE),
                                                                                                                  checkboxInput("show_fit_comps", "Show fit components", FALSE),
                                                                                                                  p("Fits and Tm' are ploted in black by default", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left") %>% strong(),
                                                                                                                  checkboxInput("show_Tm_mods_colors", "Apply colors  to Tm'", FALSE),
                                                                                                                  checkboxInput("show_fit_comps_colors", "Apply colors  to fit lines", FALSE),
                                                                                                                  uiOutput("update_model_plots")
                                                                                                      
                                                                                                  ),
                                                                                                  
                                                                                                  
                                                                                                  bsCollapsePanel(p("Improve fits", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                                                  p("Fit data only in the following temperature range", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),
                                                                                                           
                                                                                                                  uiOutput("trim_ends"),
                                                                                                                  p("Update initial model  guesses", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),
                                                                                                                  p("To fit models to data, DSFworld begins by guessing initial values for model parameters, and then improves upon them until the fit is optimized. If fits have failed, or describe the data poorly, manually defining more accurate starting guesses for the Tm' below can help.", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center"),
                                                                                                                 
                                                                                                                  uiOutput("update_fits_button")
                                                                                                  ),
                                                                                                  bsCollapsePanel(p("Select the best model for each condition", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                                                  p("Click 'Plot model comparision' below to display the four models side-by-side. Use this plot to select the best model for each condition. ", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                                                  uiOutput("show_BIC_plot"),
                                                                                                                  
                                                                                                                  p("", style = "font-family: 'Avenir Next'; font-size: 8px; color: black",align = "center"),
                                                                                                                  p("For each condition, the model with the lowest Bayesian Information Criterion (BIC) is selected by default, to maximize model quality without over-fitting (see 'About the analysis').", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center"),
                                                                                                                  
                                                                                                                  p("", style = "font-family: 'Avenir Next'; font-size: 8px; color: black",align = "center"),
                                                                                                                  p("To change model selections", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),
                                                                                                                  p("Select a model for each condition manually by double-clicking on the desired model in the plot. ", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center"),
                                                                                                                  
                                                                                                                  p("", style = "font-family: 'Avenir Next'; font-size: 8px; color: black",align = "center"),
                                                                                                                  
                                                                                                                  p("Or, to change multiple model selections at a time, edit the table below and press 'Apply model selections from table'. ", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center"),
                                                                                                                  uiOutput("apply_handson_models")
                                                                                                               
                                                                                                                  
                                                                                                  )
                                                                                  ), style = "default"
                                                                  )
                                                       )
                                                       
                                                   ),
                                                   
                                                   mainPanel(
                                                       wellPanel(
                                                           tags$script("$(document).on('shiny:connected', function(event) {
                                                                                var myWidth = $(window).width();
                                                                                Shiny.onInputChange('shiny_width',myWidth)
                                                                                
                                                                                });"),
                                                           
                                                           tags$script("$(document).on('shiny:connected', function(event) {
                                                                                var myHeight = $(window).height();
                                                                                Shiny.onInputChange('shiny_height',myHeight)
                                                                                
                                                                                });"),
                                                           
                                                           id = "facet_plot_re_examined",
                                                           
                                                           tags$style(HTML('#q1 {margin-top: 30px}')),
                                                           splitLayout(cellWidths = c("20%", "80%"), 
                                                                       plotDownloadUI("plot1"), 
                                                                       textInput("plot_download_name", "Downloaded plot name", value = "dsfworld_plot")
                                                           ),
                                                           plotOutput("plot", height = "auto") %>% withSpinner(color="#525252"), style = ("overflow-y:scroll; max-height: 600px") 
                                                       ))
                                               )
                                      )
                          )) # end tabset Panel (contains all "analysis sub-panels)
                 
) # end 

# Define server logic required to draw a histogram
server <- function(session, input, output) {
    values <- reactiveValues() 
    values$data_raw <- readRDS("values_df_with_layout.rds")
    values$df <- readRDS("values_df_with_layout.rds")
    values$df_1 <- readRDS("values_df_with_layout.rds")
    values$plot_chosen <- "initial"
    
# data layout and handling server --------------------------- 
 
    # GUI elements
    output$handson_update_button <- renderUI({ # for layout
        req(values$df)
        actionButton("submit_handson_names", p("Update names from manual table (above)", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),  width = '100%')
    })

    # data handling
    layout <- reactive({
        req(input$layout_file)
        tryCatch({
            make_layout(input$layout_file$datapath) %>% # all columns are characters
                add_standardized_wells()
            
        }, error = function(e) {
            shinyalert("Please ensure that your layout is formatted correctly", "Layout file should be uploaded asa  UTF-8 csv. A layout template can be downloaded below.")
        })
    })
    
    observeEvent( layout(), { 
        req(layout()) # don't do this if the layout doesn't exist
        tryCatch({
            values$df <- join_layout_nest(values$df_1, layout() ) 
            
        }, error = function(e) {
            shinyalert("Please ensure that your layout is formatted correctly", "Layout file should be uploaded asa  UTF-8 csv. A layout template can be downloaded below.")
        })
        
    })
    
    observeEvent(input$submit_handson_names, { # when r_table is updated
        req(input$r_table)
        
        values$df<- hot_to_r(input$r_table) %>% # update the layout
            as_tibble() %>%
            ensure_standardized_wells() %>%
            join_layout_nest( values$df_1, . )
        
        write_rds(values$df, "values_df_with_layout.rds")
        
        values$r_table_update <- values$r_table_update + 1 # trigger the re-rendering of the r_table
        
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    output$r_table <- renderRHandsontable({
        
        tryCatch({
            req(values$df)
            trigger <- values$r_table_update
            
            if (is.null(input$layout_file) == FALSE) { # if there is a layout file
                layout_vars <- names(layout())[!c(names(layout()) %in% c("well_", "well_f_", "row_", "col_", "row", "column"))]
                
                handson_df <- values$df %>%
                    select( one_of( layout_vars )) # this will always include "condition"
                
            } else { # if no layout file
                handson_df <- values$df %>%
                    select(well, condition)
            }
            rhandsontable( handson_df, height = 200, useTypes = TRUE, stretch = "all") %>% hot_col(c("well"), readOnly = TRUE)
            
        }, error = function(e) {
            shinyalert("Please ensure that your layout is formatted correctly", "Layout file should be uploaded asa  UTF-8 csv. A layout template can be downloaded below.")
        })
        
        
    })

# plotting server  ---------------------------    

    output$update_plot <- renderUI({
        req(values$df)
        actionButton("update_plot", p("Update plot", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),  width = '100%')
    })
    
    # eval selections, to pass to the plotting function 
    output$wrap_by <- renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
        req(values$df)
        if (input$facet == "none" | input$facet == "grid") return(NULL)
        varSelectInput("wrap_by", label = "Sub-plot by",  # as select input
                       data = values$df %>% get_layout_vars()
        ) # this is able to take a reactive value
    })
    
    output$grid_rows <- renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
        req(values$df)
        if (input$facet == "none" | input$facet == "wrap") return(NULL)
        varSelectInput("grid_rows", label = "Sub-plot grid, rows",  # as select input
                       data = values$df %>% get_layout_vars()
        ) # this is able to take a reactive value
    })
    
    output$grid_cols <- renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
        req(values$df)
        req(input$grid_rows)
        if (input$facet == "none" | input$facet == "wrap") return(NULL)
        
        tryCatch({
            col_data <- values$df %>% 
                get_layout_vars() %>%
                select(-!!input$grid_rows)
        }, error = function(e) {
            col_data <- NULL
        })
        
        varSelectInput("grid_cols", label = "Sub-plot grid, columns",  # as select input
                       data = col_data,
                       selected = "- ")
    }) 
    
    output$color_by <-renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
        req(values$df)
        varSelectInput("color_by", label = "Color",  # as select input
                       data = values$df %>% get_layout_vars(),
                       selected = "-"
        ) # this is able to take a reactive value
    })  
    
    output$linetype_by <-renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
        req(values$df)
        if (input$use_linetypes == FALSE) return(NULL)
        varSelectInput("linetype_by", label = "Line types",  # as select input
                       data = values$df %>% get_layout_vars(),
                       selected = "-"
        ) # this is able to take a reactive value
    })
    
    output$linetype_title <-renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
        req(values$df)
        if (input$use_linetypes == FALSE) return(NULL)
        textInput("linetype_title", "Line type legend title", "Condition2")
    })
    
    plot_title_d <- reactive({input$plot_title})
    plot_legend_d <- reactive({input$legend_title})
    plot_legend_linetype <- reactive({ input$linetype_title })
    
    legend_position <- reactive({ 
        if (input$hide_legend == TRUE ) { legend_pos <- "none"
        } else { legend_pos <- "right"}
        legend_pos})
    
    #end eval selections, to pass to the plotting function 
    
    # update and re-render the plot only when the update-plot button is clicked!
    plot_initial <- eventReactive( values$data_raw,  { # only when the "update plot" button is clicked, update the plot 
        print("plot changed")
        req(values$df) # only render the plot if there is data
        
        unnest(values$df) %>%
            ggplot(aes(x = Temperature, y = value, group = well)) +
            geom_line(size = 0.5, alpha = 0.7) +
            labs(title = "Raw RFU Data", x = "Temperature (ºC)", y = "RFU") +
            theme_bw() +
            dsfworld_default +
            theme(  text = element_text(size = 10*1.25),
                    axis.text = element_text(size = 10),
                    plot.title = element_text(lineheight=.8, face="bold", size = 10*1.5)) 
    })
    
    plot_updated <- eventReactive( input$update_plot,  { # only when the "update plot" button is clicked, update the plot 
        print("plot changed")
        req(values$df) # only render the plot if there is data
        
        df_RFU_plot <- unnest(values$df) %>%
            plyr::mutate("-" = rep("", nrow(.))) %>%
            plyr::mutate("- " = rep("", nrow(.)))
        
        facet_func(df = df_RFU_plot,# reacts to the appearance and changes to the dataframe, to the uploading of format files
                   mean_or_each = input$mean_or_each,
                   color_by = !!input$color_by,
                   linetype_by = !!input$linetype_by,
                   use_linetypes = input$use_linetypes,
                   facet = input$facet,
                   facet_by = !!input$wrap_by,
                   facet_rows = !!input$grid_rows,
                   facet_cols = !!input$grid_cols,
                   set_title = plot_title_d(),
                   legend_title = plot_legend_d(),
                   legend_linetype_title = plot_legend_linetype(),
                   fix_free = input$fix_free,
                   text_size = input$text_size,
                   legend_position = legend_position(),
                   x_title = input$x_title,
                   y_title = input$y_title)
    })
    
    observeEvent(values$data_raw, { values$plot_chosen <- "initial"  })    
    observeEvent(input$update_plot, { values$plot_chosen <- "updated"  })
    observeEvent(values$show_model_plot, { values$show_model_plot <- "model"  })  
    
    chosen_plot <- reactive({
        if (values$plot_chosen == "updated") { # TRUE when the "update plot" button was clicked more recently than new data uploads or "show model plot"
            plot_updated()
        } else if (values$plot_chosen == "model") { # TRUE when the "show model plot" button was clicked more recently than new data uploads or "update model plot"
            plot2()
        } else { # if its the initial plot
            plot_initial()  # this is the default
        }
    })
    
    plot_height <- eventReactive(input$update_plot, {
        if (input$facet == "none") {
            height <- 400 
        } else {
            # adapted from https://github.com/rstudio/shiny/issues/650
            h_dyn <- gg_facet_nrow_ng(plot_updated()) * ((session$clientData$output_data_width-100)/(gg_facet_ncol_ng(plot_updated())))*(1/1.618)
            
            if ( h_dyn < session$clientData$output_data_width * (1/1.618) ) { 
                height <- session$clientData$output_data_width * (1/1.618)
            } else { height <- h_dyn
            }
        } 
        height
    })
    
    output$plot <- renderPlot({ # is there a way to implement renderCachedPlot that would be worthwhile here?
        chosen_plot()
    }, height = function() 
        if (values$plot_chosen == "initial") { 400 
        } else {
            plot_height() 
        }  
    ) 
    
# tm determination server  ---------------------------  

    ## by tma
    observeEvent( values$df, {
        # starting from values$df gives this calculation a fresh slate should the user re-format their data multiple times
        values$df_tms <- values$df %>% #df_int %>% # add the first derivative Tms
            plyr::mutate(sgd1 = purrr::map(data, sgfilt_nest, m_ = 1)) %>% # add the first derivative data
            plyr::mutate(dRFU_tma = as_vector(purrr::map2(data, sgd1, Tm_by_dRFU)))
        
        if ("condition" %in% names(values$df_tms)) {
            # make the averaged table
            values$tm_table_dRFU <- values$df_tms %>%
                select(condition, dRFU_tma)  %>%
                group_by(condition)  %>%
                summarise( mean_tm = mean(dRFU_tma) ,
                           sd_tm = sd(dRFU_tma)) %>%
                mutate_if(is.numeric, round, 2)
        } else {
            values$tm_table_dRFU <- values$df_tms %>%
                select(well, dRFU_tma)  %>%
                group_by(well)  %>%
                summarise( mean_tm = mean(dRFU_tma) ,
                           sd_tm = sd(dRFU_tma)) %>%
                mutate_if(is.numeric, round, 2)
        }

    })
    
    output$tm_table_render <- DT::renderDataTable({
        req(values$df_tms)
        
        if( "dRFU_tma" %in% names(values$df_tms)) { # if there are tms to report, render a table
            df <- values$tm_table_dRFU  %>%
                set_names( c("Condition", "Tm'", "Stdev"))
            
        } else {df <- NULL }
        
        df 
    },
    options = list(scrollX = TRUE, scrollY = 200, scrollCollapse = TRUE, paging = FALSE, dom = 'tr')) #scroller = TRUE, dom = 'tr'

    
    # ## by model fitting
    # observeEvent( { input$s1
    #     input$s1_d
    #     input$s2
    #     input$s2_d }, {
    #         
    #         req(values$df_models)
    #         
    #         #### fit any newly requested models
    #         if (input$s1_d == TRUE) { # if the button for a model is clicked
    #             if ("s1_d_pred" %in% values$df_models$which_model == FALSE) { # if it hasn't already been fit, then fit it and append the values to the summary tibbles
    #                 values$s1_d_list <- model_all(s1_d_model, "s1_d_pred", values$start_pars)
    #                 values$df_models <- values$df_models %>% bind_rows(values$s1_d_list$df_models)
    #                 values$df_BIC_models <- values$df_BIC_models %>% bind_rows(values$s1_d_list$df_BIC)
    #                 values$df_tm_models <- values$df_tm_models %>% bind_rows(values$s1_d_list$tm_table_models)
    #             }}
    #         
    #         if (input$s2 == TRUE) {
    #             if ("s2_pred" %in% values$df_models$which_model == FALSE) {
    #                 values$s2_list <- model_all(s2_model, "s2_pred", values$start_pars)
    #                 values$df_models <- values$df_models %>% bind_rows(values$s2_list$df_models)
    #                 values$df_BIC_models <- values$df_BIC_models %>% bind_rows(values$s2_list$df_BIC)
    #                 values$df_tm_models <- values$df_tm_models %>% bind_rows(values$s2_list$tm_table_models)
    #                 
    #             }}
    #         
    #         if (input$s2_d == TRUE) {
    #             if ("s2_d_pred" %in% values$df_models$which_model == FALSE) {
    #                 values$s2_d_list <- model_all(s2_d_model, "s2_d_pred", values$start_pars)
    #                 values$df_models <- values$df_models %>% bind_rows(values$s2_d_list$df_models)
    #                 values$df_BIC_models <- values$df_BIC_models %>% bind_rows(values$s2_d_list$df_BIC)
    #                 values$df_tm_models <- values$df_tm_models %>% bind_rows(values$s2_d_list$tm_table_models)
    #             }}
    #         
    #         ### update the tm table for display df_tm_models_table <- df_tm_models %>%
    #         model_name_all <- c("s1_pred", "s1_d_pred", "s2_pred", "s2_d_pred")# doesn't need to be in the server or the observer but is fast enough to justify, since it makes the next step clearer
    #         model_name_true <- reactive(model_name_all[c(input$s1, input$s1_d, input$s2, input$s2_d)])
    #         # print(model_name_true )
    #         
    #         values$df_tm_models_table <- values$df_tm_models %>%
    #             dplyr::filter( which_model %in% model_name_true()  ) %>%
    #             plyr::mutate( which_model = grep_and_gsub(.$which_model, c("s1_pred", "s1_d_pred", "s2_pred","s2_d_pred"), c("Model 1", "Model 2", "Model 3", "Model 4"), c("Other")))  %>% # move this to later, for the for-display table only!
    #             set_names(c("Condition", "Model", "Tm' 1", "Tm' 1 SD", "Tm' 2", "Tm' 2 SD")) %>%
    #             discard(~all(is.na(.x)))
    #         
    #         # update which models are available for plotting
    #         mods_available <- named_mods[c(input$s1, input$s1_d, input$s2, input$s2_d)] # the original named_mods is created outside the server
    #         updateRadioButtons(session, "choose_model_tm",
    #                            choices = mods_available,
    #                            selected = mods_available[1]
    #         )
    #         
    #     })
    # 
    # observeEvent(values$df, {
    #     print("entering the modeling ring")
    #     win3d <<- 7#floor(3/((n2r(1) - n2r(0))/n_meas))
    #     
    #     peak_finder_nest <<- make_peak_finder_nest(win3d) ###### brute forced, notice!!!
    #     
    #     values$start_pars <- get_start_pars(values$df)
    #     
    #     
    #     # first, fit the s1 model
    #     # this will over-write the summary dataframes, re-setting the models to follow
    #     values$s1_list <- model_all(s1_model, "s1_pred", values$start_pars) # Warning: Error in rbind: numbers of columns of arguments do not match
    #     print(values$s1_list)
    #     values$df_models <- values$s1_list$df_models
    #     values$df_BIC_models <- values$s1_list$df_BIC
    #     values$df_tm_models <- values$s1_list$tm_table_models
    #     # head(values$df_tm_models)
    #     #
    #     values$df_tm_models_table <- values$df_tm_models %>%
    #         dplyr::filter( which_model == "s1_pred"  ) %>%
    #         plyr::mutate( which_model = grep_and_gsub(.$which_model, c("s1_pred", "s1_d_pred", "s2_pred","s2_d_pred"), c("Model 1", "Model 2", "Model 3", "Model 4"), c("Other")))  %>% # move this to later, for the for-display table only!
    #         set_names(c("Condition", "Model", "Tm' 1", "Tm' 1 SD", "Tm' 2", "Tm' 2 SD")) %>%
    #         discard(~all(is.na(.x)))
    # 
    #     # if new data is uploaded, reset all of the buttons as well. perhaps we should set these to watch values$data (unnamed), so it doesn't get over-written by renaming, but i'd need to think more carefully about how to incorporate the names downstream....
    #     updateButton(session, "s1",  value = TRUE)
    #     updateButton(session, "s1_d",  value = FALSE)
    #     updateButton(session, "s2",  value = FALSE)
    #     updateButton(session, "s2_d",  value = FALSE)
    #     
    # })
    
    
    ### straight from the old app
    #sigmoid fitting
    #############
    ##### for all models, draw from these starting parameters
    
    # the first things that happen, as soon as the data is uploaded. We may want to wait to trigger this until the sigmoid fitting section is opened, in case doing it this way interferes with the initial plotting, even if no fits are desired
    
    # the following happen automatically, and without user request
 
    
    observeEvent(values$df, { # data_raw(), { ### CHANGE THIS BACK TO data_raw() for integration!!!!!!!!!
        
        tryCatch({
            low_T <-  values$df %>% unnest() %>% .$Temperature %>% min() 
            high_T <- values$df %>% unnest() %>% .$Temperature %>% max() 
            n_meas <- values$df %>% unnest() %>% .$Temperature %>% unique() %>% length() 
            
            ### alternative 
            # low_T <- isolate( data_raw()$Temperature %>% min() )
            # high_T <- isolate( data_raw()$Temperature %>% max() )
            # n_meas <- isolate( data_raw() %>% nrow() )
            
            n2r <<- make_temp_n2r(range(low_T:high_T)) #make_temp_n2r(range(values$data$Temperature)) # an example of how this could be used
            win3d <<- floor(3/((n2r(1) - n2r(0))/n_meas))
            if ( win3d < 5 ) { win3d <<- 5 }
            
            sgfilt_nest <<- sgfilt_set_n(n_ = find_sgolay_width( win3d ))
        },   
        error = function(e) {
            print("win3 errored! setting win3d to 7")
            n2r <<- make_temp_n2r(range(25:95)) ### this is a vulnerable thing to hard-code in~~
            #n2r <<- make_temp_n2r(range(low_T:high_T)) #make_temp_n2r(range(values$data$Temperature)) # an example of how this could be used
            win3d <<- 7
            sgfilt_nest <<- sgfilt_set_n( n_ = find_sgolay_width( 7 ) )
        })
    }) # write to values
    
    
    observeEvent(values$df, {
        # print("entering the modeling ring")
        # win3d <<- 7#floor(3/((n2r(1) - n2r(0))/n_meas))
        # 
        # peak_finder_nest <<- make_peak_finder_nest(win3d) ###### brute forced, notice!!!
        
        values$start_pars <- get_start_pars(values$df)
        print("values$start_pars")
        print(values$start_pars)
        
        # first, fit the s1 model
        # this will over-write the summary dataframes, re-setting the models to follow
        values$s1_list <- model_all(s1_model, "s1_pred", values$start_pars)
        # #print(values$s1_list)
        values$df_models <- values$s1_list$df_models
        values$df_BIC_models <- values$s1_list$df_BIC
        values$df_tm_models <- values$s1_list$tm_table_models
        # head(values$df_tm_models)
        #
        values$df_tm_models_table <- values$df_tm_models %>%
            dplyr::filter( which_model == "s1_pred"  ) %>%
            plyr::mutate( which_model = grep_and_gsub(.$which_model, c("s1_pred", "s1_d_pred", "s2_pred","s2_d_pred"), c("Model 1", "Model 2", "Model 3", "Model 4"), c("Other")))  %>% # move this to later, for the for-display table only!
            set_names(c("Condition", "Model", "Tm' 1", "Tm' 1 SD", "Tm' 2", "Tm' 2 SD")) %>%
            discard(~all(is.na(.x)))
        
        # if new data is uploaded, reset all of the buttons as well. perhaps we should set these to watch values$data (unnamed), so it doesn't get over-written by renaming, but i'd need to think more carefully about how to incorporate the names downstream....
        updateButton(session, "s1",  value = TRUE)
        updateButton(session, "s1_d",  value = FALSE)
        updateButton(session, "s2",  value = FALSE)
        updateButton(session, "s2_d",  value = FALSE)
        
    })
    
    output$tm_table_render_models <- DT::renderDataTable({ ### new for models
        req(values$df_tm_models_table)
        values$df_tm_models_table
    },
    options = list(scrollX = TRUE, scrollY = 200, scrollCollapse = TRUE, paging = FALSE, dom = 'tr'))
    
    observeEvent( { input$s1
        input$s1_d
        input$s2
        input$s2_d }, {
            
            req(values$df_models)
            
            #### fit any newly requested models
            if (input$s1_d == TRUE) { # if the button for a model is clicked
                if ("s1_d_pred" %in% values$df_models$which_model == FALSE) { # if it hasn't already been fit, then fit it and append the values to the summary tibbles
                    values$s1_d_list <- model_all(s1_d_model, "s1_d_pred", values$start_pars)
                    values$df_models <- values$df_models %>% bind_rows(values$s1_d_list$df_models)
                    values$df_BIC_models <- values$df_BIC_models %>% bind_rows(values$s1_d_list$df_BIC)
                    values$df_tm_models <- values$df_tm_models %>% bind_rows(values$s1_d_list$tm_table_models)
                }}
            
            if (input$s2 == TRUE) {
                if ("s2_pred" %in% values$df_models$which_model == FALSE) {
                    values$s2_list <- model_all(s2_model, "s2_pred", values$start_pars)
                    values$df_models <- values$df_models %>% bind_rows(values$s2_list$df_models)
                    values$df_BIC_models <- values$df_BIC_models %>% bind_rows(values$s2_list$df_BIC)
                    values$df_tm_models <- values$df_tm_models %>% bind_rows(values$s2_list$tm_table_models)
                    
                }}
            
            if (input$s2_d == TRUE) {
                if ("s2_d_pred" %in% values$df_models$which_model == FALSE) {
                    values$s2_d_list <- model_all(s2_d_model, "s2_d_pred", values$start_pars)
                    values$df_models <- values$df_models %>% bind_rows(values$s2_d_list$df_models)
                    values$df_BIC_models <- values$df_BIC_models %>% bind_rows(values$s2_d_list$df_BIC)
                    values$df_tm_models <- values$df_tm_models %>% bind_rows(values$s2_d_list$tm_table_models)
                }}
            
            ### update the tm table for display df_tm_models_table <- df_tm_models %>%
            model_name_all <- c("s1_pred", "s1_d_pred", "s2_pred", "s2_d_pred")# doesn't need to be in the server or the observer but is fast enough to justify, since it makes the next step clearer
            model_name_true <- reactive(model_name_all[c(input$s1, input$s1_d, input$s2, input$s2_d)])
            # print(model_name_true )
            
            values$df_tm_models_table <- values$df_tm_models %>%
                dplyr::filter( which_model %in% model_name_true()  ) %>%
                plyr::mutate( which_model = grep_and_gsub(.$which_model, c("s1_pred", "s1_d_pred", "s2_pred","s2_d_pred"), c("Model 1", "Model 2", "Model 3", "Model 4"), c("Other")))  %>% # move this to later, for the for-display table only!
                set_names(c("Condition", "Model", "Tm' 1", "Tm' 1 SD", "Tm' 2", "Tm' 2 SD")) %>%
                discard(~all(is.na(.x)))
            
            # update which models are available for plotting
            mods_available <- named_mods[c(input$s1, input$s1_d, input$s2, input$s2_d)] # the original named_mods is created outside the server
            updateRadioButtons(session, "choose_model_tm",
                               choices = mods_available,
                               selected = mods_available[1]
            )
            
        })
    
    
    
    
    
    
} # end server
# Run the application 
shinyApp(ui = ui, server = server)




