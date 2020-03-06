# perhaps we could put the loading of packages into an observe event, to keep them all froam loading right at the start. would this make the first page faster?
library(quantmod) # contains the findValleys function, which maybe we should just extract and put verbatim in a source file instead of loading this whole thing...?
library(minpack.lm) # contains the nlsLM function, which we use for our fitting
library(modelr) # used in both the data modeling and the analysis model fitting 
library(SciViews) # contains the ln function used in the data modeling
library(signal) # contains the savistky golay filter (savgolfilt), used to generate the first derivative data in both data modeling and analysis model fitting  
library(assertive.types) # for dynamic sizing of the facet plots
library(shinyBS) # drop-down panels
library(tidyverse) #  handling data structures and plotting

source("support_scripts/dsfworld5_data_analysis.R") # scripts written to analyze and visualize DSF data
source("support_scripts/20190929_dsfworld_modeling.R") # compute the interactive models; make the interactive modeling plot  
source("support_scripts/DSF_data_parser_dsfw4_v2.R")
source("support_scripts/dsfworld5_model_fitting.R")
source("support_scripts/upload_formatters.R")
source("support_scripts/layout_handling.R")

library(shinyalert)
library(shinycssloaders) # spinning plot loading icon
library(rhandsontable) # user-interactive tables 
library(shiny) # for shiny web-apps 

named_mods <- c("s1_pred", "s1_d_pred", "s2_pred", "s2_d_pred") %>%
    set_names("Model 1", "Model 2", "Model 3", "Model 4")

df_sample <- read.csv("sample_data_file.csv")

ui <- navbarPage("",
                 #tabPanel(p("to data analysis", style = "font-family: 'Avenir Next'; font-size: 20px; color: grey",align = "right")),
                 # Data Analysis --------------------------------------------------------------------------------
                 tabPanel(p("to data analysis", style = "font-family: 'Avenir Next'; font-size: 20px; color: grey",align = "center"), value = "data_analysis_mother", # end tab panel (tabset, div, main still remaining)
                          tabsetPanel(id = "inTabset_analysis", # tabset for all analysis sub-tabs
                                      # analyze and visualize --------------------------- 
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
                                                                                                  textInput("plot_title", "Plot title", "Data Summary"),
                                                                                                  textInput("legend_title", "Legend title", "Condition"),
                                                                                                  uiOutput("linetype_title"),
                                                                                                  textInput("y_title", "y-axis title", "RFU"),
                                                                                                  textInput("x_title", "x-axis title", "Temperature (C)"),
                                                                                                  numericInput("text_size", "Plot text size", 10, min = 4, max = 20),
                                                                                                  checkboxInput("hide_legend", "Hide legend", FALSE)),
                                                                                  style = "default")
                                                       ),
                                                       bsCollapse(id = "tm_table", open = "Panel 2",
                                                                  bsCollapsePanel(p("Find apparent Tms", style = "font-family: 'Avenir Next'; font-size: 16px; color: black",align = "center"),
                                                                                  bsCollapsePanel(p("By dRFU", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                                                                  #splitLayout(cellWidths = c("50%", "50%"),
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
                                                                                                  # bsCollapsePanel(p("Choose model(s) to fit", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                                  #                 p("Fit all models, and select the best ones", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center")
                                                                                                  # ),
                                                                                                  DT::dataTableOutput("tm_table_render_models"), #style = "height:400px;"
                                                                                                  p(" ", style = "font-family: 'Avenir Next'; font-size: 8px; color: black",align = "center"),
                                                                                                  bsCollapsePanel(p("Add Tm' and fits to plots", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                                                  # radioButtons("choose_model_tm", "Select a model to plot",
                                                                                                                  #              #label = c("Model 1"),
                                                                                                                  #              choices = c("")#("Model 1" = "s1_pred")#named_mods
                                                                                                                  # ),
                                                                                                                  uiOutput("choose_model_tm"),
                                                                                                                  checkboxInput("show_Tm_mods", "Show Tm' on plots", FALSE),
                                                                                                                  checkboxInput("show_fit", "Show fits", FALSE),
                                                                                                                  checkboxInput("show_fit_comps", "Show fit components", FALSE),
                                                                                                                  p("Fits and Tm' are ploted in black by default", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left") %>% strong(),
                                                                                                                  checkboxInput("show_Tm_mods_colors", "Apply colors  to Tm'", FALSE),
                                                                                                                  checkboxInput("show_fit_comps_colors", "Apply colors  to fit lines", FALSE),
                                                                                                                  uiOutput("update_model_plots")
                                                                                                                  
                                                                                                                  #actionButton("update_model_plots", p("Update plot", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(), width = "100%")
                                                                                                  ),
                                                                                                  
                                                                                                  
                                                                                                  bsCollapsePanel(p("Improve fits", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                                                  p("Fit data only in the following temperature range", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),
                                                                                                                  #p("", style = "font-family: 'Avenir Next'; font-size: 10px; color: black", align = "center"),
                                                                                                                  uiOutput("trim_ends"),
                                                                                                                  p("Update initial model  guesses", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),
                                                                                                                  p("To fit models to data, DSFworld begins by guessing initial values for model parameters, and then improves upon them until the fit is optimized. If fits have failed, or describe the data poorly, manually defining more accurate starting guesses for the Tm' below can help.", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center"),
                                                                                                                  #rHandsontableOutput("start_pars"),
                                                                                                                  # bsTooltip("trim_ends", p("To perform each fit, DSFworld starts with an initial guess for the model parameters. If fits have failed, or describe the data poorly, manually defining more accurate starting guesses for the Tm' below can help.", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center"),
                                                                                                                  #           "right", options = list(container = "body")),
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
                                                                                                                  #rHandsontableOutput("selected_models"),
                                                                                                                  uiOutput("apply_handson_models")
                                                                                                                  #rHandsontableOutput("start_pars"),
                                                                                                                  # bsTooltip("trim_ends", p("To perform each fit, DSFworld starts with an initial guess for the model parameters. If fits have failed, or describe the data poorly, manually defining more accurate starting guesses for the Tm' below can help.", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center"),
                                                                                                                  #           "right", options = list(container = "body")),
                                                                                                                  
                                                                                                  )
                                                                                  ),
                                                                                  #DT::dataTableOutput("tm_table_render"), #style = "height:400px;"
                                                                                  style = "default"
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
                                                           plotOutput("data", height = "auto") %>% withSpinner(color="#525252"), style = ("overflow-y:scroll; max-height: 600px") 
                                                       ))
                                               )
                                      )
                                      # end tabpanel
                          )) # end tabset Panel (contains all "analysis sub-panels)
                 
) # end 


# Define server logic required to draw a histogram
server <- function(session, input, output) {
    values <- reactiveValues() 
    values$data_raw <- df_sample
    values$r_table_update <- 1 # initialize the r_table update counter
    
    observeEvent(values$data_raw, {
        print("observe1")
        values$data_raw <- df_sample 
        # set the following values based on the data
        tryCatch({
            low_T <- isolate( values$data_raw$Temperature %>% min() )
            high_T <- isolate( values$data_raw$Temperature %>% max() )
            n_meas <- isolate( values$data_raw %>% nrow() )
            
            n2r <<- make_temp_n2r(range(low_T:high_T)) #make_temp_n2r(range(values$data$Temperature)) # an example of how this could be used
            win3d <<- floor(3/((n2r(1) - n2r(0))/n_meas))
            if ( win3d < 5 ) { win3d <<- 5 }
            
            sgfilt_nest <<- sgfilt_set_n(n_ = find_sgolay_width( win3d ))
            print("end observe1")
        },   
        error = function(e){
            print("win3 errored! setting win3d to 7")
            win3d <<- 7
            sgfilt_nest <<- sgfilt_set_n( n_ = find_sgolay_width( 7 ) )
        })
    }) # write to values
    
    observeEvent(values$data_raw, { # ultimately, observe the transfer to the analysis page
        print("observe2")
        req(values$data_raw) # but leave this requirement as is
        
        tryCatch({
            values$df <- nest_raw(values$data_raw) %>% # active dataframe, used for plotting and calculations
                add_standardized_wells()
            
            values$df_1 <- nest_raw(values$data_raw) %>% # original dataframe, used when a "clean slate" is needed, e.g. if a new layout file is uploaded
                add_standardized_wells()
            print("end observe2")
            
        }, error = function(e) {
            shinyalert("Please ensure that your data is formatted correctly", "In the 'upload data' tab, you data should be displayed with Temperature in the first column, and RFU data in the columns to the right.")
        }
        )
    })
    
    ########## Render GUI elements for the analysis page ######
    output$update_plot <- renderUI({
        req(values$df)
        actionButton("update_plot", p("Update plot", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),  width = '100%')
    })
    
    
    output$current_names <- renderTable({values$df %>% select_if(is.character)})
    
    output$handson_update_button <- renderUI({
        req(values$df)
        actionButton("submit_handson_names", p("Update names from manual table (above)", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),  width = '100%')
    })
    
    output$update_fits_button <- renderUI({
        req(values$df)
        actionButton("update_fits", p("Re-fit data", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong() ,  width = '100%')
    })
    
    
    output$update_model_plots <- renderUI({
        req(values$df)
        actionButton("update_model_plots", p("Update plot", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(), width = "100%")
    })
    
    output$update_dRFU_plots <- renderUI({
        req(values$df)
        actionButton("update_dRFU_plots", p("Update plot", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(), width = "100%")
    })
    
    output$show_BIC_plot <- renderUI({
        req(values$df)
        actionButton("show_BIC_plots", p("Plot model comparison.", style = "font-family: 'Avenir Next'; font-size: 12px; color: black", align = "center") %>% strong(),  width = '100%')
    })
    
    output$apply_handson_models <- renderUI({
        req(values$df)
        actionButton("apply_handson_models", p("Apply model selections from table", style = "font-family: 'Avenir Next'; font-size: 12px; color: black", align = "center") %>% strong(),  width = '100%')
    })
    
    output$choose_model_tm <- renderUI({
        req(values$df)
        
        radioButtons("choose_model_tm", "Select a model to plot",
                     #label = c("Model 1"),
                     choices = c("Model 1" = "s1_pred"))#named_mods
        
    })
    
    ########## End render GUI elements for the analysis page ######
    
    
    ######## eval selections, to pass to the plotting function #########
    output$wrap_by <- renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
        req(values$df)
        if (input$facet == "none" | input$facet == "grid") return(NULL)
        varSelectInput("wrap_by", label = "Sub-plot by",  # as select input
                       data = values$df %>%
                           select_if(is.character) %>%
                           plyr::mutate("-" = rep("", nrow(.))),
                       selected = "-"
                       #mutate()
        ) # this is able to take a reactive value
    })
    
    output$grid_rows <- renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
        req(values$df)
        if (input$facet == "none" | input$facet == "wrap") return(NULL)
        varSelectInput("grid_rows", label = "Sub-plot grid, rows",  # as select input
                       data = values$df %>%
                           plyr::mutate("-" = rep("", nrow(.))) %>%
                           select_if(is.character),
                       selected = "-") # this is able to take a reactive value
        
    })
    
    output$grid_cols <- renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
        req(values$df)
        req(input$grid_rows)
        if (input$facet == "none" | input$facet == "wrap") return(NULL)
        varSelectInput("grid_cols", label = "Sub-plot grid, columns",  # as select input
                       data = values$df %>%
                           plyr::mutate("-" = rep("", nrow(.))) %>%
                           plyr::mutate("- " = rep("", nrow(.))) %>%
                           select_if(is.character) %>%
                           select(-!!input$grid_rows),
                       selected = "- "
        ) # %>% debounce(1000)# this is able to take a reactive value
    }) 
    
    output$color_by <-renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
        req(values$df)
        varSelectInput("color_by", label = "Color",  # as select input
                       data = values$df%>%
                           plyr::mutate("-" = rep("", nrow(.))) %>%
                           select_if(is.character),
                       selected = "-") # this is able to take a reactive value
    })  
    
    output$linetype_by <-renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
        req(values$df)
        if (input$use_linetypes == FALSE) return(NULL)
        varSelectInput("linetype_by", label = "Line types",  # as select input
                       data = values$df%>%
                           plyr::mutate("-" = rep("", nrow(.))) %>%
                           select_if(is.character),
                       selected = "-") # this is able to take a reactive value
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
    
    ######## end eval selections, to pass to the plotting function #########   
    
    ####### this responds to the upoading of a layout file
    layout <- reactive({
        req(input$layout_file)
        
        make_layout(input$layout_file$datapath) %>% # all columns are characters
            add_standardized_wells()
    })
    
    observeEvent( layout(), { 
        values$df <- join_layout_nest(values$df_1, layout() ) 
    })
    
    
    observeEvent(input$submit_handson_names, { # when r_table is updated
        values$df<- hot_to_r(input$r_table) %>% # update the layout
            as_tibble() %>%
            ensure_standardized_wells() %>%
            join_layout_nest( values$df_1, . )
        
        values$r_table_update <- values$r_table_update +1 # trigger the re-rendering of the r_table
        
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    
    output$r_table <- renderRHandsontable({
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
        rhandsontable( handson_df, height = 200, useTypes = TRUE, stretch = "all") %>% hot_col("well", readOnly = TRUE) #%>%  hot_context_menu(allowRowEdit = FALSE, allowColEdit = TRUE)
    })
    
    # plot <- reactive({
    #     req(values$df) # only render the plot if there is data
    #     
    #     df_RFU_plot <- unnest(values$df) %>%
    #         plyr::mutate("-" = rep("", nrow(.))) %>%
    #         plyr::mutate("- " = rep("", nrow(.)))
    #     
    #     ####### Tm calculations 
    #     
    #     observeEvent( values$df, {
    #         print("observed!")
    #         
    #         # starting from values$df gives this calculation a fresh slate should the user re-format their data multiple times
    #         values$df_tms <- values$df %>% #df_int %>% # add the first derivative Tms
    #             plyr::mutate(sgd1 = purrr::map(data, sgfilt_nest, m_ = 1)) %>% # add the first derivative data
    #             plyr::mutate(dRFU_tma = as_vector(purrr::map2(data, sgd1, Tm_by_dRFU)))
    #         
    #         if ("condition" %in% names(values$df_tms)) {
    #             # make the averaged table
    #             values$tm_table_dRFU <- values$df_tms %>%
    #                 select(condition, dRFU_tma)  %>%
    #                 group_by(condition)  %>%
    #                 summarise( mean_tm = mean(dRFU_tma) ,
    #                            sd_tm = sd(dRFU_tma)) %>%
    #                 mutate_if(is.numeric, round, 2)
    #         } else {
    #             values$tm_table_dRFU <- values$df_tms %>%
    #                 select(well, dRFU_tma)  %>%
    #                 group_by(well)  %>%
    #                 summarise( mean_tm = mean(dRFU_tma) ,
    #                            sd_tm = sd(dRFU_tma)) %>%
    #                 mutate_if(is.numeric, round, 2)  
    #         }
    #     })
    #     
    #     facet_func(df = df_RFU_plot, # reacts to the appearance and changes to the dataframe, to the uploading of format files
    #                mean_or_each = input$mean_or_each,
    #                color_by = !!input$color_by,
    #                linetype_by = !!input$linetype_by,
    #                use_linetypes = input$use_linetypes,
    #                facet = input$facet,
    #                facet_by = !!input$wrap_by,
    #                facet_rows = !!input$grid_rows,
    #                facet_cols = !!input$grid_cols,
    #                set_title = plot_title_d(),
    #                legend_title = plot_legend_d(),
    #                legend_linetype_title = plot_legend_linetype(),
    #                fix_free = input$fix_free,
    #                text_size = input$text_size,
    #                legend_position = legend_position(),
    #                x_title = input$x_title,
    #                y_title = input$y_title)
    # })
    # 
    # # make an initial plot
    # observeEvent( values$data_raw, { # when new data is uploaded
    #     output$data <- renderPlot({ # is there a way to implement renderCachedPlot that would be worthwhile here?
    #         req(values$df)
    #         # 
    #         # df_RFU_plot <- unnest(values$df) %>%
    #         #     plyr::mutate("-" = rep("", nrow(.))) %>%
    #         #     plyr::mutate("- " = rep("", nrow(.)))
    #         
    #         facet_func(df = unnest(values$df), # reacts to the appearance and changes to the dataframe, to the uploading of format files
    #                    mean_or_each = input$mean_or_each,
    #                    color_by = !!input$color_by,
    #                    linetype_by = !!input$linetype_by,
    #                    use_linetypes = input$use_linetypes,
    #                    facet = input$facet,
    #                    facet_by = !!input$wrap_by,
    #                    facet_rows = !!input$grid_rows,
    #                    facet_cols = !!input$grid_cols,
    #                    set_title = plot_title_d(),
    #                    legend_title = plot_legend_d(),
    #                    legend_linetype_title = plot_legend_linetype(),
    #                    fix_free = input$fix_free,
    #                    text_size = input$text_size,
    #                    legend_position = legend_position(),
    #                    x_title = input$x_title,
    #                    y_title = input$y_title)
    #         
    #     }, height = function() {
    #         if (input$facet == "none") {
    #             height <<- 400#session$clientData$output_plot1_width * (1/1.618)
    #         } else {
    #             # adapted from https://github.com/rstudio/shiny/issues/650
    #             h_dyn <<- gg_facet_nrow_ng(plot()) * ((session$clientData$output_data_width-100)/(gg_facet_ncol_ng(plot())))*(1/1.618)
    #             if (h_dyn < session$clientData$output_data_width * (1/1.618) ) { height <- session$clientData$output_data_width * (1/1.618)
    #             } else { height <<- h_dyn}
    #         }
    #         height
    #     }) 
    # })
    # 
    # observeEvent( input$update_plot, {  # when the update plot button is pressed'
    #     
    #     df_RFU_plot <- unnest(values$df) %>%
    #         plyr::mutate("-" = rep("", nrow(.))) %>%
    #         plyr::mutate("- " = rep("", nrow(.)))
    #     
    #     ####### Tm calculations 
    #     
    #     observeEvent( values$df, {
    #         print("observed!")
    #         
    #         # starting from values$df gives this calculation a fresh slate should the user re-format their data multiple times
    #         values$df_tms <- values$df %>% #df_int %>% # add the first derivative Tms
    #             plyr::mutate(sgd1 = purrr::map(data, sgfilt_nest, m_ = 1)) %>% # add the first derivative data
    #             plyr::mutate(dRFU_tma = as_vector(purrr::map2(data, sgd1, Tm_by_dRFU)))
    #         
    #         if ("condition" %in% names(values$df_tms)) {
    #             # make the averaged table
    #             values$tm_table_dRFU <- values$df_tms %>%
    #                 select(condition, dRFU_tma)  %>%
    #                 group_by(condition)  %>%
    #                 summarise( mean_tm = mean(dRFU_tma) ,
    #                            sd_tm = sd(dRFU_tma)) %>%
    #                 mutate_if(is.numeric, round, 2)
    #         } else {
    #             values$tm_table_dRFU <- values$df_tms %>%
    #                 select(well, dRFU_tma)  %>%
    #                 group_by(well)  %>%
    #                 summarise( mean_tm = mean(dRFU_tma) ,
    #                            sd_tm = sd(dRFU_tma)) %>%
    #                 mutate_if(is.numeric, round, 2)  
    #         }
    #     })
    #     
    #     plot <- facet_func(df = df_RFU_plot, # reacts to the appearance and changes to the dataframe, to the uploading of format files
    #                        mean_or_each = input$mean_or_each,
    #                        color_by = !!input$color_by,
    #                        linetype_by = !!input$linetype_by,
    #                        use_linetypes = input$use_linetypes,
    #                        facet = input$facet,
    #                        facet_by = !!input$wrap_by,
    #                        facet_rows = !!input$grid_rows,
    #                        facet_cols = !!input$grid_cols,
    #                        set_title = plot_title_d(),
    #                        legend_title = plot_legend_d(),
    #                        legend_linetype_title = plot_legend_linetype(),
    #                        fix_free = input$fix_free,
    #                        text_size = input$text_size,
    #                        legend_position = legend_position(),
    #                        x_title = input$x_title,
    #                        y_title = input$y_title)
    #     
    #     ##
    #     output$data <- renderPlot({ # is there a way to implement renderCachedPlot that would be worthwhile here?
    #         plot #%>% withSpinner(color="#525252")
    #     }, height = function() {
    #         if (input$facet == "none") {
    #             height <<- 400#session$clientData$output_plot1_width * (1/1.618)
    #         } else {
    #             # adapted from https://github.com/rstudio/shiny/issues/650
    #             h_dyn <<- gg_facet_nrow_ng(plot()) * ((session$clientData$output_data_width-100)/(gg_facet_ncol_ng(plot())))*(1/1.618)
    #             if (h_dyn < session$clientData$output_data_width * (1/1.618) ) { height <- session$clientData$output_data_width * (1/1.618)
    #             } else { height <<- h_dyn}
    #             
    #         }
    #         height
    #     })
    #     
    # })
    # 
    # 
    # callModule(plotDownload, "plot1", data)
    # 
    # output$trim_ends <-renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
    #     req(values$df)
    #     sliderInput("trim_ends", "", min = min(unnest(values$df)$Temperature), max = max(unnest(values$df)$Temperature), 
    #                 value = c(min(unnest(values$df)$Temperature),max(unnest(values$df)$Temperature)), 
    #                 #value = c(min(values$df$Temperature)), 
    #                 step = 1)
    # }) # trim the ends off of the data to improve fitting
    # 
    # 
    # 
    # 
    # ####### Tm calculations
    # observeEvent( values$df, {
    #     print("observed!")
    #     
    #     # starting from values$df gives this calculation a fresh slate should the user re-format their data multiple times
    #     values$df_tms <- values$df %>% #df_int %>% # add the first derivative Tms
    #         plyr::mutate(sgd1 = purrr::map(data, sgfilt_nest, m_ = 1)) %>% # add the first derivative data
    #         plyr::mutate(dRFU_tma = as_vector(purrr::map2(data, sgd1, Tm_by_dRFU)))
    #     
    #     if ("condition" %in% names(values$df_tms)) {
    #         # make the averaged table
    #         values$tm_table_dRFU <- values$df_tms %>%
    #             select(condition, dRFU_tma)  %>%
    #             group_by(condition)  %>%
    #             summarise( mean_tm = mean(dRFU_tma) ,
    #                        sd_tm = sd(dRFU_tma)) %>%
    #             mutate_if(is.numeric, round, 2)
    #     } else {
    #         values$tm_table_dRFU <- values$df_tms %>%
    #             select(well, dRFU_tma)  %>%
    #             group_by(well)  %>%
    #             summarise( mean_tm = mean(dRFU_tma) ,
    #                        sd_tm = sd(dRFU_tma)) %>%
    #             mutate_if(is.numeric, round, 2)
    #     }
    #     
    #     
    # })
    # 
    # 
    # 
    # output$tm_table_render <- DT::renderDataTable({
    #     req(values$df_tms)
    #     
    #     if( "dRFU_tma" %in% names(values$df_tms)) { # if there are tms to report, render a table
    #         df <- values$tm_table_dRFU  %>%
    #             set_names( c("Condition", "Tm'", "Stdev"))
    #         
    #     } else {df <- NULL }
    #     
    #     df # display this dataframe
    # },
    # options = list(scrollX = TRUE, scrollY = 200, scrollCollapse = TRUE, paging = FALSE, dom = 'tr')) #scroller = TRUE, dom = 'tr'
    # 
    # 
    # output$tm_table_render_models <- DT::renderDataTable({ ### new for models
    #     req(values$df_tm_models_table)
    #     values$df_tm_models_table
    # },
    # options = list(scrollX = TRUE, scrollY = 200, scrollCollapse = TRUE, paging = FALSE, dom = 'tr'))
    # 
    # 
    # observeEvent(input$update_model_plots, { # when the user goes to update the modeling plots
    #     plot_which_model <- reactive({ input$choose_model_tm })
    #     show_fits <- reactive({input$show_fit   })
    #     show_components <- reactive({input$show_fit_comps  })
    #     show_model_tm <- reactive({input$show_Tm_mods  })
    #     
    #     color_model_tm <-reactive({input$show_Tm_mods_colors  })
    #     color_fits <- reactive({input$show_fit_comps_colors  })
    #     
    #     # checkboxInput("show_Tm_dRFU", "Show Tm' on plot", FALSE),
    #     # checkboxInput("show_Tm_dRFU_colors", "Apply colors  to Tm'", FALSE),
    #     # 
    #     # 
    #     # uiOutput("choose_model_tm"),
    #     # checkboxInput("show_Tm_mods", "Show Tm' on plots", FALSE),
    #     # checkboxInput("show_fit", "Show fits", FALSE),
    #     # checkboxInput("show_fit_comps", "Show fit components", FALSE),
    #     # checkboxInput("show_fit_comps_colors", "Apply colors  to fit lines", FALSE),
    #     # checkboxInput("show_Tm_mods_colors", "Apply colors  to Tm'", FALSE),
    # })
    # 
    # #sigmoid fitting
    # #############
    # ##### for all models, draw from these starting parameters
    # 
    # # the first things that happen, as soon as the data is uploaded. We may want to wait to trigger this until the sigmoid fitting section is opened, in case doing it this way interferes with the initial plotting, even if no fits are desired
    # 
    # # the following happen automatically, and without user request
    # # observeEvent(values$data_raw, {
    # #     # creation of data-sepcific functions and values
    # #     low_T <- isolate( values$data_raw$Temperature %>% min() )
    # #     high_T <- isolate( values$data_raw$Temperature %>% max() )
    # #     n_meas <- isolate( values$data_raw$Temperature %>% unique() %>% length())
    # #     
    # #     #n2r <<- make_temp_n2r(range(low_T:high_T)) #make_temp_n2r(range(values$data$Temperature)) # an example of how this could be used
    # #     #win3d <<- 7#floor(3/((n2r(1) - n2r(0))/n_meas))
    # #     #win3d <<- floor(3/((n2r(1) - n2r(0))/n_meas))
    # #     win3d <<- 7#floor(3/((n2r(1) - n2r(0))/n_meas))
    # #     sgfilt_nest <<- sgfilt_set_n(n_ = find_sgolay_width( win3d ))
    # #     print("finished with initial uploads")
    # #     
    # # })
    # 
    # observeEvent(values$df, {
    #     # print("entering the modeling ring")
    #     # win3d <<- 7#floor(3/((n2r(1) - n2r(0))/n_meas))
    #     
    #     peak_finder_nest <<- make_peak_finder_nest(win3d) ###### brute forced, notice!!!
    #     
    #     values$start_pars <- get_start_pars(values$df)
    #     
    #     
    #     # first, fit the s1 model
    #     # this will over-write the summary dataframes, re-setting the models to follow
    #     values$s1_list <- model_all(s1_model, "s1_pred", values$start_pars)
    #     #print(values$s1_list)
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
    # 
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
    
} # end server
# Run the application 
shinyApp(ui = ui, server = server)
