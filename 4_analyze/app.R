# 4_analyze/app.R
library(quantmod) # contains the findValleys function, which maybe we should just extract and put verbatim in a source file instead of loading this whole thing...?
library(minpack.lm) # contains the nlsLM function, which we use for our fitting
library(modelr) # used in both the data modeling and the analysis model fitting 
library(SciViews) # contains the ln function used in the data modeling
library(signal) # contains the savistky golay filter (savgolfilt), used to generate the first derivative data in both data modeling and analysis model fitting  


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

named_mods <- c("s1_pred", "s1_d_pred", "s2_pred", "s2_d_pred") %>%
    #set_names("Model 1", "Model 2", "Model 3", "Model 4")
    set_names("Fit 1", "Fit 2", "Fit 3", "Fit 4")

ui <- navbarPage( useShinyalert(),
                
                
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
                                                                                  rHandsontableOutput("r_table"),
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
                                                                                                  #wellPanel(
                                                                                                  p("Select the models you would like to fit to your data below.", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),
                                                                                                  splitLayout(cellWidths = c("25%", "25%", "25%", "25%"), 
                                                                                                              p("Fit 1", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center"),
                                                                                                              p("Fit 2", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center"),
                                                                                                              p("Fit 3", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center"),
                                                                                                              p("Fit 4", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center")
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
                                                                                                  
                                                                                                  bsTooltip("s1", "Fit 1: One sigmoid with decay",
                                                                                                            "right", options = list(container = "body")),
                                                                                                  bsTooltip("s1_d", "Fit 2: One sigmoid with decay and starting-temperature fluorescence",
                                                                                                            "right", options = list(container = "body")),
                                                                                                  bsTooltip("s2", "Fit 3: Two sigmoids with decays",
                                                                                                            "right", options = list(container = "body")),
                                                                                                  bsTooltip("s2_d", "Fit 4: Two sigmoids with decays and starting-temperature fluorescence",
                                                                                                            "right", options = list(container = "body")),
                                                                                                  p(" ", style = "font-family: 'Avenir Next'; font-size: 8px; color: black",align = "center"),
                                                                                                
                                                                                                  
                                                                                                     
                                                                                                      DT::dataTableOutput("tm_table_render_models"), #style = "height:400px;"
                                                                                                  p("-----", style = "font-family: 'Avenir Next'; font-size: 30px; color: white",align = "center"),
                                                                                                  uiOutput("trim_ends"),  

                                                                                                  p(" ", style = "font-family: 'Avenir Next'; font-size: 8px; color: black",align = "center"),
                                                                                                  bsCollapsePanel(p("Plot and selet models for each condition", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                                                  #p("Click 'Plot model comparision' below to display the four models side-by-side. Use this plot to select the best model for each condition. ", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
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
                                                       ), width = 5
                                                       
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
                                                       ), width = 7)
                                               )
                                      )
                          )) # end tabset Panel (contains all "analysis sub-panels)
#div (dataTableOutput ("tm_table_render_models"), style = "font-size: 70%")            
) # end 

# Define server logic required to draw a histogram
server <- function(session, input, output) {
    values <- reactiveValues() 
    values$data_raw <- readRDS("values_df_with_layout.rds")
    
    values$df <- readRDS("values_df_with_layout.rds")
    values$df_1 <- readRDS("values_df_with_layout.rds")
    
    values$plot_chosen <- "initial"
    
# data layout and handling server --------------------------- 

# plotting server  ---------------------------    

    output$update_plot <- renderUI({
        req(values$df)
        actionButton("update_plot", p("Update plot", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),  width = '100%')
    })
    
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
        print("plot changed") # REVISIT
        req(values$df) # only render the plot if there is data
        
        unnest(values$df) %>%
            ggplot(aes(x = Temperature, y = value, group = well)) +
            geom_line(size = 0.5, alpha = 0.7, color = "red") +
            labs(title = "Updated plot", x = "Temperature (ºC)", y = "RFU") +
            theme_bw() +
            dsfworld_default +
            theme(  text = element_text(size = 10*1.25),
                    axis.text = element_text(size = 10),
                    plot.title = element_text(lineheight=.8, face="bold", size = 10*1.5)) 
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
    observeEvent(values$df, {values$df_fit <- values$df} ) # if values$df changes, re-do the fitting. When layouts are updated this will trigger unnecessary re-fitting, but the fitting is fast enough that the extra computations are worth the added simplicty of doing it this way
    
    ## by tma
    observeEvent( values$df_fit, {
        # starting from values$df gives this calculation a fresh slate should the user re-format their data multiple times
        values$df_tms <- values$df_fit %>% #df_int %>% # add the first derivative Tms
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
                set_names( c("Condition", "Tma", "SD"))
            
        } else { df <- NULL }
        
        df 
    },
    options = list(scrollX = TRUE, scrollY = 200, scrollCollapse = TRUE, paging = FALSE, dom = 'tr')) #scroller = TRUE, dom = 'tr'

    # ## by model fitting
    # set initial values for smoothing and normalizing
    output$trim_ends <- renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
        req(values$df)
        sliderInput("trim_ends", 
                    p("Restrict fits to a temperature range", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"), 
                    min = min(unnest(values$df)$Temperature), max = max(unnest(values$df)$Temperature),
                    value = c(min(unnest(values$df)$Temperature),
                              max(unnest(values$df)$Temperature)),
                    step = 1)
    }) # trim the ends off of the data to improve fitting
    
    observeEvent(input$trim_ends, {print("input$trim_ends")
        print(input$trim_ends)
        print(str(input$trim_ends))
        
        if ( (input$trim_ends[2] - input$trim_ends[1]) < 25 ) {
            updateSliderInput(session, "trim_ends", 
                              
                              value = c(min(unnest(values$df)$Temperature),
                                        max(unnest(values$df)$Temperature)),
                              step =  1)
            shinyalert("Not enough data points remaining", "Please increase range to at least 25 measurements")
        } else {
            values$df_fit <- values$df %>%
                mutate(data = map(data, ~ filter(., Temperature %>% between(input$trim_ends[1], input$trim_ends[2])))) 
        }
    })
    
    observeEvent(values$df_fit, { # data_raw(), { ### CHANGE THIS BACK TO data_raw() for integration!!!!!!!!!
        tryCatch({ # REVISIT-- should this be called on data_raw() for any reason?
            low_T <-  values$df_fit %>% unnest() %>% .$Temperature %>% min() 
            high_T <- values$df_fit %>% unnest() %>% .$Temperature %>% max() 
            n_meas <- values$df_fit %>% unnest() %>% .$Temperature %>% unique() %>% length() 
            
            n2r <<- make_temp_n2r(range(low_T:high_T)) #
            win3d <<- floor(3/((n2r(1) - n2r(0))/n_meas))
            if ( win3d < 5 ) { win3d <<- 5 }
            peak_finder_nest <<- make_peak_finder_nest(win3d)
            sgfilt_nest <<- sgfilt_set_n(n_ = find_sgolay_width( win3d ))
        },   
        error = function(e) {
            print("win3 errored! setting win3d to 7")
            n2r <<- make_temp_n2r(range(25:95)) ### REVISIT
            win3d <<- 7
            sgfilt_nest <<- sgfilt_set_n( n_ = find_sgolay_width( 7 ) )
            peak_finder_nest <<- make_peak_finder_nest(win3d)
        })
    }) # write to values
    
    # fit the requested models
    observeEvent(values$df_fit, {
        values$start_pars <- get_start_pars(values$df_fit)
        print("values$start_pars")
        print(values$start_pars)
        
        # first, fit the s1 model
        # this will over-write the summary dataframes, re-setting the models to follow
        values$s1_list <- model_all(s1_model, "s1_pred", values$start_pars)
        values$df_models <- values$s1_list$df_models
        values$df_BIC_models <- values$s1_list$df_BIC
        values$df_tm_models <- values$s1_list$tm_table_models
        
        values$df_tm_models_table <- values$df_tm_models %>%
                        dplyr::filter( which_model == "s1_pred"  ) %>%
                        plyr::mutate( which_model = grep_and_gsub(.$which_model, c("s1_pred", "s1_d_pred", "s2_pred","s2_d_pred"), c("Fit 1", "Fit 2", "Fit 3", "Fit 4"), c("Other")))  %>% # move this to later, for the for-display table only!
            set_names(c("Condition", "Model", "Tma 1", "Tma 1 SD", "Tma 2", "Tma 2 SD")) %>%          
            discard(~all(is.na(.x))) #%>% # get rid of the columns which are all NA (true if the model is not selected)
        # write_rds(values$s1_list, "values_s1_list.rds")
        
        # if new data is uploaded, reset all of the buttons as well. perhaps we should set these to watch values$data (unnamed), so it doesn't get over-written by renaming, but i'd need to think more carefully about how to incorporate the names downstream....
        updateButton(session, "s1",  value = TRUE)
        updateButton(session, "s1_d",  value = FALSE)
        updateButton(session, "s2",  value = FALSE)
        updateButton(session, "s2_d",  value = FALSE)
    })
    
    # ahandle model selections
    observeEvent( { input$s1
                    input$s1_d
                    input$s2
                    input$s2_d }, {
            
            req(values$df_models)
            
            # fit any newly requested models
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
            # write_rds(values$df_models, "values_df_models.rds")
            # write_rds(values$df_BIC_models, "values_df__BIC_models.rds")
            # write_rds(values$df_tm_models, "values_df_tm_models.rds")
            # write_rds(values$df_models, "values_df_models.rds")
            
            #update the tm table for display df_tm_models_table <- df_tm_models %>%
            model_name_all <- c("s1_pred", "s1_d_pred", "s2_pred", "s2_d_pred")# doesn't need to be in the server or the observer but is fast enough to justify, since it makes the next step clearer
            model_name_true <- reactive(model_name_all[c(input$s1, input$s1_d, input$s2, input$s2_d)])
           
            values$df_tm_models_table <- values$df_tm_models %>%
                dplyr::filter( which_model %in% model_name_true()  ) %>%
                plyr::mutate( which_model = grep_and_gsub(.$which_model, c("s1_pred", "s1_d_pred", "s2_pred","s2_d_pred"), c("Fit 1", "Fit 2", "Fit 3", "Fit 4"), c("Other")))  %>% # move this to later, for the for-display table only!
                set_names(c("Condition", "Model", "Tma 1", "Tma 1 SD", "Tma 2", "Tma 2 SD")) %>%
                discard(~all(is.na(.x)))
            
            # update which models are available for plotting
            mods_available <- named_mods[c(input$s1, input$s1_d, input$s2, input$s2_d)] # the original named_mods is created outside the server
            updateRadioButtons(session, "choose_model_tm",
                               choices = mods_available,
                               selected = mods_available[1]
            )
        })
    
    # render the model table
    output$tm_table_render_models <- DT::renderDataTable({ ### new for models
        req(values$df_tm_models_table)
        values$df_tm_models_table 
    },
    options = list(scrollX = TRUE, scrollY = 200, scrollCollapse = TRUE, paging = FALSE, dom = 'tr'))
    

    
    ## display the model plot, with all  comonents. 
    ### choose the best model 
    
    output$show_BIC_plot <- renderUI({
        req(values$df)
        actionButton("show_BIC_plots", 
                     p("Display data with fits for all selected models.", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),  width = '100%')
                     
                     
                     #p("Plot model comparison.", style = "font-family: 'Avenir Next'; font-size: 12px; color: black", align = "center") %>% strong(),  width = '100%')
    })
    
    model_plot <- renderPlot({
        plot_all_fits_shiny(values$df_models, values$df_BIC_models)
    })
    
    # 
} # end server
# Run the application 
shinyApp(ui = ui, server = server)




