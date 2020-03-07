library(shinyBS) # drop-down panels
library(tidyverse) #  handling data structures and plotting

source("support_scripts/upload_formatters.R")
source("support_scripts/layout_handling.R")

library(shinyalert) # pop-up error messages
library(shinycssloaders) # spinning plot loading icon
library(rhandsontable) # user-interactive tables 
library(shiny) # for shiny web-apps 

ui <- navbarPage(useShinyalert(),
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
                                       # end tabpanel
                           )) # end tabset Panel (contains all "analysis sub-panels)

                 ) # end 


# Define server logic required to draw a histogram
server <- function(session, input, output) {
    values <- reactiveValues() 
    df_sample <- readRDS("values_df_with_layout.rds")
    
    observeEvent(values$data_raw, {
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
        },   
        error = function(e){
            print("win3 errored! setting win3d to 7")
            win3d <<- 7
            sgfilt_nest <<- sgfilt_set_n( n_ = find_sgolay_width( 7 ) )
        })
    }) # write to values
    
    observeEvent(values$data_raw, { # ultimately, observe the transfer to the analysis page
        req(values$data_raw) # but leave this requirement as is
        tryCatch({
            values$df <- values$data_raw %>% # this is the active dataframe, used for plotting and calculations
                gather(well, value, -Temperature) %>%
                group_by(well) %>%
                mutate(value_norm = BBmisc::normalize(value, method = "range", range = c(0,1)), ###### if we do this as a mutate, it ignores the groups!!!!!!!
                       Temperature_norm = BBmisc::normalize(Temperature, method = "range", range = c(0,1)))  %>%
                nest %>%
                plyr::mutate(new_names = well)
            
            values$df_1 <- values$data_raw %>% # this is the original dataframe, used when a "clean slate" is needed, e.g. if a new layout file is uploaded
                gather(well, value, -Temperature) %>%
                group_by(well) %>%
                mutate(value_norm = BBmisc::normalize(value, method = "range", range = c(0,1)), ###### if we do this as a mutate, it ignores the groups!!!!!!!
                       Temperature_norm = BBmisc::normalize(Temperature, method = "range", range = c(0,1)))  %>%
                nest %>%
                plyr::mutate(new_names = well)
        }, error = function(e){
            shinyalert("Please ensure that your data is formatted correctly", "In the 'upload data' tab, you data should be displayed with Temperature in the first column, and RFU data in the columns to the right.")
        }
        )
    })
    
    ### handsontable for renaming
    output$update_plot <- renderUI({
        req(values$df)
        actionButton("update_plot", p("Update plot", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),  width = '100%')
    })
    
    output$r_table <- renderRHandsontable({ 
        req(values$df)
        rhandsontable( values$df %>% select_if(is.character), height = 200, useTypes = TRUE, stretch = "all") %>% hot_col("well", readOnly = TRUE) #%>%  hot_context_menu(allowRowEdit = FALSE, allowColEdit = TRUE)
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
    

    observeEvent(input$submit_handson_names, { # when table1 is updated
        
        new_names_raw <- hot_to_r(input$r_table) %>%
            as.data.frame()
        
        if ("condition" %in% names(new_names_raw)) {
            new_names <- new_names_raw %>%
                select(-condition, -well)  %>% # remove the old condition column, and the well column
                unite("condition", 1:ncol(.), remove = FALSE)
            
        } else {
            print("nope")
            new_names <- new_names_raw %>%
                select(-well) %>%
                unite("condition", 1:ncol(.), remove = FALSE)
            print("new_names")
            print(new_names)
            print(str(new_names))
        }
        
        overwrite <- names(new_names) %>% # determine the columns to overwrite
            intersect(names(values$df))
        print("overwrite")
        print(overwrite)
        print(names(dplyr::select(values$df, -overwrite)))
        print(names(new_names))
        
        # a previous version of this  stopped working, i think because of a difference in the default handling behavior of grouped dataframes...?
        values$df <- values$df %>%
            ungroup() %>%
            dplyr::select( -overwrite) %>%
            bind_cols(new_names)
        
        
        means <- unnest(values$df)  %>%
            group_by(Temperature, condition) %>% # once the layout is uploaded, handle the replicates
            dplyr::summarize(mean = mean(value),
                             sd = sd(value)) %>%
            ungroup()
        
        df_interm <- values$df %>%
            unnest()
        
        if ("mean" %in% names(df_interm)) {
            print("mean is present")
            values$df <- df_interm %>%
                select( -mean, -sd) %>%
                merge(means, by = c("x" = "Temperature", "y"  = "condition")) %>%
                group_by_at(vars(one_of(names(select_if(., is_character))))) %>%
                nest
            #nest_legacy()
            
        } else {
            print("mean is not present")
            values$df <- df_interm %>%
                merge(means, by = c("x" = "Temperature", "y"  = "condition")) %>%
                group_by_at(vars(one_of(names(select_if(., is_character))))) %>%
                nest
            #nest_legacy()
        }
    }, ignoreInit = TRUE, ignoreNULL = TRUE
    )
    
    ####### this responds to the upoading of a layout file
    layout <- reactive({
        layout_file <- input$layout_file
        
        if (is.null(layout_file)) { # if there is no layout file, keep df the same
            return(df())
        } else { # if there is a layout file, do the following
            layout_list <- data.table::fread( layout_file$datapath, header = TRUE ) %>%
                as_tibble() %>%
                split( . ,  . $Type)
            
            # put into a merge-able form
            layout <- df_to_layout(layout_list[[1]], names(layout_list)[[1]])[c(1,2,4)] # initialize the list
            for (i in c(1:length(layout_list))) {
                #name = names(layout_list)[[i]]
                layout <- layout %>% ##### YOU PROBABLY HAVE TO MAKE THESE ALL CHARACTER TYPE!!!!
                    plyr::mutate("var" =  as_vector(df_to_layout(layout_list[[i]], layout_type = names(layout_list)[[i]])[3] )) %>% # append the column of interest
                    set_names(c(names(layout), names(layout_list)[[i]])) # rename based on the column of interest
            }
            layout <- layout %>% unite("condition", c(4:ncol(.)), remove = FALSE) # create a unique column, used to define groups after averaging
        }
    })
    
    observeEvent( input$layout_file, { 
        ###### once a layout is available, update the df to incorporate it this can all be triggered to occur together by the availability of the layout file, in a single action.
        values$df <- select(unnest(values$df_1), c("Temperature", "value","well")) %>% # always pull from the un-named values$df_1, to avoid collisions when new formats are uploaded
            merge(layout(), by = "well") %>%  # this dataframe is now missing most of the variable and indexing columns
            group_by_at(vars(one_of(names(select_if(., is_character))))) %>% # group by all of the character-type columns (not great for concentration or numeric variables..?)
            mutate(value_norm = BBmisc::normalize(value, method = "range", range = c(0,1)), ###### if we do this as a mutate, it ignores the groups!!!!!!!
                   Temperature_norm = BBmisc::normalize(Temperature, method = "range", range = c(0,1)))  %>%
            nest() %>%
            plyr::mutate(new_names = well)

        means <- unnest(values$df)  %>%
            group_by(Temperature, condition) %>% # once the layout is uploaded, handle the replicates
            dplyr::summarize(mean = mean(value),
                             sd = sd(value)) %>%
            ungroup()
        
        values$df <- values$df %>% 
            unnest() %>%
            merge(means, by = c("x" = "Temperature", "y"  = "condition")) %>%
            group_by_at(vars(one_of(names(select_if(., is_character))))) %>%
            nest
    })
    
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
    
 
    
    observeEvent(input$update_model_plots, { # when the user goes to update the modeling plots
        plot_which_model <- reactive({ input$choose_model_tm })
        show_fits <- reactive({input$show_fit   })
        show_components <- reactive({input$show_fit_comps  })
        show_model_tm <- reactive({input$show_Tm_mods  })
        
        color_model_tm <-reactive({input$show_Tm_mods_colors  })
        color_fits <- reactive({input$show_fit_comps_colors  })
        
        # checkboxInput("show_Tm_dRFU", "Show Tm' on plot", FALSE),
        # checkboxInput("show_Tm_dRFU_colors", "Apply colors  to Tm'", FALSE),
        # 
        # 
        # uiOutput("choose_model_tm"),
        # checkboxInput("show_Tm_mods", "Show Tm' on plots", FALSE),
        # checkboxInput("show_fit", "Show fits", FALSE),
        # checkboxInput("show_fit_comps", "Show fit components", FALSE),
        # checkboxInput("show_fit_comps_colors", "Apply colors  to fit lines", FALSE),
        # checkboxInput("show_Tm_mods_colors", "Apply colors  to Tm'", FALSE),
    })
    
 
    plot <- reactive({
        req(values$df) # only render the plot if there is data
     
        df_RFU_plot <- unnest(values$df) %>%
            plyr::mutate("-" = rep("", nrow(.))) %>%
            plyr::mutate("- " = rep("", nrow(.)))

        ####### Tm calculations 
        
        observeEvent( values$df, {
            print("observed!")
            
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
        
        facet_func(df = df_RFU_plot, # reacts to the appearance and changes to the dataframe, to the uploading of format files
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
    
    # make an initial plot
    observeEvent( values$data_raw, { # when new data is uploaded
        output$data <- renderPlot({ # is there a way to implement renderCachedPlot that would be worthwhile here?
            req(values$df)
            # 
            # df_RFU_plot <- unnest(values$df) %>%
            #     plyr::mutate("-" = rep("", nrow(.))) %>%
            #     plyr::mutate("- " = rep("", nrow(.)))
            
            facet_func(df = unnest(values$df), # reacts to the appearance and changes to the dataframe, to the uploading of format files
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
            
        }, height = function() {
            if (input$facet == "none") {
                height <<- 400#session$clientData$output_plot1_width * (1/1.618)
            } else {
                # adapted from https://github.com/rstudio/shiny/issues/650
                h_dyn <<- gg_facet_nrow_ng(plot()) * ((session$clientData$output_data_width-100)/(gg_facet_ncol_ng(plot())))*(1/1.618)
                if (h_dyn < session$clientData$output_data_width * (1/1.618) ) { height <- session$clientData$output_data_width * (1/1.618)
                } else { height <<- h_dyn}
            }
            height
        }) 
    })
    
    observeEvent( input$update_plot, {  # when the update plot button is pressed'
            
            df_RFU_plot <- unnest(values$df) %>%
                plyr::mutate("-" = rep("", nrow(.))) %>%
                plyr::mutate("- " = rep("", nrow(.)))
            
            ####### Tm calculations 
            
            observeEvent( values$df, {
                print("observed!")
                
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
            
            plot <- facet_func(df = df_RFU_plot, # reacts to the appearance and changes to the dataframe, to the uploading of format files
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
        
        ##
        output$data <- renderPlot({ # is there a way to implement renderCachedPlot that would be worthwhile here?
            plot #%>% withSpinner(color="#525252")
        }, height = function() {
            if (input$facet == "none") {
                height <<- 400#session$clientData$output_plot1_width * (1/1.618)
            } else {
                # adapted from https://github.com/rstudio/shiny/issues/650
                h_dyn <<- gg_facet_nrow_ng(plot()) * ((session$clientData$output_data_width-100)/(gg_facet_ncol_ng(plot())))*(1/1.618)
                if (h_dyn < session$clientData$output_data_width * (1/1.618) ) { height <- session$clientData$output_data_width * (1/1.618)
                } else { height <<- h_dyn}

            }
            height
        })

    })
    
 
    callModule(plotDownload, "plot1", data)
    
    output$trim_ends <-renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
        req(values$df)
        sliderInput("trim_ends", "", min = min(unnest(values$df)$Temperature), max = max(unnest(values$df)$Temperature), 
                    value = c(min(unnest(values$df)$Temperature),max(unnest(values$df)$Temperature)), 
                    #value = c(min(values$df$Temperature)), 
                    step = 1)
    }) # trim the ends off of the data to improve fitting
    
    
    
    
    ####### Tm calculations
    observeEvent( values$df, {
        print("observed!")
        
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
        
        df # display this dataframe
    },
    options = list(scrollX = TRUE, scrollY = 200, scrollCollapse = TRUE, paging = FALSE, dom = 'tr')) #scroller = TRUE, dom = 'tr'
    

    output$tm_table_render_models <- DT::renderDataTable({ ### new for models
        req(values$df_tm_models_table)
        values$df_tm_models_table
    },
    options = list(scrollX = TRUE, scrollY = 200, scrollCollapse = TRUE, paging = FALSE, dom = 'tr'))
    
    #sigmoid fitting
    #############
    ##### for all models, draw from these starting parameters
    
    # the first things that happen, as soon as the data is uploaded. We may want to wait to trigger this until the sigmoid fitting section is opened, in case doing it this way interferes with the initial plotting, even if no fits are desired
    
    # the following happen automatically, and without user request
    # observeEvent(values$data_raw, {
    #     # creation of data-sepcific functions and values
    #     low_T <- isolate( values$data_raw$Temperature %>% min() )
    #     high_T <- isolate( values$data_raw$Temperature %>% max() )
    #     n_meas <- isolate( values$data_raw$Temperature %>% unique() %>% length())
    #     
    #     #n2r <<- make_temp_n2r(range(low_T:high_T)) #make_temp_n2r(range(values$data$Temperature)) # an example of how this could be used
    #     #win3d <<- 7#floor(3/((n2r(1) - n2r(0))/n_meas))
    #     #win3d <<- floor(3/((n2r(1) - n2r(0))/n_meas))
    #     win3d <<- 7#floor(3/((n2r(1) - n2r(0))/n_meas))
    #     sgfilt_nest <<- sgfilt_set_n(n_ = find_sgolay_width( win3d ))
    #     print("finished with initial uploads")
    #     
    # })
    
    observeEvent(values$df, {
        # print("entering the modeling ring")
        # win3d <<- 7#floor(3/((n2r(1) - n2r(0))/n_meas))
        
        peak_finder_nest <<- make_peak_finder_nest(win3d) ###### brute forced, notice!!!
        
        values$start_pars <- get_start_pars(values$df)
        
        
        # first, fit the s1 model
        # this will over-write the summary dataframes, re-setting the models to follow
        values$s1_list <- model_all(s1_model, "s1_pred", values$start_pars)
        #print(values$s1_list)
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
