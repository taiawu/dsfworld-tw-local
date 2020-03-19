# 0_inegrated_modules/app.R
library(quantmod) # contains the findValleys function, which maybe we should just extract and put verbatim in a source file instead of loading this whole thing...?
library(minpack.lm) # contains the nlsLM function, which we use for our fitting
library(modelr) # used in both the data modeling and the analysis model fitting 
library(SciViews) # contains the ln function used in the data modeling
library(signal) # contains the savistky golay filter (savgolfilt), used to generate the first derivative data in both data modeling and analysis model fitting  

library(shinyBS) # drop-down panels
library(tidyverse) #  handling data structures and plotting

source("scripts/data_modeling.R") # computational models and all associate plots

source("scripts/upload_formatting.R") # functions to assist with raw data uploading
source("scripts/layout_handling.R") # functions to apply layouts to data
source("scripts/plotting.R") # functions to make all plots displayable in the analysis window
source("scripts/analysis.R") # perform Tma analyses by dRFU or curve fitting

library(shinyalert) # pop-up error messages
library(shinycssloaders) # spinning plot loading icon
library(rhandsontable) # user-interactive tables 
library(shiny) # for shiny web-apps


named_mods <- c("s1_pred", "s1_d_pred", "s2_pred", "s2_d_pred") %>% # determine the model names
    set_names("Fit 1", "Fit 2", "Fit 3", "Fit 4")

# generate vectors of possible well names, for matching to layouts
WELLS1 <- make_well_names("ROWS", "1") # create well names, used in the uploading page 
wells_any <- c(WELLS1, # e.g. A1 .... P24
               make_well_names("ROWS", "01"), # e.g. A01 .... P24
               make_well_names("rows", "1"), # e.g. a1 .... p24
               make_well_names("rows", "01") # e.g. a01 .... p24
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
    ########### inter-website navigations 
    observeEvent(input$jumpToAnalysis, {
        updateTabsetPanel(session, "inTabset_analysis",
                          selected = "analysis_tab")
    })
    
    ########### interactive modeling ----------
    df_model <-  reactive({
        #make_model_df(start_T = (273+25), end_T = (273+95), dHu_ = 270, T_half_ = (55+273), dCp_ = 8, Ea_ = 100, T_star_ = 85+273, v_ = 1, nat_dye = 0, unf_dye = 1, fin_dye = 1, decay_rate = 0.8) -> df
        make_model_df(start_T = (273+25), end_T = (273+95), 
                      dHu_ = input$dHu_, 
                      T_half_ = input$T_half_ + 273, #55+273, 
                      dCp_ = input$dCp_, 
                      Ea_ = input$Ea_, 
                      T_star_ = input$T_star_ + 273, 
                      v_ = input$v_, 
                      nat_dye =input$nat_dye_, 
                      unf_dye = input$unf_dye_, 
                      fin_dye = input$fin_dye_, 
                      decay_rate = input$decay_rate_
        )
    })
    
    p_model <- reactive(make_model_plot(df_model()))
    output$plot_model <- renderPlot(p_model())
    ####### interactive modeling 
    
    
    ########## data analysis #########
    values <- reactiveValues() # initalize the reactive values container
    
    ###### data uploading #####
                # inputs: 
                        # input$uploaded_file # the file uploaded by the user
                # outputs: 
                        ## used beginning of the data handling structures
                        # data_raw()
                        # values$data_raw # raw data, retained without labels, layouts, or any modifications past reformatting
                        # values$df  # the active, nested dataframe to which layouts are added. Passed to layouts, plots, and analysis
                        # values$df_1 # the nested dataframe created directly from data_raw() and used to reset values$df_1
                        
                        ## used for model fitting
                        # low_T 
                        # high_T 
                        # n_meas 
                        # n2r 
                        # win3d 
                        # sgfilt_nest 
    
    ####### data upoading functions ####
    output$download_sample_input <- downloadHandler(
        filename = function() {
            paste('dsfworld_upload_format.csv', sep='')
        },
        content = function(file) {
            read_csv("sample_file.csv")
            write.csv(read_csv("sample_file.csv"), file, row.names = FALSE)
        }
    )
    
    data_raw <- reactive({
        print("uploading raw file")
        req(input$uploaded_file)
        file <- input$uploaded_file$datapath
        
        tryCatch({
            
            if (input$reformat == "none" ) {          df <- read.table(input$uploaded_file$datapath, # file
                                                                       header = input$header, # colnames
                                                                       sep = input$sep,
                                                                       quote = input$quote,
                                                                       stringsAsFactors =  FALSE)
            
            } else if (input$reformat == "biorad") { df <- read.table(input$uploaded_file$datapath, # file
                                                                      header = input$header, # colnames
                                                                      sep = input$sep,
                                                                      quote = input$quote,
                                                                      stringsAsFactors =  FALSE) %>% 
                format_biorad()
            
            } else if (input$reformat == "stratagene") { df <- read.table(input$uploaded_file$datapath, # file
                                                                          header = input$header, # colnames
                                                                          sep = input$sep,
                                                                          quote = input$quote,
                                                                          stringsAsFactors =  FALSE) %>% 
                format_stratagene()
            
            } else if (input$reformat == "quantStudio") { df <- read_quantStudio(input$uploaded_file$datapath)
            } else if (input$reformat == "qTower") { df <- read_qTower(input$uploaded_file$datapath) # take path bc read.table will error on this file
            }
            
            df <- df %>% # in case someone has a file that reads in as characters
                mutate_if(is.factor, as.character) %>% # make any factors characters
                mutate_all(as.numeric) # make all numeric
            
            if (input$name_to_well == TRUE) {
                df <- df %>%
                    set_names(c("Temperature", WELLS1[c(1:(ncol(.)-1))]))
            }
            # cycle number to temperature
            if (input$cycle_to_T == TRUE) {
                Temps_calc <- cycle_to_T_func(as.numeric(input$start_T), as.numeric(input$increment_T), df)
                df_cycle <- dplyr::bind_cols(Temps_calc, df)[-2] 
                names(df_cycle)[1] <- "Temperature"
                return(df_cycle)
                
            } else {
                names(df)[1] <- "Temperature"
                df
            }
        },   
        error = function(e){
            shinyalert("This file needs pre-formatting", "Please select your instrument from 'Reformat raw from instrument'. If you don't see your instrument there, please format your data as shown in the downloadable template and upload again.")
            values$data_raw <<- NULL
        }
        )
    }) # read the input file
    
    observeEvent(data_raw(), {
        #values$data_raw <- data_raw()
        
        # set the following values based on the data
        tryCatch({
            low_T <- isolate( data_raw()$Temperature %>% min() )
            high_T <- isolate( data_raw()$Temperature %>% max() )
            n_meas <- isolate( data_raw() %>% nrow() )
            
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
    
    observeEvent(data_raw(), { # ultimately, observe the transfer to the analysis page
        req(data_raw()) # but leave this requirement as is
        tryCatch({
                    values$df <- nest_raw(data_raw()) %>%  add_standardized_wells() # REVISIT1 # active dataframe, used for plotting and calculations
                    values$df_1 <- nest_raw(data_raw()) %>%  add_standardized_wells()  # REVISIT1 # needs an option for when there are non-well names in the data
        }, error = function(e) {
            shinyalert("Please ensure that your data is formatted correctly", "In the 'upload data' tab, you data should be displayed with Temperature in the first column, and RFU data in the columns to the right.")
        })
    })
    
    ### the first steps of the analysis app, to make sure they stitch together ok
    output$input_file <- renderDataTable({
        req(input$uploaded_file)
        tryCatch(
            data_raw(),
            error = function(e){
                shinyalert("File needs pre-formatting!", "Please select your instrument from 'Supported Reformatting'. Or, if you don't see your instrument there, please format your data as shown in the downloadable template and upload again.")
            }
        )
    }, options = list(scrollX = TRUE, scrollY = 500, scrollCollapse = TRUE, paging = FALSE, dom = 't')
    )
    
    ##### end data uploads applet
    
    
    ###### layouts #####
                # inputs: 
                    # input$layout_file # the user-uploaded layout file
                    # values$df_1 # the nested raw data to which the layout information is applied

                # outputs: 
                    # values$df, modified # adds the layout information to values$df_1 to generate values$df
    
    
    ####### layout functions #####
    values$r_table_update <- 1 # initialize the r_table update counter

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
       # write_rds(values$df, "values_df_with_layout.rds")

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
    #### end 2_layouts applet server
    
    
    
    ###### plotting #####
    # inputs: 
    # values$df #
    
    # inputs from the GUI for evals for the plot
    # input$facet                   
    # input$mean_or_each,
    # input$color_by,
    # input$linetype_by,
    # input$use_linetypes,
    # input$facet,
    # input$wrap_by,
    # input$grid_rows,
    # input$grid_cols,
    # plot_title_d(),
    # plot_legend_d(),
    # plot_legend_linetype(),
    # input$fix_free,
    # nput$text_size,
    # legend_position(),
    # input$x_title,
    # input$y_title
    
    # outputs: 
    # plot_initial # a simple, quick-rendering plot
    # plot_updated # the user-made plot from the eval statements
    # fit_plots # the BIC plots of all visualized fits
    # fit_plots_best # the best fits only 
    
    # values$plot_chosen # a string indicating which plot the user has chosen to display 
    # chosen_plot # the plot which will actually be displayed in the window
    
    # output$final_plot
    # output$download_plot # a GUI element
   
    ############### plotting functions #######
    output$update_plot <- renderUI({
        req(values$df)
        actionButton("update_plot", p("Update plot", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),  width = '100%')
    })
    
    # eval sections
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
    
    ##end eval selections
    
    # update and re-render the plot only when the update-plot button is clicked!
    #init_plot <- eventReactive( input$trigger_df_1, { # when new data is uploaded
    plot_initial <- eventReactive( data_raw(),  { # only when the "update plot" button is clicked, update the plot 
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
    
    fit_plots <- eventReactive( input$show_BIC_plot, {
        print("plotting all fits")
        plot_all_fits_shiny(values$df_models_p, values$df_BIC_models_p ) 
    })
    
    fit_plots_best <- eventReactive( input$show_best_fits, {
        print("plotting best fits")
        plot_best_fits_shiny(values$df_models_p, values$df_BIC_best)
    })
    
    values$plot_chosen <- "initial" # the starting value
    observeEvent(values$data_raw, { values$plot_chosen <- "initial"  })    
    observeEvent(input$update_plot, { values$plot_chosen <- "updated"  })
    observeEvent(input$show_BIC_plot, { values$plot_chosen <- "all_model"  })  
    observeEvent(input$show_best_fits, { values$plot_chosen <- "best_model"  })
    
    chosen_plot <- reactive({
        values$plot_chosen
        if (values$plot_chosen == "updated") { # TRUE when the "update plot" button was clicked more recently than new data uploads or "show model plot"
            plot_updated()
        } else if (values$plot_chosen == "all_model") { # TRUE when the "show model plot" button was clicked more recently than new data uploads or "update model plot"
            fit_plots()
        } else if (values$plot_chosen == "best_model") {
            fit_plots_best()
        } else { # if its the initial plot
            plot_initial()  # this is the default
        }
    })
    
    plot_height <- reactive({
        values$plot_chosen
        if (values$plot_chosen == "updated") { # TRUE when the "update plot" button was clicked more recently than new data uploads or "show model plot"
                            #plot_updated()
                            if (input$facet == "none") {
                                height <- 400  # the initial plot has a 400 px height
                            } else {
                                # adapted from https://github.com/rstudio/shiny/issues/650
                                h_dyn <- gg_facet_nrow_ng(plot_updated()) * ((session$clientData$output_plot_width-100)/(gg_facet_ncol_ng(plot_updated())))*(1/1.618)
                                if ( h_dyn < session$clientData$output_plot_width * (1/1.618) ) { # if the calulcated height fits within the screen
                                    height <- session$clientData$output_plot_width * (1/1.618) # if the calulcated height fits within the screen
                                } else { height <- h_dyn }
                            }
                            height # return this
            
        } else if (values$plot_chosen == "all_model") { # TRUE when the "show model plot" button was clicked more recently than new data uploads or "update model plot"
                    # fit_plots()
                    h_dyn <- gg_facet_nrow_ng(fit_plots()) * ((session$clientData$output_plot_width-100)/(gg_facet_ncol_ng(fit_plots())))*(1/1.618)
                    if ( h_dyn < session$clientData$output_plot_width * (1/1.618) ) { # if the calulcated height fits within the screen
                        height <- session$clientData$output_plot_width * (1/1.618) # if the calulcated height fits within the screen
                    } else { height <- h_dyn }
                    height
        } else if (values$plot_chosen == "best_model") {
            # fit_plots_best()
            h_dyn <- gg_facet_nrow_ng(fit_plots_best()) * ((session$clientData$output_plot_width-100)/(gg_facet_ncol_ng(fit_plots_best())))*(1/1.618)
            if ( h_dyn < session$clientData$output_plot_width * (1/1.618) ) { # if the calulcated height fits within the screen
                height <- session$clientData$output_plot_width * (1/1.618) # if the calulcated height fits within the screen
            } else { height <- h_dyn }
            height
        } else { # if its the initial plot
                    400 # return this #plot_initial()  # this is the default
        }
    })
    
    output$plot <- renderPlot({ # is there a way to implement renderCachedPlot that would be worthwhile here?
        chosen_plot()
    }, height = function() 
        plot_height()
    )
    
    output$final_plot <- renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
        req(values$df)
        sliderInput("trim_ends", 
                    p("Restrict fits to a temperature range", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"), 
                    min = min(unnest(values$df)$Temperature), max = max(unnest(values$df)$Temperature),
                    value = c(min(unnest(values$df)$Temperature),
                              max(unnest(values$df)$Temperature)),
                    step = 1)
    }) # trim the ends off of the data to improve fitting
    
    ## download the plot
    output$download_plot <- downloadHandler(
        filename = function() { paste(Sys.Date(), '-dsfworld_plot.pdf', sep='') },
        content = function(file) {
            ggsave(file, plot = chosen_plot(), device = "pdf")
        }
    )

    
    ###### analysis #####
    # inputs: 
    
    
    # outputs: 
    ############### analysis functions
    
}

###### GUI #####
ui <- navbarPage(useShinyalert(),
                 tabPanel(p("-", style = "font-family: 'Avenir Next'; font-size: 20px; color: grey",align = "center"),
                          column(3),
                          column(6,
                                 p("Welcome to DSF world", style = "font-family: 'Avenir Next'; font-size: 30px; color: white",align = "center"),
                                 tags$hr(style="border-color: black;"),
                                 p("Welcome to DSF world", style = "font-family: 'Avenir Next'; font-size: 50px",align = "center"),
                                 tags$hr(style="border-color: black;"),
                                 shiny::div(tags$img(src = "dye_request_image_v0_small.png", width = "500px"), style = "text-align: center;"),
                                 p("Welcome to DSF world", style = "font-family: 'Avenir Next'; font-size: 15px; color: white",align = "center"),
                                 p("This website was created and is maintained by the Gestwicki lab at UCSF.", style = "font-family: 'Avenir Next'; font-size: 12px",align = "center"),
                                 p("email - dsfworlducsf@gmail.com", style = "font-family: 'Avenir Next'; font-size: 12px",align = "center"),
                                 p("twitter - @GestwickiLab", style = "font-family: 'Avenir Next'; font-size: 12px",align = "center")
                          ),
                          column(3)
                 ),
                 tabPanel( p("to interactive modeling", style = "font-family: 'Avenir Next'; font-size: 20px; color: grey",align = "center"), value = "interactive_modeling_mother",
                           tabsetPanel(
                               tabPanel(p("...", style = "font-family: 'Avenir Next'; font-size: 20px; color: grey",align = "center"), value = "modeling_welcome",
                                        column(3),
                                        column(6,
                                               p("   ", style = "font-family: 'Avenir Next'; font-size: 30px",align = "center"),
                                               p("Welcome to interactive modeling!", style = "font-family: 'Avenir Next'; font-size: 30px",align = "center"),
                                               p("DSF is a simple readout for a complex phenomenon. These interactive models showcase how the processes underlying DSF--thermodynamics and kinetics of unfolding, and relative dye detection of various protein states--can ultimately effect the data obtained.", style = "font-family: 'Avenir Next'; font-size: 20px",align = "left")
                                        ),
                                        column(3)
                               ), # end tabPanel
                               # DSF data modeler  -----------------------------------------------------------------------
                               tabPanel(p("interactive modeling", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"), value = "model_plot",
                                        tags$head( # set the slider aesthetic
                                            tags$style(
                                                ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: grey; border-color: transparent;}",
                                                ".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: grey; border-color: transparent;}",
                                                ".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: grey; border-color: transparent;}",
                                                
                                                ".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: grey; border-color: transparent;}",
                                                ".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: grey; border-color: transparent;}",
                                                ".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: grey; border-color: transparent;}",
                                                
                                                ".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: grey; border-color: transparent;}",
                                                ".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: grey; border-color: transparent;}",
                                                ".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar {background: grey; border-color: transparent;}",
                                                ".js-irs-9 .irs-single, .js-irs-9 .irs-bar-edge, .js-irs-9 .irs-bar {background: grey; border-color: transparent;}",
                                                ".js-irs-10 .irs-single, .js-irs-9 .irs-bar-edge, .js-irs-10 .irs-bar {background: grey; border-color: transparent;}"
                                                
                                            )
                                        ),
                                        sidebarLayout(
                                            sidebarPanel(
                                                p("Tune model parameters below", style = "font-family: 'Avenir Next'; font-size: 20px; color: black",align = "center"),
                                                # # "Tune model parameters",
                                                bsCollapse(id = "thermo_pars", open = "Panel 1",
                                                           bsCollapsePanel(p("Thermodynamic parameters", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                                           sliderInput("T_half_", "Thermodynamic melting temperature (C)", min = 25, max = 95, value = 55, step = 1),
                                                                           bsTooltip("T_half_", "At this temperature, the equilibrium ratio of folded to reversibly unfolded states is 1:1.",
                                                                                     "right", options = list(container = "body")),
                                                                           
                                                                           sliderInput("dHu_", "Enthalpy of unfolding (kJ/mol)", min = 1, max = 600, value = 250, step = 10),
                                                                           bsTooltip("dHu_", "Change in enthalpy between the folded and reversibly unfolded state. This model assumes the enthalpy of reversibly and irreversibly unfolded states are equal, a common simplification.",
                                                                                     "right", options = list(container = "body", color = "white")),
                                                                           
                                                                           sliderInput("dCp_", "Change in heat capacity with unfolding (kJ/mol)", min = 0.1, max = 50, value = 8, step = 0.5),
                                                                           bsTooltip("dCp_", "When a protein unfolds, the heat capacity of the solution changes. The magnitude of this change influences the Tm of the protein.",
                                                                                     "right", options = list(container = "body", color = "white"))
                                                                           , style = "default")
                                                           
                                                           
                                                ),
                                                
                                                bsCollapse(id = "kinetic_pars", open = "Panel 2",
                                                           bsCollapsePanel(p("Kinetic parameters", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                                           sliderInput("Ea_", "Activation energy of unfolding (kJ/mol)", min = 10, max = 1000, value = 200, step = 10),
                                                                           bsTooltip("Ea_", "The energy barrier between the reversibly and irreversibly unfolded states. In this model, this value changes with temperature according to the Arrhenius Equation.",
                                                                                     "right", options = list(container = "body")),
                                                                           
                                                                           sliderInput("T_star_", "Temperature at which irreversible unfolding becomes significant, T* (C)", min = 25, max = 95, value = 55, step = 1),
                                                                           bsTooltip("T_star_", "Specifically, the temperature at which the rate constant of irreversible unfolding is 1/min.",
                                                                                     "right", options = list(container = "body")),
                                                                           
                                                                           sliderInput("v_", "DSF experiment heating rate (C/min)", min = 0.1, max = 15, value = 1, step = 0.1),
                                                                           bsTooltip("v_", "The thermocycling ramp rate used in the simulated DSF experiment.",
                                                                                     "right", options = list(container = "body")),
                                                                           style = "default")
                                                ),
                                                
                                                bsCollapse(id = "dye_pars", open = "Panel 3",
                                                           bsCollapsePanel(p("Dye parameters", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"), 
                                                                           # the heading above causes this warning: Warning in if (getAttribs(panels[[i]])$value %in% open) { : the condition has length > 1 and only the first element will be used
                                                                           sliderInput("nat_dye_", "Folded state", min = 0, max = 1, value = 0, step = 0.1),
                                                                           bsTooltip("nat_dye_", "The degree of dye binding and activation observed to the folded state of the protein. Detection of the folded state underlies many high background issues with DSF for hydrophobic proteins.",
                                                                                     "right", options = list(container = "body")),
                                                                           
                                                                           sliderInput("unf_dye_", "Reversibly unfolded state", min = 0, max = 1, value = 1, step = 0.1),
                                                                           bsTooltip("unf_dye_", "The degree of dye binding and activation observed to the reverisibly unfolded state of the protein. Low detection of this state may underlie the invisibility of some proteins in DSF.",
                                                                                     "right", options = list(container = "body")),
                                                                           
                                                                           sliderInput("fin_dye_", "Irreversibly unfolded state", min = 0, max = 1, value = 1, step = 0.1),
                                                                           bsTooltip("fin_dye_", "The degree of dye binding and activation observed to the irreversibly unfolded state of the protein. This parameter is non-zero for most proteins.",
                                                                                     "right", options = list(container = "body")),
                                                                           
                                                                           sliderInput("decay_rate_", "Temperature sensitivity of dye activation", min = 0, max = 1, value = 0.85, step = 0.1),
                                                                           bsTooltip("decay_rate_", "If all dye binding sites remained constant over the experiment, this is the rate at which fluorescence would decrease with temperature. Empirically this is close to 0.8, and is related to the general temperature-sensitivity of fluorophore quantum yields and the strength of hydrophobic effect.",
                                                                                     "right", options = list(container = "body"))
                                                                           
                                                                           ,style = "default")
                                                )
                                                
                                            ),
                                            mainPanel(
                                                p("Welcome to DSF world", style = "font-family: 'Avenir Next'; font-size: 8px; color: white",align = "center"),
                                                shiny::div(tags$img(src = "20191108_dsfworld_modeling_scheme_v0.png", width = "700px"), style = "text-align: center;"),
                                                p("Welcome to DSF world", style = "font-family: 'Avenir Next'; font-size: 8px; color: white",align = "center"),
                                                plotOutput("plot_model", width = "100%", height = "400px")
                                            ))
                                        
                               ), # end fluidRow
                               
                               tabPanel(p("about the model", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"), value = "learn_about_model",
                                        
                                        p("about the model", style = "font-family: 'Avenir Next'; font-size: 25px; color: white",align = "center"),
                                        shiny::div(tags$iframe(src = "dsfworld_about_the_model.pdf", width = "100%", height = "500px"), style = "text-align: center;"),
                                        
                               ))),
                 
                 # Data Analysis --------------------------------------------------------------------------------
                 tabPanel(p("to data analysis", style = "font-family: 'Avenir Next'; font-size: 20px; color: grey",align = "center"), value = "data_analysis_mother", # end tab panel (tabset, div, main still remaining)
                          tabsetPanel(id = "inTabset_analysis", # tabset for all analysis sub-tabs, used by the "jumpToAnalysis" button 
                                      tabPanel(p("1 | upload data", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"), value = "uploads_tab",
                                               
                                               ###### begin UI from uploads applet 
                                               sidebarLayout( # Sidebar layout with input and output definitions
                                                   sidebarPanel(# Sidebar panel for inputs
                                                       #uploading---------------------------
                                                       fileInput("uploaded_file", p("Browse or drag-and-drop raw (RFU) DSF data", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"), # Input: Select a file
                                                                 multiple = FALSE,
                                                                 accept = c("text/csv",
                                                                            "text/comma-separated-values,text/plain",
                                                                            ".csv",
                                                                            ".xls",
                                                                            ".xlsx")),
                                                       
                                                       bsTooltip("help1", HTML("To analyze data, please upload it as a .tsv, .csv, .xls, or .xlsx, formatted with Temperature in the first column and raw fluorescence measurements in the remaining columns. A correctly-formatted example file can be downloaded at left. Minor reformatting can be done after uploading using the Reformatting assistance options. DSFworld can accept and reformat data files exactly as they are exported from the instruments listed in under Supported Reformatting (at left). See the Instructions tab for more information. Incompatible with Explorer versions 9 and earlier."),
                                                                 "right", options = list(container = "body"), trigger = "hover"),
                                                       bsCollapse(id = "upload_help", open = "Panel 1",
                                                                  bsCollapsePanel(p("Uploading instructions", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                                                  p("Upload raw RFU data by either of the following two methods:", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left"),
                                                                                  p("(i) exactly as exported from the instruments listed under 'reformat raw from instrument', below", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left"),
                                                                                  p("(ii) as formatted in the example file, (download it below): a UTF-8 csv file, with Temperature in the first column and RFU data in the columns to the right. ", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left"),
                                                                                  p(" ", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                  p("After uploading, your data will appear in its current format in a table at right. Minor adjustments can then be made, such as replacing cycle number values with Temperature.", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                  p("To use the plate-layout capabilities (e.g. setting replicates and making custom plots) in the analysis window, each data column must be named by well. Most instruments automatically export data with wells as column names, but if necessary, you can artifically write well names onto your data under 'alter delminiters, headers', below", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                  downloadButton("download_sample_input", "Download example file", width = '50%',style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff")
                                                                  )),
                                                       bsCollapse(id = "file_parse_types", open = "Panel 1",
                                                                  bsCollapsePanel(p("Alter delimiters, headers", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                                                  checkboxInput("header", "Header", TRUE), # Input: Checkbox if file has header
                                                                                  checkboxInput("name_to_well", "Overwrite column names with wells", FALSE), # Input: Checkbox if file has header
                                                                                  radioButtons("sep", "Separator",  # Input: Select separator
                                                                                               choices = c(Comma = ",",
                                                                                                           Semicolon = ";",
                                                                                                           Tab = "\t"),
                                                                                               selected = ","),
                                                                                  radioButtons("quote", "Quote",  # Input: Select quotes
                                                                                               choices = c(None = "",
                                                                                                           "Double Quote" = '"',
                                                                                                           "Single Quote" = "'"),
                                                                                               selected = '"'))),
                                                       bsCollapse(id = "instrument_reformatting", open = "Panel 1",
                                                                  bsCollapsePanel(p("Reformat raw from instrument", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                                                  p("Select your instrument from the list below, and upload data exactly as exported.", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left"),
                                                                                  radioButtons("reformat", "", # Input: Select type of reformatting necessary
                                                                                               choices = c(None = "none",
                                                                                                           Biorad = "biorad",
                                                                                                           Stratagene = "stratagene",
                                                                                                           quantStudio = "quantStudio",
                                                                                                           qTower = "qTower" # will have to figure out how to deal with multiple reader errors
                                                                                               ),
                                                                                               selected = "none"))),
                                                       bsCollapse(id = "cycle_to_T_panel", open = "Panel 1",
                                                                  bsCollapsePanel(p("Convert cycle number to temperature", style = "font-family: 'Avenir Next'; font-size: 14px; color: black", align = "center"),
                                                                                  checkboxInput("cycle_to_T", "Convert cycle number to temperature? (if yes, specify below)", FALSE),
                                                                                  textInput(inputId="start_T", label="Starting Temp (C)", value = 25),
                                                                                  textInput(inputId="increment_T", label="Increase per cycle (C)", value = 1)
                                                                  )
                                                       ),
                                                       actionButton('jumpToAnalysis', p("Analyze", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                                    icon("chart-area"), width = '100%', style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff")
                                                       
                                                       
                                                   ),  # end sidebar panel
                                                   
                                                   # Main panel for displaying outputs
                                                   mainPanel(
                                                       tags$style(type='text/css', "#instructions {font-size: 18px; line-height: +2;} "),
                                                       #HTML("instructions"),
                                                       dataTableOutput("input_file"), style = "overflow-x: scroll;"
                                                   ) # end main panel
                                               ) # end sidebarLayout
                                               
                                               ###### end UI from uploads 
                                               
                                      ), # end tabpanel
                                      # analyze and visualize --------------------------- 
                                      tabPanel(p("2 | analyze and visualize", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"), value = "analysis_tab",
                                               
                                               ####### 2_layouts applet GUI
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
                                                                                                  DT::dataTableOutput("tm_table_render"), #style = "height:400px;"
                                                                                                  p("-----", style = "font-family: 'Avenir Next'; font-size: 10px; color: white",align = "center"),
                                                                                                  downloadButton('download_dRFU_tma', "Download Tmas by dRFU"),
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
                                                                                                  p("-----", style = "font-family: 'Avenir Next'; font-size: 10px; color: white",align = "center"),
                                                                                                  downloadButton('download_fit_tma', "Download Tmas from fits"),
                                                                                                  p("-----", style = "font-family: 'Avenir Next'; font-size: 30px; color: white",align = "center"),
                                                                                                  uiOutput("trim_ends"),  
                                                                                                  p(" ", style = "font-family: 'Avenir Next'; font-size: 8px; color: black",align = "center"),
                                                                                                  bsCollapsePanel(p("Select the best fit for each dataset", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                                                                  uiOutput("show_BIC_plot_button"),
                                                                                                                  DT::dataTableOutput("best_model_table"),
                                                                                                                  p("", style = "font-family: 'Avenir Next'; font-size: 8px; color: black",align = "center"),
                                                                                                                  p("Selecting fits", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),
                                                                                                                  
                                                                                                                  p("For each condition, the fit option (1-4) with the lowest Bayesian Information Criterion (BIC) is selected by default. This is meant to maximize model quality without over-fitting (see 'About the analysis').", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "left"),
                                                                                                                  
                                                                                                                  p("", style = "font-family: 'Avenir Next'; font-size: 8px; color: black",align = "center"),
                                                                                                                  
                                                                                                                  p("However, you can select a fit manually by double-clicking on the desired fit in the 'All fits' plot.", style = "font-family: 'Avenir Next'; font-size: 10px; color: black",align = "center"),
                                                                                                                  uiOutput("show_best_fits_button")
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
                                                           downloadButton('download_plot', "Download plot"),
                                                           
                                                           plotOutput("plot", 
                                                                      height = "auto", 
                                                                      dblclick = dblclickOpts(
                                                                          id = "plot_dblclick")) %>% 
                                                               withSpinner(color="#525252"), style = ("overflow-y:scroll; max-height: 600px"),
                                                           verbatimTextOutput("dblclick_info")
                                                           
                                                       ), width = 7)  
                                               )
                                               
                                               ###### end 2_layouts applet GUI
                                      ), # end tabPanel
                                      # Analysis visualzation panel ----------              
                                      tabPanel(p("3 | download results", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"), value = "downloads_tab", 
                                               
                                               ##### begin downloads applet GUI
                                               sidebarLayout(
                                                   # p("Uploading instructions", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                   # p("Upload raw RFU data by either of the following two methods:", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left"),
                                                   # Sidebar panel for inputs 
                                                   sidebarPanel(p("Select and preview downloads", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center") %>% strong(), #HTML("<h2><strong>Select and preview downloads</h2></strong>"),
                                                                p("Plots can be downloaded directly from the analysis window.", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left"), #HTML("<i><h3>To download plots, right-click on them directly in the analysis window and select 'save'. </i></h3>"),
                                                                # Input: Choose dataset 
                                                                selectInput("dataset1", p("Quick results, averaged by user-defined replicates", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left") %>% strong(),
                                                                            choices = c("Tma by dRFU",
                                                                                        "Tma by best fit",
                                                                                        "Replicate-averaged raw data",
                                                                                        "RFU data with fits")
                                                                ),
                                                                textInput("dataset1_download_name", p("Set file name for Quick Result", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left") , value = Sys.Date()),
                                                                downloadButton("downloadData1", "Download quick result"),
                                                                p("----", style = "font-family: 'Avenir Next'; font-size: 20px; color: white",align = "center"),
                                                                
                                                                selectInput("dataset2", p("Supplemental files", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left") %>% strong(),
                                                                            choices = c("Tma by dRFU, no replicate averaging",
                                                                                        "Tma by best fit, no replicate averaging",
                                                                                        "Reformatted raw data",
                                                                                        "Normalized raw data",
                                                                                        "First derivative of raw data") 
                                                                ), 
                                                                
                                                                textInput("dataset2_download_name", p("Set file name for Supplemental File", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left"), value = Sys.Date()),
                                                                downloadButton("downloadData2", "Download supplemental file"),
                                                                p("----", style = "font-family: 'Avenir Next'; font-size: 20px; color: white",align = "center"),
                                                                
                                                                selectInput("dataset3", p("R outputs (.rmd)", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left") %>% strong(),
                                                                            choices = c("Labeled, nested data" ,
                                                                                        "All fitted models, full results" ,
                                                                                        "All fitted models, BIC rankings",
                                                                                        "All fitted models, Tmas" ,
                                                                                        "All fitted models, all outputs")), 
                                                                
                                                                textInput("dataset3_download_name", p("Set file name for rmd", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left"), value = Sys.Date()),
                                                                downloadButton("downloadData3", "Download supplemental file"),
                                                                p("----", style = "font-family: 'Avenir Next'; font-size: 20px; color: white",align = "center")
                                                   ), # end sidebarPanel 
                                                   
                                                   # Main panel for displaying outputs 
                                                   mainPanel(
                                                       tabsetPanel(
                                                           tabPanel("Preview: quick downloads", dataTableOutput("table_set1"), style = "overflow-x: scroll;"),
                                                           tabPanel("Preview: supplemental files", dataTableOutput("table_set2"), style = "overflow-x: scroll;")
                                                       )
                                                   ) # end main panel
                                               ) # end sidebarLayout
                                               
                                               ##### end downloads applet GUI
                                      ), # end tabpanel
                                      # About the analysis ------
                                      tabPanel(p("about the analysis", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"), value = "about_analysis_tab",
                                               shiny::div(tags$iframe(src = "dsfworld_about_the_analysis.pdf", width = "100%", height = "500px"), style = "text-align: center;")
                                      ), # end tabPanel
                                      
                                      
                                      tabPanel(p("instructions", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"), value = "instructions_tab") # end tabPanel
                          )), # end tabset Panel (contains all "analysis sub-panels)
                 tabPanel( p(" . . .", style = "font-family: 'Avenir Next'; font-size: 20px; color: grey",align = "center"), value = "closing_remarks_tab",
                           column(4,
                                  shiny::div(tags$img(src = "dsfworld_logo_grey.png", width = "400px"), style = "text-align: center;")
                           ),
                           column(6,
                                  p("DSFworld was created, and is maintained, by the Gestwicki Lab at the University of California San Francisco.", style = "font-family: 'Avenir Next'; font-size: 20px",align = "left"),
                                  p("We created DSFworld to make DSF more accessible. We're always open to feedback, so don't hesitate to email dsfworlducsf (at) gmail (dot) com if you have comments or questions.", style = "font-family: 'Avenir Next'; font-size: 15px",align = "left"),
                                  p("DSFworld is written in R, as are all associated scripts. Therefore, DSFworld is indebted to the creators of R, RShiny, and the many software packages used herein. These packages are: shiny, shinydashboard, shinythemes, shinyjs, rhandsontable, shinyWidgets, shinyBS, DT;  ggplot2, platetools, viridis;  readxl, readr, signal, stats,  matrixStats, SciViews, nnet, baseline, peakPick, naniar;  tidyr, dplyr, purrr, tibble, base, magrittr, and reshape2; please forgive our crude implementations. Thanks also to the countless users of StackOverflow and GitHub, whose questions and answers smoothed many hurdles during the creation of this website. ", style = "font-family: 'Avenir Next'; font-size: 15px",align = "left"),
                                  p("Thank you also to our many dedicated beta testers, without whom using this application would likely be a terrible experience; you know who you are.", style = "font-family: 'Avenir Next'; font-size: 15px",align = "left")
                           ),
                           column(2)))
###### END GUI #####

shinyApp(ui = ui, server = server)
