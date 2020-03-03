ui <- navbarPage(
    useShinyalert(),
                                      tabPanel(
                                          p("1 | upload data", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"), value = "uploads_tab",
                                               sidebarLayout( # Sidebar layout with input and output definitions
                                                   sidebarPanel(# Sidebar panel for inputs
                                                       #uploading---------------------------
                                                       fileInput("uploaded_file", p("Choose or drag-and-drop raw DSF data", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"), # Input: Select a file
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
                                                                                  downloadButton("samplefile", "Download example file", width = '50%',style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff")
                                                                  )),
                                                       bsCollapse(id = "file_parse_types", open = "Panel 1",
                                                                  bsCollapsePanel(p("Alter delimiters, headers", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                                                  checkboxInput("header", "Header", TRUE), # Input: Checkbox if file has header
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
                                                                                                           # qTower = "qTower", # will have to figure out how to deal with multiple reader errors
                                                                                                           Biorad = "biorad",
                                                                                                           Stratagene = "stratagene",
                                                                                                           quantStudio = "Quant Studio (must select to display data)"
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

                                      )
                 )

server <- function(input, output) {
    
        output$input_file <- renderDataTable({
            req(input$uploaded_file)
            
            tryCatch(
                expr = {
                    read.csv(input$uploaded_file$datapath, header = input$header)
                },
                error = function(e){ 
                    shinyalert("Oops!", "Something went wrong.")
                }
                
            )
            
                

        }, # render the input file data table
        options = list(scrollX = TRUE, scrollY = 500, scrollCollapse = TRUE, paging = FALSE, dom = 't'))
}

shinyApp(ui, server)

# library(shiny)
# library(shinyalert)
# 
# ui <- fluidPage(
#     useShinyalert(),  # Set up shinyalert
#     actionButton("preview", "Preview")
# )
# 
# server <- function(input, output, session) {
#     observeEvent(input$preview, {
#         # Show a modal when the button is pressed
#         shinyalert("Oops!", "Something went wrong.")
#     })
# }
# 
# shinyApp(ui, server)

# # perhaps we could put the loading of packages into an observe event, to keep them all froam loading right at the start. would this make the first page faster?
# library(quantmod) # contains the findValleys function, which maybe we should just extract and put verbatim in a source file instead of loading this whole thing...?
# library(minpack.lm) # contains the nlsLM function, which we use for our fitting
# library(modelr) # used in both the data modeling and the analysis model fitting
# library(SciViews) # contains the ln function used in the data modeling
# library(signal) # contains the savistky golay filter (savgolfilt), used to generate the first derivative data in both data modeling and analysis model fitting
# library(assertive.types) # for dynamic sizing of the facet plots
# library(shinyBS) # drop-down panels
# library(tidyverse) #  handling data structures and plotting
# library(shinycssloaders) # spinning plot loading icon
# library(rhandsontable) # user-interactive tables
# library(shiny) # for shiny web-apps
# 
# # on start-up, this app produces multiple of the following error
# # Warning in if (getAttribs(panels[[i]])$value %in% open) { :
# #         the condition has length > 1 and only the first element will be used
# 
# # this error appears to correspond at least to the use of p() arguments as bscollapse panel names. p() returns something with multiple elements (a list?), including p, the text, the style, as individual elements.
# # regardless the warning, it uses them all, and this is an easy way to style the buttom label appearance, so we'll take it
# 
# ui <- navbarPage(
#     useShinyalert(),
#                                       tabPanel(
#                                           p("1 | upload data", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"), value = "uploads_tab",
#                                                sidebarLayout( # Sidebar layout with input and output definitions
#                                                    sidebarPanel(# Sidebar panel for inputs
#                                                        #uploading---------------------------
#                                                        fileInput("uploaded_file", p("Choose or drag-and-drop raw DSF data", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"), # Input: Select a file
#                                                                  multiple = FALSE,
#                                                                  accept = c("text/csv",
#                                                                             "text/comma-separated-values,text/plain",
#                                                                             ".csv",
#                                                                             ".xls",
#                                                                             ".xlsx")),
#                                                        # div(style="display:inline-block",downloadButton("samplefile", "Download example file", width = '50%',style="font-size: 11px; color: #fff;")),
#                                                        # div(style="display:inline-block", actionButton('help1', HTML('<strong><i>Help</strong></i>'), style = "color: #2c3e50; background-color: #ecf0f1; border-color:#ecf0f1")),
# 
#                                                        bsTooltip("help1", HTML("To analyze data, please upload it as a .tsv, .csv, .xls, or .xlsx, formatted with Temperature in the first column and raw fluorescence measurements in the remaining columns. A correctly-formatted example file can be downloaded at left. Minor reformatting can be done after uploading using the Reformatting assistance options. DSFworld can accept and reformat data files exactly as they are exported from the instruments listed in under Supported Reformatting (at left). See the Instructions tab for more information. Incompatible with Explorer versions 9 and earlier."),
#                                                                  "right", options = list(container = "body"), trigger = "hover"),
#                                                        bsCollapse(id = "upload_help", open = "Panel 1",
#                                                                   bsCollapsePanel(p("Uploading instructions", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
#                                                                                   downloadButton("samplefile", "Download example file", width = '50%',style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff")
#                                                                   )),
#                                                        bsCollapse(id = "file_parse_types", open = "Panel 1",
#                                                                   bsCollapsePanel(p("Alter delimiters, headers", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
#                                                                                   checkboxInput("header", "Header", TRUE), # Input: Checkbox if file has header
#                                                                                   radioButtons("sep", "Separator",  # Input: Select separator
#                                                                                                choices = c(Comma = ",",
#                                                                                                            Semicolon = ";",
#                                                                                                            Tab = "\t"),
#                                                                                                selected = ","),
#                                                                                   radioButtons("quote", "Quote",  # Input: Select quotes
#                                                                                                choices = c(None = "",
#                                                                                                            "Double Quote" = '"',
#                                                                                                            "Single Quote" = "'"),
#                                                                                                selected = '"'))),
#                                                        bsCollapse(id = "instrument_reformatting", open = "Panel 1",
#                                                                   bsCollapsePanel(p("Reformat raw from instrument", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
#                                                                                   p("Select your instrument from the list below, and upload data exactly as exported.", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left"),
#                                                                                   radioButtons("reformat", "", # Input: Select type of reformatting necessary
#                                                                                                choices = c(None = "none",
#                                                                                                            # qTower = "qTower", # will have to figure out how to deal with multiple reader errors
#                                                                                                            Biorad = "biorad",
#                                                                                                            Stratagene = "stratagene",
#                                                                                                            quantStudio = "Quant Studio (must select to display data)"
#                                                                                                ),
#                                                                                                selected = "none"))),
#                                                        bsCollapse(id = "cycle_to_T_panel", open = "Panel 1",
#                                                                   bsCollapsePanel(p("Convert cycle number to temperature", style = "font-family: 'Avenir Next'; font-size: 14px; color: black", align = "center"),
#                                                                                   checkboxInput("cycle_to_T", "Convert cycle number to temperature? (if yes, specify below)", FALSE),
#                                                                                   textInput(inputId="start_T", label="Starting Temp (C)", value = 25),
#                                                                                   textInput(inputId="increment_T", label="Increase per cycle (C)", value = 1)
#                                                                                   )
#                                                                   ),
#                                             
#                                                        actionButton('jumpToAnalysis', p("Analyze", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
#                                                                     icon("chart-area"), width = '100%', style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff")
# 
#                                                      
#                                                    ),  # end sidebar panel
# 
#                                                    # Main panel for displaying outputs
#                                                    mainPanel(
#                                                        tags$style(type='text/css', "#instructions {font-size: 18px; line-height: +2;} "),
#                                                        #HTML("instructions"),
#                                                        dataTableOutput("input_file"), style = "overflow-x: scroll;"
#                                                    ) # end main panel
#                                                ) # end sidebarLayout
# 
#                                       )
#                  )
# 
# # Define server logic required to draw a histogram
# server <- function(session, input, output) {
# 
#     ########### data uploading  ----------
#     # input: raw user data, in various formats
#     # output: by_variable, a nested dataframe containing all of the formatted data, ready for analysis and visualization
# 
#     values <- reactiveValues() # initalize the reactive values container (is this effectively a class? is shiny OOP-like...?) wish i'd realized that earlier)
#     
#     # data_raw <- reactive({
#     #     req(input$uploaded_file)
#     #     tryCatch(
#     #         expr = {
#     #             df_input <- read_table(input$uploaded_file$datapath, # file
#     #                                    header = input$header, # colnames
#     #                                    sep = input$sep,
#     #                                    quote = input$quote,
#     #                                    stringsAsFactors =  FALSE)
#     #             
#     #             df_input
#     #         },
#     #         error = function(e) {
#     #             # shinyalert("Oops!", "Something went wrong.")
#     #             # if ( input$reformat == "quantStudio") {df_input <- read_quantStudio(input$uploaded_file$datapath)
#     #             # } else if ( input$reformat == "qTower") {df_input <- format_none(df_input)
#     #             # } else if ( input$reformat == "biorad") {df_input <- format_none(df_input)
#     #             # } else if ( input$reformat == "biorad") {df_input <- format_stratagene(df_input) }
#     #         }
#     #         
#     #     )
#     # 
#     #         # tryCatch(  # try reading the file directly into a tibble; this works for most files--not qTower or quantStudio
#     #         #     df_input <- read_table(input$uploaded_file$datapath, # file
#     #         #                            header = input$header, # colnames
#     #         #                            sep = input$sep,
#     #         #                            quote = input$quote,
#     #         #                            stringsAsFactors =  FALSE),
#     #         #     error = function(c) "Can't read the uploaded file!"
#     #         #     )
#     #         # 
#     #         # 
#     #         # 
#     #         # 
#     #         #     if (input$reformat == "quantStudio") {
#     #         #         df_input <- read_quantStudio(input$uploaded_file$datapath)
#     #         #     } else {
#     #         #         df_input <- read.table(input$uploaded_file$datapath, # file
#     #         #                                header = input$header, # colnames
#     #         #                                sep = input$sep,
#     #         #                                quote = input$quote,
#     #         #                                stringsAsFactors =  FALSE)
#     #         #     }
#     #         # 
#     #         #     # supported instrument reformatting
#     #         #     if (input$reformat == "none") { df <- format_none(df_input)
#     #         #     } else if (input$reformat == "qTower") { df <- format_none(df_input)
#     #         #     } else if (input$reformat == "biorad") { df <- format_biorad(df_input)
#     #         #     } else if (input$reformat == "stratagene") {df <- format_stratagene(df_input)
#     #         #     }
#     #         # 
#     #         #     df <- df %>%
#     #         #         mutate_all(as.numeric)
#     #         # 
#     #         #     # cycle number to temperature
#     #         #     if (input$cycle_to_T == TRUE) {
#     #         #         Temps_calc <- cycle_to_T_func(as.numeric(input$start_T), as.numeric(input$increment_T), df)
#     #         #         df <- dplyr::bind_cols(Temps_calc, df)[-2]
#     #         #     }
#     #         # 
#     #         #     df
#     #         # 
#     #         # } ,
#     #         # error = function(e) {
#     #         #     renderText({
#     #         #         stop(
#     #         #             "error in uploading"
#     #         #         )
#     #         #     })
#     #         # })
#     # }) # read the input file
#     # 
#     # 
#     # observeEvent(data_raw(), {
#     #     values$data_raw <- data_raw()
#     # }) # write to values
#     # 
#     # output$input_file <- renderDataTable({
#     #     req(input$uploaded_file) # only render if there is an uploaded file
#     #             tryCatch(
#     #                 expr = {
#     #                     return(values$data_raw)
#     #                 },
#     #                 error = function(e) {
#     #                     shinyalert("Oops!", "Something went wrong.")
#     #                 }
#     #                 
#     #             )
#     # }, # render the input file data table
#     # options = list(scrollX = TRUE, scrollY = 500, scrollCollapse = TRUE, paging = FALSE, dom = 't'))
#     
#     values <- reactiveValues() # initalize the reactive values container (is this effectively a class? is shiny OOP-like...?) wish i'd realized that earlier)
#     data_raw <- reactive({
#         req(input$uploaded_file)
#         tryCatch(
#             {
#                 if (input$reformat == "quantStudio") {
#                     df_input <- read_quantStudio(input$uploaded_file$datapath)
#                 } else {
#                     df_input <- read.table(input$uploaded_file$datapath, # file
#                                            header = input$header, # colnames
#                                            sep = input$sep,
#                                            quote = input$quote,
#                                            stringsAsFactors =  FALSE)
#                 }
#                 
#                 # supported instrument reformatting
#                 if (input$reformat == "none") { df <- format_none(df_input)
#                 } else if (input$reformat == "qTower") { df <- format_none(df_input)
#                 } else if (input$reformat == "biorad") { df <- format_biorad(df_input)
#                 } else if (input$reformat == "stratagene") {df <- format_stratagene(df_input)
#                 } else if (input$reformat == "quantStudio") { df <- format_none(df_input) }
#                 
#                 df <- df %>%
#                     mutate_all(as.numeric)
#                 
#                 # cycle number to temperature
#                 if (input$cycle_to_T == TRUE) {
#                     Temps_calc <- cycle_to_T_func(as.numeric(input$start_T), as.numeric(input$increment_T), df)
#                     df <- dplyr::bind_cols(Temps_calc, df)[-2]
#                 }
#                 
#                 df
#                 
#             } ,
#             error = function(e) {
#                 renderText({
#                     stop(
#                         "error in uploading"
#                     )
#                 })
#             })
#     }) # read the input file
#     
#     observeEvent(data_raw(), {
#         values$data_raw <- data_raw()
#     }) # write to values
#     
#     output$input_file <- renderDataTable({
#         infile <- input$uploaded_file
#         if (is.null(infile) == TRUE ) { # if there is no input file
#             return(NULL) # leave "contents" empty (display instead the uploading instructions)
#         } else {
#             return((values$data_raw)) # otherwise, assign the uploaded file to data_raw()
#         }
#     }, # render the input file data table
#     options = list(scrollX = TRUE, scrollY = 500, scrollCollapse = TRUE, paging = FALSE, dom = 't'))
#  } # end server
# # Run the application
# shinyApp(ui = ui, server = server)
