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
    
    ########### data uploading  ----------
    values <- reactiveValues() # initalize the reactive values container (is this effectively a class? is shiny OOP-like...?) wish i'd realized that earlier)
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
            
            } else if (input$reformat == "quantStudio") { df <- format_quantStudio(input$uploaded_file$datapath)
            } else if (input$reformat == "qTower") { df <- format_qTower(input$uploaded_file$datapath) # take path bc read.table will error on this file
            }
            
            df <- df %>% # in case someone has a file that reads in as characters
                mutate_if(is.factor, as.character) %>% # make any factors characters
                mutate_all(as.numeric) # make all numeric
            
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
            shinyalert("File needs pre-formatting!", "Please select your instrument from 'Supported Reformatting'. Or, if you don't see your instrument there please format your data as shown in the downloadable template and upload again.")
            values$data_raw <<- NULL
        }
        )
    }) # read the input file
    
    observeEvent(data_raw(), {
        values$data_raw <- data_raw()
    }) # write to values
  
        output$input_file <- renderDataTable({
            req(input$uploaded_file)
            tryCatch(
                values$data_raw,
                error = function(e){
                    shinyalert("File needs pre-formatting!", "Please select your instrument from 'Supported Reformatting'. Or, if you don't see your instrument there please format your data as shown in the downloadable template and upload again.")
                }
            )
        }, options = list(scrollX = TRUE, scrollY = 500, scrollCollapse = TRUE, paging = FALSE, dom = 't')
        )
}

shinyApp(ui, server)
