# all modules combined
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

WELLS1 <- make_well_names("ROWS", "1")
wells_any <- c(WELLS1, # e.g. A1 .... P24
               make_well_names("ROWS", "01"), # e.g. A01 .... P24
               make_well_names("rows", "1"), # e.g. a1 .... p24
               make_well_names("rows", "01") # e.g. a01 .... p24
)

named_mods <- c("s1_pred", "s1_d_pred", "s2_pred", "s2_d_pred") %>%
    set_names("Fit 1", "Fit 2", "Fit 3", "Fit 4")

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
                          tabsetPanel(id = "inTabset_analysis", # tabset for all analysis sub-tabs
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


# Define server logic required to draw a histogram
server <- function(session, input, output) {
    values <- reactiveValues()
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
    
    ####### uploading ####
    ########### data uploading  ----------
    # download a sample file
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
        values$data_raw <- data_raw()
        
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
    
    
    ### the first steps of the analysis app, to make sure they stitch together ok
    output$input_file <- renderDataTable({
        req(input$uploaded_file)
        tryCatch(
            values$data_raw,
            error = function(e){
                shinyalert("File needs pre-formatting!", "Please select your instrument from 'Supported Reformatting'. Or, if you don't see your instrument there, please format your data as shown in the downloadable template and upload again.")
            }
        )
    }, options = list(scrollX = TRUE, scrollY = 500, scrollCollapse = TRUE, paging = FALSE, dom = 't')
    )
    ###### uploading  ####
    
    
} # end server
# Run the application 
shinyApp(ui = ui, server = server)
