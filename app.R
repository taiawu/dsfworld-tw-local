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

library(shinycssloaders) # spinning plot loading icon
library(rhandsontable) # user-interactive tables 
library(shiny) # for shiny web-apps 

named_mods <- c("s1_pred", "s1_d_pred", "s2_pred", "s2_d_pred") %>%
    set_names("Model 1", "Model 2", "Model 3", "Model 4")

ui <- navbarPage("",
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
                                                # 
                                                # bsCollapse(id = "lig_pars", open = "Panel 3",
                                                #            bsCollapsePanel("Dye",
                                                #                            sliderInput("comp_nat_dye_", "Competition with dye for the native state", min = 0, max = 1, value = 0, step = 0.1),
                                                #                            bsTooltip("comp_nat_dye_", "Sometimes DSF dyes bind the active site of a folded protein, competing with a non-dye ligand.",
                                                #                                      "right", options = list(container = "body")),
                                                #                            
                                                #                            sliderInput("comp_unf_dye", "Competition with dye for the reversibly unfolded state", min = 0, max = 1, value = 1, step = 0.1),
                                                #                            bsTooltip("comp_unf_dye_", "Some small molecules may compete with dye for binding to reversibly unfolded states, impacting resulting DSF data.",
                                                #                                      "right", options = list(container = "body")),
                                                #                            
                                                #                            sliderInput("comp_fin_dye_", "Irreversibly unfolded state", min = 0, max = 1, value = 1, step = 0.1),
                                                #                            bsTooltip("comp_fin_dye_", "Some small molecules may compete with dye for binding to irreversibly unfolded states, impacting resulting DSF data.",
                                                #                                      "right", options = list(container = "body")),
                                                #                            
                                                #                            sliderInput("dHu_bind_", "Enthalpy of ligand binding (kJ/mol)", min = 0, max = 1, value = 0, step = 0.1),
                                                #                            sliderInput("Ea_bind_", "Change in unfolding activation energy upon dye binding (kJ/mol)", min = 0, max = 0, value = 1, step = 0.1),
                                                #                            # bsTooltip("dHu_bind_", "Some small molecules may compete with dye for binding to irreversibly unfolded states, impacting resulting DSF data.",
                                                #                            #           "right", options = list(container = "body")),
                                                #                            
                                                #                            sliderInput("decay_rate_", "Temperature sensitivity of dye activation", min = 0, max = 1, value = 0.85, step = 0.1),
                                                #                            bsTooltip("decay_rate_", "If all dye binding sites remained constant, the rate at which fluorescence would decrease with temperature. Empirically, this is close to 0.8, and related to the general temperature-sensitivity of the strength of hydrophobic effect and fluorophore quantum yields.",
                                                #                                      "right", options = list(container = "body"))
                                                #                            ,style = "default")
                                                # )
                                            ),
                                            mainPanel(
                                                p("Welcome to DSF world", style = "font-family: 'Avenir Next'; font-size: 8px; color: white",align = "center"),
                                                shiny::div(tags$img(src = "20191108_dsfworld_modeling_scheme_v0.png", width = "700px"), style = "text-align: center;"),
                                                p("Welcome to DSF world", style = "font-family: 'Avenir Next'; font-size: 8px; color: white",align = "center"),
                                                plotOutput("plot_model", width = "100%", height = "400px")
                                            ))
                                        
                               ), # end fluidRow
                               
                               tabPanel(p("about the model", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"), value = "learn_about_model",
                                        column(2),
                                        column(7,
                                               p("about the model", style = "font-family: 'Avenir Next'; font-size: 25px; color: white",align = "center"),
                                               withMathJax(
                                                   #helpText('DSF is a simple readout for a complex phenomenon. This portion of the website is meant to showcase how the processes underlying DSF--thermodynamics and kinetics of unfolding, and relative dye detection of various protein states--can ultimately effect the data obtained.'),
                                                   helpText(''),
                                                   helpText('The unfolding of most proteins is highly complex, with many potential unfolded states and as many or more pathways to get to them. However, many simplified models of unfolding have been described, the simplest of which is:'),
                                                   helpText('$${Native \\ (folded) \\rightleftharpoons Unfolded}$$[model 1]'),
                                                   helpText('In model 1, the unfolding transition is reversible, and the folded and unfolded state are assumed to be at equilibrium. That is, model 1 is a simple two-state thermodynamic system.'),
                                                   helpText('However, when proteins denature upon heating (like in a DSF experiment) the kinetcs of unfolding often plays an important role as well. Therefore, the relative population of each state is dependent on not only the thermodynamics of the protein fold but also its kinetics, as well as the temperature, and the ramp rate used to melt the protein.'),
                                                   helpText('This mixed thermodynamic-kinetic process of unfolding is described in a series of models called Lumry-Eyring models, which are distinct in that they contain both reversible and irreversible steps. The DSF data modeling in this section is based on the simplest of these Lumry-Eyring models:'),
                                                   helpText('$${Native \\rightleftharpoons\\ reversible \\ unfolded \\longrightarrow\\ irreversible \\ unfolded}$$[model 2] (the one used in this data modeling section)'),
                                                   helpText('An aside: the influence of kinetics on heat denaturation has some important (if theoretical) rammifications: because the unfolding process is not purely thermodynamic, it is unwise to assume that the folded:unfolded ratio observed at any given temperature is representative of the equilibrium value. Therefore, because the definition of a protein Tm is the temperature at which the \\(\\Delta G\\) of folding is zero, and the protein is therefore 50% folded, 50% unfolded at equilibrium, the Tm determined with any heat-based denaturation method (not just DSF; also heat-based circular dichroism and differential scanning calorimetry), is technically a "Tm apparent", not a pure Tm.'),
                                                   helpText(''),
                                                   helpText('The first plot in this DSF data model, which describes the relative population of folded, reversibly-unfolded, and irreversibly-unfolded states, is based on a set of theoretical papers which modeled Differential Scanning Calorimetry based on both thermodynamic and kinetic properties of a protein. The equations and their full derivations can be found at (cite!), but for convenience, are summarized below.'),
                                                   helpText('$${Relative \\ population \\ of \\ the \\ native \\ state: N(T)} =  \\frac{1}{K+ 1} - e^{L(T)}$$      [Eq. 1]'),
                                                   helpText('$${Relative \\ population \\ of \\ the \\ reversibly \\ unfolded \\ state:  U(T)} =  \\frac{K}{K+ 1} - e^{L(T)}$$      [Eq. 2]'),
                                                   helpText('$${Relative \\ population\\  of\\  the\\  irreversibley \\ unfolded\\  state: F(T)} =  1 - e^{L(T)}$$      [Eq. 3]'),
                                                   helpText('Where $${L(T) = } -\\frac{1}{v}\\int_{T_o}^{T}\\frac{kK}{K+ 1} \\,dT$$'), 
                                                   helpText('And where the equilibrium constant K is described in the typical manner: $${K = } \\frac{[U]}{[N]} = \\frac{X_U}{X_N} = e^{- \\frac{\\Delta H_U}{R}  \\left[\\frac{1}{T} - \\frac{1}{T_{1/2}}\\right]}$$      [Eq. 4]'),
                                                   helpText('And where dependence of the rate constant k on temperature is described by the Arrhenius equation: $${k = } e^{-\\frac{E}{R}  \\left[\\frac{1}{T} - \\frac{1}{T^*}\\right] }$$      [Eq. 5]'),
                                                   helpText('From equations [1] - [5], we get the relative population of each of the three folding states.'),
                                                   helpText('For the DSF model presented here, these relative populations are then multiplied by a constant which describes the degree to which that state is detected by the DSF dye. This constant encompasses both binding and associated increase in fluorescence as these two parameters are not distinguished in a typical DSF experiment.'),
                                                   helpText('Theoretically, the dye-detection constant is zero for the native state, one for the unfolded state, and zero again for the final state.'),
                                                   helpText('In reality, these constants vary from protein to protein, and these variations can influence the DSF data obtained even for proteins with identical relative population of folded states at a given temperature.'),
                                                   helpText('This influence is calculate by the following equations:'),
                                                   helpText('$${Detection \\ of \\ the \\ native \\ state: RFU_{Native}(T, t) = S(T)[n_N * N(T, t)]} $$      [Eq. 6]'),
                                                   helpText('$${Detection \\ of \\ the \\ reversible \\ unfolded \\ state: RFU_{Unfolded}(T, t) = S(T)[u_N * U(T, t)]}$$      [Eq. 7]'),
                                                   helpText('$${Detection \\ of \\ the \\ irreversible \\ unfolded \\ state: RFU_{Final}(T, t) = S(T)[f_N * F(T, t)]} $$      [Eq. 8]'),
                                                   helpText('Where T is temperature, and t is time (as the protein unfolding state at a particular temperature can depend on ramp rate)'),
                                                   helpText('And where S(T) is a linear function which corrects for the experimentally observed decay in fluorescence of SYPRO Orange with temperature, independent of the number or type of binding sites available to it:'),
                                                   helpText('$${S(T) = }1- 0.9 \\left[\\frac{T}{70}\\right] $$      [Eq. 9]'),
                                                   helpText('The single line on the second plot is the sum of equations 6-8, and is meant to represent the DSF data observed for the unfolding process described in the first plot. That is:'),
                                                   helpText('$${RFU_{total}(T, t) = RFU_{Native}(T, t) + RFU_{Unfolded}(T, t) + RFU_{Final}(T, t)} $$      [Eq. 8]'),
                                                   helpText('The third and final plot is simply the first derivative of the second plot (equations 6-8), as the first derivative if often used to calculate the apparent Tm of the protein.')
                                               ), style = "overflow-y:scroll; max-height: 500px"), # end tabset Panel
                                        column(3)
                               ))),
                 
                 #tabPanel(p("to data analysis", style = "font-family: 'Avenir Next'; font-size: 20px; color: grey",align = "right")),
                 # Data Analysis --------------------------------------------------------------------------------
                 tabPanel(p("to data analysis", style = "font-family: 'Avenir Next'; font-size: 20px; color: grey",align = "center"), value = "data_analysis_mother", # end tab panel (tabset, div, main still remaining)
                          tabsetPanel(id = "inTabset_analysis", # tabset for all analysis sub-tabs
                                      tabPanel(p("1 | upload data", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"), value = "uploads_tab",
                                               
                                               sidebarLayout( # Sidebar layout with input and output definitions
                                                   sidebarPanel(# Sidebar panel for inputs
                                                       # uploading---------------------------                                     
                                                       fileInput("uploaded_file", p("Choose or drag-and-drop raw DSF data", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"), # Input: Select a file
                                                                 multiple = FALSE,
                                                                 accept = c("text/csv",
                                                                            "text/comma-separated-values,text/plain",
                                                                            ".csv",
                                                                            ".xls",
                                                                            ".xlsx")),
                                                       # div(style="display:inline-block",downloadButton("samplefile", "Download example file", width = '50%',style="font-size: 11px; color: #fff;")),
                                                       # div(style="display:inline-block", actionButton('help1', HTML('<strong><i>Help</strong></i>'), style = "color: #2c3e50; background-color: #ecf0f1; border-color:#ecf0f1")),
                                                       
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
                                                                  bsCollapsePanel(p("Convert cycle number to temperature", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                                                  checkboxInput("cycle_to_T", "Convert cycle number to temperature? (if yes, specify below)", FALSE),
                                                                                  textInput(inputId="start_T", label="Starting Temp (C)", value = 25),
                                                                                  textInput(inputId="increment_T", label="Increase per cycle (C)", value = 1))),
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
                                               
                                      ), # end tabpanel
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
                                                                  bsCollapsePanel(p("Make plots", style = "font-family: 'Avenir Next'; font-size: 16px; color: black",align = "center"),
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
                                                                                  # checkboxInput("mean_or_each", "Show replicates as averaged", FALSE),
                                                                                  # checkboxInput("show_Tm_lollipops", "Show apparent Tm", FALSE),
                                                                                  # checkboxInput("fix_free", "Equalize y axes of sub-plots", FALSE),
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
                                                       
                                                       # bsCollapse(id = "tm_table", open = "Panel 2",
                                                       #            bsCollapsePanel(p("Apparent Tms", style = "font-family: 'Avenir Next'; font-size: 16px; color: black",align = "center"),
                                                       #                            bsCollapsePanel(p("By dRFU", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                       #                                            #checkboxInput("show_Tm_lollipops_dRFU", "Show Tm' on plot", FALSE),
                                                       #                                            DT::dataTableOutput("tm_table_render")
                                                       #                            ),
                                                       #                            style = "default")
                                                       # ),
                                                       # 
                                                       
                                                       bsCollapse(id = "tm_table", open = "Panel 2",
                                                                  bsCollapsePanel(p("Find apparent Tms", style = "font-family: 'Avenir Next'; font-size: 16px; color: black",align = "center"),
                                                                                  bsCollapsePanel(p("By dRFU", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                                                                  #splitLayout(cellWidths = c("50%", "50%"),
                                                                                                  checkboxInput("show_Tm_dRFU", "Show Tm' on plot", FALSE),
                                                                                                  checkboxInput("show_Tm_dRFU_colors", "Apply colors  to Tm'", FALSE),
                                                                                                  # checkboxInput("show_Tm_lollipops_dRFU", HTML("Show Tm' <br/> on plot"), FALSE),
                                                                                                  # checkboxInput("show_Tm_lollipops_dRFU2", HTML("Apply colors <br/>  to Tm'"), FALSE),
                                                                                                  # # ,
                                                                                                  # uiOutput("update_model_plots")
                                                                                                  #),
                                                                                                  # checkboxInput("show_Tm_lollipops_dRFU", "Show Tm' on plot", FALSE),
                                                                                                  # checkboxInput("show_Tm_lollipops_dRFU2", HTML("Apply colors to Tm'"), FALSE),
                                                                                                  DT::dataTableOutput("tm_table_render"), #style = "height:400px;"
                                                                                                  #uiOutput("update_dRFU_plots"),
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
                                                           #plotOutput("facet_plot_re_examined")
                                                           
                                                           tags$style(HTML('#q1 {margin-top: 30px}')),
                                                           splitLayout(cellWidths = c("20%", "80%"), 
                                                                       plotDownloadUI("plot1"), 
                                                                       textInput("plot_download_name", "Downloaded plot name", value = "dsfworld_plot")
                                                           ),
                                                           plotOutput("data", height = "auto") %>% withSpinner(color="#525252"), style = ("overflow-y:scroll; max-height: 600px") 
                                                           # p <- plotOutput("data", height = "auto") 
                                                           # if (values$df != NULL) { p <- p %>% withSpinner(color="#525252") }
                                                           # , 
                                                           # style = ("overflow-y:scroll; max-height: 600px") 
                                                           #https://cran.r-project.org/web/packages/periscope/vignettes/downloadablePlot-module.html ## this might help with the downloading
                                                       ))
                                               )
                                      ), # end tabPanel
                                      # Analysis visualzation panel ----------              
                                      tabPanel(p("3 | download results", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"), value = "downloads_tab", 
                                               sidebarLayout(
                                                   # Sidebar panel for inputs 
                                                   sidebarPanel(p("Select and preview downloads", style = "font-family: 'Avenir Next'; font-size: 16px; color: black",align = "center") %>% strong,
                                                                p("Plots can be downloaded from the analysis window.", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                # Input: Choose dataset 
                                                                selectInput("dataset1", p("Quick results, replicates averaged", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                                            choices = c("the data")),
                                                                #choices = c("Single Tm apparent, by first derivative", "Melting temperatures, up to triple Tm", "Smoothed raw data (RFU vs T)", "First derivative of smoothed (dRFU/dT vs T)")),
                                                                textInput("dataset1_download_name", p("Set custom name for quick result", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"), value = Sys.Date()),
                                                                downloadButton("downloadData1", p("Download quick result", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"), 
                                                                               width = '10%'),
                                                                p("Welcome to DSF world", style = "font-family: 'Avenir Next'; font-size: 8px; color: white",align = "center"),
                                                                # htmlOutput("blank_line_3"),
                                                                bsCollapsePanel(p("Additional files", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left") %>%strong,
                                                                                selectInput("dataset2", "",
                                                                                            choices = c("Melting temperatures, single Tm, no replicate averaging", "Melting temperatures, up to triple Tm, no replicate averaging", "Smoothed raw data (RFU vs T), no replicate averaging", "First derivative of smoothed (dRFU/dT vs T), no replicate averaging", "Unsmoothed raw data (RFU vs T), no replicate averaging", "Smoothed data truncated at maximum (useful for sigmoid fitting), replicate-averaged","Smoothed data truncated at maximum (useful for sigmoid fitting), no replicate averaging")),
                                                                                textInput("dataset2_download_name", p("Set custom name for additional file", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"), value = Sys.Date()),
                                                                                downloadButton("downloadData2", p("Download additional file", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"))
                                                                )
                                                                
                                                   ), # end sidebarPanel 
                                                   # open up a bit of space
                                                   # Main panel for displaying outputs 
                                                   mainPanel(
                                                       p("Welcome to DSF world", style = "font-family: 'Avenir Next'; font-size: 8px; color: white",align = "center"), # a bit of white space
                                                       p("See the 'About the analysis' tab for information about the downloadable results", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                       tabsetPanel(
                                                           p("tables will go here")
                                                           # tabPanel("Preview: quick downloads", dataTableOutput("table_set1"), style = "overflow-x: scroll;"),
                                                           # tabPanel("Preview: supplemental files", dataTableOutput("table_set2"), style = "overflow-x: scroll;")
                                                       )
                                                   ) # end main panel
                                               ) # end sidebarLayout
                                               
                                               
                                               
                                               
                                               
                                      ), # end tabpanel
                                      # About the analysis ------
                                      tabPanel(p("about the analysis", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"), value = "about_analysis_tab"), # end tabPanel
                                      #column(4),
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
    
    
    ########### data uploading  ----------
    values <- reactiveValues() # initalize the reactive values container (is this effectively a class? is shiny OOP-like...?) wish i'd realized that earlier)
    data_raw <- reactive({
        req(input$uploaded_file)
        tryCatch(
            {
                if (input$reformat == "quantStudio") {
                    df_input <- read_quantStudio(input$uploaded_file$datapath)
                } else {
                    df_input <- read.table(input$uploaded_file$datapath, # file
                                           header = input$header, # colnames
                                           sep = input$sep,
                                           quote = input$quote,
                                           stringsAsFactors =  FALSE)
                }
                
                # supported instrument reformatting
                if (input$reformat == "none") { df <- format_none(df_input)
                } else if (input$reformat == "qTower") { df <- format_none(df_input)
                } else if (input$reformat == "biorad") { df <- format_biorad(df_input)
                } else if (input$reformat == "stratagene") {df <- format_stratagene(df_input)
                } else if (input$reformat == "quantStudio") { df <- format_none(df_input) }
                
                df <- df %>%
                    mutate_all(as.numeric)
                
                # cycle number to temperature
                if (input$cycle_to_T == TRUE) {
                    Temps_calc <- cycle_to_T_func(as.numeric(input$start_T), as.numeric(input$increment_T), df)
                    df <- dplyr::bind_cols(Temps_calc, df)[-2]
                }
                
                df
                
            } ,
            error = function(e) {
                renderText({
                    stop(
                        "error in uploading"
                    )
                })
            })
    }) # read the input file
    
    observeEvent(data_raw(), {
        values$data_raw <- data_raw()
    }) # write to values
    
    output$input_file <- renderDataTable({
        infile <- input$uploaded_file
        if (is.null(infile) == TRUE ) { # if there is no input file
            return(NULL) # leave "contents" empty (display instead the uploading instructions)
        } else {
            return((values$data_raw)) # otherwise, assign the uploaded file to data_raw()
        }
    }, # render the input file data table
    options = list(scrollX = TRUE, scrollY = 500, scrollCollapse = TRUE, paging = FALSE, dom = 't'))
    
    
    ########### data analysis and visualization  ----------
    # values$df_1 <- reactive({values$df_raw %>% # this will ultimately be reactive, treat it as such now
    #     gather(well, value, -Temperature) %>%
    #     group_by(well) %>%
    #     nest %>%
    #     mutate(new_names = well) })
    # 
    # values$df <- reactive({values$df_raw %>% # this will ultimately be reactive, treat it as such now
    #         gather(well, value, -Temperature) %>%
    #         group_by(well) %>%
    #         nest %>%
    #         mutate(new_names = well) })
    
    observeEvent( data_raw(), {
        ###### the way that worked just fine before attempting to modify to make the model fitting process more streamlined ######
        # values$df <- data_raw() %>% # this is the active dataframe, used for plotting and calculations
        #         gather(well, value, -Temperature) %>%
        #         group_by(well) %>%
        #         nest %>%
        #         plyr::mutate(new_names = well) 
        # 
        # values$df_1 <- data_raw() %>% # this is the original dataframe, used when a "clean slate" is needed, e.g. if a new layout file is uploaded
        #     gather(well, value, -Temperature) %>%
        #     group_by(well) %>%
        #     nest %>%
        #     plyr::mutate(new_names = well) 
        ###### the way that worked just fine before attempting to modify to make the model fitting process more streamlined ######
        
        values$df <- data_raw() %>% # this is the active dataframe, used for plotting and calculations
            gather(well, value, -Temperature) %>%
            group_by(well) %>%
            mutate(value_norm = BBmisc::normalize(value, method = "range", range = c(0,1)), ###### if we do this as a mutate, it ignores the groups!!!!!!!
                   Temperature_norm = BBmisc::normalize(Temperature, method = "range", range = c(0,1)))  %>%
            nest %>%
            plyr::mutate(new_names = well)
        
        values$df_1 <- data_raw() %>% # this is the original dataframe, used when a "clean slate" is needed, e.g. if a new layout file is uploaded
            gather(well, value, -Temperature) %>%
            group_by(well) %>%
            mutate(value_norm = BBmisc::normalize(value, method = "range", range = c(0,1)), ###### if we do this as a mutate, it ignores the groups!!!!!!!
                   Temperature_norm = BBmisc::normalize(Temperature, method = "range", range = c(0,1)))  %>%
            nest %>%
            plyr::mutate(new_names = well)
        
        
        # create two closures which will be used later in the model fitting
        values$temps_extracted <- data_raw() %>% ## this is untested and might not work....?
            select(Temperature) %>%
            as_vector() 
        
        # uncomment this once the model fitting functions are loaded into the app. 
        ## creation of the data-specific closures
        #temperatures_mock <- c(25:95)
        # n2r <- make_temp_n2r(range(values$temps_extracted )) # function to convert from normalized to non-normalized temperatures
        # win3d <- floor(3/((n2r(1) - n2r(0))/length(values$temps_extracted )))  # a three-degree window
        # 
        # # sgfilt_nest <- find_sgolay_width( win3d ) # this will ultimately be set by the uploaded data
        # sgfilt_nest <- sgfilt_set_n(n_ = find_sgolay_width( win3d )) # the sg filter, with a 9-degree smoothing window
        # peak_finder_nest <- make_peak_finder_nest(win3d) # the peak finder, where no two peaks can be < 3 degrees apart
        
        
        
    })
    
    
    ### handsontable for renaming
    output$r_table <- renderRHandsontable({ 
        req(values$df)
        rhandsontable( values$df %>% select_if(is.character), height = 200, useTypes = TRUE, stretch = "all") %>% hot_col("well", readOnly = TRUE) #%>%  hot_context_menu(allowRowEdit = FALSE, allowColEdit = TRUE)
    })
    
    # output$start_pars <- renderRHandsontable({ 
    #     req(values$df)
    #     rhandsontable( values$df %>% select_if(is.character), height = 200, useTypes = TRUE, stretchH = "all") %>% hot_col("well", readOnly = TRUE) #%>%  hot_context_menu(allowRowEdit = FALSE, allowColEdit = TRUE)
    # })
    # 
    # output$selected_models <- renderRHandsontable({ 
    #     req(values$df)
    #     rhandsontable( values$df %>% select_if(is.character), height = 200, useTypes = TRUE, stretchH = "all") %>% hot_col("well", readOnly = TRUE) #%>%  hot_context_menu(allowRowEdit = FALSE, allowColEdit = TRUE)
    # })
    
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
        
        # values$df <- data_raw() %>% # this is the active dataframe, used for plotting and calculations
        #     gather(well, value, -Temperature) %>%
        #     group_by(well) %>%
        #     mutate(value_norm = BBmisc::normalize(value, method = "range", range = c(0,1)), ###### if we do this as a mutate, it ignores the groups!!!!!!!
        #            Temperature_norm = BBmisc::normalize(Temperature, method = "range", range = c(0,1)))  %>%
        #     nest %>%
        #     plyr::mutate(new_names = well)
        
        
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
    
    # output$mean_or_each <- renderUI({
    #     
    #     # to make this responsive to the presence of the mean, must ensure input$mean_or_each exists even when ui not rendered
    #     # req(values$df)
    #     # df_names <- values$df %>% unnest() %>% names
    #     # if ("mean" %in% df_names) {
    #         
    #         radioButtons("mean_or_each", "Show mean?",
    #                      c("Each replicate" = "each",
    #                        "Mean" = "mean"))  
    #         
    #    # } else { NULL }
    #     })
    
    
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
    
    # plot <- reactive({
    #     req(values$df) # only render the plot if there is data
    #     
    #     if (input$show_Tm_dRFU == TRUE ) { req(values$df_tm_models_table) }  # need tms to show tms
    #     
    #     # values$df_tm_models_table <- values$df_tm_models %>%
    #     #     dplyr::filter( which_model == "s1_pred"  ) %>%
    #     #     plyr::mutate( which_model = grep_and_gsub(.$which_model, c("s1_pred", "s1_d_pred", "s2_pred","s2_d_pred"), c("Model 1", "Model 2", "Model 3", "Model 4"), c("Other")))  %>% # move this to later, for the for-display table only!
    #     #     set_names(c("Condition", "Model", "Tm' 1", "Tm' 1 SD", "Tm' 2", "Tm' 2 SD")) %>%
    #     #     discard(~all(is.na(.x)))
    #     
    #     # df_RFU_plot <- unnest(values$df) %>%
    #     #     plyr::mutate("-" = rep("", nrow(.))) %>%
    #     #     plyr::mutate("- " = rep("", nrow(.)))
    #     # 
    #     # df_RFU_tms <- unnest(values$df) %>%
    #     #     select_if(is.character) %>%
    #     #     full_join(values$df_tm_models_table) %>%
    #     #     values$df_tm_models_table %>%
    #     #                 select()
    #     
    #     # tm_df_dRFU <-  by_variable %>%
    #     #     unnest() %>%
    #     #     select_if(is.character) %>%
    #     #     full_join(df_tm_models) %>%
    #     #     pivot_longer(
    #     #         cols = contains("xmid"),
    #     #         names_to = "Tm_type",
    #     #         values_to = "Tm",
    #     #         values_drop_na = TRUE )
    # 
    #     facet_func(df = df,# reacts to the appearance and changes to the dataframe, to the uploading of format files
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
    
    # plot <- reactive({
    #     # the requirements logic
    #     req(values$df) # ony require the modeling dataframe if it's called
    #     
    #     ### make the necessary dataframes
    #     df <- unnest(values$df) %>%
    #         plyr::mutate("-" = rep("", nrow(.))) %>%
    #         plyr::mutate("- " = rep("", nrow(.)))
    #     
    #     
    #     ### choose the plot to make
    #     # show_components <- FALSE
    #     # show_fits <- FALSE
    #     if (show_components == TRUE | show_fits == TRUE ) {
    #         req() ### the modeling dataframe
    #         out_plot <- facet_func_with_model_2(df_for_models = ########model_df_for_plot %>% filter(well %in% c("G1", "G2", "C1", "E1")) %>% filter(which_model == "s2_pred"),
    #                                             color_by = !!input$color_by,
    #                                             facet = input$facet,
    #                                             facet_by = !!input$wrap_by,
    #                                             facet_rows = !!input$grid_rows,
    #                                             facet_cols = !!input$grid_cols,
    #                                             set_title = plot_title_d(),
    #                                             legend_title = plot_legend_d(),
    #                                             legend_linetype_title = "component",
    #                                             fix_free = input$fix_free,
    #                                             text_size = input$text_size,
    #                                             legend_position = legend_position(),
    #                                             x_title = input$x_title,
    #                                             y_title = input$y_title,
    #                                             
    #                                             show_fits = TRUE, 
    #                                             show_components = TRUE,
    #                                             color_models =FALSE,
    #                                             tm_df_models_gathered = #######df_tm_for_plot %>% filter(well %in% c("G1", "G2", "C1", "E1")) %>% filter(which_model == "s2_pred"),
    #                                             show_tm_mod = TRUE,
    #                                             show_tm_mod_colors = TRUE)
    #     } else {
    #         
    #         #     facet_func(df = df,# reacts to the appearance and changes to the dataframe, to the uploading of format files
    #         #                mean_or_each = input$mean_or_each, 
    #         #                color_by = !!input$color_by,
    #         #                linetype_by = !!input$linetype_by,
    #         #                use_linetypes = input$use_linetypes, 
    #         #                facet = input$facet,
    #         #                facet_by = !!input$wrap_by,
    #         #                facet_rows = !!input$grid_rows,
    #         #                facet_cols = !!input$grid_cols,
    #         #                set_title = plot_title_d(),
    #         #                legend_title = plot_legend_d(),
    #         #                legend_linetype_title = plot_legend_linetype(),
    #         #                fix_free = input$fix_free,
    #         #                text_size = input$text_size,
    #         #                legend_position = legend_position(),
    #         #                x_title = input$x_title,
    #         #                y_title = input$y_title)
    #         
    #         
    #         out_plot <-facet_func_with_dRFU_2(
    #             df_for_dRFU  = ######%>% filter(well %in% c("G1", "G2", "C1", "E1")), #%>% filter(which_model == "s2_pred"),
    #             mean_or_each = input$mean_or_each,
    #             color_by = !!input$color_by,
    #             use_linetypes = input$use_linetypes, 
    #             linetype_by = !!input$linetype_by,
    #             facet = input$facet,
    #             facet_by = !!input$wrap_by,
    #             facet_rows = !!input$grid_rows,
    #             facet_cols = !!input$grid_cols,
    #             set_title = plot_title_d(),
    #             legend_title = plot_legend_d(),
    #             legend_linetype_title = plot_legend_linetype(),
    #             fix_free = input$fix_free,
    #             text_size = input$text_size,
    #             legend_position = legend_position(),
    #             x_title = input$x_title,
    #             y_title = input$y_title,
    #             
    #             tm_df_dRFU_gathered = ######tm_df_dRFU %>% filter(well %in% c("G1", "G2", "C1", "E1")), #%>% filter(which_model == "s2_pred"),
    #             show_tm_dRFU = TRUE,
    #             show_tm_dRFU_colors = FALSE
    #         ) 
    #     }
    #     
    #     
    #     
    #     
    # })
    
    plot <- reactive({
        req(values$df) # only render the plot if there is data
        
        # if (input$show_Tm_dRFU == TRUE ) { req(values$df_tm_models_table) }  # need tms to show tms
        
        # values$df_tm_models_table <- values$df_tm_models %>%
        #     dplyr::filter( which_model == "s1_pred"  ) %>%
        #     plyr::mutate( which_model = grep_and_gsub(.$which_model, c("s1_pred", "s1_d_pred", "s2_pred","s2_d_pred"), c("Model 1", "Model 2", "Model 3", "Model 4"), c("Other")))  %>% # move this to later, for the for-display table only!
        #     set_names(c("Condition", "Model", "Tm' 1", "Tm' 1 SD", "Tm' 2", "Tm' 2 SD")) %>%
        #     discard(~all(is.na(.x)))
        
        df_RFU_plot <- unnest(values$df) %>%
            plyr::mutate("-" = rep("", nrow(.))) %>%
            plyr::mutate("- " = rep("", nrow(.)))
        # 
        # df_RFU_tms <- unnest(values$df) %>%
        #     select_if(is.character) %>%
        #     full_join(values$df_tm_models_table) %>%
        #     values$df_tm_models_table %>%
        #                 select()
        
        # tm_df_dRFU <-  by_variable %>%
        #     unnest() %>%
        #     select_if(is.character) %>%
        #     full_join(df_tm_models) %>%
        #     pivot_longer(
        #         cols = contains("xmid"),
        #         names_to = "Tm_type",
        #         values_to = "Tm",
        #         values_drop_na = TRUE )
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
    
    # plot_dRFU_Tm <- reactive({ 
    #     req(values$df) # only render the plot if there is data
    #     df <- unnest(values$df) %>%
    #         plyr::mutate("-" = rep("", nrow(.))) %>%
    #         plyr::mutate("- " = rep("", nrow(.)))
    #     
    #     facet_func(df = df,# reacts to the appearance and changes to the dataframe, to the uploading of format files
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
    # plot_dRFU_Tm <- reactive({ 
    #     req(values$df) # only render the plot if there is data
    #     df <- unnest(values$df) %>%
    #         plyr::mutate("-" = rep("", nrow(.))) %>%
    #         plyr::mutate("- " = rep("", nrow(.)))
    #     
    #     facet_func(df = df,# reacts to the appearance and changes to the dataframe, to the uploading of format files
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
    
    output$data <- renderPlot({ # is there a way to implement renderCachedPlot that would be worthwhile here?
        req(values$df)
        plot() #%>% withSpinner(color="#525252")
        # p <- plotOutput("data", height = "auto") 
        # if (values$df != NULL) { p <- p %>% withSpinner(color="#525252") }
        # , 
        # style = ("overflow-y:scroll; max-height: 600px") 
        
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
        
        
        
        # means <- unnest(values$df)  %>%
        #     group_by(Temperature, condition) %>% # once the layout is uploaded, handle the replicates
        #     dplyr::summarize(mean = mean(value),
        #                      sd = sd(value)) %>%
        #     ungroup()
        #
        # df_interm <- values$df %>%
        #     unnest()
        #
        # if ("mean" %in% names(df_interm)) {
        #     print("mean is present")
        #     values$df <- df_interm %>%
        #         select( -mean, -sd) %>%
        #         merge(means, by = c("x" = "Temperature", "y"  = "condition")) %>%
        #         group_by_at(vars(one_of(names(select_if(., is_character))))) %>%
        #         nest
        
        
    })
    
    
    # ####### Tm calculations
    # observeEvent( values$df, {
    #     print("observed!")
    # 
    #     # starting from values$df gives this calculation a fresh slate should the user re-format their data multiple times
    #     values$df_tms <- values$df %>% #df_int %>% # add the first derivative Tms
    #                         plyr::mutate(sgd1 = purrr::map(data, sgfilt_nest, m_ = 1)) %>% # add the first derivative data
    #                         plyr::mutate(dRFU_tma = as_vector(purrr::map2(data, sgd1, Tm_by_dRFU)))
    #     
    #     if ("condition" %in% names(values$df_tms)) {
    #         tm_means <- unnest(values$df_tms)  %>%
    #             group_by(Temperature, condition) %>% # once the layout is uploaded, handle the replicates
    #             dplyr::summarize( tm_dRFU_mean = mean(value) %>% round(2),
    #                              tm_dRFU_sd = sd(value) %>% round(2) ) %>%
    #             ungroup()
    #         
    #         # make the averaged table
    #         # values$tm_table_dRFU <- values$df_tms %>%
    #         #     select(condition, dRFU_tma)  %>%
    #         #     group_by(condition)  %>%
    #         #     summarise( mean_tm = mean(dRFU_tma) ,
    #         #                sd_tm = sd(dRFU_tma)) %>%
    #         #     mutate_if(is.numeric, round, 2)
    #     } else {
    #         tm_means <- unnest(values$df_tms) %>%
    #             select(well, dRFU_tma)  %>%
    #             group_by(well)  %>%
    #             #group_by(Temperature, well) %>%
    #             summarise( tm_dRFU_mean = mean(dRFU_tma) %>% round(2), 
    #                        tm_dRFU_sd = sd(dRFU_tma)%>% round(2))
    #     }
    #     
    # 
    #     df_interm_tm <- values$df %>%
    #                     unnest()
    # 
    #     if ("tm_dRFU_mean" %in% names(df_interm_tm)) {
    #         print("tm mean is present")
    #         values$df_tms  <- df_interm_tm %>%
    #             select( -tm_dRFU_mean, -tm_dRFU_sd) %>%
    #             merge(tm_means, by = c("x" = "Temperature", "y"  = "condition")) %>%
    #             group_by_at(vars(one_of(names(select_if(., is_character))))) %>%
    #             nest
    #         #nest_legacy()
    #         
    #     } else {
    #         print("tm mean is not present")
    #         values$df_tms  <- df_interm_tm %>%
    #             merge(tm_means, by = c("x" = "Temperature", "y"  = "condition")) %>%
    #             group_by_at(vars(one_of(names(select_if(., is_character))))) %>%
    #             nest
    #         #nest_legacy()
    #     }
    #     
    #     
    # 
    #     # if ("condition" %in% names(values$df_tms)) {
    #     #     # make the averaged table
    #     #     values$tm_table_dRFU <- values$df_tms %>%
    #     #         select(condition, dRFU_tma)  %>%
    #     #         group_by(condition)  %>%
    #     #         summarise( mean_tm = mean(dRFU_tma) ,
    #     #                    sd_tm = sd(dRFU_tma)) %>%
    #     #         mutate_if(is.numeric, round, 2)
    #     # } else {
    #     #     values$tm_table_dRFU <- values$df_tms %>%
    #     #         select(well, dRFU_tma)  %>%
    #     #         group_by(well)  %>%
    #     #         summarise( mean_tm = mean(dRFU_tma) ,
    #     #                    sd_tm = sd(dRFU_tma)) %>%
    #     #         mutate_if(is.numeric, round, 2)
    #     # }
    # 
    # 
    # 
    #     # means <- unnest(values$df)  %>%
    #     #     group_by(Temperature, condition) %>% # once the layout is uploaded, handle the replicates
    #     #     dplyr::summarize(mean = mean(value),
    #     #                      sd = sd(value)) %>%
    #     #     ungroup()
    #     #
    #     # df_interm <- values$df %>%
    #     #     unnest()
    #     #
    #     # if ("mean" %in% names(df_interm)) {
    #     #     print("mean is present")
    #     #     values$df <- df_interm %>%
    #     #         select( -mean, -sd) %>%
    #     #         merge(means, by = c("x" = "Temperature", "y"  = "condition")) %>%
    #     #         group_by_at(vars(one_of(names(select_if(., is_character))))) %>%
    #     #         nest
    # 
    # 
    # })
    
    
    output$tm_table_render <- DT::renderDataTable({
        req(values$df_tms)
        
        if( "dRFU_tma" %in% names(values$df_tms)) { # if there are tms to report, render a table
            # if ("tm_dRFU_mean" %in% names(values$df_tms)) { # if there are means, show the means
            #     df <- values$df_tms %>%
            #         select(condition, tm_dRFU_mean, tm_dRFU_sd) %>%
            #         set_names( c("Condition", "Tm'", "Stdev"))
            # } else { # if there aren't means
            # if ("condition" %in% names(values$df_tms)) {
            #     df <- values$df_tms  %>%
            #                 select(condition, tm_dRFU_mean, tm_dRFU_sd) %>%
            #                 set_names( c("Condition", "Tm'", "Stdev"))
            # } else {
            #     df <- values$df_tms  %>%
            #         select(well, tm_dRFU_mean, tm_dRFU_sd) %>%
            #         set_names( c("Condition", "Tm'", "Stdev"))
            # }
            # 
            # }
            # 
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
    observeEvent(values$data_raw, {
        # creation of data-sepcific functions and values
        low_T <- isolate( values$data_raw$Temperature %>% min() )
        high_T <- isolate( values$data_raw$Temperature %>% max() )
        n_meas <- isolate( values$data_raw$Temperature %>% unique() %>% length())
        
        #n2r <<- make_temp_n2r(range(low_T:high_T)) #make_temp_n2r(range(values$data$Temperature)) # an example of how this could be used
        #win3d <<- 7#floor(3/((n2r(1) - n2r(0))/n_meas))
        #win3d <<- floor(3/((n2r(1) - n2r(0))/n_meas))
        win3d <<- 7#floor(3/((n2r(1) - n2r(0))/n_meas))
        sgfilt_nest <<- sgfilt_set_n(n_ = find_sgolay_width( win3d ))
        print("finished with initial uploads")
        
    })
    
    observeEvent(values$df, {
        print("entering the modeling ring")
        win3d <<- 7#floor(3/((n2r(1) - n2r(0))/n_meas))
        
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
    
    
    
    
    
    
    ############ data downloading 
    
    # # Reactive value for selected dataset ----
    datasetInput1 <- reactive({
        switch(input$dataset1,
               "the data" = values$df
               # "Melting temperatures, single Tm" = Tm_mean_single(),
               # "Melting temperatures, up to triple Tm" = Tm_mean_double(),
               # "Smoothed raw data (RFU vs T)" = data_sgd0_mean_interc(),
               # "First derivative of smoothed (dRFU/dT vs T)" = data_sgd1_mean_interc()
               
        )
    })
    # 
    # datasetInput2 <- reactive({
    #     switch(input$dataset2,
    #            "Melting temperatures, single Tm, no replicate averaging" = Tms(),
    #            "Melting temperatures, up to triple Tm, no replicate averaging" = Tms_double(),
    #            "Smoothed raw data (RFU vs T), no replicate averaging" = data_sgd0(),
    #            "First derivative of smoothed (dRFU/dT vs T), no replicate averaging" = data_sgd1(),
    #            "Unsmoothed raw data (RFU vs T), no replicate averaging" = renamed_df_raw(),
    #            "Smoothed data truncated at maximum (useful for sigmoid fitting), replicate-averaged" = data_mean_trunc_at_max(),
    #            "Smoothed data truncated at maximum (useful for sigmoid fitting), no replicate averaging" = data_trunc_at_max()
    #     )
    # })
    # 
    # # Table of selected dataset ----
    # output$table_set1 <- renderDataTable(datasetInput1(), options = list(scrollX = TRUE, scrollY = 400, scrollCollapse = TRUE, paging = FALSE, dom = 'tr'))
    # 
    # output$table_set2 <- renderDataTable(datasetInput2(), options = list(scrollX = TRUE, scrollY = 400, scrollCollapse = TRUE, paging = FALSE, dom = 'tr'))
    # 
    # 
    # # Downloadable csv of selected dataset ----
    output$downloadData1 <- downloadHandler(
        filename = function() {
            paste(input$dataset1_download_name, "-", input$dataset1, ".rds", sep = "")
        },
        content = function(file) {
            saveRDS(datasetInput1(), file)
            # write.csv(datasetInput1(), file, row.names = FALSE)
        }
    )
    # 
    # output$downloadData2 <- downloadHandler(
    #     filename = function() {
    #         paste(input$dataset2_download_name, "-", input$dataset2, ".csv", sep = "")
    #     },
    #     content = function(file) {
    #         write.csv(datasetInput2(), file, row.names = FALSE)
    #     }
    # )
} # end server
# Run the application 
shinyApp(ui = ui, server = server)
