# # 0_integrated_modules/app.R
# library(quantmod) # contains the findValleys function, which maybe we should just extract and put verbatim in a source file instead of loading this whole thing...?
# library(minpack.lm) # contains the nlsLM function, which we use for our fitting
# library(modelr) # used in both the data modeling and the analysis model fitting 
# library(SciViews) # contains the ln function used in the data modeling
# library(signal) # contains the savistky golay filter (savgolfilt), used to generate the first derivative data in both data modeling and analysis model fitting  
# 
# library(shinyBS) # drop-down panels
# library(tidyverse) #  handling data structures and plotting
# 
# source("scripts/data_modeling.R") # computational models and all associate plots
# 
# source("scripts/upload_formatting.R") # functions to assist with raw data uploading
# source("scripts/layout_handling.R") # functions to apply layouts to data
# source("scripts/plotting.R") # functions to make all plots displayable in the analysis window
# source("scripts/analysis.R") # perform Tma analyses by dRFU or curve fitting
# 
# library(shinyalert) # pop-up error messages
# library(shinycssloaders) # spinning plot loading icon
# library(rhandsontable) # user-interactive tables 
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
# four_variable_layout
# one_variable_layout
# one_variable_layout_96_well
# two_variable_layout
# Define server logic required to draw a histogram
server <- function(session, input, output) {
    

    
    output$download_paper <- downloadHandler(
        filename = "20200318_dsfworld_preprint.pdf",
        content = function(file) {
            file.copy("20200318_dsfworld_preprint.pdf", file)
        }
    )
    
    output$download_SI <- downloadHandler(
        filename = "20200318_dsfworld_preprint_SI.pdf",
        content = function(file) {
            file.copy("20200318_dsfworld_preprint_SI.pdf", file)
        }
    )
}

###### GUI #####
ui <- navbarPage(useShinyalert(),
                 
                 # Data Analysis --------------------------------------------------------------------------------
                 tabPanel(p("to data analysis", style = "font-family: 'Avenir Next'; font-size: 20px; color: grey",align = "center"), value = "data_analysis_mother", # end tab panel (tabset, div, main still remaining)
                          tabsetPanel(id = "inTabset_analysis", # tabset for all analysis sub-tabs, used by the "jumpToAnalysis" button 
                                      
                                      tabPanel( p(" . . .", style = "font-family: 'Avenir Next'; font-size: 20px; color: grey",align = "center"), value = "closing_remarks_tab",
                                                column(4,
                                                       shiny::div(tags$img(src = "dsfworld_logo_grey.png", width = "400px"), style = "text-align: center;")
                                                ),
                                                column(6,
                                                       tags$br(),
                                                       tags$br(),
                                                       tags$p("DSFworld was created to help users complete more successful DSF experiments."), 
                                                       #tags$p("We hope that the interactive models and data analyses offered here can help users develop a strong working relationship with DSF results--both the underlying concepts and real data."),
                                                       tags$p("The ode for DSFworld is available on GitHub from https://github.com/gestwicki-lab/dsfworld. This includes the full code for this website and all associated scripts, as well as modular mini-web applications for each of the tasks tackled here: interactive modeling, data uploading, data layouts, plotting, analyses, and downloads."),
                                                       tags$p("DSFworld is presented in a publication, alongside practical tips for DSF and a deeper discussion of Model 2. You can download a version of that paper, and its supplementary information below."),
                                                       downloadButton("download_paper", "Download the full paper", width = '50%',style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff"),
                                                       downloadButton("download_SI", "Download the supplementary information", width = '50%',style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff"),
                                                       tags$p(),
                                                       tags$br(),
                                                       tags$p("Thank you to the many beta-testers of DSFworld, especially Ziyang Zhang, Douglas Wassarman, and Sarah Williams."),
                                                       tags$p("DSFworld is written in R. we're indebted to the many creators of R, R Studio, and the R packages who made this project possible. Particularly, thank you to Joe Cheng, Hadley Wickham, and their teams. R Shiny and the tidyverse are the foundation of both this website and underlying analyses."),
                                                       
                                                       tags$ul(
                                                           tags$li("The user interface was created using R Shiny, as well as the packages shinyBS for drop-down panels, shinyalert for pop-up messages, shinycssloaders for busy spinners, and rhandsontable for manual entry and editing of condition names in the analysis window. "), 
                                                           tags$br(),
                                                           tags$li("The model fitting uses modelr and broom for data and model handling, minpack.lm for fitting, signal for Savistky-Golay filtering, quantmod to assist in the finding of local maximima and minima for starting parameter estimates, and SciViews to perform natural logarithms.")
                                                           )),
                                                column(2))# end tabpanel
                                      # analyze and visualize --------------------------- 
                                      
                          ), # end tabset Panel (contains all "analysis sub-panels)
))

shinyApp(ui = ui, server = server)
