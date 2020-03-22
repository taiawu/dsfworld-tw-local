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
    
    output$download_ex_layout_96_well <- downloadHandler(
        filename = function() {
            paste('dsfworld_one_variable_layout_96_well.csv', sep='')
        },
        content = function(file) {
            read_csv("dsfworld_one_variable_layout_96_well.csv")
            write.csv(read_csv("dsfworld_one_variable_layout_96_well.csv"), file, row.names = FALSE)
        }
    )
    
    output$download_ex_layout_1 <- downloadHandler(
        filename = function() {
            paste('dsfworld_one_variable_layout.csv', sep='')
        },
        content = function(file) {
            read_csv("dsfworld_one_variable_layout.csv")
            write.csv(read_csv("dsfworld_one_variable_layout.csv"), file, row.names = FALSE)
        }
    )
    
    output$download_ex_layout_2 <- downloadHandler(
        filename = function() {
            paste('dsfworld_two_variable_layout.csv', sep='')
        },
        content = function(file) {
            read_csv("dsfworld_two_variable_layout.csv")
            write.csv(read_csv("dsfworld_two_variable_layout.csv"), file, row.names = FALSE)
        }
    )
    
    output$download_ex_layout_3 <- downloadHandler(
        filename = function() {
            paste('dsfworld_four_variable_layout.csv', sep='')
        },
        content = function(file) {
            read_csv("dsfworld_four_variable_layout.csv")
            write.csv(read_csv("dsfworld_four_variable_layout.csv"), file, row.names = FALSE)
        }
    )
} # end server

###### GUI #####
ui <- navbarPage(useShinyalert(),
                 
                 # Data Analysis --------------------------------------------------------------------------------
                 tabPanel(p("to data analysis", style = "font-family: 'Avenir Next'; font-size: 20px; color: grey",align = "center"), value = "data_analysis_mother", # end tab panel (tabset, div, main still remaining)
                          tabsetPanel(id = "inTabset_analysis", # tabset for all analysis sub-tabs, used by the "jumpToAnalysis" button 
                                      
                                      tabPanel(p("instructions", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"), value = "instructions_tab", # end tabPanel
                                     column(3),
                                     column(6,
                                                tags$div(
                                          tags$h1("Instructions for DSFworld data analysis"), 
                                          tags$p("These instructions explain how to use DSFworld to visualize and analyze raw DSF data. For more information on the analyses performed, please see the “about the analysis” tab."),
                                          tags$h3("1. Upload raw data"), 
                                          tags$p("To use DSFworld, start by uploading your raw fluorescence versus temperature data. Before moving on to analysis, your uploaded data must be formatted with Temperature in the first column, and fluorescence readings in the columns to the right. DSFworld offers a few tools to help you do this. The uploaded data is displayed in its current format in a table to the right of the grey uploads panel to guide you."),
                                          # uploading image
                                          shiny::div(tags$img(src = "well_formatted_data.png", width = "100%"), style = "text-align: center;"),
                                          tags$h5("DSFworld offers some tools to help you achieve this. "),
                                          tags$ul(
                                              tags$li(tags$strong("Reformat raw from instrument."),
                                                      "If the data was collected on any of the instruments listed under “Reformat raw from instrument” (currently, Biorad, Stratagene, quantStudio, and qTower), raw data can be uploaded exactly as exported from the instrument and DSFworld will automatically format the data for you."), 
                                              tags$br(),
                                              tags$li(tags$strong("Pre-format by template."),
                                                      "If your instrument is not listed under the supported reformatting, format your data prior to uploading with Temperature in the first column, and fluorescence values for each well in the columns to the right. A properly-formatted example file can be downloaded under “Uploading instructions” in the “upload data” tab. Be sure to save your data as a comma-separated value (csv) prior to uploading."),
                                              tags$br(),
                                              tags$li(tags$strong("Convert cycle number to temperature."),
                                                      "Some qPCR instruments export cycle numbers (e.g. 1, 2, 3 ...) for each measurement, instead of the temperature corresponding to that measurement. You can convert these cycle numbers to temperatures under the 'Convert cycle number to temperature' panel by setting a starting temperature, and a number of degrees to increase per cycle."),
                                              tags$br()
                                          ),
                                          
                                          tags$h5("A few tips on uploading data:"),
                                          tags$ul(
                                              tags$li(tags$strong("There is no hard limit to the number of wells you can analyze in a single upload at DSFworld."),
                                                      "We have tested up to full 384-well plates without trouble. However, DSFworld is not intended for use for high throughput screening. Be aware that for larger datasets (e.g. > 50 wells), displaying plots and tables will take a bit more time."), 
                                              tags$br(),
                                              tags$li(tags$strong("We strongly recommend using well names (e.g. A1, A2 ...) for your data columns."),
                                                      "This is the default for most instruments. While it isn't necessary for analysis, having well names for columns enables DSFworld to assign any experimental variables you define to your data in the 'layouts' portion of analysis. This is useful because it lets you make descriptive visualizations of your data (e.g. color lines based on compound concentration). If your data doesn't have well names but you want to use these features, you can overwrite your column names with well names in the uploads panel--see below for more information."), 
                                              tags$br(),
                                              tags$li(tags$strong("If data isn't upload successfully, try re-saving it as a UTF-8 csv."),
                                                      "(even if it is already a csv!) and re-uploading. You can do this on your computer with Save As > CSV, UTF-8 encoding."), 
                                          ),
                                          
                                          tags$h3("2. Analyze."), 
                                          tags$p("Once data is properly formatted, it can be visualized and analyzed in the analysis tab. DSFworld has features for both visualizing and analyzing the uploaded data. Apparent melting temperatures are displayed in a drop-down table at left of the analysis tab, from where they can be directly downloaded. These melting temperatures available immediately upon proceeding to the analysis tab, and do not depend on plotting or plate layouts. However, because it is always good to visualize your data before exporting processed results, the following instructions are presented in the order in which we recommend proceeding through analysis."),
                                          tags$ol(
                                              tags$li( tags$strong("Define your experimental layout."), 
                                                       "If you would like, you can define an experiment layout for the uploaded data--that is, what conditions were tested in each well. This is useful because it allows DSFworld to average experimental replicates for you, and more importantly, allows you to to use the plotting features of DSFworld to make plots that visualize the effects of your experimental variables."), 
                                              tags$p("You can define a single experimental variable by entering the variable in the editable table under 'Set plate layout and replicates' tab, and pressing 'Update names from manual table'. For replicates, enter identical names."),
                                              tags$p(" For more complicated experiments with more than one variable, you can define any number of experimental variables (e.g. protein, compound name, and compound concentration) by uploading a plate layout as a csv. The point of an experimental layout is to tell DSFworld what conditions are present in each well."),
                                              tags$p("It's often easiest to get the hang of plate layouts by looking at examples."),
                                              tags$ul(
                                                  tags$li(tags$strong("Here's an example of a simple layout with only one variable."),
                                                          "In this example, DSF was performed on three different mutants of a protein. The name of the experimental variable is entered to the left of the block--in this example, its 'Mutant'."), 
                                                  downloadButton("download_ex_layout_1", "Download one variable layout example", width = '50%',style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff"),
                                                  downloadButton("download_ex_layout_96_well", "Download one variable layout example, 96-well", width = '50%',style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff"),
                                                  tags$p(),
                                                  tags$br(),
                                                  tags$li(tags$strong("Here's an example of a layout with two variables."),
                                                          "SYPRO Orange concentration, and protein concentration. The second variable is defined in a new block, directly below the first. This is a layout for an experiment in which we compared DSF data collected for lysozyme (10 µM SYPRO Orange, 1 µM lysozyme) to three different negative controls: buffer alone, 10 µM SYPRO Orange without protein, and 1 µM lysozyme without dye. Fun fact: this is the experimental layout that we use to test whether a new batch of microtiter plates is compatible with DSF. It's the same one used to generate the data presented in Figure 3a and Supplemental Figure 4 of the paper associated with this website."), 
                                                  downloadButton("download_ex_layout_2", "Download two variable layout example", width = '50%',style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff"),
                                                  tags$p(),
                                                  tags$br(),
                                                  tags$li(tags$strong("Here's an example of a layout for a more complicated experiment."),
                                                          "This time there are four variables: SYPRO Orange concentration, compound, compound concentration, and protein concentration. Like before, each experimental variable is defined in its own block, and each new block is pasted directly below the block above it, and the name of that variable defined in the left-most column. This is a layout for an experiment in which we tested the impact of aggregation of four compounds (vemurafenib, miconazole, clotrimazole, and ritonavir) on DSF signal in the absence and presence of lysozyme. Fun fact: this is the experimental layout used to generate some of the results presented in Figure 4 and Supplementary Figures 5-7 of the paper associated with this website."),
                                                  downloadButton("download_ex_layout_3", "Download four variable layout example", width = '50%',style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff"),
                                                  tags$p(),
                                                  tags$br()
                                                  ),
                                              tags$li(tags$strong("Make custom plots."), 
                                                      tags$p("The uploaded experimental variables are automatically made available for the creation of custom plots. These variables can be used to create sub-plots, vary color, or vary line-type. Any wells which are identical in all experimental variables are considered replicates, and data can be plotted either as individual replicates, or as means with standard deviations. For sub-plots, the y-axes can be fixed as equal, or allowed to vary between the sub-plots."),
                                                      tags$p("The following plot aesthetics can also be customized: titles for the plot, legend, and x and y axes can be set; text size can be altered; and the legend (for colors and/or linetypes) can be shown or hidden."),
                                                      tags$p("To apply changes to a plot, press the 'Update plot' button. The currently displayed plot can be downloaded from the analysis window using the 'Download plot' button.")), 
                                              
                                              tags$li(tags$strong("Determine apparent melting temperatures."), "Apparent melting temperatures can be calculated by either the maximum of the first derivative, or by sigmoid fitting. For more information on the analysis methods, see the 'about the analysis' tab."),
                                              tags$ul(
                                                  tags$li( tags$strong("Maximum of the first derivative (dRFU)"), 
                                                           "Apparent melting temperatures are calculated by maximum of the first derivative automatically. Experimental replicates are averaged automatically. Results without replicate averaging can be downloaded from the Downloads tab."), 
                                                  tags$li( tags$strong("Sigmoid fitting"), 
                                                           tags$p("DSFworld offers four different models for sigmoid fitting, termed 'Fit 1', 'Fit 2', 'Fit 3', and 'Fit 4'. Upon uploading, apparent melting temperatures are automatically calculated by Fit 1. Additional models can be fit to the data by clicking the buttons for that fit in the 'By sigmoid fitting' drop-down menu."),
                                                           tags$p("You can trim the temperature range used for the fits using the slider-bar."),
                                                           tags$p("If multiple fits are applied to the data, DSFworld will automatically display the Fit with the lowest Bayesian Information Criterion (BIC). However, the best fit can also be selected manually. To do this, open the 'Select the best fit for each dataset'  dropdown tab. A table is displayed in this drop-down window which shows the currently-selected fit for each data set. Click the 'Display/update fit plot.' button. A new plot will be shown in the plot panel. This plot is specifically designed to facilitate manual fit selection. Each individual sub-plot shows a single data set, with one Fit option. Each row of sub-plots contains an individual data set, and each column shows a fit option. To select a particular fit option for a given data set, double click on that sub-plot."),
                                                           tags$p("To plot only the selected fits, press the 'Plot selected fits' button.")
                                                           )
                                              )
                                          ),
                                          tags$h3("3. Download."),
                                          tags$p("All plots and replicate-averaged apparent melting temperatures can be downloaded directly from the analysis window. However, if you would like to download additional forms of the data, this can be done in the 'download results' panel. If you would like to download results for a particular fit, you must first select that fit in the data analysis window."))),
                                     column(3)
                                      ) # end tabpanel
                                      # analyze and visualize --------------------------- 
                                      
                          ), # end tabset Panel (contains all "analysis sub-panels)
))

shinyApp(ui = ui, server = server)
