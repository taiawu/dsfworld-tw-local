# supp_select_best_model/app.R

library(shinyBS) # drop-down panels
library(tidyverse) #  handling data structures and plotting

source("../4_analyze/support_scripts/plotting.R")

library(shinyalert) # pop-up error messages
library(shinycssloaders) # spinning plot loading icon
library(rhandsontable) # user-interactive tables 
library(shiny) # for shiny web-apps 



ui <- navbarPage("UCSF Dye Screen Processing",
             
                 tabPanel("Pick hits",
                          fluidRow(
                              column(9,
                                     wellPanel(id = "facet_plot",
                                               plotOutput("plot1", dblclick = "plot_dblclick"),
                                               style = "overflow-y:scroll; max-height: 600px")
                              ),
                              column(3,
                                     actionButton("save_dt_button", "Save hit list to final folder"),
                                     DT::dataTableOutput("table")))
                 )
)


server <- function(session, input, output) {
    values <- reactiveValues()
    values$df_models_shiny <- readRDS("../4_analyze/values_df_models.rds")
    values$df_BIC_models_shiny <- readRDS("../4_analyze/values_df__BIC_models.rds")
    values$df_BIC_best <-  readRDS("../4_analyze/values_df__BIC_models.rds") %>%
                            cond_df_BIC_for_plot() %>%
                            filter(is_min == TRUE) %>%
                            select(c(well, condition, which_model)) 
    
    # make the Rshiny visualized version of the hit-calling plot
    output$plot1 <- renderPlot({
        plot_all_fits_shiny(values$df_models_shiny, values$df_BIC_models_shiny)
    })
    

    output$table <- DT::renderDataTable( {
        values$fit_sel <-  subset(
            nearPoints(values$df_models_shiny %>% cond_df_model_for_plot(., values$df_BIC_models_shiny),
                        input$plot_dblclick,
                        threshold = 1000, # set large,so anywhere in the plot area will work 
                        allRows = TRUE),
                        selected_ == TRUE) %>%
                        select(c(well, condition, which_model)) %>%
                        distinct(which_model, .keep_all = TRUE) %>%
                        arrange(condition, well)
        
        values$df_BIC_best <<- values$df_BIC_best %>%
                              filter(! well %in% values$fit_sel$well ) %>% # remove the wells to be overwritten
                              full_join(values$fit_sel) %>%
                              arrange(condition, well) 
            
        values$df_BIC_display <<- values$df_BIC_best %>%
                                mutate(`best model` = recode(which_model,
                                                            s1_pred = "Fit 1", 
                                                            s1_d_pred = "Fit 2",
                                                            s2_pred = "Fit 3",
                                                            s2_d_pred = "Fit 4")) %>%
                                select(c(well, condition, `best model`))
         values$df_BIC_display # this will render in the table 
    })
    
    # output$facet_plot_re_examined <- renderPlot({
    #     results()$data %>%
    #         dplyr::filter(dye %in% df$dye) %>%
    #         mutate(dye_well = as_vector(map2(.$dye, .$well, paste, sep = "-"))) %>%
    #         re_ex_grid_shiny(paste0(prefix, "re-examined_traces"))
    #     
    # })

    
} # end server




shinyApp(ui, server)