# library(shiny)
# 
# ui <- shinyUI(fluidPage(
# 
#     titlePanel("Testing File upload"),
# 
#     sidebarLayout(
#         sidebarPanel(
#             fileInput('file_input', 'upload file ( . pdf format only)', accept = c('.pdf'))
#         ),
# 
#         mainPanel(
#             #shiny::div(tags$iframe(src = "dsfworld_about_the_analysis.pdf", width = "100%", height = "500px"), style = "text-align: center;"),
#             shiny::div(tags$iframe(src = "dsfworld_about_the_model.pdf", width = "100%", height = "500px"), style = "text-align: center;"),
#         )
#     )
# ))
# 
# server <- function(session, input, output) {
# }
# 
# shinyApp(ui = ui, server = server)

library(shiny)
library(ggplot2)

    ui <- fluidPage(downloadButton('download_plot', "Download plot"),
                    downloadButton('download_dRFU_tma', "Download file"),
                    plotOutput("rendered_plot")
                    )
    
    server <- function(input, output) {
        plot <- reactive({
            qplot(speed, dist, data = cars)
        })
        
        values <- reactiveValues()
        values$df <- head(mtcars)
        
        output$rendered_plot <- renderPlot( plot() )
        plotInput = function() {
            plot()
        }
        
        output$download_plot <- downloadHandler(
            filename = function() { paste(Sys.Date(), '-dsfworld_plot.pdf', sep='') },
            content = function(file) {
                ggsave(file, plot = plotInput(), device = "pdf")
            }
        )
        
        output$download_dRFU_tma <- downloadHandler(
            filename = function() {
               paste0(Sys.Date(), '-dsfworld_tma_by_dRFU.csv', sep='')
            },
            content = function(file) {
                write.csv(values$df, file, row.names = FALSE)
            }
        )
        
    }
    
        # output$download_file <- downloadHandler(
        #     filename = function() { paste(Sys.Date(), '-dsfworld_tm_by_model_fitting.csv', sep='') },
        #     content = function(file) {
        #         write.csv(values$df)
        #     }3
        # )
        
        # output$foo = downloadHandler(
        #     filename = 'test.png',
        #     content = function(file) {
        #         device <- function(..., width, height) {
        #             grDevices::png(..., width = width, height = height,
        #                            res = 300, units = "in")
        #         }
        #         ggsave(file, plot = plotInput(), device = device)
        #     })
   # }

    shinyApp(ui = ui, server = server)