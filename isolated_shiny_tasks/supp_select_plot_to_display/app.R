library(shiny)
# this mini app determines which plot to render based on the current status of a value which depends on multiple actions 
# in the app, this is used to determine which plot to display, dependent on:
# the initial plot, which is set each time that new data is uploaded Specifically, it responds to changes in values$df_1
# the user-made plot, which responds to the "update plot" button
# or the model plot, which responds to the "show model plot" button

# soution adapted from https://stackoverflow.com/questions/48312392/shiny-allow-users-to-choose-which-plot-outputs-to-display

ui <- fluidPage(
    sidebarLayout(
        
        sidebarPanel(
            uiOutput("choose_plot1"),
            uiOutput("choose_plot2"),
            uiOutput("update_data"),
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Graph Viewer", plotOutput("selected_graph"))
            ))
    ))

server <- function(input, output) {
    # initialize the reactive values
    values <- reactiveValues()
    values$df <- mtcars
    values$plot_chosen <- "Plot1" # set the initial plot choice
    
    output$choose_plot1 <- renderUI({
        actionButton("choose_plot1", p("Make green plot", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),  width = '100%')
    })
    
    output$choose_plot2 <- renderUI({
        actionButton("choose_plot2", p("Make red plot", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),  width = '100%')
    })
    
    output$update_data <- renderUI({
        actionButton("update_data", p("Update the data", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),  width = '100%')
    })
    
    observeEvent(input$choose_plot1, {
        print("Plot 1 chosen!")
        values$plot_chosen <- "Plot1"
        print(values$plot_chosen)
    })
    
    observeEvent(input$choose_plot2, {
        print("Plot 2 chosen!")
        values$plot_chosen <- "Plot2"
        print(values$plot_chosen)
    })
    
    observeEvent(input$update_data, { ## this should observe the updating of the raw data, but not values$df, which will get it stuck in a loop 
        print("Updating data!")
        values$plot_chosen <- "Plot3"
        nrow <- runif(1, 1, 100) %>% as.integer()
        print(nrow)
        values$df <- mtcars[c(1:nrow),]
        print(values$plot_chosen)
    })
    
    plot1 <- reactive({
        values$df %>%
            ggplot(aes(x = mpg, y = cyl)) +
            geom_point(color = "green", size = 5)
    })
    
    plot2 <- reactive({
        values$df %>%
            ggplot(aes(x = mpg, y = cyl)) +
            geom_point(color = "red", size = 10)
    })
    
    plot3 <- reactive({
        values$df %>%
            ggplot(aes(x = mpg, y = cyl)) +
            geom_point(color = "black", size = 1)
    })
    
    graphInput <- reactive({
        print("values$plot_chosen updated")
        print(values$plot_chosen)
        if (values$plot_chosen == "Plot1") {
            plot1()
        } else if (values$plot_chosen == "Plot2") {
            plot2()
    } else {
        plot3()  # this is the default
    }
    })
    
    output$selected_graph <- renderPlot({
        graphInput()
    })
}

shinyApp(ui, server)