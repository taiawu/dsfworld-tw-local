ui <- fluidPage(sidebarLayout(
    sidebarPanel(
        sliderInput("controller", "Controller", 1, 3, 1)
    ),
    mainPanel(
        tabsetPanel(id = "inTabset_analysis",
                    tabPanel(title = "Panel 1", value = "panel1", "Panel 1 content"),
                    tabPanel(title = "Panel 2", value = "panel2", "Panel 2 content"),
                    tabPanel(title = "Panel 3", value = "panel3", "Panel 3 content")
        )
    )
))

server <- function(input, output, session) {
    observeEvent(input$controller, {

        updateTabsetPanel(session, "inTabset",
                          selected = paste0("panel", input$controller))
                          
        })
        
        observeEvent(input$controller, {
            print(input$inTabset_analysis == "panel1")
            
        
        
    })
}

shinyApp(ui, server)

# ui <- navbarPage('Test App',id = "inTabset",
#                  tabPanel(title = "Panel 1", value = "panel1", 
#                           actionButton('jumpToP2', 'Jump to Second Tab')),
#                  tabPanel(title = "Panel 2", value = "panel2", 
#                           actionButton('jumpToP1', 'Jump to First Tab'))
# )
# 
# server <- function(input, output, session) {
#     observeEvent(input$jumpToP2, {
#         updateTabsetPanel(session, "inTabset",
#                           selected = "panel2")
#     })
#     
#     observeEvent(input$jumpToP1, {
#         updateTabsetPanel(session, "inTabset",
#                           selected = "panel1")
#     })
#     
# }
# 
# shinyApp(ui, server)



# library(shiny)
# navbarPageWithText <- function(..., text) {
#     navbar <- navbarPage(...)
#     textEl <- tags$p(class = "navbar-text", text)
#     navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
#         navbar[[3]][[1]]$children[[1]], textEl)
#     navbar
# }
# 
# ui <- navbarPageWithText(
#     "Test app",
#     tabPanel("tab1", "tab 1"),
#     tabPanel("tab2", "tab 2"),
#     text = "User: Dean"
# )
# server <- function(input, output, session) {
# }
# 
# shinyApp(ui = ui, server = server)


# library(shiny)
# 
# ui <- (
#     fluidPage(
#     sidebarLayout(
#         sidebarPanel(tabPanel(p("to data analysis", style = "font-family: 'Avenir Next'; font-size: 20px; color: grey",align = "center"), value = "data_analysis_mother", # end tab panel (tabset, div, main still remaining)
#                               tabsetPanel(id = "inTabset_analysis", # tabset for all analysis sub-tabs, used by the "jumpToAnalysis" button
#                                           tabPanel(p("1 | upload data", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"), value = "uploads_tab"),
#                                           tabPanel(p("2 | analyze", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"), value = "analysis_tab"))
# 
#     )),
#     mainPanel(
#         verbatimTextOutput("text")
#     )
# 
# 
# 
# ))
# )
# 
# server <- (function(session,input,output){
# print(session$id)
# output$text <- renderText( str(session) )
# 
# })

# shinyApp(ui, server)

# library(shiny)
# # soution adapted from https://stackoverflow.com/questions/48312392/shiny-allow-users-to-choose-which-plot-outputs-to-display
# 
# ui <- fluidPage(
#     sidebarLayout(
# 
#         sidebarPanel(
#             uiOutput("choose_plot1"),
#             uiOutput("choose_plot2")
#         ),#end of sidebar panel
# 
#         mainPanel(
#             tabsetPanel(
#                 tabPanel("Graph Viewer", plotOutput("selected_graph"))
#     ))
# 
# ))
# 
# server <- function(input, output) {
#     values <- reactiveValues()
#     values$df <- head(mtcars)
#     values$plot_chosen <- "Plot1"
#     
#         output$choose_plot1 <- renderUI({
#             actionButton("choose_plot1", p("Make green plot", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),  width = '100%')
#         })
#         
#         output$choose_plot2 <- renderUI({
#             actionButton("choose_plot2", p("Make red plot", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),  width = '100%')
#         })
# 
#     
#     observeEvent(input$choose_plot1, {
#         print("Plot 1 chosen!")
#         values$plot_chosen <- "Plot1"
#         print(values$plot_chosen)
#         })
#     
#     observeEvent(input$choose_plot2, {
#         print("Plot 2 chosen!")
#         values$plot_chosen <- "Plot2"
#         print(values$plot_chosen)
#         })
#     
#         plot1 <- reactive({
#             values$df %>%
#                 ggplot(aes(x = mpg, y = cyl)) +
#                 geom_point(color = "green", size = 5)
#         })
# 
#         plot2 <- reactive({
#             values$df %>%
#                 ggplot(aes(x = mpg, y = cyl)) +
#                 geom_point(color = "red", size = 10)
#         })
#         
#             graphInput <- reactive({
#                 #plot1()
#                 print("values$plot_chosen updated")
#                 print(values$plot_chosen)
#                 if (values$plot_chosen == "Plot1") {
#                     plot1()
#                 } else {
#                     plot2()
#                 }
#             })
#             
#                 output$selected_graph <- renderPlot({
#                     graphInput()
#                 })
# }
# 
# shinyApp(ui, server)
# 
# 
# # ui <- fluidPage(
# #     sidebarLayout(
# #         
# #         sidebarPanel(
# #             fileInput('file1', 'Choose CSV File',
# #                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
# #             # selectInput("graph", "Choose a graph to view:",
# #             #             choices = c("Plot1", "Plot2")),
# #             uiOutput("plot1_action"),
# #             uiOutput("plot2_action"),
# #     
# #             submitButton("Update View")
# #         ),#end of sidebar panel
# #         
# #         mainPanel(
# #             tabsetPanel(
# #                 tabPanel("Graph Viewer", plotOutput("selected_graph")) 
# #     ))
# #  
# # ))
# # 
# # server <- function(input, output, session) {
# #     values <- reactiveValues()
# #     values$df <- head(mtcars)
# #     
# #     output$plot1_action <- renderUI({
# #         actionButton("plot1", p("Make green plot", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),  width = '100%')
# #     })
# #     
# #     output$plot2_action <- renderUI({
# #         actionButton("plot2", p("Make red plot", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center") %>% strong(),  width = '100%')
# #     })
# #     
# #     plot1 <- reactive({
# #         values$df %>%
# #             ggplot(aes(x = mpg, y = cyl)) +
# #             geom_point(color = "green", size = 5)
# #     })
# #     
# #     plot2 <- reactive({
# #         values$df %>%
# #             ggplot(aes(x = mpg, y = cyl)) +
# #             geom_point(color = "red", size = 10)
# #     })
# #     
# #     plot_choice <- reactive({"Plot1"})
# #     plot_choice <- eventReactive(input$plot1_action, { print("Plot1 chosen")  })
# #     plot_choice <- eventReactive(input$plot2, { print("Plot2 chosen")  })
# #     
# #     # observeEvent(input$plot2, {
# #     #     print("observed pllot 2 click")
# #     #     values$plot_choice <- "Plot2"})
# #     
# #     # Return the requested graph
# #     graphInput <- reactive({
# #         print("values$plot_choice")
# #         print(plot_choice())
# #         if (plot_choice() == "Plot1") {
# #             plot1()
# #         } else {
# #             plot2() 
# #         }
# #     })
# #     
# #     output$selected_graph <- renderPlot({ 
# #         graphInput()
# #     })
# # }
# # 
# # shinyApp(server = server, ui = ui)
# 
# ############# the filtering app, scratchwork
# # library(shiny)
# # ui <- fluidPage(
# #     titlePanel("Dashboard"),
# #     
# #     # Sidebar with a slider input for number of bins 
# #     sidebarLayout(
# #         sidebarPanel(
# # uiOutput("select_col"),
# # uiOutput("select_row")
# #             ),
# #         mainPanel(
# #            dataTableOutput("active_table")
# #         )
# #     )
# # )
# # 
# # # Define server logic required to draw a histogram
# # server <- function(input, output) {
# # 
# #     values <- reactiveValues()
# #     values$df <- head(mtcars)
# #     values$filtered_data <- head(mtcars)
# #     
# #     output$select_col <- renderUI({ # user selects a variable to filter by
# #         req(values$df)
# #         selectInput(inputId = "sel_col", label = ("col in data"),
# #                     choices = names( values$df) )
# #     })
# #     
# #     output$select_row <- renderUI({ # user selects the values of the variable they would like to look at 
# #        req(values$filtered_data, input$sel_col)
# # 
# #        #values$sel_col <- values$df %>% select(one_of(input$sel_col)) # all possible values of the variable
# #         values$sel_col <- values$filtered_data %>% select(one_of(input$sel_col)) # all possible values of the variable
# #        
# #        values$sel_col_filt <- values$filtered_data %>% select(one_of(input$sel_col)) # currently selected values of the variable
# # 
# #        values$choices <- values$sel_col %>% # the possible variable values, as a list
# #                            unique() %>%
# #                            as_vector() %>%
# #                            as.list() %>%
# #                            set_names(.)
# #         #names(values$choices) <- values$choices
# #         # print("values$choices")
# #         # print( values$choices)
# #         # 
# #         values$chosen <- values$sel_col_filt %>% # the selected variable values, as a list
# #                         unique() %>%
# #                         as_vector() %>%
# #                         as.list() %>%
# #                         set_names(.)
# #         #names(values$chosen) <- values$chosen
# #         # print("values$chosen")
# #         # print( values$chosen)
# #         # 
# #         # values$chosen_bool <- c(values$choices %in% values$chosen) %>% # the selected variable values, as TRUE/FALSE of all possible
# #         #                 as.list() %>%
# #         #                 set_names(values$choices) 
# #         # 
# #         checkboxGroupInput("sel_val", label = h3("Checkbox group"),
# #                            choices = values$choices,
# #                            selected = values$chosen )
# #     })
# #     
# #     observeEvent( input$sel_val, {
# #         print("input sel_val")
# #         print(input$sel_val)
# #         
# #         values$chosen_bool <- (values$choices %>% as_vector()) %in% input$sel_val # a boolean for which values have been selected
# #         print("values$chosen_bool")
# #         print( values$chosen_bool )
# #         #values$filtered_data <- inner_join(values$filtered_data, values$choices[values$chosen_bool,])
# #         values$filtered_data <- inner_join(values$filtered_data, values$df[values$chosen_bool,])
# # 
# #  
# #         # values$chosen <- values$sel_col_filt %>% # the selected variable values, as a list
# #         #                 unique() %>%
# #         #                 as_vector() %>%
# #         #                 as.list() %>%
# #         #     set_names(.)
# #         # print(names(input$sel_val))
# #         # req(values$df, values$filtered_data)
# #         # 
# #         # values$chosen_new <- values$chosen_bool[values$chosen_bool == TRUE] %>% names()  # use the chosen bool to re-update chosen, as a list of names
# #         # 
# #         # selected <- input$sel_val %>% as_vector() # which datasets are still selected to plot
# #         # col_isolated <- values$sel_col_isolated %>% as_vector()
# #         # filt_bool_raw <- col_isolated %in% selected
# #         # 
# #         # values$filtered_data <- dplyr::inner_join(values$filtered_data, values$spin[filt_bool_raw,])
# #     })
# #     
# #     output$active_table <-  renderDataTable({
# #         values$filtered_data
# #     })
# # }
# # 
# # 
# # shinyApp(ui = ui, server = server)