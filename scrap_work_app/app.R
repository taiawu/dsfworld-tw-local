library(shiny)
ui <- fluidPage(
    titlePanel("Dashboard"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
uiOutput("select_col"),
uiOutput("select_row")
            ),
        mainPanel(
           dataTableOutput("active_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    values <- reactiveValues()
    values$df <- head(mtcars)
    values$filtered_data <- head(mtcars)
    
    output$select_col <- renderUI({ # user selects a variable to filter by
        req(values$df)
        selectInput(inputId = "sel_col", label = ("col in data"),
                    choices = names( values$df) )
    })
    
    output$select_row <- renderUI({ # user selects the values of the variable they would like to look at 
       req(values$filtered_data, input$sel_col)

       #values$sel_col <- values$df %>% select(one_of(input$sel_col)) # all possible values of the variable
        values$sel_col <- values$filtered_data %>% select(one_of(input$sel_col)) # all possible values of the variable
       
       values$sel_col_filt <- values$filtered_data %>% select(one_of(input$sel_col)) # currently selected values of the variable

       values$choices <- values$sel_col %>% # the possible variable values, as a list
                           unique() %>%
                           as_vector() %>%
                           as.list() %>%
                           set_names(.)
        #names(values$choices) <- values$choices
        # print("values$choices")
        # print( values$choices)
        # 
        values$chosen <- values$sel_col_filt %>% # the selected variable values, as a list
                        unique() %>%
                        as_vector() %>%
                        as.list() %>%
                        set_names(.)
        #names(values$chosen) <- values$chosen
        # print("values$chosen")
        # print( values$chosen)
        # 
        # values$chosen_bool <- c(values$choices %in% values$chosen) %>% # the selected variable values, as TRUE/FALSE of all possible
        #                 as.list() %>%
        #                 set_names(values$choices) 
        # 
        checkboxGroupInput("sel_val", label = h3("Checkbox group"),
                           choices = values$choices,
                           selected = values$chosen )
    })
    
    observeEvent( input$sel_val, {
        print("input sel_val")
        print(input$sel_val)
        
        values$chosen_bool <- (values$choices %>% as_vector()) %in% input$sel_val # a boolean for which values have been selected
        print("values$chosen_bool")
        print( values$chosen_bool )
        #values$filtered_data <- inner_join(values$filtered_data, values$choices[values$chosen_bool,])
        values$filtered_data <- inner_join(values$filtered_data, values$df[values$chosen_bool,])

 
        # values$chosen <- values$sel_col_filt %>% # the selected variable values, as a list
        #                 unique() %>%
        #                 as_vector() %>%
        #                 as.list() %>%
        #     set_names(.)
        # print(names(input$sel_val))
        # req(values$df, values$filtered_data)
        # 
        # values$chosen_new <- values$chosen_bool[values$chosen_bool == TRUE] %>% names()  # use the chosen bool to re-update chosen, as a list of names
        # 
        # selected <- input$sel_val %>% as_vector() # which datasets are still selected to plot
        # col_isolated <- values$sel_col_isolated %>% as_vector()
        # filt_bool_raw <- col_isolated %in% selected
        # 
        # values$filtered_data <- dplyr::inner_join(values$filtered_data, values$spin[filt_bool_raw,])
    })
    
    output$active_table <-  renderDataTable({
        values$filtered_data
    })
}


shinyApp(ui = ui, server = server)