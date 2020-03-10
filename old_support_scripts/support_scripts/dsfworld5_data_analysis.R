qT_parse_2 <- function(datafile_with_path, start_T, end_T) {
  df_qt <- read.csv(datafile_with_path, row.names = NULL, skip = 18, header = FALSE, stringsAsFactors = FALSE)  %>%
    set_names( . ,  c("well", c(start_T:end_T)))
  
  channel_rows <- df_qt[[1]] %in% c("FAM", "JOE", "TAMRA", "ROX", "Cy5", "Cy5.5", "Sypro") # determine the rows which contain the channels
  
  channels <- df_qt[channel_rows,] %>% # determine the actual measured channels
    .[[1]]
  
  channel_rep <- c()
  for (i in channels) {
    a <- rep(i, 384)
    channel_rep <- c(channel_rep,a)
  }
  
  df_qt2 <- df_qt[!channel_rows,] %>%
    mutate( . , well_channel = as_vector(map2( . $well,  channel_rep, paste, sep = ":"))) %>% # remove the rows which contain only channels from the dataframe
    .[-1]
  
  df_qt_t <- as_tibble(cbind(Temperature = names( df_qt2 ), t( df_qt2 ))) %>%
    set_names( . , c("Temperature", df_qt2$well_channel))
  
  # list_out <- list(df_qt_2, df_qt_t)
  
  df_qt_t
  df_m <- df_qt_t %>%
    reshape2::melt(id.vars = "Temperature") %>%
    mutate( . , well = as_vector(lapply((strsplit(as.character( . $variable), ':')), function(x) {x[1]} ))) %>%
    mutate( . , channel = as_vector(lapply((strsplit(as.character( . $variable), ':')), function(x) {x[2]} )))
  
  out_list <- list(long = df_m, wide = df_qt_t)
}

# new daughter layout function
df_to_layout <- function(df, layout_type) {
  df_m <-   set_names( df ,  c("type","row",as.numeric( df [1,-c(1,2)]))) %>%
    . [ -1 , -1] %>%
    reshape2::melt( . ,id.vars = "row") %>%
    mutate( . , well = as_vector(map2( . $row,  . $variable, paste0)) ) %>%
    set_names( . , c("row", "column", layout_type, "well"))
  df_m
}

dsfworld_default <- theme( # adapted from free amino acids hit call
  text = element_text(size = 10),
  # axis.title.x = element_blank(),
  # axis.title.y = element_blank(),
  axis.text = element_text(size = 8),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(), 
  strip.background = element_blank(),
  aspect.ratio = (1/1.618)
  # axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  # axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')
)

# https://ggplot2.tidyverse.org/reference/aes.html
# https://ggplot2.tidyverse.org/reference/vars.html

facet_func <- function(df, mean_or_each, color_by, linetype_by, facet, facet_by, facet_rows, facet_cols, set_title, legend_title, legend_linetype_title, use_linetypes, fix_free, text_size, legend_position, x_title, y_title) {
  #df <- dplyr::filter(df, Allele != !!enquo(drop_cols))
  #if (color_by == "Uncolored" ) { color_by <- NULL } else { color_by <- enquo(color_by) }
  color_by <- enquo(color_by) # tell how to color
  linetype_by <- enquo(linetype_by)
   #print(length(unique(df$color_by)))

  if (mean_or_each == "mean") {
    p <- ggplot(df, aes(x = Temperature, y = mean, group = well, color = !!color_by)) +
      geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.5)
  } else if (mean_or_each == "each") {
    p <- ggplot(df, aes(x = Temperature, y = value, group = well, color = !!color_by))
    
  }
  
  if (facet == "wrap") {
    facet_by = enquo(facet_by)
    p <- p + 
      facet_wrap(vars(!!facet_by), scales = fix_free)
  } 
  else if (facet == "grid") {
    facet_rows = enquo(facet_rows)
    facet_cols = enquo(facet_cols)
    p <- p +
      facet_grid(rows = vars(!!facet_rows), cols = vars(!!facet_cols), scales = fix_free)
  }
  
  if (use_linetypes == TRUE ) {
    p <- p +
      geom_line(  aes( linetype = !!linetype_by ))  + ###delete the aes to revert
      theme_bw() + 
      scale_color_viridis_d() +
      labs(title = set_title, color = legend_title, x = x_title, y = y_title, linetype = legend_linetype_title) +
      dsfworld_default +
      theme(  text = element_text(size = text_size*1.25),
              axis.text = element_text(size = text_size),
              plot.title = element_text(lineheight=.8, face="bold", size = text_size*1.5),
              legend.position = legend_position)
  } else {
    p <- p +
      geom_line()  + ###delete the aes to revert
      theme_bw() + 
      scale_color_viridis_d() +
      labs(title = set_title, color = legend_title, x = x_title, y = y_title) +
      dsfworld_default +
      theme(  text = element_text(size = text_size*1.25),
              axis.text = element_text(size = text_size),
              plot.title = element_text(lineheight=.8, face="bold", size = text_size*1.5),
              legend.position = legend_position) 
    
  }

  p
}

gg_facet_nrow_ng <- function(p){ # determine the number of rows in a ggplot
  assertive.types::assert_is_any_of(p, 'ggplot')
  p %>%
    ggplot2::ggplot_build() %>%
    magrittr::extract2('layout') %>%
    magrittr::extract2('layout') %>%
    magrittr::extract2('ROW') %>%
    unique() %>%
    length()
}

gg_facet_ncol_ng <- function(p){
  assertive.types::assert_is_any_of(p, 'ggplot')
  p %>%
    ggplot2::ggplot_build() %>%
    magrittr::extract2('layout') %>% 
    magrittr::extract2('layout') %>%
    magrittr::extract2('COL') %>%
    unique() %>%
    length()
}

plotDownloadUI <- function(id, height = 400) {
  ns <- NS(id)
  downloadButton(ns("download_plot"), "Download figure")
}


plotDownload <- function(input, output, session, plotFun) { #https://github.com/czeildi/shiny-modules-examples
  output$plot <- renderPlot({
    plotFun()
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      #filename <- paste0("test", ".pdf")#paste0(input$plot_download_name, ".pdf")
      paste0(as.character(input$plot_download_name), ".pdf")
    },
    content = function(file) {
      ggsave(file, width = session$clientData$output_data_width/100, height = height/100, limitsize = FALSE)
    }
  )
}



######## determination of Tm by dRFU

##the fitting functions
sgfilt_set_n <- function(n_) { # a closure to set the sg filter length, which will be based on the length of the input data
  function(data, m_) {
    x <- data$value
    out <- sgolayfilt(x, p = 3, n = n_, m = m_)
  }
}

find_sgolay_width <- function(df) { # this works with the true temperature window
  range <- max(df$Temperature) - min(df$Temperature)
  nmeas <- length(unique(df$Temperature))
  out <- floor(3/(range/nmeas)) - floor(3/(range/nmeas))%%2 # ensure it's odd
  
  if (out < 5 ) {out <- 5}
  
  out
}

#sgfilt_nest <- sgfilt_set_n(n_ = find_sgolay_width(df)) # this will ultimately be set by the uploaded data
sgfilt_nest <- sgfilt_set_n(n_ = 13) # this will ultimately be set by the uploaded data

Tm_by_dRFU <- function( data,  sgd1) {
  df <- tibble( x = data$Temperature, 
                y = sgd1)
  
  grid <- tibble( x = seq(min(df$x), max(df$x), by = 0.1) ) %>% modelr::add_predictions(loess(y ~ x, data = df, span = 0.1))
  
  tma <- grid$x[which(grid$pred == max(grid$pred))]
  
  tma # the apparent Tm from the first derivative
}

#######