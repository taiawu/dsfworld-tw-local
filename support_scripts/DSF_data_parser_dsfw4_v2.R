library(outliers)
library(readr) # to read files into dataframes
library(signal) # contains the savitsky-golay filter, whichs masks poly() in stats
library(stats)
library(tibble)
library(dplyr)
library(purrr)
library(nnet)

cycle_to_T_func <- function(start_T, inc_T, df) { # returns a numeric vector
  Temps <- rep(start_T, nrow(df))
  steps <- seq.int(from = 0, to = (nrow(df)-1), by = 1)
  steps2 <- steps*inc_T
  Temps2 <- steps2 + Temps
  df <- data.frame("Temperature" = Temps2)
  df
} 

# qTower and quantStudio formats require both reading and reformatting
# qTower
read_qTower <- function(filename) {
  df <- read_csv(filename) %>%
    . [is.na( . [[11]]) == FALSE,] %>%
    t(.) %>%
    as_tibble(.) %>%
    set_names(., c("Temperature", .[1,][-1])) %>%
    . [-1,] %>%
    mutate_all( . , as.numeric)
  
  df
}

# quantStudio
read_quantStudio <- function(filename) { # completed
  
  df_raw <- read_excel(filename, skip=5, sheet = 4) %>% # read the third sheet of the excel file
    . [is.na(.$X__1) == FALSE, c(2,4,5)] %>% # isolate only the rows which contain the long-form data
    set_names( . , c("Well", "Temperature", "RFU")) %>% # set the column names 
    .[-1,] 
  
  df_numeric <- mutate_all(df_raw[c(2,3)], as.numeric)
  df <- cbind(df_raw[1], df_numeric) %>%
    dcast(data = . , formula = Temperature~Well,fun.aggregate = sum,value.var = "RFU") # convert data to wide-form
  
  df # return the reformatted dataframe 
}


format_stratagene <- function(df) {
  # determine if the data frame is a single chunk, or multiple
  chunk_num_raw <- table(df[,1]) # will be null if a single chunk
  d_rows <- which(df[,1] == "Dissociation Curve") # determine start rows of multiple chunks, if present
  df <- df[,-1] # remove the first column, needed only to determine chunk presence and locations
  
  if (length(chunk_num_raw) > 0 ) { # if multiple chunks present
    chunk_num <- (chunk_num_raw[[1]] + 1) # determine how many
    df_final <- df[1:d_rows[1],]
    
    for (i in c(1:(length(d_rows)-1))) { # stitch everything together into a single dataframe 
      df_chunk <- df[(d_rows[i]+1):(d_rows[i+1]),]
      df_final <- dplyr::bind_cols(df_final, df_chunk) # append temperature column  
    }
    df <- df_final # over-write input df 
  }
  
  df <- df[rowSums(is.na(df)) < ncol(df),] # remove rows which are all NAs, once present between chunks
  well_names <- as_vector(df[1,]) # extract well names
  well_names <- c("Temperature" , well_names[c(TRUE, FALSE)]) # remove empty columns from well names
  df <- df[-c(1,2),] #  remove leading empty rows
  temperatures <- data.frame( "Temperatures" = df[1]) # extract temperatures

  to_delete <- seq(1, ncol(df), 2) # remove duplicated temperature rows
  df <- df[,-to_delete] # remove duplicated temperature rows

  df <- dplyr::bind_cols(temperatures, df) # append temperature column
  df <- set_names(df, nm = well_names) # re-set the names
}

format_none <- function(df) { df }

format_biorad <- function(df) {
  df[,-1]
}