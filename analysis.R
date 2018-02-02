library(dplyr)

loadData = function(id = 1:332, directory = "specdata") {
   filepaths <- function(directory, id) {
      filenames <- function(id) {
         formatC(id, width=3, format="d", flag="0")
      }
      paste(directory, "/", filenames(id), ".csv", sep = "")
   }
   
   data = lapply(filepaths(directory, id), function(file) {
      read.csv(file, colClasses = c("Date", "numeric", "numeric", "numeric"))
   })
   
   Reduce(function(x, y) {
      rbind(x, y)
   }, data)
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
   # Convert string to symbol.
   p <- rlang::sym(pollutant)
   
   loadData(id) %>% 
      na.omit() %>% 
      summarise(mean(!!p)) # Quasi-quote the symbol.
}

complete <- function(directory, id = 1:332) {
   loadData(id, directory) %>% 
      na.omit() %>%
      group_by(ID) %>% 
      summarise(nobs = n()) %>% 
      mutate(ID = as.integer(ID))
}

corr <- function(directory, threshold = 0) {
   data <- loadData(1:332, directory)
   complete_data <- na.omit(data)
   
   valid_counts <- 
      complete_data %>% 
      group_by(ID) %>% 
      summarise(nobs = n()) %>% 
      filter(nobs >= threshold) %>%
      mutate(ID = as.integer(ID))
   
   valid_ids = valid_counts$ID
   
   valid_data <- complete_data %>% 
      filter(ID %in% valid_ids)
   
   corr <- valid_data %>%
      group_by(ID) %>%
      summarise(cor = cor(sulfate, nitrate))
   
   corr$cor
}