library(tidyverse)
library(readxl)
library(readr)
library(janitor)

remove_punc <- function(x) {
  return(gsub('[[:punct:] ]+',' ',x))
}

process_attendees <- function(x) {
  x <- map(x, ~ str_split(., "-")[[1]])
  x <- lapply(x, gsub, "+", "")
  x <- lapply(x, as.numeric)
  x <- lapply(x, mean)
  x <- unlist(x)
  return(x)
}

extract_event <- function(start_row, end_row = NULL) {
  description <- details[event_starts[i] + 1:((event_middle[i] - 1) - (event_starts[i] + 2)), ] %>%
    t() %>%
    data.frame() %>% 
    row_to_names(row_number=1)
  names(description) <- names(description) %>%
    remove_punc() %>%
    tolower() %>%
    str_trim()
  description <- description %>%
    filter(!is.na(description))
  
  description$`expected of attendees` <- description$`expected of attendees` %>%
    process_attendees()
  
  expenses <- details[event_middle[i]+1:(event_ends[i]-event_middle[i]),] %>% 
    row_to_names(row_number=1) %>%
    filter(!is.na(Amount)) %>%
    select(-Details) %>%
    t() %>%
    data.frame() %>% 
    row_to_names(row_number=1)
  names(expenses) <- gsub(":.*","",names(expenses))
  
  result <- cbind(description,expenses)
  row.names(result) <- NULL
  
  return(result)
}


# run ---------------------------------------------------------------------

data <- list()
for (file in list.files('data')) {
  print(paste0('Parsing ', file))
  retrospective <- read_excel(paste0('data/',file),sheet=2, skip=4)
  budget <- read_excel(paste0('data/',file),sheet=3)
  details <- read_excel(paste0('data/',file),sheet=4)
  
  # parse detailed expenses page --------------------------------------------
  print('Parsing detailed expenses')

  # details2 <- details %>% t()
  details <- details[!is.na(details[, 1]), ]
  event_starts <- which(str_detect(unlist(details[, 1]), "^Expense Name"))
  event_middle <- which(str_detect(unlist(details[, 1]), "^Expenses Breakdown"))
  event_middle <- event_middle[((length(event_middle) - length(event_starts)) + 1):(length(event_middle))]
  event_ends <- which(str_detect(unlist(details[, 1]), "^TOTAL EXPENSE"))
  event_ends <- event_ends[((length(event_ends) - length(event_starts)) + 1):(length(event_ends))]
  if (length(event_ends)<length(event_starts)) {
    event_ends <- c(event_ends,length(details))
  }

  df <- data.frame()
  for (i in length(event_starts)) {
    df <- bind_rows(df,extract_event(i))
  }

  # parse retrospective page ------------------------------------------------
  print('Parsing retrospective')
  
  # df$budgeted <- sum(retrospective$Budgeted)
  df$actual <- sum(retrospective$Actual)
  df$attendees <- retrospective$`Attendees (if applicable)` %>%
    process_attendees() %>%
    sum()
  df$events <- nrow(retrospective)
  
  # parse budget page -------------------------------------------------------
  print('Parsing budget')
  
  df$org <- names(budget)[4]
  rollover <- budget[[9,3]] %>% parse_number()
  external <- budget[10:15,3] %>% 
    map(parse_number) %>%
    unlist() %>% 
    sum(na.rm=T)
  df$totalexternal <- rollover + external
  
  data <- bind_rows(data, df)
}
