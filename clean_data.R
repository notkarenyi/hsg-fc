library(tidyverse)
library(readxl)
library(readr)
library(janitor)

remove_punc <- function(x) {
  return(gsub("[[:punct:] ]+", " ", x))
}

process_attendees <- function(x) {
  x <- map(x, ~ str_split(., "-")[[1]])
  x <- lapply(x, str_replace, "\\+", "")
  x <- lapply(x, as.numeric)
  x <- lapply(x, mean)
  x <- unlist(x)
  return(x)
}

extract_event <- function(details, start_row) {
  description <- details[event_starts[i]:(event_middle[i] - 1), ]
  names(description) <- c('column','v2','v3')
  description <- description %>%
    remove_empty(c("rows", "cols")) %>% 
    mutate(column=str_replace(column,regex(".*attendees"),"ExpectedAttendees")) %>% 
    mutate(column=str_replace(column,regex(".*UChicago.*"),"CollabRSO")) %>% 
    mutate(column=str_replace(column,regex("Collabo.*"),"CollabHSO")) %>% 
    mutate(column=str_replace(column,regex(".*speaker.*"),"Speaker"))
  
  description <- description %>%
    t() %>%
    data.frame() %>%
    row_to_names(row_number = 1)
  names(description) <- names(description) %>%
    remove_punc() %>%
    tolower() %>%
    str_trim()

  description$expectedattendees <- description$expectedattendees %>%
    process_attendees()

  expenses <- details[event_middle[i] + 1:(event_ends[i] - event_middle[i]), 1:3]

  expenses <- expenses %>%
    remove_empty(c("rows", "cols")) %>%
    row_to_names(row_number = 1)
  names(expenses) <- c("Category", "Details", "Amount")
  expenses <- expenses %>%
    mutate(Details=na_if(str_trim(expenses$Details), "")) %>%
    distinct()
  expenses$Amount <- as.numeric(expenses$Amount)
  expenses <- expenses %>% 
    mutate(Category=str_replace(Category,regex("Miscellaneous.*"),"Miscellaneous")) %>% 
    mutate(Category=str_replace(Category,regex("Supplies.*"),"Supplies")) %>% 
    mutate(Category=str_replace(Category,regex("Equipment.*"),"Equipment")) %>% 
    mutate(Category=str_replace(Category,regex("Refreshments.*"),"Refreshments")) %>% 
    mutate(Category=str_replace(Category,regex("Contractual.*"),"Services")) %>% 
    mutate(Category=str_replace(Category,regex("TOTAL.*|Total.*"),"EventTotal")) %>% 
    mutate(Category=str_replace(Category,regex("EXPENSE .*"),"EXPENSE"))
  expenses <- expenses[!is.na(expenses[,3]),]
  expenses <- expenses %>%
    select(-Details) %>%
    group_by(Category) %>%
    summarize(Amount=sum(Amount)) %>%
    t() %>%
    data.frame() %>%
    row_to_names(row_number = 1)
  
  expenses <- expenses %>%
    mutate(across(everything(), as.numeric))
  
  result <- cbind(description, expenses)
  row.names(result) <- NULL

  # print(result)
  return(result)
}


# run ---------------------------------------------------------------------

data <- list()
files <- list.files("data")
for (file in files[1:length(files)]) {
# for (file in files[25:length(files)]) {
    print(paste0("Parsing ", file))
  if (length(excel_sheets(paste0("data/", file))) == 4) {
    retrospective <- read_excel(paste0("data/", file), sheet = 2, skip = 4)
    budget <- read_excel(paste0("data/", file), sheet = 3)
    details <- read_excel(paste0("data/", file), sheet = 4)
  } else {
    retrospective <- data.frame(
      "Budgeted" = NA,
      "Actual" = NA,
      "Attendees" = NA
    )
    budget <- read_excel(paste0("data/", file), sheet = 2)
    details <- read_excel(paste0("data/", file), sheet = 3)
  }

  # parse detailed expenses page --------------------------------------------
  print("Parsing detailed expenses")

  # details2 <- details %>% t()
  details <- details[!is.na(details[, 1]), ]
  event_starts <- which(str_detect(unlist(details[, 1]), "^Expense Name"))
  if (any(str_detect(unlist(details[, 1]), "^EXAMPLE"))) {
    event_starts <- event_starts[2:length(event_starts)]
  }
  event_middle <- which(str_detect(unlist(details[, 1]), "^Expenses Breakdown"))
  event_middle <- event_middle[((length(event_middle) - length(event_starts)) + 1):(length(event_middle))]
  event_ends <- which(str_detect(unlist(details[, 1]), "^TOTAL EXPENSE"))
  event_ends <- event_ends[((length(event_ends) - length(event_starts)) + 1):(length(event_ends))]
  if (length(event_ends)<length(event_starts)) {
    event_ends <- c(event_ends,length(details))
  }

  df <- data.frame()
  for (i in 1:length(event_starts)) {
    df <- bind_rows(df, extract_event(details, i))
  }

  # parse retrospective page ------------------------------------------------
  print("Parsing retrospective")

  # df$budgeted <- sum(retrospective$Budgeted)
  df$actual <- sum(retrospective$Actual)
  df$attendees <- retrospective$`Attendees (if applicable)` %>%
    process_attendees() %>%
    sum()
  df$events <- nrow(retrospective)

  # parse budget page -------------------------------------------------------
  print("Parsing budget")

  df$org <- names(budget)[4]
  rollover <- budget[[9, 3]] %>% parse_number()
  external <- budget[10:15, 3] %>%
    map(parse_number) %>%
    unlist() %>%
    sum(na.rm = T)
  df$totalexternal <- rollover + external

  print(names(df))
  data <- bind_rows(data, df)
}

write.csv(data,paste0(quarter,'.csv'))
