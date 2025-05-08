# define functions --------------------------------------------------------

remove_punc <- function(x) {
  return(gsub("[[:punct:] ]+", " ", x))
}

process_attendees <- function(x) {
  #' Attendees are often reported as strings like "50+" or "30-40"
  #' Transform to integer

  x <- map(x, ~ str_split(., "-")[[1]]) # split into a list of numbers if range
  x <- lapply(x, str_replace, "\\+", "") # remove +
  x <- lapply(x, as.numeric)
  x <- lapply(x, mean) # average max and min if a range was reported
  x <- unlist(x)
  return(x)
}

extract_event <- function(details, i) {
  #' From the Details tab of the finance application,
  #' detect and extract each event into its own row
  #' @param i Which event are we extracting?

  # Details about event are between "EXPENSE #X" and "Expenses Breakdown"
  description <- details[event_starts[i]:(event_middle[i] - 1), 1:3]
  names(description) <- c("column", "v2", "v3")

  # Standardize row names to later transpose and combine
  description <- description %>%
    remove_empty(c("rows", "cols")) %>%
    mutate(column = str_replace(column, regex(".*attendees"), "ExpectedAttendees")) %>%
    mutate(column = str_replace(column, regex(".*UChicago.*"), "CollabRSO")) %>%
    mutate(column = str_replace(column, regex("Collabo.*"), "CollabHSO")) %>%
    mutate(column = str_replace(column, regex(".*speaker.*"), "Speaker"))

  description <- description %>%
    t() %>% # transpose
    data.frame() %>%
    row_to_names(row_number = 1) # turn first row into col names

  names(description) <- names(description) %>%
    remove_punc() %>%
    tolower() %>%
    str_trim()

  description$expectedattendees <- description$expectedattendees %>%
    process_attendees()

  # Itemized expenses are between "Expenses Breakdown" and "TOTAL EXPENSE #X"
  expenses <- details[event_middle[i] + 1:(event_ends[i] - event_middle[i]), 1:3]

  expenses <- expenses %>%
    remove_empty(c("rows", "cols")) %>%
    row_to_names(row_number = 1) # turn first row into col names
  names(expenses) <- c("Category", "Details", "Amount")

  # Remove unused categories
  expenses <- expenses %>%
    mutate(Details = na_if(str_trim(expenses$Details), "")) %>%
    distinct()
  expenses <- expenses[!is.na(expenses[, 3]), ]

  # Standardize row names to later transpose and combine
  expenses <- expenses %>%
    mutate(Category = str_replace(Category, regex("Miscellaneous.*"), "Miscellaneous")) %>%
    mutate(Category = str_replace(Category, regex("Supplies.*"), "Supplies")) %>%
    mutate(Category = str_replace(Category, regex("Equipment.*"), "Equipment")) %>%
    mutate(Category = str_replace(Category, regex("Refreshments.*"), "Refreshments")) %>%
    mutate(Category = str_replace(Category, regex("Contractual.*"), "Services")) %>%
    mutate(Category = str_replace(Category, regex("TOTAL.*|Total.*"), "EventTotal")) %>%
    mutate(Category = str_replace(Category, regex("EXPENSE .*"), "EXPENSE"))

  # Group items in the same category
  expenses <- expenses %>%
    filter(Category != "EventTotal") %>% # event total is duplicate of itemized amounts
    select(-Details) %>%
    group_by(Category) %>%
    mutate(Amount = as.numeric(Amount)) %>%
    summarize(Amount = sum(Amount)) %>%
    t() %>% # transpose
    data.frame() %>%
    row_to_names(row_number = 1) # turn first row into col names

  expenses <- expenses %>%
    mutate(across(everything(), as.numeric))

  # Combine description and expenses into one table and return
  result <- cbind(description, expenses)
  row.names(result) <- NULL

  # print(result)
  return(result)
}


# run ---------------------------------------------------------------------

data <- list()
files <- list.files(current_quarter) # get all files in / folder

for (file in files[1:length(files)]) {
  # for (file in files[25:length(files)]) {
  print(paste0("Parsing ", file))

  # existing orgs have a Retrospective tab for last quarter expenses
  fp <- paste0(current_quarter, "/", file)
  if (length(excel_sheets(fp)) == 4) {
    retrospective <- read_excel(fp, sheet = 2, skip = 4)
    budget <- read_excel(fp, sheet = 3)
    details <- read_excel(fp, sheet = 4)
  } else {
    # new orgs may not have a Retrospective tab for last quarter expenses
    retrospective <- data.frame(
      "Budgeted" = NA,
      "Actual" = NA,
      "Attendees" = NA
    )
    budget <- read_excel(fp, sheet = 2)
    details <- read_excel(fp, sheet = 3)
  }

  # parse detailed expenses page --------------------------------------------
  print("Parsing detailed expenses")

  # details2 <- details %>% t()
  details <- details[!is.na(details[, 1]), ]

  # events start at Expense Name and end at TOTAL EXPENSE
  event_starts <- which(str_detect(unlist(details[, 1]), "^Expense Name"))
  # skip example expense if any
  if (any(str_detect(unlist(details[, 1]), "^EXAMPLE"))) {
    event_starts <- event_starts[2:length(event_starts)]
  }
  event_middle <- which(str_detect(unlist(details[, 1]), "^Expenses Breakdown"))
  # match number of event middle to number of event starts
  event_middle <- event_middle[((length(event_middle) - length(event_starts)) + 1):(length(event_middle))]
  event_ends <- which(str_detect(unlist(details[, 1]), "^TOTAL EXPENSE"))
  # match number of event ends to number of event starts
  event_ends <- event_ends[((length(event_ends) - length(event_starts)) + 1):(length(event_ends))]
  if (length(event_ends) < length(event_starts)) {
    event_ends <- c(event_ends, length(details))
  }

  # for each event start detected, extract the corresponding event
  # combine all events into one table
  df <- data.frame()
  for (i in 1:length(event_starts)) {
    df <- bind_rows(df, extract_event(details, i))
  }
  df$eventsplanned <- length(event_starts)

  # parse retrospective page ------------------------------------------------
  print("Parsing retrospective")

  retrospective <- retrospective %>%
    remove_empty(c("rows"))

  if (!(any(grepl("Budgeted", names(retrospective))))) {
    retrospective <- retrospective %>%
      row_to_names(row_number = 1) # turn first row into col names
  }

  if (nrow(retrospective)) {
    names(retrospective) <- c("Event", "Date", "Attendees", "Budgeted", "Actual", "Notes")
    retrospective <- retrospective %>%
      mutate(
        Budgeted = as.numeric(Budgeted),
        Actual = as.numeric(Actual),
      )
    # people do not accurately report amount budgeted
    # so we will get this data from last quarter's applications
    # df$budgeted <- sum(retrospective$Budgeted)
    names(retrospective)[1] <- "Event"
    df$expenditureactual <- retrospective %>%
      filter(Event != "Example") %>%
      select(Actual) %>%
      sum()
    df$attendactual <- retrospective %>%
      mutate(Attendees = process_attendees(Attendees)) %>%
      group_by(Event) %>%
      summarize(total = mean(Attendees, na.rm = T)) %>%
      select(total) %>%
      sum(na.rm = T)
    df$eventsactual <- retrospective %>%
      filter(Event != "Example") %>%
      filter(!is.na(Event)) %>%
      distinct(Event) %>%
      count() %>%
      sum()
  }

  # parse budget page -------------------------------------------------------
  print("Parsing budget")

  df$org <- names(budget)[4]
  rollover_i <- which(str_detect(unlist(budget[, 2]), "^Rollover$"))
  rollover <- budget[rollover_i,3] %>% unlist() 
  if (any(grepl('\\d',rollover))) {
    df$rollover <- parse_number(rollover) %>% sum()
  } else {
    df$rollover <- as.numeric(rollover) %>% sum()
  }
  
  df$externalreceived <- budget[(rollover_i+1):15, 3] %>%
    map(parse_number) %>%
    unlist() %>%
    sum(na.rm = T)
  df$totalrollover <- rowSums(df[,c('rollover', 'externalreceived')],na.rm=T)

  # combine data for all orgs------------------------------------------------
  # print(names(df))
  print(nrow(df))
  data <- bind_rows(data, df)
}

names(data) <- tolower(names(data))

# pivot by item
data <- data %>%
  pivot_longer(
    cols = c(
      "refreshments",
      "supplies",
      "equipment",
      "services",
      "miscellaneous"
    ),
    names_to = "type",
    values_to = "amount"
  )

# categorize events
data$category <- "speaker"
search <- function(col, x, category, orig) {
  pattern <- regex(paste0(".*", paste(x, collapse = ".*|.*"), ".*"))
  col <- tolower(col)
  return(ifelse(str_detect(col, pattern), category, orig))
}
data <- data %>%
  mutate(category = search(`expense name`, c("cultur", "reyes", "celebration"), "cultural", category)) %>%
  mutate(category = search(`expense name`, c("speaker", "policy", "talk"), "speaker", category)) %>%
  mutate(category = search(`expense name`, c("equipment", "publication", "print"), "publication", category)) %>%
  mutate(category = search(`expense name`, c("forum", "conference", "strategic"), "conference", category)) %>%
  mutate(category = search(`expense name`, c("drag", "museum", "hca", "volunteer", "food bank", "trip", "visit", "service"), "community engagement", category)) %>%
  mutate(category = search(`expense name`, c("transition", "study hall", "raise your glass", "eoy", "game night", "bonfire", "gala", "cookout", "all hands", "board", "greet", "social", "budd", "genera", "party", "welcome", "potluck", "mixer", "alumni", "pub\\b"), "social", category)) %>%
  mutate(category = search(`expense name`, c("merch", "regalia", "recruit", "swag", "sticker", "canva", "subscri", "email"), "promotion", category)) %>%
  mutate(category = search(`expense name`, c("personal finance", "program", "case", "negotiation", "challenge", "bootcamp", "interview", "thon", "course", "network", "training", "hire", "workshop", "resume"), "workshop", category)) %>%
  mutate(category = search(`expense name`, c("debate"), "debate", category)) %>%
  mutate(category = search(`expense name`, c("film", "movie", "screen"), "film screening", category))

# reorder columns
other_names <- names(data)[!(names(data) %in% c("org", "type", "category"))]
data <- data[, c("org", "category", "type", other_names)]
write.csv(data, paste0(current_quarter, ".csv"))
