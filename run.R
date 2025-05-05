
# config ------------------------------------------------------------------

report_quarter <- "Winter 2025"

current_quarter <- "Spring 2025"

update_names <- function(x) {
  x <- x %>%
    mutate(org = search(org, c("acpf", "caste"), "Anti-Caste Policy Forum", org)) %>%
    mutate(org = search(org, c("apf", "asian policy"), "Asian Policy Forum", org)) %>%
    mutate(org = search(org, c("alas", "latinx"), "Association of Latinx Students for Social Justice", org)) %>%
    mutate(org = search(org, c("bapps", "black"), "Black Action in Public Policy Studies", org)) %>%
    mutate(org = search(org, c("bep", "behavior"), "Behavioral Science and Public Policy", org)) %>%
    mutate(org = search(org, c("cjpa", "crime"), "Crime & Justice Policy Association", org)) %>%
    mutate(org = search(org, c("cpr", "chicago"), "Chicago Policy Review", org)) %>%
    mutate(org = search(org, c("dep", "ethics"), "Data, Ethics, & Policy Conference", org)) %>%
    mutate(org = search(org, c("epf", "europe"), "European Policy Forum", org)) %>%
    mutate(org = search(org, c("epsa", "education"), "Education Policy Student Assocation", org)) %>%
    mutate(org = search(org, c("hca", "community"), "Harris Community Action", org)) %>%
    mutate(org = search(org, c("hcc", "consult"), "Harris Consulting Club", org)) %>%
    mutate(org = search(org, c("heea", "he&ea", "environment"), "Harris Energy and Environmental Association", org)) %>%
    mutate(org = search(org, c("hfp", "palestine"), "Harris Students for Palestine", org)) %>%
    mutate(org = search(org, c("hsg", "government"), "Harris Student Government", org)) %>%
    mutate(org = search(org, c("hive", "venture"), "Harris Innovation & Venture Ecosystem", org)) %>%
    mutate(org = search(org, c("hii", "invest"), "Harris Impact Investing", org)) %>%
    mutate(org = search(org, c("hni", "health"), "Health Nexus Initiative", org)) %>%
    mutate(org = search(org, c("idpa", "international"), "International Development Policy Association", org)) %>%
    mutate(org = search(org, c("lam", "latin america"), "Latin America Matters", org)) %>%
    mutate(org = search(org, c("mash", "military"), "Military-Affiliated Students and Veterans", org)) %>%
    mutate(org = search(org, c("mipps", "minorities"), "Minorities in Public Policy Studies", org)) %>%
    mutate(org = search(org, c("outpolitik"), "OutPolitik", org)) %>%
    mutate(org = search(org, c("pfa", "public finance"), "Public Finance Association", org)) %>%
    mutate(org = search(org, c("pmpsa", "management"), "Public Management and Policy Student Association", org)) %>%
    mutate(org = search(org, c("rac", "rural"), "Rural America Caucus", org)) %>%
    mutate(org = search(org, c("saspa", "south asian"), "South Asian Students' Policy  Association", org)) %>%
    mutate(org = search(org, c("ssc", "civic"), "South Side Civic", org)) %>%
    mutate(org = search(org, c("tap", "african"), "The African Perspective", org)) %>%
    mutate(org = search(org, c("uc3p", "podcast"), "University of Chicago Public Policy Podcasts", org)) %>%
    mutate(org = search(org, c("unum"), "Unum", org)) %>%
    mutate(org = search(org, c("upsa", "urban"), "Urban Policy Student Association", org)) %>%
    mutate(org = search(org, c("wipp", "women"), "Women in Public Policy", org)) %>%
    mutate(org = search(org, c("heea", "environment"), "Unum", org))
  
  return(x)
}

# prepping ----------------------------------------------------------------

# compile data for this quarter's allocations
if (!file.exists(paste0(current_quarter, ".csv"))) {
  source("clean_data.R")
}

# standardize org names
actual <- read.csv(paste0(current_quarter, ".csv"))
actual <- update_names(actual)

# add totals requested and planned from last quarter
planned <- read.csv(paste0(report_quarter, ".csv")) %>%
  update_names()

planned <- planned %>%
  select(-actual, -attendactual, -eventsactual)
actual <- actual %>%
  group_by(org) %>%
  summarize(
    actual = mean(actual),
    attendactual = mean(attendactual),
    eventsactual = mean(eventsactual)
  )

df <- left_join(planned, actual)

# add totals allocated for last quarter
allocated <- read_excel(paste0(report_quarter, " Allocations.xlsx")) %>%
  remove_empty('rows')
names(allocated)[1] <- 'org'
allocated <- update_names(allocated)

df <- left_joined(df, allocated)
names(df) <- tolower(names(df))

# run reports -------------------------------------------------------------

for (format in c("pdf", "html")) {
  fp <- paste0(
    "Harris Student Government Finance Committee Transparency Report ",
    report_quarter
  )

  rmarkdown::render(
    paste0(report_quarter, ".Rmd"),
    params = list(report_quarter),
    output_file = fp,
    output_format = paste0(format, "_document")
  )
}

file.remove(paste0(fp, ".log"))
