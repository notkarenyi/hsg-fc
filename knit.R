report_quarter <- "Winter 2025"
current_quarter <- "Spring 2025"

source('clean_data.R')

for (format in c("pdf", "html")) {
  fname <- paste0(
    "Harris Student Government Finance Committee Transparency Report ",
    report_quarter
  )

  rmarkdown::render(
    paste0(report_quarter, ".Rmd"),
    encoding = encoding,
    output_file = fname,
    params = list(report_quarter),
    output_format = paste0(format, "_document")
  )
}

file.remove(paste0(fname, ".log"))
