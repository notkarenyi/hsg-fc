quarter <- "Winter 2025"

for (format in c("pdf", "html")) {
  fname <- paste0(
    "Harris Student Government Finance Committee Transparency Report ",
    quarter
  )

  rmarkdown::render(
    paste0(quarter, ".Rmd"),
    encoding = encoding,
    output_file = fname,
    params = list(quarter),
    output_format = paste0(format, "_document")
  )
}

file.remove(paste0(fname, ".log"))
