# function for output dependent inline code formatting
ttcode <- function(x, type = "tt") {
  outputFormat <- knitr:::pandoc_to()
  if (outputFormat %in% c('latex', 'beamer'))
    paste0("\\texttt{", escape_latex(x), "}")
  else if (outputFormat == 'html')
    paste0("<", type, ">", x, "</", type, ">")
  else
    x
}

# Formatierung von gt-Tabellen
tabopts <- function(x) {
    fmt_number(x, decimals = 3, drop_trailing_zeros = T) %>%
  tab_options(table_body.hlines.color = "white", 
              column_labels.border.bottom.color = "black", 
             column_labels.border.top.color = "black",
             table_body.border.bottom.color = "black", 
             table.border.bottom.color = "black",
             column_labels.font.weight = "bold", 
             table.font.color = "black", 
             table.font.size = 16)
}