bookdown::render_book("index.Rmd",
                      "bookdown::html_document2")

# Run in shell to invoke non-interactive R session:
# Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::html_document2", params = list(compile_analysisdata = FALSE))'
# Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::html_document2", params = list(connect_watina = TRUE))'
