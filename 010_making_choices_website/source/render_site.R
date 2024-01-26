library(rprojroot)

gitroot <- find_root(is_git_root)
choicesroot <- file.path(gitroot, "010_making_choices_website")
sourceroot <- file.path(choicesroot, "source")
sitesourceroot <- file.path(choicesroot, "source/site")
stopifnot(dir.exists(sitesourceroot))

renv::restore(project = sourceroot)

library(rmarkdown)
library(purrr)

unlink(file.path(choicesroot, "docs"), recursive = TRUE)

# create HTML website
oldwd <- setwd(sitesourceroot)
render_site(output_format = "bookdown::html_book",
            encoding = "UTF-8")

# add 'detailed' PDF files to website
detailed <-
    c("030_concepten",
      "040_systeemschema"
    )
detailed_filenames <-
    c("concepten_details.Rmd",
      "systeemschema_details.Rmd"
    )

source_dirs <- file.path(choicesroot, "source/detailed",
                           detailed)

files_dirs <- file.path(choicesroot, "docs/site/files", detailed)
walk(files_dirs, ~dir.create(., recursive = TRUE, showWarnings = FALSE))

pwalk(list(source_dirs, files_dirs, detailed_filenames),
      function(sd, fd, fn) {
          setwd(sd)
          render(input = fn,
                 output_format = "bookdown::pdf_document2",
                 encoding = "UTF-8",
                 output_dir = fd)
})

# code to interactively build PDF report of the website
if (FALSE) {
    setwd(sitesourceroot)
    render_site(output_format = "bookdown::pdf_document2",
                encoding = "UTF-8")
}
    ### Note that this PDF needs a manual second compilation from the tex file, in order
    ### to avoid some missing cross-references from figure captions:
    ### - verplaats de map mnm_keuzes_files die staat onder _bookdown_files, 1 niveau hoger.
    ###   Dit maakt dat bij de hercompilatie van mnm_keuzes.tex de betreffende figuur-pdf's gevonden worden.
    ### - verplaats mnm_keuzes.tex naar source/site
    ### - vervang in mnm_keuzes.tex de string \\ref door \ref
    ### - hercompileer mnm_keuzes.tex; het resultaat is de pdf onder source/site

# code to interactively build a html page for the detailed files as well
if (FALSE) {
    pwalk(list(source_dirs, detailed_filenames),
          function(sd, fn) {
              setwd(sd)
              render(input = fn,
                     output_format = "bookdown::html_document2",
                     encoding = "UTF-8")
          })
}


# reset working dir
setwd(oldwd)
