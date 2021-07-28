# Miscellaneous functions

execshell <- function(commandstring, intern = FALSE) {
    if (.Platform$OS.type == "windows") {
        res <- shell(commandstring, intern = TRUE)
    } else {
        res <- system(commandstring, intern = TRUE)
    }
    if (!intern) cat(res, sep = "\n") else return(res)
}

