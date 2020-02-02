renv::restore()
library(rmarkdown)
library(here)
render_all <- function() {
  to_do <- list.files(here("source"), pattern = ".rmd", ignore.case = TRUE,
                      recursive = TRUE, full.names = TRUE)
  to_do <- normalizePath(to_do)
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  for (i in to_do) {
    setwd(dirname(i))
    message(i)
    z <- render(i)

    target <- gsub(
      file.path(old_wd, "source"),
      file.path(old_wd, "publish"),
      dirname(i)
    )
    dir.create(target, showWarnings = FALSE, recursive = TRUE)

    # copy html
    file.copy(z, file.path(target, basename(z)), overwrite = TRUE)
    file.remove(z)

    # copy libs
    file.copy(file.path(dirname(z), "libs"), target,
              recursive = TRUE)

    # copy md
    z <- gsub(".html", ".md", z)
    file.copy(z, file.path(target, basename(z)), overwrite = TRUE)
    md <- readLines(z)
    file.remove(z)

    #copy internal links
    base_links <- md[grep("\\[.*\\]\\(.*\\)", md)]
    links <- character(0)
    while (length(base_links) > 0) {
      links <- c(links, gsub(".*?\\[.*?\\]\\((.*?)\\).*", "\\1", base_links))
      base_links <- gsub(".*?\\[.*?\\]\\((.*?)\\)(.*)", "\\2", base_links)
      base_links <- base_links[grep("\\[.*\\]\\(.*\\)", base_links)]
    }
    links <- links[!grepl("^https?://", links)]
    links <- normalizePath(
      file.path(dirname(i), links),
      mustWork = TRUE,
      winslash = "/"
    )
    if (length(links) > 0) {
      link_target <- gsub(
        file.path(old_wd, "source"),
        file.path(old_wd, "publish"),
        links
      )
      vapply(
        unique(dirname(link_target)),
        dir.create,
        logical(1),
        recursive = TRUE,
        showWarnings = FALSE
      )
      file.copy(links, link_target, overwrite = TRUE)
    }
  }
  setwd(old_wd)
}
render_all()
