library(here)
library(git2rdata)
library(tidyverse)
library(googlesheets4)
root <- repository(here())
id <- "1lVOeuqyofUMR_6wd3wZAhuBSWOtoh5aXLHZuZKnzTJI"
natuurbeheer <- read_sheet(ss = id, sheet = "Blad1")
natuurbeheer %>%
  gather(Type, Oppervlakte, -Jaartal, na.rm = TRUE, factor_key = TRUE) %>%
  write_vc(
    file = "source/natuurbeheer/oppervlakte", root = root, stage = TRUE,
    optimize = FALSE, sorting = c("Jaartal", "Type")
  )
