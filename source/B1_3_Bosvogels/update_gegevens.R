library(here)
library(git2rdata)
library(tidyverse)
root <- repository(here())

# genereer dummy trend data
set.seed(20190920)
expand.grid(
  categorie = c("A", "B", "C"),
  jaar = 2000:2019
) %>%
  group_by(categorie) %>%
  mutate(
    schatting = 2 + rnorm(1, sd = 0.1) + cumsum(rnorm(n(), sd = 0.2))
  ) %>%
  ungroup() %>%
  mutate(
    sd = rnorm(n(), mean = 0.2, sd = 0.05),
    q05 = qnorm(0.05, mean = schatting, sd = sd),
    q20 = qnorm(0.20, mean = schatting, sd = sd),
    q35 = qnorm(0.35, mean = schatting, sd = sd),
    q65 = qnorm(0.65, mean = schatting, sd = sd),
    q80 = qnorm(0.80, mean = schatting, sd = sd),
    q95 = qnorm(0.95, mean = schatting, sd = sd)
  ) %>%
  mutate_at(c("schatting", "q05", "q20", "q35", "q65", "q80", "q95"), exp) ->
  dummy_trend

write_vc(dummy_trend, file = "source/template/dummy_trend", root = root,
         sorting = c("jaar", "categorie"), optimize = FALSE, stage = TRUE)

# eenvoudige tabel

tribble(
  ~"categorie", ~aantal,
  "regionaal uitgestorven",	3,
  "ernstig bedreigd",	7,
  "bedreigd",	3,
  "kwetsbaar",	8,
  "bijna in gevaar",	5,
  "momenteel niet in gevaar",	15,
  "onvoldoende data",	1,
) %>%
  mutate(categorie = factor(categorie, levels = categorie, ordered = TRUE)) %>%
  write_vc(
    file = "source/template/dummy_tabel", root = root, sorting = "categorie",
    stage = TRUE
  )
