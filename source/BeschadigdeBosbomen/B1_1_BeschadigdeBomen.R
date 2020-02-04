#########################
# B_1_Beschadigde bomen #
#########################

# Packages

library(knitr)
library(git2rdata)
library(plotly)
library(tidyverse)
library(effectclass) # remotes::install_github("inbo/effectclass") -> niets updaten
library(INBOtheme)
library(htmlwidgets)
library(DT) # Interactieve tabellen
theme_set(theme_inbo(10))
# theme_inbo <- theme_update(axis.title.x = element_text(colour = "black"),
#                            axis.title.y = element_text(colour = "#843860"),
#                            plot.title = element_text(colour = "black"),
#                            legend.key = element_rect(fill = "white"))
# update_geom_defaults("point", aes(size = 2))
# update_geom_defaults("line", aes(size = 0.25))

# Data

gegevens <- read.csv("BeschadigdeBomen.csv", sep=";", dec = ",")
gegevenslong <- gegevens %>% 
  gather(key = "soort", value = "perc", -Jaar) %>% 
  filter(!soort %in% c("Naaldbomen", "Loofbomen", "Totaal"))

# Analyse

# Grafieken

ggplot(gegevenslong, aes(x = Jaar, y = perc, colour = soort)) +
  geom_smooth(aes(fill = factor(soort))) +
  labs(title = "Aandeel beschadigde bosbomen", 
       y = "Beschadigde bomen (%)") +
  theme(plot.title = element_text(size = 14, margin = margin(t = 20), vjust = 5),
        axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_blank()) +
  guides(fill = FALSE)


