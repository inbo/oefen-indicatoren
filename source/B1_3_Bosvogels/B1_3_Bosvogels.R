#################
# B_3_Bosvogels #
#################

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

gegevens <- read.csv("Bosvogels.csv", sep=";", dec = ",")

# Analyse

# Grafieken

ggplot(gegevens, aes(x = jaar, y = index)) +
  geom_line(colour = "#729BB7", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#729BB7", alpha=0.2) +
  labs(title = "Algemene broedvogelindex voor bosgebieden", 
       y = "Index (2007 = 1)") +
    geom_hline(yintercept = 1, linetype = 2) +
  ylim(0.5, 1.3) +
  scale_x_continuous(labels = scales::number_format(big.mark = "", accuracy = 1), 
                     breaks = scales::pretty_breaks(n = 9)) +
  theme(plot.title = element_text(size = 14, margin = margin(t = 20), vjust = 5),
        axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_blank())
  

