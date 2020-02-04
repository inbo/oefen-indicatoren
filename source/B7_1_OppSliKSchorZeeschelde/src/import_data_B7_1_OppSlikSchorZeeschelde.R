#voorlopig wordt script niet gebruikt, want version control is  te stringert
library(knitr)
opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
library(tinytex)
library(git2rdata)
library(plotly)
library(tidyverse)
library(effectclass)
library(INBOtheme)
library(htmlwidgets)
library(DT) # Interactieve tabellen
set.seed(20190920)
setWidgetIdSeed(20190920)
options(htmlwidgets.TOJSON_ARGS = list(pretty = TRUE))

root<-(".")
gegevens<-read.csv2("Fysiotopen_ruwedata.csv")
df <- data.frame(gegevens)
df$deel <- as.factor(df$deel)
df$zone <- as.factor(df$zone)
df$fysiotoop <- as.factor(df$fysiotoop)
df1<-data.frame(df %>%
                  arrange(Jaar, zone, fysiotoop) %>%
                  group_by(Jaar,zone) %>%
                  summarise(Oppervlakte = sum(oppervlakte)) )%>%
  filter(zone!='subtidaal')
df1 %>%group_by(zone) %>%
  summarise(minopp=min(Oppervlakte),
            maxopp=max(Oppervlakte),
            toename=(maxopp-minopp))
write_vc(df1, file="df1", root=root, sorting= "Jaar", Sorting ='zone')
