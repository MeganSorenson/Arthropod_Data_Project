setwd("~/Documents/2021-2022/renr480/FinalProject/data/final_tables")

library(plyr)
library(dplyr)
library(tidyverse)
library(sciplot)
library(ggplot2)
library(lmtest)
library(RColorBrewer)

graphics.off()
x11(width = 5, length = 5)

dev.off()
x11(width = 5, height = 5)

# import data
data <- read.csv("Lepidops_Climate_monthly.csv")
# take away non needed months and families
families <- c(
    "STAPHYLINIDAE", "CARABIDAE", "COCCINELLIDAE",
    "TENEBRIONIDAE", "HYDROPHILIDAE", "CANTHARIDAE",
    "CRYPTOPHAGIDAE", "MELYRIDAE",
    "SCARABAEIDAE", "HETEROCERIDAE",
    "BATRACHEDRIDAE", "COLEOPHORIDAE", "GELECHIIDAE",
    "GEOMETRIDAE", "GRACILLARIIDAE", "LYONETIDAE",
    "NEPTICULIDAE", "NOCTUIDAE", "OECOPHORIDAE",
    "PLUTELLIDAE", "PYRALIDAE", "TORTRICIDAE",
    "YPONOMEUTIDAE"
)
data_test <- data %>%
    subset(Month != 1 & Month != 2 & Month != 3 & Month != 12) %>%
    subset(family %in% families)
# havve weird NA's... take out
data_test <- data_test %>%
    filter(!is.na(Year))

# plot
data_test$Month <- as.factor(data_test$Month)

ggplot(data_test, aes(x = Precip, y = Count, col = Month)) +
    geom_point() +
    #geom_point(shape = 1) +
    theme_bw() +
    scale_color_brewer(palette = "Dark2") +
    theme(
        legend.position = "top",
        legend.text = element_text(size = 18, face = "bold", family = "Times New Roman"),
        panel.grid = element_blank(),
        axis.title.y = element_text(
            size = 18, face = "bold",
            family = "Times New Roman"
        ),
        axis.text.y = element_text(size = 18, family = "Times New Roman"),
        axis.text.x = element_text(
            size = 18, face = "bold",
            family = "Times New Roman"
        ),
        title = element_text(
            size = 20, face = "bold",
            family = "Times New Roman"
        )
    ) +
    xlab("Precipitation (mm)") +
    ylab("Insect Count (# individuals)") +
    ggtitle("C. Insect Counts vs Precipitation (1992-2009) for 13 Lepidopteran Families") +
    facet_wrap(~ factor(family, levels = c(
        "YPONOMEUTIDAE", "NOCTUIDAE", "TORTRICIDAE", "PYRALIDAE",
        "GRACILLARIIDAE", "PLUTELLIDAE", "OECOPHORIDAE", "GEOMETRIDAE",
        "LYONETIDAE", "COLEOPHORIDAE", "NEPTICULIDAE",
        "GELECHIIDAE", "BATRACHEDRIDAE"
    )))
