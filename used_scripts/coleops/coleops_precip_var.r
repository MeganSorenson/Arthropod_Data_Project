setwd("~/Documents/2021-2022/renr480/FinalProject/data/final_tables")

library(plyr)
library(dplyr)
library(tidyverse)
library(sciplot)
library(ggplot2)

graphics.off()
x11(width = 5, length = 5)

dev.off()
x11(width = 5, height = 5)

# import data.
# import data
data <- read.csv("Coleops_Climate_monthly.csv")
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

#calculate average monthly temperatures
monthly_averages <- data_test %>%
    group_by(Month) %>%
    summarize(mean(Precip))

# create new column within dataset that is percent difference from monthly average precip
difference <- c()
i <- 1
for (row in data_test$Precip) {
    if (data_test$Month[i] == 4) {
        difference <- c(difference, row / 36.0556 * 100)
    }
    else if (data_test$Month[i] == 5) {
        difference <- c(difference, row /46.3588 * 100)
    }
    else if (data_test$Month[i] == 6) {
        difference <- c(difference, row / 58.4709 * 100)
    }
    else if (data_test$Month[i] == 7) {
        difference <- c(difference, row / 66.6909 * 100)
    }
    else if (data_test$Month[i] == 8) {
        difference <- c(difference, row / 72.8889 * 100)
    }
    else if (data_test$Month[i] == 9) {
        difference <- c(difference, row / 62.3483 * 100)
    }
    else if (data_test$Month[i] == 10) {
        difference <- c(difference, row / 63.7486 * 100)
    }
    else if (data_test$Month[i] == 11) {
        difference <- c(difference, row / 54.1667 * 100)
    }
    i <- i + 1
}

data_test$precip_deviation <- difference

# summer coleop counts by family
data_test$Family2 <- data_test$family
families <- c(
    "STAPHYLINIDAE", "CARABIDAE", "COCCINELLIDAE",
    "TENEBRIONIDAE", "HYDROPHILIDAE", "CANTHARIDAE",
    "CRYPTOPHAGIDAE", "MELYRIDAE",
    "SCARABAEIDAE", "HETEROCERIDAE"
)
data_test <- data_test %>%
    filter(family %in% families)

#plot
data_test$Year <- as.factor(data_test$Year)

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

ggplot(data_test, aes(x = precip_deviation, y = Count, col = Year)) +
    geom_point() +
    #geom_point(shape = 1) +
    theme_bw() +
    scale_color_manual(values = c25) +
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
    ylim(0, 50) +
    xlab("Deviation from Average Monthly Precipitation (%)") +
    ylab("Insect Count (# individuals)") +
    ggtitle("E. Insect Counts vs Precipitation Deviation (1992-2009) for 10 Coleopteran Families") +
    facet_wrap(~ factor(family, levels = c(
        "CARABIDAE", "STAPHYLINIDAE", "TENEBRIONIDAE",
        "COCCINELLIDAE", "HYDROPHILIDAE", "CANTHARIDAE",
        "CRYPTOPHAGIDAE", "MELYRIDAE",
        "SCARABAEIDAE", "HETEROCERIDAE"
    )))
