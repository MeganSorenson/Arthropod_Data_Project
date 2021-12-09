setwd("~/Documents/2021-2022/renr480/FinalProject/data/r_scripts/used_scripts")

library(plyr)
library(dplyr)
library(tidyverse)
library(sciplot)
library(ggplot2)
library(RColorBrewer)

graphics.off()
x11(width = 5, length = 5)

dev.off()
x11(width = 5, height = 5)

# import data.
temp <- read.csv("coleops_temp_dev_cors.csv")
precip <- read.csv("coleops_precip_dev_cors.csv")

variable <- c(
        "Temperature Correlation Coefficients", "Temperature Correlation Coefficients", 
        "Temperature Correlation Coefficients", "Temperature Correlation Coefficients", 
        "Temperature Correlation Coefficients", "Temperature Correlation Coefficients", 
        "Temperature Correlation Coefficients", "Temperature Correlation Coefficients",
        "Precipitation Correlation Coefficients", "Precipitation Correlation Coefficients", 
        "Precipitation Correlation Coefficients", "Precipitation Correlation Coefficients", 
        "Precipitation Correlation Coefficients", "Precipitation Correlation Coefficients", 
        "Precipitation Correlation Coefficients", "Precipitation Correlation Coefficients")

table <- data.frame(
    Variable = variable,
    Month = c(temp$Month, precip$Month),
    Cors = c(temp$Cor_temp, precip$Cor_precip),
    LowerCI = c(temp$LowerCI, precip$LowerCI),
    UpperCI = c(temp$UpperCI, precip$UpperCI)
)


# temp precip plot

table$Month <- as.factor(table$Month)

ggplot(table, aes(x = Month, y = Cors, ymin = LowerCI, ymax = UpperCI, col = Variable)) +
    geom_point(size = 3, pch = c(20,19,19,19,19,20,20,20,20,20,20,20,20,20,20,20)) +
    geom_errorbar(width = 0.2, lwd = 1.1, lty = c(5,1,1,1,1,5,5,1,5,5,1,1,5,5,5,1)) +
    geom_hline(yintercept = 0, lty = 2) +
    scale_color_manual(values = c("#827f32", "#94251c")) +
    theme_bw() +
    theme(
        legend.position = "none",
        panel.grid = element_blank(),
        axis.title.y = element_text(
            size = 18, face = "bold",
            family = "Times New Roman"
        ),
        axis.text.y = element_text(size = 18, face = "bold", family = "Times New Roman"),
        axis.text.x = element_text(
            size = 18, face = "bold",
            family = "Times New Roman"
        ),
        title = element_text(
            size = 20, face = "bold",
            family = "Times New Roman"
        ),
        strip.text = element_text(
            size = 20, face = "bold",
            family = "Times New Roman"
        )
    ) +
    ylim(-0.5, 0.5) +
    ylab("Kendall's Correlation Coefficient (tau)") +
    ggtitle("Response Function Analysis for 10 Coleopteran Families") +
    facet_grid(~Variable)

