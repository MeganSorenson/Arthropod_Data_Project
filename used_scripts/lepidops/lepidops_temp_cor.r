setwd("~/Documents/2021-2022/renr480/FinalProject/data/final_tables")

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

#calculate temperature deviation
difference <- c()
i <- 1
for (row in data_test$Temp) {
    if (data_test$Month[i] == 4) {
        difference <- c(difference, row - 7.9552)
    }
    else if (data_test$Month[i] == 5) {
        difference <- c(difference, row - 12.6087)
    }
    else if (data_test$Month[i] == 6) {
        difference <- c(difference, row - 15.9056)
    }
    else if (data_test$Month[i] == 7) {
        difference <- c(difference, row - 18.2056)
    }
    else if (data_test$Month[i] == 8) {
        difference <- c(difference, row - 18.0333)
    }
    else if (data_test$Month[i] == 9) {
        difference <- c(difference, row - 14.3571)
    }
    else if (data_test$Month[i] == 10) {
        difference <- c(difference, row - 9.791)
    }
    else if (data_test$Month[i] == 11) {
        difference <- c(difference, row - 4.8889)
    }
    i <- i + 1
}

data_test$temp_deviation <- difference


# group by family and month to get average monthly count (by combining all years)
table <- data_test %>%
    group_by(Year, Month, family) %>%
    summarize(avg_monthly_count = mean(Count), median_monthly_count = median(Count), temp = mean(Temp), temp_dev = mean(temp_deviation))

# kendall correlation values for each monthly temperature
table$avg_monthly_count <- as.numeric(table$avg_monthly_count)
table$temp <- as.numeric(table$temp)
table$temp_dev <- as.numeric(table$temp_dev)

stat_table <- data.frame(stats = c("month", "temp_cor", "temp_cor_p", "lowerCI", "upperCI"))
i <- 4
x <- 2
while (i <= 11) {
    data_table <- table%>%
        filter(Month == i)
    cor <- cor(data_table$avg_monthly_count, data_table$temp, method = c("kendall"))
    cor_p <- cor.test(data_table$avg_monthly_count, data_table$temp, method = c("kendall"))$p.value

    # calculate the sample size
    n <- length(data_table$temp)

    # Bootstrap for 1000 times

    B <- 1000
    boot.cor.all <- c()

    for (a in 1:B){
        index <- sample(1:n, replace=T)
        boot_temp <- data_table$temp[index]
        boot_count <- data_table$avg_monthly_count[index]
        boot.cor <- cor(boot_temp, boot_count, method = c("kendall"))
        boot.cor.all <- c(boot.cor.all, boot.cor)
    }
    # percentile bootstrap 95CI
    lower = quantile(boot.cor.all, prob = c(0.025, 0.975), na.rm = T)[1]
    upper = quantile(boot.cor.all, prob = c(0.025, 0.975), na.rm = T)[2]

    stat_table[x] <- c(i, cor, cor_p, lower, upper)

    i <- i + 1
    x <- x + 1
}

stat_table2 <- data.frame(stats = c("year", "temp_dev_cor", "temp_dev_cor_p", "lowerCI", "upperCI"))
i <- 1992
x <- 2
while (i <= 2009) {
    data_table <- table%>%
        filter(Year == i)
    cor <- cor(data_table$avg_monthly_count, data_table$temp_dev, method = c("kendall"))
    cor_p <- cor.test(data_table$avg_monthly_count, data_table$temp_dev, method = c("kendall"))$p.value

    # calculate the sample size
    n <- length(data_table$temp_dev)

    # Bootstrap for 1000 times

    B <- 1000
    boot.cor.all <- c()

    for (a in 1:B){
        index <- sample(1:n, replace=T)
        boot_temp_dev <- data_table$temp_dev[index]
        boot_count_dev <- data_table$avg_monthly_count[index]
        boot.cor <- cor(boot_temp_dev, boot_count_dev, method = c("kendall"))
        boot.cor.all <- c(boot.cor.all, boot.cor)
    }
    # percentile bootstrap 95CI
    lower = quantile(boot.cor.all, prob = c(0.025, 0.975), na.rm = T)[1]
    upper = quantile(boot.cor.all, prob = c(0.025, 0.975), na.rm = T)[2]

    stat_table2[x] <- c(i, cor, cor_p, lower, upper)
    i <- i + 1
    x <- x + 1
}

month <- c()
i <- 2
while (i < 10){
    month <- c(month, stat_table[1, i])
    i <- i + 1
}
cor_temp <- c()
i <- 2
while (i < 10){
    cor_temp <- c(cor_temp, stat_table[2, i])
    i <- i + 1
}
cor_temp_p <- c()
i <- 2
while (i < 10) {
    cor_temp_p <- c(cor_temp_p, stat_table[3, i])
    i <- i + 1
}
lowerCI <- c()
i <- 2
while (i < 10) {
    lowerCI <- c(lowerCI, stat_table[4, i])
    i <- i + 1
}
upperCI <- c()
i <- 2
while (i < 10) {
    upperCI <- c(upperCI, stat_table[5, i])
    i <- i + 1
}
year <- c()
i <- 2
while (i < 20) {
    year <- c(year, stat_table2[1, i])
    i <- i + 1
}
cor_temp_dev <- c()
i <- 2
while (i < 20){
    cor_temp_dev <- c(cor_temp_dev, stat_table2[2, i])
    i <- i + 1
}
cor_temp_dev_p <- c()
i <- 2
while (i < 20) {
    cor_temp_dev_p <- c(cor_temp_dev_p, stat_table2[3, i])
    i <- i + 1
}
lowerCI_dev <- c()
i <- 2
while (i < 20) {
    lowerCI_dev <- c(lowerCI_dev, stat_table2[4, i])
    i <- i + 1
}
upperCI_dev <- c()
i <- 2
while (i < 20) {
    upperCI_dev <- c(upperCI_dev, stat_table2[5, i])
    i <- i + 1
}

stats <- data.frame(
    Month = month,
    Cor_temp = cor_temp,
    Cor_temp_p = cor_temp_p,
    Cor_temp_adj_p = p.adjust(cor_temp_p, method = "holm", n = 18),
    LowerCI = lowerCI,
    UpperCI = upperCI
)

write.csv(
    stats,
    "lepidops_temp_cors.csv",
    row.names = F
)

stats2 <- data.frame(
    Year = year,
    Cor_temp_dev = cor_temp_dev,
    Cor_temp_dev_p = cor_temp_dev_p,
    Cor_temp_dev_adj_p = p.adjust(cor_temp_dev_p, method = "holm", n = 18),
    LowerCI = lowerCI_dev,
    UpperCI = upperCI_dev
)


