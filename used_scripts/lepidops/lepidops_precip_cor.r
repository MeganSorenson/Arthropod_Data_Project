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

#calculate preciperature deviation
difference <- c()
i <- 1
for (row in data_test$Precip) {
    if (data_test$Month[i] == 4) {
        difference <- c(difference, row / 36.1076 * 100)
    }
    else if (data_test$Month[i] == 5) {
        difference <- c(difference, row /45.8139 * 100)
    }
    else if (data_test$Month[i] == 6) {
        difference <- c(difference, row / 58.3333 * 100)
    }
    else if (data_test$Month[i] == 7) {
        difference <- c(difference, row / 67.2222 * 100)
    }
    else if (data_test$Month[i] == 8) {
        difference <- c(difference, row / 72.8889 * 100)
    }
    else if (data_test$Month[i] == 9) {
        difference <- c(difference, row / 62.4249 * 100)
    }
    else if (data_test$Month[i] == 10) {
        difference <- c(difference, row / 63.6009 * 100)
    }
    else if (data_test$Month[i] == 11) {
        difference <- c(difference, row / 54.1667 * 100)
    }
    i <- i + 1
}

data_test$precip_deviation <- difference


# group by family and month to get average monthly count (by combining all years)
table <- data_test %>%
    group_by(Year, Month, family) %>%
    summarize(avg_monthly_count = mean(Count), median_monthly_count = median(Count), precip = mean(Precip), precip_dev = mean(precip_deviation))

# kendall correlation values for each monthly preciperature
table$avg_monthly_count <- as.numeric(table$avg_monthly_count)
table$precip <- as.numeric(table$precip)
table$precip_dev <- as.numeric(table$precip_dev)

stat_table <- data.frame(stats = c("month", "precip_cor", "precip_cor_p", "lowerCI", "upperCI"))
i <- 4
x <- 2
while (i <= 11) {
    data_table <- table%>%
        filter(Month == i)
    cor <- cor(data_table$avg_monthly_count, data_table$precip, method = c("kendall"))
    cor_p <- cor.test(data_table$avg_monthly_count, data_table$precip, method = c("kendall"))$p.value

    # calculate the sample size
    n <- length(data_table$precip)

    # Bootstrap for 1000 times

    B <- 1000
    boot.cor.all <- c()

    for (a in 1:B){
        index <- sample(1:n, replace=T)
        boot_precip <- data_table$precip[index]
        boot_count <- data_table$avg_monthly_count[index]
        boot.cor <- cor(boot_precip, boot_count, method = c("kendall"))
        boot.cor.all <- c(boot.cor.all, boot.cor)
    }
    # percentile bootstrap 95CI
    lower = quantile(boot.cor.all, prob = c(0.025, 0.975), na.rm = T)[1]
    upper = quantile(boot.cor.all, prob = c(0.025, 0.975), na.rm = T)[2]

    stat_table[x] <- c(i, cor, cor_p, lower, upper)

    i <- i + 1
    x <- x + 1
}

stat_table2 <- data.frame(stats = c("year", "precip_dev_cor", "precip_dev_cor_p", "lowerCI", "upperCI"))
i <- 1992
x <- 2
while (i <= 2009) {
    data_table <- table%>%
        filter(Year == i)
    cor <- cor(data_table$avg_monthly_count, data_table$precip_dev, method = c("kendall"))
    cor_p <- cor.test(data_table$avg_monthly_count, data_table$precip_dev, method = c("kendall"))$p.value

    # calculate the sample size
    n <- length(data_table$precip_dev)

    # Bootstrap for 1000 times

    B <- 1000
    boot.cor.all <- c()

    for (a in 1:B){
        index <- sample(1:n, replace=T)
        boot_precip_dev <- data_table$precip_dev[index]
        boot_count_dev <- data_table$avg_monthly_count[index]
        boot.cor <- cor(boot_precip_dev, boot_count_dev, method = c("kendall"))
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
cor_precip <- c()
i <- 2
while (i < 10){
    cor_precip <- c(cor_precip, stat_table[2, i])
    i <- i + 1
}
cor_precip_p <- c()
i <- 2
while (i < 10) {
    cor_precip_p <- c(cor_precip_p, stat_table[3, i])
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
cor_precip_dev <- c()
i <- 2
while (i < 20){
    cor_precip_dev <- c(cor_precip_dev, stat_table2[2, i])
    i <- i + 1
}
cor_precip_dev_p <- c()
i <- 2
while (i < 20) {
    cor_precip_dev_p <- c(cor_precip_dev_p, stat_table2[3, i])
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
    Cor_precip = cor_precip,
    Cor_precip_p = cor_precip_p,
    Cor_precip_adj_p = p.adjust(cor_precip_p, method = "holm", n = 18),
    LowerCI = lowerCI,
    UpperCI = upperCI
)

write.csv(
    stats,
    "lepidops_precip_cors.csv",
    row.names = F
)

stats2 <- data.frame(
    Year = year,
    Cor_precip_dev = cor_precip_dev,
    Cor_precip_dev_p = cor_precip_dev_p,
    Cor_precip_dev_adj_p = p.adjust(cor_precip_dev_p, method = "holm", n = 18),
    LowerCI = lowerCI_dev,
    UpperCI = upperCI_dev
)


