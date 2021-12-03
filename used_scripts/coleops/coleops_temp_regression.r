setwd("~/Documents/2021-2022/renr480/FinalProject/data/final_tables")

library(plyr)
library(dplyr)
library(tidyverse)
library(sciplot)
library(ggplot2)
library(lmtest)

graphics.off()
x11(width = 5, length = 5)

dev.off()
x11(width = 5, height = 5)

# import data.
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


# test for normality and homegeneity of variances
plot(lm(data_test$Count ~ data_test$Temp))
res <- residuals(lm(data_test$Count ~ data_test$Temp))
hist(res)

# shows non-parametric and non homogeneity
# use spearman correlation for robust correlation calculations
# BUT... probably can't use these because they assume linearity of the relationship
cor(data_test$Count, data_test$Temp, method = c("spearman"))

# soooo.... should do non-linear regression
# i'm thinking exponential or signmoidal increasing
# try exponential
plot(Count ~ Temp, data = data_test)
curve(1 + 0.05 * 1.5^x, add = T)
model <- nls(Count ~ a + b * c^Temp, data = data_test, start = list(a = 0, b = 0.05, c = 1.5))
summary(model)
plot(Count ~ Temp, data = data_test)
curve(-6.0308 + 0.2629 * 1.3548^x, add = T)
# because a and b not very significant, just plotted the exponential c^x to see how it fit
model2 <- nls(Count ~ c^Temp, data = data_test, start = list(c = 1.5))
summary(model2)
curve(1.254082^x, add = T)
# compare both versions of exponential using AIC
AIC(model)
AIC(model2)

# overall coleopteran regression
model <- nls(Count ~ c^Temp, data = data_test, start = list(c = 1.5))
summary(model)
# now lets do it for each family
family_names_top4 <- c("CARABIDAE", "STAPHYLINIDAE", "TENEBRIONIDAE", "COCCINELLIDAE")
coefficients_a_b_c <- c()
for (family_name in family_names_top4) {
    data <- data_test %>%
        filter(family == family_name)
    model <- nls(Count ~ a + b * c^Temp, data = data, start = list(a = 0, b = 0.05, c = 1.5))
    coef <- list(coef(model))
    param <- list(summary(model)$parameters)
    coefficients_a_b_c <- c(coefficients_a_b_c, coef, param)
}
# carabidae coefficients
coefficients_a_b_c[1]
# carabidae parameters
coefficients_a_b_c[2]

# staphylinidae coefficients
coefficients_a_b_c[3]
# staphylinidae parameters
coefficients_a_b_c[4]

# tenebrionidae coefficients
coefficients_a_b_c[5]
# tenebrionidae parameters
coefficients_a_b_c[6]

# coccinellidae coefficients
coefficients_a_b_c[7]
# coccinellidae parameters
coefficients_a_b_c[8]

# for families with less strong of a relationship, need to have different starting coef in nls()
other_family_names <- c("HYDROPHILIDAE", "CANTHARIDAE", "CRYPTOPHAGIDAE")
data_test_other <- data_test %>%
    filter(family == "HYDROPHILIDAE" | family == "CANTHARIDAE" | family == "CRYPTOPHAGIDAE")

# couldn;t find models that fit Heteroceridae, scarabaeidae, melydridae
other_coefficients_a_b_c <- c()
for (family_name1 in other_family_names) {
    data1 <- data_test_other %>%
        filter(family == family_name1)
    model <- nls(Count ~ a + b * c^Temp, data = data1, start = list(a = 0, b = 0.05, c = 1.2))
    coef <- list(coef(model))
    param <- list(summary(model)$parameters)
    other_coefficients_a_b_c <- c(other_coefficients_a_b_c, coef, param)
}

# test starting points
plot(Count ~ Temp, data = data_test_other)
curve(1 + 0.05 * 1.4^x, add = T)
nls(Count ~ a + b * c^Temp, data = data_test_other, start = list(a = 0, b = 0.05, c = 1.2))

# hydrophilidae coefficients
other_coefficients_a_b_c[1]
# hydrophilidae parameters
other_coefficients_a_b_c[2]

# cantharidae coefficients
other_coefficients_a_b_c[3]
# cantharidae parameters
other_coefficients_a_b_c[4]

# cryptophagidae coefficients
other_coefficients_a_b_c[5]
# cryptophagidae parameters
other_coefficients_a_b_c[6]

data_test1 <- data_test %>%
filter(family == "CARABIDAE")
nls(Count ~ a + b * c^Temp, data = data_test1, start = list(a = 0, b = 0.05, c = 1.2))
data_test2 <- data_test %>%
filter(family == "HYDROPHILIDAE")
nls(Count ~ a + b * c^Temp, data = data_test2, start = list(a = 0, b = 0.05, c = 1.2))
