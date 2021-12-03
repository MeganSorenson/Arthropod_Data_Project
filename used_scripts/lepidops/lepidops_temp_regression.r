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
levels(data_test$family)
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
curve(1 + 0.05 * 1.7^x, add = T)
model <- nls(Count ~ a + b * c^Temp, data = data_test, start = list(a = 0, b = 0.05, c = 1.7))
summary(model)
plot(Count ~ Temp, data = data_test)
curve(-4.90493 + 0.29585 * 1.46293^x, add = T)
# because a and b not very significant, just plotted the exponential c^x to see how it fit
nls(Count ~ c^Temp, data = data_test, start = list(c = 1.7))
curve(1.372^x, add = T)
# compare both versions of exponential using AIC
AIC(nls(Count ~ a + b * c^Temp, data = data_test, start = list(a = 0, b = 0.05, c = 1.7)))
AIC(nls(Count ~ c^Temp, data = data_test, start = list(c = 1.7)))

# overall lepidopteran exponential increase
model <- nls(Count ~ c^Temp, data = data_test, start = list(c = 1.5))
summary(model)

# now lets do it for each family
family_names_top4 <- c("YPONOMEUTIDAE", "NOCTUIDAE", "TORTRICIDAE", "PYRALIDAE")
coefficients_a_b_c <- c()
for (family_name in family_names_top4) {
    data <- data_test %>%
        filter(family == family_name)
    model <- nls(Count ~ a + b * c^Temp, data = data, start = list(a = 0, b = 0.05, c = 1.7), control = nls.control(maxiter = 1000))
    coef <- list(coef(model))
    param <- list(summary(model)$parameters)
    coefficients_a_b_c <- c(coefficients_a_b_c, coef, param)
}
# yponomeutidae coefficients
coefficients_a_b_c[1]
# yponomeutidae parameters
coefficients_a_b_c[2]

# noctuidae coefficients
coefficients_a_b_c[3]
# noctuidae parameters
coefficients_a_b_c[4]

# tortricidae coefficients
coefficients_a_b_c[5]
# tortricidae parameters
coefficients_a_b_c[6]

# pyralidae coefficients
coefficients_a_b_c[7]
# pyralidae parameters
coefficients_a_b_c[8]

# for families with less strong of a relationship, need to have different starting coef in nls()
other_family_names <- c(
    "GRACILLARIIDAE", "PLUTELLIDAE", "OECOPHORIDAE",
    "GEOMETRIDAE", "LYONETIDAE"
)
data_test_other <- data_test %>%
    filter(family == "GRACILLARIIDAE" | family == "PLUTELLIDAE" | family == "OECOPHORIDAE" |
        family == "GEOMETRIDAE" | family == "LYONETIDAE")

# test starting points
plot(Count ~ Temp, data = data_test_other)
curve(1 + 0.05 * 1.5^x, add = T)
nls(Count ~ a + b * c^Temp, data = data_test_other, start = list(a = 0, b = 0.05, c = 1.5))
# couldn;t find models that fit coleophoridae, nepticulidae, gelechidae, batrachedridae
other_coefficients_a_b_c <- c()
for (family_name in other_family_names) {
    data <- data_test %>%
        filter(family == family_name)
    model <- nls(Count ~ a + b * c^Temp, data = data, start = list(a = 0, b = 0.05, c = 1.5), control = nls.control(maxiter = 1000))
    coef <- list(coef(model))
    param <- list(summary(model)$parameters)
    other_coefficients_a_b_c <- c(coefficients_a_b_c, coef, param)
}
# gracillaridae coefficients
other_coefficients_a_b_c[1]
# gracillaridae parameters
other_coefficients_a_b_c[2]

# plutellidae coefficients
other_coefficients_a_b_c[3]
# plutellidae parameters
other_coefficients_a_b_c[4]

# oecophotidae coefficients
other_coefficients_a_b_c[5]
# oecophoridae parameters
other_coefficients_a_b_c[6]

# geometridae coefficients
other_coefficients_a_b_c[7]
# geometridae parameters
other_coefficients_a_b_c[8]

# lyonetidae coefficients
other_coefficients_a_b_c[9]
# lyonetidae parameters
other_coefficients_a_b_c[10]