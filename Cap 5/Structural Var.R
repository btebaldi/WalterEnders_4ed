#  clear all
rm(list = ls())

# library
library(readxl)
library(ggplot2)
library(tidyr)
library(vars)
library(dplyr)

data <- read_excel("./Database/ENDERS_HOLT.xls", 
                   col_types = c("numeric", "numeric",
                                 "numeric", "numeric"),
                   range = cell_limits(c(1, 2), c(NA, 5)),
                   na = "NA")

# show the data
head(data)

# Create a sequence column
data$Id <- 1:nrow(data)

# remove NA from data
data <- na.omit(data)

# plot of data
ggplot(tidyr::pivot_longer(data, cols = -Id)) +
  geom_line(aes(Id, value)) + 
  facet_wrap(~name, scale="free")


data.core <- data[,-5]
var_12 <- vars::VAR(y=data.core, p=12, type = "const")

s = summary(var_12)

logLik(var_12)
AIC(var_12)

# AIC is defined as:
# AICm = ln(det(sigma)) + (2pk^2)/N
# p = lag order, K = num serie
p=12
k=4
N <- nrow(data) - p
var_12$obs
AIC_12 = log(det(s$covres))  + (2*p*k^2)/N
AIC_12
summary(var_12)

vars::VARselect(data.core, lag.max = 12, type = "none")

residuo <- cbind(var_12$varresult$PE$residuals, var_12$varresult$ex$residuals, var_12$varresult$r$residuals, var_12$varresult$pg$residuals)

crossprod(residuo)/s$covres

head(residuals(var_12))

(455-12) - 12*4

length( na.omit( lag(data.core$PE, 11)))
nrow(embed(as.matrix(data.core$PE), 13))

sample <- (455-12)
sigma.det <- det(s$covres)

detint <- 0

aa <- log(sigma.det) + (2/sample) * (p * k^2 + k * 0)


job(data.core, lag.max = 12, type = "none")

x <- 1:20
embed (x, 10)







