# The file COINT_PPP.XLS contains monthly values of the Japanese, Canadian, and
# Swiss consumer price levels and the bilateral exchange rates with the United
# States. The file also contains the U.S. consumer price level. The names on the
# individual series should be self-evident. For example, JAPANCPI is the
# Japanese price level and JAPANEX is the bilateral Japanese/U.S. exchange rate.
# The starting date for all variables is January 1974 while the availability of
# the variables is such that most end near the end of 2013. The price indices
# have been normalized to equal 100 in January 1973 and only the U.S. price
# index is seasonally adjusted.



# Setup -------------------------------------------------------------------
rm(list = ls())

library(readxl)
library(dplyr)
library(urca)
library(tsDyn)

data <- read_excel("Database/Coint_ppp.38141045.xlsx",
                   na = "NA")

head(data)

series <- c("USCPI","CANEX","CANCPI","JAPANEX","JAPANCPI","SWEX","SWCPI")

data.ln <- data %>% mutate_at(.vars = series, .funs = log)

data.ln <- data.ln %>% filter(YEAR < 2013)

plot(data.ln$SWCPI)

for (col in series) {
  cat(sprintf("Series: %s",col),  "\n")
  plot(data.ln[[col]])
  ADF_test <- urca::ur.df(na.omit(data.ln[[col]]), selectlags = "BIC", lags = 4 )
  
  cat(sprintf("teststat: %f", ADF_test@teststat), "\n")
  cat(ADF_test@cval,  "\n\n")
}

mdl <- lm(JAPANEX ~ JAPANCPI + USCPI, data = data.ln)
summary(mdl)



tbl <- tibble(u = mdl$residuals)

tbl <- tbl %>% mutate(Du = u- lag(u), 
                      u_1 = lag(u),
                      Du_1 = lag(Du, n = 1),
                      Du_2 = lag(Du, n = 2),
                      Du_3 = lag(Du, n = 3),
                      Du_4 = lag(Du, n = 4),
                      Du_5 = lag(Du, n = 5),
                      Du_6 = lag(Du, n = 6),
                      Du_7 = lag(Du, n = 7),
                      Du_8 = lag(Du, n = 8),
                      Du_9 = lag(Du, n = 9),
                      Du_10 = lag(Du, n = 10),
                      Du_11 = lag(Du, n = 11),
                      Du_12 = lag(Du, n = 12)
                      )

tbl

mdl2 <- lm(sprintf("Du ~ -1 + u_1 + %s", paste("Du", 1:11, collapse = " + ", sep = "_")),
           data = tbl)
summary(mdl2)


y <- data.ln %>% select(JAPANEX, JAPANCPI, USCPI)
VECMmdl <- tsDyn::VECM(data = y,
            lag = 1,
            r = 1,
            # beta = c(1, 0.104, 0.768, -9.97),
            beta = c(JAPANEX= 1, USCPI = -25.6621, JAPANCPI = -0.6111205),
            # include = "const",
            LRinclude = "none",
            estim = "ML"
            # estim = "2OLS"
            )

summary(VECMmdl)










