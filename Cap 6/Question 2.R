# clear all
rm(list=ls())

# libraries
library(readxl)
library(ggplot2)

# 2. The data file COINT6.XLS contains the three simulated series used in Sections 5 and 9.
# a. Use the data to reproduce the results in Section 5.
# b. Obtain the impulse responses and variance decompositions using the ordering such that
#    yt → zt → wt . Do these seem reasonable, given the way in which the variables were
#    constructed?
# c. Use the data to reproduce the results in Section 9.
# d. Examine Table 6.1. Show that yt and zt , but not wt , are weakly exogenous.
# e. Use the data to compare the ECM test to the Engle–Granger and Johansen tests treating
#    yt and zt as weakly exogenous.


# Load database
table <- read_excel("Database/COINT6.xlsx")

# preview the database
head(table)

# Reproduction of the figure 6.2
table %>% mutate(Id=row_number()) %>% 
  ggplot() +
  geom_line(aes(x=Id, y=y), color="black") +
  geom_line(aes(x=Id, y=z), color="red") +
  geom_line(aes(x=Id, y=w), color="blue") +
  labs(title = "FIGURE 6.2 Three Cointegrated Series",
       x= NULL,
       y= NULL)



# Estimation of the three models on page 366
mdl.y <- lm(y~z+w, data = table)
summary(mdl.y)

mdl.z <- lm(z~y+w, data = table)
summary(mdl.z)

mdl.w <- lm(w~y+z, data = table)
summary(mdl.w)


# save the error of the model w
table$e_wt <- mdl.w$residuals



# Results of table 6.3
error.table <- tibble(e_y=mdl.y$residuals, e_z=mdl.z$residuals, e_w=mdl.w$residuals)

error.table <- error.table %>% 
  mutate(De_y = e_y - lag(e_y),
         De_z = e_z - lag(e_z),
         De_w = e_w - lag(e_w),
         e_y1 = lag(e_y),
         e_z1 = lag(e_z),
         e_w1 = lag(e_w), 
         De_y1 = lag(De_y),
         De_z1 = lag(De_z),
         De_w1 = lag(De_w),
         De_y2 = lag(De_y, n=2),
         De_z2 = lag(De_z, n=2),
         De_w2 = lag(De_w, n=2),
         De_y3 = lag(De_y, n=3),
         De_z3 = lag(De_z, n=3),
         De_w3 = lag(De_w, n=3),
         De_y4 = lag(De_y, n=4),
         De_z4 = lag(De_z, n=4),
         De_w4 = lag(De_w, n=4))

error.table

mdl.ey_0 <- lm(De_y ~ e_y1 - 1, data = error.table)
mdl.ey_4 <- lm(De_y ~ e_y1 + De_y1 + De_y2 + De_y3 + De_y4 -1, data=error.table)

summary(mdl.ey_0)
summary(mdl.ey_4)


mdl.ez_0 <- lm(De_z ~ e_z1 - 1, data = error.table)
mdl.ez_4 <- lm(De_z ~ e_z1 + De_z1 + De_z2 + De_z3 + De_z4 -1, data=error.table)

summary(mdl.ez_0)
summary(mdl.ez_4)

mdl.ew_0 <- lm(De_w ~ e_w1 - 1, data = error.table)
mdl.ew_4 <- lm(De_w ~ e_w1 + De_w1 + De_w2 + De_w3 + De_w4 -1, data=error.table)

summary(mdl.ew_0)
summary(mdl.ew_4)


table_6.3 <- matrix(NA, nrow = 6, ncol = 2)
table_6.3[c(1,2),1] = summary(mdl.ey_0)[["coefficients"]]["e_y1", c("Estimate", "t value")]
table_6.3[c(1,2),2] = summary(mdl.ey_4)[["coefficients"]]["e_y1", c("Estimate", "t value")]
table_6.3[c(3,4),1] = summary(mdl.ez_4)[["coefficients"]]["e_z1", c("Estimate", "t value")]
table_6.3[c(3,4),2] = summary(mdl.ez_4)[["coefficients"]]["e_z1", c("Estimate", "t value")]
table_6.3[c(5,6),1] = summary(mdl.ew_4)[["coefficients"]]["e_w1", c("Estimate", "t value")]
table_6.3[c(5,6),2] = summary(mdl.ew_4)[["coefficients"]]["e_w1", c("Estimate", "t value")]

prmatrix(table_6.3, rowlab=c("De_yt", "", "De_zt", "", "De_wt", ""), collab=c("No Lags", "4 Lags"),
             quote=FALSE, right=FALSE)



# Estimating the error-correction model.

# Construc diff variables
table <- table %>% 
  mutate(Dy = y - lag(y),
         Dz = z - lag(z),
         Dw = w - lag(w))


ecm.y <- lm(Dy ~ 1 + lag(e_wt) + lag(Dy) + lag(Dz) + lag(Dw), data = table)
ecm.z <- lm(Dz ~ 1 + lag(e_wt) + lag(Dy) + lag(Dz) + lag(Dw), data = table)
ecm.w <- lm(Dw ~ 1 + lag(e_wt) + lag(Dy) + lag(Dz) + lag(Dw), data = table)

summary(ecm.y)
summary(ecm.z)
summary(ecm.w)












