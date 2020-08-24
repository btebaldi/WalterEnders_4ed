# Clear all
rm(list = ls())

# libraries
library(readr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(vars)
library(MTS)
library(urca)
library(tsDyn)

# Load Databse
table <- read_csv("Matlab examples/table.txt", 
                  col_types = cols(Time = col_date(format = "%d-%b-%Y")))
head(table)

g1 <- table %>% 
ggplot() +
  geom_line(aes(x=Time, y=GDP)) + 
  labs(title = "Gross Domestic Product",
       x="Index",
       y="Date")

g2 <- table %>% 
  ggplot() +
  geom_line(aes(x=Time, y=GDPDEF)) + 
  labs(title = "GDP Deflator",
       x="Index",
       y="Date")


g3 <- table %>% 
  ggplot() +
  geom_line(aes(x=Time, y=COE)) + 
  labs(title = "Paid Compensation of Employees",
       x="Billions of $",
       y="Date")


g4 <- table %>% 
  ggplot() +
  geom_line(aes(x=Time, y=HOANBS)) + 
  labs(title = "Nonfarm Business Sector Hours",
       x="Index",
       y="Date")

cowplot::plot_grid(g1, g2, g3, g4)

g5 <- table %>% 
  ggplot() +
  geom_line(aes(x=Time, y=FEDFUNDS)) + 
  labs(title = "Federal Funds Rate",
       x="Percent",
       y="Date")

g6 <- table %>% 
  ggplot() +
  geom_line(aes(x=Time, y=PCEC)) + 
  labs(title = "Consumption Expenditures",
       x="Billions of $",
       y="Date")


g7 <- table %>% 
  ggplot() +
  geom_line(aes(x=Time, y=GPDI)) + 
  labs(title = "Gross Private Domestic Investment",
       x="Billions of $",
       y="Date")



cowplot::plot_grid(g5, g6, g7)


# Stabilize all series, except the federal funds rate, by applying the log transform.
# Scale the resulting series by 100 so that all series are on the same scale.
table$ln_GDP = 100*log(table$GDP);
table$ln_GDPDEF = 100*log(table$GDPDEF);
table$ln_COE = 100*log(table$COE);
table$ln_HOANBS = 100*log(table$HOANBS);
table$ln_PCEC = 100*log(table$PCEC);
table$ln_GPDI = 100*log(table$GPDI);


# Create a VEC(1) model using the shorthand syntax. Specify the variable names.
y.mat <- table %>% dplyr::select(starts_with("ln"), FEDFUNDS)
vecm.mdl <- urca::ca.jo(y.mat, type="trace", ecdet="trend", K=2)
vecm.mdl
summary(vecm.mdl)

vecm.mdl@PI

vecm.estimate <- urca::cajorls(vecm.mdl, r = 4)
sprintf("%f", vecm.estimate$rlm$coefficients["constant",])


mdl2 <- tsDyn::VECM(y.mat, lag=1, r = 4, include = "const",
     beta = NULL, estim = "ML", LRinclude = "none", exogen = NULL)

mdl2
summary(mdl2)

MTS::ECMvar(x=y.mat, p=2, include.const = TRUE, ibeta = 4)









phi1=matrix(c(.2,-.6,.3,1.1),2,2) # Input phi_1
phi1

sig=matrix(c(1,0.8,0.8,2),2,2) # Input sigma_a
 sig

 m1=eigen(phi1) # Obtain eigenvalues & vectors
m1
$values
$vectors

I4=diag(4) ## Create the 4-by-4 identity matrix
pp=kronecker(phi1,phi1) # Kronecker product
pp

c1=c(sig)
c1

dd=I4-pp
ddinv=solve(dd) ## Obtain inverse
gam0=ddinv%*%matrix(c1,4,1) # Obtain Gamma_0
gam0

g0=matrix(gam0,2,2)
g1=phi1%*%g0 ## Obtain Gamma_1
g1

g2=phi1%*%g1
g2

D=diag(sqrt(diag(g0))) # To compute cross-correlation matrices
D
Di=solve(D)
Di%*%g0%*%Di
Di%*%g1%*%Di
Di%*%g2%*%Di

MTS::ec
