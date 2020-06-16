# clear variables
rm(list = ls())

# Author: Bruno Tebaldi
# 2020-06-16
#
# Impulse Response Function
# 
# This notebook goes through simulation of structural VAR models (chalecki decomposition)
# and their Impulse Response Functions.
# It also goes through two different ways of analytically calculate the IRF from parametres of the sVAR models.
# It replicates the results of Enders p. 297


# load libraries
library(dplyr)
library(ggplot2)
library(latex2exp)


# --- Definiton of matrix coeficients ----

# X = [y_t, z_t]
# Xt = A + B1 Xt-1 + S e
A = matrix(0, nrow = 2, ncol = 1)
B1 = matrix(c(0.7,0.2,0.2,0.7), nrow = 2, ncol = 2)
S =  matrix(c(1,0,0.8,1), nrow = 2, ncol = 2)


# --- Simulation of VAR ----
# number of points to be simulated
nsim <- 200
sim <- matrix(0, ncol = 2, nrow = nsim)
colnames(sim) <- c("yt", "zt")

set.seed(12345)

# Simulation of the VAR
for (i in 2:nsim) {
  e = matrix(rnorm(2), ncol = 1, nrow = 2)
  sim[i,] = A + B1 %*% sim[i-1,] + S %*% e
}

# show the head of the data
head(sim)

# transform the data to be used in ggplot
table = as_tibble(sim)
table$Id = 1:nsim

#  plot the series
ggplot(table) +
  geom_line(aes(x=Id, y=yt), color="blue") +
  geom_line(aes(x=Id, y=zt), color="red") +
  labs(title = "VAR(1) with 2 variables",
       y=NULL,
       x=NULL)

# remove unused variables
rm(list = c("table", "e", "i"))

# --- Function to plot IRF ----

# Matrix power using tail recursion
matrix.power = function(n, mat, mat_p){
  ret=NaN
  
  if(n==0){
    ret = diag(1, nrow(mat))
  } else if(n==1){
    ret = mat_p
  } else {
    ret = matrix.power(n-1, mat, mat%*%mat_p)
  }
  return(ret)
}

# Total of periods to analize the shock1s
n = 20

# matrix with the effects of the shock1
shock1 <- tibble(Id=0, y_t_1=0, z_t_1=0, y_t_2=0, z_t_2=0, .rows = n)

for(i in 1:nrow(shock1)) {
  
  # effects of the shock1
  effect = matrix.power(i-1, B1, B1) %*% S
  
  # store the calculated effects
  shock1[i,"Id"] = i-1
  shock1[i,"y_t_1"] = effect[1,1]
  shock1[i,"z_t_1"] = effect[2,1]
  shock1[i,"y_t_2"] = effect[1,2]
  shock1[i,"z_t_2"] = effect[2,2]
}

# plot the series  
ggplot(shock1) + 
  geom_line(aes(x=Id, y=y_t_1), color="blue") +
  # geom_point(aes(x=Id, y=y_t_1), color="blue") +
  geom_line(aes(x=Id, y=z_t_1), color="red") +
  labs(title = latex2exp::TeX("Response to $\\epsilon_{yt}$ shock1"),
       y=NULL,
       x=NULL,
       caption = "Legend: Blue line = {yt} sequence Red Line = {zt} sequence"
  )


ggplot(shock1) + 
  geom_line(aes(x=Id, y=y_t_2), color="blue") +
  geom_line(aes(x=Id, y=z_t_2), color="red") +
  labs(title = latex2exp::TeX("Response to $\\epsilon_{zt}$ shock1"),
       y=NULL,
       x=NULL,
       caption = "Legend: Blue line = {yt} sequence Red Line = {zt} sequence"
  )

# remove unused variables
rm(list = c("i", "effect"))


# define a matrix B2 for the second part of the panel
B2 = matrix(c(0.7,-0.2,-0.2,0.7), nrow = 2, ncol = 2)

# matrix with the effects of the shock1
shock2 <- tibble(Id=0, y_t_1=0, z_t_1=0, y_t_2=0, z_t_2=0, .rows = n)

for(i in 1:nrow(shock2)) {
  
  # effects of the shock1
  effect = matrix.power(i-1, B2, B2) %*% S
  
  # store the calculated effects
  shock2[i,"Id"] = i-1
  shock2[i,"y_t_1"] = effect[1,1]
  shock2[i,"z_t_1"] = effect[2,1]
  shock2[i,"y_t_2"] = effect[1,2]
  shock2[i,"z_t_2"] = effect[2,2]
}


# plot the series  
ggplot(shock2) + 
  geom_line(aes(x=Id, y=y_t_1), color="blue") +
  # geom_point(aes(x=Id, y=y_t_1), color="blue") +
  geom_line(aes(x=Id, y=z_t_1), color="red") +
  labs(title = latex2exp::TeX("Response to $\\epsilon_{yt}$ shock1"),
       y=NULL,
       x=NULL,
       caption = "Legend: Blue line = {yt} sequence Red Line = {zt} sequence"
  )


ggplot(shock2) + 
  geom_line(aes(x=Id, y=y_t_2), color="blue") +
  geom_line(aes(x=Id, y=z_t_2), color="red") +
  labs(title = latex2exp::TeX("Response to $\\epsilon_{zt}$ shock1"),
       y=NULL,
       x=NULL,
       caption = "Legend: Blue line = {yt} sequence Red Line = {zt} sequence"
  )

# remove unused variables
rm(list = c("i", "effect"))




