# clear variables
rm(list = ls())

# Author: Bruno Tebaldi
# 2020-06-11
#
# Cross Correlation Function CCF¶
# The plots are of the Cross Covariance Function CCVF (Enders page 271)

# load libraries
library(dplyr)
library(ggplot2)
library(latex2exp)
library(cowplot)

source(file = "./helper fucntions/ggplot_Acf_Pacf.R")

# par(mfrow=c(1,4))

# Set the number of points
nsim <- 10000

# initialize the series y_t and z_t
yt <- rep(0,nsim)
zt <- rnorm(nsim)


for (i in 5:nsim) {
  yt[i] <- 0.8*yt[i-1] + zt[i-3] + 1.5*zt[i-4]+ rnorm(1)
}

g1 <- ggplot_Ccf(yt, zt, lag.max=20, type="correlation")

g1$plot <- g1$plot +
  xlim(0, 20) +
  labs(
    title = "Panel (a)",
    subtitle = latex2exp::TeX("$Y_t = 0.8 Y_{t-1} + Z_{t-3} + 1.5 Z_{t-4} + \\epsilon_{t}$"),
    caption = "FIGURE 5.4 Standardized Cross-Correlograms",
    x = NULL,
    y = NULL
  )

for (i in 5:nsim) {
  yt[i] <- 0.8*yt[i-1] + zt[i-3] - 1.5*zt[i-4]+ rnorm(1)
}

g2 <- ggplot_Ccf(yt, zt, lag.max=20, type="correlation")

g2$plot <- g2$plot +
  xlim(0, 20) +
  labs(
    title = "Panel (b)",
    subtitle = latex2exp::TeX("$Y_t = 0.8 Y_{t-1} - Z_{t-3} + 1.5 Z_{t-4} + \\epsilon_{t}$"),
    caption = "FIGURE 5.4 Standardized Cross-Correlograms",
    x = NULL,
    y = NULL
  )


for (i in 5:nsim) {
  yt[i] <- 0.8*yt[i-1] - 0.6*yt[i-2] + zt[i-3]+ rnorm(1)}

g3 <- ggplot_Ccf(yt, zt, lag.max=20, type="correlation")

g3$plot <- g3$plot +
  xlim(0, 20) +
  labs(
    title = "Panel (c)",
    subtitle = latex2exp::TeX("$Y_t = 0.8 Y_{t-1} - Y_{t-2} + Z_{t-3} + \\epsilon_{t}$"),
    caption = "FIGURE 5.4 Standardized Cross-Correlograms",
    x = NULL,
    y = NULL
  )


for (i in 5:nsim) {
  yt[i] <- 1.4*yt[i-1] - 0.6*yt[i-2] + zt[i-3]+ rnorm(1)
}

g4 <- ggplot_Ccf(yt, zt, lag.max=20, type="correlation")

g4$plot <- g4$plot +
  xlim(0, 20) +
  labs(
    title = "Panel (d)",
    subtitle = latex2exp::TeX("$Y_t = 1.4 Y_{t-1} - 0.6 Y_{t-2} + Z_{t-3} + \\epsilon_{t}$"),
    caption = "FIGURE 5.4 Standardized Cross-Correlograms",
    x = NULL,
    y = NULL
  )

cowplot::plot_grid(g1$plot, g2$plot, g3$plot, g4$plot)

  
 