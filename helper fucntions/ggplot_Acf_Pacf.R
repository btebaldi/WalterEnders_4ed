ggplot_Acf_Pacf <- function(x, lag.max = NULL){
  require(tibble)
  require(ggplot2)
  
  conf.level <- 0.95
  n = length(x)
  ciline <- qnorm((1 - conf.level)/2)/sqrt(n)
  
  acf_1 = acf(x, plot = F, lag.max = lag.max)
  pacf_1 = pacf(x, plot = F, lag.max = lag.max)
  
  acf_1_df =  tibble(lag = as.vector(acf_1$lag), ACF = as.vector(acf_1$acf)) 
  pacf_1_df =  tibble(lag = as.vector(pacf_1$lag), PACF = as.vector(pacf_1$acf)) 
  
  p1 <- ggplot(data=acf_1_df, mapping=aes(x=lag, y=ACF)) +
    geom_bar(stat = "identity", position = "identity", fill="lightsteelblue") + 
    geom_hline(yintercept = ciline, colour="red", size = 0.8, linetype="dashed") + 
    geom_hline(yintercept = -ciline, colour="red", size = 0.8, linetype="dashed") 
  
  p2 <- ggplot(data=pacf_1_df, mapping=aes(x=lag, y=PACF)) +
    geom_bar(stat = "identity", position = "identity", fill="lightsteelblue") + 
    geom_hline(yintercept = ciline, colour="red", size = 0.8, linetype="dashed") + 
    geom_hline(yintercept = -ciline, colour="red", size = 0.8, linetype="dashed")
  
  ret = list(ACF = p1, PACF = p2)
  
  return(ret)
}

# Cross- Covariance and -Correlation
ggplot_Ccf <- function(x, y, lag.max = NULL, type = c("correlation", "covariance"),
                       na.action = na.fail, conf.level = 0.95,...){
  require(tibble)
  require(ggplot2)
  
  type <- match.arg(type)
  
  if (is.matrix(x) || is.matrix(y))  {
    stop("univariate time series only")
  }
  
  conf.level <- conf.level
  n = length(x)
  ciline <- qnorm((1 - conf.level)/2)/sqrt(n)
  
  X <- ts.intersect(as.ts(x), as.ts(y))
  
  colnames(X) <- c(deparse(substitute(x))[1L], deparse(substitute(y))[1L])
  
  acf.out <- acf(X, lag.max = 20, plot = FALSE, type = type, na.action = na.action)
  
  lag <- c(rev(acf.out$lag[-1, 2, 1]), acf.out$lag[, 1, 2])
  
  y <- c(rev(acf.out$acf[-1, 2, 1]), acf.out$acf[, 1, 2])
  
  
  # acf.out$acf <- array(y, dim = c(length(y), 1L, 1L))
  # acf.out$lag <- array(lag, dim = c(length(y), 1L, 1L))
  # acf.out$snames <- paste(acf.out$snames, collapse = " & ")
  
  acf.out2 <- tibble(lag = array(lag, dim = c(length(y), 1L, 1L)),
                     value = array(y, dim = c(length(y), 1L, 1L))) 
  
  p1 <- ggplot(data=acf.out2, mapping=aes(x=lag, y=value)) +
    geom_bar(stat = "identity", position = "identity", fill="lightsteelblue") + 
    geom_hline(yintercept = ciline, colour="red", size = 0.5, linetype="dashed") + 
    geom_hline(yintercept = -ciline, colour="red", size = 0.5, linetype="dashed") 
  
  ret <- list(plot = p1, Data = acf.out2)

  return(invisible(ret))
}