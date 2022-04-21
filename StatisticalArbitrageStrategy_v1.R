
rm(list=ls())
library(dplyr)
nacols <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) any(is.na(x))))]
}

data.without.na <- function(df) {
  na.columns <- nacols(df)
  df[, which(!(names(df) %in% na.columns)), drop=FALSE]
}

load_data <- function(filename, index.name="SP500"){
  data <- read.csv(filename)
  rownames(data) <- data$Date
  if (index.name != "") {
    index.data <- data[, which(names(data) %in% c(index.name))]
    data <- data[, which(!(names(data) %in% c('Date', index.name)))]
    list(index=index.data, components=data)
  } else {
    data <- data[, which(!(names(data) %in% c('Date', index.name)))]
    list(components=data)
  }
}

filter_data_by_date <- function(df, start, end) {
  df[which(rownames(df) >= start & rownames(df) < end), ]
}


Strat.PCA.by.variance <- function(Variance, data) {
  data <- data.without.na(data) #somethings need to fixed
  avg <- apply(data, 2, mean)
  std <- apply(data, 2, sd)
  scale.data <-  sweep(sweep(data, 2, avg, '-'), 2, std, '/') #same as scale(data)
  pca <- prcomp(scale.data)  # PCA
  vars <- pca$sdev^2
  props <- vars / sum(vars)  # Var propotion of each PC
  cumprops <- cumsum(props)  # Cum Var propotion
  if (length(which(cumprops< Variance)) == 0) {
    k <- 1
  } else {
    k <- max(which(cumprops< Variance)) + 1
  }
  pcs=pca$rotation[,1:k]
  weight <- pcs/std /(sum(pcs/std ))
  #return(list(PCw = w , K = max(k,5)))  # pick PCs makes %Variance variance
  return(list(pcs=as.data.frame(pcs), k=k, weight=as.data.frame(weight)))
}

find_factors_and_ret <- function(variance, data, analysis.period=252) {
  analysis.df <- data[1:analysis.period, ]
  PCAE <- Strat.PCA.by.variance(variance, analysis.df)
  factor.weight <- PCAE$weight
  factor.return <-as.matrix(data) %*% as.matrix(factor.weight)
  return (list(factor.weight=factor.weight, factor.return=factor.return))
}


Regression <- function(stock.returns, factor.returns) { # regression target port. aginst Picked PCs previously
  
  coefficients.array = list()
  residuals.array = list()
  for (i in 1:length(stock.returns[1,])) {
    stock.code <- names(stock.returns)[i]
    reg <- lm(stock.returns[,i]~factor.returns)
    coefficients.array[[stock.code]] <- reg$coefficients
    residuals.array[[stock.code]] <- reg$residuals
  }
  c <- data.frame(coefficients.array)
  r <- data.frame(residuals.array)
  list(residual=r, coefs=c)
}

TargetSScore <- function(residuals, k, verbose=F) {
  m.array <- list()
  sigma.eq.array <- list()
  Xts <- cumsum(residuals)
  
  for (i in 1:length(residuals[1,])) {
    code <- names(residuals)[i]
    tryCatch(
      {
        #res <- arima(Xts[,i], order= c(1,0,0), include.mean = FALSE, method ="ML")
        res <- arima(Xts[,i], order= c(1,0,0), include.mean = T, method ="ML")
        b <- res$coef[1]
        if (-log(b)*252 > k) {
          #temp <- (Xts[,i] - lag(Xts[,i]) *b)[2:length(Xts[,i])]
          #a <- mean(temp)
          a <- res$coef[2]
          cosi <- temp - a
          m.array [code] <- a/(1-b)
          sigma.eq.array[code] <- sqrt(var(cosi)/(1-b^2))
        }
      },
      error= function(e) {
        if (verbose) {
          print(paste("code: ", code, "~error~", e))
        }
      }
    )
  }
  m <- data.frame(m.array)
  sigma.eq <- data.frame(sigma.eq.array)
  m.bar <- apply(m, 1, mean)
  m <- m - m.bar
  s_score <- -m/sigma.eq
  s_score
}

#full_data <- load_data("full_data_pct_2000.csv", index.name="SP500")
full_data <- load_data("full_data_pct_2000_v1.csv", index.name="")
data <- full_data$components

backtest <- filter_data_by_date(data, '2006-01-01', '2013-12-31')
backtest.clean <- data.without.na(backtest)
factor.info <- find_factors_and_ret(0.55, backtest.clean, analysis.period = 252)
factor.weight <- factor.info$factor.weight
factor.return <- factor.info$factor.return

sbo <- 2
sso <- 2
sbc <- 0.75
ssc <- 0.5

#reg.result <- Regression(backtest.clean[(t-60+1):t, ], factor.return[(t-60+1):t, ])
#a <- arima(cumsum(reg.result$residual[,1]), order= c(1,0,0), include.mean = T, method ="ML")
#a$coef

for (t in 252:400) {
  reg.result <- Regression(backtest.clean[(t-60+1):t, ], factor.return[(t-60+1):t, ])
  residuals <- reg.result$residual
  s.scores <- TargetSScore(residuals, 8.4)
  print(paste(t, "~~~~", sum(apply(s.scores, 1, function(x) {x > sso})), sum(apply(s.scores, 1, function(x) {x < -sso}))))
}

S.SCORES



