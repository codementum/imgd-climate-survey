library(ggplot2)
library(dplyr)
library(coin)
library(pwr)
library(shiny)
library(miniUI)
library(boot)
library(tidyr)
library(irr)

# Reporting
report <- function(data, attr) {
  cat("M=",   round( mean( data[[attr]] )  , 1), ",",
      "sd=",  round( sd( data[[attr]] )    , 1), ",",
      "Mdn=", round( median( data[[attr]] ), 1), ",",
      "mad=", round( mad( data[[attr]] )   , 1),
      sep="")
}

# Filtering

madfilter <- function(data, attr, fac) {
  mad <- mad( data[[attr]] )
  median <- median( data[[attr]] )
  data <-
    data[ data[[attr]] < median + fac*mad &
            data[[attr]] > median - fac*mad, ]
}

rangefilter <- function(data, attr, lo, hi) {
  data <-
    data[ data[[attr]] < hi &
            data[[attr]] > lo, ]
}

# Bootstrap 95% CI for mean
# function to obtain mean from the data (with indexing)
mean.fun <- function(D, d) {
  return( mean(D[d]) )
}

ciplot2 <- function(V, range) {
  # bootstrapping with 1000 replications
  fadeci <- boot.ci(
    boot(data=fade[[V]], statistic=mean.fun, R=1000, sim="ordinary")
  )

  nofadeci <- boot.ci(
    boot(data=nofade[[V]], statistic=mean.fun, R=1000, sim="ordinary")
  )

  mean1 <- mean(fade[[V]])
  mean2 <- mean(nofade[[V]])

  df <- data.frame(
    trt = factor(c('fade', 'nofade')),
    resp = c(mean1, mean2),
    group = factor(c('fade', 'nofade')),
    upper = c(fadeci$bca[,5], nofadeci$bca[,5]),
    lower = c(fadeci$bca[,4], nofadeci$bca[,4])
  )

  p <- ggplot(df, aes(trt, resp, colour = group))
  p <- p + scale_color_manual(values=c("#F1A340", "#998EC3"))
  p <- p + theme(axis.title=element_text(size=20), axis.text=element_text(size=18))
  p <- p + geom_pointrange(aes(ymin = lower, ymax = upper))
  p <- p + expand_limits(y = range)
  p <- p + ylab(V)
  p <- p + xlab("")
  p <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1)
  p <- p + coord_flip()
  p <- p + theme_bw()
  p <- p + theme(plot.title=element_text(hjust=0))
  p <- p + theme(panel.border=element_blank())
  p <- p + theme(panel.grid.minor=element_blank())
  p <- p + theme(axis.ticks=element_blank())
  p <- p + theme(legend.key=element_rect(color="white"))
  p <- p + theme(text=element_text(family="Avenir Next Medium"))
  p <- p + theme(axis.text.y = element_blank())
  p <- p + guides(colour=FALSE)
  p
}

ciplot <- function(V) {
  # bootstrapping with 1000 replications
  fadeci <- boot.ci(
    boot(data=fade[[V]], statistic=mean.fun, R=1000, sim="ordinary")
  )

  nofadeci <- boot.ci(
    boot(data=nofade[[V]], statistic=mean.fun, R=1000, sim="ordinary")
  )

  mean1 <- mean(fade[[V]])
  mean2 <- mean(nofade[[V]])

  df <- data.frame(
    trt = factor(c('fade', 'nofade')),
    resp = c(mean1, mean2),
    group = factor(c('fade', 'nofade')),
    upper = c(fadeci$bca[,5], nofadeci$bca[,5]),
    lower = c(fadeci$bca[,4], nofadeci$bca[,4])
  )

  p <- ggplot(df, aes(trt, resp, colour = group))
  p <- p + theme(axis.title=element_text(size=20), axis.text=element_text(size=18))
  p <- p + geom_pointrange(aes(ymin = lower, ymax = upper))
  p <- p + expand_limits(y = 0)
  p <- p + ylab(V)
  p <- p + xlab("")
  p <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1)
  p <- p + coord_flip()
  p
}