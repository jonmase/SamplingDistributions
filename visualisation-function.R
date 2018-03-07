# Sampling distribution functions
# Authors: Tom Fanshawe and Jason Oke, Nuffield Department of Primary Care Health Sciences, University of Oxford
# License: GPL v3, https://www.gnu.org/licenses/gpl.html
#
#Allow the following arguments to change according to user input:
#distribution: "normal" or "skewed"
#N: number of data-points in each sample
#reps: number of samples
#alpha: for the skewed distribution; numbers with higher magnitude indicate greater skew; negative numbers indicate negative skew
#what: plot a histogram of either the sample "mean"s, sample "median"s, sample "sd"s, sample "var"iances, or sample "range"s
#plot.all: plot each sample together (TRUE) or one at a time (FALSE)?

#Separate short functions for plotting sample mean, median, sd, variance, range
mean_f <- function(x, what, plot.all, N, reps, i = 1) {
  h <-
    hist(
      rowMeans(x),
      breaks = seq(-3, 3, 0.05),
      main = "",
      xlab = "",
      xlim = c(-3, 3),
      cex.axis = 1.5,
      cex.lab = 1.5
    )
  b <- h$breaks
  c <- h$counts
  text(
    paste("average of sample means = ", round(mean(rowMeans(
      x
    )), 3)),
    x = -3,
    y = max(c) * 0.95,
    pos = 4,
    cex=1.5
  )
  text(
    paste("sd of sample means = ", round(sd(rowMeans(
      x
    )), 3)),
    x = -3,
    y = max(c) * 0.85,
    pos = 4,
    cex=1.5
  )
  if (plot.all) {
    text(paste("N =", N),
         x = -3,
         y = max(c) * 0.75,
         pos = 4,
         cex=1.5)
    text(
      paste("reps =", reps),
      x = -3,
      y = max(c) * 0.65,
      pos = 4,
      cex=1.5
    )
    title(paste0("Sample ", what, "s"),
          cex.main=2)
  } else
    (title(paste0("First ", i, " sample ", what, "s"),
           cex.main=2))
}

median_f <- function(x, what, plot.all, N, reps, i = 1) {
  h <- hist(
    apply(x, 1, median),
    breaks = seq(-3, 3, 0.05),
    main = "",
    xlab = "",
    cex.axis = 1.5,
    cex.lab = 1.5
  )
  b <- h$breaks
  c <- h$counts
  text(
    paste("average of sample medians =", round(mean(
      apply(x, 1, median)
    ), 3)),
    x = -3,
    y = max(c) * 0.95,
    pos = 4,
    cex=1.5
  )
  text(
    paste("sd of sample medians =", round(sd(
      apply(x, 1, median)
    ), 3)),
    x = -3,
    y = max(c) * 0.85,
    pos = 4,
    cex=1.5
  )
  if (plot.all) {
    text(paste("N =", N),
         x = -3,
         y = max(c) * 0.75,
         pos = 4,
         cex=1.5)
    text(
      paste("reps =", reps),
      x = -3,
      y = max(c) * 0.65,
      pos = 4,
      cex=1.5
    )
    title(paste0("Sample ", what, "s"),
          cex.main=2)
  } else
    (title(paste0("First ", i, " sample ", what, "s"),
           cex.main=2))
}

sd_f <- function(x, what, plot.all, N, reps, i = 1) {
  h <-
    hist(
      apply(x, 1, sd),
      breaks = seq(-1, 3, 0.05),
      main = "",
      xlab = "",
      xaxt = "n",
      cex.axis = 1.5,
      cex.lab = 1.5
    )
  b <- h$breaks
  c <- h$counts
  axis(1, at = 0:3)
  text(
    paste("average of sample sd's =", round(mean(apply(
      x, 1, sd
    )), 3)),
    x = -1,
    y = max(c) * 0.95,
    pos = 4,
    cex=1.5
  )
  text(
    paste("sd of sample sd's =", round(sd(apply(
      x, 1, sd
    )), 3)),
    x = -1,
    y = max(c) * 0.85,
    pos = 4,
    cex=1.5
  )
  if (plot.all) {
    text(paste("N =", N),
         x = -1,
         y = max(c) * 0.75,
         pos = 4,
         cex=1.5)
    text(
      paste("reps =", reps),
      x = -1,
      y = max(c) * 0.65,
      pos = 4,
      cex=1.5
    )
    title(paste0("Sample ", what, "s"),
          cex.main=2)
  } else
    (title(paste0("First ", i, " sample ", what, "s"),
           cex.main=2))
}

var_f <- function(x, what, plot.all, N, reps, i = 1) {
  h <-
    hist(
      apply(x, 1, var),
      breaks = seq(-1, 3, 0.05),
      main = "",
      xlab = "",
      xaxt = "n",
      cex.axis = 1.5,
      cex.lab = 1.5
    )
  b <- h$breaks
  c <- h$counts
  axis(1, at = 0:3)
  text(
    paste("average of sample variances =", round(mean(apply(
      x, 1, var
    )), 3)),
    x = -1,
    y = max(c) * 0.95,
    pos = 4,
    cex=1.5
  )
  text(
    paste("sd of sample variances =", round(sd(apply(
      x, 1, var
    )), 3)),
    x = -1,
    y = max(c) * 0.85,
    pos = 4,
    cex=1.5
  )
  if (plot.all) {
    text(paste("N =", N),
         x = -1,
         y = max(c) * 0.75,
         pos = 4,
         cex=1.5)
    text(
      paste("reps =", reps),
      x = -1,
      y = max(c) * 0.65,
      pos = 4,
      cex=1.5
    )
    title(paste0("Sample ", what, "s"),
          cex.main=2)
  } else
    (title(paste0("First ", i, " sample ", what, "s"),
           cex.main=2))
}

range_f <- function(x, what, plot.all, N, reps, i = 1) {
  r <- apply(x, 1, function(y)
    max(y) - min(y))
  h <- hist(
    r,
    main = "",
    xlab = "",
    breaks = seq(min(r) - 1, max(r) + 0.5, 0.05),
    cex.axis = 1.5,
    cex.lab = 1.5
  )
  b <- h$breaks
  c <- h$counts
  text(
    paste("average of sample ranges =", round(mean(
      apply(x, 1, max) - apply(x, 1, min)
    ), 3)),
    x = min(b),
    y = max(c) * 0.95,
    pos = 4,
    cex=1.5
  )
  text(
    paste("sd of sample ranges =", round(sd(
      apply(x, 1, max) - apply(x, 1, min)
    ), 3)),
    x = min(b),
    y = max(c) * 0.85,
    pos = 4,
    cex=1.5
  )
  if (plot.all) {
    text(
      paste("N =", N),
      x = min(b),
      y = max(c) * 0.75,
      pos = 4,
      cex=1.5
    )
    text(
      paste("reps =", reps),
      x = min(b),
      y = max(c) * 0.65,
      pos = 4,
      cex=1.5
    )
    title(paste0("Sample ", what, "s"),
          cex.main=2)
  } else
    (title(paste0("First ", i, " sample ", what, "s"),
           cex.main=2))
}

#Function for plotting data from a single sample
single_samp <- function(x) {
  h <-
    hist(
      x,
      breaks = seq(-100, 100, by = 0.1),
      main = "Data from single sample",
      xlab = "",
      xlim = c(-3, 3),
      cex.main = 2,
      cex.axis = 1.5,
      cex.lab = 1.5
    )
  b <- h$breaks
  c <- h$counts
  text(
    paste("sample mean = ", round(mean(x), 3)),
    x = -3,
    y = max(c) * 0.95,
    pos = 4,
    cex=1.5
  )
  text(
    paste("sample sd = ", round(sd(x), 3)),
    x = -3,
    y = max(c) * 0.85,
    pos = 4,
    cex=1.5
  )
}

#Main plotting function: f
f <-
  function(distribution = "normal",
           alpha = 5,
           N = 5,
           reps = 1000,
           what = "mean",
           plot.all = TRUE) {
    #Normal(0,1) distribution
    if (distribution == "normal") {
      x <- matrix(NA, reps, N)
      for (i in 1:reps) {
        x[i, ] <- rnorm(N, 0, 1)
      }
      
      if (plot.all) {
        par(mfrow = c(3, 1))
        curve(
          dnorm,
          -3,
          3,
          yaxt = "n",
          xlab = "",
          ylab = "",
          main = "Population",
          cex.main = 2,
          cex.axis = 1.5
        )
        single_samp(x = x[reps, ])
        switch(
          what,
          mean = mean_f(x, what, plot.all, N, reps),
          median = median_f(x, what, plot.all, N, reps),
          sd = sd_f(x, what, plot.all, N, reps),
          var = var_f(x, what, plot.all, N, reps),
          range = range_f(x, what, plot.all, N, reps)
        )
      }
      
      if (!(plot.all)) {
        for (i in 1:reps) {
          par(mfrow = c(3, 1))
          curve(
            dnorm,
            -3,
            3,
            yaxt = "n",
            xlab = "",
            ylab = "",
            main = "Population",
            cex.main = 2
          )
          single_samp(x = x[i, ])
          switch(
            what,
            mean = mean_f(matrix(x[1:i, ], i, N), what, plot.all, N, reps, i),
            median = median_f(matrix(x[1:i, ], i, N), what, plot.all, N, reps, i),
            sd = sd_f(matrix(x[1:i, ], i, N), what, plot.all, N, reps, i),
            var = var_f(matrix(x[1:i, ], i, N), what, plot.all, N, reps, i),
            range = range_f(matrix(x[1:i, ], i, N), what, plot.all, N, reps, i)
          )
          readline()
        }
      }
    }
    
    #Skewed distribution: alpha*sqrt(2)/sqrt(pi*(1+alpha^2)) is subtracted to centre to mean zero
    if (distribution == "skewed") {
      x <- matrix(NA, reps, N)
      for (i in 1:reps) {
        x[i, ] <- rsn(N, alpha = alpha) - alpha * sqrt(2) / sqrt(pi * (1 + alpha ^
                                                                         2))
      }
      
      if (plot.all) {
        par(mfrow = c(3, 1))
        curve(
          dsn(x + alpha * sqrt(2) / sqrt(pi * (1 + alpha ^ 2)), alpha = alpha),
          -3,
          3,
          yaxt = "n",
          xlab = "",
          ylab = "",
          main = "Population",
          cex.main = 2,
          cex.axis = 1.5
        )
        single_samp(x = x[reps, ])
        switch(
          what,
          mean = mean_f(x, what, plot.all, N, reps),
          median = median_f(x, what, plot.all, N, reps),
          sd = sd_f(x, what, plot.all, N, reps),
          var = var_f(x, what, plot.all, N, reps),
          range = range_f(x, what, plot.all, N, reps)
        )
      }
      
      if (!(plot.all)) {
        for (i in 1:reps) {
          par(mfrow = c(3, 1))
          curve(
            dsn(x + alpha * sqrt(2) / sqrt(pi * (1 + alpha ^ 2)), alpha = alpha),
            -3,
            3,
            yaxt = "n",
            xlab = "",
            ylab = "",
            main = "Population",
            cex.main = 2,
            cex.axis = 1.5
          )
          single_samp(x = x[i, ])
          switch(
            what,
            mean = mean_f(matrix(x[1:i, ], i, N), what, plot.all, N, reps, i),
            median = median_f(matrix(x[1:i, ], i, N), what, plot.all, N, reps, i),
            sd = sd_f(matrix(x[1:i, ], i, N), what, plot.all, N, reps, i),
            var = var_f(matrix(x[1:i, ], i, N), what, plot.all, N, reps, i),
            range = range_f(matrix(x[1:i, ], i, N), what, plot.all, N, reps, i)
          )
          readline()
        }
      }
    }
  }

