# Script created by Frank D'Agostino 2020
# Code was based on work done by Dr. Caroline Buckee and colleagues
# Found here: http://rstudio-pubs-static.s3.amazonaws.com/6852_c59c5a2e8ea3456abbeb017185de603e.html

rm(list = ls())
# Clear console
library(deSolve)
library(data.table)

#-------------------------------------------------------------------------------
# Data Cleanup
#-------------------------------------------------------------------------------

SIR <- function(parameters) {
  
  # Let's see the inputted parameters
  print(parameters)
  
  # Create the time and initialization parameters
  time <- seq(from = 0, to = 91, by = 1)
  init <- c(S = 0.9999999999, I = 0.00000000001, R = 0)
  
  diff_eqs <- function(time, init, parameters) {
    with(as.list(c(init, parameters)), {
      
      dS <- birth - beta*I*S - death*S
      dI <- beta*I*S - recovery*I - death*I
      dR <- recovery*I - death*R
      
      return(list(c(dS, dI, dR)))
      
    })
  }
  
  res <- ode(func = diff_eqs, times = time, y = init, parms = parameters)
  res <- as.data.frame(res)
  
  # Return our results
  return(res)
}

disp_param <- c(beta = 2.9, recovery = 2.55, death = 0.001, birth = 0.001)
out <- SIR(disp_param)

matplot(out[, 1], out[, 3:3]*popNY, type="l", lty=1)
legend("topright", col=1:2, legend=c("I", "R"), lwd=1)

# Populations of the 5 cities from Google search (https://worldpopulationreview.com/states/)
popNY <- 19491339
popWA <- 7797100
popCA <- 39747267
popNJ <- 8936570
popMA <- 6979600

#-------------------------------------------------------------------------------
# New York Data
#-------------------------------------------------------------------------------

# Make dates array
dates = c('3/2/20', '3/3/20', '3/4/20', '3/5/20', '3/6/20',
          '3/7/20', '3/8/20', '3/9/20', '3/10/20', '3/11/20',
          '3/12/20', '3/13/20', '3/14/20', '3/15/20', '3/16/20',
          '3/17/20', '3/18/20', '3/19/20', '3/20/20', '3/21/20',
          '3/22/20', '3/23/20', '3/24/20', '3/25/20', '3/26/20',
          '3/27/20', '3/28/20', '3/29/20', '3/30/20', '3/31/20',
          '4/1/20', '4/2/20', '4/3/20', '4/4/20', '4/5/20', '4/6/20',
          '4/7/20', '4/8/20', '4/9/20', '4/10/20', '4/11/20',
          '4/12/20', '4/13/20', '4/14/20', '4/15/20', '4/16/20',
          '4/17/20', '4/18/20', '4/19/20', '4/20/20', '4/21/20',
          '4/22/20', '4/23/20', '4/24/20', '4/25/20', '4/26/20',
          '4/27/20', '4/28/20', '4/29/20', '4/30/20', '5/1/20', 
          '5/2/20', '5/3/20', '5/4/20', '5/5/20', '5/6/20',
          '5/7/20', '5/8/20', '5/9/20', '5/10/20', '5/11/20',
          '5/12/20', '5/13/20', '5/14/20', '5/15/20', '5/16/20',
          '5/17/20', '5/18/20', '5/19/20', '5/20/20', '5/21/20',
          '5/22/20', '5/23/20', '5/24/20', '5/25/20', '5/26/20',
          '5/27/20', '5/28/20', '5/29/20', '5/30/20', '5/31/20')

# Get New York Data
all_dat <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

ny_temp <- subset(all_dat, Province_State == "New York")

# Fill each state array with S, E, I, R real data
ny_mat <- matrix(ncol = 2, nrow=length(dates))

k <- 1
tempI <- 0
# Iterate through dates and create matrix
for (i in dates) {
  nyI <- sum(ny_temp[[i]]) - tempI
  tempI <- sum(ny_temp[[i]])
  nyS <- popNY - nyI
  
  # Iterate through matrix
  ny_mat[k,1] <- nyS
  ny_mat[k,2] <- nyI
  k <- k + 1
}

ny <- data.frame(ny_mat)

day <- 1:91
plot(day, ny$X1/popNY)
matplot(day, ny$X2, type="l", lty=1, add=TRUE)

#-------------------------------------------------------------------------------
# Least-Squares Optimization
#-------------------------------------------------------------------------------



##################################################################
