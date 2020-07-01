# Script created by Frank D'Agostino 2020
# Code was based on work done by Dr. Caroline Buckee and colleagues
# Found here: http://rstudio-pubs-static.s3.amazonaws.com/6852_c59c5a2e8ea3456abbeb017185de603e.html

rm(list = ls())
# Clear console
library(deSolve)

#-------------------------------------------------------------------------------
# Data Cleanup
#-------------------------------------------------------------------------------

SIR <- function(parameters) {
  
  # Let's see the inputted parameters
  print(parameters)
  
  # Create the time and initialization parameters
  time <- seq(from = 0, to = 500, by = 1)
  init <- c(S = 0.95, I = 0.05, R = 0)
  
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

disp_param <- c(beta = 0.05, recovery = 0.005, death = 0.001, birth = 0.001)
out <- SIR(disp_param)

matplot(out[, 1], out[, 2:4], type="l", lty=1)
legend("topright", col=1:3, legend=c("S", "I", "R"), lwd=1)

##################################################################
