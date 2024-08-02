############################
##### List of subsidiary functions for bateman function in ledalab
############################
# - bateman_gauss()
# - bateman()
############################

# TODO need to know why we convulating with gaussian distribution
#' @title convoluted output of bateman function and guassian distribution
#' @param time time data 
#' @param onset set time at onset to 0
#' @param amp if amp is 0 then normalized bateman (i.e. area(bateman)  <- 1/sr)
#' @param tau1 time constant in Bateman function that characterizes the steepness of onset
#' @param tau2 time constant in Bateman function that characterizes the steepness of recovery
#' @param sigma standard deviation of normal distribution used to convolve with the Batement function
#'
#' @author Nidhi Desai
#'
bateman_gauss <- function(time, onset, amp, tau1, tau2, sigma){
  component <- bateman(time, onset, 0, tau1, tau2)

  if (sigma > 0){
    sr <- round(1/mean(diff(time)))
    winwidth2 <- ceiling(sr*sigma*4) # round half winwidth: 4 SD to each side
    t <- c(1:(winwidth2*2+1)) # odd number (2*winwidth-half+1)
    g <- dnorm(t, winwidth2+1, sigma*sr) # normpdf
    g <- (g/max(g)) * amp
    
    bg <- pracma::conv(c(rep(1,winwidth2)*component[1], component, rep(1,winwidth2)*component[length(component)]), g)
    
    component <- bg[(winwidth2*2+1):(length(bg)-winwidth2*2)]
  }
  return(component)
}


#' @title calculating the bateman function
#' @param time time data 
#' @param onset set time at onset to 0
#' @param amp if amp is 0 then normalized bateman (i.e. area(bateman)  <- 1/sr)
#' @param tau1 time constant in Bateman function that characterizes the steepness of onset
#' @param tau2 time constant in Bateman function that characterizes the steepness of recovery
#' @return conductance
#' 
#' @author Nidhi Desai
#'
bateman <- function(time, onset, amp, tau1, tau2){
  if (tau1 < 0){
    stop(paste("ERROR inside bateman function: tau1 =", toString(tau1), "< 0"))
  } else if (tau2 < 0){
    stop(paste("ERROR inside bateman function: tau2 =", toString(tau2), "< 0"))
  } else if (tau1 == tau2) {
    stop(paste("ERROR inside bateman function: tau1 == tau2 =", toString(tau1)))
  }
  
  conductance <- rep(0,length(time))
  range <- which(time > onset)
  
  if (length(range) == 0){
    return(conductance)
  }
  
  xr <- time[range] - onset

  if (amp > 0){
    maxx <- tau1 * tau2 * log(tau1/tau2) / (tau1 - tau2) # b' <- 0
    maxamp <- abs(exp(-maxx/tau2) - exp(-maxx/tau1))
    ct <-  amp/maxamp
  } else { # amp == 0: normalized bateman, area(bateman) <- 1/sr
    sr <- round(1/mean(diff(time)))
    ct <- 1/((tau2 - tau1) * sr)
  }
  
  if (tau1 > 0){
    conductance[range] <- ct * (exp(-xr/tau2) - exp(-xr/tau1))
  } else {
    conductance[range] <- ct * exp(-xr/tau2)
  }
  
  return(conductance)
}

