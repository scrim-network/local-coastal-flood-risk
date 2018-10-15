##################################################################################
# Empirical_probability_calculator.R
#
# The script include functions that return either a vector of return periods
# or annual exceedance probabilities for an input vector of block maxima 
# observations. See "Contents" below for more details.
#
# Contact K. Joel Roop-Eckart (kjr30@psu.edu) regarding questions.
##################################################################################
# Copyright 2018 K. Joel Roop-Eckart
# This file is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this file.  If not, see <http://www.gnu.org/licenses/>.
#
############################## Contents ##########################################
# mean.rt: returns a vector of the mean return periods for an input vector of block maxima observations
# mean.prob: returns a vector of the mean annual exceedence probabilities for an input vector of block maxima observations

# jenkinson.rt: returns a vector of the jenkinson approximation of the median return periods for an input vector of block maxima observations
# jenkinson.prob: returns a vector of jenkinson approximation of the median annual exceedence probabilities for an input vector of block maxima observations

# median.rt: returns a vector of the median return periods for an input vector of block maxima observations
# median.prob: returns a vector of the median annual exceedence probabilities for an input vector of block maxima observations
############################## Instructions #######################################
# for continuous block maxima observations, input a vector of the block maxima observations
# The return period for a single historical or paleo observation with a return period far exceeding the historical record
# may be computed by entering a vector of any values that is the length of the historical or paleo record
# E.g. If the event happened 1,000 years ago, enter: vector(mode = 'numeric', length = 1000), and take the max()
# return period output, or min() probability output from the function.

# note: probabilities and return periods are interchangable: 1/rt = prob, 1/prob = rt
# note: Cumulative exceedence probability (used in quantile functions) is 1-annual exceedence probability (1-prob: 1-1/rt)
############################## Packages ###########################################
library('DEoptim') # load DEoptim for optimization
# Functions are only defined for continuous datasets (Paleo and historical data not supportedo)
############################## Mean probability function ###############################
# mean probability return period formula
mean.rt <- function(obs){
  l <- length(obs)
  out <- sort((1+l)/1:l, decreasing = FALSE)
  return(out)
}

# mean probability formula
mean.prob <- function(obs){
  l <- length(obs)
  out <- sort(1:l/(1+l), decreasing = TRUE)
  return(out)
}

####################### Jenkinson approximate median probability function #####################
# Jenkinson approximate median probability return period formula
jenkinson.rt <- function(obs){
  l <- length(obs)
  out <- sort(1/(1-(1:l-0.31)/(l+0.38)), decreasing = FALSE)
  return(out)
}
# Jenkinson approximate median probability formula
jenkinson.prob <- function(obs){
  l <- length(obs)
  out <- sort((1-(1:l-0.31)/(l+0.38)), decreasing = TRUE)
  return(out)
}
####################### Numerical median probability function #####################
# define emperical probability searching function
median.auxiliary.func <- function(p, e, n){
  out <- abs((1-pbinom(e-1, n, p))-0.5)
  return(out)
}
# Numerical median probability return period formula
median.rt <- function(obs){
  l <- length(obs)
  # define variables
  e <- 1:l # the ranks of the events
  n <- l # sample size for the events
  pb <- txtProgressBar(min = 0, max = l, initial = 0, char = '=', style = 1) # loading bar
  prob <- vector(mode = 'numeric', length = l)
  for (i in 1:l) {
    setTxtProgressBar(pb, i) # loading bar
    fit <- DEoptim(median.auxiliary.func, lower = 0, upper = 1, e = e[i], n = n, control = DEoptim.control(trace = FALSE))
    prob[i] <- fit$optim$bestmem
  }
  close(pb)
  out <- sort(1/prob, decreasing = FALSE)
  return(out)
}
# Numerical median probability formula
median.prob <- function(obs){
  l <- length(obs)
  # define variables
  e <- 1:l # the ranks of the events
  n <- l # sample size for the events
  pb <- txtProgressBar(min = 0, max = l, initial = 0, char = '=', style = 1) # loading bar
  prob <- vector(mode = 'numeric', length = l)
  for (i in 1:l) {
    setTxtProgressBar(pb, i) # loading bar
    fit <- DEoptim(median.auxiliary.func, lower = 0, upper = 1, e = e[i], n = n, control = DEoptim.control(trace = FALSE))
    prob[i] <- fit$optim$bestmem
  }
  close(pb)
  out <- sort(prob, decreasing = TRUE)
  return(out)
}
