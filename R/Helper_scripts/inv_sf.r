#######################################################################
#
# inv_sf.r    09 Dec 2015
#
# Author: Gregory Garner (ggg121@psu.edu)
#
# Function that calculates the inverse of a survival function
#
# To use this function, simply source this file:
#   source("inv_sf.r")
#
# Version History:
#   1.0 - 09 Dec 2015 - Initial coding (G.G.)
#
# Function Name: inv.sf
# Parameters:
#   x - vector of data
#   val - value you're trying to get an SF value for
#
# Example:
# inv.sf(data[ ,1], 3.25)
#   - Will return the SF value for 3.25 given the data in data[ ,1]. You can
#     use this function in an apply() function to get the values over the
#     entirematrix of data.
# apply(data, 2, inv.sf, val = 3.25)
#
# NOTE: That the function returns "NA" if the value cannot be calculated
# (i.e., val is outside of the range of x).  Also, if val does not have
# an exact match in x, the SF value is linearly interpolated between the
# two closest points.
#
# THIS CODE IS PROVIDED AS-IS WITH NO WARRANTY (NEITHER EXPLICIT
# NOR IMPLICIT).  I SHARE THIS CODE IN HOPES THAT IT IS USEFUL,
# BUT I AM NOT LIABLE FOR THE BEHAVIOR OF THIS CODE IN YOUR OWN
# APPLICATION.  YOU ARE FREE TO SHARE THIS CODE SO LONG AS THE
# AUTHOR(S) AND VERSION HISTORY REMAIN INTACT.
#
#######################################################################

inv.sf <- function(x, val) {
  
  # Is val outside the range?
  if(max(x) < val | min(x) > val){
    return(NA)
  }
  
  # Sort the vector and get its length
  sort.x <- sort(x)
  num.x <- length(x)
  sf <- seq(1,1/num.x, by=-1/num.x)
  
  # Determine the bounding values
  lb.i <- findInterval(val, sort.x)
  ub.i <- lb.i + 1
  
  # Note, if the lower bound is the last
  # element in the vector, return the 
  # SF value for the last item
  # (i.e. x[num.x] == val)
  if (lb.i == num.x) {
    return(1/num.x)
  }
  
  # Set the lower and upper bounds for interpolation
  lb.x <- sort.x[lb.i]
  ub.x <- sort.x[ub.i]
  lb.sf <- sf[lb.i]
  ub.sf <- sf[ub.i]
  
  # Fit the interpolation
  ((val - lb.x)/(ub.x - lb.x)) * (ub.sf - lb.sf) + lb.sf
  
}