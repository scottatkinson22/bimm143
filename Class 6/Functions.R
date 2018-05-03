# Class 6 Bioinformatics

# Functions

add <- function(x, y=1) {
  # Sum x and y inputs
  x + y
}

# My second function
rescale <- function(x) {
  rng <-range(x, na.rm=TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
# Test on a small example where you know the answer
rescale(1:10)
# Test on a small example where you know the answer
rescale(1:10)
# How would you get your function to work here...
rescale( c(1,2,NA,3,10) )
# What should your function do here?
rescale( c(1,10,"string") )
##new code
rescale2 <- function(x, na.rm=TRUE, plot=FALSE) {
  if(na.rm) {
    rng <-range(x, na.rm=TRUE)
  } else {
    rng <-range(x)
  }
  print("Hello")
  answer <- (x - rng[1]) / (rng[2] - rng[1])
  return(answer)
  print("is it me you are looking for?")
  if(plot) {
    plot(answer, typ="b", lwd=4)
  }
  print("I can see it in ...")
}

#Lets test rescale2
rescale2(c(1,2,NA,3,10))
###new
rescale3 <- function(x, na.rm=TRUE, plot=FALSE) {
  if(na.rm) {
    rng <-range(x, na.rm=na.rm)
  } else {
    rng <-range(x)
  }
  print("Hello")
  answer <- (x - rng[1]) / (rng[2] - rng[1])
  print("is it me you are looking for?")
  if(plot) {
    plot(answer, typ="b", lwd=4)
  }
  print("I can see it in ...")
}