## File: lab_x002-debugging.r
## Description: Sample code on how to debug in R. 
##              Material from The Art of R programming (N. Matloff, 2011)
## Date: Oct 20 by jc

rm(list = ls())

## I. Example : findruns ####
## _a. Meaningless naming, bad style : this is difficult to read and debug ! ####
joe=function(x,k){
n=length(x)
r=NULL
for(i in 1:(n-k)) if(all(x[i:i+k-1]==1)) r<-c(r,i)
r
}
# joe(c(1,0,0,1,1,1,0,1,1),2) # for testing

## _b. Better (buggy) version with corrected style and naming ####
## Find runs of consecutive 1s in 0-1 vectors
## e.g. findruns(c(1,0,0,1,1,1,0,1,1),2) shown return (4,5,8).
## Arguments   
##             x : vector containing 0s and 1s 
##             k : min size of the run
findruns <- function(x, k) {
  n <- length(x)
  runs <- NULL
  for (i in 1:(n - k + 1)) {
    if (all(x[i:(i + k - 1)] == 1)) runs <- c(runs, i)
    
  }
  return(runs)
}

debug(findruns)
# findruns(c(1, 0, 0, 1, 1, 1, 0, 1, 1), 2) # for testing
undebug(findruns)


## _c. Correct version with corrected style and naming ####
## Find runs of consecutive 1s in 0-1 vectors
## e.g. findruns(c(1,0,0,1,1,1,0,1,1),2) shown return (4, 5, 8).
## Arguments   
##             x : vector containing 0s and 1s 
##             k : min size of the run
findruns <- function(x, k) {
  n <- length(x)
  runs <- NULL
  for (i in 1:(n - k + 1)) {
    if (all(x[i:(i + k - 1)] == 1)) runs <- c(runs, i)
  }
  return(runs)
}

debug(findruns)
# findruns(c(1, 0, 0, 1, 1, 1, 0, 1, 1), 2) # for testing
undebug(findruns)


## II. (cf fichier lab2-exo2.r) ####

# 1. versions alternatives
f1 <- function(n){
  res <- 0 
  for (i in 1:n) res <- res + i

  return(res)
}

f2 <- function(n) sum(seq(n))

f3 <- function(n) n * (n + 1) / 2

# 2. Temps d'éxecution

n <- 1e10
#system.time(f1(n))
system.time(f2(n))
system.time(f3(n))

## IV. Simulation par la méthode du rejet

# [Ex. 1] Simulation par la méthode de rejet ######################

# 1. Fonction de densité d'une va triagulaire
# Input
#    x :  abscisse
f_tri <- function(x) {
  SUP01 <- (0 <= x) & (x < 1)
  SUP12 <- (1 <= x) & (x < 2) 
  if (SUP01) {
    return(x)
  } else {if (SUP12) { 
    return(2 - x)
  } else {
    return(0)
  }
  }
}

# Version optimizée
# f_tri2 <- function(x) {
#   SUP01 <- (0 <  x) & (x < 1)
#   SUP12 <- (1 <= x) & (x < 2) 
#   ifelse(SUP01, x, ifelse(SUP12, 2 - x, 0))
# }

# Test f_tri
# x <- seq(-1, 3, length.out = 101)
# f_tri_x <- numeric(length(x)) 
# for(i in seq_along(x)) f_tri_x[i] <- f_tri(x[i])

#plot(x, f_tri_x, type = 'l')
#rug(Observations)


# 2. 
# Input
#    fx   :  fonction de densité
#    a, b :  bornes du support de fx
#    M    :  borne supérieure de fx
rejection <- function(fx, a, b, M) {
  while (TRUE) { 
    x <- runif(1, a, b)
    y <- runif(1, 0, M)  
    if (y < fx(x)) return(x)
  }
}


# 3. Test rejection
nreps <- 1000
Observations <- numeric(nreps)
for (i in seq_along(Observations)) 
  Observations[i] <- rejection(f_tri, 0, 2, 1)




## V. Example : Finding City Pairs ####
# returns the minimum value of d[i,j], i != j, 
# and the row/col attaining that minimum, for square 
# symmetric matrix d; no special policy on ties;
# motivated by distance matrices

## _a. ####

mind <- function(d) {
  n <- nrow(d)
  # add a column to identify row number for apply()
  dd <- cbind(d, 1:n)
  wmins <- apply(dd[-n, ], 1, imin)
  # wmins will be 2xn, 1st row being indices and 2nd being values
  i <- which.min(wmins[1, ])
  j <- wmins[2, i]
  return(c(d[i, j], i, j))
}


# finds the location, value of the minimum in a row x
imin <- function(x) {
  n <- length(x)
  i <- x[n]
  j <- which.min(x[(i + 1):(n - 1)])
  return(c(j, x[j]))
}


## _b. ####


## _c. ####

##-------------------------------------------   -------------------------------------------

## File: lab2-exo2.r
## Description: corrigé du TP2, exo 2
## Date: Oct 20 by jc

# Fonction originale
f1<-function(n)res<-NULL;for(i in 1:n)res<-res+1

# 2. avec style R
f1 <- function(n){
  res <- NULL
  for (i in 1:n) {
    res <- res+1
  }
}

# 3. testing
f1(-2) # f1(0)
f1(1)  # wrong
f1(2)  # rien
f1(10) # rien

# 4. Debug 
debug(f1)
f1(10)

# initialisation de res à 0 au lieu de NULL & renvoi de res
f1 <- function(n){
  stopifnot(as.integer(n) == n)
  stopifnot(n > 0)
  res <- 0 
  for (i in 1:n) {
    res <- res + i
  }
  return(res)
}