rm(list = ls())

## 1st version ####   ----------------------------------------------------------

iniMaju <- function(chaine) {
  return(paste(toupper(substr(chaine, 0, 1)),
               tolower(substr(chaine, 1, nchar(chaine))), sep = ""))}


iniMaju("wAssERMan")  # Test => result is wrong !!! 
debug(iniMaju)        # debug is useless here


## 2nd version ####   ---------------------------------------------------------

iniMaju2 <- function(chaine) {
  initial <- toupper(substr(chaine, 0, 1))
  rest    <- tolower(substr(chaine, 1, nchar(chaine)))

  return(paste(initial, rest, sep = ""))
}

iniMaju2("wAssERMan")  # Test => result is still wrong !!! 
debug(iniMaju2)        # debug will help here


## 3rd version ####   ---------------------------------------------------------

iniMaju3 <- function(chaine) {
  initial <- toupper(substr(chaine, 1, 1))
  rest    <- tolower(substr(chaine, 2, nchar(chaine)))
  
  return(paste(initial, rest, sep = ""))
}

iniMaju3("wAssERMan")  # Test => yes result is right, coffee break


## Timmings -------------------------------------------------------------------

x <- runif(1e6)
y <- runif(1e6)
z <- vector(length = 1e6)

system.time(z <- x + y)
system.time(for(i in 1:length(x)) z[i] <- x[i] + y[i])


library(microbenchmark)

m <- microbenchmark(z <- x + y,
               for(i in 1:length(x)) z[i] <- x[i] + y[i],
               times = 10)

ggplot2::autoplot(m)