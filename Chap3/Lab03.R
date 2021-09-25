## ---1 Amélioration du temps d’exécution---
# 1ère version
somme1 <- function(x, y) {
z <- 0
for(i in seq_along(x)) z[i] <- x[i] + y[i]
return(z)
}

# 2ème version
somme2 <- function(x, y) {
z <- numeric(length(x))
for(i in seq_along(x)) z[i] <- x[i] + y[i]
return(z)
}

# 3ème version
somme3 <- function(x, y) return(x + y)

##---EXERCICES---

##2. Pouvez vous identifier quelle version sera la plus lente ? et la plus rapide ? Pourquoi ?
##3. Chronométrez le temps d’exécution de 3 version de la somme avec microbenchmark sur de vecteurs de longueur 1e6.
##4. Un simple changement permet d’améliorer le temps d’exécution par rapport à la v3. Chronométrez
##l’expression x + y (cad, sans appel à une fonction).
