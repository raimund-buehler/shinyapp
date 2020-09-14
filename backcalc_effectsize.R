es.backconv <- function(x, n){
  r <- x
  type_ES <- para$es
  
  # conversion

    z <- 0.5 * log((1 + r)/(1 - r))
    d  <- (2 * r)/(sqrt(1 - r^2))
    n <- sum(n, na.rm = TRUE)
    J = 1 - (3 / (4 * (n - 2) - 1))
    g <- d * J
    logOR <- pi * d / sqrt(3)
   
    # output
  
if (type_ES == "z") {
  es.out <- z
} else if (type_ES == "d") {
  es.out <- d
} else if (type_ES == "g"){
  es.out <- g
} else if (type_ES == "logOR"){
  es.out <- logOR
} else if (type_ES == "r"){
  es.out <- r
}
return(es.out)
}