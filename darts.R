score <- function(x, y) {
  x1 <- 0
  y1 <- 0
  val <- sqrt((x-x1)^2 + (y-y1)^2)
  if (val > 10){
    return(0)
  } else if (val <= 10 && val > 5){
    return(1)
  } else if (val <= 5 && val > 1){
    return(5)
  } else if (val <= 1){
    return(10)
  }
}

score(0,10)
