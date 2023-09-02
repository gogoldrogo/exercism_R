hamming <- function(strand1, strand2) {
  count = 0
  if (nchar(strand1) == nchar(strand2)){
    for (x in 1:nchar(strand1)){
      if (substr(strand1,start=x,stop=x) != substr(strand2,start=x,stop=x)){
        count = count + 1
      }
    }
  } else
  {
    stop()
  }
  return(count)
}