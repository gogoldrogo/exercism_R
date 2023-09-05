anagram <- function(subject, candidates) {
  len <- length(candidates)
  subject <- tolower(subject)
  lev <- tolower(candidates)
  lev <- lev[! lev %in% subject]
  if (length(lev) == 0){
    stop()
  }
  subject <- tolower(paste0(sort(unlist(strsplit(subject,split=""))),collapse=""))
  
  lst <- c()
  for (i in 1:len) {
    can_iter <- tolower(paste0((sort(unlist(strsplit(lev[i],split="")))),collapse=""))
    lst <- c(lst,can_iter)
  }
  
  values <- c()
  for (i in 1:len){
    if (subject == lst[i]){
      values <- c(values,i)
    }
  }
  if (length(values) == 0){
    return(NULL)
  }
  
  fi <- length(values)
  val <- c()
  gal <- c()
  for (i in 1:fi){
    val <- values[i]
    gal <- c(gal,(lev[val]))
  }
 final <- tolower(candidates)
 result <- final %in% gal
 call <- which(result == TRUE)
 
 
 ji <- length(call)
 cal <- c()
 sol <- c()
 for (i in 1:ji){
   cal <- call[i]
   sol <- c(sol,(candidates[cal]))
 }
 return(sol)
}