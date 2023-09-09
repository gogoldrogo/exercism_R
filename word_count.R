word_count <- function(input) {
  removing_punctuations <- gsub("[^[:alnum:][:space:]']"," ",input)
  removing_newline <- gsub("\n"," ",removing_punctuations)
  removing_tab <- gsub("\t"," ",removing_newline)
  final <- strsplit(removing_tab,split=" ") |> unlist() |> tolower()
  final_len <- length(final)
  
  lst <- c()
  for(i in 1:final_len){
    if(nchar(final[i]) != 0){
      lst <- c(lst,final[i])
    }
  }
  
  sorted_lst <- sort(lst)
  sorted_lst_len <- length(sorted_lst)
  
  word_lst <- c()
  for(i in 1:sorted_lst_len){
    if(! sorted_lst[i] %in% word_lst){
      word_lst <- c(word_lst,sorted_lst[i])
    }
  }
  
  count <- c()
  for(s in 1:length(word_lst)){
    count[s] <- c(0)
  }
  
  for(j in 1:sorted_lst_len){
    for(k in 1:length(word_lst)){
      if(sorted_lst[j] == word_lst[k]){
        count[k] <- count[k] + 1
      }
    }
  }
  emp_list <- list()
  emp_list <- append(emp_list,count)
  names(emp_list) <- word_lst
  return(emp_list)
}