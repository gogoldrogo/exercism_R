parse_phone_number <- function(number_string) {
  number_string <- gsub("\\D","",number_string)
  if (nchar(number_string) > 11 || nchar(number_string) < 10){
    return(NULL)
  } else if (nchar(number_string) == 11){
    check1 <- substr(x=number_string,start=1,stop=1)
    check2 <- substr(x=number_string,start=2,stop=2)
    check5 <- substr(x=number_string,start=5,stop=5)
    if (check1 == '1'){
      check2_i <- strtoi(check2)
      check5_i <- strtoi(check5)
      if (check2_i < 2 || check5_i < 2){
        return(NULL)
      } else{
        return(substr(x=number_string,start=2,stop=11))
      }
    } else if (check1 != '1'){
      return(NULL)
    }
  } else if (nchar(number_string) == 10){
    check1 <- substr(x=number_string,start=1,stop=1)
    check4 <- substr(x=number_string,start=4,stop=4)
    check1_i <- strtoi(check1)
    check4_i <- strtoi(check4)
    if (check1_i < 2 || check4_i < 2){
      return(NULL)
    } else{
      return(number_string)
    }
  }
}