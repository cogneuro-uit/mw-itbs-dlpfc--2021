fmt_APA_numbers <- function(num, .p = FALSE, .psym = FALSE, .low_val = FALSE, .chr = FALSE){
  require(purrr)
  
  purrr::map(num, \(num){
    o_num <- num
    # Store original value
    num <- as.numeric(num)
    # Transform to numeric (if possible)
    if(is.na(num)){
      return(o_num)
      # if is not numeric after transformation, return original
    }
    
    if(.psym){
      if(num < .001){
        return("< .001")
      } else {
        return( round(num, 3) |> as.character() |> str_replace("0.", "= ") )
      }
    }
    if(.p){
      if(num < .001){
        return("< .001")
      } else {
        return( round(num, 3) |> as.character() |> str_replace("0.", ".") )
      }
    }
    
    # NORMAL VALS
    if(num >= 100 | num <= -100){
      num <- round(num, 0)
    }
    if(num >= 10 | num <= -10){
      num <- round(num, 1)
    }
    
    # IF LOW VALUES
    if(num >= 1 | num <= -1 | num < 1 & !.low_val | num > -1 & !.low_val){
      num <- round(num, 2)
    }
    if(num < 1 & .low_val | num > -1 & .low_val){
      num <- round(num, 3)
    }
    # IF .chr
    if(.chr){
      return( as.character(num) )
    } else {
      return( num )
    }
  }) |> unlist()
}
