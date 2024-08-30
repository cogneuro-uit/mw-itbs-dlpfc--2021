# Format numbers to APA standard
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
    
    # p with symbol
    # = symbol:
    if(.psym){
      if(num < .001){
        return("< .001")
      } else {
        return( round(num, 3) |> as.character() |> str_replace("0.", "= ") )
      }
    }
    
    # < than symbol
    if(.p){
      if( num == 1 ){
        return( "1.00" )
      }
      
      num <- round(num, 3)
      if( num == 1 ){
        return( "> .999")
      }
      
      if(num < .001){ 
        num <- "< .001"
      } else {
        num <- num |> as.character() |> str_replace("0.", ".")
        if(str_length(num) <= 3){
          for(x in 1:(4 - str_length( num)) ){
            num <- paste0(num, "0")
          }
        }
      }
      return( num ) 
    }
    
    # > 100
    if(num >= 100 | num <= -100){
      num <- round(num, 0)
      if(.chr){
        return( as.character( num) ) 
      }
      return( num )
    }
    
    # > 10
    if(num >= 10 | num <= -10){
      num <- round(num, 1)
      # Add an extra 0 using strings to return tidy APA numbers 
      if(.chr){
        if(str_length( as.character( abs(num) ) ) == 2){
          num <- paste0(num, ".0") # if exactly two, always add .0)
        }
        return( as.character(num) )
      }
    }
    
    # > 1  | > 1 & .low_val
    if(num >= 1 | num <= -1 | num < 1 & !.low_val | num > -1 & !.low_val){
      num <- round(num, 2)
      
      if(.chr){
        if(str_length( as.character( abs(num) ) ) <= 3){
          # Add an extra 0 using strings to return tidy APA numbers 
          for(x in 1:(4 - str_length( as.character( abs(num) ) )) ){
            if(str_detect(num,"\\.", negate = TRUE)){
              num <- paste0(num, ".")
            } else{
              num <- paste0(num, "0")
            }
          }
        }
        return( as.character(num) )
      }
    }
    
    
    # Low values 
    if(num < 1 & .low_val | num > -1 & .low_val){
      num <- round(num, 3)
      if(.chr){
        if(str_length( as.character( abs(num) ) ) <= 4){
        # Add an extra 0 using strings to return tidy APA numbers 
          for(x in 1:(5 - str_length( as.character( abs(num) ) )) ){
            if(str_detect(num,"\\.", negate = TRUE)){
              num <- paste0(num, ".")
            } else {
              num <- paste0(num, "0")
            }
          }
        }
        return( as.character(num) )
      }
    }
    
    return( num )
  }) |> unlist()
}
