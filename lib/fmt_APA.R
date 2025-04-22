# Format numbers to APA standard
fmt_APA_numbers <- function(x, .p = FALSE, .psym = FALSE, .low_val = FALSE, .chr = FALSE, .rm_leading_0 = FALSE){
  require(purrr)
  
  purrr::map(x, \(num){
    # Store original value
    o_num <- num
    
    # Force to numeric, otherwise return original
    num <- as.numeric(num)
    if( is.na(num) ){
      # if is not numeric after transformation, return the original 
      return(o_num)
    }
    
    # Frequentist PROBABILITY (p) with SYMBOLS
    # = symbol:
    if(.psym & !.p){
      if( num > .999 ){ # for some reason we do not have "1"
        return("> .999")
      }
      if( num < .001 ){
        return("< .001")
      } 
      return( round(num, 3) |> as.character() |> str_replace("0.", "= .") )
    }
    
    # Frequentist PROBABILITY (p) 
    if(.p){
      if( num > .999 ) return( ".999")
      if( num < .001 ) return( ".001") 
      
      # else...
      num <- 
        num |> 
        round(digits = 3) |> 
        as.character() |> 
        str_replace("0.", ".")
      
      if( str_length(num) <= 3){
        for(x in 1:(4 - str_length( num )) ){
          num <- paste0(num, "0")
        }
      }
      return( num ) 
    }
    
    # At 100, no preceding 0 (e.g., 153 instead of 153.382)
    if( num >= 100 | num <= -100 ){
      num <- round( num, 0 ) 
      if(.chr){
        num <- as.character( num )
        if(.rm_leading_0) num <- str_remove(num, "^0")
      } 
      return( num )
    }
    
    # At 10 or more, one preceeding 0 (e.g., 12.1 instead of 12.132)
    if( num >= 10 | num <= -10 ){
      num <- round(num, 1)
      
      # Add an extra 0 using strings to return tidy APA numbers 
      if(.chr){
        if(str_length( as.character( abs(num) ) ) == 2){
          num <- paste0(num, ".0") # if exactly two, always add .0)
        }
        if(.rm_leading_0) num <- str_remove(num, "^0")
        return( as.character(num) )
      }
    }
    
    # At 1 or more 1, two preceding 0's (e.g., 4.29 instead of 4.293125)
    # *Also with low-val 
    if( num >= 1 | num <= -1 | 
        num < 1 & !.low_val | num > -1 & !.low_val){
      num <- round(num, 2)
      
      if(.chr){
        if( str_length( as.character( abs(num) ) ) <= 3 ){

          # Add an extra 0 using strings to return tidy APA numbers 
          for(x in 1:(4 - str_length( as.character( abs(num) ) )) ){
            if( str_detect(num,"\\.", negate = TRUE) ){
              num <- paste0(num, ".")
            } else{
              num <- paste0(num, "0")
            }
          }
        }
        if(.rm_leading_0) num <- str_remove(num, "^0")
        return( as.character(num) )
      }
    }
    
    
    # At values below 1, 3 preceding 0's (e.g., 0.234, instead of 0.234423)
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
        if(.rm_leading_0) num <- str_remove(num, "^0")
        return( as.character(num) )
      }
    }
    
    return( num )
  }) |> unlist()
  
}


tab_fmt_APA <- function(data){
  require(gt)
  #' Supplementary function to quickly transform a GT table to an approxiate APA 7 format
  #' 
  
  tab_line_width <- 1
  tab_line_colour <- "black"
  
  data |>
    tab_style(cell_text(size = 12, align="left"), cells_title()) |>
    tab_options(
      # General TABLE params
      table.background.color = "white",
      table.border.top.color = "white",
      table.border.bottom.color = "white",
      table_body.border.bottom.color = tab_line_colour,
      table.width = pct(100),
      # Column label things
      column_labels.border.top.width = tab_line_width, # +1 ?
      column_labels.border.top.color = tab_line_colour,
      column_labels.border.bottom.width = tab_line_width,
      column_labels.border.bottom.color = tab_line_colour,
      # ROW groups (if relevant)
      row_group.border.bottom.color = tab_line_colour,
      row_group.border.bottom.width = tab_line_width,
      row_group.border.top.color = tab_line_colour,
      row_group.border.top.width = tab_line_width,
    ) |>
    # set table style
    tab_style(
      style = list(
        # remove horizontal lines
        cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = px(1)
        ),
        # remove row striping in Markdown documents
        cell_fill(color = "white", alpha = NULL)
      ),
      # do this for all columns and rows
      locations = cells_body(
        columns = everything(),
        rows = everything()
      ))
}
