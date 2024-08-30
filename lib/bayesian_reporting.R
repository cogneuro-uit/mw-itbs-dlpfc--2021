bay_hdi <- function(data, ...){
  arg <- list(...)
  f_hdi <- do.call(fmt_APA_numbers, c(list(hdi(data)), arg))
  paste0("[", f_hdi[1], ", ", f_hdi[2], "]")
}

bay_er <- function(data, ...){
  arg <- list(...)
  m <- mean(data)
  do.call(fmt_APA_numbers, c(
    ifelse(
      m>0, 
      sum(data>0) / sum(data<=0),
      sum(data<0) / sum(data>=0)
    ), arg))
}  

bay_p <- function(data, ...){
  arg <- list(...)
  m <- mean(data)
  do.call(fmt_APA_numbers, c(
    ifelse(
      m>0,
      sum(data>0) / length(data), 
      sum(data<0) / length(data)
    ), arg))
}
