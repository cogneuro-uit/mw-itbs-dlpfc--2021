bay_hdi <- function(data, ...){
  arg <- list(...)
  f_hdi <- do.call(fmt_APA_numbers, c(list(hdi(data)), arg))
  paste0("[", f_hdi[1], ", ", f_hdi[2], "]")
}

bay_er <- function(data){
  m <- mean(data)
  ifelse(
    m>0, 
    sum(data>0) / sum(data<=0), 
    sum(data<0) / sum(data>=0)
  ) |>
    fmt_APA_numbers(num=_)
}  

bay_p <- function(data){
  m <- mean(data)
  ifelse(
    m>0,
    sum(data>0) / length(data), 
    sum(data<0) / length(data)
  ) |>
    fmt_APA_numbers(num=_, .p=T)
}