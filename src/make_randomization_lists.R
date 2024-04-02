#' Create randomization lists for the two studies
#' 
#' - after every 8 participants, there should be an equal number
#'   of sham vs. real 
#' - subject-codes are "AG001", "AG002", ... and "PFC001", "PFC002", ...
#' - these should be translated into individual cards in envelopes
#' 
library(tidyverse)

num.blocks=5 # 5x8=40 but do an extra block of 8 for spill-over
N=num.blocks*8
conditions=c("sham","real")

session1.ag <- map(1:num.blocks, ~ sample( rep(conditions, each=4), size = 8, replace=F)) |> unlist()
session1.pfc <- map(1:num.blocks, ~ sample( rep(conditions, each=4), size = 8, replace=F)) |> unlist()

tibble(
  subj=sprintf("AG%03i", 1:N),
  region="Angular Gyrus",
  session1=session1.ag,
  session2=map_chr(session1.ag, ~ setdiff(conditions, .x))
) -> randlist.ag


tibble(
  subj=sprintf("PFC%03i", 1:N),
  region="Prefrontal Cortex",
  session1=session1.pfc,
  session2=map_chr(session1.pfc, ~ setdiff(conditions, .x))
) -> randlist.pfc



library(R.rsp)
library(hashids)

## make a random has based on the current timestamp
now=Sys.time()
h = hashid_settings(salt = 'rTMS study')
liststamp=encode(as.integer(now), h) 

#' write csv file with prepended comments
#' 
write_csv_with_comments <- function(object, fname, comments=NULL, comment.char="#"){
  con <- file(fname, open="wt")
  if(!is.null(comments)){
    writeLines(paste(comment.char, comments), con)
  }
  write.csv( object, con)
  close(con)
}

comments=c(sprintf("Generated on %s", now),
           sprintf("Hash: %s", liststamp))

## write plain labels sham/real
write_csv_with_comments(randlist.ag, 
                        sprintf("data/export/randlist_ag_%s.csv", liststamp), 
                        comments=comments)
write_csv_with_comments(randlist.pfc,
                        sprintf("data/export/randlist_pfc_%s.csv", liststamp), 
                        comments=comments)


## write A/B (hidden for later analysis)
codebook=tibble(A=sample(conditions,1), B=setdiff(conditions, A))
randlist.ag.hidden <- randlist.ag |>
  mutate(session1=fct_recode(session1, A=codebook$A, B=codebook$B),
         session2=fct_recode(session2, A=codebook$A, B=codebook$B)) 
randlist.pfc.hidden <- randlist.pfc |>
  mutate(session1=fct_recode(session1, A=codebook$A, B=codebook$B),
         session2=fct_recode(session2, A=codebook$A, B=codebook$B)) 

write_csv_with_comments(randlist.ag.hidden, 
                        sprintf("data/export/randlist_ag_hidden_%s.csv", liststamp), 
                        comments=comments)
write_csv_with_comments(randlist.pfc.hidden,
                        sprintf("data/export/randlist_pfc_hidden_%s.csv", liststamp), 
                        comments=comments)



randlist=randlist.ag
fname=sprintf("randcard_ag_%s.tex", liststamp)
rsource("src/randcards/randcard.tex.rsp", output = paste0("src/randcards/",fname), buffered=TRUE)
system(sprintf("cd src/randcards && pdflatex -interaction nonstopmode %s", fname))

randlist=randlist.pfc
fname=sprintf("randcard_pfc_%s.tex", liststamp)
rsource("src/randcards/randcard.tex.rsp", output = paste0("src/randcards/",fname), buffered=TRUE)
system(sprintf("cd src/randcards && pdflatex -interaction nonstopmode %s", fname))


