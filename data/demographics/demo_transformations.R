# demographic changes
library(haven)
read_spss("data/demographics/ft-rsgt_rtms_dlpfc_sheet-demographics.sav") -> raw_demo_pfc

randlist.pfc <- read_csv("data/export/randlist_pfc_Nyp7ObM.csv", comment="#")

# Connect true stim on demographics
randlist.pfc |> mutate(subj = as.numeric(gsub("PFC", "", subj)),
                       true_stim1 = session1,
                       true_stim2 = session2) |> 
  select(true_stim1, true_stim2, subj) |>
  right_join(raw_demo_pfc, join_by(subj==Code)) |> 
  mutate(across(true_stim1:true_stim2, ~ ifelse(.x=="real", 1,0))) |> 
  select(subj:Notater, true_stim1,true_stim2) -> demo_pfc

save(demo_pfc, file = "data/demo_pfc.rdata")
