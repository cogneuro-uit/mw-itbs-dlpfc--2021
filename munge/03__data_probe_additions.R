# https://osf.io/2g8nz#61950fb10e2f500009c358da
library(lubridate)

demo_pfc$S2_Snack[demo_pfc$S2_Snack==9] <- 0 # This is a typo.

## PFC         -----
paste0("PFC0", seq(1, 40, 1)) %>%
  replace(., str_length(.) == 5, gsub("PFC", "PFC0", .[str_length(.) == 5])) -> t

### Music & meditation         -----      
tibble(s = t, 
       music_year = factor(demo_pfc$Music_years, ordered=T),
       meditation = factor(demo_pfc$Meditation-1, ordered=T)) -> m_m_to_probe

### Fatigue        -----
# Reverse scaleing 
tibble(subj = t, 
       S1 = demo_pfc$S1_Tired, 
       S2 = demo_pfc$S2_Tired) |> 
  mutate(S1 = 11-S1,
         S2 = 11-S2) |>
  pivot_longer(c(S1,S2)) |> 
  mutate(value = factor(value, ordered=T)) -> fatigue_to_probe

### Snack        -----
tibble(subj = t, 
       S1 = factor(demo_pfc$S1_Snack), 
       S2 = factor(demo_pfc$S2_Snack)) |> 
  pivot_longer(c(S1,S2)) -> snack_to_probe

### Time of day        -----
tibble(subj = t, 
       S1 = demo_pfc$S1_Time, 
       S2 = demo_pfc$S2_Time) |>
  pivot_longer(c(S1,S2)) -> time_to_probe

### Time of year         -----
# currently missing this information 
tibble(subj = t, 
       S1 = as.Date(demo_pfc$S1_Date), 
       S2 = as.Date(demo_pfc$S2_Date)) |>
  pivot_longer(c(S1,S2)) -> date_to_probe


### Bind to data probe (d.pro)         -----
d.pro.stim_pfc |> 
  left_join(m_m_to_probe, join_by(subj==s)) |>
  left_join(fatigue_to_probe, by = c("subj", "session"="name")) |> 
  left_join(snack_to_probe, by = c("subj", "session"="name")) |>  
  left_join(time_to_probe, by = c("subj", "session"="name")) |>
  left_join(date_to_probe, by = c("subj", "session"="name")) |> 
  rename(fatigue = value.x, snack = value.y, time = value.x.x, date = value.y.y
         ) -> d.pro.stim_pfc

### Save ----
save(d.pro.stim_pfc, file="data/d.pro.stim_pfc.Rdata")

rm(date_to_probe)
rm(fatigue_to_probe)
rm(m_m_to_probe)
rm(snack_to_probe)
rm(time_to_probe)
rm(t)