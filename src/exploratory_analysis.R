library(ProjectTemplate)
load.project()

# Exploratory analyses

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
script_save_tables <- FALSE
  #' Tables are saved to "tables" folder.
script_save_figures <- FALSE
  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# prep
# Reverse the probes (higher value correspond to the probe Q)
d.pro.stim_pfc |> 
  mutate(
    probe1 = ordered(5-as.numeric(probe1)),
    probe2 = ordered(5-as.numeric(probe2)),
    probe3 = ordered(5-as.numeric(probe3)),
    probe1_n = as.integer(probe1),
    probe2_n = as.integer(probe2),
    probe3_n = as.integer(probe3),
    fatigue1 = as.integer(fatigue),
    stimulation = factor(stimulation, levels = c("sham", "real")),
    meditation1 = ifelse(meditation>0, 1, 0), #as.integer(meditation),
    music_year1 = ifelse(music_year>0, 1, 0), 
  ) -> pfc

# Pre-test      =====
## TMS expectation =====
# prep data
pfc |> 
  mutate(subj2 = as.numeric( str_split(subj, "PFC") |> map_chr(2)) ) |>
  summarise(
    .by = c(subj2, stimulation),
    session = unique(session),
    mw = mean(probe1_n),
    bv = mean(zlogbv),
    ae = mean(zlogapen)
  ) |>
  left_join(
    demo_pfc |> 
      select(subj, S1_TMS_expectation, S2_TMS_expectation) |> 
      pivot_longer(ends_with("expectation"),  values_to = "expectation") |>
      mutate(name = str_split(name, "_") |> map_chr(1)),
    by = join_by(subj2==subj, session==name)
  ) -> pfc_exp_sum

#' 0 = "No expectation" 
#' 1 = "Yes, increase" 
#' 2 = "Yes, reduce"
#' 3 = "Yes, but not how" 
#' 4 = "Don't know"

tms_expectation_tbl <- 
  pfc_exp_sum |>
  filter(expectation %in% c(0, 1, 2)) |>
  pivot_longer(c(mw,bv,ae)) |>
  summarise(
    .by = name, 
    no_m = mean(value[expectation==0]),
    no_sd = sd(value[expectation==0]),
    i_m = mean(value[expectation==1]),
    i_sd = sd(value[expectation==1]),
    d_m = mean(value[expectation==2]),
    d_sd = sd(value[expectation==2]),
    i_mdiff    = mean(value[expectation==1]) - mean(value[expectation==0]), 
    i_t    = t.test(value[expectation==1], value[expectation==0])$statistic,
    i_df   = t.test(value[expectation==1], value[expectation==0])$parameter,
    i_p    = t.test(value[expectation==1], value[expectation==0])$p.value,
    i_bf01 = 1/extractBF(ttestBF(value[expectation==1], value[expectation==0]))$bf,
    d_mdiff    = mean(value[expectation==2]) - mean(value[expectation==0]), 
    d_t    = t.test(value[expectation==2], value[expectation==0])$statistic,
    d_df   = t.test(value[expectation==2], value[expectation==0])$parameter,
    d_p    = t.test(value[expectation==2], value[expectation==0])$p.value,
    d_bf01 = 1/extractBF(ttestBF(value[expectation==2], value[expectation==0]))$bf
  ) |>
  mutate(
    across(ends_with("_p"), ~fmt_APA_numbers(.x, .p=T)),
    across(where(is.double), ~fmt_APA_numbers(.x)),
    e="", e2="",
    name = fct_recode(name, MW="mw", BV="bv",AE="ae")
  ) |>
  rename(Variable="name") |>
  gt() |>
  tab_spanner("Increase", starts_with("i_")) |>
  tab_spanner("Decrease", starts_with("d_")) |>
  tab_spanner("No expectation", starts_with("no_")) |>
  cols_move(e, i_bf01) |>
  cols_move(e2, no_sd) |>
  cols_label(
    ends_with("_m")~md("*M*"),
    ends_with("_t")~md("*t*"),
    ends_with("_df")~md("*df*"),
    ends_with("_p")~md("*p*"),
    ends_with("_bf01")~md("BF~01~"), 
    ends_with("_sd") ~md("*SD*"),
    ends_with("diff") ~ md("*M*~diff~"),
    starts_with("e") ~"",
  ) |>
  cols_align("center")
tms_expectation_tbl

if(script_save_tables){
  gtsave(tms_expectation_tbl, "tables/tms_expectation_table.docx")
}
  
##  Music & Meditation        =====
# get data
m_m_data <- 
  pfc |> 
  pivot_longer(c(meditation1, music_year1), names_to = "cat", values_to="cat_val") |>
  mutate(probe1_n = scale(probe1_n)) |>
  summarise(
    .by = c(subj, cat),
    cat_v = unique(cat_val),
    mw = mean(probe1_n),
    ae = mean(zlogapen),
    bv = mean(zlogbv)) |>
  pivot_longer(c(mw,bv,ae))

### Tabel     =====
music_meditation_tbl <- 
  m_m_data |>
  summarise(
    .by = c(cat, name), 
    m0_m  = mean(value[cat_v==0]),
    m0_sd = sd(value[cat_v==0]),
    m1_m  = mean(value[cat_v==1]),
    m1_sd = sd(value[cat_v==1]),
    mdiff = mean(value[cat_v==1]) - mean(value[cat_v==0]),
    t  = t.test(value[cat_v==1], value[cat_v==0])$statistic, 
    df = t.test(value[cat_v==1], value[cat_v==0])$parameter, 
    p  = t.test(value[cat_v==1], value[cat_v==0])$p.value, 
    bf = 1/extractBF( ttestBF(value[cat_v==1], value[cat_v==0]) )$bf
  ) |> 
  mutate(.by = cat, p.adj = p.adjust(p, "bonferroni")) |> 
  mutate(
    across(contains("p"), ~fmt_APA_numbers(.x,.p=T)),
    across(where(is.double), ~fmt_APA_numbers(.x)),
    name = fct_recode(name, MW="mw", BV="bv", AE="ae"),
    cat = fct_recode(cat, Meditation="meditation1", Music="music_year1"),
    e="", e2="", 
  ) |>
  rename(Variable="name") |>
  gt(groupname_col = "cat") |>
  tab_spanner("No experience", starts_with("m0_")) |>
  tab_spanner("Any experience", starts_with("m1_")) |>
  cols_label(
    ends_with("_m") ~ md("*M*"),
    ends_with("_sd") ~ md("*SD*"),
    t=md("*t*"), df = md("*df*"), p=md("*p*"), p.adj = md("*p*~adj~"), 
    bf = md("BF~01~"), mdiff=md("*M*~diff~"),
    starts_with("e")~""
  ) |>
  cols_move(p.adj, p) |>
  cols_move(e, m0_sd) |>
  cols_move(e2, m1_sd) |>
  cols_align("center")
music_meditation_tbl
if(script_save_tables){
  gtsave(music_meditation_tbl, "tables/meditation_and_musical_experience.docx")
}

### Figure      =====
m_m_data |>
  mutate(
    cat_v = factor(cat_v, label=c("No", "Any")),
    cat = ifelse(cat=="meditation1", "Meditation", "Music"),
    name = fct_recode(name, AE="ae", BV="bv", MW="mw") |> 
      fct_relevel("MW","BV","AE")
  ) |>
  rename(Experience = cat_v) |>
  ggplot(aes(Experience, value, col=Experience)) + 
  facet_wrap(cat~name) +
  stat_summary(fun.data=mean_se) + 
  labs(y="Standardized values") +
  geom_hline(yintercept=0, linetype="dashed") + 
  theme(legend.position = "none")
if(script_save_figures){
  ggsave("figs/exploratory/meditation_and_music.svg")
}

# Post      =====

## Accumulating effects of TMS      =====
# table
accumen_data_tbl <- 
  pfc |>
  mutate(probe1_n = as.integer(probe1)) |>
  select(subj, region, stimulation, block, proberound, zlogapen, zlogbv, probe1_n) |>
  # baseline  (B0) correct
  pivot_longer(c(zlogapen, zlogbv, probe1_n), names_to="name", values_to = "val") |> 
  pivot_wider(names_from = c(block, name), values_from = val) |> 
  mutate(B0_ae=0,
         B1_ae=B1_zlogapen-B0_zlogapen,
         B2_ae=B2_zlogapen-B0_zlogapen,
         B3_ae=B3_zlogapen-B0_zlogapen,
         B0_bv=0,
         B1_bv=B1_zlogbv-B0_zlogbv,
         B2_bv=B2_zlogbv-B0_zlogbv,
         B3_bv=B3_zlogbv-B0_zlogbv,
         B0_mw=0,
         B1_mw=B1_probe1_n-B0_probe1_n,
         B2_mw=B2_probe1_n-B0_probe1_n,
         B3_mw=B3_probe1_n-B0_probe1_n) |> 
  select(-ends_with("zlogapen"), -ends_with("zlogbv"), -ends_with("probe1_n")) |>
  pivot_longer(starts_with("B"), names_to = c("block","variable"), names_sep = "_") |> 
  mutate(variable=fct_recode(variable, AE="ae",BV="bv",MW="mw"),
         variable=factor(variable, levels=c("MW","BV","AE"))) |>
  pivot_wider(names_from=stimulation, values_from=value) |>
  # calculate difference between cond
  summarise(
    .by=c(subj, block, variable), 
    sham = mean(sham),
    real = mean(real),
  ) |>
  mutate(diff = real-sham) |>
  select(-sham, -real) |>
  pivot_wider(names_from=c(block), values_from=diff) |>
  # take the difference between b1-b2 and b2-b3 
  mutate(B2 = B2 - B1, B3 = B3 - B2)  |> 
  pivot_longer(c(B1, B2, B3)) |>
  select(-B0) 


accumen_test_tbl <- 
  accumen_data_tbl |>
  mutate(nill = 0) |>
  summarise(
    .by    = c(variable, name), 
    m   = mean(value), 
    sd  = sd(value), 
    t   = t.test(value, mu=0)$statistic,
    df  = t.test(value, mu=0)$parameter,
    p   = t.test(value, mu=0)$p.value,
    d   = lsr::cohensD(value, nill, method="paired"),
    bf10  = extractBF(ttestBF(value, mu = 0))$bf,
  ) |> mutate(.by = variable, p.adj  = p.adjust(p, "bonferroni")) |>
  mutate(name=case_when(
    name=="B1"~"B1 - B0", 
    name=="B2"~"B2 - B1", 
    name=="B3"~"B3 - B2"),
    across(contains("p"), ~ fmt_APA_numbers(.x, .p=T)), 
    across(where(is.double), ~fmt_APA_numbers(.x))
  ) |> 
  pivot_wider(names_from=variable, values_from=c(m,sd,t,df,p,p.adj, bf10, d)) 
  

# Table
accumen_tbl <- 
  accumen_test_tbl |>
  mutate(e="", e2="") |>
  gt() |>
  tab_spanner("Mind Wandering", ends_with("MW")) |>
  tab_spanner("Behavioural Variability", ends_with("BV")) |>
  tab_spanner("Approximate Entropy", ends_with("AE")) |>
  cols_move(ends_with("MW"), name) |>
  cols_move(ends_with("BV"), bf10_MW) |>
  cols_move(e, bf10_MW) |>
  cols_move(e2, bf10_BV) |>
  cols_label(
    starts_with("m_") ~ md("*M*~diff~"),
    starts_with("sd_") ~ md("*SD*~diff~"),
    starts_with("t_") ~ md("*t*"),
    starts_with("df_") ~ md("*df*"),
    starts_with("p_") ~ md("*p*"),
    starts_with("p.adj_") ~ md("*p*~adj~"),
    starts_with("bf10_") ~ md("BF~10~"),
    starts_with("e") ~ ""
  )

if(script_save_tables){
  gtsave(accumen_tbl, "tables/accumulating effect of the tms.docx")
}


# test vis
accumen_data_tbl |>
  ggplot(aes(name, value)) + 
  facet_wrap(~variable) +
  stat_summary(aes(group=subj), position = position_jitter(.15, seed = 147), alpha =.1) +
  stat_summary(aes(group=subj), geom = "line", position = position_jitter(.15, seed = 147), alpha =.1) +
  stat_summary(col="red")+
  stat_summary(geom="line")

accumen_data_tbl |>
  ggplot(aes(name, value)) + 
  facet_wrap(~variable) +
  stat_summary(col="red")+
  stat_summary(geom="line")
