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

expectations <- list(
  `0` ="No expectation", `1` = "Yes, increase", `2` = "Yes, reduce", 
  `1` ="Yes, but not how", `4` = "Don't know") 

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
    e="",
    name = fct_recode(name, MW="mw", BV="bv",AE="ae")
  ) |>
  rename(Variable="name") |>
  gt() |>
  tab_spanner("Increase", starts_with("i_")) |>
  tab_spanner("Decrease", starts_with("d_")) |>
  tab_spanner("No expectation", starts_with("no_")) |>
  cols_move(e, i_bf01) |>
  cols_label(
    ends_with("_m")~md("*M*"),
    ends_with("_t")~md("*t*"),
    ends_with("_df")~md("*df*"),
    ends_with("_p")~md("*p*"),
    ends_with("_bf01")~md("BF~01~"), 
    ends_with("_sd") ~md("*SD*"),
    ends_with("diff") ~ md("*M*~diff~"),
    e=""
  ) |>
  cols_align("center")
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
## Stimulation guesses     ======
# Fetch the values
guess_table <- demo_pfc |>
  select(contains("FB_stimulation"), contains("researcher"),subj, -contains("conf")) |>
  mutate(across(everything(), as.integer)) |>
  pivot_longer(c(contains("Researcher"), contains("_FB_"))) |>
  mutate(s = ifelse(str_detect(name, "[Ss]1"), "S1", "S2"),
         pers = ifelse(str_detect(name, "Resear"), "Researcher", "Participant"),
  ) |>
  left_join(
    demo_pfc |>
      select(contains("true"), subj) |>
      pivot_longer(contains("true")) |> 
      mutate(name = ifelse(name=="true_stim1", "S1","S2")) |>
      rename(true_stim=value),
    by = join_by("subj"=="subj", "s"=="name" )
  ) |>
  summarise(
    .by = c(s, pers, true_stim),
    val1 = sum(value),
    val0 = sum(value==0)
  ) |> 
  arrange(s, pers, true_stim) |>
  pivot_wider(names_from=pers, values_from = c(val1, val0)) |>
  select(s, true_stim, val0_Participant, val1_Participant, val0_Researcher, val1_Researcher)
guess_table

# Create the table
guess_table_d <- 
  guess_table |>
  add_row(
    guess_table |> 
      pivot_longer(contains("val")) |>
      summarise(
        .by = c(s, name),
        sum = sum(value)
      ) |>
      pivot_wider(names_from = name, values_from = sum) |>
      mutate(true_stim=c(3,3))
  ) |> 
  arrange(s, true_stim) |> 
  mutate(
    par_sum = val0_Participant+val1_Participant,
    res_sum = val0_Researcher+val1_Researcher,
    true_stim = case_when(
      true_stim==1~"True",
      true_stim==0~"False",
      true_stim==3~"Total",)
  ) |>
  mutate(b="") |>
  gt() |>
  tab_spanner("Participants",
              c(val0_Participant,val1_Participant, par_sum) )|>
  tab_spanner("Researcher", 
              c(val0_Researcher, val1_Researcher, res_sum)) |>
  tab_spanner("True state", c(s, true_stim)) |>
  cols_label( val0_Participant = "False",
              val1_Participant = "True",
              val0_Researcher = "False", 
              val1_Researcher = "True", 
              par_sum = "Total", res_sum = "Total",
              b = "", s = "", true_stim="") |>
  cols_move(b, par_sum) 
guess_table_d
if(script_save_tables){
  gtsave(guess_table_d, "tables/Blinding_Guesses_for_Participants_and_Researchers.docx")  
}

### Test predictions         ======
#### Frequent      ======
demo_pfc |>
  select(contains("FB_stimulation"), contains("researcher"),subj, -contains("conf"))  |>
  mutate(across(everything(), as.integer)) |>
  pivot_longer(c(contains("Researcher"), contains("_FB_"))) |>
  mutate(s = ifelse(str_detect(name, "[Ss]1"), "S1", "S2"),
         pers = ifelse(str_detect(name, "Resear"), "Researcher", "Participant"),
         name=NULL,
  ) |>
  left_join(
    demo_pfc |>
      select(contains("true"), subj) |>
      pivot_longer(contains("true")) |> 
      mutate(name = ifelse(name=="true_stim1", "S1","S2")) |>
      rename(true_stim=value),
    by = join_by("subj"=="subj", "s"=="name" )) |>
  summarise(
    .by    = c(s, pers), 
    chisq_s  = chisq.test(value, true_stim)$statistic,
    chisq_p  = chisq.test(value, true_stim)$p.value,
    fisher_p = fisher.test(value, true_stim)$p.value,
  )

#### Bayesian ======
# Fit a Bayesian Poisson model to the observed counts
b_guess_data <- 
  demo_pfc |>
  select(subj, S1_FB_stimulation, S2_FB_stimulation, true_stim1, true_stim2, 
         S1_Researcher_stim, S2_Researcher_stim) |>
  pivot_longer(c(starts_with("S1"),starts_with("S2"))) |>
  mutate(
    split = str_split(name, "_"), 
    session = map_chr(split, 1),
    pers = ifelse(map_chr(split, 2)=="Researcher", "Researcher", "Participant"),
    true_stim = ifelse(session=="S1", true_stim1, true_stim2),
    pred_stim = as.integer(value)
  ) |>
  select(-true_stim1, -true_stim2, -name, -value, -split)

map(c("Participant", "Researcher"), \(per){
  map(c("S1","S2"), \(sess){
    b_guess_data |>
      filter(pers == per & session == sess) -> d
    
    tibble(
      pers = per,
      session = sess,
      bf10 = extractBF(
        contingencyTableBF(
          table(d[["pred_stim"]], d[["true_stim"]]), sampleType = "jointMulti"
        )
      )[["bf"]]
    )
  }) |> list_rbind()
}) |> list_rbind()

## Confidence in guesses       ======
# Fetch confidence values
demo_pfc |> 
  select(contains("conf"), subj, -contains("_task_")) |>
  mutate(across(everything(), as.integer)) |>
  pivot_longer(contains("_")) |>
  mutate(
    split = ifelse(str_detect(name, "conf"), "conf", "true_stim"),
    res = ifelse(str_detect(name, "[Rr]es"), "Researcher", "Participant"),
    session = ifelse(str_detect(name, "[Ss]1"), "S1", "S2")
  )  |>
  left_join(
    demo_pfc |>
      select(subj, contains("true_stim")) |>
      pivot_longer(contains("true_stim")) |>
      mutate(name = ifelse(str_detect(name,"1"), "S1", "S2")) |>
      rename(true_stim = value),
    by = join_by(subj, session == name)
  ) -> r_p_conf

## Accumulating effects of TMS      =====
# table
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
  mutate(B1.b2 = B1 - B2, B2.b3 = B2 - B3)  |>
  # ggplot(aes(B1.b2))+geom_histogram()
  summarise(
    .by    = variable, 
    b1_m   = mean(B1), 
    b1_sd  = sd(B1), 
    b1_t   = t.test(B1, mu=0)$statistic,
    b1_df  = t.test(B1, mu=0)$parameter,
    b1_p   = t.test(B1, mu=0)$p.value,
    b1_bf  = extractBF(ttestBF(B1, mu = 0))$bf,
    b2_m   = mean(B1.b2), 
    b2_sd  = sd(B1.b2), 
    b2_t   = t.test(B1.b2, mu=0)$statistic,
    b2_df  = t.test(B1.b2, mu=0)$parameter,
    b2_p   = t.test(B1.b2, mu=0)$p.value,
    b2_bf  = extractBF(ttestBF(B1.b2, mu = 0))$bf,
    b3_t   = t.test(B2.b3, mu=0)$statistic,
    b3_df  = t.test(B2.b3, mu=0)$parameter,
    b3_p   = t.test(B2.b3, mu=0)$p.value,
    b3_bf  = extractBF(ttestBF(B2.b3, mu = 0))$bf,
  ) |> mutate(
    b2_p.adj  = p.adjust(b2_p, "bonferroni"), 
    b3_p.adj  = p.adjust(b3_p, "bonferroni"), 
  )
