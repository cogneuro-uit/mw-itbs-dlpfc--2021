library(ProjectTemplate)
# migrate.project()
load.project()
library(patchwork)
library(gt)

save_tables <- FALSE

# Demographics        ======
## Age ===== 
demo_pfc |> summarize(
  mean = mean(Age), sd=sd(Age), min=min(Age), max=max(Age))

demo_pfc |>
  ggplot(aes(x=Age))+
  geom_density()
  geom_histogram()

## Gender         ======
demo_pfc |> summarize(
  male = sum(Gender==1),
  female = sum(Gender==0))

## Music          ======
demo_pfc |> summarise(
  min = min(Music_years),
  max = max(Music_years),
  mean = mean(Music_years), 
  sd = sd(Music_years), 
  sum_music_exp = sum( Music_years > 0 ), 
  sum_no_music_exp = sum( Music_years == 0 ) )

demo_pfc |>
  ggplot(aes(Music_years))+
  geom_bar()

## Meditation       ======
demo_pfc |> summarise(
  min = min(Meditation), 
  max = max(Meditation), 
  mean = mean(Meditation), 
  sd = sd(Meditation),
  sum_med_exp = sum(Meditation>1), 
  sum_no_med_exp=sum(Meditation==1))

demo_pfc |>
  ggplot(aes(Meditation))+
  geom_bar()


# Feedback      ======
##  TMS expectation       ========
### Descriptives         =====
stimulation_expectation <- 
  demo_pfc |>
  select(subj, contains("_expectation")) |> 
  pivot_longer(contains("expectation")) |>
  summarise(
    .by = name, 
    `No expectation`   = sum(value==0),
    `Yes, but not how` = sum(value==3),
    `Yes, reduce`      = sum(value==2),
    `Yes, increase`    = sum(value==1),
    `Don't know`       = sum(value==4)
  ) |> 
  pivot_longer(c(everything(), -name), names_to="Expectation") |>
  pivot_wider(names_from = name, values_from = value) |>
  rename(`Session 1` = S1_TMS_expectation, `Session 2` = S2_TMS_expectation ) |>
  mutate(n =  "") |>
  gt() |>
  cols_move(n, after ="Session 1") |>
  cols_label(n = "") -> stimulation_expectation
if(save_tables){
  gtsave(stimulation_expectation, "tables/Stimulation_Expectation.docx")
}

## Guesses     ======
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
if(save_tables){
  # Save it 
  gtsave(guess_table_d, "tables/Blinding_Guesses_for_Participants_and_Researchers.docx")  
}

### Statistic         ======
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
      bf = extractBF(
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
    

### Descriptives       =====
# Mean confidence rating across participant and researcher for real and sham. 
r_p_conf |>
  select(-name) |>
  summarise(
    .by = c(res, true_stim, session),
    m = mean(value),
    sd = sd(value),
  ) |>
  mutate(across(c(m,sd), ~fmt_APA_numbers(.x)), 
         e1="",e2="",e3="") |>
  pivot_wider(names_from=c(res,true_stim), values_from = c(m,sd)) |>
  gt() |>
  tab_spanner("Sham", contains("Participant") & ends_with("_0") ) |>
  tab_spanner("Sham ", contains("Researcher") & ends_with("_0") ) |>
  tab_spanner("Real", contains("Participant") & ends_with("_1") ) |>
  tab_spanner("Real ", contains("Researcher") & ends_with("_1") ) |>
  tab_spanner("Participants", c(e1, contains("Participant") ) ) |>
  tab_spanner("Researchers", c(e3, contains("Researcher") ) ) |>
  cols_move(contains("cher_0"), sd_Participant_1) |>
  cols_label(
    starts_with("m_") ~ md("*M*"),
    starts_with("sd_") ~ md("*SD*"),
    starts_with("e") ~ "",
  ) |>
  cols_move(e1, sd_Participant_0) |>
  cols_move(e2, sd_Participant_1) |>
  cols_move(e3, sd_Researcher_0) 
  # |> gtsave("tables/confidence_in_guesses.docx")

### Tests       =====
#### Real vs. sham (sess & res)     ====
r_p_conf |>
  select(-name) |>
  summarise(
    .by = c(session, res),
    r_m = mean(value[true_stim==1]),
    r_sd = sd(value[true_stim==1]),
    s_m = mean(value[true_stim==0]),
    s_sd = sd(value[true_stim==0]),
    dif = mean(value[true_stim==1] - value[true_stim==0]),
    t = t.test(value[true_stim==1], value[true_stim==0], paired = T)$statistic,
    df = t.test(value[true_stim==1], value[true_stim==0], paired = T)$parameter,
    p = t.test(value[true_stim==1], value[true_stim==0], paired = T)$p.value,
  ) |>
  mutate( p.adj = p.adjust(p, "bonferroni") )

#### S1 v. S2 (stim, res)        =====
r_p_conf |>
  select(-name) |>
  summarise(
    .by = c(true_stim,res),
    s2_m = mean(value[session=="S2"]),
    s2_sd = sd(value[session=="S2"]),
    s1_m = mean(value[session=="S1"]),
    s1_sd = sd(value[session=="S1"]),
    dif = mean(value[session=="S2"] - value[session=="S1"]),
    t = t.test(value[session=="S2"], value[session=="S1"], paired = T)$statistic,
    df = t.test(value[session=="S2"], value[session=="S1"], paired = T)$parameter,
    p = t.test(value[session=="S2"], value[session=="S1"], paired = T)$p.value,
  ) |> mutate( p.adj = p.adjust(p, "bonferroni") )

#### S1 v. S2 (pers)        ======
r_p_conf |>
  select(-name, -true_stim) |> 
  pivot_wider(names_from = c(session), values_from=value) |>
  summarise(
    .by = res,
    s1_m = mean(S1),
    s1_sd = sd(S1),
    s2_m = mean(S2),
    s2_sd = sd(S2),
    dif = mean(S2 - S1, na.rm=T),
    t = t.test(S2, S1, paired = T)$statistic,
    df = t.test(S2, S1, paired = T)$parameter,
    p = t.test(S2, S1, paired = T)$p.value,
  ) |> mutate(p.adj = p.adjust(p, "bonferroni") )
  # INCREASED OVER SESSION

#### Real v. sham (pers)         =====
r_p_conf |>
  select(-name, -session) |>
  pivot_wider(names_from=true_stim, values_from = value) |>
  summarise(
    .by = res,
    s_m = mean(`0`),
    s_sd = sd(`0`),
    r_m = mean(`1`),
    r_sd = sd(`1`),
    dif = mean(`1` - `0`, na.rm=T),
    t = t.test(`1`, `0`, paired = T)$statistic,
    df = t.test(`1`, `0`, paired = T)$parameter,
    p = t.test(`1`, `0`, paired = T)$p.value,
  ) |> mutate(p.adj = p.adjust(p, "bonferroni") )

## Change prediction?       ======
demo_pfc |>
  select(S2_FB_change_answer) |> 
  summarise(
    `No`       = sum(S2_FB_change_answer==3, na.rm=T),
    `Not sure` = sum(S2_FB_change_answer==2, na.rm=T),
    `Yes`      = sum(S2_FB_change_answer==1, na.rm=T),
  )

### test the changes      ====
demo_pfc |>
  select(contains("stimulation"), -contains("confidence"), contains("true_stim"), S2_FB_change_answer) |>
  mutate(across(where(is.numeric), as.integer),
         S1_FB_stimulation = case_when(
           S2_FB_change_answer==3 & S1_FB_stimulation == 1 ~ 0,
           S2_FB_change_answer==3 & S1_FB_stimulation == 0 ~ 1,
           T ~ S1_FB_stimulation
         )) -> change_guesses
# freq
change_guesses |>
  summarise(
    f.p = fisher.test(S1_FB_stimulation, true_stim1)$p.value,
    chi.t = chisq.test(S1_FB_stimulation, true_stim1)$statistic,
    chi.p = fisher.test(S1_FB_stimulation, true_stim1)$p.value,
  )
# bayes
extractBF(
  contingencyTableBF(
    table(change_guesses$S1_FB_stimulation, change_guesses$true_stim1),
    sampleType = "jointMulti"
  )
)[["bf"]]



# Descriptives - Tired, motivation, conf, tracker     =====
# prepare data: 
data_fb_coll <- 
  demo_pfc |> 
  select(subj, S1_FB_task_confidence, S2_FB_task_confidence, 
         S1_FB_motivation, S2_FB_motivation,
         S1_FB_subject_tracker, S2_FB_subject_tracker,
         S1_Tired, S2_Tired,
         true_stim1, true_stim2) |>
  mutate(across(matches("S[12]"), ~as.integer(.x))) |>
  pivot_longer(c(starts_with("S1"), starts_with("S2"))) |>
  rowwise() |>
  mutate(
    split = str_split(name,"_"),
    names = case_when(
      length(split) == 2 ~ nth(split,2),
      length(split) == 3 ~ nth(split,3),
      length(split) == 4 ~ paste(nth(split,3), nth(split,4)),
    ),
    session = str_split(name, "_") |> map_chr(1),
    stim = ifelse(session=="S1", true_stim1, true_stim2)
  ) |>  ungroup()  |>
  mutate(names = case_when(
    names=="task confidence" ~ "Attention confidence",
    names=="motivation" ~ "Randomness motivation", 
    names=="subject tracker" ~ "Subject tracker", 
    T~names
  )) |>
  select(-name, - split, -true_stim1, -true_stim2) 

combined_fb_data <- 
  pfc_anova_data |>
  mutate(.before=1, subj2 = as.integer( str_split(subj, "PFC") |> map_chr(2) ) ) |>
  summarise(
    .by = c(subj2, stimulation),
    MW = mean(MW1),
    BV = mean(BV),
    AE = mean(AE),
  ) |>
  left_join(data_fb_coll |>
              mutate(stimu = ifelse(stim==0, "sham","real")),
            # pivot_wider(names_from=names, values_from=value), 
            by = join_by(subj2==subj, stimulation==stimu)) |>
  mutate(stimulation = factor(stimulation, levels = c("sham","real")))
  
## Test - conditions       =====
combined_fb_data |>
  summarise(
    .by = names,
    r_m  = mean(value[stimulation=="real"]),
    r_sd = sd(value[stimulation=="real"]),
    s_m  = mean(value[stimulation=="sham"]),
    s_sd = sd(value[stimulation=="sham"]),
    dif = mean(value[stimulation=="real"] - value[stimulation=="sham"]),
    t = t.test(value[stimulation=="real"], value[stimulation=="sham"], paired = T)$statistic,
    df = t.test(value[stimulation=="real"], value[stimulation=="sham"], paired = T)$parameter,
    p = t.test(value[stimulation=="real"], value[stimulation=="sham"], paired = T)$p.value,
  ) |>
  mutate(
    p.adj = p.adjust(p, "bonferroni"),
    across(starts_with("p"), ~fmt_APA_numbers(.x, .p=T)),
    across(where(is.double), ~fmt_APA_numbers(.x))
  ) |> rename_with(~paste0("rs_",.x)) -> fb_tabel_rs

## Test - session          =====
combined_fb_data |>
  summarise(
    .by = names,
    s1_m  = mean(value[session=="S1"]),
    s1_sd = sd(value[session=="S1"]),
    s2_m  = mean(value[session=="S2"]),
    s2_sd = sd(value[session=="S2"]),
    dif = mean(value[session=="S1"] - value[session=="S2"]),
    t = t.test(value[session=="S1"], value[session=="S2"], paired = T)$statistic,
    df = t.test(value[session=="S1"], value[session=="S2"], paired = T)$parameter,
    p = t.test(value[session=="S1"], value[session=="S2"], paired = T)$p.value,
  ) |> 
  mutate(
    p.adj = p.adjust(p, "bonferroni"),
    across(starts_with("p"), ~fmt_APA_numbers(.x, .p=T)),
    across(where(is.double), ~fmt_APA_numbers(.x))
  ) |>
  rename_with(~paste0("se_",.x)) |>
  mutate(e="", e2="",e3="", e4="",e5="") |>
  left_join(fb_tabel_rs, by = join_by(se_names==rs_names)) |>
  gt() |>
  tab_spanner("Session 1", starts_with("se_s1")) |>
  tab_spanner("Session 2", starts_with("se_s2")) |>
  tab_spanner("Session", c(starts_with("se_"),e2, e3)) |>
  tab_spanner("Sham", starts_with("rs_s")) |>
  tab_spanner("Real", starts_with("rs_r")) |>
  tab_spanner("Stimulation", c(starts_with("rs_"),e4,e5)) |>
  cols_move(e2, se_s1_sd) |>
  cols_move(e3, se_s2_sd) |>
  cols_move(e4, rs_r_sd) |>
  cols_move(e5, rs_s_sd) |>
  cols_label(
    ends_with("m") ~ md("*M*"), ends_with("sd") ~ md("*SD*"),
    ends_with("dif") ~ md("*M*~diff~"), ends_with("_t") ~ md("*t*"), 
    ends_with("df") ~ md("*df*"), ends_with("_p") ~ md("*p*"), 
    ends_with("p.adj") ~ md("*p*~adj~"), starts_with("e") ~ ""
  ) |>
  gtsave("tables/pre-fb-sheet-diff-cond-and-session.docx")

  

##  Test relationships     =====
### Motivation      ======
lmer(MW ~ value*stimulation + (1|subj2), combined_fb_data |> filter(names=="motivation")) |> summary()
lm(MW ~ value*stimulation, combined_fb_data |> filter(names=="motivation")) |> summary()
lm(BV ~ value*stimulation, combined_fb_data |> filter(names=="motivation")) |> summary()
lm(AE ~ value*stimulation, combined_fb_data |> filter(names=="motivation")) |> summary()
#' Randomness motivation generally influenced MW, but did not interact with stimulation.
#' It did NOT however affect BV or AE
lm(value ~ stimulation, combined_fb_data |> filter(names=="motivation")) |> summary()

# raw is skewed (cannot be used in out model?)
combined_fb_data |> 
  filter(names=="motivation") |>
  mutate(value = scale(value)) |>
  ggplot(aes(value)) + geom_histogram()


# diff is normal 
combined_fb_data |> 
  filter(names=="motivation") |> 
  select(-session, - stim) |>
  pivot_wider(names_from = stimulation, values_from=c(MW,BV,AE,value)) |> 
  mutate(m_dif = value_real - value_sham) |> 
  ggplot(aes(m_dif))+geom_histogram()



### Tired         ======
lm(MW ~ value*stimulation, combined_fb_data |> filter(names=="Tired")) |> summary()
lm(BV ~ value*stimulation, combined_fb_data |> filter(names=="Tired")) |> summary()
lm(AE ~ value*stimulation, combined_fb_data |> filter(names=="Tired")) |> summary()
#' Tiredness generally increase MW 


### Tracker       ======
lm(MW ~ value*stimulation, combined_fb_data |> filter(names=="subject tracker")) |> summary()
lm(BV ~ value*stimulation, combined_fb_data |> filter(names=="subject tracker")) |> summary()
lm(AE ~ value*stimulation, combined_fb_data |> filter(names=="subject tracker")) |> summary()
#' No influence 


###  Attention confidence       ======
lm(MW ~ value*stimulation, combined_fb_data |> filter(names=="attention confidence")) |> summary()
lm(BV ~ value*stimulation, combined_fb_data |> filter(names=="attention confidence")) |> summary()
lm(AE ~ value*stimulation, combined_fb_data |> filter(names=="attention confidence")) |> summary()





#   Adverse effects:          ======
demo_pfc |>
  select(contains("Checklist"), -S2_Checklist_comments, -S1_Checklist_comments, subj,
         -S1_Checklist_other_specify) |> 
  pivot_longer( c(everything(), -subj), ) |> 
  mutate( session = str_split(name, "_" ) |> map_chr(1),
          name = str_split( name, "_Checklist_" ) |> map_chr(2)) |> 
  select( subj, session, name, value ) |> #-> tms_checklist
  # Add stimulation type
  # tms_checklist |>
  left_join(demo_pfc |> 
              select(subj, true_stim1, true_stim2) |> 
              pivot_longer(c(true_stim1, true_stim2), 
                           names_to = "session", values_to = "stimulation") |>
              mutate(session = ifelse(session=="true_stim1", "S1", "S2")), 
            by = join_by(subj, session)) |>  #-> tms_checklist_stim
  select(-session) |>
  pivot_wider(names_from = stimulation, values_from = value) |>
  # Transforme NA to 1 (assumed)
  # If this is used ????
  mutate(`0` = ifelse(`0` %in% c(NA), 1, `0`),
         `1` = ifelse(`1` %in% c(NA), 1, `1`)) ->
  tms_checklist_stim_diff

# Rating of adverse outcomes (general)
tms_checklist_stim_diff |> 
  filter(!str_detect(name, "_TMS")) |> 
  group_by(name) |>
  summarize(
    t = t.test(`0`, `1`, paired=T)$statistic,
    df = t.test(`0`, `1`, paired=T)$parameter,
    p = t.test(`0`, `1`, paired=T)$p.value,
    sham_m = mean(`0`, na.rm = T),
    sham_sd = sd(`0`, na.rm = T),
    real_m = mean(`1`, na.rm = T),
    real_sd = sd(`1`, na.rm = T),
    diff_m = mean(sham_m - real_m, na.rm = T),
    ) ->
  #diff_sd = sd(sham_m - real_m, na.rm = T)
  checklist_outcomes

  # tms_checklist_stim_diff |> 
  #   filter(!str_detect(name, "_TMS")) |> 
  #   group_by(name) |>
  #   filter(name=="Neck_pain") |> View()
  # na.omit() |>
  #   summarise(
  #     t = t.test(`0`, `1`, paired=T)$statistic,
  #     p = t.test(`0`, `1`, paired=T)$p.value,
  #   )
  
# Adverse outcome related to TMS stimulation?
tms_checklist_stim_diff |> 
  filter(str_detect(name, "_TMS")) |> 
  group_by(name) |>
  summarize(
    r_t = t.test(`0`, `1`, paired=T)$statistic,
    r_t = t.test(`0`, `1`, paired=T)$parameter,
    r_p = t.test(`0`, `1`, paired=T)$p.value,
    sham_m_r_t = mean(`0`, na.rm = T),
    sham_sd_r_t = sd(`0`, na.rm = T),
    #    sh_max = max(`0`, na.rm=T),
    # Sham max? 
    real_m_r_t = mean(`1`, na.rm = T),
    real_sd_r_t = sd(`1`, na.rm = T),
    m_diff = mean(sham_m_r_t - real_m_r_t)) |>
  # Rename to join together
  mutate( name = str_split(name, "_TMS") |> map_chr(1) ) ->
  checklist_o_related_tms


## Generate table      =====
full_join(checklist_outcomes, checklist_o_related_tms, by=join_by(name)) |>
  #select(c(everything(), real_m_r_t, real_sd_r_t)) |> View()
  mutate(name = str_replace_all(name, "_", " "),
         empty1="",
         empty2="",
         empty3="",
         empty4="",
         empty5="",) |> 
  rename(Symptom = name) |> 
  # GT table
  gt() |> 
  opt_table_font(font = c("san-serif", "Times New Roman")) |>
  fmt_number() |>
  cols_align("center", c(2:13)) |>
  cols_move(empty1, sham_sd) |>
  cols_move(empty2, real_sd) |>
  cols_move(empty3, diff_m) |>
  cols_move(empty4, sham_sd_r_t) |>
  cols_move(empty5, real_sd_r_t) |>
  tab_spanner(
    "Sham",
    c(sham_m, sham_sd) ) |> 
  tab_spanner(
    "Real",
    c(real_m, real_sd) ) |> 
  tab_spanner(
    "Sham", id = "Sham2",
    c(sham_m_r_t, sham_sd_r_t) ) |> 
  tab_spanner(
    "Real", id = "Real2",
    c(real_m_r_t, real_sd_r_t) ) |> 
  tab_spanner(
    "Symptom report",
    c(sham_m, sham_sd, real_m, real_sd, diff_m, empty1, empty2) ) |>
  tab_spanner(
    "Relation to TMS",
    c(sham_m_r_t, sham_sd_r_t, real_m_r_t, real_sd_r_t, m_diff, empty4, empty5) ) |>
  tab_footnote(md("Note. Symptom reports  are reported using four categories:\
                  (1) absent, (2) mild, (3) moderate, and (4) severe.\
                  The relation of the symptom to the TMS are answered using 5 categories: \
                  (1) no, (2) unlikely, (3) possible, (4) probable, and (5) definitively.")) |> 
  tab_header(md("**TMS adverse outcomes**")) |>
  cols_label(
    sham_m = md("M"),
    sham_sd = md("SD"),
    real_m = md("M"),
    real_sd = md("SD"),
    diff_m = md("Mdiff"),
    sham_m_r_t = md("M"),
    sham_sd_r_t = md("SD"),
    real_m_r_t = md("M"),
    real_sd_r_t = md("SD"),
    m_diff = md("Mdiff"),
    empty1 = "",
    empty2 = "",
    empty3 = "",
    empty4 = "",
    empty5 = "") |>
  tab_options(
    # hide the top-most border
    table.border.top.color = "white",
    # make the title size match the body text
    heading.title.font.size = px(16),
    # change the column labels section
    column_labels.border.top.width = 3,
    column_labels.border.top.color = "black", 
    column_labels.border.bottom.width = 3,
    column_labels.border.bottom.color = "black",
    # change the bottom of the body
    table_body.border.bottom.color = "black",
    # hide the bottom-most line or footnotes
    # will have a border
    table.border.bottom.color = "white",
    # make the width 100%
    table.width = pct(100),
    table.background.color = "white"
  ) |>
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
    #do this for all columns and rows
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )) # |> # only save when done
gtsave("tables/adverse_outcomes_table-V2.docx")

##  T-tests     ======
t_tests <- tibble()
for(x in unique(tms_checklist_stim_diff$name)){
  print(x)
  t.test(tms_checklist_stim_diff$`0`[tms_checklist_stim_diff$name == x],
         tms_checklist_stim_diff$`1`[tms_checklist_stim_diff$name == x], 
         paired = T) -> test
  print(tms_checklist_stim_diff$`0`[tms_checklist_stim_diff$name == x])
  print(tms_checklist_stim_diff$`1`[tms_checklist_stim_diff$name == x])
  rbind(t_tests, tibble(var = x,
                        df = test$parameter, 
                        t = test$statistic,
                        p = test$p.value)) -> t_tests
}

length(unique(tms_checklist_stim_diff$name))

t_tests |>
  mutate(
    adjust_p = p.adjust(p, method = "bonferroni")) ->
  t_tests

t_tests |>
  filter(!str_detect(var, "_TMS")) |>
  mutate(symptom_corr_p = p.adjust(p, method = "bonferroni"))
t_tests |>
  filter(str_detect(var, "_TMS")) |>
  mutate(tms_corr_p = p.adjust(p, method = "bonferroni"))



##  Figures    ======

# Symptom report
p1 <-
tms_checklist_stim_diff |> 
  filter(!str_detect(name, "_TMS")) |>
  mutate( name = str_replace_all(name, "_TMS", ""),
          name = str_replace_all(name, "_", " "),
          name = factor(name, levels = c(
            "other",
            "Trouble concentrating",
            "Tingling",
            "Sudden mood change",
            "Sleepiness",
            "Skin redness",
            "Scalp pain",
            "Neck pain",
            "Itching",
            "Headache",
            "Burning sensation"), ordered = T)) |>
  rename(Sham = `0`, Real = `1`) |>
  pivot_longer(c(Sham, Real), names_to = "stim") |>
  ggplot(aes(value, name, group = stim)) +
  facet_wrap( ~ stim) + 
  geom_point(position = position_jitter(width = 0.1, height = 0.3), alpha =.25) +
  stat_summary(fun.data = mean_se, col="red", position = position_nudge()) +
  scale_x_continuous(breaks = 1:5, 
                     labels = c("(1) No ", "(2) Unlikely", "(3) Possible", "(4) Probable", "(5) Definitively")) +
  labs(y="Symptom", x = "", title = "A) Side-effect report")
#ggsave("figs/TMS_Checklist/symptom_report.svg", p1,  height = 5, width = 10)

# Relation to TMS
p2 <-
  tms_checklist_stim_diff |> 
  filter(str_detect(name, "_TMS")) |>
  mutate( name = str_replace_all(name, "_TMS", ""),
          name = str_replace_all(name, "_", " "),
          name = factor(name, levels = c(
            "other",
            "Trouble concentrating",
            "Tingling",
            "Sudden mood change",
            "Sleepiness",
            "Skin redness",
            "Scalp pain",
            "Neck pain",
            "Itching",
            "Headache",
            "Burning sensation"), ordered = T)) |>
  rename(Sham = `0`, Real = `1`) |>
  pivot_longer(c(Sham, Real), names_to = "stim") |>
  ggplot(aes(value, name, group = stim)) +
  facet_wrap( ~ stim) + 
  geom_point(position = position_jitter(width = 0.1, height = 0.3), alpha =.25) +
  stat_summary(fun.data = mean_se, col="red", position = position_nudge()) +
  scale_x_continuous(breaks = 1:5, 
                     labels = c("(1) No ", "(2) Unlikely", "(3) Possible", "(4) Probable", "(5) Definitively")) +
  labs(y="Symptom", x = "Response", title = "B) Relation to TMS")
#ggsave("figs/TMS_Checklist/relation_to_tms.svg", p2, height = 5, width = 10)

library(patchwork)
p1+p2+plot_layout(nrow = 2)
ggsave("figs/TMS_Checklist/full.png", height=10, width=12)



p3 <-""
  tms_checklist_stim_diff |> 
  rename(Sham = `0`, Real = `1`) |>
  rowwise() |> mutate(inter = Sham * Real) |> ungroup() |>
  mutate(TMS = ifelse(str_detect(name, "_TMS"), "TMS", "Effect"), 
         name = str_replace_all(name, "_TMS", ""),
         name = str_replace_all(name, "_", " "),
         name = factor(name, levels = c(
           "other",
           "Trouble concentrating",
           "Tingling",
           "Sudden mood change",
           "Sleepiness",
           "Skin redness",
           "Scalp pain",
           "Neck pain",
           "Itching",
           "Headache",
           "Burning sensation"), ordered = T)) |>
  # pivot_wider(names_from = TMS, values_from=c("Sham", "Real", "inter")) 
  pivot_longer(c("Sham", "Real", "inter"), names_to ="stim") -> pl_dat
pl_dat |>
  filter(!(stim=="inter")) |>
  ggplot(aes(value, name, group = stim, col=interaction(TMS,stim))) +
  facet_wrap(TMS ~ stim) + 
  geom_point(
    data = pl_dat |> filter(TMS=="Effect"), 
    aes(y = name,),
    position = position_jitter(.1,.1)
  )


position_jitterdodge(dodge)
  



# Ridges (not finished))
library(ggridges)
tms_checklist_stim_diff |>
  filter(str_detect(name, "_TMS")) |> 
  mutate(name = factor(name)) |>
  ggplot(aes(y=name, x=`0`))+
  geom_density_ridges(scale = .7, jittered_points = T, 
                      position = position_points_jitter(width = .1)) + 
  scale_x_continuous(breaks = 1:5)
geom_point(position = position_(.01, dodge.width =  0))
