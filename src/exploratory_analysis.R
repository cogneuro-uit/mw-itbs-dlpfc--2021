library(ProjectTemplate)
load.project()

# Exploratory:            ======
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
  tab_spanner("No", starts_with("m0_")) |>
  tab_spanner("Any", starts_with("m1_")) |>
  tab_spanner("Experience", c(matches("m[01]"),"e")) |>
  cols_label(
    ends_with("_m") ~ md("*M*"),
    ends_with("_sd") ~ md("*SD*"),
    t=md("*t*"), df = md("*df*"), p=md("*p*"), p.adj = md("*p*~adj~"), 
    bf = md("BF~01~"), mdiff=md("*M*~diff~"),
    starts_with("e")~""
  ) |>
  cols_move(p.adj, p) |>
  cols_move(e, m0_sd) |>
  cols_move(e2, m1_sd) 
  
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
  geom_hline(yintercept=0, linetype="dashed")


## Test accumulating effects of TMS      =====
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
