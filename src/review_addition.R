library(ProjectTemplate)
load.project()


# Test the "accumulating" effect. 
pfc_t |> 
  summarise(
    .by = c(subj, stimulation, block, variable), 
    value = mean(value)
  ) |>
  pivot_wider(names_from = block, values_from = value) |> 
  mutate( 
    d_b0_b1 = B0 - B1,
    d_b1_b2 = B1 - B2,
    d_b2_b3 = B2 - B3,
  ) |>
  select(-starts_with("B")) |>
  pivot_wider(names_from=stimulation, values_from=starts_with("d_")) |>
  mutate(
    diff_b1 = d_b0_b1_real - d_b0_b1_sham,
    diff_b2 = d_b1_b2_real - d_b1_b2_sham,
    diff_b3 = d_b2_b3_real - d_b2_b3_sham,
  ) |>
  select(-starts_with("d_")) |>
  pivot_longer(c(diff_b1, diff_b2, diff_b3)) |> 
  summarise(
    .by=c(variable,name), 
    t = t.test(value, mu = 0)$statistic,
    df = t.test(value, mu = 0)$parameter,
    p = t.test(value, mu = 0)$p.value,
  )
  pivot_longer(c(d_b0_b1, d_b1_b2, d_b2_b3)) |>
  summarise(
    .by = c(variable, name),
    t = t.test(d_b0_b1, d_b1_b2, paired = T)$statistic,
    df = t.test(d_b0_b1, d_b1_b2, paired = T)$parameter,
    p = t.test(d_b0_b1, d_b1_b2, paired = T)$p.value,
  )
  







# Full model  (BV*AE)          =====
mod.pfc.mw.behav <- brm(probe1 ~ stimulation + block*stimulation + zlogapen * zlogbv + scale(proberound) + (1|subj), 
                        init=0, family=cumulative("probit"), data=pfc, 
                        backend = "cmdstanr", chains = 6, iter=3000)
bayes_plot(mod.pfc.mw.behav) + labs(title="Full model") -> p_mw1
p_mw1

# Add LOO criteria 
mod.pfc.mw |> add_criterion(criterion = c("loo","bayes_R2")) -> mod.pfc.mw


##  MW, BV, AE model plot        =====
as.data.frame(mod.pfc.mw2) |> 
  select(starts_with("b_block"), starts_with("b_stimulation")) |> 
  rename_with(~paste0("mw_", .x)) |> 
  # combine:
  cbind( as.data.frame(mod.pfc.bv) |> 
           select(starts_with("b_block"), starts_with("b_stimulation")) |>
           rename_with(~paste0("bv_", .x)) ) |>
  cbind( as.data.frame(mod.pfc.ae) |>
           select(starts_with("b_block"), starts_with("b_stimulation")) |> 
           rename_with(~paste0("ae_", .x)) )  |> #View()
  # Transform
  mutate(B0_mw_sham=0,
         B0_mw_real=0,
         B1_mw_sham=mw_b_blockB1,
         B1_mw_real=mw_b_blockB1+`mw_b_blockB1:stimulationreal`,
         B2_mw_sham=mw_b_blockB2,
         B2_mw_real=mw_b_blockB2+`mw_b_blockB2:stimulationreal`,
         B3_mw_sham=mw_b_blockB3,
         B3_mw_real=mw_b_blockB3+`mw_b_blockB3:stimulationreal`) |>
  mutate(B0_bv_sham=0,
         B0_bv_real=0,
         B1_bv_sham=bv_b_blockB1,
         B1_bv_real=bv_b_blockB1+`bv_b_stimulationreal:blockB1`,
         B2_bv_sham=bv_b_blockB2,
         B2_bv_real=bv_b_blockB2+`bv_b_stimulationreal:blockB2`,
         B3_bv_sham=bv_b_blockB3,
         B3_bv_real=bv_b_blockB3+`bv_b_stimulationreal:blockB3`) |>
  mutate(B0_ae_sham=0,
         B0_ae_real=0,
         B1_ae_sham=ae_b_blockB1,
         B1_ae_real=ae_b_blockB1+`ae_b_stimulationreal:blockB1`,
         B2_ae_sham=ae_b_blockB2,
         B2_ae_real=ae_b_blockB2+`ae_b_stimulationreal:blockB2`,
         B3_ae_sham=ae_b_blockB3,
         B3_ae_real=ae_b_blockB3+`ae_b_stimulationreal:blockB3`) |>
  select(starts_with("B", ignore.case = F)) |>
  #gather(var, val) 
  pivot_longer( everything() ) |>
  separate_wider_delim(name, delim = "_", names = c("block", "var", "stimulation")) |>
  mutate(var = case_when(var == "mw"~"MW",
                         var == "bv"~"BV",
                         var == "ae"~"AE"),
         var = factor(var, levels = c("MW", "BV", "AE")),
         stimulation = factor( stimulation, levels = c("sham", "real"))) |>
  ggplot(aes( x = block, y = value, color = stimulation ))+
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(y  = "") +
  stat_summary(fun.data=mean_qi, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))+
  scale_y_continuous( breaks = seq(-1,1, 0.1) ) +
  facet_wrap( ~ var )
ggsave("figs/prereg/bayesian-model_MW-BV-AE+block+stim.png", dpi=300, width=10, height=4)



# CTRL MOTIVATION       =====
data_task_mot <- 
  demo_pfc |> 
  select(subj, S1_FB_motivation, S2_FB_motivation,true_stim1, true_stim2) |>
  pivot_longer(ends_with("motivation")) |>
  mutate(
    stim = ifelse( 
      str_detect( str_split(name, "_") |> map_chr(1), pattern="S1"), 
      true_stim1, true_stim2 
    ),
    session = str_split(name, "_") |> map_chr(1), 
    value = as.integer(value),
    subj  = as.integer(subj),
  ) |>
  select(-starts_with("true"), -name)
data_task_tired <- 
  demo_pfc |> 
  select(subj, S1_FB_task_confidence, S2_FB_task_confidence, true_stim1, true_stim2) |>
  pivot_longer(ends_with("confidence")) |>
  mutate(
    stim = ifelse( 
      str_detect( str_split(name, "_") |> map_chr(1), pattern="S1"), 
      true_stim1, true_stim2 
    ),
    session = str_split(name, "_") |> map_chr(1), 
    value = as.integer(value),
    subj  = as.integer(subj),
  ) |>
  select(-starts_with("true"), -name)


pfc |>
  mutate(
    subj2 = as.integer( str_split(subj, "PFC") |> map_chr(2))
  ) |>
  left_join(data_task_tired, by = join_by(subj2==subj, session)) -> pfc_2

mod.pfc.mw_cnt <- brm(probe1 ~ block*stimulation + value + scale(proberound) + (1|subj), 
                      init=0, family=cumulative("probit"), data=pfc_2, 
                      backend = "cmdstanr", chains = 6, iter=3000)
bayes_plot(mod.pfc.mw_cnt)
summary(mod.pfc.mw_cnt)




#  TEST MEDITATION/MUSIC        =====
## Combined meditation and music  =====
brm(
  zlogbv ~ stimulation*block + music_year1 + meditation1 + scale(proberound) + (1|subj), data = pfc,
  chains = 6, iter=4000, cores=6, init=0, backend="cmdstanr"
) -> exp_mus_med_mod
bayes_plot(exp_mus_med_mod)

exp_mus_med_mod |>
  as_tibble() |> 
  pivot_longer(c(b_music_year1, b_meditation1)) |> 
  summarise(
    .by = name, 
    m = mean(value),
    hdi = paste0("[", fmt_APA_numbers(hdi(value)[1]),
                 ", ", fmt_APA_numbers(hdi(value)[2]), "]"),
    er = mean(sum(value<0) / sum(value>=0)),
    p  = sum(value<0) / length(value),
  )


## Music  =====
#bv
brm(
  zlogbv ~ stimulation*block + music_year1 + scale(proberound) + (1|subj), data = pfc,
  chains = 6, iter=4000, cores=6, init=0, backend="cmdstanr"
) -> exp_mus_mod1
bayes_plot(exp_mus_mod1)

exp_mus_mod1 |>
  as_tibble() |>
  summarise(
    m = mean(b_music_year1),
    hdi = hdi(b_music_year1),
    er = mean(sum(b_music_year1<0) / sum(b_music_year1>=0))
  )

#ae
brm(
  zlogapen ~ stimulation*block + meditation1 + scale(proberound) + (1|subj), data = pfc,
  chains = 6, iter=4000, cores=6, init=0, backend="cmdstanr"
) -> exp_med_mod3
bayes_plot(exp_med_mod3)

exp_med_mod3 |> 
  as_tibble() |>
  summarise(
    m = mean(b_meditation1),
    hdi = hdi(b_meditation1),
    er = mean(sum(b_meditation1<0) / sum(b_meditation1>=0))
  )

#bv
brm(
  zlogbv ~ stimulation*block + meditation1 + scale(proberound) + (1|subj), data = pfc,
  chains = 6, iter=4000, cores=6, init=0, backend="cmdstanr"
) -> exp_med_mod
bayes_plot(exp_med_mod)

exp_med_mod |>
  as_tibble() |>
  summarise(
    m = mean(b_meditation1),
    hdi = hdi(b_meditation1),
    er = mean(sum(b_meditation1<0) / sum(b_meditation1>=0))
  )

#mw
brm(
  probe1 ~ stimulation*block + meditation1 + scale(proberound) + (1|subj), data = pfc,
  family=cumulative("probit"), chains = 6, iter=4000, cores=6, init=0, backend="cmdstanr"
) -> exp_med_mod2
bayes_plot(exp_med_mod2)

exp_med_mod |>
  as_tibble() |>
  summarise(
    m = mean(b_meditation1),
    hdi = hdi(b_meditation1),
    er = mean(sum(b_meditation1<0) / sum(b_meditation1>=0))
  )

# Test accumulating effect      ====
##  Mind wandering        =====
mod.pfc.mw |>
  as_tibble() |>
  select(matches("B[123]")) |> 
  mutate(
    .before=1,
    B1   = `b_stimulationreal:blockB1`,
    B2_B1 = `b_stimulationreal:blockB2` - `b_stimulationreal:blockB1`,
    B3_B2 = `b_stimulationreal:blockB3` - `b_stimulationreal:blockB2`,
  ) -> mw_mod_diff

mw_mod_diff |>
  pivot_longer(c(B1, B2_B1, B3_B2)) |>
  ggplot(aes(name, value)) +
  stat_summary()

mw_mod_diff |>
  pivot_longer(c(B2_B1, B3_B2)) |>
  summarise(
    .by = name,
    m = mean(value),
    hdi = bay_hdi(value),
    er = bay_er(value),
    p = bay_p(value)
  )

##  Behavioural variability        =====
mod.pfc.bv |>
  as_tibble() |>
  select(matches("B[123]")) |> 
  mutate(
    .before=1,
    B1   = `b_stimulationreal:blockB1`,
    B2_B1 = `b_stimulationreal:blockB2` - `b_stimulationreal:blockB1`,
    B3_B2 = `b_stimulationreal:blockB3` - `b_stimulationreal:blockB2`,
  ) -> bv_mod_diff

bv_mod_diff |>
  pivot_longer(c(B2_B1, B3_B2)) |>
  ggplot(aes(name, value)) +
  stat_summary()

bv_mod_diff |>
  pivot_longer(c(B2_B1, B3_B2)) |>
  summarise(
    .by = name,
    m = mean(value),
    hdi = bay_hdi(value),
    er = bay_er(value),
    p = bay_p(value)
  )
mod.pfc.bv |>
  as_tibble() |>
  pivot_longer(matches("real:blockB[123]")) |>
  ggplot(aes(name,value))+
  stat_summary()

##  Approximate Entropy        =====
mod.pfc.ae |>
  as_tibble() |>
  select(matches("B[123]"), ends_with("real")) |> 
  mutate(
    .before=1,
    B2_B1 = `b_stimulationreal:blockB2` - `b_stimulationreal:blockB1`,
    B3_B2 = `b_stimulationreal:blockB3` - `b_stimulationreal:blockB2`,
  ) -> ae_mod_diff

ae_mod_diff |>
  pivot_longer(c(B2_B1, B3_B2)) |>
  ggplot(aes(name, value)) +
  stat_summary()

ae_mod_diff |>
  pivot_longer(c(B2_B1, B3_B2)) |>
  summarise(
    .by = name,
    m = mean(value),
    hdi = bay_hdi(value),
    er = bay_er(value),
    p = bay_p(value)
  )

mod.pfc.ae |>
  as_tibble() |>
  pivot_longer(matches("real:blockB[123]")) |>
  ggplot(aes(name,value))+
  stat_summary()
