library(ProjectTemplate)
# migrate.project() # you might need to run this. 
load.project()



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Toggles           =====
script_load_bayesian_data <- TRUE 
  #' TRUE will load the saved Bayesian models in (paper_vars) 
  #' FALSE will NOT load any Bayesian models, and will therefore RUN all Bayesian models. 

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# Bayesian plotting function
bayes_plot <- function( data_list, variables = NULL ){
  int <- variables(data_list)[str_detect(variables(data_list), "Intercept")]
  remove_list <- c(c("disc","lp__", "lprior"), variables,int)
  gpars <- setdiff(variables(data_list), remove_list)
  brms::rhat(data_list) -> b
  
  if(max(b, na.rm=T) > 1.01){
    print(paste("max:", max(b, na.rm=T), " <- High, check model"))
  } else {
    print(paste("max:", max(b, na.rm=T)))
  }
  print(paste("mean:", mean(b, na.rm=T)))
  print(paste("median:", median(b, na.rm=T)))
  
  mcmc_intervals(as.matrix(data_list), pars=gpars, prob_outer = 0.95) + 
    geom_vline(xintercept = 0, linetype = "dashed", )+
    labs(title = colnames(data_list$data)[1])
}


# Data transformation        =====
#' Reverse probes values.
#' Data transformation for MW, MB and spontaneous MW 
#' From the task script, the quesiton, and values correspond to: 
#'   instruction_text = u"To what degree where you focused on the task right before this question?",
#'   _labels = [u"Clearly \n NOT FOCUSED", "", "", u"Clearly \n FOCUSED"]) # 0 - 4 respectively
#'   instruction_text = u"To the degree to which you were not focusing on the task, were you thinking about nothing or were you thinking about something?",
#'   scale_labels = [u"Clearly \n NOTHING", "", "", u"Clearly \n SOMETHING"]) # 0 - 4 respectively
#'   instruction_text = u"Were you deliberate about where you focused your attention (either on-task or elsewhere) or did it happen spontaneously?",
#'   scale_labels = [u"Clearly \n SPONTANEOUS", "", "", u"Clearly \n DELIBERATE"]) # 0 - 4 respectively
#'  
#'  With this transformation:
#'  - The mind wandering probe (probe1) indicate 0 as task focus and 4 as mind wandering.
#'  - The mind blanking probe (probe2) indicate 0 as content and 4 as mind blanking.
#'  - The spontaneous mind wandeirng (probe3) indicate 0 as deliberate mind wandering, and 4 as spontaneous mind wandering. 

d.pro.stim_pfc |> 
  mutate(
    probe1 = ordered(5-as.numeric(probe1)),
    probe2 = ordered(5-as.numeric(probe2)),
    probe3 = ordered(5-as.numeric(probe3)),
    stimulation = factor(stimulation, levels = c("sham", "real"))
  ) -> pfc


# rmANOVA                 =====

#' We will perform a 2x4 rmANOVA (repeated measures analysis of variance) 
#' of task-focus (mind wandering) with stimulation type (active versus sham) 
#' and block (B0, B1, B2, & B3).

# Transformation
pfc |>  
  select(subj,block,proberound,MW1=probe1, MW2=probe2, MW3=probe3, AE=zlogapen, BV=zlogbv, stimulation) |>
  group_by(subj,block,stimulation) |>
  summarize(MW1=mean(as.numeric(MW1)), 
            MW2=mean(as.numeric(MW2)), 
            MW3=mean(as.numeric(MW3)), 
            AE =mean(AE), BV=mean(BV)) |> 
  ungroup() -> 
  pfc_anova_data

# Export data for JASP. 
pfc_anova_data |>
  pivot_wider(names_from = c(stimulation, block), values_from = c(MW1, MW2, MW3, AE, BV)) -> pfc_anova_data2
write_csv(pfc_anova_data2, file="data/pfc_anova.csv")


# Results from these functions seems to correspond to JASP
afex::aov_car(MW1 ~ block * stimulation  + Error(subj/(block*stimulation)), 
              pfc_anova_data) -> aov_mw
anova(aov_mw, es="pes")
aov_mw |> summary() 
  # sphericity not met for block

afex::aov_car(BV ~ block*stimulation + Error(subj/(block*stimulation)), 
              pfc_anova_data)  -> aov_bv
anova(aov_bv, es="pes")
aov_bv |> summary()

afex::aov_car(AE ~ block*stimulation + Error(subj/(block*stimulation)), 
              pfc_anova_data)  -> aov_ae
anova(aov_ae, es="pes")
aov_ae |> summary()

#' No interaction between block and stimulation was found. We therefore do not do
#' the contrast analysis.  

pfc_anova_data |>
  summarise(
    .by = c(stimulation, block),
    MW_m = mean(MW1),
    MW_sd = sd(MW1),
    BV_m = mean(BV),
    BV_sd = sd(BV),
    AE_m = mean(AE),
    AE_sd = sd(AE)
  ) |>
  mutate(
    across(contains("_"), ~fmt_APA_numbers(.x, .chr=T))
  ) |> 
  pivot_wider(names_from = stimulation, values_from=ends_with(c("_m","_sd"))) |>
  mutate(e="",e2="",e3="",e4="", e5="",e6="") |>
  gt() |>
  cols_move(ends_with("sham"), after = 1) |>
  tab_spanner("MW", c(starts_with("MW") & ends_with("sham"))) |>
  tab_spanner("BV", c(starts_with("BV") & ends_with("sham"))) |>
  tab_spanner("AE", c(starts_with("AE") & ends_with("sham"))) |>
  tab_spanner("MW ", c(starts_with("MW") & ends_with("real"))) |>
  tab_spanner("BV ", c(starts_with("BV") & ends_with("real"))) |>
  tab_spanner("AE ", c(starts_with("AE") & ends_with("real"))) |>
  tab_spanner("Sham", c(ends_with("_sham"), e,e2,e3)) |>
  tab_spanner("Real", c(ends_with("_real"), e4,e5,e6)) |>
  cols_label(
    contains("_m_") ~ md("*M*"),
    contains("_sd_") ~ md("*SD*"),
    starts_with("e") ~ ""
  ) |> cols_move(e, MW_sd_sham) |> 
  cols_move(e2, BV_sd_sham) |>
  cols_move(e3, AE_sd_sham) |>
  cols_move(e4, MW_sd_real) |>
  cols_move(e5, BV_sd_real) |>
  cols_move(e6, AE_sd_real) 
  gtsave("tables/anova_descriptives.docx")

##  Contrasts     =====
# Note. We do not report these.  
# Check for significant increase in variables compared to baseline
### MW            =====
with(filter(pfc_t, variable=="MW", block=="B1"),
     t.test(value ~ stimulation, paired=T))
with(filter(pfc_t, variable=="MW", block=="B2"),
     t.test(value ~ stimulation, paired=T))
# Sig 
# Compared to baseline, block 2 is significantly increased. 
with(filter(pfc_t, variable=="MW", block=="B3"),
     t.test(value ~ stimulation, paired=T))
### AE               =====
with(filter(pfc_t, variable=="AE", block=="B1"),
     t.test(value ~ stimulation, paired=T))
with(filter(pfc_t, variable=="AE", block=="B2"),
     t.test(value ~ stimulation, paired=T))
with(filter(pfc_t, variable=="AE", block=="B3"),
     t.test(value ~ stimulation, paired=T))
### BV                =======
with(filter(pfc_t, variable=="BV", block=="B1"),
     t.test(value ~ stimulation, paired=T))
with(filter(pfc_t, variable=="BV", block=="B2"),
     t.test(value ~ stimulation, paired=T))
# sig
with(filter(pfc_t, variable=="BV", block=="B3"),
     t.test(value ~ stimulation, paired=T))


# Plots                            ======
## Effect over block and stimulation:         =====
# Transform the data for visualization.
pfc |>
  mutate(probe1_n = as.integer(probe1)) |>
  select(subj, region, stimulation, block, proberound, zlogapen, zlogbv, probe1_n) |>
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
         variable=factor(variable, levels=c("MW","BV","AE"))) -> pfc_t

###  MW, BV and AE          ======
pfc_t |>
  mutate(variable=fct_recode(variable, `Approximate Entropy`="AE",
                             `Behavioural Variability`="BV", `Mind Wandering`="MW"),
         Stimulation = fct_recode( factor(stimulation, levels=c("real","sham")),
                                   Real ="real", Sham = "sham")) |>
  ggplot(aes(x = block, y = value, group = Stimulation, color = Stimulation))+
  facet_wrap(~variable)+
  stat_summary(fun.data=mean_se, geom = "line", position = position_dodge(.1)) +
  stat_summary(geom = "pointrange", position = position_dodge(.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  # # Uncomment to add more/less arrows: 
  # geom_segment(aes(x=0.75, y=.025, xend = 0.75, yend=.165), 
  #              arrow = arrow(), linejoin="round", color = "black", size=.7 )+
  # geom_text(aes(x=.75, y=.185), 
  #           label = "More", color = "black", size = 4)+
  # geom_segment(aes(x=0.75, y=-.025, xend = 0.75, yend=-.165), 
  #              arrow = arrow(), linejoin="round", color = "black", size=.7 )+
  # geom_text(aes(x=.75, y=-.185), label = "Less", color = "black", size = 4)+
  labs(x="Block", y = "Difference to baseline") +
  geom_text(
    data=tibble(
      variable = factor(
      c("Mind Wandering", "Behavioural Variability", "Approximate Entropy"),
      levels = c("Mind Wandering", "Behavioural Variability", "Approximate Entropy")),
      lab = c("a)","b)","c)")),
    aes("B0", .29, col = NULL, group=NULL, label = lab), size = 5, show.legend = F)
  theme(legend.position = "top", legend.direction = "horizontal") # -> p1
ggsave("figs/prereg/descriptive_MW-BV-AE+block+stim-v4.svg", dpi=300, width=6.5, heigh=3.5)


### MB and S-MW             =====
# Data transformation for MB & SMW:
pfc |> 
  na.omit() |> 
  select(subj, region, stimulation, block, proberound, zlogapen, zlogbv, starts_with("probe")) |>
    # fixed
  filter(probe1>2) |> ## only MW probes!
  mutate(blank=as.integer(probe2), #>2
         spontaneous=as.integer(probe3)) |> #>2
  select(-probe1,-probe2,-probe3) |> 
  group_by(subj, stimulation, block) |>
  summarize(across(c(blank, spontaneous), mean)) |>
  gather(var,val,blank,spontaneous) |>
  pivot_wider(names_from=c(block,var), values_from=val) |>
  mutate(across(starts_with("B"), as.numeric)) |>
  mutate(B0_mb=0,
         B1_mb=B1_blank-B0_blank,
         B2_mb=B2_blank-B1_blank,
         B3_mb=B3_blank-B2_blank,
         B0_smw=0,
         B1_smw=B1_spontaneous-B0_spontaneous,
         B2_smw=B2_spontaneous-B1_spontaneous,
         B3_smw=B3_spontaneous-B2_spontaneous) |>
  select(-ends_with("zlogapen"), -ends_with("zlogbv"), -ends_with("probe1"), -ends_with("blank"), -ends_with("spontaneous")) |>
  pivot_longer(starts_with("B"), names_to = c("block","variable"), names_sep = "_") |>
  mutate(variable=fct_recode(variable, MB="mb", `SMW`="smw")) -> data.probe.cond.diff4

# Plot for MB & SMW
data.probe.cond.diff4 |>
  filter(variable %in% c("MB", "SMW")) |>
  mutate(variable = ifelse(variable=="MB", "Mind Blanking", "Spontaneous Mind Wandering"),
         Stimulation=ifelse(stimulation=="real","Real","Sham")) |>
  ggplot(aes(y=value, x=block, color=Stimulation))+
  stat_summary(fun.data=mean_se, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))+
  geom_hline(yintercept = 0, linetype="dashed")+
  #scale_color_manual(values=c("sham"="cornflowerblue",real="red"))+
  labs(y = "Difference to baseline", x="Block", col="Stimulation")+
  facet_grid(~ variable) +
  theme_bw()
  # theme(legend.position ="top", legend.direction = "horizontal")+
ggsave(filename ="figs/prereg/descriptive_MB-SMW+block+stim-v2.svg", dpi = 300, width=6, height=3.5)


# Bayesian models     ======
if(!script_load_bayesian_data){
    
  ## MW           =====
  mod.pfc.mw <- brm(probe1 ~ stimulation + block*stimulation + scale(proberound) + (1|subj), 
                    init=0, family=cumulative("probit"), data=pfc, 
                    backend = "cmdstanr", chains = 6, iter=3000)
  bayes_plot(mod.pfc.mw)+ labs(title="Full -AE&BE")
  summary(mod.pfc.mw)
  
  
  ## AE           ======
  mod.pfc.ae <- brm(zlogapen ~ stimulation + block*stimulation + scale(proberound) + (1|subj), 
                    init=0, data=pfc, backend = "cmdstanr", chains = 6, iter=3000)
  bayes_plot(mod.pfc.ae, "sigma")
  summary(mod.pfc.ae)
  
  
  ## BV           ======
  mod.pfc.bv <- brm(zlogbv ~ stimulation + block*stimulation + scale(proberound) + (1|subj), 
                    init=0, data=pfc, backend = "cmdstanr", chains = 6, iter=3000)
  bayes_plot(mod.pfc.bv, "sigma")
  summary(mod.pfc.bv)
  
  
  ## MB           =====
  # For these latter probe (MB, SMW), we ignore the responses that were not preceeded by mind wandering.
  pfc |> 
    filter(probe1 > 2) -> dd
  
  mod.pfc.mb <- brm(probe2 ~ stimulation + block*stimulation + scale(proberound) + (1|subj), 
                   init=0, data=dd, family=cumulative(link="probit"), cores=6, 
                   backend = "cmdstanr", chains = 6, iter=3000)
  bayes_plot(mod.pfc.mb)
  summary(mod.pfc.mb)
  
  
  ## SMW          =====
  mod.pfc.smw <- brm(probe3 ~ stimulation + block*stimulation + scale(proberound) + (1|subj), 
                   init=0, data=dd, family=cumulative(link="probit"),  
                   backend = "cmdstanr", chains = 6, iter=3000)
  bayes_plot(mod.pfc.smw)
  summary(mod.pfc.smw)
  
  
  ## LOO criteria & save data ====
  mod.pfc.mw  <- brms::add_criterion(mod.pfc.mw,  criterion = c("bayes_R2", "loo"))
  mod.pfc.ae  <- brms::add_criterion(mod.pfc.ae,  criterion = c("bayes_R2", "loo"))
  mod.pfc.bv  <- brms::add_criterion(mod.pfc.bv,  criterion = c("bayes_R2", "loo"))
  mod.pfc.mb  <- brms::add_criterion(mod.pfc.mb,  criterion = c("bayes_R2", "loo"))
  mod.pfc.smw <- brms::add_criterion(mod.pfc.smw, criterion = c("bayes_R2", "loo"))
  save(mod.pfc.ae, mod.pfc.bv, mod.pfc.mw, mod.pfc.mb, mod.pfc.smw, file="data/export/paper_vars.RData")
}

if(script_load_bayesian_data){
 load("data/export/paper_vars.RData")
}

## FULL BAYES      =====
if(!script_load_bayesian_data){
  brm(
    probe1 ~ zlogapen*zlogbv*stimulation*block + scale(proberound) + (1|subj), data = pfc,
    family=cumulative("probit"), chains = 6, iter=4000, cores=6, init=0, backend="cmdstanr"
  ) -> larg_mod_test
  bayes_plot(larg_mod_test)
  
  save(larg_mod_test, file = "data/export/paper_large_model.Rdata")
} 
if(script_load_bayesian_data){
  load("data/export/paper_large_model.Rdata")
}

l_mod_table <- 
  as_tibble(larg_mod_test) |> 
  select(starts_with("b_"), sd_subj__Intercept) |>
  gather(variable,val) |>
  group_by(variable) |>
  summarize(mean=mean(val), 
            q5 = hdi(val)[1],
            q95 = hdi(val)[2],
            erat = sum(val>0)/sum(val<=0),
            erat = ifelse(erat<1, 1/erat, erat),
            pd = ifelse(mean(val < 0) < 0.5, mean(val > 0), mean(val < 0))) |>
  mutate(variable=fct_recode(
    variable,
    Threshold1="b_Intercept[1]", Threshold2="b_Intercept[2]", Threshold3="b_Intercept[3]", 
    # Block
    `Stimulation (B0)`="b_stimulationreal",
    B1="b_blockB1", 
    B2="b_blockB2", 
    B3="b_blockB3",
    `B1 x stimulation`="b_stimulationreal:blockB1", 
    `B2 x stimulation`="b_stimulationreal:blockB2",
    `B3 x stimulation`="b_stimulationreal:blockB3", 
    Trial="b_scaleproberound",  
    # Behaviour
    BV = "b_zlogbv", 
    `BV x stimulation` = "b_zlogbv:stimulationreal", 
    AE = "b_zlogapen", 
    `AE x stimulation` = "b_zlogapen:stimulationreal",
    `BV x AE` = "b_zlogapen:zlogbv",
    `BV x AE x stimulation` = "b_zlogapen:zlogbv:stimulationreal",
    # behaviour and block 
    `BV x B1` = "b_zlogbv:blockB1", 
    `BV x B2` = "b_zlogbv:blockB2", 
    `BV x B3` = "b_zlogbv:blockB3",
    `BV x B1 x stimulation` = "b_zlogbv:stimulationreal:blockB1", 
    `BV x B2 x stimulation` = "b_zlogbv:stimulationreal:blockB2", 
    `BV x B3 x stimulation` = "b_zlogbv:stimulationreal:blockB3",
    `AE x B1` = "b_zlogapen:blockB1", 
    `AE x B2` = "b_zlogapen:blockB2", 
    `AE x B3` = "b_zlogapen:blockB3",
    `AE x B1 x stimulation` = "b_zlogapen:stimulationreal:blockB1", 
    `AE x B2 x stimulation` = "b_zlogapen:stimulationreal:blockB2", 
    `AE x B3 x stimulation` = "b_zlogapen:stimulationreal:blockB3",
    `AE x B1` = "b_zlogapen:blockB1", 
    `AE x B2` = "b_zlogapen:blockB2", 
    `AE x B3` = "b_zlogapen:blockB3",
    `BV x AE x B1` = "b_zlogapen:zlogbv:blockB1",
    `BV x AE x B2` = "b_zlogapen:zlogbv:blockB2",
    `BV x AE x B3` = "b_zlogapen:zlogbv:blockB3",
    `BV x AE x B1 x stimulation` = "b_zlogapen:zlogbv:stimulationreal:blockB1",
    `BV x AE x B2 x stimulation` = "b_zlogapen:zlogbv:stimulationreal:blockB2",
    `BV x AE x B3 x stimulation` = "b_zlogapen:zlogbv:stimulationreal:blockB3",
    `Sigma (subjects)`="sd_subj__Intercept"),
    variable=ordered(variable, levels=c(
      "Threshold1", "Threshold2", "Threshold3",
      "Trial", "Stimulation (B0)", 
      "B1","B2","B3", 
      "B1 x stimulation", "B2 x stimulation", "B3 x stimulation", 
      "BV", 
      "BV x stimulation", 
      "AE", 
      "AE x stimulation", 
      "BV x AE", 
      "BV x AE x stimulation", 
      "BV x B1", 
      "BV x B2", 
      "BV x B3", 
      "BV x B1 x stimulation", 
      "BV x B2 x stimulation", 
      "BV x B3 x stimulation", 
      "AE x B1", 
      "AE x B2", 
      "AE x B3", 
      "AE x B1 x stimulation", 
      "AE x B2 x stimulation", 
      "AE x B3 x stimulation", 
      "BV x AE x B1", 
      "BV x AE x B2", 
      "BV x AE x B3", 
      "BV x AE x B1 x stimulation", 
      "BV x AE x B2 x stimulation", 
      "BV x AE x B3 x stimulation", 
      "Sigma (subjects)"))) |> 
  arrange(variable) |>
  mutate(group=case_when(variable %in% c("Sigma (subjects)") ~ "Model fit",
                         T ~ "Coefficients")) |>
  mutate(b = sprintf("%.2f%s", mean, ifelse(pd>=0.95 & group=="Coefficients", "*", "")),
         HDI = sprintf("[%.2f, %.2f]", q5, q95),
         pd = sprintf("%.2f", pd)) |>
  select(-mean,-q5, -q95)

r2 <- brms::bayes_R2(larg_mod_test)
lo <- brms::loo(larg_mod_test)$estimates["looic",]
  
l_mod_table |> 
  add_row(variable="R2", pd="", #²
          b = sprintf("%.2f", r2[1]),
          HDI = sprintf("[%.2f, %.2f]", r2[3], r2[4]),
          group="Model fit") |>
  add_row(variable="LOOIC", pd="", 
          b = sprintf("%.2f", lo[1]),
          HDI = sprintf("(SE=%.2f)", lo[2]),
          group = "Model fit") |>
  mutate(erat = fmt_APA_numbers(erat, .chr=T)) |>
  gt(groupname_col = "group") |>
  cols_move(c(erat, pd), HDI) |>
  cols_label(
    b = md("*b*"), 
    erat = md("ER~b~"),
    pd = md("*p*~b~")
  ) |>
  cols_align("center", 2:6) |>
  gtsave("tables/large_bayes_model.docx")
  

# Other         =====
## Visualize Bayesian models           =====
###  MW, BV & AE                =====
as.data.frame(mod.pfc.mw) |> 
  select(starts_with("b_block"), starts_with("b_stimulation")) |> 
  rename_with(~paste0("mw_", .x)) |> 
  # combine:
  cbind( as.data.frame(mod.pfc.bv) |> 
           select(starts_with("b_block"), starts_with("b_stimulation")) |>
           rename_with(~paste0("bv_", .x)) ) |> 
  cbind( as.data.frame(mod.pfc.ae) |> 
           select(starts_with("b_block"), starts_with("b_stimulation")) |>
           rename_with(~paste0("ae_", .x)) ) |>
  # Transform
  mutate(B0_mw_sham = 0,
         B0_mw_real = 0,
         B1_mw_sham = mw_b_blockB1,
         B1_mw_real = mw_b_blockB1+`mw_b_stimulationreal:blockB1`,
         B2_mw_sham = mw_b_blockB2,
         B2_mw_real = mw_b_blockB2+`mw_b_stimulationreal:blockB2`,
         B3_mw_sham = mw_b_blockB3,
         B3_mw_real = mw_b_blockB3+`mw_b_stimulationreal:blockB3`) |>
  mutate(B0_bv_sham = 0,
         B0_bv_real = 0,
         B1_bv_sham = bv_b_blockB1,
         B1_bv_real = bv_b_blockB1+`bv_b_stimulationreal:blockB1`,
         B2_bv_sham = bv_b_blockB2,
         B2_bv_real = bv_b_blockB2+`bv_b_stimulationreal:blockB2`,
         B3_bv_sham = bv_b_blockB3,
         B3_bv_real = bv_b_blockB3+`bv_b_stimulationreal:blockB3`) |>
  mutate(B0_ae_sham = 0,
         B0_ae_real = 0,
         B1_ae_sham = ae_b_blockB1,
         B1_ae_real = ae_b_blockB1+`ae_b_stimulationreal:blockB1`,
         B2_ae_sham = ae_b_blockB2,
         B2_ae_real = ae_b_blockB2+`ae_b_stimulationreal:blockB2`,
         B3_ae_sham = ae_b_blockB3,
         B3_ae_real = ae_b_blockB3+`ae_b_stimulationreal:blockB3`) 
  select(starts_with("B", ignore.case = F)) |>
  pivot_longer( everything() ) |>
  separate_wider_delim(name, delim = "_", names = c("block", "var", "stimulation")) |>
  mutate(var = case_when(var == "mw" ~ "MW",
                         var == "bv" ~ "BV",
                         var == "ae" ~ "AE",),
         var = factor(var, levels = c("MW", "BV", "AE")),
         stimulation = factor( stimulation, levels = c("sham", "real")))  |>
  ggplot(aes( x = block, y = value, color = stimulation )) +
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))+
  stat_summary(fun.data = median_hdi, geom="pointrange", position=position_dodge(width=0.2)) 
geom_hline(yintercept=0, linetype = "dashed") +
  scale_y_continuous( breaks = seq(-1,1, 0.1) ) +
  facet_wrap( ~ var ) +
  labs(y  = "", title = "b)")


###    MB & SMW        =====
as.data.frame(mod.pfc.mb) |> 
  select(starts_with("b_block"), starts_with("b_stimulation")) |> 
  rename_with(~paste0("mb_", .x)) |> 
  # combine:
  cbind( as.data.frame(mod.pfc.smw) |> 
           select(starts_with("b_block"), starts_with("b_stimulation")) |>
           rename_with(~paste0("smw_", .x)) ) |> 
  # Transform
  mutate(B0_mb_sham = 0,
         B0_mb_real = 0,
         B1_mb_sham = mb_b_blockB1,
         B1_mb_real = mb_b_blockB1+`mb_b_stimulationreal:blockB1`,
         B2_mb_sham = mb_b_blockB2,
         B2_mb_real = mb_b_blockB2+`mb_b_stimulationreal:blockB2`,
         B3_mb_sham = mb_b_blockB3,
         B3_mb_real = mb_b_blockB3+`mb_b_stimulationreal:blockB3`) |>
  mutate(B0_smw_sham = 0,
         B0_smw_real = 0,
         B1_smw_sham = smw_b_blockB1,
         B1_smw_real = smw_b_blockB1+`smw_b_stimulationreal:blockB1`,
         B2_smw_sham = smw_b_blockB2,
         B2_smw_real = smw_b_blockB2+`smw_b_stimulationreal:blockB2`,
         B3_smw_sham = smw_b_blockB3,
         B3_smw_real = smw_b_blockB3+`smw_b_stimulationreal:blockB3`) |>
  select(starts_with("B", ignore.case = F)) |>
  #gather(var, val) 
  pivot_longer( everything() ) |>
  separate_wider_delim(name, delim = "_", names = c("block", "var", "stimulation")) |>
  mutate(var = case_when(var == "mb" ~ "MB",
                         var == "smw" ~ "SMW",),
         var = factor(var, levels = c("MB", "SMW")),
         stimulation = factor( stimulation, levels = c("sham", "real")))  |>
  ggplot(aes( x = block, y = value, color = stimulation ))+
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(y  = "", title = "b)") +
  stat_summary(fun.data=mean_qi, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))+
  scale_y_continuous( breaks = seq(-1,1, 0.1) ) +
  facet_wrap( ~ var )
ggsave("figs/prereg/bayesian-model_MB-SMW+block+stim.png", dpi=300, width=6, height=4)





# WITH  BV & AE
mod.pfc.test <- brm(probe1 ~ zlogbv * zlogapen + 
                      blockB1 + blockB1:stimulation + 
                      blockB2 + blockB2:stimulation + 
                      blockB3 + blockB3:stimulation + 
                      zproberound + (1|subj),  data=pfc_t,
                    init=0, family=cumulative("probit"), backend = "cmdstanr", chains = 6, iter=3000)
bayes_plot(mod.pfc.test)

add_criterion(mod.pfc.test, criterion = c("loo", "bayes_R2")) -> mod.pfc.test
loo_compare(mod.pfc.mw2, mod.pfc.test)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 



## Quantify differences using the Bayesian regression models     =====
pfc |>
  select(subj,block,proberound,MW1=probe1, MW2=probe2, MW3=probe3, AE=zlogapen, BV=zlogbv, stimulation) |>
  group_by(subj,block,stimulation) |>
  summarize(MW1=mean(as.numeric(MW1)), MW2=mean(as.numeric(MW2)), MW3=mean(as.numeric(MW3)),
            AE=mean(AE), BV=mean(BV)) |>
  mutate(stimulation = factor(stimulation, levels=c("sham", "real"))) -> pfc_data_rdy


## other models =====


as.data.frame(mod.pfc.mw) |> 
  select(starts_with("b_block"), starts_with("b_stimulation")) |> 
  mutate(B0_mw_sham=0,
         B0_mw_real=0, 
         B1_mw_sham=b_blockB1, 
         B1_mw_real=b_blockB1+`b_stimulationreal:blockB1`, 
         B2_mw_sham=b_blockB2,
         B2_mw_real=b_blockB2+`b_stimulationreal:blockB2`,
         B3_mw_sham=b_blockB3,
         B3_mw_real=b_blockB3+`b_stimulationreal:blockB3`) |> 
  select(starts_with("B", ignore.case=F)) |>
  gather(var,val) |>
  separate(var, c("block","var","stimulation"), sep="_") |>
  ggplot(aes(x=block, y=val, color=stimulation))+
  stat_summary(fun.data=mean_qi, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))+
  scale_y_continuous(breaks = seq(-1,1,0.1)) + 
  labs(x="", y = "Z-score value") + 
  theme(legend.position = "none") -> bPlot3  # title="MW5: Common baseline") -> bPlot3

### BV                  ======

as.data.frame(mod.pfc.mw_no_ae) |> 
  select(starts_with("b_block"), starts_with("b_stimulation")) |> 
  mutate(B0_mw_sham=0,
         B0_mw_real=0, # b_stimulationreal, 
         B1_mw_sham=b_blockB1,
         B1_mw_real=b_blockB1+`b_stimulationreal:blockB1`,
         B2_mw_sham=b_blockB2,
         B2_mw_real=b_blockB2+`b_stimulationreal:blockB2`,
         B3_mw_sham=b_blockB3,
         B3_mw_real=b_blockB3+`b_stimulationreal:blockB3`) |> 
  select(starts_with("B", ignore.case=F)) |>
  gather(var,val) |>
  separate(var, c("block","var","stimulation"), sep="_") |>
  ggplot(aes(x=block, y=val, color=stimulation))+
  stat_summary(fun.data=mean_qi, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))+
  labs(title="MW3: Stim*block + BV") -> bPlot1


#### Stim*block model    =====
as.data.frame(mod.pfc.mw) |> 
  select(starts_with("b_block"), starts_with("b_stimulation")) |> 
  mutate(B0_mw_sham=0,
         B0_mw_real=0, # b_stimulationreal, 
         B1_mw_sham=b_blockB1,
         B1_mw_real=b_blockB1+`b_stimulationreal:blockB1`,
         B2_mw_sham=b_blockB2,
         B2_mw_real=b_blockB2+`b_stimulationreal:blockB2`,
         B3_mw_sham=b_blockB3,
         B3_mw_real=b_blockB3+`b_stimulationreal:blockB3`) |> 
  select(starts_with("B", ignore.case=F)) |>
  gather(var,val) |>
  separate(var, c("block","var","stimulation"), sep="_") |>
  ggplot(aes(x=block, y=val, color=stimulation))+
  stat_summary(fun.data=mean_qi, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))+
  labs(x = "Block", y = "") -> bPlot2


### Gathered plot           ======
bPlot1+bPlot2+bPlot3
ggsave("figs/three_bayes_models.png", width=12, heigh=4)



