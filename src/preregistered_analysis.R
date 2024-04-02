library(ProjectTemplate)
load.project()

library(brms)
library(bayesplot)
library(cmdstanr)
library(tidybayes)


bayes_plot <- function( data_list, variables = NULL ){
  int <- variables(data_list)[str_detect(variables(data_list), "Intercept")]
  remove_list <- c(c("disc","lp__", "lprior"),variables,int)
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


# Transformations       =====
# Reverse the probes (higher value correspond to the probe Q)
d.pro.stim_pfc |> 
  mutate(
    probe1 = ordered(5-as.numeric(probe1)),
    probe2 = ordered(5-as.numeric(probe2)),
    probe3 = ordered(5-as.numeric(probe3)),
    stimulation = factor(stimulation, levels = c("sham", "real"))
  ) -> pfc


#' We will perform a 2x4 rmANOVA (repeated measures analysis of variance) 
#' of task-focus (mind wandering) with stimulation type (active versus sham) 
#' and block (B0, B1, B2, & B3).
#' 

# ANOVA test    =====
## Anova
pfc |>  
  select(subj,block,proberound,MW1=probe1, MW2=probe2, MW3=probe3, AE=zlogapen, BV=zlogbv, stimulation) |>
  group_by(subj,block,stimulation) |>
  summarize(MW1=mean(as.numeric(MW1)), 
            MW2=mean(as.numeric(MW2)), 
            MW3=mean(as.numeric(MW3)), 
            AE =mean(AE), BV=mean(BV)) |> 
  ungroup() -> 
  pfc_anova_data


# To jasp
pfc_anova_data |>
  pivot_wider(names_from = c(stimulation, block), values_from = c(MW1, MW2, MW3, AE, BV)) -> pfc_anova_data2
write_csv(pfc_anova_data2, file="data/pfc_anova.csv")

## These result in the same numbers 
aov(MW1 ~ block*stimulation + Error(subj/(block*stimulation)), 
                            # Within subject
    data=pfc_anova_data) |> summary()
  # Block & stim

aov(AE ~ block * stimulation + Error(subj/(block*stimulation)), 
    data=pfc_anova_data) |> summary()
  # sig block

aov(BV ~ block * stimulation + Error(subj/(block*stimulation)), 
    data=pfc_anova_data) |> summary()
  # nothing


#' If this rmANOVA is significant, we will calculate the difference between 
#' real and sham session (real-sham) for each block (both measures are 
#' within-subject). We will then compare this difference between B0 
#' (baseline) and the other three blocks using planned contrast (with Tukey's 
#' adjustment for multiple comparisons). We expect that this difference will 
#' be larger than baseline (B0) in all three post-stimulation blocks (B1, B2, 
#' and B3) indicating that real stimulation reduced the amount of MW relative 
#' to sham stimulation. Next, we will investigate whether this tentative effect 
#' will grow with repeated stimulations by comparing real-sham difference 
#' between B1 and B2, B1 and B3 as well as B2 and B3. We expect that all these 
#' comparisons to be larger than zero, indicating that effect gets stronger 
#' with repeated application of the stimulation. 


# first plot below


#' prop MB and spontaneous MW across blocks
#' from stimulation script:
#'   instruction_text = u"To what degree where you focused on the task right before this question?",
#'   _labels = [u"Clearly \n NOT FOCUSED", "", "", u"Clearly \n FOCUSED"])
#'   instruction_text = u"To the degree to which you were not focusing on the task, were you thinking about nothing or were you thinking about something?",
#'   scale_labels = [u"Clearly \n NOTHING", "", "", u"Clearly \n SOMETHING"])
#'   instruction_text = u"Were you deliberate about where you focused your attention (either on-task or elsewhere) or did it happen spontaneously?",
#'   scale_labels = [u"Clearly \n SPONTANEOUS", "", "", u"Clearly \n DELIBERATE"])
#'  This has been reversed at the start:
#'  Higher MW is indicated with higher value;
#'  Higher MB is indicated wiht higher value;
#'  Higher SMW is indicated with higher value.

# Plots  ======

## basic = mw+bv+ae         ======

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


##   p1 - MW+BV+AE         ======
# over B & S  # -> p1
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
  theme(legend.position = "top", legend.direction = "horizontal")
  # -> p1
ggsave("figs/prereg/descriptive_MW-BV-AE+block+stim-v4.svg", dpi=300, width=6.5, heigh=3.5)


pfc_t |>
  mutate(variable=fct_recode(variable, `c) Approximate Entropy`="AE",
                             `b) Behavioural Variability`="BV", `a) Mind Wandering`="MW"),
         Stimulation = fct_recode( factor(stimulation, levels=c("real","sham")),
                                   Real ="real", Sham = "sham")) -> 
  pfc_plot_dat
  
pfc_plot_dat |>
  filter(variable == "Mind Wandering") |>
  ggplot(aes(x = block, y = value, group = Stimulation, color = Stimulation))+
  # facet_wrap(~variable)+
  stat_summary(fun.data=mean_se, geom = "line", position = position_dodge(.1)) +
  stat_summary(geom = "pointrange", position = position_dodge(.1)) +
  geom_hline(yintercept=0, linetype="dashed")+
  labs(title="a)         Mind Wandering")



## p2 = MB & SMW    =====
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
  facet_grid(~ variable) 
  theme(legend.position ="top", legend.direction = "horizontal")
ggsave(filename ="figs/prereg/descriptive_MB-SMW+block+stim-v2.svg", dpi = 300, width=6, height=3.5)

### p1+p2 = Combined plot         ====
# # not used
# p1+(p2+ggplot()+theme_void())+plot_layout(nrow = 2)#,   widths = c(3, 2))
# ggsave(filename = "figs/prereg/pfc_paper_summary.jpeg", width=11, height=3)



# Bayesian models   ======

#' Quantify differences using the Bayesian regression models
# pfc |>  
#   select(subj,block,proberound,MW1=probe1, MW2=probe2, MW3=probe3, AE=zlogapen, BV=zlogbv, stimulation) |>
#   group_by(subj,block,stimulation) |>
#   summarize(MW1=mean(as.numeric(MW1)), MW2=mean(as.numeric(MW2)), MW3=mean(as.numeric(MW3)), 
#             AE=mean(AE), BV=mean(BV)) |>
#   mutate(stimulation = factor(stimulation, levels=c("sham", "real"))) -> pfc_data_rdy
# 

## Mind wandering       =====

### Full model  (BV*AE)      =====
mod.pfc.mw.behav <- brm(probe1 ~ stimulation + block*stimulation + zlogapen * zlogbv + scale(proberound) + (1|subj), 
                 init=0, family=cumulative("probit"), data=pfc, 
                 backend = "cmdstanr", chains = 6, iter=3000)
bayes_plot(mod.pfc.mw.behav) + labs(title="Full model") -> p_mw1
p_mw1

### MW w/out BV*AE       =====
mod.pfc.mw <- brm(probe1 ~ stimulation + block*stimulation + scale(proberound) + (1|subj), 
                  init=0, family=cumulative("probit"), data=pfc, 
                  backend = "cmdstanr", chains = 6, iter=3000)
bayes_plot(mod.pfc.mw)+ labs(title="Full -AE&BE") -> p_mw2
p_mw2

# Add LOO criteria 
mod.pfc.mw |> add_criterion(criterion = c("loo","bayes_R2")) -> mod.pfc.mw
# compare LOO 
loo_compare(mod.pfc.mw.behav, mod.pfc.mw) 



### MW - common baseline    =====
data.probe.cond.pfc <- pfc |> 
  mutate(blockB1=as.integer(block=="B1"),
         blockB2=as.integer(block=="B2"),
         blockB3=as.integer(block=="B3"),
         zproberound=scale(proberound))

mod.pfc.mw2 <- brm(probe1 ~ blockB1 + blockB1:stimulation + blockB2 + blockB2:stimulation + blockB3 + 
                     blockB3:stimulation + zproberound + (1|subj),  data=data.probe.cond.pfc,
                   init=0, family=cumulative("probit"), backend = "cmdstanr", chains = 6, iter=3000)
bayes_plot(mod.pfc.mw2) + labs(title="Common baseline -AE&BV") -> p_mw3
p_mw3

add_criterion(mod.pfc.mw2, criterion = c("loo", "bayes_R2")) -> mod.pfc.mw2
loo_compare(mod.pfc.mw2, mod.pfc.mw)


## 3-way interaction BV      =====
# 3 way intr BV
mod.pfc.test <- brm(probe1 ~ zlogbv + zlogapen + stimulation + 
                      # block
                      blockB1 + blockB1:stimulation +
                      blockB2 + blockB2:stimulation + 
                      blockB3 + blockB3:stimulation + 
                      # BV
                      blockB1:zlogbv + zlogbv:blockB1:stimulation + 
                      blockB2:zlogbv + zlogbv:blockB2:stimulation + 
                      blockB3:zlogbv + zlogbv:blockB3:stimulation +
                      # AE
                      blockB1:zlogapen + zlogapen:blockB1:stimulation + 
                      blockB2:zlogapen + zlogapen:blockB2:stimulation + 
                      blockB3:zlogapen + zlogapen:blockB3:stimulation +
                      zproberound + (1|subj),  
                    data=data.probe.cond.pfc,
                   init=0, family=cumulative("probit"), backend = "cmdstanr", chains = 6, iter=3000, cores=6)

# BV prediction on MW 
as.data.frame(mod.pfc.test) |> 
  #colnames()
  # Transform
  mutate(B0_mw_sham=b_zlogbv,
         B0_mw_real=b_zlogbv,
         B1_mw_sham=b_zlogbv + `b_zlogbv:blockB1` ,
         B1_mw_real=b_zlogbv + `b_zlogbv:blockB1` +`b_zlogbv:stimulationreal:blockB1`,
         B2_mw_sham=b_zlogbv + `b_zlogbv:blockB2` ,
         B2_mw_real=b_zlogbv + `b_zlogbv:blockB2` +`b_zlogbv:stimulationreal:blockB2`,
         B3_mw_sham=b_zlogbv + `b_zlogbv:blockB3` ,
         B3_mw_real=b_zlogbv + `b_zlogbv:blockB3` +`b_zlogbv:stimulationreal:blockB3`,
         ) |>
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
  stat_summary(fun.data=mean_qi, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))+
  scale_y_continuous( breaks = seq(-1,1, 0.1) ) +
  labs(title="BV+Block+stim -> MW ", y="")

# AE prediction on MW 
as.data.frame(mod.pfc.test) |> 
  # colnames()
  # combine:
  # Transform
  mutate(B0_mw_sham=b_zlogapen,
         B0_mw_real=b_zlogapen,
         B1_mw_sham=b_zlogapen + `b_zlogapen:blockB1` ,
         B1_mw_real=b_zlogapen + `b_zlogapen:blockB1` +`b_zlogapen:stimulationreal:blockB1`,
         B2_mw_sham=b_zlogapen + `b_zlogapen:blockB2` ,
         B2_mw_real=b_zlogapen + `b_zlogapen:blockB2` +`b_zlogapen:stimulationreal:blockB2`,
         B3_mw_sham=b_zlogapen + `b_zlogapen:blockB3` ,
         B3_mw_real=b_zlogapen + `b_zlogapen:blockB3` +`b_zlogapen:stimulationreal:blockB3`,
         ) |>
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
  stat_summary(fun.data=mean_qi, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))+
  scale_y_continuous( breaks = seq(-1,1, 0.1) ) +
  labs(y  = "", title ="AE+Block+stim  ->  MW") 





### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# with bv/ae
mod.pfc.test <- brm(probe1 ~ zlogbv * zlogapen + blockB1 + blockB1:stimulation + blockB2 + blockB2:stimulation + blockB3 + 
                     blockB3:stimulation + zproberound + (1|subj),  data=data.probe.cond.pfc,
                   init=0, family=cumulative("probit"), backend = "cmdstanr", chains = 6, iter=3000)
bayes_plot(mod.pfc.test)

add_criterion(mod.pfc.test, criterion = c("loo", "bayes_R2")) -> mod.pfc.test
loo_compare(mod.pfc.mw2, mod.pfc.test)
#

# Save params
p_mw1+p_mw2+p_mw3
ggsave("figs/model_params.png", width=12, height=4)
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 




## Models      =====
pfc |> 
  filter(probe1>2) -> dd

### MB probit model
mod.pfc.mb <- brm(probe2 ~ stimulation + block*stimulation + scale(proberound) + (1|subj), 
                 init=0, data=dd, family=cumulative(link="probit"), cores=6, 
                 backend = "cmdstanr", chains = 6, iter=3000)
bayes_plot(mod.pfc.mb)
summary(mod.pfc.mb)


### spontaneous MW    =====
mod.pfc.smw <- brm(probe3 ~ stimulation + block*stimulation + scale(proberound) + (1|subj), 
                 init=0, data=dd, family=cumulative(link="probit"),  
                 backend = "cmdstanr", chains = 6, iter=3000)
bayes_plot(mod.pfc.smw)
summary(mod.pfc.smw)


### AE     ======
mod.pfc.ae <- brm(zlogapen ~ stimulation + block*stimulation + scale(proberound) + (1|subj), 
                 init=0, data=pfc, backend = "cmdstanr", chains = 6, iter=3000)
bayes_plot(mod.pfc.ae, "sigma")
summary(mod.pfc.ae)


### BV     ======
mod.pfc.bv <- brm(zlogbv ~ stimulation + block*stimulation + scale(proberound) + (1|subj), 
                 init=0, data=pfc, backend = "cmdstanr", chains = 6, iter=3000)
bayes_plot(mod.pfc.bv, "sigma")
summary(mod.pfc.bv)


### LOO criteria & save data ====

mod.pfc.ae  <- brms::add_criterion(mod.pfc.ae,  criterion = c("bayes_R2", "loo"))
mod.pfc.bv  <- brms::add_criterion(mod.pfc.bv,  criterion = c("bayes_R2", "loo"))
mod.pfc.mb  <- brms::add_criterion(mod.pfc.mb,  criterion = c("bayes_R2", "loo"))
mod.pfc.smw <- brms::add_criterion(mod.pfc.smw, criterion = c("bayes_R2", "loo"))


save(mod.pfc.ae, mod.pfc.bv, mod.pfc.mw, mod.pfc.mb, mod.pfc.smw, file="data/export/paper_vars.RData")
load("data/export/paper_vars.RData")



#   Contrasts ???     =====
with(filter(pfc_t, variable=="MW", block=="B1"),
     t.test(value ~ stimulation, paired=T))
with(filter(pfc_t, variable=="MW", block=="B2"),
     t.test(value ~ stimulation, paired=T))
# Sig
with(filter(data.probe.cond.diff4, variable=="MW", block=="B3"),
     t.test(value ~ stimulation, paired=T))

with(filter(data.probe.cond.diff, variable=="AE", block=="B1"),
     t.test(value ~ stimulation, paired=T))
with(filter(data.probe.cond.diff, variable=="AE", block=="B2"),
     t.test(value ~ stimulation, paired=T))
with(filter(data.probe.cond.diff, variable=="AE", block=="B3"),
     t.test(value ~ stimulation, paired=T))

with(filter(data.probe.cond.diff, variable=="BV", block=="B1"),
     t.test(value ~ stimulation, paired=T))
with(filter(data.probe.cond.diff, variable=="BV", block=="B2"),
     t.test(value ~ stimulation, paired=T))
# sig
with(filter(data.probe.cond.diff, variable=="BV", block=="B3"),
     t.test(value ~ stimulation, paired=T))



### Plot of the models     =====
### Model with BV       =====
as.data.frame(mod.pfc.mw_no_ae) |> 
  select(starts_with("b_block"), starts_with("b_stimulation")) |> 
  mutate(B0_mw_sham=0,
         B0_mw_real=0, # b_stimulationreal, 
         # This should also be 0? Correct?
         # participants were NOT stimulated on the baseline 
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
          # This should also be 0? Correct?
          # participants were NOT stimulated on the baseline 
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



#### common baseline model     ======
as.data.frame(mod.pfc.mw2) |> 
  select(starts_with("b_block"), starts_with("b_stimulation")) |> 
  mutate(B0_mw_sham=0,
         B0_mw_real=0, 
         B1_mw_sham=b_blockB1, 
         B1_mw_real=b_blockB1+`b_blockB1:stimulationreal`, 
         B2_mw_sham=b_blockB2,
         B2_mw_real=b_blockB2+`b_blockB2:stimulationreal`,
         B3_mw_sham=b_blockB3,
         B3_mw_real=b_blockB3+`b_blockB3:stimulationreal`) |> 
  select(starts_with("B", ignore.case=F)) |>
  gather(var,val) |>
  separate(var, c("block","var","stimulation"), sep="_") |>
  ggplot(aes(x=block, y=val, color=stimulation))+
  stat_summary(fun.data=mean_qi, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))+
  scale_y_continuous(breaks = seq(-1,1,0.1)) + 
  labs(x="", y = "Z-score value") + 
  theme(legend.position = "none") -> bPlot3  # title="MW5: Common baseline") -> bPlot3
         
bPlot1+bPlot2+bPlot3
ggsave("figs/three_bayes_models.png", width=12, heigh=4)






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


##    MB & SMW        =====
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
