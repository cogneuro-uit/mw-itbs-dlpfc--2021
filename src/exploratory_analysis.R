library(ProjectTemplate)
load.project()

library(tidyverse)
library(gt)


q_t.test <- function(var, group, name = NULL){
  if(is.null(name)){
    "group_0" -> g_name
    "group_1" -> g_name2
  } else {
    paste0(name, "_0") -> g_name
    paste0(name, "_1") -> g_name2
  }
  
  c(
    set_names(mean(var[group==0]), g_name),
    set_names(sd(var[group==0]), g_name),
    set_names(mean(var[group==1]), g_name2),
    set_names(sd(var[group==1]), g_name2),
    set_names(mean(var[group==0] - var[group==1]), "mean_diff"),
    #set_names(sd(var[group==0] - var[group==1]), "sd_diff"),
    set_names(t.test(var[group==0], var[group==1])$parameter, "df"),
    set_names(t.test(var[group==0], var[group==1])$statistic, "t,val"),
    set_names(t.test(var[group==0], var[group==1])$p.value, "p.val"))
}

q_t.names <- function(name){
  c(paste0(name, "_0_mean"), paste0(name, "_0_sd"), 
    paste0(name, "_1_mean"), paste0(name, "_1_sd"), "mean_diff", #"sd_diff",
    "df", "t.val", "p.val")
}
# q_t.test(pfc$probe1_n, pfc$meditation1)


# library(brms)
# testing_M <- brm(probe1 ~ zlogapen*zlogbv*stimulation + block*stimulation, 
#                  pfc, family=cumulative(link = "probit"), init=0, backend = "cmdstanr",
#     cores = 6, chains=6, iter = 4000 )
# bayes_plot(testing_M)


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
    music_year = ifelse(music_year>0, 1, 0), 
  ) -> pfc


# Exploratory:            ======
##   Meditation          =====
med_data <- pfc |> 
  summarize(
    .by = c(subj, meditation1),
    mw = mean(probe1_n),
    ae = mean(zlogapen),
    bv = mean(zlogbv)) |> 
  reframe(
    names = q_t.names("meditation"),
    MW = q_t.test(mw, meditation1),
    BV = q_t.test(bv, meditation1),
    AE = q_t.test(ae, meditation1)
  ) |>
  pivot_longer(c(MW,BV,AE)) |>
  pivot_wider(names_from = names) |>
  mutate(p.adj = p.adjust(p.val, "bonferroni"))

### Table        =====
med_data
  gt() |> 
  fmt_number() |>
  tab_spanner("No experience", c(	meditation_0_mean, 	meditation_0_sd) )  |>
  tab_spanner("Any experience", c(	meditation_1_mean, 	meditation_1_sd) ) |>
  cols_label(
    ends_with("_mean") ~ md("*M*"),
    ends_with("_sd") ~ md("*SD*"),
    mean_diff = "Mdiff", df = md("*df*"),
    t.val = md("*t*"), p.adj = md("*p*"), name = "" ) |>
  cols_hide(p.val) |>
  tab_footnote(md("*Note*. Bonferroni correct for three comparisons.")) |>
  tab_header("Meditation experience on mind wandering (MW), behavioural variability (BV) and approximate entropy (AE)", 
             ) |>
  tab_options(heading.align = "left", 
              table.font.names = "Times New Roman"
              # ) |>  gtsave("report/tables/meditational_experience-mw+bv+ae.docx")
)
  
###    Plot      ======
p_meditation <-
  pfc |> 
  mutate(probe1_n = scale(probe1_n)) |> 
  group_by(subj, meditation1) |>
  summarize(MW = mean(probe1_n),
            AE = mean(zlogapen),
            BV = mean(zlogbv)) |> #pull(mw)
  ungroup() |> 
  pivot_longer(c(MW,AE,BV)) |>
  mutate(name = factor(name, levels = c("MW", "BV", "AE"))) |>
  ggplot(aes(factor(meditation1), value, col=factor(meditation1)))+
  geom_hline(yintercept=0, linetype="dashed")+
  facet_wrap(~name)+
  #geom_point(position = position_nudge(x=-.1), alpha=.3)+
  stat_summary()+
  labs(x="Meditation experience",y="Z-scored difference", title="a")+
  theme(legend.position="none")
p_meditation
#ggsave("figs/exploratory/meditation_experience.svg", p_meditation, width=6, height=3)

# pfc |> 
#   mutate(probe1_n = scale(probe1_n)) |> 
#   group_by(subj, block, meditation1) |>
#   summarize(MW = mean(probe1_n),
#             AE = mean(zlogapen),
#             BV = mean(zlogbv)) |> #pull(mw)
#   ungroup() |> 
#   pivot_longer(c(MW,AE,BV)) |>
#   mutate(name = factor(name, levels = c("MW", "BV", "AE"))) |>
#   ggplot(aes(block, value, col=factor(meditation1)))+
#   facet_wrap(~name)+
#   geom_point(position = position_nudge(x=-.1), alpha=.3)+
#   stat_summary()+
#   labs(x="Meditation experience",y="Z-scored difference")+
#   theme(legend.position="none")


##   Music experience        ======
### Calculate           =====
pfc |> 
  summarize(
    .by = c(subj, music_year1),
    mw = mean(probe1_n),
    ae = mean(zlogapen),
    bv = mean(zlogbv)) |> 
  reframe(
    names = q_t.names("music"),
    MW = q_t.test(mw, music_year1),
    BV = q_t.test(bv, music_year1),
    AE = q_t.test(ae, music_year1)
  ) |>
  pivot_longer(c(MW,BV,AE)) |>
  pivot_wider(names_from = names) |>
  mutate(p.adj = p.adjust(p.val, "bonferroni")) -> mus_data
  
### Table       =====
mus_data |> 
  gt() |> 
  fmt_number() |>
  tab_spanner("No experience", c(music_0_mean, music_0_sd) )  |>
  tab_spanner("Any experience", c(music_1_mean, music_1_sd) ) |>
  cols_label(
    ends_with("_mean") ~ md("*M*"),
    ends_with("_sd") ~ md("*SD*"),
    mean_diff = md("*M*~diff~"), df = md("*df*"), 
    t.val = md("*t*"), p.adj = md("*p*"), name = "" ) |>
  cols_hide(p.val) |>
  tab_footnote(md("*Note*. Bonferroni correct for three comparisons.")) |>
  tab_header("Musical experience on mind wandering (MW) ", "two") |>
  tab_options(heading.align = "left", 
              table.font.names = "Times New Roman"
             # ) |>  gtsave("report/tables/musical_experience-mw+bv+ae.docx")
)
  # No-sig

###  Plot      =====
p_music <- pfc |> 
  mutate(probe1_n = scale(probe1_n)) |> 
  group_by(subj, music_year1) |>
  summarize(MW = mean(probe1_n),
            AE = mean(zlogapen),
            BV = mean(zlogbv)) |> #pull(mw)
  ungroup() |> 
  pivot_longer(c(MW,AE,BV)) |>
  mutate(name = factor(name, levels = c("MW", "BV", "AE"))) |>
  ggplot(aes(factor(music_year1), value, col=factor(music_year1)))+
  geom_hline(yintercept=0, linetype="dashed")+
  facet_wrap(~name)+
#  geom_point(position = position_nudge(x=-.1), alpha=.3)+
  stat_summary()+
  labs(x="Music experience",y="Z-scored difference", title="b")+
  theme(legend.position="none")
p_music
# ggsave("figs/exploratory/music_experience.svg", p_music, width=6, height=3)



## COMBINED     =====

### Plots       =====
library(patchwork)
p_meditation+p_music+plot_layout(nrow=2)
ggsave("figs/exploratory/meditation_music-MW+BV+AE.png", dpi=300, height = 6, width = 6)


### Tables      =====
med_data |> 
  mutate(.before=1, over="meditation") |>
  rename( 
    mean_0 = meditation_0_mean, 
    sd_0 = meditation_0_sd,
    mean_1 = meditation_1_mean, 
    sd_1 = meditation_1_sd,
    ) |> bind_rows(
      mus_data |> 
        mutate(.before=1, over="music") |>
        rename(
          mean_0 = music_0_mean, 
          sd_0 = music_0_sd,
          mean_1 = music_1_mean, 
          sd_1 = music_1_sd,
        )
    ) |>
  mutate(
    across(contains("p."), ~fmt_APA_numbers(.x, .p=T)),
    across(c(df, t.val, contains("_")), ~fmt_APA_numbers(.x))
  ) |>
  gt() |>
  tab_spanner("No experience", c(mean_0, sd_0) )  |>
  tab_spanner("Any experience", c(mean_1, sd_1) ) |>
  cols_label(
    starts_with("mean") ~ md("*M*"),
    starts_with("sd") ~ md("*SD*"),
    mean_diff = md("*M*~diff~"),
    df = md("*df*"), t.val = md("*t*"), p.val = md("*p*"),
    p.adj = md("*p*~adj~")
  ) |>
  tab_header("Meditation and musical experience on mind wandering (MW), behavioural variability (BV) and approximate entropy (AE)"
             ) |>
  tab_footnote(md("*Note*. *p*~adj~ is Bonferroni correct for three comparisons.")) |>
  tab_options(heading.align = "left", 
              table.font.names = "Times New Roman"
              # ) |>  gtsave("report/tables/musical_experience-mw+bv+ae.docx")
  ) |>
  gtsave("tables/meditation_and_musical_experience.docx")

  