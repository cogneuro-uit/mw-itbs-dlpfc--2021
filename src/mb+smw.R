library(ProjectTemplate)
load.project()


#### Models         ====
if(script_run_bayesian_models){
  mod.mb <- brm(probe2 ~ stimulation * (block + zlogapen + zlogbv) + scale(proberound) + (1|subj), 
                 data = d.pro.stim_pfc |> filter(probe1>2), 
                 family = cumulative("probit"),  backend = "cmdstanr", 
                 init = 0, chains = 6, iter = 6000)
  bayes_coef_plot(mod.mb, add_label = T)
  bayes_chain_stab(mod.mb)
  
  mod.smw <- brm(probe3 ~ stimulation * (block + zlogapen + zlogbv) + scale(proberound) + (1|subj), 
                data = d.pro.stim_pfc |> filter(probe1>2), 
                family = cumulative("probit"), backend = "cmdstanr", 
                init = 0, chains = 6, iter = 6000)
  
  bayes_diag(mod.smw)
  bayes_chain_stab(mod.mb)
}