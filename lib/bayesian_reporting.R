#' A diverse set of function related to bayesian diagnostics and reporting. 


bayes_hdi <- function(bayes_vector, ...){
  #' Get the highest density interval for a posterior vector. 
  #' 
  require(tidyverse)
  require(tidybayes)
  
  bayes_vector |> 
    tidybayes::hdi() |>
    dplyr::as_tibble() |>
    dplyr::mutate( dplyr::across(tidyselect::everything(), ~fmt_APA_numbers( .x, .chr=T, ... )) ) |>  
    stringr::str_glue_data("[{V1}, {V2}]") |> 
    as.character()
}

bayes_er <- function(bayes_vector, ...){
  #' Get the evidence ratio that the posterior coefficient is in the beta specified direction. 
  #' 
  
  mean <- mean(bayes_vector)
  
  ifelse(mean > 0, sum(bayes_vector > 0) / sum(bayes_vector <= 0), 
                   sum(bayes_vector < 0) / sum(bayes_vector >= 0) ) |>
    fmt_APA_numbers(...)
} 

bayes_p <- function(bayes_vector, ...){
  #' Get the probability that the vector is in the (mean) specified direction. 
  #' 
  
  mean <- mean(bayes_vector)
  
  ifelse(mean > 0, sum(bayes_vector > 0) / length(bayes_vector), 
                   sum(bayes_vector < 0) / length(bayes_vector) ) |>
    fmt_APA_numbers(...)
}

coef_hdi_s_text <- function(x, ...){
  #' Produce a simple text output of a Bayesian posterior vector
  #' 
  
  mean                     <- mean(x) |> fmt_APA_numbers(...)
  hdi                      <- bayes_hdi(x, ...)
  evidence_ratio_direction <- bayes_er(x,...)
  probability_direction    <- bayes_p(x, .psym = T)
  
  sprintf(
    "b = %s, %s, ER = %s, p %s", 
    mean, hdi, evidence_ratio_direction, probability_direction)
}

coef_hdi_text <- function(x, ..., .preserve_negative = TRUE){
  #' Report Bayesian posterior statisticss
  
  require(tidybayes)
  
  mean                     <- mean(x) |> fmt_APA_numbers(...)
  hdi                      <- bayes_hdi(x, ...)
  evidence_ratio_direction <- bayes_er(x, ...)
  probability_direction    <- bayes_p(x, .psym = T)
  
  if(.preserve_negative & mean < 0 & round(mean, 2)==0){
    mean_fix <- paste0("-", fmt_APA_numbers(mean, ...))
  } else {
    mean_fix <- mean |> fmt_APA_numbers(...)
  }
  
  if(is.infinite(evidence_ratio_direction)){
    evidence_ratio_direction = if_else(evidence_ratio_direction < 0, "-\\infty", "\\infty")
  }
  
  sprintf("$b = %s$, %s, $\\text{ER}_\\text{dir} = %s$, $p_\\text{dir} %s$", 
          mean_fix, hdi, evidence_ratio_direction, probability_direction)
}


bayes_coef_plot <- function( bayes_model, add_label = TRUE, offset = 0){
  #' Generate a Bayesian coefficient plot.
  
  require(brms)
  require(bayesplot)
  
  coefficients <- paste0("b_", bayes_model |> fixef() |> rownames())
  
  # Generate main plot 
  p <- bayesplot::mcmc_intervals( bayes_model, coefficients, prob_outer = .95) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = .4) +
    ggplot2::labs(title = paste( "Dependent variable:",  bayes_model[["formula"]][["formula"]][[2]] ) )
  
  # Get offset for each variable
  calc_offset <-
    bayes_model |>
    tibble::as_tibble() |>
    dplyr::select( tidyselect::all_of(coefficients) ) |> 
    tidyr::pivot_longer( tidyselect::everything() ) |>
    dplyr::summarise(
      .by = name, 
      m = mean(value),
      sd = sd(value)
    ) |>
    dplyr::mutate(offset = dplyr::if_else(m>=0, m+sd, m-sd) / 2) |>
    dplyr::pull(offset)
  
  # Add text 
  if(add_label){
    for(coefficient in 1:length(coefficients)){
      # Generate the label for the coefficient
      coefficient_label <- 
        as.matrix(bayes_model)[ ,coefficients[ coefficient ] ] |>
        coef_hdi_s_text()
      
      # Get the coefficient offset
      coefficient_offset <- if_else(
        calc_offset[coefficient] >= 0, 
        calc_offset[coefficient] + offset, 
        calc_offset[coefficient] - offset)
        
      # Add text to plot
      p <- p + 
        ggplot2::geom_text(
          label = coefficient_label,
          x = coefficient_offset,
          y = coefficients[coefficient],
          check_overlap = T, 
          vjust = 1.5,
        )
    }
  }
  p
}

bayes_chain_stab <- function( bayes_model ){
  
  require(tidyverse)
  require(tidybayes)
  require(bayesplot)
  
  bayesplot::mcmc_rank_ecdf(
    bayes_model, paste0("b_", bayes_model |> brms::fixef() |> rownames()), 
    plot_diff = T, prob = .99)
}

bayes_diag <- function( bayes_model, convergence = TRUE, offset = 0){
  #'
  #' Criteria: 
  #'   -  Rhat > 1.05
  #'   -  ESS <= 1000 
  
  require(tidyverse)
  require(tidybayes)
  require(bayesplot)
  
  tidybayes::summarise_draws(bayes_model) |>
    dplyr::summarise(
      rhat  = dplyr::if_else( any(rhat >= 1.05, na.rm=T), 
                     paste("CHECK (", round(max(rhat, na.rm=T), 0), ")"),
                     paste("OK    (", round(max(rhat, na.rm=T), 0), ")")), 
      ESS_Bulk = dplyr::if_else( any(ess_bulk <= 1000, na.rm=T),
                     paste("CHECK (", round(min(ess_bulk, na.rm=T), 0), ")"),
                     paste("OK    (", round(min(ess_bulk, na.rm=T), 0), ")")),
      ESS_Tail = dplyr::if_else( any(ess_tail <= 1000, na.rm=T),
                     paste("CHECK (", round(min(ess_tail, na.rm=T), 0), ")"),
                     paste("OK    (", round(min(ess_tail, na.rm=T), 0), ")")) 
    ) |> print()
  
  bayes_coef_plot( bayes_model, offset = offset )
}

bayes_tbl_sum <- function(
    bayes_model
    , add_sigma = FALSE 
    , add_loo = FALSE 
    , add_R2 = FALSE
    , add_loo_R2 = FALSE
    , add_convergence = FALSE
    , apa_table = FALSE 
    , .low_val = FALSE 
    , fmt_md = FALSE){
  #' Generate a table for the Bayesian posterior statistics.
  #' 
  
  require(tidyverse)
  require(tidybayes)
  
  if(apa_table){
    add_sigma  = T
    add_loo    = T
    add_R2     = T
    add_loo_R2 = T
  }
  
  # Get model data:
  mod_data <- as.matrix(bayes_model)
  
  # Get model variable names:
  mod_variables_names <- bayes_model |> 
    tibble::as_tibble() |> 
    dplyr::select( tidyselect::starts_with("b_") ) |> 
    colnames()
  
  # If we add convergence, then get the data: 
  if(add_convergence) mod_sum <- tidybayes::summarise_draws(bayes_model)
  
  tbl <- map(mod_variables_names, \(coefficient){
    tibble::tibble(
      var = stringr::str_replace(coefficient, "b_", "")
      , m   = mean( as.numeric( mod_data[, coefficient] ) ) |> fmt_APA_numbers(.chr=T, .low_val=.low_val)
      , hdi = bayes_hdi( mod_data[, coefficient] )
      , er  = bayes_er(  mod_data[, coefficient], .chr=T ) 
      , p   = bayes_p(   mod_data[, coefficient], .chr=T, .p=T) ) 
    }) |> purrr::list_rbind()
  
  if(add_convergence){
    tbl <- tbl |> dplyr::left_join(
      mod_sum |> 
        dplyr::filter( variable %in% mod_variables_names ) |>
        dplyr::select( variable, rhat, ess_bulk, ess_tail ) |> 
        dplyr::mutate( variable = stringr::str_remove_all(variable, "b_") )
      , by = c( "var"="variable")
    )
  }
  
  if(apa_table) tbl <- tbl |> dplyr::mutate(.before=1, group="Coefficients")

  # Determine the sigma name:  
  sigma_name <- ifelse(fmt_md, "$\\sigma\\,\\text{(subjects)}$", "Sigma (subjects)")
  if(add_sigma){
    sigma <- tibble::tibble(
      var   = sigma_name
      , m   = mean( as.numeric( mod_data[, "sd_subj__Intercept"] ) ) |> fmt_APA_numbers(.chr=T, .low_val=.low_val)
      , hdi = bayes_hdi(mod_data[, "sd_subj__Intercept"])
      , er  = bayes_er( mod_data[, "sd_subj__Intercept"], .chr=T)
      , p   = bayes_p(  mod_data[, "sd_subj__Intercept"], .chr=T, .p=T)
    )
    if(add_convergence){
      sigma <- sigma |> dplyr::left_join(
        mod_sum |> 
          dplyr::filter( variable %in% "sd_subj__Intercept" ) |>
          dplyr::select( variable, rhat, ess_bulk, ess_tail ) |> 
          dplyr::mutate( variable = sigma_name )
        , by = c("var"="variable")
      )
    }
    
    if(apa_table) sigma <- sigma |> dplyr::mutate(group="Model fit") 
    
    tbl <- tbl |> dplyr::bind_rows(sigma)
  }
  
  if(add_loo){
    message("Adding LOOIC...")
    
     if( !is.null(bayes_model[["criteria"]][["loo"]]) ){
       looic_name <- ifelse(fmt_md, "$\\text{LOOIC}$", "LOOIC")
        looic <-  tibble::tibble(
          var = looic_name
          , m   = bayes_model[["criteria"]][["loo"]][["estimates"]]["looic", "Estimate"] |> fmt_APA_numbers(.chr=T)
          , hdi = sprintf("SE = %s", bayes_model[["criteria"]][["loo"]][["estimates"]]["looic", "SE"] |>
                            fmt_APA_numbers(.chr=T) )
          , er = ""
          , p  = ""
          )
        
        if(apa_table) looic <- looic |> dplyr::mutate(group="Model fit")
        
        tbl <- tbl |> dplyr::bind_rows(looic)
        
       message("Added LOOIC.")
     } else {
       warning("NO LOOIC criterion found! Skipping... \n   Did you forget to `add_loo()`?")
     }
  } 
  
  if(add_loo_R2){
    message("Adding loo adjusted R2...")
    
    if( !is.null(bayes_model[["criteria"]][["loo_R2"]]) ){
      loo_R2_name <- if_else(fmt_md, "$\\text{LOO}\\,\\text{R}^2$", "LOO R2")
      
      loo_r2 <- tidybayes::mean_hdi( bayes_model[["criteria"]][["loo_R2"]] ) |> 
        fmt_APA_numbers(.chr=T, .low_val=T, .rm_leading_0 = T)
      
      loo_r2_df <- tibble::tibble(
        var   = loo_R2_name
        , m   = loo_r2[["y"]]
        , hdi = sprintf( "[%s, %s]", loo_r2[["ymin"]], loo_r2[["ymax"]] )
        , er  = ""
        , p   = "")
      
      if(apa_table) loo_r2_df <- loo_r2_df |> mutate(group="Model fit")
      
      tbl <- tbl |> dplyr::bind_rows(loo_r2_df)
        
      message("Added LOO adjusted R2.")
    } else {
       warning("NO LOO R2 criterion found! Skipping... \n   Did you forget to `add_criterion()`?")
    }
  }
  
  if(add_R2){
    message("Adding R2...")
    
    if( !is.null(bayes_model[["criteria"]][["loo"]]) ){
      R2_name <- ifelse(fmt_md, "$\\text{R}^2$", "R2")
      
      r2 <- tidybayes::mean_hdi( bayes_model[["criteria"]][["bayes_R2"]] ) |> 
        fmt_APA_numbers(.chr=T, .low_val=T, .rm_leading_0 = T)
      
      r2_df <- tibble::tibble(
          var   = R2_name
          , m   = r2[["y"]]
          , hdi = sprintf("[%s, %s]", r2[["ymin"]], r2[["ymax"]])
          , er  = ""
          , p   = "")
      
      if(apa_table) r2_df <- r2_df |> mutate(group="Model fit")
      
      tbl <- tbl |> dplyr::bind_rows(r2_df)
        
      message("Added R2.")
    } else {
       warning("NO R2 criterion found! Skipping... \n   Did you forget to `add_criterion()`?")
    }
  }
  
  tbl
}


bayes_tbl_add_sig <- function(data, sig = .95, na.rm = T){
  #' Function to add significance marker to tables produced from @bayes_tbl_sum.
  
  require(tidyverse)
  
  data <- 
    data |> mutate(ppp222 = as.numeric( str_remove_all(p, "[<>=]") )) 
  if(na.rm) data <- data |> mutate( ppp222 = if_else( is.na(ppp222), 0, ppp222) )
  data |> 
    mutate(
      m  = if_else(ppp222 > sig, paste0(m,"*"), m),
      ppp222 = NULL
    )
}


tab_bayes_generics <- function(data, fmt_md_var = F, pre_footnote="", post_footnote=""){
  c_names <- names(data$`_data`)
  vars <- data$`_data`$var
  
  # ER
  if( any( str_ends(c_names, "er" ) ) ){
    er_dir <- "ER~dir~ indicates the evidence ratio that the effect is in the *b* specified direction."
    if(pre_footnote != ""){
      er_dir = paste0(" ", er_dir)
    }
  } else { er_dir = ""}
  # P
  if( any( str_ends(c_names, "p") ) ){
    p_dir <- " *p*~dir~ indicates the probability that the effect is in the *b* specified direction."
  } else { p_dir <- "" }
  
  # Convergence
  if( any( str_ends(c_names, "rhat") ) ){
    rhat <- " $\\hat{R}^2$ indicates convergence between and within-chain."
  } else { rhat <- "" }
  if( any( str_ends(c_names, "ess_bulk") ) ){
    ess_bulk <- " $\\text{ESS}_{bulk}$ indicates the effective sample size of the bulk of the distribution."
  } else { ess_bulk <- "" }
  if( any( str_ends(c_names, "ess_tail") ) ){
    ess_tail <- " $\\text{ESS}_{bulk}$ indicates the effective sample size of the tails of the distribution."
  } else { ess_tail <- "" }
  
  # Add empty space if some are present
  if( !("" %in% c(er_dir, p_dir)) ){
    post_footnote = paste0(" ", post_footnote)
  }
  
  # general transformation.
  data <- 
    data |>
    cols_label(
      "var" = "",
      ends_with("m")    ~ md("*b*"),
      ends_with("hdi")  ~ md("HDI"),
      ends_with("er")   ~ md("ER~dir~"),
      ends_with("p")    ~ md("*p*~dir~"),
      ends_with("rhat") ~ md("$\\hat{R}$"),
      ends_with("e")    ~ "",
    ) |>
    tab_footnote(
      md(str_glue("*Note.* {pre_footnote}{er_dir}{p_dir}{rhat}{ess_bulk}{ess_tail}{post_footnote}") )
      , placement = "left"
    ) |>
    tab_footnote( md("\\* *p*~dir~ > .95") ) |> # Probability notation
    cols_align("center", -var) |>
    cols_align("left", var) |> # Align
    tab_fmt_APA()  # General table transformations
  
  if(fmt_md_var){
    data <- data |>
      fmt_markdown(columns = var)
  }
  
  data
}

  
