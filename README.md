# 2024-mw-itbs

Data, analysis and experimental materials for our paper "Increasing Mind Wandering With Accelerated Intermittent Theta Burst Stimulation Over the Left Dorsolateral Prefrontal Cortex".

Preprint is available at [PsyArXiv]([292297](https://doi.org/10.31234/osf.io/fkx3w)).

If you want to use this data/analysis in a research publication, please cite [our paper](https://doi.org/10.31234/osf.io/fkx3w).

~~~{bibtex}
@article{mw-rtms2024,
  author={Steffen Rygg Aasen and Ragnhild Nicolaisen Drevland and Gábor Csifcsák and Matthias Mittner},
  title={IncreasingMind Wandering With AcceleratedIntermittent Theta Burst Stimulation Over the Left Dorsolateral Prefrontal Cortex},
  year=2024,
  journal={},
  volume=,
  number=,
  doi=https://doi.org/10.31234/osf.io/fkx3w
}
~~~

## Requirements

Analysis are coded in [R](http://r-project.org). The scripts rely on a fair number of R-packages, in addition to [Stan](http://mc-stan.org) for Bayesian analysis. 

## Setup

This repository uses the
[ProjectTemplate](http://projecttemplate.net/) directory layout. 

## Data

Raw behavioural data is located in `data/raw` and is provided in `.csv` format. Demographics data are provided under the `demographics` folder. The `export` folder contains blinding related output, generated from the `make_randomization_list.R`), and Bayesian model outputs used for the manuscript (`paper_vars.RData`). 

### Preprocessing 

First, load the library (`library(ProjectTemplate)`) and then load the project by `load.project()`. This will munge the raw data to a finished preprocessed output (see below), load the most relevant packages and other local packages.

~~~
 subj   session block proberound probe1 probe2 probe3      apen     bv logapen logbv zlogapen zlogbv
   <chr>  <chr>   <chr>      <dbl> <ord>  <ord>  <ord>      <dbl>  <dbl>   <dbl> <dbl>    <dbl>  <dbl>   
 1 PFC001 S1      B0             1 4      3      4       0.662    0.0333   3.47  -3.40    1.36  -1.03
 2 PFC001 S1      B0             2 4      4      3       0.685    0.0368   4.81  -3.30    2.71  -0.877
 3 PFC001 S1      B0             3 3      3      3       0.669    0.0372   3.72  -3.29    1.61  -0.862
 4 PFC001 S1      B0             4 4      4      4       0.500    0.0448   1.64  -3.11   -0.469 -0.577
 5 PFC001 S1      B0             5 3      3      3       0.616    0.0366   2.56  -3.31    0.451 -0.885
 6 PFC001 S1      B0             6 2      4      4      -0.000945 0.0824   0.365 -2.50   -1.75   0.361
 7 PFC001 S1      B0             7 4      4      3       0.642    0.0434   2.96  -3.14    0.856 -0.624
 8 PFC001 S1      B0             8 3      3      3       0.642    0.0701   2.97  -2.66    0.859  0.113
 9 PFC001 S1      B0             9 1      3      2       0.335    0.0531   1.03  -2.93   -1.09  -0.313
10 PFC001 S1      B0            10 4      4      4       0.686    0.0401   4.92  -3.22    2.82  -0.746
# ℹ 3,510 more rows
# ℹ 8 more variables: region <chr>, stimulation <chr>, music_year <ord>, meditation <ord>, fatigue <ord>, snack <fct>, time <hms>, date <date>
~~~

Relevant variables are coded as follows:

## Analyses

The analyses are located in the `src/` folder as `preregistered_analysis.R`, `exploratory_analysis`, and `descriptives.R`. 

In addition, a script related to fixes of errors in the data files can be found in `fix_data_file_issues.R`, and a power analysis for the study can be found in the `power.R` script. 

Under the `report/` folder there is a Quarto document used to generating the broad strokes for the results section.
