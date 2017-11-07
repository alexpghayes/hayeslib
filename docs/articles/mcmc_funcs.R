## ------------------------------------------------------------------------
library(hayeslib)
library(tidyverse)  # hack for now
library(coda)

num_samples <- 10^3

postr <- tibble(alpha = rnorm(num_samples),
                beta = rnorm(num_samples))

postr %>% 
  mcmc_trace()

## ------------------------------------------------------------------------
postr %>% 
  t_mcmc()

## ------------------------------------------------------------------------
postr %>% 
  t_mcmc() %>% 
  mcmc_stats()

## ------------------------------------------------------------------------
postr %>% 
  t_mcmc() %>% 
  mcmc_stats() %>% 
  mcmc_hpd_int()

