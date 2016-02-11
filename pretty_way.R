#-------
# Half the fun is in the data faking
# Daniel Wall
# 10 Feb 2016
#-------

setwd("/git_repositories/halfTheThrillIsInTheChase/")

tbl1 <- read.csv("table1.csv", stringsAsFactors = FALSE)

#----functions ----

sd_samp <- function(df, reps) {
  nper <- as.numeric(df[1, "n_per"])
  minscore <- as.numeric(df[1, "min_score"])
  maxscore <- as.numeric(df[1, "max_score"])
  mean <- as.numeric(df[1, "mean"])
  pooledsd <- as.numeric(df[1, "pooled_sd"])
  
  reps <- replicate(reps,
                    sd(rtruncnorm(nper, a = minscore, b = maxscore, mean = mean, 
                                  sd = pooledsd))) 
  reps <- as.data.frame(reps)
  
  names(reps) <- "rep_sd"
  
  reps
}


library(dplyr)
library(truncnorm)
library(ggplot2)
library(plotrix)
# add in the number of participants
tbl1 <- tbl1 %>%
  mutate(n_per = 11,
         sd = se * sqrt(n_per)) %>%
  arrange(variable) 

# create sd_sd and mean_sd for each variable
tbl1sum <- tbl1 %>%
  group_by(variable) %>%
  summarise(sd_sd_obs = sd(sd),
            pooled_sd = mean(sd),
            mean_n = mean(n_per),
            se_sds = pooled_sd/(sqrt(2*mean_n)),
            psi = sd_sd_obs / se_sds)

tbl1 <- merge(tbl1, tbl1sum)

reps <- 100000

tbl1_reps <- tbl1 %>%
  group_by(variable, manip) %>%
  do(sd_samp(df = ., reps = reps))

tbl1_reps <- tbl1_reps %>%
  group_by(variable, manip) %>%
  mutate(n_rep = 1:reps)

tbl1_reps <- merge(tbl1_reps, tbl1sum)

tbl1_reps1 <- tbl1_reps %>%
  group_by(variable, n_rep) %>%
  summarise(sim_sd_sds = sd(rep_sd),
            sim_pooled_sd = mean(rep_sd)) %>%
  mutate(sim_se_sds = sim_pooled_sd/(sqrt(2 * 11)),
         sim_psi = sim_sd_sds/sim_se_sds)


tbl1_reps1 <- merge(tbl1sum, tbl1_reps1)

tbl1_reps1 <- tbl1_reps1 %>%
  mutate(sd_sd_obs_gt_sd_sd_sim = ifelse(sd_sd_obs >= sim_sd_sds, TRUE, FALSE))



# plot the histograms
ggplot(tbl1_reps1, aes(x = sim_sd_sds)) +
  geom_histogram() +
  geom_vline(aes(xintercept = sd_sd_obs)) +
  facet_grid(variable ~ .)

tbl1_reps1_sum <- tbl1_reps1 %>%
  group_by(variable) %>%
  summarise(sum_sim_gt_obs = sum(sd_sd_obs_gt_sd_sd_sim),
            n_reps = max(n_rep)) %>%
  mutate(p_val = sum_sim_gt_obs/n_reps)

tbl1_reps1_meanpsi <- tbl1_reps1 %>%
  group_by(n_rep) %>%
  summarise(mean_sim_psi = mean(sim_psi)) %>%
  mutate(study_psi = 0.3178941,
         simpsi_gt_stupsi = ifelse(mean_sim_psi > study_psi, TRUE, FALSE))

# this likely isn't correct as there could be correlations between variables
# this correlation can't be 0 

save.image("analyzed_data.Rdata")