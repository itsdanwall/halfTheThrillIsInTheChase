#-------
# Half the fun is in the data faking
# Daniel Wall
# 10 Feb 2016
#-------

setwd("/Users/DanWall/Documents/halfTheThrillIsInTheDataFaking/")

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
            mean_sd = mean(sd))

tbl1 <- merge(tbl1, tbl1sum)

reps <- 100000

tbl1_reps <- tbl1 %>%
  group_by(variable, manip) %>%
  do(replicate(reps, sd(rtruncnorm(.$n_per, a = as.numeric(.$min_score[1]), b = as.numeric(.$max_score[1]), mean = .$mean, sd = .$mean_sd))) %>% 
       data.frame())

tbl1_reps <- tbl1_reps %>%
  rename_("rep_sd" = ".") %>%
  group_by(variable, manip) %>%
  mutate(n_rep = 1:reps)


tbl1_reps1 <- tbl1_reps %>%
  group_by(variable, n_rep) %>%
  summarise(sd_sd = sd(rep_sd))

tbl1_reps1 <- merge(tbl1sum, tbl1_reps1) 

tbl1_reps1 <- tbl1_reps1 %>%
  mutate(sd_sd_obs_gt_sd_sd_sim = ifelse(sd_sd_obs >= sd_sd, TRUE, FALSE))

# plot the histograms
ggplot(tbl1_reps1, aes(x = sd_sd)) +
  geom_histogram() +
  geom_vline(aes(xintercept = sd_sd_obs)) +
  facet_grid(variable ~ .)

tbl1_reps1_sum <- tbl1_reps1 %>%
  group_by(variable) %>%
  summarise(sum_sim_gt_obs = sum(sd_sd_obs_gt_sd_sd_sim),
            n_reps = max(n_rep)) %>%
  mutate(p_val = sum_sim_gt_obs/n_reps)



save.image("analyzed_data.Rdata")