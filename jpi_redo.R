# redo of Simonsohn
library(dplyr)
library(truncnorm)
# function to calculate sd of sample
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


jpi_tbl1 <- data.frame(cond = c("high", "low", "ctrl"),
                       mean = c(39.74, 85.74, 65.73),
                       sd = c(25.09, 24.58, 25.65),
                       n_per = c(15, 15, 15),
                       min_score = c(0, 0, 0),
                       max_score = c(100, 100, 100)) %>%
  mutate(pooled_sd = mean(sd),
         sd_sds = sd(sd), 
         mean_n = mean(n_per),
         se_sds = pooled_sd/(sqrt(2*mean_n)),
         psi = sd_sds / se_sds)

jpi_tbl1_reps <- jpi_tbl1 %>%
  group_by(cond) %>%
  do(sd_samp(df = ., reps = 100000))

jpi_tbl1_reps <-jpi_tbl1_reps %>%
  group_by(cond) %>%
  mutate(n_rep = 1:n()) 

jpi_tbl1_reps_sum <- jpi_tbl1_reps %>%
  ungroup() %>%
  group_by(n_rep) %>%
  summarise(sim_sd_sds = sd(rep_sd),
            pooled_sd = mean(rep_sd),
            se_sds = pooled_sd/(sqrt(2*15)),
            sim_psi = sim_sd_sds / se_sds) %>%
  mutate(study_sd_sds = 0.5351947,
         study_psi = 0.1167571,
         studysd_gt_simsd = ifelse(study_sd_sds > sim_sd_sds, TRUE, FALSE),
         studypsi_gt_simpsi = ifelse(study_psi > sim_psi, TRUE, FALSE))


