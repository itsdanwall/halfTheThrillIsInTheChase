
emorat <- c(3.14, 3.67, 2.81)
sd_emorat <- sd(emorat)

# convert standard errors into standard deviations
sd_emorat <- c(2.39, 2.01, 2.23)

sd_sd_emorat <- sd(sd_emorat)
pooled_sd_emorat <- mean(sd_emorat)

# Willingness to pay 
emorat_mean_sd <- data.frame(study_cond = c("anger", "fear", "sad"),
                             mean = emorat,
                             sd = sd_emorat )

emorat_mean_sd <- emorat_mean_sd %>%
  mutate(pooled_sd = mean(sd))

J = 100000
rnorms_emorat = list(anger = rep(0, J), 
                     fear = rep(0, J), 
                     sad = rep(0, J))

for(i in 1:nrow(emorat_mean_sd)) {
  for(j in 1:J) {
    rnorms_emorat[[i]][j] = sd(rtruncnorm(30, a = 1, b = 9, mean = emorat_mean_sd[i, "mean"], sd = emorat_mean_sd[i, "sd"]))
  }
}

rnorms_emorat_df <- as.data.frame(rnorms_emorat)



rnorms_emorat_df1 <- rnorms_emorat_df %>%
  mutate(replication = 1:J) %>%
  group_by(replication) %>%
  mutate(sd = RowSD(cbind(anger, fear, sad)),
         stdev_lt = ifelse(sd <= sd_sd_emorat, TRUE, FALSE))
rnorms_emorat_df1$stdev_lt %>% summary()
