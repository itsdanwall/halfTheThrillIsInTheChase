library(dplyr)
library(truncnorm)

#---Product Evaluation-----

prod_eval <- c(2.28, 1.41, 1.31, 1.48, 1.35)
sd_prod_eval <- sd(prod_eval)

# convert standard errors into standard deviations
se_prod_eval <- c(.23, .23, .25, .23, .27)
sd_prod_eval <- se_prod_eval * sqrt(11)

sd_sd_prod_eval <- sd(sd_prod_eval)
pooled_sd_prod_eval <- mean(sd_prod_eval)

# Willingness to pay 
prod_eval_mean_sd <- data.frame(study_cond = c("app_sens", "avoid_sens", "ctrl1", "ctrl2", "app_inf"),
                          mean = prod_eval,
                          sd = sd_prod_eval )

prod_eval_sd <- prod_eval_mean_sd %>%
  mutate(pooled_sd = mean(sd))

# wtp_appsens <- rnorm(11, mean = wtp[1], sd = pooled_sd)
# lapply(, rnorm, n =  11, mean = 1.49, sd = 1.06132)

# bootsds <- wtp_mean_sd %>% 
#   group_by(study_cond) %>%
#   bootstrap(100) %>%
#   do(as.data.frame(sd(rnorm(11, mean = .$mean, sd = .$pooled_sd))))
# 
# number of replications 
J = 100000
rnorms = list(app_sens = rep(0, J), 
              avoid_sens = rep(0, J), 
              ctrl1 = rep(0, J), 
              ctrl2 = rep(0, J), 
              app_inf = rep(0, J))

for(i in 1:nrow(prod_eval_sd)) {
  for(j in 1:J) {
    rnorms[[i]][j] = sd(rtruncnorm(11, a = 1, b = 7, mean = prod_eval_mean_sd[i, "mean"], sd = prod_eval_mean_sd[i, "sd"]))
  }
}

# lapply()

rnorms_df <- as.data.frame(rnorms)

RowSD <- function(x) {
  sqrt(rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1))
}

rnorms_df1 <- rnorms_df %>%
  mutate(replication = 1:J) %>%
  group_by(replication) %>%
  mutate(sd = RowSD(cbind(app_sens,
                             avoid_sens,
                             ctrl1,
                             ctrl2,
                             app_inf)),
         stdev_lt = ifelse(sd <= 0.05932959, TRUE, FALSE))


rnorms_df1$stdev_lt %>% summary


# do phis
se_pooled_sd <- pooled_sd_prod_eval/sqrt(2*55)

sd_sd_prod_eval/se_pooled_sd

#---- Willingness to pay-----

wtp <- c(1.49, .33, .19, .85, .96)
sd_wtp <- sd(wtp)

# convert standard errors into standard deviations
se_wtp <- c(.31, .30, .33, .31, .35)
sd_wtp <- se_wtp * sqrt(11)

sd_sd_wtp <- sd(sd_wtp)
pooled_sd_wtp <- mean(sd_wtp)

# Willingness to pay 
wtp_mean_sd <- data.frame(study_cond = c("app_sens", "avoid_sens", "ctrl1", "ctrl2", "app_inf"),
                          mean = c(1.49, .33, .19, .85, .96),
                          sd = sd_wtp )

wtp_mean_sd <- wtp_mean_sd %>%
  mutate(pooled_sd = mean(sd))


rnorms_wtp = list(app_sens = rep(0, J), 
              avoid_sens = rep(0, J), 
              ctrl1 = rep(0, J), 
              ctrl2 = rep(0, J), 
              app_inf = rep(0, J))

for(i in 1:nrow(wtp_mean_sd)) {
  for(j in 1:J) {
    rnorms_wtp[[i]][j] = sd(rtruncnorm(11, a = 0, b = Inf, mean = wtp_mean_sd[i, "mean"], sd = wtp_mean_sd[i, "sd"]))
  }
}

rnorms_wtp_df <- as.data.frame(rnorms_wtp)



rnorms_wtp_df1 <- rnorms_wtp_df %>%
  mutate(replication = 1:J) %>%
  group_by(replication) %>%
  mutate(sd = RowSD(cbind(app_sens,
                          avoid_sens,
                          ctrl1,
                          ctrl2,
                          app_inf)),
         stdev_lt = ifelse(sd <= 0.0663325, TRUE, FALSE))

rnorms_wtp_df1$stdev_lt %>% summary


#---Approach sensation-----

appsen <- c(3.70, 1.91, 2.05, 1.87, 2.33)
sd_appsen <- sd(appsen)

# convert standard errors into standard deviations
se_appsen <- c(.49, .49, .54, .49, .57)
sd_appsen <- se_appsen * sqrt(11)

sd_sd_appsen <- sd(sd_appsen)
pooled_sd_appsen <- mean(sd_appsen)

# Willingness to pay 
appsen_mean_sd <- data.frame(study_cond = c("app_sens", "avoid_sens", "ctrl1", "ctrl2", "app_inf"),
                          mean = appsen,
                          sd = sd_appsen )

appsen_mean_sd <- appsen_mean_sd %>%
  mutate(pooled_sd = mean(sd))


rnorms_appsen = list(app_sens = rep(0, J), 
                  avoid_sens = rep(0, J), 
                  ctrl1 = rep(0, J), 
                  ctrl2 = rep(0, J), 
                  app_inf = rep(0, J))

for(i in 1:nrow(appsen_mean_sd)) {
  for(j in 1:J) {
    rnorms_appsen[[i]][j] = sd(rtruncnorm(11, a = 1, b = 7, mean = appsen_mean_sd[i, "mean"], sd = appsen_mean_sd[i, "sd"]))
  }
}

rnorms_appsen_df <- as.data.frame(rnorms_appsen)



rnorms_appsen_df1 <- rnorms_appsen_df %>%
  mutate(replication = 1:J) %>%
  group_by(replication) %>%
  mutate(sd = RowSD(cbind(app_sens,
                          avoid_sens,
                          ctrl1,
                          ctrl2,
                          app_inf)),
         stdev_lt = ifelse(sd <= 0.1232071, TRUE, FALSE))
rnorms_appsen_df1$stdev_lt %>% summary()


# do phis

