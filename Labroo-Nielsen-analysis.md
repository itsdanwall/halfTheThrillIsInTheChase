---
title: "Labroo Nielsen Analysis"
author: 
date: 20191024
output: 
  html_document: 
      number_sections: yes
      toc: yes
      toc_float: true
      code_folding: hide
      keep_md: true
---






```r
# This function takes in a data frame of characteristics and simulates nreps standard deviations
sd_samp <- function(df, reps) {
  # Number of people in each cell
  nper <- as.numeric(df[1, "n_per"])
  # Minimum score on the scale
  minscore <- as.numeric(df[1, "min_score"])
  # Maximum score on the scale
  maxscore <- as.numeric(df[1, "max_score"])
  # mean observed
  mean <- as.numeric(df[1, "mean"])
  # pooled sd 
  pooledsd <- as.numeric(df[1, "pooled_sd_obs"])
  
  # Simulate SDs
  # NOTE: I'm using a truncated normal distribution because they use a 1-7 scale
  reps <- replicate(reps,
                    sd(rtruncnorm(nper, a = minscore, b = maxscore, mean = mean, 
                                  sd = pooledsd))) 
  reps <- as.data.frame(reps)
  
  names(reps) <- "sd_rep"
  
  reps
}
```



```r
# Add in the number of participants per cell and the standard deviation to table 1
tbl1 <- tbl1 %>%
  mutate(n_per = 11,
         sd = se * sqrt(n_per)) %>%
  arrange(variable) 

# create sd_sd and mean_sd for each variable in the study
tbl1_sum <- tbl1 %>%
  group_by(variable) %>%
  summarise(sd_sd_obs = sd(sd),
            pooled_sd_obs = mean(sd),
            mean_n_obs = mean(n_per),
            se_sds_obs = pooled_sd_obs/(sqrt(2*mean_n_obs)),
            psi_obs = sd_sd_obs / se_sds_obs)

# merge dataframes (need this for the simulation)
tbl1 <- merge(tbl1, tbl1_sum)
```



```r
reps <- 100000

# simulate SDs
tbl1_reps <- tbl1 %>%
  group_by(variable, manip) %>%
  do(sd_samp(df = ., reps = reps))

# add in the replication number
tbl1_reps <- tbl1_reps %>%
  group_by(variable, manip) %>%
  mutate(n_rep = 1:reps) 

# Summarized simulated data
tbl1_reps_sum <- tbl1_reps %>% 
  group_by(variable, n_rep) %>%
  summarise(sd_sd_sim = sd(sd_rep),
            pooled_sd_sim = mean(sd_rep)) %>%
    # get the simulated standard deviations and psi's
  mutate(se_sd_sim = pooled_sd_sim/(sqrt(2 * 11)),
         psi_sim = sd_sd_sim/se_sd_sim)

# merge simulated data with empirical data
tbl1_reps_sum <- tbl1_reps_sum %>% 
  left_join(tbl1_sum)
```

```
## Joining, by = "variable"
```

```r
# compare the number of observed sds that are larger than the simulated sds
tbl1_reps_sum <- tbl1_reps_sum %>%
  mutate(obs_gt_sim = ifelse(sd_sd_obs >= sd_sd_sim, TRUE, FALSE))
```

# Summary 

Labroo and Nielsen (2010) Experiment 1 "demonstrates a positive effect of embodied movement in space toward an otherwise aversive product."

In table 1 of the paper, the standard devations within measures are quite similar to one another. 

![Table 1](Table1.jpg)

Simonsohn (2013) found that similar standard deviations were unlikely to occur do to chance. 
![Table 2](SimonsohnTable1.jpg)

I follow the analysis strategy of Simonsohn below. 

# SD Similarity 

## Within measures

Density plots of the simulated standard deviations for each of the measures reported in Table 1 of Labroo and Nielsen. 
Vertical Lines represent the observed standard deviation of standard deviations. 
Note that the observed standard deviations are all unlikely


```r
# plot the histograms
ggplot(tbl1_reps_sum, aes(x = sd_sd_sim)) +
  geom_density() +
  geom_vline(aes(xintercept = sd_sd_obs)) +
  facet_grid(variable ~ .) +
  theme_classic()
```

![](Labroo-Nielsen-analysis_files/figure-html/Plot-1.png)<!-- -->


## Combined measures

Now combining the standard deviations across measures.  
From Simonsohn 2013 pg. 1878

>  Before aggregating, however, I had to get around the problem that the studies differed in scale (e.g., grams of hot sauce vs.
number of fish) and sample size (n = 15 vs. n = 20).
An easy way to do this was to divide the standard
deviation of the standard deviations by the standard error
of the (pooled) standard deviation. This yielded an intuitive measure of deviation the number of standard errors by which the standard deviations differed within a given study. I refer to this number as Ψ

NOTE: Whereas Simonsohn (2013) combined dependent variables across multiple studies, I am combining across multiple measures within a single study.
This should not be an issue unless we believe that there is a correlation between standard deviations. Given that there is no a priori reason why measures would be correlated with one another, I continue assuming they are uncorrelated. 


```r
sim_meanpsi <- tbl1_reps_sum %>%
  group_by(n_rep) %>%
  # get mean psi for each replication
  summarise(mean_psi_sim = mean(psi_sim),
            psi_obs = psi_obs[1]) %>%
  # get the number of simulated SD's which are larger than the observed
  mutate(simpsi_gt_stupsi = ifelse(mean_psi_sim > psi_obs, TRUE, FALSE))
```

Of the simulated standard deviations, 100000 of the 10^{5} had higher $\psi$ values than the observed $\psi$ value. That is, the combined measure of the variability of standard deviation was higher for every simulation than the observed data. 


Density plot of the simulated $\psi$ values. 
The observed $\psi$ value is denoted by a vertical line. 
Note that the observed $\psi$ is below any of the simulated $\psi$ values. 


```r
sim_meanpsi %>% 
  ggplot(aes(x = mean_psi_sim)) +
  geom_density() +
  geom_vline(aes(xintercept = psi_obs)) +
  theme_classic()
```

![](Labroo-Nielsen-analysis_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

# Further note

This study is on a similar topic to Sanna et al (2011). 
Both Sanna and Nielsen were affiliated with the University of North Carolina. http://u.arizona.edu/~jesper/Vita.pdf
https://sites.google.com/view/lawrencejsanna/employment?authuser=0
