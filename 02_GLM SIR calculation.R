
library(dplyr)
library(purrr)
library(regclass)
library(data.table)

# OG
full_dat <- read.csv("data/clean_birth_data.csv")
# outcome: placental insufficiency
full_dat$hypertension_code <- ifelse(full_dat$hypertension %in% c("C", "E", "H", "P"),
                                     "hyp", "no")
full_dat$pl_insuf <- case_when(full_dat$sga10_by_sex > full_dat$birthweightgrams ~ 1, # sga
                               full_dat$hypertension %in% c("H", "E", "P") ~ 1)
full_dat$pl_insuf <- ifelse(is.na(full_dat$pl_insuf), 0, 1)

# outcome: gestational hypertension
full_dat$gestational_hyp <- ifelse(full_dat$hypertension %in% c("H", "E", "P"), 1, 0)

# outcome: gestational diabetes
full_dat$gestational_diab <- ifelse(full_dat$gestationaldiabetes == 1, 1, 0)

# outcome: preterm birth
full_dat$preterm <- ifelse(full_dat$gestation < 34, "preterm", "term")
full_dat$preterm_code <- ifelse(full_dat$preterm == "preterm", 1, 0)

# outcome: very preterm birth
full_dat$very_preterm <- ifelse(full_dat$gestation < 30, "preterm", "term")

# *negative control* outcome: gender
full_dat$gender_code <- ifelse(full_dat$gender == "M", 1, 0)

# grab confounders
dat <- fread("/Users/brenna/Downloads/birth_exposures_with_confounders.csv")

confounders <- dat[, c("momid", "grid_id", "birthccyy", "birthmm", 
                       "mommedicaid_char", "mommaritalstatus_char",
                       "momwic_char", "momeducation_char", "momage",
                       "poverty_rate")]
rm(dat)

# and a few more
mom <- read.csv('../../data/health data/births.csv')
mom_2013 <- fread('/Users/brenna/Downloads/births 2013+.csv')
mom_2016 <- fread('/Users/brenna/Downloads/births 2016+.csv')

mom <- rbind(mom_2013, mom_2016)

mom <- mom |>
  mutate(MomHeightFeet = as.numeric(MomHeightFeet),
         MomHeightFeet = ifelse(MomHeightFeet == 1, NA, MomHeightFeet),
         MomHeightInches = as.numeric(MomHeightInches),
         race_ethn = case_when(MomHispanicOrigin == 1 | MomHispanicMex == 1 |
                                 MomHispanicCuban == 1 | MomHispanicPuertoRican == 1 |
                                 MomHispanicOther == 1 | MomHispanicOtherSpec != "" |
                                 !MomHispanicOtherSpec %in% c("*", "") ~ # primacy
                                 "hispanic",
                               MomRaceWhite == 1 ~ "white",
                               MomRaceBlack == 1 ~ "black",
                               MomRaceAmIndian == 1 ~ "amindian",
                               MomRaceChinese == 1 | MomRaceJapanese == 1 |
                                 MomRaceFilipino == 1 | MomRaceOtherAsian == 1 |
                                 MomRaceAsianIndian == 1 | MomRaceKorean == 1 |
                                 MomRaceVietnamese == 1 ~ "asian",
                               MomRaceHawaiian == 1 | MomRaceSamoan == 1 |
                                 MomRaceGuamanian == 1 | MomRacePacIslander == 1 |
                                 MomRaceTongan == 1 ~ "nhopi",
                               MomRaceOther == 1 | MomRaceOtherSpec1 == 1 |
                                 MomRaceOtherSpec2 == 1 | MomRaceUnknown == 1 ~ "other"),
         race_ethn = ifelse(is.na(race_ethn), "other", race_ethn),
         # there's one maternal_wt = 550; 
         # this could be a recording error, but it's prior to 2013 so don't bother with it
         MomWeightPrior = as.numeric(MomWeightPrior),
         # bmi = ((lbs * 703) / inches) / inches
         mom_bmi = ((MomWeightPrior * 703) / 
                      (12 * MomHeightFeet + MomHeightInches)) / 
           (12 * MomHeightFeet + MomHeightInches))

names(mom) <- tolower(names(mom))
mom <- mom[, c("momid", "birthccyy", "birthmm",
               "race_ethn")] #, "mom_bmi", "momheightfeet",
               #"momheightinches", "momweightprior"
               # note: too much missing in BMI
mom$momid <- as.numeric(mom$momid)

# and a couple more
table(full_dat[, c("prevpregnowliving", "prevpregnowdead", "prevpregstillborn")])
# none — these were excluded; our cohort is nulligravida (as far as we know)

# birth data
full_dat <- merge(full_dat, mom, by = c("momid", "birthccyy", "birthmm"))
z <- read.csv("data/birth_traj_trunc_jun08.csv")

z_y <- merge(z, full_dat[, c("momid", "grid_id", "birthccyy", "birthmm",
                             "sga", "hypertension_code",
                             "pl_insuf", "preterm", "preterm_code",
                             "gestational_hyp", "very_preterm",
                             "gestational_diab", "gender_code",
                             "race_ethn")], 
             by = c("momid", "grid_id")) # N = 44984

dat_con <- merge(z_y, confounders, by = c("momid", "grid_id", 
                                          "birthccyy", "birthmm"))

dat_con$poverty_rate <- ifelse(is.na(dat_con$poverty_rate), 0, dat_con$poverty_rate)
dat_con$poverty_rate_sc <- (dat_con$poverty_rate - mean(dat_con$poverty_rate)) / 10



recode_fx <- function(x) {
  x = case_when(x == 10 ~ "1",
                x == 0 ~ "rm")
  x = ifelse(is.na(x), "0", x)
  return(x)
}

store_sir <- list()

for(i in c(1:12)) {
  
  recode_fx <- function(x) {
    x = case_when(x == i ~ "1",
                  x == 0 ~ "rm")
    x = ifelse(is.na(x), "0", x)
    return(x)
  }
  
  dat_recoded <- dat_con |>
    mutate(across(week_1:week_33, ~ recode_fx(.)))
  
  cluster_week = list()
  
  for(j in 1:33) {
    j = j + 5
    
    dat_recoded_filter <- dat_recoded |>
      filter(dat_recoded[, j] != "rm")
    
    preterm_noncluster = table(dat_recoded_filter[, j], dat_recoded_filter$preterm)[1]
    preterm_cluster = table(dat_recoded_filter[, j], dat_recoded_filter$preterm)[2]
    term_noncluster = table(dat_recoded_filter[, j], dat_recoded_filter$preterm)[3]
    term_cluster = table(dat_recoded_filter[, j], dat_recoded_filter$preterm)[4]
    
    if(preterm_cluster == 0) {
      df <- data.frame(cluster = i,
                       week = j - 5,
                       sample_size = nrow(dat_recoded_filter),
                       preterm_noncluster = preterm_noncluster,
                       preterm_cluster = preterm_cluster,
                       term_noncluster = term_noncluster,
                       term_cluster = term_cluster,
                       rate_preterm_exposed = preterm_cluster / (preterm_noncluster + preterm_cluster),
                       rate_term_exposed = term_cluster / (term_noncluster + term_cluster),
                       sir = NA, #m1$coefficients[[("dat_recoded_filter[, j]1")]],
                       lwr = NA, #confint(m1, "dat_recoded_filter[, j]1")[[1]],
                       upr = NA) #confint(m1, "dat_recoded_filter[, j]1")[[2]])
      
      cluster_week[[j - 5]] <- df
    } else{
      m1 <- glm(preterm_code ~ dat_recoded_filter[, j] + poverty_rate_sc + 
                  momage + momeducation_char + race_ethn,
                family = binomial(link = "logit"),
                data = dat_recoded_filter)
      
      # print(summary(m1))

      df <- data.frame(cluster = i,
                       week = j - 5,
                       sample_size = nrow(dat_recoded_filter),
                       preterm_noncluster = preterm_noncluster,
                       preterm_cluster = preterm_cluster,
                       term_noncluster = term_noncluster,
                       term_cluster = term_cluster,
                       rate_preterm_exposed = preterm_cluster / (preterm_noncluster + preterm_cluster),
                       rate_term_exposed = term_cluster / (term_noncluster + term_cluster),
                       sir = m1$coefficients[[("dat_recoded_filter[, j]1")]],
                       lwr = confint(m1, "dat_recoded_filter[, j]1")[[1]],
                       upr = confint(m1, "dat_recoded_filter[, j]1")[[2]],
                       p = coef(summary(m1))[2, 4],
                       p_bon = coef(summary(m1))[2, 4] * 396)
      print(df)
      cluster_week[[j - 5]] <- df
    }
  }
  
  store_sir[[i]] <- cluster_week
  
}





sir_unlist <- data.frame()

for(i in 1:length(store_sir)) {
  test <- map_df(store_sir[[i]], ~as.data.frame(.))
  sir_unlist <- rbind(sir_unlist, test)
}

# sir_unlist <- rbind(map_df(store_sir[[5]], ~as.data.frame(.)),
#                     map_df(store_sir[[10]], ~as.data.frame(.)))

sir_unlist$sir_exp <- exp(sir_unlist$sir)
sir_unlist$lwr_exp <- exp(sir_unlist$lwr)
sir_unlist$upr_exp <- exp(sir_unlist$upr)

sir_unlist[which(sir_unlist$p_bon < 0.05), ]

sir_unlist$significant <- ifelse(sir_unlist$p_bon < 0.05, "significant", "not")
cols <- c("significant" = "red", "not" = "black")

## plots
sir_unlist |>
  filter(cluster %in% c(5, 10)) |>
  ggplot(aes(x = week, y = sir_exp)) +
  geom_point(aes(colour = significant)) +
  geom_linerange(aes(ymin = lwr_exp, ymax = upr_exp, colour = significant)) +
  scale_colour_manual(values = cols) +
  geom_hline(yintercept = 1) +
  theme_classic() +
  facet_wrap(~ cluster, scales = "fixed") +
  ggtitle("PTB") +
  ylab("RR")

ggplot(sir_unlist, aes(x = week, y = sir_exp)) +
  geom_point(aes(colour = significant)) +
  # scale_fill_manual(values = cols) +
  geom_linerange(aes(ymin = lwr_exp, ymax = upr_exp, colour = significant)) +
  scale_colour_manual(values = cols) +
  geom_hline(yintercept = 1) +
  theme_classic() +
  facet_wrap(~ cluster, scales = "fixed") +
  ggtitle("PTB")

sir_unlist[which(sir_unlist$cluster == 5), ]
  

#####
# out of interest, testing exposure to cluster 10 and cluster 5
#####

recode_fx_10 <- function(x) {
  x = case_when(x == 10 ~ "1",
                x == 0 ~ "rm")
  x = ifelse(is.na(x), "0", x)
  return(as.factor(x))
}

recode_fx_5 <- function(x) {
  x = case_when(x == 5 ~ "1",
                x == 0 ~ "rm")
  x = ifelse(is.na(x), "0", x)
  return(x)
}


dat_recoded <- dat_con |>
  mutate(across(week_1:week_40, ~ recode_fx_10(.)),
         across(week_1:week_40, ~ as.factor(.)))

# # dat_recoded$week_11 <- as.factor(dat_recoded$week_11)
# dat_recoded$week_28 <- as.factor(dat_recoded$week_28)

dat_recoded_filter <- dat_recoded |>
      filter(week_20 != "rm") # removing person-weeks assigned to cluster 0 <?>; was 28

names(dat_con)[6:45]

# time-to-event; format long
library(tidyr)
test <- dat_recoded_filter |>
  pivot_longer(cols = 6:45, names_to = "week",
               values_to = "person_cluster")

test <- test |>
  mutate(week = as.numeric(
    gsub("week_", "", test$week))) |>
  filter(person_cluster != "rm")

test$person_cluster <- as.factor(ifelse(test$person_cluster == 1, 1, 0))
test$week <- as.factor(test$week)
test$momid <- as.factor(test$momid)

library(INLA)

# test_m3 <- inla(preterm_code ~ f(person_cluster)*week +
#                   momeducation_char + race_ethn, # + poverty_rate_sc, 
#                 family = "binomial",
#                 data = test)
test_m3 <- glm(preterm_code ~ week*person_cluster +
                 momeducation_char + race_ethn, # + poverty_rate_sc,
               family = binomial(link = "logit"),
               data = test)
summary(test_m3)
summary(test_m2)
summary(test_m1)

mx <- glm(preterm_code ~ #week_1 + week_2 + week_3 + week_4 + week_5 + week_6 + week_7 + week_8 + 
            week_9 + week_10 + week_11 +
          week_12 + week_13 + week_14 + week_15 + 
            #week_16 + week_17 + week_18 + week_19 + week_20 + week_21 + week_22 +
          #week_23 + week_24 + week_25 + week_26 + week_27 + week_28 + week_29 + week_30 + week_31 + week_32 + week_33 +
            poverty_rate_sc + 
            momage + momeducation_char + race_ethn,
          family = binomial(link = "logit"),
          data = dat_recoded)
summary(mx)

VIF(mx)


# bonferroni
alpha = 0.05
0.05 / 396 # 0.0001262
1 - (1 - alpha)^(1 / (396)) # 2.590537e-05



preterm_noncluster = table(dat_recoded_filter[, "week_11"], dat_recoded_filter$preterm)[1],
preterm_cluster = table(dat_recoded_filter[, "week_11"], dat_recoded_filter$preterm)[2],
term_noncluster = table(dat_recoded_filter[, "week_11"], dat_recoded_filter$preterm)[3],
term_cluster = table(dat_recoded_filter[, "week_11"], dat_recoded_filter$preterm)[4],
rate_preterm_exposed = preterm_cluster / (preterm_noncluster + preterm_cluster),
rate_term_exposed = term_cluster / (term_noncluster + term_cluster)


# must do models for cluster 3 outside the loop

dat_con$race_ethn <- relevel(as.factor(dat_con$race_ethn), ref = "white")

summary(
  m1 <- glm(preterm_code ~ week_11 + poverty_rate_sc + 
              momage + momeducation_char + race_ethn,
    family = binomial(link = "logit"),
    data = dat_con)
)


confint(m1, paste0("week_11", "1"))

VIF(m1)
summary(m1)


############
## trajectories, for duration
############

# dat_con$duration_10 <- apply(dat_con[, 6:36], 1, function(x) length(which(x=="10"))) # all weeks

durations <- data.frame()

for(i in 1:6) {
  dat_con$duration_10 <- apply(dat_con[, 14:19], 1, function(x) length(which(x==as.character(i))))
  # dat_con$duration_10 <- apply(dat_con[, 30:34], 1, function(x) length(which(x=="5")))
  # use 9-15 (that's when it's significant for cluster 10 prior to bonferroni)
  # for cluster 5, use 25-29 (when it's significant for cluster 5 prior to bonferroni)
  names(dat_con)
  
  test <- as.data.frame(table(dat_con$duration_10,
                              dat_con$preterm))
  names(test) <- c("duration", "preterm", "count")
  
  test <- test |>
    pivot_wider(id_cols = "duration", values_from = "count", names_from = "preterm")
  
  test$preterm_denom <- 945
  test$term_denom <- 44039
  
  test$rate_exposed_preterm <- test$preterm / test$preterm_denom
  test$rate_exposed_term <- test$term / test$term_denom
  
  plot(test$duration, (test$rate_exposed_preterm / test$rate_exposed_term),
       xlab = "weeks of exposure to cluster 10 in 9-14",
       ylab = "relative rate")
  
  
  test_duration <- dat_con |>
    mutate(exp_0 = as.factor(ifelse(duration_10 == 0, 1, 0)),
           exp_1 = as.factor(ifelse(duration_10 == 1, 1, 0)),
           exp_2 = as.factor(ifelse(duration_10 == 2, 1, 0)),
           exp_3 = as.factor(ifelse(duration_10 == 3, 1, 0)),
           exp_4 = as.factor(ifelse(duration_10 == 4, 1, 0)),
           exp_5 = as.factor(ifelse(duration_10 == 5, 1, 0)),
           exp_6 = as.factor(ifelse(duration_10 == 6, 1, 0)))#,
           #exp_7 = as.factor(ifelse(duration_10 == 7, 1, 0)),
           #exp_8 = as.factor(ifelse(duration_10 == 8, 1, 0)))#,
  # exp_9 = as.factor(ifelse(duration_5 == 9, 1, 0)),
  # exp_10 = as.factor(ifelse(duration_5 == 10, 1, 0)),
  # exp_11 = as.factor(ifelse(duration_5 == 11, 1, 0)))#,
  # exp_12 = as.factor(ifelse(duration_10 == 12, 1, 0)),
  # exp_13 = as.factor(ifelse(duration_10 == 13, 1, 0)),
  # exp_14 = as.factor(ifelse(duration_10 == 14, 1, 0)),
  # exp_15 = as.factor(ifelse(duration_10 == 15, 1, 0)),
  # exp_16 = as.factor(ifelse(duration_10 == 16, 1, 0)),
  # exp_17 = as.factor(ifelse(duration_10 == 17, 1, 0)),
  # exp_18 = as.factor(ifelse(duration_10 == 18, 1, 0)),
  # exp_19 = as.factor(ifelse(duration_10 == 19, 1, 0)),
  # exp_20 = as.factor(ifelse(duration_10 == 20, 1, 0)),
  # exp_21 = as.factor(ifelse(duration_10 == 21, 1, 0)))
  
  summary(
    m2 <- glm(preterm_code ~ exp_1 + exp_2 + exp_3 + exp_4 + 
                #exp_5 + exp_6 + #exp_7 + #exp_8 + #exp_9 + 
                # exp_10 + exp_11 + #exp_12 + exp_13 + exp_14 + 
                # exp_15 + exp_16 + exp_17 +# exp_18 + exp_19 + 
                # exp_20 + exp_21 +
                #poverty_rate_sc + 
                momage + momeducation_char + race_ethn,
              family = binomial(link = "logit"),
              data = test_duration)
  )
  
  
  
  names(dat_con)
  
  
  # VIF(m2)
  summary(m2)
  # library(aod)
  # wald.test(b = coef(m2), Sigma = vcov(m2), Terms = 2:9)
  
  
  n_durations_start <- 2
  n_durations <- length(names(m2$coefficients[2:7])) + 1
  
  
  duration_exposure <- data.frame(cluster = i,
                                  duration = gsub("exp_", "", names(m2$coefficients[n_durations_start:n_durations])),
                                  sir = m2$coefficients[n_durations_start:n_durations],
                                  lwr = confint(m2, names(m2$coefficients[n_durations_start:n_durations]))[, 1],
                                  upr = confint(m2, names(m2$coefficients[n_durations_start:n_durations]))[, 2])#,
  # p = coef(summary(m1))[n_durations_start:n_durations, 4])
  duration_exposure$duration <- str_sub(duration_exposure$duration, start = 1, 
                                        end = (nchar(duration_exposure$duration) - 1))
  duration_exposure$duration <- as.numeric(duration_exposure$duration)
  
  duration_exposure$sir_exp <- exp(duration_exposure$sir)
  duration_exposure$lwr_exp <- exp(duration_exposure$lwr)
  duration_exposure$upr_exp <- exp(duration_exposure$upr)
  
  durations <- rbind(durations, duration_exposure)
  
  #cluster_12 <- 
  test <- ggplot(duration_exposure, aes(x = duration, y = sir_exp)) +
    geom_hline(yintercept = 0:6, linetype = 'dashed', alpha = 0.5) +
    geom_point() +
    geom_linerange(aes(ymin = lwr_exp, ymax = upr_exp)) +
    geom_hline(yintercept = 1) +
    theme_classic() +
    ggtitle(paste0("PTB, cluster ", i)) +
    ylim(0, 7)
  
  plot(test)
  
}


#### out of the loop

dat_con$duration_10 <- apply(dat_con[, 14:19], 10, function(x) length(which(x==as.character(i))))

library(stringr)

dat_con$duration_10 <- paste(dat_con$week_9, dat_con$week_10,
                             dat_con$week_11, dat_con$week_12,
                             dat_con$week_13, dat_con$week_14)

dat_con$duration_10 <- str_count(dat_con$duration_10, "10")

dat_con$duration_10 <- as.factor(dat_con$duration_10)

summary(
  m2 <- glm(preterm_code ~ duration_10 + 
              #exp_5 + exp_6 + #exp_7 + #exp_8 + #exp_9 + 
              # exp_10 + exp_11 + #exp_12 + exp_13 + exp_14 + 
              # exp_15 + exp_16 + exp_17 +# exp_18 + exp_19 + 
              # exp_20 + exp_21 +
              #poverty_rate_sc + 
              momage + momeducation_char + race_ethn,
            family = binomial(link = "logit"),
            data = dat_con)
)




library(ggplot2)

ggplot(durations, aes(x = duration, y = sir_exp)) +
  geom_hline(yintercept = 0:6, linetype = 'dashed', alpha = 0.5) +
  geom_point() +
  geom_linerange(aes(ymin = lwr_exp, ymax = upr_exp)) +
  geom_hline(yintercept = 1) +
  theme_classic() +
  ggtitle(paste0("PTB, cluster ", i)) +
  ylim(0, 7) +
  facet_wrap(~ cluster)


# cluster_12

# ggarrange(cluster_01, cluster_02,
#           cluster_03, cluster_04,
#           cluster_05, cluster_06,
#           cluster_07, cluster_08,
#           cluster_09, cluster_10,
#           cluster_11, cluster_12)

# cluster_5 <- ggplot(duration_exposure, aes(x = duration, y = sir_exp)) +
#   geom_hline(yintercept = 0:4, linetype = 'dashed', alpha = 0.5) +
#   geom_point() +
#   geom_linerange(aes(ymin = lwr_exp, ymax = upr_exp)) +
#   geom_hline(yintercept = 1) +
#   theme_classic() +
#   ggtitle("PTB, cluster 5") +
#   ylim(0, 5)

library(ggpubr)
ggarrange(cluster_5, cluster_10, ncol = 2)

######
# table one
######
library(moments)
library(tableone)

head(dat_con)

# hist(dat_con$momage)
# skewness(dat_con$poverty_rate)
# hist(dat_con$birthccyy)

tab1 <- CreateTableOne(vars = c("momage", "poverty_rate",
                                "race_ethn", "momeducation_char",
                                "birthccyy"), 
                       factorVars = c("race_ethn", "momeducation_char",
                                      "birthccyy"),
                       strata = c("preterm"), data = dat_con,
                       testNonNormal = kruskal.test)
tab1




