library(tidyverse)    # Data wrangling
library(sf)           # Spatial analysis
library(caret)        # For machine learning and preprocessing
library(lme4)         # For random effects models
library(sandwich)     # For clustered se
library(lmtest)     # For clustered se

library(sjPlot)
library(sjmisc)
library(stargazer)
library(scales)      # for rescale function

df <- read.csv("../midsave/regression.csv")

results = data.frame()

temp <- df %>% 
  mutate(
    cp_per_1000 = log(cpc_per_1000),
    yt_per_1000 = log(yt_per_1000),
    wa_per_1000 = log(wa_per_1000),
    tor_per_1000 = log(tor_per_1000),
    code_reg = as.character(code_reg),
    code_dep = as.character(code_dep),
    Sexual_violence_per_1000 = sv_com_17_21,
    Densité.de.population..historique.depuis.1876..2020 = log(Densité.de.population..historique.depuis.1876..2020)) %>% 
  select(-starts_with('log'),
         -starts_with('cpc'),
         -sv_17_21, -sv_com_17_21) %>% 
  rename('log_cpc_per_1000' = cp_per_1000,
         'log_YouTube_per_1000' = yt_per_1000,
         'log_Web_Adult_per_1000' = wa_per_1000,
         'log_Tor_per_1000' = tor_per_1000,
         'log_pop_density' = Densité.de.population..historique.depuis.1876..2020,
         'Share_of_singles' = Part.des.pers..de.15.ans.ou...célibataires.2020,
         'Poverty_rate' = Taux.de.pauvreté.2020,
         'Drug_abuse_rate' = Usage.de.stupéfiants..taux..2022,
         'Employment_rate' = Taux.de.chômage.annuel.moyen.2022,
         'Electoral_turnout_2017' = X..Exp.Ins,
         'Share_Le_Pen' = X..Voix.Ins_lepen,
         'Share_Macron' = X..Voix.Ins_macron,
         'POI_adult_entertainment' = adult_entertainment,
         'POI_sports_teams' = amateur_sports_team,
         'POI_church' = church_cathedral,
         'POI_mosque' = mosque,
         'POI_religious_org' = religious_organization,
         'POI_school' = school,
         'queries_csa' = lin_1,
         'queries_ambiguous' = lin_2,
         'queries_focus' = lin_3
  )  %>% 
  select(-code_com, -code_epci,
         -code_dep, 
         -code_reg,
         -queries_focus,
         -queries_csa,
         -queries_ambiguous,
         -PC2,
         -Drug_abuse_rate
  ) %>% 
  drop_na

temp <- temp %>%
  mutate(across(('Share_of_singles':'Share_Macron'), ~ .x/100)) %>%
  mutate(across(('POI_adult_entertainment':'POI_school'), ~ scale(.x, scale = T))
  )

# Estimating CPC
cpc_fit1 <- lm('log_cpc_per_1000 ~ .', temp %>% 
                 select(-log_YouTube_per_1000, -log_Web_Adult_per_1000, -log_Tor_per_1000))
cpc_fit2 <- lm('log_cpc_per_1000 ~ .', temp %>% 
                 filter(Sexual_violence_per_1000>0) %>% 
                 select(-log_YouTube_per_1000, -log_Web_Adult_per_1000, -log_Tor_per_1000))
cpc_fit3 <- lm('log_cpc_per_1000 ~ .', temp %>% 
                 select(-log_YouTube_per_1000, -log_Web_Adult_per_1000))
cpc_fit4 <- lm('log_cpc_per_1000 ~ .', temp %>% 
                 filter(Sexual_violence_per_1000>0) %>% 
                 select(-log_YouTube_per_1000, -log_Web_Adult_per_1000))
cpc_fit5 <- lm('log_cpc_per_1000 ~ .', temp)
cpc_fit6 <- lm('log_cpc_per_1000 ~ .', temp %>% 
                 filter(Sexual_violence_per_1000>0))

cpc_fit1_cse <- coeftest(cpc_fit1, vcov = vcovHC)[, c("Estimate", "Pr(>|t|)")]['Sexual_violence_per_1000',]
cpc_fit2_cse <- coeftest(cpc_fit2, vcov = vcovHC)[, c("Estimate", "Pr(>|t|)")]['Sexual_violence_per_1000',]
cpc_fit3_cse <- coeftest(cpc_fit3, vcov = vcovHC)[, c("Estimate", "Pr(>|t|)")]['Sexual_violence_per_1000',]
cpc_fit4_cse <- coeftest(cpc_fit4, vcov = vcovHC)[, c("Estimate", "Pr(>|t|)")]['Sexual_violence_per_1000',]
cpc_fit5_cse <- coeftest(cpc_fit5, vcov = vcovHC)[, c("Estimate", "Pr(>|t|)")]['Sexual_violence_per_1000',]
cpc_fit6_cse <- coeftest(cpc_fit6, vcov = vcovHC)[, c("Estimate", "Pr(>|t|)")]['Sexual_violence_per_1000',]

# Estimating Sexual Violence

sv_fit1 <- lm('Sexual_violence_per_1000 ~ .', temp %>% 
                select(-log_YouTube_per_1000, -log_Web_Adult_per_1000, -log_Tor_per_1000))
sv_fit2 <- lm('Sexual_violence_per_1000 ~ .', temp %>% 
                filter(Sexual_violence_per_1000>0) %>% 
                select(-log_YouTube_per_1000, -log_Web_Adult_per_1000, -log_Tor_per_1000))
sv_fit3 <- lm('Sexual_violence_per_1000 ~ .', temp %>% 
                select(-log_YouTube_per_1000, -log_Web_Adult_per_1000))
sv_fit4 <- lm('Sexual_violence_per_1000 ~ .', temp %>% 
                filter(Sexual_violence_per_1000>0) %>% 
                select(-log_YouTube_per_1000, -log_Web_Adult_per_1000))
sv_fit5 <- lm('Sexual_violence_per_1000 ~ .', temp)
sv_fit6 <- lm('Sexual_violence_per_1000 ~ .', temp %>% 
                filter(Sexual_violence_per_1000>0))

sv_fit1_cse <- coeftest(sv_fit1, vcov = vcovHC)[, c("Estimate", "Pr(>|t|)")]['log_cpc_per_1000',]
sv_fit2_cse <- coeftest(sv_fit2, vcov = vcovHC)[, c("Estimate", "Pr(>|t|)")]['log_cpc_per_1000',]
sv_fit3_cse <- coeftest(sv_fit3, vcov = vcovHC)[, c("Estimate", "Pr(>|t|)")]['log_cpc_per_1000',]
sv_fit4_cse <- coeftest(sv_fit4, vcov = vcovHC)[, c("Estimate", "Pr(>|t|)")]['log_cpc_per_1000',]
sv_fit5_cse <- coeftest(sv_fit5, vcov = vcovHC)[, c("Estimate", "Pr(>|t|)")]['log_cpc_per_1000',]
sv_fit6_cse <- coeftest(sv_fit6, vcov = vcovHC)[, c("Estimate", "Pr(>|t|)")]['log_cpc_per_1000',]

sv_fit = data.frame(est = c(sv_fit1_cse[1], sv_fit2_cse[1], sv_fit3_cse[1],
                            sv_fit4_cse[1], sv_fit5_cse[1], sv_fit6_cse[1]),
                    p_val = c(sv_fit1_cse[2], sv_fit2_cse[2], sv_fit3_cse[2],
                              sv_fit4_cse[2], sv_fit5_cse[2], sv_fit6_cse[2]),
                    adj_r = c(summary(sv_fit1)$adj.r.squared, summary(sv_fit2)$adj.r.squared,
                              summary(sv_fit3)$adj.r.squared,summary(sv_fit4)$adj.r.squared,
                              summary(sv_fit5)$adj.r.squared,summary(sv_fit6)$adj.r.squared),
                    type = c(1,2,3,4,5,6),
                    coef = 'log_cpc_per_1000')

cpc_fit = data.frame(est = c(cpc_fit1_cse[1], cpc_fit2_cse[1], cpc_fit3_cse[1],
                             cpc_fit4_cse[1], cpc_fit5_cse[1], cpc_fit6_cse[1]),
                     p_val = c(cpc_fit1_cse[2], cpc_fit2_cse[2], cpc_fit3_cse[2],
                               cpc_fit4_cse[2], cpc_fit5_cse[2], cpc_fit6_cse[2]),
                     adj_r = c(summary(cpc_fit1)$adj.r.squared, summary(cpc_fit2)$adj.r.squared,
                               summary(cpc_fit3)$adj.r.squared,summary(cpc_fit4)$adj.r.squared,
                               summary(cpc_fit5)$adj.r.squared,summary(cpc_fit6)$adj.r.squared),
                     type = c(1,2,3,4,5,6),
                     coef = 'Sexual_violence_per_1000')


results = results %>% 
  bind_rows(sv_fit) %>% 
  bind_rows(cpc_fit)

results %>% filter(p_val <= 0.1) %>% filter(type >= 5) %>% filter(est >= 0)

cpc_fit1_cse <- coeftest(cpc_fit1, vcov = vcovHC)
sv_fit1_cse <- coeftest(sv_fit1, vcov = vcovHC)

cpc_fit2_cse <- coeftest(cpc_fit2, vcov = vcovHC)
sv_fit2_cse <- coeftest(sv_fit2, vcov = vcovHC)

cpc_fit3_cse <- coeftest(cpc_fit3, vcov = vcovHC)
sv_fit3_cse <- coeftest(sv_fit3, vcov = vcovHC)

cpc_fit4_cse <- coeftest(cpc_fit4, vcov = vcovHC)
sv_fit4_cse <- coeftest(sv_fit4, vcov = vcovHC)

cpc_fit5_cse <- coeftest(cpc_fit5, vcov = vcovHC)
sv_fit5_cse <- coeftest(sv_fit5, vcov = vcovHC)

cpc_fit6_cse <- coeftest(cpc_fit6, vcov = vcovHC)
sv_fit6_cse <- coeftest(sv_fit6, vcov = vcovHC)

tab_model(cpc_fit1, sv_fit1)
stargazer(cpc_fit1_cse, sv_fit1_cse,
          title="Results", digits = 2)

tab_model(cpc_fit2, sv_fit2)
stargazer(cpc_fit2_cse, sv_fit2_cse,
          title="Results", digits = 2)

tab_model(cpc_fit3, sv_fit3)
stargazer(cpc_fit3_cse, sv_fit3_cse,
          title="Results", digits = 2)

tab_model(cpc_fit4, sv_fit4)
stargazer(cpc_fit4_cse, sv_fit4_cse,
          title="Results", digits = 2)

tab_model(cpc_fit5, sv_fit5)
stargazer(cpc_fit5_cse, sv_fit5_cse,
          title="Results", digits = 2)

tab_model(cpc_fit6, sv_fit6)
stargazer(cpc_fit6_cse, sv_fit6_cse,
          title="Results", digits = 2)

tab_model(cpc_fit5, cpc_fit6)
stargazer(cpc_fit5_cse, cpc_fit6_cse,
          title="Results", digits = 2)

tab_model(sv_fit5, sv_fit6)
stargazer(sv_fit5_cse, sv_fit6_cse,
          title="Results", digits = 2)

