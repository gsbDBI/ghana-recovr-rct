---
title: "Ghana RECOVR Experiment Analysis"
author: "Molly Offer-Westort, Erika Kirgios"
date: "5/19/2020"
output: 
  html_document:
    keep_md: true
    highlight: haddock
    theme: journal
    number_sections: no
    toc: yes
    toc_depth: 3
    toc_float: yes
---

<style>

table, td, th {
  border: none;
  padding-left: 1em;
  padding-right: 1em;
  margin-left: auto;
  margin-right: auto;
  margin-top: 1em;
  margin-bottom: 1em;
}

</style>







## Overivew
The Ghana Core RECOVR project was launched [XXX] by Innovations for Poverty Action, as a nationally representative phone survey delivered to approximately 4,000 respondents. The objective of the study is to [XXX]. 

The component of the study featured here is an experimental intervention included in the phone survey, with the objective of using nudges to increase information-seeking behavior. 


Treated participants were asked to *name and reflect on someone at high risk of dying from coronavirus who they care about*. Control participants were not asked these questions. Treatment was assigned to randomly sorted telephone numbers prior to the intervention. 

*The script of the Ghana Core RECOVR survey is linked  [here](https://docs.google.com/spreadsheets/d/1uqAGQHUpbxKGCtAXBXmUp77TwE52Rk0PZgYN2J-WXuo/edit?usp=sharing).*


### Primary hypothesis

The primary hypothesis is that treated subjects will be more likely to 

(1) indicate willingness to comply with recommendations for reducing the spread of coronavirus like social distancing, hand washing, etc. and 
(2) seek more information about coronavirus. 




## Data

### Treatment and dependent variable  


```r
dat <- read_csv('../data/ghana_recovr.csv')
dat$treat <- 1*(dat$rand1 == 'Identifiable victim')

# some people did not recieve treatment assignment
table(dat$treat, useNA = 'ifany')
```

```
## 
##    0    1 <NA> 
##  402  370   28
```

```r
# keep only if consented & received treatment assignment
dat <- dat %>% 
  filter(consent_survey == 'Yes',
         !is.na(treat))
```


We have two key dependent variables: (1) behavior change intentions and (2) information seeking.

**Behavior change intentions:** Participants are asked “Would you be willing to cancel social activities, avoid going to the markets and crowded places as much as possible, and wash your hands often with soap and water if possible to protect yourself, your loved ones, and other Ghanaians?” The dependent variable is a binary indicator for whether a participant responded with “Yes”. “No” and refusal to respond will both be coded as 0. 

**Information seeking:** We will offer participants the opportunity to learn about a free service they can use to get more information about coronavirus. Specifically, we will tell them “The best way to protect yourself, your loved ones, and other Ghanaians is to stay informed about COVID-19 and how to prevent its spread. Would you like me to provide you with a phone number you can call to get free and accurate information about coronavirus and learn how you can keep yourself, your loved ones, and other Ghanaians safe?” The dependent variable is a binary indicator for whether participants said “Yes” to this offer. Participants who refuse to reply will be coded as 0 along with those who say “no”.

**Confirm experiment was delivered as expected:**


```r
# *Treatment delivery
# (EXP2) Is there anyone you care about who is at high risk of dying from
# coronavirus?
# - confirm only responses for treated individuals
table(dat$treat, dat$exp2, useNA = 'ifany')
```

```
##    
##      No Yes <NA>
##   0   0   0  402
##   1 294  76    0
```

```r
# (EXP3) *If YES to exp2* Can you tell me about them?
# - confirm only responses for those individuals that responded 'yes' to exp2
table(dat$exp2, dat$exp3, useNA = 'ifany')
```

```
##       
##        Yes, respondent thinks of or identifies an individual <NA>
##   No                                                       0  294
##   Yes                                                     76    0
##   <NA>                                                     0  402
```

```r
# *Treatment delivery
# (EXP4) *If NO to exp2* Is there anyone in your neighborhood, family or broader 
# community who is over sixty-five or has any health problems that could put 
# them at higher risk?
# - confirm only responses for those individuals that responeded 'no' to exp2 
table(dat$exp2, dat$exp4, useNA = 'ifany')
```

```
##       
##         No Refuses to Answer Yes <NA>
##   No   261                 2  31    0
##   Yes    0                 0   0   76
##   <NA>   0                 0   0  402
```

```r
# *Treatment delivery
# (EXP5) "*If YES to exp4* Can you tell me about them?"
# - confirm only responses for those individuals that responeded 'no' to exp4 
table(dat$exp2, dat$exp5, useNA = 'ifany')
```

```
##       
##        Yes, respondent thinks of or identifies an individual <NA>
##   No                                                      31  263
##   Yes                                                      0   76
##   <NA>                                                     0  402
```

```r
# *Treatment delivery
# (EXP6) *If YES to exp2 or exp4* Can you tell me a bit about why they are 
# important to you?
# - confirm only responses for those individuals that responeded 'yes' to exp2/4 
table(dat$exp2, dat$exp6, useNA = 'ifany')
```

```
##       
##        Refused, respondent didn't say anything
##   No                                         3
##   Yes                                        4
##   <NA>                                       0
##       
##        Yes, respondent thinks of or identifies an individual <NA>
##   No                                                      28  263
##   Yes                                                     72    0
##   <NA>                                                     0  402
```

```r
table(dat$exp4, dat$exp6, useNA = 'ifany')
```

```
##                    
##                     Refused, respondent didn't say anything
##   No                                                      0
##   Refuses to Answer                                       0
##   Yes                                                     3
##   <NA>                                                    4
##                    
##                     Yes, respondent thinks of or identifies an individual
##   No                                                                    0
##   Refuses to Answer                                                     0
##   Yes                                                                  28
##   <NA>                                                                 72
##                    
##                     <NA>
##   No                 261
##   Refuses to Answer    2
##   Yes                  0
##   <NA>               402
```

```r
# *Treatment delivery
# (EXP7) *If YES to exp2 or exp4*  Would you be upset if something happened to 
# them?
# - confirm only responses for those individuals that responeded 'yes' to exp2/4 
table(dat$exp2, dat$exp7, useNA = 'ifany')
```

```
##       
##         No Yes <NA>
##   No     1  30  263
##   Yes    4  72    0
##   <NA>   0   0  402
```

```r
table(dat$exp4, dat$exp7, useNA = 'ifany')
```

```
##                    
##                      No Yes <NA>
##   No                  0   0  261
##   Refuses to Answer   0   0    2
##   Yes                 1  30    0
##   <NA>                4  72  402
```

```r
# *Behavioral outcome, treated who received treatment
# (EXP8) *If YES to exp2 or exp4* I really appreciate you sharing that. Would 
# you be willing to cancel social activities, avoid going to the markets and 
# crowded places as much as possible, and wash your hands often with soap and 
# water if possible to protect yourself, your loved ones, and other Ghanaians?
# - confirm only responses for those individuals that responeded 'yes' to exp2/4 
table(dat$exp2, dat$exp8, useNA = 'ifany')
```

```
##       
##         No Yes <NA>
##   No     1  30  263
##   Yes    3  73    0
##   <NA>   0   0  402
```

```r
table(dat$exp4, dat$exp8, useNA = 'ifany')
```

```
##                    
##                      No Yes <NA>
##   No                  0   0  261
##   Refuses to Answer   0   0    2
##   Yes                 1  30    0
##   <NA>                3  73  402
```

```r
# *Behavioral outcome, control + treated who did not recieve treatment
# (EXP9) *If CONTROL or no to exp2 and exp4* Would you be willing to cancel 
# social activities, avoid going to the markets and crowded places as much as
# possible, and wash your hands often with soap and water if possible to protect
# yourself, your loved ones, and other Ghanaians?
# - confirm only responses for those individuals that responeded 'no' to exp2/4 
# or control
table(dat$treat, dat$exp9, useNA = 'ifany')
```

```
##    
##     Don't know  No Yes <NA>
##   0          1  24 377    0
##   1          1  14 248  107
```

```r
table(dat$exp2, dat$exp9, useNA = 'ifany')
```

```
##       
##        Don't know  No Yes <NA>
##   No            1  14 248   31
##   Yes           0   0   0   76
##   <NA>          1  24 377    0
```

```r
table(dat$exp4, dat$exp9, useNA = 'ifany')
```

```
##                    
##                     Don't know  No Yes <NA>
##   No                         1  14 246    0
##   Refuses to Answer          0   0   2    0
##   Yes                        0   0   0   31
##   <NA>                       1  24 377   76
```

```r
# *Information-seeking outcome, all
# (EXP10) The best way to protect yourself, your loved ones, and other Ghanaians 
# is to stay informed about COVID-19 and how to prevent its spread. Would you 
# like me to provide you with a phone number you can call to get free and 
# accurate information about coronavirus and learn how you can keep yourself, 
# your loved ones, and other Ghanaians safe? 
table(dat$exp10, useNA = 'ifany')
```

```
## 
##  No Yes 
##  71 701
```

```r
# Code outcome variables
dat$Y_behav <- 1*(coalesce(dat$exp8, dat$exp9)=='Yes')
table(dat$treat, dat$Y_behav)
```

```
##    
##       0   1
##   0  25 377
##   1  19 351
```

```r
dat$Y_info <- 1*(dat$exp10=='Yes')
table(dat$treat, dat$Y_info)
```

```
##    
##       0   1
##   0  36 366
##   1  35 335
```

### Cleaning

We will include the following control variables in our analysis: an indicator for being male, a continuous measure of age, indicators for region, indicators for education level, an indicator for whether the participant’s household is below poverty level, an indicator for the phone surveyor who administered the survey, indicators for consent version (government, research, or policymaker beneficiary), and an indicator for the date when the phone survey was initiated. 

For categorical variables (e.g., gender, education, region, etc) where information is missing, we will add a category for “Unknown”. When information is missing for continuous variables, we will fill in the missing information with the dataset average for that category and add a missing indicator for the variable in question. Thus, if someone’s age is missing and the average age in the dataset is 65.7, we will fill in the missing information with 65.7 and a “Missing Age” indicator will take on a value of 1 for that observation.



```r
# an indicator for being male
dat$male <- 1*(dat$dem2 == 'Male')
dat$male_flag <- 1*(is.na(dat$dem2)) # currently no missingness 5/20/20
dat$male_c <- dat$male - mean(dat$male) # mean-centered

# a continuous measure of age
dat$age <- dat$dem1
dat$age_flag <- 1*(is.na(dat$dem1)) # currently no missingness 5/20/20
dat[which(is.na(dat$dem1))] <- mean(dat$dem1, na.rm = TRUE)
dat$age_c <- dat$age - mean(dat$age) # mean-centered

# indicators for region
dat$reg <- dat$region <- factor(dat$dem3) # creates factor for region
dat <- dat %>% # creates region dummies
  mutate(dummy = 1) %>% 
  spread(key = reg,
         sep = "_",
         value = dummy,
         fill = 0
  ) %>% # mean-centered variables for regions
  mutate_at(vars(contains('reg_')), .funs = list(c = ~.-mean(.)))


# indicators for education level
dat$ed <- dat$education <- factor(dat$dem11) # creates factor for education
dat <- dat %>%
  mutate(dummy = 1) %>% 
  spread(key = ed,
         sep = "_",
         value = dummy,
         fill = 0
  ) %>%
  mutate_at(vars(contains('ed_')), .funs = list(c = ~.-mean(.)))


# indicator for whether the participant’s household is below poverty level
dat$pov_level <- 1*(dat$pov1 == 'a') # !!!CONFIRM!!!
dat$pov_level_flag <- 1*(dat$pov1 == -888 | dat$pov1 == -999)
dat$pov_level_c <- dat$pov_level - mean(dat$pov_level)
dat$pov_level_flag_c <- dat$pov_level_flag - mean(dat$pov_level_flag)

# indicator for the phone surveyor who administered the survey
dat$sv <- dat$surveyor <- factor(dat$survyeorid)
dat <- dat %>%
  mutate(dummy = 1) %>% 
  spread(key = sv,
         sep = "_",
         value = dummy,
         fill = 0
  ) %>%
  mutate_at(vars(contains('sv_')), .funs = list(c = ~.-mean(.)))

# indicators for consent version (gov, research, or policymaker beneficiary)
dat$cv <- dat$consent_version <- factor(dat$cons_rand_vers)
dat <- dat %>% 
  mutate(dummy = 1) %>% 
  spread(key = cv,
         sep = "_",
         value = dummy,
         fill = 0
  ) %>% 
  mutate_at(vars(contains('cv_')), .funs = list(c = ~.-mean(.)))

# indicator for the date when the phone survey was initiated. 
dat$dt <- dat$int_date <- factor(dat$date)
dat <- dat %>% 
  mutate(dummy = 1) %>% 
  spread(key = dt,
         sep = "_",
         value = dummy,
         fill = 0
  ) %>% 
  mutate_at(vars(contains('dt_')), .funs = list(c = ~.-mean(.)))
```

[[Balance tables to be included]]


```r
# WIP
```



### Analysis

For each of our key DV’s, we plan to run an ordinary least squares (OLS) regression with (HC2) robust standard errors predicting the dependent variable with an indicator for assignment to our treatment condition (this will be our primary predictor variable).

As a robustness check, we will also test all our models using logistic regression rather than OLS regression. We will also report a simple difference in means for all our DVs (without any controls). 

Finally, for each DV, we will also report the Lin Estimator: an estimate of our OLS regression model where we include the interaction between an indicator for our treatment and all pre-treatment covariates. 



```r
# Informational outcomes

# Difference in means
lm1i_ols <- lm(Y_info ~ treat, data = dat)
# OLS adjusted
lm2i_ols <- lm(Y_info ~ treat +
                 male + male_flag + age + age_flag + region + education + 
                 pov_level + pov_level_flag + surveyor + consent_version +
                 int_date, data = dat)

# Lin estimator
# (computed by hand below for regression tables)
lm3i_lin <- lm_lin(Y_info ~ treat, 
                   covariates = ~ male + male_flag + age + age_flag + region +
                     education + pov_level + pov_level_flag + surveyor +
                     consent_version + int_date, data = dat)

# Logistic regression 
glm1i_log <- glm(Y_info ~ treat, data = dat, family = binomial() )
glm2i_log <- glm(Y_info ~ treat +
                   male + male_flag + age + age_flag + region + education +
                   pov_level + pov_level_flag + surveyor + consent_version +
                   int_date, data = dat, family = binomial() )


# Stated behavioral outcomes

# OLS
lm1b_ols <- lm(Y_behav ~ treat, data = dat)

lm2b_ols <- lm(Y_behav ~ treat +
                 male + male_flag + age + age_flag + region + education + 
                 pov_level + pov_level_flag + surveyor + consent_version +
                 int_date, 
               data = dat)

# Lin estimator
# (computed by hand below for regression tables)
lm3b_lin <- lm_lin(Y_behav ~ treat, 
                   covariates = ~ male + male_flag + age + age_flag + region +
                     education + pov_level + pov_level_flag + surveyor +
                     consent_version + int_date, 
                   data = dat)

# Logistic regression 
glm1b_log <- glm(Y_behav ~ treat, data = dat, family = binomial() )
glm2b_log <- glm(Y_behav ~ treat +
                   male + male_flag + age + age_flag + region + education +
                   pov_level + pov_level_flag + surveyor + consent_version +
                   int_date, 
                 data = dat, family = binomial() )
```




```r
stargazer(lm1i_ols, lm2i_ols, lm4i_lin, glm1i_log, glm2i_log, type = 'html',
          # Standard errors are HC2
          se = list(sqrt(diag(vcov(lm1i_ols, type = 'HC2'))),
              sqrt(diag(vcov(lm2i_ols, type = 'HC2'))),
              sqrt(diag(vcov(lm4i_lin, type = 'HC2')))),
          add.lines = 
            list(c('Covariate adjusted', 'No', 'Yes','Yes (Lin)', 'No', 'Yes')), 
          keep = c('^Constant$', '^treat$'),
          covariate.labels = c('Identifiable Victim', NA),
          dep.var.labels = c('Information-Seeking Outcome'),
          keep.stat = c('n', 'rsq', 'adj.rsq', 'll') )
```


<table style="text-align:center"><tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="5"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="5" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="5">Information-Seeking Outcome</td></tr>
<tr><td style="text-align:left"></td><td colspan="3"><em>OLS</em></td><td colspan="2"><em>logistic</em></td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Identifiable Victim</td><td>-0.005</td><td>-0.011</td><td>-0.013</td><td>-0.060</td><td>-0.236</td></tr>
<tr><td style="text-align:left"></td><td>(0.021)</td><td>(0.021)</td><td>(0.021)</td><td>(0.249)</td><td>(0.304)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.910<sup>***</sup></td><td>0.744<sup>***</sup></td><td>0.914<sup>***</sup></td><td>2.319<sup>***</sup></td><td>0.819</td></tr>
<tr><td style="text-align:left"></td><td>(0.014)</td><td>(0.159)</td><td>(0.014)</td><td>(0.175)</td><td>(2.072)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Covariate adjusted</td><td>No</td><td>Yes</td><td>Yes (Lin)</td><td>No</td><td>Yes</td></tr>
<tr><td style="text-align:left">Observations</td><td>772</td><td>772</td><td>772</td><td>772</td><td>772</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.0001</td><td>0.163</td><td>0.239</td><td></td><td></td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>-0.001</td><td>0.090</td><td>0.096</td><td></td><td></td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td></td><td></td><td></td><td>-237.028</td><td>-164.795</td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="5" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

```r
stargazer(lm1b_ols, lm2b_ols, lm4b_lin, glm1b_log, glm2b_log, type = 'html', 
          se = list(sqrt(diag(vcov(lm1b_ols, type = 'HC2'))),
              sqrt(diag(vcov(lm2b_ols, type = 'HC2'))),
              sqrt(diag(vcov(lm4b_lin, type = 'HC2')))),
          add.lines = 
            list(c('Covariate adjusted', 'No', 'Yes','Yes (Lin)', 'No', 'Yes')), 
          keep = c('^Constant$', '^treat$'),
          covariate.labels = c('Identifiable Victim', NA),
          dep.var.labels = c('Behavioral Outcome'),
          keep.stat = c('n', 'rsq', 'adj.rsq', 'll') )
```


<table style="text-align:center"><tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="5"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="5" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="5">Behavioral Outcome</td></tr>
<tr><td style="text-align:left"></td><td colspan="3"><em>OLS</em></td><td colspan="2"><em>logistic</em></td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Identifiable Victim</td><td>0.011</td><td>0.011</td><td>0.010</td><td>0.203</td><td>0.289</td></tr>
<tr><td style="text-align:left"></td><td>(0.017)</td><td>(0.016)</td><td>(0.016)</td><td>(0.313)</td><td>(0.399)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.938<sup>***</sup></td><td>0.508<sup>***</sup></td><td>0.937<sup>***</sup></td><td>2.713<sup>***</sup></td><td>15.217</td></tr>
<tr><td style="text-align:left"></td><td>(0.012)</td><td>(0.124)</td><td>(0.011)</td><td>(0.207)</td><td>(5,667.476)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Covariate adjusted</td><td>No</td><td>Yes</td><td>Yes (Lin)</td><td>No</td><td>Yes</td></tr>
<tr><td style="text-align:left">Observations</td><td>772</td><td>772</td><td>772</td><td>772</td><td>772</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.001</td><td>0.201</td><td>0.255</td><td></td><td></td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>-0.001</td><td>0.131</td><td>0.115</td><td></td><td></td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td></td><td></td><td></td><td>-168.561</td><td>-110.235</td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="5" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>


#### Secondary analyses

As exploratory analyses, we will test the following potential moderators for the effect of treatment on each of our DV’s: gender and education level. To test these hypotheses, we will run the regression described in Section 5 above with an added interaction between our treatment indicator and an indicator for the moderator being tested (gender or education level). 


```r
lm1i_ols_g <- lm(Y_info ~ treat*male, data = dat)
lm2i_ols_g <- lm(Y_info ~ treat*male 
               + male_flag + age + age_flag + region + education + 
                 pov_level + pov_level_flag + surveyor + consent_version +
                 int_date, 
               data = dat)

lm1i_ols_e <- lm(Y_info ~ treat*education, data = dat)
lm2i_ols_e <- lm(Y_info ~ treat*education +
                 male + male_flag + age + age_flag + region +
                 pov_level + pov_level_flag + surveyor + consent_version +
                 int_date, 
               data = dat)


lm1b_ols_g <- lm(Y_info ~ treat*male, data = dat)
lm2b_ols_g <- lm(Y_info ~ treat*male 
               + male_flag + age + age_flag + region + education + 
                 pov_level + pov_level_flag + surveyor + consent_version +
                 int_date, 
               data = dat)

lm1b_ols_e <- lm(Y_info ~ treat*education, data = dat)
lm2b_ols_e <- lm(Y_info ~ treat*education +
                 male + male_flag + age + age_flag + region +
                 pov_level + pov_level_flag + surveyor + consent_version +
                 int_date, 
               data = dat)
```



```r
stargazer(lm1i_ols_g, lm2i_ols_g, lm1i_ols_e, lm2i_ols_e,
          type = 'html',
          # Standard errors are HC2
          se = list(sqrt(diag(vcov(lm1i_ols_g, type = 'HC2'))),
              sqrt(diag(vcov(lm2i_ols_g, type = 'HC2'))),
              sqrt(diag(vcov(lm1i_ols_e, type = 'HC2'))),
              sqrt(diag(vcov(lm2i_ols_e, type = 'HC2')))),
          add.lines = 
            list(c('Covariate adjusted', 'No', 'Yes', 'No', 'Yes'),
                 c('Moderator', 'Male', 'Male', 'Ed.', 'Ed.')), 
          keep = c('Constant', 'treat', 'male'), 
          # covariate.labels = c(NA, 'Identifiable Victim'),
          dep.var.labels = c('Information-Seeking Outcome'),
          keep.stat = c('n', 'rsq', 'adj.rsq', 'll') )
```


<table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="4"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="4" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="4">Information-Seeking Outcome</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">treat</td><td>0.004</td><td>-0.007</td><td>0.400<sup>**</sup></td><td>0.477<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.033)</td><td>(0.033)</td><td>(0.183)</td><td>(0.179)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">male</td><td>0.039</td><td>0.022</td><td></td><td>0.018</td></tr>
<tr><td style="text-align:left"></td><td>(0.030)</td><td>(0.030)</td><td></td><td>(0.021)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">male_flag</td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:male</td><td>-0.014</td><td>-0.008</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.043)</td><td>(0.042)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationMiddle/ Junior High</td><td></td><td></td><td>-0.426<sup>**</sup></td><td>-0.488<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.188)</td><td>(0.184)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationNone</td><td></td><td></td><td>-0.300</td><td>-0.394<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.209)</td><td>(0.206)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationPost-Secondary</td><td></td><td></td><td>-0.425<sup>**</sup></td><td>-0.528<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.187)</td><td>(0.183)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationPrimary</td><td></td><td></td><td>-0.487<sup>**</sup></td><td>-0.600<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.202)</td><td>(0.198)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationRefuses to Answer</td><td></td><td></td><td>-0.400</td><td>-0.481</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.448)</td><td>(0.440)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationSecondary or Vocational</td><td></td><td></td><td>-0.386<sup>**</sup></td><td>-0.464<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.186)</td><td>(0.182)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.887<sup>***</sup></td><td>0.742<sup>***</sup></td><td>0.600<sup>***</sup></td><td>0.500<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.023)</td><td>(0.159)</td><td>(0.129)</td><td>(0.182)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Covariate adjusted</td><td>No</td><td>Yes</td><td>No</td><td>Yes</td></tr>
<tr><td style="text-align:left">Moderator</td><td>Male</td><td>Male</td><td>Ed.</td><td>Ed.</td></tr>
<tr><td style="text-align:left">Observations</td><td>772</td><td>772</td><td>772</td><td>772</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.003</td><td>0.163</td><td>0.017</td><td>0.177</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>-0.001</td><td>0.089</td><td>-0.0001</td><td>0.098</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="4" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

```r
stargazer(lm1b_ols_g, lm2b_ols_g, lm1b_ols_e, lm2b_ols_e,
          type = 'html',
          # Standard errors are HC2
          se = list(sqrt(diag(vcov(lm1b_ols_g, type = 'HC2'))),
              sqrt(diag(vcov(lm2b_ols_g, type = 'HC2'))),
              sqrt(diag(vcov(lm1b_ols_e, type = 'HC2'))),
              sqrt(diag(vcov(lm2b_ols_e, type = 'HC2')))),
          add.lines = 
            list(c('Covariate adjusted', 'No', 'Yes', 'No', 'Yes'),
                 c('Moderator', 'Male', 'Male', 'Ed.', 'Ed.')), 
          keep = c('Constant', 'treat', 'male'), 
          # covariate.labels = c(NA, 'Identifiable Victim'),
          dep.var.labels = c('Behavioral Outcome'),
          keep.stat = c('n', 'rsq', 'adj.rsq', 'll') )
```


<table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="4"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="4" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="4">Behavioral Outcome</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">treat</td><td>0.004</td><td>-0.007</td><td>0.400<sup>**</sup></td><td>0.477<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.033)</td><td>(0.033)</td><td>(0.183)</td><td>(0.179)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">male</td><td>0.039</td><td>0.022</td><td></td><td>0.018</td></tr>
<tr><td style="text-align:left"></td><td>(0.030)</td><td>(0.030)</td><td></td><td>(0.021)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">male_flag</td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:male</td><td>-0.014</td><td>-0.008</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.043)</td><td>(0.042)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationMiddle/ Junior High</td><td></td><td></td><td>-0.426<sup>**</sup></td><td>-0.488<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.188)</td><td>(0.184)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationNone</td><td></td><td></td><td>-0.300</td><td>-0.394<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.209)</td><td>(0.206)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationPost-Secondary</td><td></td><td></td><td>-0.425<sup>**</sup></td><td>-0.528<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.187)</td><td>(0.183)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationPrimary</td><td></td><td></td><td>-0.487<sup>**</sup></td><td>-0.600<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.202)</td><td>(0.198)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationRefuses to Answer</td><td></td><td></td><td>-0.400</td><td>-0.481</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.448)</td><td>(0.440)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationSecondary or Vocational</td><td></td><td></td><td>-0.386<sup>**</sup></td><td>-0.464<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.186)</td><td>(0.182)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.887<sup>***</sup></td><td>0.742<sup>***</sup></td><td>0.600<sup>***</sup></td><td>0.500<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.023)</td><td>(0.159)</td><td>(0.129)</td><td>(0.182)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Covariate adjusted</td><td>No</td><td>Yes</td><td>No</td><td>Yes</td></tr>
<tr><td style="text-align:left">Moderator</td><td>Male</td><td>Male</td><td>Ed.</td><td>Ed.</td></tr>
<tr><td style="text-align:left">Observations</td><td>772</td><td>772</td><td>772</td><td>772</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.003</td><td>0.163</td><td>0.017</td><td>0.177</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>-0.001</td><td>0.089</td><td>-0.0001</td><td>0.098</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="4" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

## Additional information

*Outliers and Exclusions.* 

Anyone who fails to reach the section of the phone survey where random assignment would change their experience will be excluded from the study.  Anyone who hears even the beginning of a script that differs due to random assignment will be included and assigned 0’s for both dependent variables if they drop out before dependent variables are collected.


*Further research*

After this phone survey, participants will be asked whether they want to opt-in to receive text messages with information and questions about coronavirus over a four week period. We will test whether our treatment affects willingness to opt-in to receive these messages using an OLS regression with robust standard errors, as described above. 

We will attempt to call participants for a final phone survey about six weeks after this initial phone survey. We will test whether our treatment affects their reported behaviors and beliefs about coronavirus in that final survey as well as their likelihood to agree to take the second survey. The contents of this second phone survey will not be finalized until after the launch of the original phone survey, so these analyses will be considered exploratory. We plan to use OLS regressions with the same control variables as listed above (in section 5) to conduct these analyses. 
