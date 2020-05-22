---
title: 'Ghana RECOVR Experiment Analysis'
author: 'Molly Offer-Westort, Erika Kirgios'
date: '5/19/2020'
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
The Ghana Core RECOVR project was launched May 6, 2020 by Innovations for Poverty Action, as a nationally representative phone survey. 

The component of the study featured here is an experimental intervention included in the phone survey, with the objective of using nudges to increase information-seeking behavior. 

*The script of the Ghana Core RECOVR survey is linked  [here](https://docs.google.com/spreadsheets/d/1uqAGQHUpbxKGCtAXBXmUp77TwE52Rk0PZgYN2J-WXuo/edit?usp=sharing).*

# Survey overview

** DATA LAST DOWNLOADED 5/22/20**

## Demographics

Selected questions. 


![](ghana_recovr_analysis_files/figure-html/overview-1.png)<!-- -->![](ghana_recovr_analysis_files/figure-html/overview-2.png)<!-- -->![](ghana_recovr_analysis_files/figure-html/overview-3.png)<!-- -->![](ghana_recovr_analysis_files/figure-html/overview-4.png)<!-- -->

**[[To add: income/poverty distribution]]**

## COVID-19 
Selected questions. 

*In the past 7 days, how many days did members of your household go to a market or food store?*
![](ghana_recovr_analysis_files/figure-html/covid3-1.png)<!-- -->

*In the past 7 days, have you washed your hands with soap and water more often, less often, or about the same as you did before mid-March (or before government closed schools)?*
![](ghana_recovr_analysis_files/figure-html/covid4-1.png)<!-- -->

*In the last 7 days have you worn any type of face mask? If yes, what type?*
![](ghana_recovr_analysis_files/figure-html/covid5-1.png)<!-- -->

*In the last 7 days, why have you not worn a facemask?*
![](ghana_recovr_analysis_files/figure-html/covid6-1.png)<!-- -->

*Do you feel that you or anyone in your household is at risk of contracting covid-19?*
![](ghana_recovr_analysis_files/figure-html/covid7-1.png)<!-- -->

*If NO to COV7...Why do you feel that your household is not at risk of contracting covid-19?*
![](ghana_recovr_analysis_files/figure-html/covid8-1.png)<!-- -->

*Do you think the reaction of your country’s government to the current coronavirus outbreak is appropriate, too extreme, or not sufficient?*
![](ghana_recovr_analysis_files/figure-html/covid9-1.png)<!-- -->

*What is your main source of concern related to the effects of the Coronavirus crisis on Ghanaians?*
![](ghana_recovr_analysis_files/figure-html/covid10-1.png)<!-- -->

# Experiment

Treated participants were asked to *name and reflect on someone at high risk of dying from coronavirus who they care about*. Control participants were not asked these questions. Treatment was assigned to randomly sorted telephone numbers prior to the intervention. 




### Primary hypothesis

The primary hypothesis is that treated subjects will be more likely to 

(1) indicate willingness to comply with recommendations for reducing the spread of coronavirus like social distancing, hand washing, etc. and 
(2) seek more information about coronavirus. 


## Data

### Treatment and dependent variable  


```r
dat$treat <- 1*(dat$rand1 == 'Identifiable victim')

# some people did not recieve treatment assignment
table(dat$treat, useNA = 'ifany')
```

```
## 
##    0    1 <NA> 
##  688  640   29
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





```r
# Code outcome variables
dat$Y_behav <- 1*(coalesce(dat$exp8, dat$exp9)=='Yes')
table(dat$treat, dat$Y_behav)
```

```
##    
##       0   1
##   0  36 652
##   1  38 602
```

```r
dat$Y_info <- 1*(dat$exp10=='Yes')
table(dat$treat, dat$Y_info)
```

```
##    
##       0   1
##   0  67 621
##   1  57 583
```


### Cleaning

We will include the following control variables in our analysis: an indicator for being male, a continuous measure of age, indicators for region, indicators for education level, an indicator for whether the participant’s household is below poverty level, an indicator for the phone surveyor who administered the survey, indicators for consent version (government, research, or policymaker beneficiary), and an indicator for the date when the phone survey was initiated. 

For categorical variables (e.g., gender, education, region, etc) where information is missing, we will add a category for “Unknown”. When information is missing for continuous variables, we will fill in the missing information with the dataset average for that category and add a missing indicator for the variable in question. Thus, if someone’s age is missing and the average age in the dataset is 65.7, we will fill in the missing information with 65.7 and a “Missing Age” indicator will take on a value of 1 for that observation.



```r
# an indicator for being male
dat <- dat %>% 
    mutate(male = 1 *( replace_na(dem2, 0) == 'Male'),
         # currently no missingness 5/20/20
         male_flag = 1*(dem2 == -888 | dem2 == -999 | is.na(dem2)),
         male_c = male - mean(male), # mean-centered
         male_flag_c = male_flag - mean(male_flag), 
# a continuous measure of age
         age = replace_na(dem1, mean(dem1, na.rm = TRUE)),
         # currently no missingness 5/20/20
         age_flag = 1*(dem1 == -888 | dem1 == -999 | is.na(dem1)),
         age_c = age - mean(age), # mean-centered
         age_flag_c = age_flag - mean(age_flag))

# indicators for region
dat$reg <- dat$region <- factor(dat$dem3) # creates factor for region
dat <- dat %>% # creates region dummies
  mutate(dummy = 1) %>% 
  spread(key = reg,
         sep = '_',
         value = dummy,
         fill = 0
  ) %>% # mean-centered variables for regions
  mutate_at(vars(contains('reg_')), .funs = list(c = ~.-mean(.)))


# indicators for education level
dat$ed <- dat$education <- factor(dat$dem11) # creates factor for education
dat <- dat %>% # creates education dummies
  mutate(dummy = 1) %>% 
  spread(key = ed,
         sep = '_',
         value = dummy,
         fill = 0
  ) %>% # mean-centered variable
  mutate_at(vars(contains('ed_')), .funs = list(c = ~.-mean(.)))


# indicator for whether the participant’s household is below poverty level
dat <- dat %>% 
  mutate(pov_level = 1 *( replace_na(pov1, 0) == 'a'), # !!!CONFIRM!!!
         pov_level_flag = 1*(pov1 == -888 | pov1 == -999 | is.na(pov1)),
         pov_level_c = pov_level - mean(pov_level),
         pov_level_flag_c = pov_level_flag - mean(pov_level_flag))

# indicator for the phone surveyor who administered the survey
dat$sv <- dat$surveyor <- factor(dat$survyeorid)
dat <- dat %>% # creates surveyor dummies
  mutate(dummy = 1) %>% 
  spread(key = sv,
         sep = '_',
         value = dummy,
         fill = 0
  ) %>% # mean-centered variable
  mutate_at(vars(contains('sv_')), .funs = list(c = ~.-mean(.)))

# indicators for consent version (gov, research, or policymaker beneficiary)
dat$cv <- dat$consent_version <- factor(dat$cons_rand_vers)
dat <- dat %>% # creates consent dummies
  mutate(dummy = 1) %>% 
  spread(key = cv,
         sep = '_',
         value = dummy,
         fill = 0
  ) %>%  # mean-centered variable
  mutate_at(vars(contains('cv_')), .funs = list(c = ~.-mean(.)))

# indicator for the date when the phone survey was initiated. 
dat$dt <- dat$int_date <- factor(dat$date)
dat <- dat %>%  # creates date dummies
  mutate(dummy = 1) %>% 
  spread(key = dt,
         sep = '_',
         value = dummy,
         fill = 0
  ) %>%  # mean-centered variable
  mutate_at(vars(contains('dt_')), .funs = list(c = ~.-mean(.)))
```

[[Balance tables to be included]]


```r
# WIP
```



### Analysis



For each of our key DVs, we report an unadjusted difference in means.
We also run ordinary least squares (OLS) regression with (HC2) robust standard errors predicting the dependent variable with an indicator for assignment to our treatment condition.

As a robustness check, we also test all our models using logistic regression rather than OLS regression.  

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

# Difference in means
lm1b_ols <- lm(Y_behav ~ treat, data = dat)
# OLS adjusted
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
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Identifiable Victim</td><td>0.008</td><td>0.001</td><td>-0.001</td><td>0.098</td><td>-0.110</td></tr>
<tr><td style="text-align:left"></td><td>(0.016)</td><td>(0.016)</td><td>(0.016)</td><td>(0.189)</td><td>(0.219)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.903<sup>***</sup></td><td>1.085<sup>***</sup></td><td>0.906<sup>***</sup></td><td>2.227<sup>***</sup></td><td>36.252</td></tr>
<tr><td style="text-align:left"></td><td>(0.011)</td><td>(0.212)</td><td>(0.011)</td><td>(0.129)</td><td>(7,048.619)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Covariate adjusted</td><td>No</td><td>Yes</td><td>Yes (Lin)</td><td>No</td><td>Yes</td></tr>
<tr><td style="text-align:left">Observations</td><td>1,328</td><td>1,328</td><td>1,328</td><td>1,328</td><td>1,328</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.0002</td><td>0.134</td><td>0.178</td><td></td><td></td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>-0.001</td><td>0.084</td><td>0.079</td><td></td><td></td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td></td><td></td><td></td><td>-411.908</td><td>-306.781</td></tr>
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
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Identifiable Victim</td><td>-0.007</td><td>-0.006</td><td>-0.006</td><td>-0.134</td><td>-0.134</td></tr>
<tr><td style="text-align:left"></td><td>(0.013)</td><td>(0.012)</td><td>(0.012)</td><td>(0.239)</td><td>(0.302)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.948<sup>***</sup></td><td>1.218<sup>***</sup></td><td>0.946<sup>***</sup></td><td>2.897<sup>***</sup></td><td>56.940</td></tr>
<tr><td style="text-align:left"></td><td>(0.009)</td><td>(0.156)</td><td>(0.008)</td><td>(0.171)</td><td>(11,822.680)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Covariate adjusted</td><td>No</td><td>Yes</td><td>Yes (Lin)</td><td>No</td><td>Yes</td></tr>
<tr><td style="text-align:left">Observations</td><td>1,328</td><td>1,328</td><td>1,328</td><td>1,328</td><td>1,328</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.0002</td><td>0.250</td><td>0.289</td><td></td><td></td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>-0.001</td><td>0.207</td><td>0.204</td><td></td><td></td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td></td><td></td><td></td><td>-285.407</td><td>-184.595</td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="5" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>


#### Secondary analyses

As exploratory analyses, we test the following potential moderators for the effect of treatment on each of our DV’s: gender and education level. To test these hypotheses, we run the regressions described in Section 5 above with an added interaction between our treatment indicator and an indicator for the moderator being tested (gender or education level). 


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
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">treat</td><td>-0.009</td><td>-0.016</td><td>-0.000</td><td>0.072</td></tr>
<tr><td style="text-align:left"></td><td>(0.026)</td><td>(0.025)</td><td>(0.336)</td><td>(0.330)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">male</td><td>0.026</td><td>0.017</td><td></td><td>0.031<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.023)</td><td>(0.022)</td><td></td><td>(0.016)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">male_flag</td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:male</td><td>0.026</td><td>0.027</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.033)</td><td>(0.032)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationKindergarten</td><td></td><td></td><td>0.286</td><td>0.236</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.368)</td><td>(0.360)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationMiddle/ Junior High</td><td></td><td></td><td>-0.033</td><td>-0.101</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.337)</td><td>(0.332)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationNone</td><td></td><td></td><td>0.114</td><td>-0.015</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.344)</td><td>(0.337)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationPost-Secondary</td><td></td><td></td><td>-0.003</td><td>-0.089</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.337)</td><td>(0.331)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationPrimary</td><td></td><td></td><td>-0.078</td><td>-0.169</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.344)</td><td>(0.337)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationSecondary or Vocational</td><td></td><td></td><td>0.035</td><td>-0.043</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.337)</td><td>(0.331)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.887<sup>***</sup></td><td>1.101<sup>***</sup></td><td>1.000<sup>***</sup></td><td>1.024<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.018)</td><td>(0.213)</td><td>(0.291)</td><td>(0.325)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Covariate adjusted</td><td>No</td><td>Yes</td><td>No</td><td>Yes</td></tr>
<tr><td style="text-align:left">Moderator</td><td>Male</td><td>Male</td><td>Ed.</td><td>Ed.</td></tr>
<tr><td style="text-align:left">Observations</td><td>1,328</td><td>1,328</td><td>1,328</td><td>1,328</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.005</td><td>0.134</td><td>0.012</td><td>0.140</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.002</td><td>0.084</td><td>0.002</td><td>0.086</td></tr>
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
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">treat</td><td>-0.009</td><td>-0.016</td><td>-0.000</td><td>0.072</td></tr>
<tr><td style="text-align:left"></td><td>(0.026)</td><td>(0.025)</td><td>(0.336)</td><td>(0.330)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">male</td><td>0.026</td><td>0.017</td><td></td><td>0.031<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.023)</td><td>(0.022)</td><td></td><td>(0.016)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">male_flag</td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:male</td><td>0.026</td><td>0.027</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.033)</td><td>(0.032)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationKindergarten</td><td></td><td></td><td>0.286</td><td>0.236</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.368)</td><td>(0.360)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationMiddle/ Junior High</td><td></td><td></td><td>-0.033</td><td>-0.101</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.337)</td><td>(0.332)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationNone</td><td></td><td></td><td>0.114</td><td>-0.015</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.344)</td><td>(0.337)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationPost-Secondary</td><td></td><td></td><td>-0.003</td><td>-0.089</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.337)</td><td>(0.331)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationPrimary</td><td></td><td></td><td>-0.078</td><td>-0.169</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.344)</td><td>(0.337)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">treat:educationSecondary or Vocational</td><td></td><td></td><td>0.035</td><td>-0.043</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.337)</td><td>(0.331)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.887<sup>***</sup></td><td>1.101<sup>***</sup></td><td>1.000<sup>***</sup></td><td>1.024<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.018)</td><td>(0.213)</td><td>(0.291)</td><td>(0.325)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Covariate adjusted</td><td>No</td><td>Yes</td><td>No</td><td>Yes</td></tr>
<tr><td style="text-align:left">Moderator</td><td>Male</td><td>Male</td><td>Ed.</td><td>Ed.</td></tr>
<tr><td style="text-align:left">Observations</td><td>1,328</td><td>1,328</td><td>1,328</td><td>1,328</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.005</td><td>0.134</td><td>0.012</td><td>0.140</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.002</td><td>0.084</td><td>0.002</td><td>0.086</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="4" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

## Additional information

*Outliers and Exclusions.* 

Anyone who fails to reach the section of the phone survey where random assignment would change their experience will be excluded from the study.  Anyone who hears even the beginning of a script that differs due to random assignment will be included and assigned 0s for both dependent variables if they drop out before dependent variables are collected.


*Further research*

After this phone survey, participants will be asked whether they want to opt-in to receive text messages with information and questions about coronavirus over a four week period. We will test whether our treatment affects willingness to opt-in to receive these messages using an OLS regression with robust standard errors, as described above. 

We will attempt to call participants for a final phone survey about six weeks after this initial phone survey. We will test whether our treatment affects their reported behaviors and beliefs about coronavirus in that final survey as well as their likelihood to agree to take the second survey. The contents of this second phone survey will not be finalized until after the launch of the original phone survey, so these analyses will be considered exploratory. We plan to use OLS regressions with the same control variables as listed above (in section 5) to conduct these analyses. 
