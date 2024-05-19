
library(tidyverse)     # For ggplot, %>%, mutate, filter, group_by, and friends
library(haven)         # For loading data from Stata

library(broom)         # For showing models as data frames
library(fixest)        # For fast, nice, fixed effects regression
library(modelsummary)  # For side-by-side regression tables

setwd("~/Downloads/MP_Kausalanalyse")

crime <-  read_stata("CrimebyBlock.dta")
polizeipraesenz <- read_stata("MonthlyPanel_paper.dta")

# Load terror data
dat <- polizeipraesenz %>% 
  mutate(monat = 
           case_when(
             mes == 4 ~ "April",
             mes == 5 ~ "Mai",
             mes == 6 ~ "Juni",
             mes == 7 ~ "Juli (1 - 17)",
             mes == 72 ~ "Juli (18 - 31)",
             mes == 73 ~ "Juli (18 - 31)",
             mes == 8 ~ "August",
             mes == 9 ~ "September",
             mes == 10 ~ "Oktober",
             mes == 11 ~ "November",
             mes == 12 ~ "Dezember"
           ) %>%
           ordered()
  ) %>%
  mutate(
    monat = fct_relevel(
      monat, 
      c("April", "Mai", "Juni", "Juli (1 - 17)", "Juli (18 - 31)", "August", "September", "Oktober", "November", "Dezember")
    )
  ) %>%
  rename(
    distanz = distanci,
    institut = institu1,
  ) %>%
  group_by(observ, monat) %>%
  mutate(totrob = sum(totrob)) %>%
  filter(mes != 73) %>%
  ungroup()


# Table 2
dat_listcol <- dat %>% 
  group_by(observ, monat) %>%
  mutate(totrob = sum(totrob)) %>%
  transmute(
    d2more = list(totrob[distanz > 2]),
    d2 = list(totrob[distanz == 2]),
    d1 = list(totrob[distanz == 1]),
    same = list(totrob[institut == 1])
    ) 

# means and SDs
dat_listcol %>%
  group_by(monat) %>%
  summarise(
    across(d2more:same,
           list(mean = ~ mean(unlist(.)), sd = ~ sd(unlist(.)))
    ),
  )

the_diffs <- dat_listcol %>%
  group_by(monat) %>%
  summarise(
    diff1 = list( t.test(x = unlist(same), y = unlist(d2more)) ),
    diff2 = list( t.test(x = unlist(d1), y = unlist(same)) ),
    diff3 = list( t.test(x = unlist(d2), y = unlist(d2more)) ), 
    .groups = "keep"
  ) %>%
  transmute(
    across(diff1:diff3, 
           list(
             m = ~ map_dbl(., \(x) diff(x$estimate)), 
             sd = ~ map_dbl(., \(x) x$stderr)
           )
    )
  )
  

form <- the_diffs %>%
  transmute(across(contains("_m"), ~sprintf("%.4f (%.4f)", ., get(sub(".m$", "_sd", cur_column()))), .names = "{.col}_formatted"))

form %>% gt::gt()

# Table 3
library(AER)
library(plm)


# (A)
dat_DID <- dat %>%
  mutate(
    treated = institut == 1,
    group = monat > "Juli (1 - 17)",
    oneblock = distanz == 1,
    twoblocks = distanz == 2
  ) %>% 
  filter(monat != "Juli (18 - 31)")

# fixest
feols(fml = totrob ~ I(treated * group) | observ + monat, data = dat_DID, vcov = "HC1")

# plm
pdata <- pdata.frame(dat_DID, index = c("observ", "monat"))

model <- plm(formula = totrob ~ I(treated * group) , data = pdata, model = "within", effect = "twoway")
summary(model, vcov = vcovHC(model, method = "white1"))


# (B)
# fixest
feols(fml = totrob ~ I(treated * group) + I(oneblock * group) | observ + monat, data = dat_DID, vcov = "HC1")

# plm
model <- plm(formula = totrob ~ (treated + oneblock) * group, data = pdata, model = "within", effect = "twoway")
summary(model, vcov = vcovHC(model, method = "white1"))

# (C)
# fixest
feols(fml = totrob ~ I(treated * group) + I(oneblock * group) + I(twoblocks * group) | observ + monat, data = dat_DID, vcov = "HC1")

model <- plm(formula = totrob ~ (treated + oneblock + twoblocks) * group, data = pdata, model = "within", effect = "twoway")
summary(model, vcov = vcovHC(model, method = "white1"))

# (D)
pdata <- pdata.frame(dat_DID %>% filter(monat >= "August"), index = c("observ", "monat"))
model <- plm(formula = totrob ~ (treated + oneblock + twoblocks) * group, data = pdata, model = "within", effect = "time")
summary(model, vcov = vcovHC(model, method = "white1"))

# (E)
# (doesnt match for some reason)
# fixest
feols(fml = totrob ~ I(treated * group) + I(oneblock * group) + I(twoblocks * group), 
      data = dat_DID %>% filter(distanz <= 2), 
      vcov = "HC1")

pdata <- pdata.frame(dat_DID %>% filter(distanz <= 2), index = c("observ", "monat"))
model <- plm(formula = totrob ~ (treated + oneblock + twoblocks) : group, data = pdata, model = "within", effect = "individual")
summary(model, vcov = vcovHC(model, method = "white1"))


#### Table 4 ####

# (A)
dat_DID <- dat %>%
  mutate(
    treated = institut == 1,
    group = dplyr::between(mes, 5, 7),
    oneblock = distanz == 1,
    twoblocks = distanz == 2
  ) %>% 
  filter(mes <= 7)

# fixest
mod4_A <- feols(
  fml = totrob ~ 
    I(treated * group) 
  + I(oneblock * group) 
  + I(twoblocks * group) 
  | observ + monat, 
  data = dat_DID, 
  vcov = "HC1"
)

# (B)
dat_DID <- dat_DID %>%
  mutate(
    group = dplyr::between(mes, 6, 7)
  ) 

# fixest
mod4_B <- feols(
  fml = totrob ~ 
    I(treated * group) 
  + I(oneblock * group) 
  + I(twoblocks * group) 
  | observ + monat, 
  data = dat_DID, 
  vcov = "HC1"
)

# (C)
dat_DID <- dat_DID %>%
  mutate(
    group = mes == 7,
  )

# fixest
mod4_C <- feols(
  fml = totrob ~ 
    I(treated * group) 
  + I(oneblock * group) 
  + I(twoblocks * group) 
  | observ + monat, 
  data = dat_DID, 
  vcov = "HC1"
)


modelsummary(
  models = list(
    A = mod4_A,
    B = mod4_B,
    C = mod4_C
  ), 
  stars = T,
  gof_omit   = "^(?!(R2|Num.Obs.)$).*"
)

#### Table 5 ####

dat_DID_T5 <- dat_DID %>%
  mutate(
    mbc = paste0(barrio, "_", monat)
  )

# (A)
# fixest
feols(
  fml = totrob ~ 
    I(treated * group) 
  + I(oneblock * group) 
  + I(twoblocks * group) 
  | observ + monat,
  data = dat_DID_T5,
  vcov = "HC1"
)

# (B)
# fixest
feols(fml = totrob ~ 
    I(treated * group) 
  + I(oneblock * group) 
  + I(twoblocks * group) 
  | observ + monat,
  data = dat_DID_T5, 
  vcov = vcov_cluster("observ")
)

# (C)
# fixest
feols(fml = totrob ~ 
        I(treated * group) 
      + I(oneblock * group) 
      + I(twoblocks * group) 
      | observ + monat,
      data = dat_DID_T5, 
      vcov = vcov_cluster("mbc")
)

# (E)
# fixest
feols(fml = totrob ~ 
        I(treated * group) 
      + I(oneblock * group) 
      + I(twoblocks * group) 
      | observ + mbc,
      data = dat_DID_T5,
      vcov = "HC1"
)

# (F)
# fixest
feols(fml = totrob ~ 
        I(treated * group) 
      + I(oneblock * group) 
      + I(twoblocks * group) 
      | observ + monat,
      data = dat_DID_T5 %>% 
        group_by(observ) %>% 
        filter(
          sum(totrob) > 0
        ),
      vcov = "HC1"
)


