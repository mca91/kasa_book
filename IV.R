
##### Acemoglu paper #####

# https://economics.mit.edu/people/faculty/daron-acemoglu/data-archive

ADD_dat <- read_stata("~/Downloads/Supplementary/Datafortheweb/ADD_Mafia_district.dta")

# this one
ADD_dat <- read_stata("~/Downloads/Supplementary/Datafortheweb/ADD_Mafia_municipality.dta")

ADD_dat <- ADD_dat %>% 
  select(
  peasants_fasci,
  sp3m1893_n30,
  cl1_stn_sp1893_n30,
  predr_peas_fasci,
  ruralcentre1861,
  Rural_rent,
  Urban_rent,
  agricola_rel,
  seminatoritot_rel,
  sulfurproduction1868_70,
  Citrus_groves,
  Olives_groves,
  Vineyards,
  Mafia1885,
  Mafia1900,
  lnpop1861,
  lnsurface,
  centreheight,
  maxheight,
  slope2,
  pa_pdist1856,
  port2_pdist1856,
  roads1799,
  ave_temp,
  sp3m_ave_n30,
  var_sp3m_n30,
  distretto1853,
  provincia1853
)

haven::write_dta(ADD_dat, path = "~/git_projects/kausalanalyse_book/datasets/ADD_Mafia_municipality.dta")

ADD_dat <- ADD_dat %>%
  filter(
    !is.na(sp3m1893_n30), # no water station in 30km radius
    !is.na(Mafia1900) # no mafia presence classification
  )

map_chr(ADD_dat, ~ attr_getter("label")(.))

lm(Mafia1900 ~ peasants_fasci, data = ADD_dat) %>%
  summary()

library(fixest)

### Table 3 -- First Stage: Panel A, ohne Provinz-FE ###
# OLS
# (1)
fasci_mod_OLS <- feols(
  peasants_fasci ~ sp3m1893_n30, 
  vcov = ~ distretto1853 + cl1_stn_sp1893_n30,
  data = ADD_dat,
  ) 
summary(fasci_mod_OLS)

# Bootstrap standard errors
library(fwildclusterboot)
fasci_mod_OLS_boot <- boottest(
  fasci_mod_OLS, 
  clustid = ~ distretto1853 + cl1_stn_sp1893_n30,
  param = "sp3m1893_n30", 
  B = 999
)
summary(fasci_mod_OLS_boot)
1/fasci_mod_OLS_boot$t_stat * fasci_mod_OLS_boot$point_estimate

# (2)
# +  Peasant Fasci Covariates
fasci_mod_OLS_F <-feols(
  peasants_fasci ~ sp3m1893_n30 
  + predr_peas_fasci
  + ruralcentre1861
  + Rural_rent
  + Urban_rent
  + agricola_rel
  + seminatoritot_rel, 
  vcov = ~ distretto1853 + cl1_stn_sp1893_n30,
  data = ADD_dat 
  ) 

summary(fasci_mod_OLS_F)

# (3)
# + Presence Peasant Fasci Covariates + Presence Mafia Covariates
fasci_mod_OLS_FM <-feols(
  fml = peasants_fasci ~ 
    sp3m1893_n30 
  + predr_peas_fasci
  + ruralcentre1861
  + Rural_rent
  + Urban_rent
  + agricola_rel
  + seminatoritot_rel
  
  + sulfurproduction1868_70
  + Citrus_groves
  + Olives_groves
  + Vineyards
  + Mafia1885, 
  vcov = ~ distretto1853 + cl1_stn_sp1893_n30,
  data = ADD_dat 
) 

summary(fasci_mod_OLS_FM)

# (4)
fasci_mod_OLS_FMG <- feols(
  peasants_fasci ~ sp3m1893_n30 
  + predr_peas_fasci
  + ruralcentre1861
  + Rural_rent
  + Urban_rent
  + agricola_rel
  + seminatoritot_rel
  
  + sulfurproduction1868_70
  + Citrus_groves
  + Olives_groves
  + Vineyards
  + Mafia1885
  
  + lnpop1861
  + lnsurface
  + centreheight
  + maxheight
  + slope2
  + pa_pdist1856
  + port2_pdist1856
  + roads1799
  + ave_temp
  + sp3m_ave_n30
  + var_sp3m_n30
  , 
  vcov = ~ distretto1853 + cl1_stn_sp1893_n30,
  data = ADD_dat 
) 

summary(fasci_mod_OLS_FMG)



modelsummary(
  models = list(
    "Keine Controls" = fasci_mod_OLS,
    "Fasci" = fasci_mod_OLS_F,
    "Fasci + Mafia" = fasci_mod_OLS_FM,
    "Fasci + Mafia + Geo" = fasci_mod_OLS_FMG
  ), 
  coef_omit = "^(?!(sp3m1893\\_n30*)$).*",
  stars = T,
  gof_omit = "^(?!(R2|Num.Obs.|FE.*)$).*",
  coef_map = c("sp3m1893_n30" = "Rel. Niederschlag 1893")
)


### Panel B ###
# (1)
mod <- feols(
  peasants_fasci ~ sp3m1893_n30
  | provincia1853, 
  vcov = ~ distretto1853 + cl1_stn_sp1893_n30,
  data = ADD_dat,
) 
summary(mod)

# (4)
modB4a <- feols(
  peasants_fasci ~
  + predr_peas_fasci
  + ruralcentre1861
  + Rural_rent
  + Urban_rent
  + agricola_rel
  + seminatoritot_rel
  
  + sulfurproduction1868_70
  + Citrus_groves
  + Olives_groves
  + Vineyards
  + Mafia1885
  
  + lnpop1861
  + lnsurface
  + centreheight
  + maxheight
  + slope2
  + pa_pdist1856
  + port2_pdist1856
  + roads1799
  + ave_temp
  + sp3m_ave_n30
  + var_sp3m_n30
  | provincia1853, 
  vcov = ~ distretto1853 + cl1_stn_sp1893_n30,
  data = ADD_dat 
)

modB4b <- feols(
  sp3m1893_n30  ~ 
  + predr_peas_fasci
  + ruralcentre1861
  + Rural_rent
  + Urban_rent
  + agricola_rel
  + seminatoritot_rel
  
  + sulfurproduction1868_70
  + Citrus_groves
  + Olives_groves
  + Vineyards
  + Mafia1885
  
  + lnpop1861
  + lnsurface
  + centreheight
  + maxheight
  + slope2
  + pa_pdist1856
  + port2_pdist1856
  + roads1799
  + ave_temp
  + sp3m_ave_n30
  + var_sp3m_n30
  
  | provincia1853, 
  data = ADD_dat 
)

plot(modB4b$residuals, modB4a$residuals, pch = 19)


# Figure 5

ggplot(, 
       aes(x = sp3m1893_n30, y = Mafia1900 )) + 
  geom_point()

### Table 4: Two-Stage-Least-Squares Estimates 
 
# Panel A -- no fixed effects

# (1)
fasciIV_mod1 <- feols(
  fml = Mafia1900 ~ 1 | peasants_fasci ~ sp3m1893_n30,
  vcov = ~ distretto1853 + cl1_stn_sp1893_n30,
  data = ADD_dat
) 
summary(fasciIV_mod1)

# (2)
fasciIV_mod2 <- feols(
  fml = Mafia1900 ~ 
  predr_peas_fasci
  + ruralcentre1861
  + Rural_rent
  + Urban_rent
  + agricola_rel
  + seminatoritot_rel
  | peasants_fasci ~ sp3m1893_n30,
  vcov = ~ distretto1853 + cl1_stn_sp1893_n30,
  data = ADD_dat
) 
summary(fasciIV_mod2)

# (3)
fasciIV_mod3 <- feols(
  fml = Mafia1900 ~ 
  
    predr_peas_fasci
  + ruralcentre1861
  + Rural_rent
  + Urban_rent
  + agricola_rel
  + seminatoritot_rel
  
  + sulfurproduction1868_70
  + Citrus_groves
  + Olives_groves
  + Vineyards
  + Mafia1885
  
  | peasants_fasci ~ sp3m1893_n30,
  vcov = ~ distretto1853 + cl1_stn_sp1893_n30,
  data = ADD_dat
) 
summary(fasciIV_mod3)

# (4)
fasciIV_mod4 <- feols(
  fml = Mafia1900 ~ 
    
    predr_peas_fasci
  + ruralcentre1861
  + Rural_rent
  + Urban_rent
  + agricola_rel
  + seminatoritot_rel
  
  + sulfurproduction1868_70
  + Citrus_groves
  + Olives_groves
  + Vineyards
  + Mafia1885
  
  + lnpop1861
  + lnsurface
  + centreheight
  + maxheight
  + slope2
  + pa_pdist1856
  + port2_pdist1856
  + roads1799
  + ave_temp
  + sp3m_ave_n30
  + var_sp3m_n30
  
  | peasants_fasci ~ sp3m1893_n30,
  vcov = ~ distretto1853 + cl1_stn_sp1893_n30,
  data = ADD_dat
) 
summary(fasciIV_mod4)

# Panel B: province fixed Effects

# (1)
fasciIV_mod1_B <- feols(
  fml = Mafia1900 ~ 1 
  | provincia1853 
  | peasants_fasci ~ sp3m1893_n30,
  vcov = ~ distretto1853 + cl1_stn_sp1893_n30,
  data = ADD_dat
) 
summary(fasciIV_mod1_B)

# (2)
fasciIV_mod2_B <- feols(
  fml = Mafia1900 ~ 
  
    predr_peas_fasci
  + ruralcentre1861
  + Rural_rent
  + Urban_rent
  + agricola_rel
  + seminatoritot_rel
  
  | provincia1853 
  | peasants_fasci ~ sp3m1893_n30,
  vcov = ~ distretto1853 + cl1_stn_sp1893_n30,
  data = ADD_dat
) 
summary(fasciIV_mod2_B)

# (3)
fasciIV_mod3_B <- feols(
  fml = Mafia1900 ~ 
    
    predr_peas_fasci
  + ruralcentre1861
  + Rural_rent
  + Urban_rent
  + agricola_rel
  + seminatoritot_rel
  
  + sulfurproduction1868_70
  + Citrus_groves
  + Olives_groves
  + Vineyards
  + Mafia1885
  
  | provincia1853 
  | peasants_fasci ~ sp3m1893_n30,
  vcov = ~ distretto1853 + cl1_stn_sp1893_n30,
  data = ADD_dat
) 
summary(fasciIV_mod3_B)

# (4)
fasciIV_mod4_B <- feols(
  fml = Mafia1900 ~ 
    
    predr_peas_fasci
  + ruralcentre1861
  + Rural_rent
  + Urban_rent
  + agricola_rel
  + seminatoritot_rel
  
  + sulfurproduction1868_70
  + Citrus_groves
  + Olives_groves
  + Vineyards
  + Mafia1885
  
  + lnpop1861
  + lnsurface
  + centreheight
  + maxheight
  + slope2
  + pa_pdist1856
  + port2_pdist1856
  + roads1799
  + ave_temp
  + sp3m_ave_n30
  + var_sp3m_n30
  
  | provincia1853 
  | peasants_fasci ~ sp3m1893_n30,
  vcov = ~ distretto1853 + cl1_stn_sp1893_n30,
  data = ADD_dat
) 
summary(fasciIV_mod4_B)


##### IV: rain and conflicts in Africa  ####

library(tidyverse)
library(haven)
library(fixest)

#rainconflict <- read_stata("~/Downloads/dataverse_files/mss_repdata.dta")
rainconflict <- read_stata("~/Downloads/dataverse_files/mss_repdata_feb07.dta")

rainconflict <- rainconflict %>%
  select(
    any_prio,
    war_prio,
    ccode,
    country = country_name,
    gdp_g,
    gdp_g_l,
    GPCP_g,
    GPCP_g_l,
    GPCP_g_fl,
    gdp_1979 = y_0,
    polity2l,
    polity2_IV,
    ethfrac,
    relfrac,
    oil = Oil,
    lmtnest,
    lpopl1,
    tot_100_g,
    year
  )

write_csv2(rainconflict, file = "~/git_projects/kausalanalyse_book/datasets/rainconflict.csv")

glimpse(rainconflict)

rainconflict <- rainconflict %>% 
  mutate(
    ccode = as.factor(ccode)
  )

## Table 1 ##

rainconflict %>%
  summarise(
    gdp = mean(y_0),
    polity2l = mean(polity2l, na.rm = T),
    demindic = mean(polity2_IV >5, na.rm = T),
    ethfrac = mean(ethfrac), 
    relfrac = mean(relfrac),
    Oil = mean(Oil), 
    lmtnest = mean(lmtnest), 
    lpopl1 = mean(lpopl1),
    tot_100_g = mean(tot_100_g, na.rm = T)
  )


## Table 2 ##
# (1)
feols(
  fml = gdp_g ~ GPCP_g + GPCP_g_l,
  data = rainconflict,
  vcov = ~ ccode
  )

# (2)
feols(
  fml = gdp_g ~ GPCP_g + GPCP_g_l
  + y_0
  + polity2l 
  + ethfrac 
  + relfrac 
  + Oil 
  + lmtnest 
  + lpopl1
  + year:ccode,
  data = rainconflict,
  vcov = ~ ccode
) %>% 
  print(n = 10)

# (3)
feols(
  fml = gdp_g ~ GPCP_g + GPCP_g_l 
  + year:ccode
  | ccode,
  data = rainconflict,
  vcov = ~ ccode
) %>% 
  print(n = 2)

# (4)
feols(
  fml = gdp_g ~ GPCP_g + GPCP_g_l + GPCP_g_fl
  + year:ccode 
  | ccode,
  data = rainconflict,
  vcov = ~ ccode
) %>% 
  print(n = 3)

# (5)
feols(
  fml = gdp_g ~ GPCP_g + GPCP_g_l 
  + tot_100_g 
  + year:ccode 
  | ccode,
  data = rainconflict,
  vcov = ~ ccode
) %>% 
  print(n = 3)

#### Figure 1 ####

S1_res <- tibble(

  x = residuals(
    feols(
      fml = GPCP_g ~ GPCP_g_l
      + year:ccode
      | ccode,
      data = rainconflict
    )
  ),
  
  y = residuals(
    feols(
      fml = gdp_g ~ GPCP_g_l
      + year:ccode
      | ccode,
      data = rainconflict
    )
  )
  
)

ggplot(
  data = S1_res,
  mapping = aes(x = x, y = y)) +
  geom_point(pch = 19) +
  geom_smooth(method = "loess", span = .5) +
  coord_cartesian(
    xlim = c(-.4, .4), 
    ylim = c(-.1, .1)
  ) +
  theme_cowplot()

#### Table 3 ####

feols(
  fml = any_prio ~ GPCP_g + GPCP_g_l 
  + year:ccode
  | ccode,
  data = rainconflict,
  vcov = ~ ccode
) %>% 
  print(n = 2)

feols(
  fml = war_prio ~ GPCP_g + GPCP_g_l 
  + year:ccode
  | ccode,
  data = rainconflict,
  vcov = ~ ccode
) %>% 
  print(n = 2)

#### Figure 2 ####

rf_res <- tibble(
  
  x = residuals(
    feols(
      fml = GPCP_g_l ~ GPCP_g
      + year:ccode
      | ccode,
      data = rainconflict
    )
  ),
  
  y = residuals(
    feols(
      fml = any_prio ~ GPCP_g
      + year:ccode
      | ccode,
      data = rainconflict
    )
  )
  
)


ggplot(
  data = rf_res 
   # %>% filter(
   #   between(x, -.4, .4),
   #   between(y, -.2, .2)
   # )
  , 
  mapping = aes(x = x, y = y)) +
  geom_point(pch = 19) +
  geom_smooth(method = "loess") +
  coord_cartesian(
    xlim = c(-.4, .4), 
    ylim = c(-.2, .4)
  ) +
  theme_cowplot()
  


#### Table 4 ####

# (1)
mod_conf_probit <- feglm(
  fml = any_prio ~
    gdp_g + 
    gdp_g_l
  + y_0
  + polity2l 
  + ethfrac 
  + relfrac 
  + Oil 
  + lmtnest 
  + lpopl1 
  + year,
  data = rainconflict,
  vcov = ~ ccode, 
  family = binomial(link = "probit")
) 

library(marginaleffects)

mod_conf_probit_avge <- mod_conf_probit %>% 
  avg_slopes() 

# (2)
mod_conf_ols <- feols(
  fml = any_prio ~
    gdp_g + 
    gdp_g_l
  + y_0
  + polity2l 
  + ethfrac 
  + relfrac 
  + Oil 
  + lmtnest 
  + lpopl1 
  + year,
  data = rainconflict,
  vcov = ~ ccode
) 

# (3)
feols(
  fml = any_prio ~ -1
  +  gdp_g + 
    gdp_g_l
  + y_0
  + polity2l 
  + ethfrac 
  + relfrac 
  + Oil 
  + lmtnest 
  + lpopl1 
  + i(country_code, year),
  data = rainconflict,
  vcov = ~ ccode
) %>% 
  summary(n = 10)

# (4)
feols(
  fml = any_prio ~ -1
  +  gdp_g 
  +  gdp_g_l
  + i(ccode, year)
  | ccode,
  data = rainconflict,
  vcov = ~ ccode
) %>% 
  summary(n = 10)


#### IV models ####

# (5)
feols(
  fml = any_prio ~
  + y_0
  + polity2l 
  + ethfrac 
  + relfrac 
  + Oil 
  + lmtnest 
  + lpopl1 
  + i(ccode, year)
  | gdp_g + gdp_g_l ~ GPCP_g + GPCP_g_l, 
  data = rainconflict,
  vcov = ~ ccode
) %>% 
  summary(n = 10)

# (6)
feols(
  fml = any_prio ~ 
  + i(country_code, year)
  | ccode
  | gdp_g + gdp_g_l ~ GPCP_g + GPCP_g_l, 
  data = rainconflict,
  vcov = ~ ccode
) %>% 
  summary(n = 10)

# (7)
feols(
  fml = war_prio ~ 
    + i(country_code, year)
  | ccode
  | gdp_g + gdp_g_l ~ GPCP_g + GPCP_g_l, 
  data = rainconflict,
  vcov = ~ ccode
) %>% 
  summary(n = 10)

library(modelsummary)

modelsummary(
  models = list(
    mod_conf_probit_avge, 
    mod_conf_ols
    )
  )



