#IV

library(tidyverse)
library(haven)
library(fixest)

#rainconflict <- read_stata("~/Downloads/dataverse_files/mss_repdata.dta")
rainconflict <- read_stata("~/Downloads/dataverse_files/mss_repdata_feb07.dta")

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

# (6)
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


