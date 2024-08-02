
library(haven)

es <- read_dta("/Users/martin/Downloads/113838-V1/AbramsDeterrenceAEJApp2011-0005Data/AbramsDeterrence1.dta")

# Abbildung 2
es %>% 
  select(year, everaddon, pcralloffense, pcrrobgun, statepop) %>%
  mutate(
    pcrrobgun = pcrrobgun * 33.3
  ) %>%
  rename(
    Schusswaffen = pcrrobgun, 
    Gesamt = pcralloffense,
    AddOnGesetze = everaddon
  ) %>%
  pivot_longer(
    cols = Schusswaffen:Gesamt, 
    names_to = "Verbrechen", 
    values_to = "Wert"
  ) %>%
  group_by(year, AddOnGesetze, Verbrechen) %>% 
  mutate(
    AddOnGesetze = as.factor(AddOnGesetze),
    ) %>%
  summarise(
    Wert = weighted.mean(Wert, w = statepop, na.rm = T)
  ) %>%
  
  ggplot(aes(x = year, y = Wert, color = Verbrechen, lty = AddOnGesetze)) +
  geom_line() +
  scale_y_continuous(
    name = "Gesamte Anzeigen / 100.000 Einw.",
    sec.axis = sec_axis(~ . / 33.3, name = "Raubüberfälle mit Schusswaffen / 100.000 Einw.")
  ) +
  theme(legend.position = "bottom")

###

# Models 

es <- read_dta("/Users/martin/Downloads/113838-V1/AbramsDeterrenceAEJApp2011-0005Data/AbramsDeterrence1.dta")

reg_dat <- es %>% 
  mutate(across(everything(), ~ ifelse(.==-999, NA, .))) %>%
  #filter(addonyr == -999) %>%
  transmute(
    pcralloffense,
    pcrrobgun,
    lnpcrrobgun,
    FSTATE = as.factor(FSTATE),
    year,
    ymm,
    relyr,
    everaddon,
    yaddon = ifelse(relyr > 1, 0, yaddon),
    twoyears_add = relyr %in% 1:2,
    threeyears_add = relyr %in% 1:3,
    dcpoverty,
    dcunemp,
    dcblack_per,
    dcp15t17,
    dcp18t24 ,
    dcp25t34,
    dcpolice_per1,
    dcprison_per1,
    post74,
    statepop
  ) 

write_dta(data = reg_dat, path = "datasets/Abrams2012.dta")


# Table 3 (3)
feols(fml = lnpcrrobgun ~
      yaddon
   +  ymm
   
   + dcpoverty
   + dcunemp
   + dcblack_per
   + dcp15t17
   + dcp18t24 
   + dcp25t34
   + dcpolice_per1
   + dcprison_per1
   
   | FSTATE + year,
   data = reg_dat %>% 
     filter(relyr >= -7 & relyr <= 6), 
   weights = ~ statepop,
   ) %>% 
  summary()

# (4)
# + state-specific time trends
feols(fml = lnpcrrobgun ~
        yaddon
      + ymm
      
      + dcpoverty
      + dcunemp
      + dcblack_per
      + dcp15t17
      + dcp18t24 
      + dcp25t34
      + dcpolice_per1
      + dcprison_per1
      
      + FSTATE:year
      
      | FSTATE + year,
      data = reg_dat %>% 
        filter(relyr >= -7 & relyr <= 6), 
      weights = ~ statepop,
) %>% 
  summary()

# (8)
feols(fml = lnpcrrobgun ~
        yaddon
      + ymm
      
      + dcpoverty
      + dcunemp
      + dcblack_per
      + dcp15t17
      + dcp18t24 
      + dcp25t34
      + dcpolice_per1
      + dcprison_per1
      
      + FSTATE:year
      
      | FSTATE + year,
      data = reg_dat %>% 
        filter(
          relyr >= -7 & relyr <= 6, 
          post74 == 1
          ), 
      weights = ~ statepop,
) %>% 
  summary()

## Event Study regressions

reg_es <- reg_dat %>%
  mutate(
    relyr = if_else(relyr < -7, -7, relyr),
    relyr = if_else(relyr > 6, 6, relyr)
  ) %>%
  pivot_wider(
    names_from = relyr, 
    values_from = relyr, 
    names_prefix = "D", 
    values_fn = list(relyr = ~ 1), 
    values_fill = 0
  ) %>%
  rename_with(
    .fn = ~ gsub("-", "_", .), 
    .cols = starts_with("D")
  )

es_mod <- feols(fml = lnpcrrobgun ~
        
      + D_6
      + D_5
      + D_4
      + D_3
      + D_2
      + D_1
      + D0      
      + D1
      + D2
      + D3
      + D4
      + D5
      + D6
    
      + dcpoverty
      + dcunemp
      + dcblack_per
      + dcp15t17
      + dcp18t24 
      + dcp25t34
      + dcpolice_per1
      + dcprison_per1
      
      + FSTATE:year
      
      | FSTATE + year,
       data = reg_es,
      weights = ~ statepop, 
      vcov = ~ FSTATE
)


# Extract the coefficients and standard errors for the dummies
coefficients <- coef(es_mod)
std_errors <- se(es_mod)

# Select the relevant coefficients and standard errors for the dummies D_-6 to D_6
events <- -6:6
event_names <- names(coefficients)[1:13]

event_coefficients <- coefficients[event_names]
event_std_errors <- std_errors[event_names]


# Create a data frame for plotting
plot_data <- tibble(
  Event = events,
  Estimate = event_coefficients,
  SE = event_std_errors
)

# Add confidence intervals
plot_data <- plot_data %>%
  mutate(
    CI_Lower = Estimate - 1.96*SE,
    CI_Upper = Estimate + 1.96*SE
  )

# Plot the event study
ggplot(plot_data, aes(x = Event, y = Estimate)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.2) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Event Study Plot",
       x = "Event Time",
       y = "Coefficient Estimate") +
  theme_cowplot()




