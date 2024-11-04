
# install.packages("R.matlab")
library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(cowplot)
library(stringr)

thedata <- R.matlab::readMat("/Users/martin/Downloads/BMSS_replication/data/brexit_data_raw_eo_nov2018.mat")

Time <- thedata$timeline %>% as.numeric()
Countries <- thedata$country.names %>% unlist() %>% as.character()

the_vars <- which(!(names(thedata) %in% c("timeline", "variable.names", "country.names")))
the_names <- names(thedata[the_vars])

the_data <- map(seq_along(the_vars), .f = \(x) {
  d <- thedata[[the_vars[x]]]
  colnames(d) <- Countries
  bind_cols(Time = Time, Variable = the_names[x], d) %>%
    pivot_longer(cols = -c(Time, Variable), names_to = "Country", values_to = "Value")
}) %>%
  bind_rows()

# Convert to year and quarter
the_data <- the_data %>%
  mutate(Year = floor(Time), # Extract year
         quarter = as.integer((Time - Year) * 4 + 1), # Calculate quarter
         quarter = factor(quarter, labels = c("Q1", "Q2", "Q3", "Q4")),
         Variable = str_replace_all(Variable, "\\.", "_")
         ) %>% 
  filter(Country %in% c("Australia", "Finland", "Iceland", "Korea", "Norway", "Sweden", 
                        "Austria", "Belgium", "Canada", "France", "Germany", "Hungary", 
                        "Ireland", "Italy", "Japan", "Luxembourg", "Netherlands", "New Zealand", 
                        "Portugal", "Slovak Republic", "Spain", "Switzerland", "United Kingdom", "United States"))  


# Subset the data
brexit <- the_data %>% 
  filter(Time >= 1994.75) %>% 
  #drop_na() %>%
  pivot_wider(id_cols = c("Time", "Year", "quarter", "Country"), names_from = "Variable", values_from = "Value") %>%
  #drop_na() %>%
  select(Time, Year, quarter, Country, 
         real_con_raw, real_inv_raw, real_exp_raw, real_imp_raw, real_gdp_raw, real_gdp_2016, tot_emp_raw, pop_quarterly, lab_prod
  ) %>%
  group_by(Country) %>% 
  mutate(
    ConGDP = real_con_raw/real_gdp_raw,
    InvGDP = real_inv_raw/real_gdp_raw,
    ExpGDP = real_exp_raw/real_gdp_raw,
    ImpGDP = real_imp_raw/real_gdp_raw,
    LPG    = log(lab_prod/lag(lab_prod)),
    EmpSha = tot_emp_raw/pop_quarterly
    ) %>% 
  drop_na() %>% 
  mutate(
    gdp = (real_gdp_2016 - 1) * 100,
    ID = map_dbl(Country, ~ which(. == d$Country %>% unique())) %>% as.integer()
  ) %>% 
  as.data.frame()

write_csv(brexit, file = "~/git_projects/kausal_data/brexit.csv")

#################################### reproduction #################################### 


library(Synth)

dataprep_out <- dataprep(
  foo = brexit, 
  predictors = c("ConGDP", "InvGDP", "ExpGDP", "ImpGDP", "LPG", "EmpSha"), 
  dependent = "gdp", 
  unit.variable = "ID",
  time.variable = "Time", 
  treatment.identifier = 23, 
  controls.identifier = (d_synth$ID %>% unique())[-23], 
  time.predictors.prior = seq(1995, 2016.25, .25),
  time.optimize.ssr = seq(1995, 2016.25, .25),
  unit.names.variable = "Country"
)

synth_out <- synth(dataprep_out)

tb <- synth.tab(
  synth.res = synth_out, 
  dataprep.res = dataprep_out
)



doppelganger <- left_join(
  x = brexit, 
  y = tb$tab.w, 
  by = c("Country" = "unit.names")
) %>% 
  select(Time, Year, Country, gdp, w.weights) %>%
  group_by(Time, Year) %>%
  summarise(gdp = sum(gdp * w.weights, na.rm = T)) %>%
  mutate(type = "Doppelgaenger") %>%
  ungroup()


UK <- brexit %>% 
  filter(Country == "United Kingdom") %>% 
  select(Time, Year, gdp) %>%
  mutate(type = "UK")

the_gdps <- bind_rows(
  doppelganger, UK
)


gdp_gap <- the_gdps %>% 
  pivot_wider(values_from = gdp, names_from = "type") %>%
  mutate(gdp_gap = UK - Doppelgaenger) %>%
  ungroup()

sd_gap <- gdp_gap %>%
  filter(Time < 2016.25) %>% 
  summarise(
    sd = sd(gdp_gap)
  ) %>% 
  pull(sd)

gdp_gap <- gdp_gap %>% 
  mutate(gdp_gap_scaled = gdp_gap/sd_gap)

gdp_gap %>% filter(Year == 2015) %>% pull(gdp_gap_scaled) %>% mean() -> m_diff

p_gdp <- ggplot() +
  geom_ribbon(
    data = the_gdps %>% 
      filter(type == "Doppelgaenger"), 
    aes(
      x = Time, 
        ymin = gdp - sd_gap, 
        ymax = gdp + sd_gap
      ), 
    fill = alpha("red", alpha = .2), 
    color = "white"
  ) +
  geom_line(data = the_gdps, 
            aes(x = Time, y = gdp, col = type),
            lwd = 1) + 
  geom_vline(xintercept = 2016.25, lty = "dotted") +
  scale_color_discrete(name = "") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(.025,.9))

p_gdp +
  scale_x_continuous(limits = c(2015, 2021), expand = c(0, .1)) +
  scale_y_continuous(limits = c(-4, 12))

# gaps plot
ggplot() +
  scale_x_continuous(expand = c(0, 0), limits = c(2015, 2021)) +
  scale_y_continuous(limits = c(-4.5,1.5)) +
  geom_hline(yintercept = 0) +
  geom_line(
    data = gdp_gap, 
    mapping = aes(x = Time, y = gdp_gap),
    lwd = 1
  ) + 
  geom_vline(xintercept = 2016.25, lty = "dotted") +
  cowplot::theme_cowplot()

pvals <- map(seq(2017, 2020.5, .25), \(time) {
  tibble(
    Time = time,
    gap = gdp_gap %>% filter(Time == time) %>% pull(gdp_gap),
    pvalue = CPAT::Andrews.test(
      x = gdp_gap$gdp_gap, 
      M = which(gdp_gap$Time == time)
    )$p.value
  )
}) %>%  bind_rows()

pvals%>%

  ggplot(aes(x = Time, y = pvalue)) + 
  geom_hline(yintercept = c(.1,.05, .01), lty = "dotted", col = "red") +
  geom_line() +
  scale_x_continuous(expand = c(0, 0)) +
  cowplot::theme_cowplot()


#### placebo tests ####
placebo <- function(treat) {
  
  dataprep_out <- dataprep(
    foo = brexit, 
    predictors = c("ConGDP", "InvGDP", "ExpGDP", "ImpGDP", "LPG", "EmpSha"), 
    dependent = "gdp", 
    unit.variable = "ID",
    time.variable = "Time", 
    treatment.identifier = 23, 
    controls.identifier = (brexit$ID %>% unique())[-23], 
    time.predictors.prior = seq(1995, treat, .25),
    time.optimize.ssr = seq(1995, treat, .25),
    time.plot = seq(1995, 2020.75, .25), 
    unit.names.variable = "Country")
  
  synth_out <- synth(dataprep_out)
  
  tb <- synth.tab(synth.res = synth_out, dataprep.res = dataprep_out)
  
  return(
    left_join(
      x = brexit, 
      y = tb$tab.w, 
      by = c("Country" = "unit.names")
    ) %>% 
      select(Time, Country, gdp, w.weights) %>%
      group_by(Time) %>%
      summarise(gdp = sum(gdp * w.weights, na.rm = T)) %>%
      mutate(type = paste0("Placebo", treat))  
  )
  
}


placebos <- map(
  .x = seq(2010, 2016, .25), 
  .f =  \(x) placebo(x)
)

placebos_tbl <- 
  bind_rows(placebos)

  ggplot(
    data = placebos_tbl,
    mapping = aes(x = Time, y = gdp, group = type)
    ) +
  geom_line(
    lwd = .25, 
    col = "gray80",
    position = position_jitter(height = .25)
  ) +
  
  geom_line(
    data = the_gdps %>% 
      filter(type == "Doppelgaenger"), 
    mapping = aes(col = type), 
    lwd = 1
  ) +
  geom_line(
    data = the_gdps %>% 
      filter(type == "UK"), 
    mapping = aes(col = type), 
    lwd = 1
  ) +
  geom_vline(xintercept = 2016.25, lty = "dotted") +
  
  scale_x_continuous(limits = c(2015, 2021), expand = c(0, .05)) +
  scale_y_continuous(limits = c(-3, 13),  expand = c(0, 0)) +
  cowplot::theme_cowplot() +
  theme(legend.position = c(.75,.15))







library(tidysynth)

brexit %>%
  synthetic_control(
    outcome = gdp,
    unit = Country, 
    time = Time, 
    i_unit = "United Kingdom", 
    i_time = 2016.25, 
    generate_placebos = F
  ) %>%
  generate_predictor(
    time_window = seq(1995, 2016.25, .25),
    mConGDP = mean(ConGDP),
    mInvGDP = mean(InvGDP),
    mExpGDP = mean(ExpGDP),
    mImpGDP = mean(ImpGDP),
    mLPG    = mean(LPG),
    mEmpSha = mean(EmpSha),
  ) %>%
  
  # Generate the fitted weights for the synthetic control
  generate_weights(verbose = T) %>%
  
  # Generate the synthetic control
  generate_control() %>% 
  plot_trends()


# Donor pool
d_donor <- d %>% 
  filter(between(Time, 1995, 2016.25), Country == "United Kingdom")

# T2: UK statistics    
d %>% 
  filter(between(Time, 1995, 2016.25), Country == "United Kingdom") %>%
  group_by(Country) %>% 
  summarise(
    mConGDP = mean(real_con_raw/real_gdp_raw),
    mInvGDP = mean(real_inv_raw/real_gdp_raw),
    mExpGDP = mean(real_exp_raw/real_gdp_raw),
    mImpGDP = mean(real_imp_raw/real_gdp_raw),
    mLPG = mean(log(lab_prod/lag(lab_prod)), na.rm = T),
    mEmpSha = mean(tot_emp_raw/pop_quarterly)
  ) %>% 
  mutate(across(-Country, ~ . * 100))





var <- "real_gdp_raw"

realGDP1995 <- the_data %>% filter(Country == "United Kingdom", Variable == var, Year == 1995) %>% pull(Value) %>% mean()

the_data %>% filter(Country == "United Kingdom", Variable == var) %>% 
  mutate(Value = (Value - realGDP1995)/realGDP1995 * 100) %>%
  filter(Time > 1995) %>%
  
  ggplot(aes(x=Time, y = Value, col = Country)) + 
  geom_line() +
  scale_x_continuous(expand = c(0.005,0)) +
  scale_y_continuous(limits = c(-10,60), expand = c(0,0), breaks = seq(-10,60,10)) +
  cowplot::theme_cowplot()
  

var <- "real_gdp_raw"
realGDP1995 <- the_data %>% filter(Country == "United Kingdom", Variable == var, Time == 2016.25) %>% pull(Value) %>% mean()

the_data %>% filter(Country == "United Kingdom", Variable == var) %>% 
  mutate(Value = (Value - realGDP1995)/realGDP1995 * 100) %>%
  filter(between(Time, 2015, 2018.75)) %>%
  
  ggplot(aes(x=Time, y = Value)) + geom_line() + scale_y_continuous(limits = c(-4,8), breaks = c(-2,0,4)) + 
  cowplot::theme_cowplot() + 
  scale_x_continuous(expand = c(0.005,0)) 


library(xlsx)
options(java.parameters = "-Xmx8000m")

the_data <- read.xlsx("/Users/martin/Downloads/BMSS_replication/data/data_eo_nov_2018.xlsx", sheetIndex = "real_gdp_pc", rowIndex = c(6, 149:243)) %>%
  select(1, "United.Kingdom") %>%
  as.tibble() %>%
  rename(Time = Country, Value = `United.Kingdom`) %>%
  separate(Time, c("Q", "Year"), sep = "-") %>%
  mutate(Time = case_when(
    Q == "Q1" ~ as.numeric(Year),
    Q == "Q2" ~ as.numeric(paste0(Year,".25")),
    Q == "Q3" ~ as.numeric(paste0(Year,".5")),
    Q == "Q4" ~ as.numeric(paste0(Year,".75"))
  ))


realGDP1995 <- the_data %>% filter(Year == 1995) %>% pull(Value) %>% mean()

the_data %>%
  mutate(Value = (Value - realGDP1995)/realGDP1995 * 100) %>%
  filter(Year >= 1995) %>%
  
  ggplot(aes(x=Time, y = Value)) + 
  geom_line() +
  scale_x_continuous(expand = c(0.005,0)) +
  scale_y_continuous(limits = c(-10,60), expand = c(0,0), breaks = seq(-10,60,10)) +
  cowplot::theme_cowplot()
