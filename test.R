library(ggplot2)
library(dplyr)
library(cowplot)
# Generiere synthetische Zeitreihe
set.seed(1234)

# Anz. der Zeitpunkte
n <- 100
# Anz. Beobachtungen vor und nach Ereignis
pre <- 70 
post <- n - pre

# Erzeuge eine einfache Random-Walk-Zeitreihe
time_series <- cumsum(
  rnorm(
    n = n, 
    mean = 0, 
    sd = 1
  )
)

# Ereigniseffekt
effect <- 5 

# tibble-Objekt erzeugen
df <- tibble(
  time = 1:n,
  value = time_series
) %>%
  mutate(
    value = case_when(
      n > pre ~ value + effect, 
      T ~ value
    )
  )