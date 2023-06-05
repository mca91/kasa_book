
rdd:::IKbandwidth(X = BastenBetz$borderdis, Y = BastenBetz$pfl, verbose = TRUE, kernel = "triangular")


lm(formula = all ~ D * I(agecell - 21)  , data = mlda)

lm(formula = all ~ I(agecell - 21)  , data = mlda %>% filter(agecell<21))$coefficients["(Intercept)"] - 
  lm(formula = all ~ I(agecell - 21)  , data = mlda %>% filter(agecell>=21))$coefficients["(Intercept)"]


plot(house)

house <- house %>% 
  rename(StimmenTm1 = x, StimmenT = y)

house %>% 
  filter(abs(StimmenTm1) < .1) %>%
  
  ggplot(aes(x = StimmenTm1, y = after_stat(count / sum(count)))) + 
  geom_histogram(color="black", fill="steelblue", stat = "bin") +
  geom_vline(xintercept = 0, col = "red", lty = "dashed", lwd = 1) +
  theme_minimal()

house_binned <- house %>%
  mutate(D = StimmenTm1 <= 0) %>%
  group_by(D) %>%
  filter(abs(StimmenTm1) < .5) %>%
  mutate(bin = cut(StimmenTm1, breaks = 50)) %>%
  group_by(D, bin) %>%
  mutate(StimmenT = mean(StimmenT), 
         StimmenTm1 = mean(StimmenTm1)) %>%
  ungroup() %>%
  select(StimmenT, StimmenTm1) %>% 
  distinct()

plot(x = house_binned$StimmenTm1, y = house_binned$StimmenT, ylim = c(.1, .8))

### save as csv
write_csv(house, file = "datasets/house.csv")
write_csv(house_binned, file = "datasets/house_binned.csv")

write_lines(x = paste0('var house_binned = ', rjson::toJSON(house_binned), ';'), file = 'datasets/house_binned.js')
write_lines(x = paste0('var house = ', rjson::toJSON(house), ';'), file = 'datasets/house.js')



