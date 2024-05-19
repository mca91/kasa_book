library(Jmisc)
library(plm)
library(RColorBrewer)
# FE illustration
set.seed(324)
m = 8
n = 12

step = 5
alpha = runif(n,seq(0,step*n,by=step),seq(step,step*n+step,by=step))
beta = -1
y = X = matrix(NA,nrow=m,ncol=n)
for (i in 1:n) {
  X[,i] = runif(m,i,i+1)
  X[,i] = rnorm(m,i)
  y[,i] = alpha[i] + X[,i]*beta + rnorm(m,sd=.75)  
}
stackX = as.vector(X)
stackY = as.vector(y)

##

darkcols <- brewer.pal(12, "Paired")
plot(stackX,stackY,col=rep(darkcols,each=m),pch=19)

unit = rep(1:n,each=m)
# first two columns are for plm to understand the panel structure
paneldata = data.frame(unit,rep(1:m,n),stackY,stackX)
fe <- plm(stackY~stackX, data = paneldata, model = "within")
re <- plm(stackY~stackX, data = paneldata, model = "random")
pool <- plm(stackY~stackX, data = paneldata, model = "pooling")

##
library(dplyr)
paneldata <- tibble::tibble(
  ID = rep(1:12, each = 8),
  time = rep(1:8, 12),
  
  col = rep(brewer.pal(12, "Paired"), each = 8),
  
  alpha = rep(alpha, each = 8),
  beta = -1,
  
  X = stackX,
  Y = stackY,
  
  pooledalpha = pool$coefficients[1], 
  pooledbeta = pool$coefficients[2], 
  
  FEalpha = rep(fixef(fe), each = 8),
  FEbeta = fe$coefficients,
  
  REalpha = re$coefficients[1],
  REbeta = re$coefficients[2],
) %>% 
  group_by(ID) %>%
  mutate(GMX = mean(X)) %>%
  ungroup()

library(readr)

write_csv(
  x = paneldata, 
  file = "~/git_projects/kausalanalyse_book/datasets/paneldata.csv"
)



