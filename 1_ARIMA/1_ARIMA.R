library(tidyverse)
library(forecast)
library(tseries)
library(readxl)
library(lmtest)
library(TSA)

# Do not forget to change directory.
Index <- read_excel("C:/Users/kan2293/Desktop/SET/SET.xlsx")

set.index <- Index %>% 
  mutate(month = as.Date(Month,format = "%m/%d/%Y"),
         no = factor(substring(month, 6, 7)),
         year = factor(substring(month, 1, 4))) %>%
  select(month, no, year, Index, PE, Yield) %>% arrange(month)

set.index %>% select(Index) %>% 
  ts(frequency = 12, start = c(1975,4), end = c(2020,2)) %>% 
  autoplot() + 
  labs(title = "Monthly SET Index",
       subtitle = "from Apr,1975 to Mar,2020",
       x = "Year", y = "SET Index",
       caption = "Sorce: set.or.th") 

### Create Time Series Object
train <- set.index %>% filter(month <= "2015-09-01") %>% select(Index) %>% 
  ts(frequency = 12, start = c(1975,4),end = c(2015,9))

train %>% autoplot() + 
  labs(title = "Monthly SET Index",
       subtitle = "Apr,1975 - Sep,2015",
       x = "Year", y = "SET Index")
###########################################################

############################
### Model Identification ###
############################

# 1. Constant Variance
lm(train ~ time(train)) %>% bptest(studentize=F)
l <- train %>% BoxCox.lambda()
lm(BoxCox(train,l) ~ time(train)) %>% bptest(studentize=F)

# 2. Seasonality
kruskal.test(BoxCox(train,l),time(train))
set.index %>% filter(month <= "2015-09-01") %>% 
  ggplot(aes(no, Index, fill = no)) + 
  geom_boxplot(alpha = 0.5) + geom_point() +  
  labs(x = "Month") + 
  theme(legend.position="none") 

# 3. Stationary
train %>% BoxCox(lambda = l) %>% ar()
train %>% BoxCox(lambda = l) %>% adf.test(k = 2)
train %>% BoxCox(lambda = l) %>% diff() %>% adf.test(k = 2)
# Short Command
train %>% BoxCox(lambda = l) %>% ndiffs()
train %>% BoxCox(lambda = l) %>% nsdiffs()

# 4. Identify Model

m <-  train %>%  BoxCox(lambda = train %>% BoxCox.lambda()) %>% diff() %>% mean()
train %>%  BoxCox(lambda = train %>% BoxCox.lambda()) %>% 
  diff() %>% autoplot() + geom_point() +
  labs( title = "Transformation Monthly Market Index (SET)", x = "Year")   + 
  geom_hline(aes(yintercept = m), linetype = "twodash") +
  geom_hline(aes(yintercept = 1.5) , color = "#CC0033", linetype = "dashed") +
  geom_hline(aes(yintercept = -1.5) , color = "#CC0033", linetype = "dashed")

train %>% BoxCox(lambda = l) %>% diff() %>% ggtsdisplay(main="")
train %>% BoxCox(lambda = l) %>% diff() %>% ggAcf() + labs(title = "")
train %>% BoxCox(lambda = l) %>% diff() %>% ggPacf() + labs(title = "")
train %>% BoxCox(lambda = l) %>% diff() %>% eacf() 

############################
### Estimation Parameter ###
############################

# Use auto.arima(): Cheat code!!!
model <- train %>% auto.arima(lambda = l, biasadj = TRUE) 
model %>% summary()

#########################
### Diagnostics Model ###
#########################

model %>% checkresiduals()

model %>% rstandard() %>% shapiro.test()

model %>% rstandard() %>% runs()

########################
### Overfitted Model ###
########################

ofit1 <- train %>% Arima(order = c(0,1,3),lambda = l, biasadj = TRUE)
ofit1 %>% summary()

ofit2 <- train %>% Arima(order = c(1,1,2),lambda = l, biasadj = TRUE)
ofit2 %>% summary()

###################
### Forecasting ###
###################

forc <- model %>% forecast(h=54)

smp1 <- window(set.index %>% select(Index) %>% 
                 ts(frequency = 12, start = c(1975,4), end = c(2020,3)), end = c(2015,9))

smp2 <- window(set.index %>% select(Index) %>% 
                 ts(frequency = 12, start = c(1975,4), end = c(2020,3)),
               start = c(2015,10), end = c(2020,3))

df <- forc %>% as.data.frame() %>% 
  mutate(month = set.index %>% filter(month > "2015-09-01") %>% select(month) %>% pull(),
         observe = sprintf("%.2f",set.index %>% filter(month > "2015-09-01") %>% select(Index) %>% pull()),
         P.Forecast = sprintf("%.2f",`Point Forecast`),
         Lo80 = sprintf("%.2f",`Lo 80`),
         Hi80 = sprintf("%.2f",`Hi 80`),
         Lo95 = sprintf("%.2f",`Lo 95`),
         Hi95 = sprintf("%.2f",`Hi 95`)) %>%
  select(month, observe,  P.Forecast, Lo80, Hi80, Lo95, Hi95) %>% 
  as.tibble()

ylab <- 0:3

model %>% forecast(h=54, lambda = train %>% BoxCox.lambda()) %>% autoplot() + 
  autolayer(forc$mean, size = 1.5, series = "Forecasts") +
  autolayer(smp2, size = 1.5, series = "Data") +
  ggtitle("") +
  #labs(#title = "Forcasting SET Index with ARIMA(0,1,2)",
       #subtitle = expression("Model: " ~ (1-B)*X[t] == (1-0.884*B-0.0942*B^2)*epsilon[t]),
  #     x = "Year", y = "SET Index") + #,
       #caption = expression(sigma[epsilon]^2==0.3837)) +
  theme(legend.position="bottom") +
  scale_y_continuous(labels = paste0(ylab, "K"),
                     breaks = 10^3 * ylab)




#ylim(300,3300) +
#xlim(2005,2020.5) 

