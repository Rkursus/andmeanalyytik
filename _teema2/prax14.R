# Statistical Data Science
# 14. lab session

## Timeseries -------------

# Load data set
print(load(url("http://www.ms.ut.ee/mart/andmeteadus/Tallink.RData")))  
head(Tallink)
attach(Tallink)

# Convert data into a timeseries data type
y=ts(Passengers, start=c(2012,5), frequency=12)
y
plot(y)

# Fit a Holt-Winters model. For seasonality modelling use a multiplicative model
m1=HoltWinters(y, seasonal="mult")
m1


# Predict the following 2 years (24 months) with the model we have:
library(forecast)       

prog = forecast(m1, h=24)
prog

plot(prog, xlim=c(2012, 2020), 
     main="Prediction of Tallink’s passenger volume")

# More plotting options
prog = forecast(m1, h=24, level=0.95)
plot(prog, xlim=c(2012, 2020), 
     main="Tallinki reisijate arvu prognoos")

prog = forecast(m1, h=24, 
                level=c(0.5, 0.9, 0.95, 0.99))
plot(prog, xlim=c(2012, 2020), 
     main="Tallinki reisijate arvu prognoos", 
     shadecols=c("gray95", "gray85", "gray80", "gray75") )

# Add the actual passenger numbers to the plot: 
actual=c(789272, 852609, 989445, 1223901, 993078, 730631, 785583,
         654240, 811261, 549278, 620006, 686488, 779113, 853309, 
         1019421, 1238871, 1017565, 718354, 800828, 698293 ,781684,
         617374, 669849, 279507, 32181)
teg=ts(actual, start=c(2018, 4), frequency=12)
points(teg, col="red", pch=20, cex=2)


# Timeseries components (mean without seasonality, 
# trend and seasonality on given time):

plot(fitted(m1))

# Compare how another method output:
plot(stl(y, "per"))


## Correlations between variables -------

# Why are women so evil?
# Load the data
install.packages("car")
library(car)
Hartnagel[1:4,]

# Description of the data:
?Hartnagel

# Let’s have a look at the data:
attach(Hartnagel)
plot(fconvict ~ year, type="n", 
     ylab="Female convicts per 100,000 women", xlab="Year")
grid(lty=1)
points(year, fconvict, type="o", pch=16)

# Fit a simple model:
model1=lm(fconvict~tfr + partic + degrees + mconvict)
summary(model1)

# Fit a model that takes into account the dependence between observation
library("nlme")
model2 = gls(fconvict ~ tfr + partic + degrees + mconvict,
             correlation=corAR1(form=~year), method="ML")
summary(model2)

# model2 fits the data much better than the model that assumed independent observations:
AIC(model1, model2)

# Study the correlations between observations more thoroughly.
# How long does the effect of one year last?
plot(pacf(residuals(model1)))


# Fit the same model as model2 with gls():
model3 = gls(fconvict ~ tfr + partic + degrees + mconvict,
               correlation=corARMA(form=~year, p=1), method="ML")

AIC(model2, model3)


# Fit a more complex model that allows the penultimate year to play a role in the model:
model4 = gls(fconvict ~ tfr + partic + degrees + mconvict,
               correlation=corARMA(form=~year, p=2), method="ML")
AIC(model2, model4)

summary(model4)

# How to make model coefficients comparable:
library(dplyr)
Hartnagel2 <- Hartnagel %>% 
        mutate_at(c("tfr",
                    "partic",
                    "degrees",
                    "fconvict",
                    "mconvict"), 
                  ~(scale(.) %>% as.vector))


model5 = gls(fconvict ~ tfr + partic + degrees + mconvict, data = Hartnagel2,
             correlation=corARMA(form=~year, p=2), method="ML")
summary(model5)
