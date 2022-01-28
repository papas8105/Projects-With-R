## Panel Data Analysis with R

## 1. Set working directory

## setwd(...)

## 2. Load the library

library(plm)
library(knitr)
library(broom)
library(tidyverse)
library(stargazer)
library(lmtest)
library(gplots)
library(haven)

## 3. Import Dataset (STATA version)
#  Rental Data from Wooldridge
#  Indexed by city and year

RENTAL  <- read_dta("RENTAL.DTA")
rental_p <- pdata.frame(RENTAL,index = c("city","year"))

## 4. Display the data

View(RENTAL)
str(RENTAL)
str(rental_p) ## indexed RENTAL
head(RENTAL)
head(rental_p)

#  Model lrent~y90+lpop+lavginc+pctstu

## 5. OLS using lm

ols <- lm(lrent~y90+lpop+lavginc+pctstu,data = RENTAL)
summary(ols)

## 6. OLS using plm

pooled  <- plm(lrent~y90+lpop+lavginc+pctstu,data = RENTAL,model = "pooling",
              index = c("city","year"))

#OR use this format

pooled2 <- plm(lrent~y90+lpop+lavginc+pctstu,data = rental_p,model = "pooling")

## Results table

stargazer(ols,pooled,pooled2,type = "text")

## 7. Test for heteroscedasticity

res <- residuals(ols)
yhat <- fitted(ols)
plot(RENTAL$pctstu,res,xlab = "%Students",ylab = "Residuals")
plot(yhat,res,xlab = "Fitted Values",ylab = "Residuals")
## OLS is not the best model: reason heteroscedacity

## 8. Fixed Effects
# Includes within-entity effects

fe <- plm(lrent ~ y90 + lpop + lavginc + pctstu,data = rental_p,model = "within")
summary(fe)

# Show fixed effects for all 64 cities

fixef(fe)

## 9. Test for FE vs OLS
# Ho: OLS is better than FE, reject at p < 0.05

pFtest(fe,ols)
stargazer(fe,type = "text")

## 10. Random Effects

# Includes both the within-entity and between-entity effects

re <- plm(lrent ~ y90 + lpop + lavginc + pctstu,data = rental_p,model = "random")
summary(re)

## 11. FE VS RE

## Hausman Test Ho: RE is preferred, Ha: FE is preferred (p < 0.05)

phtest(fe,re)

# Beautify / Tabulate result

kable(tidy(phtest(fe,re)),caption = "Hausman endogeneity test for the random effects")

## 12. Breusch Pagan Lagrange Multiplier Test 
## Ho: No panel effect, i.e., OLS is better. 
## Ha: RE is better at p <0.05

plmtest(ols, type=c("bp"))
plmtest(pooled,type = c("bp"))

## 13. Test for cross-sectional dependence [NOTE: Suitable only for macro panels with long time
## series] [Not suitable for RENTAL dataset]
## Breusch-Pagan LM test of independence and Pasaran CD test, Ho: There is no cross-sectional dependence

pcdtest(fe,test = c("lm"))
pcdtest(fe,test = c("cd"))

## 14. Testing for serial correlation [NOTE: Suitable only for macro panels with long time series] [Not suitable for RENTAL dataset]
# Ho: There is no serial correlation

pbgtest(fe)

## 15. Breusch - Pagan test for heteroscedasticity Ho: Homoscedasticity Ha: Heteroscedasticity

bptest(lrent ~ y90 + lpop + lavginc + pctstu,data = rental_p,studentize = F)

