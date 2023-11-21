## ----setup, include=FALSE-----------------------------------------------------
# devtools::install_github("https://github.com/gmelloni/interactionRCS.git")
knitr::opts_chunk$set(echo = TRUE)

library(survival)
library(rms)
library(interactionRCS)
data(umaru, package = "interactionRCS")

## ---- warning=FALSE, message=FALSE--------------------------------------------

myformula <- Surv(time, censor) ~ treat*rcs(age, 3) + site + nonwhite + ivdrug
model_rcs <- cph(myformula , data = umaru , x = TRUE , y=TRUE)

HR_rcs_delta <- intEST( var2values = c(20:50)
                     , model = model_rcs , data = umaru , var1 ="treat", var2="age" ,ci.method = "delta")

plotINT(HR_rcs_delta , xlab = "Age")

# note that the user could directly specify the function of interest (here rcsHR) and obtain the same results
HR_rcs_delta <- rcsHR( var2values = c(20:50)
                     , model = model_rcs , data = umaru , var1 ="treat", var2="age" ,ci.method = "delta")


HR_rcs_boot <- intEST( var2values = c(20:50)
                      , model = model_rcs , data = umaru , var1 ="treat", var2="age" , ci.method = "bootstrap"
                      , ci.boot.method = "norm" , R = 500 , parallel = "multicore")

plotINT(HR_rcs_boot , xlab = "Age")

## ---- warning=FALSE, message=FALSE--------------------------------------------

myformula <- Surv(time, censor) ~ treat*age + site + nonwhite + ivdrug
model_loglin <- cph(myformula , data = umaru , x = TRUE , y=TRUE)

HR_loglin_delta <- intEST( var2values = c(20:50)
                     , model = model_loglin , data = umaru , var1 ="treat", var2="age", ci.method = "delta")

plotINT(HR_loglin_delta , xlab = "Age")

HR_loglin_boot <- intEST( var2values = c(20:50)
                      , model = model_loglin , data = umaru , var1 ="treat", var2="age", ci.method = "bootstrap"
                      , ci.boot.method = "norm" , R = 500 , parallel = "multicore")

plotINT(HR_loglin_boot , xlab = "Age")

## ---- warning=FALSE, message=FALSE--------------------------------------------

myformula <- Surv(futime, status) ~ age*rcs(bmi, 3) + male
model2_rcs <- cph(myformula , data = nafld1 , x = TRUE , y=TRUE)


HR2_rcs_delta <- intEST( var2values = c(15:50)
                     , model = model2_rcs , data = nafld1 , var1 ="age", var2="bmi" , ci=TRUE , conf = 0.95 , ci.method = "delta")

plotINT(HR2_rcs_delta , xlab = "BMI")


## ---- warning=FALSE, message=FALSE--------------------------------------------

nafld1$age5<-nafld1$age/5

myformula <- Surv(futime, status) ~ age5*rcs(bmi, 3) + male
model3_rcs <- cph(myformula , data = nafld1 , x = TRUE , y=TRUE)


HR3_rcs_delta <- intEST( var2values = c(15:50)
                     , model = model3_rcs , data = nafld1 , var1 ="age5", var2="bmi" , ci=TRUE , conf = 0.95 , ci.method = "delta")

plotINT(HR3_rcs_delta , xlab = "BMI")


## ---- warning=FALSE, message=FALSE--------------------------------------------

myformula <- Surv(futime, status) ~ age*bmi + male
model2_loglin <- cph(myformula , data = nafld1 , x = TRUE , y=TRUE)

HR2_loglin_delta <- intEST( var2values = c(15:50)
                     , model = model2_loglin , data = nafld1 , var1 ="age", var2="bmi" , ci=TRUE , conf = 0.95 , ci.method = "delta")

plotINT(HR2_loglin_delta , xlab = "BMI")

AIC(model2_loglin)
AIC(model2_rcs)

