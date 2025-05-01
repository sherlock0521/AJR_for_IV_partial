library(readr)
colony <- read_csv("D:/中山學院/113-2/Econometrics (II)/Homework/homework 01/rawdata/colony.csv")
View(colony)

data_01 = subset(colony,baseco=="1")
data_01$other = ifelse(data_01$shortnam %in% c("AUS","MLT","NZL"),1,0)

#Q1
fm = lm(logpgp95~avexpr+africa+asia+other,data_01)
summary(fm)
anova(fm)

install.packages("stargazer")
library(stargazer)

stargazer(fm, type = "text", 
          title = "OLS Regression Results",
          dep.var.labels = "Log GDP per capita (1995)",
          covariate.labels = c("Avg. Expropriation Risk", "Africa", "Asia", "Other"), 
          digits = 3)

stargazer(fm,
          type = "latex",
          title = "OLS Regression Results",
          dep.var.labels = "Log GDP per capita (1995)",
          covariate.labels = c("Avg. Expropriation Risk", "Africa", "Asia", "Other"),
          digits = 3,
          out = "regression_output.tex")  

#Q3
#stage 1
TSLS_fm1 = lm(avexpr~africa+asia+other+logem4,data_01)
data_01$avexpr_hat <- fitted(TSLS_fm1)
#stage 2
TSLS_fm2 = lm(logpgp95~avexpr_hat+africa+asia+other,data_01)

install.packages("ivreg")
library(ivreg)
iv_model <- ivreg(logpgp95 ~ avexpr + africa + asia + other | 
                    logem4 + africa + asia + other, data = data_01)

stargazer(TSLS_fm2, type = "text", 
          title = "TSLS Regression Results",
          dep.var.labels = "Log GDP per capita (1995)",
          covariate.labels = c("Avg. Expropriation Risk hat", "Africa", "Asia", "Other"), 
          digits = 3)
stargazer(TSLS_fm2, type = "latex", 
          title = "TSLS Regression Results",
          dep.var.labels = "Log GDP per capita (1995)",
          covariate.labels = c("Avg. Expropriation Risk hat", "Africa", "Asia", "Other"), 
          digits = 3)


#Q5
# 
first_stage <- lm(avexpr ~ logem4 + africa + asia + other, data = data_01)

# 
avexpr_resid <- residuals(first_stage)

# 
hausman_model <- lm(logpgp95 ~ avexpr + africa + asia + other + avexpr_resid, data = data_01)

# 
summary(hausman_model)
library(stargazer)

stargazer(hausman_model,
          type = "text",
          title = "Hausman Test (Regression-Based)",
          dep.var.labels = "Log GDP per capita (1995)",
          covariate.labels = c("Avg. Expropriation Risk",
                               "Africa", "Asia", "Other",
                               "avexpr resid"),
          digits = 3)
stargazer(hausman_model,
          type = "latex",
          title = "Hausman Test (Regression-Based)",
          dep.var.labels = "Log GDP per capita (1995)",
          covariate.labels = c("Avg. Expropriation Risk",
                               "Africa", "Asia", "Other",
                               "avexpr resid"),
          digits = 3,
          out = "hausman_test.tex")





