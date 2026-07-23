library("KnoxStats")
library("lmtest")
library("car")
library("testthat")
library("gt")
library("naniar")
library("tidyverse")
library(mice)
library(patchwork)


dt = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQO6DpUlwNciTImyFa1Q0bdPgcTuhSAGlBzpB2KT2D8BEU7F7ex9Y3TWt1ZyYUZs4uuqpdmv82rY_Oc/pub?output=csv")
summary(dt)
str(dt)
dt$GDP = as.numeric(gsub(",", "",dt$GDP))
dt$PIRate2022 = as.numeric(dt$PIRate2022)
dt$DiabetesP2024=dt$DiabetesP2024/100
dt$PIRate2022=dt$PIRate2022/100
dt$GDP = dt$GDP/10000

##check for duplication
# duplicated(d$Country)
# duplicated(dt$GDP)
# duplicated(dt$HealthEx)

##missing data
colSums(is.na(dt))
miss_var_summary(dt)


#check all models, logit transform the y/100, impute independent variables 
mi = make.method(dt)
mi["DiabetesP2024"] = ""
mi["GDP"] = "pmm"
mi["MAge2024"] = "pmm"
mi["DietScore"] = "pmm"
mi["HealthEx"] = "pmm"
mi["PIRate2022"] = "pmm"

imp = mice(dt, method = mi, m=10, seed = 50)
plot(imp)
lDiabetesP = logit(dt$DiabetesP2024)
fit = with(imp, lm(lDiabetesP~GDP + HealthEx + PIRate2022 + MAge2024 + DietScore))
result = pool(fit)
summary(result)

####
#first model assumption check
completeD1 = complete(imp, 1)
y = logit(completeD1$DiabetesP2024)
model1 = lm(y~completeD1$HealthEx + completeD1$PIRate2022 + completeD1$MAge2024 + completeD1$DietScore +completeD1$GDP, data = completeD1)
summary(model1)
r = resid(model1)
hist(r)
plot(model1, which = 2)
#normality
shapiro.test(r) ##pass
#homoscedasticity
bptest(model1) ##pass
#multicollinearity
vif(model1) ##no multicollinearity existed

####
#second model assumption check
completeD2 = complete(imp, 2)
y2 = logit(completeD2$DiabetesP2024)
model2 = lm(y2~completeD2$GDP + completeD2$HealthEx + completeD2$PIRate2022 + completeD2$MAge2024 + completeD2$DietScore, data = completeD2)
summary(model2)
r2 = resid(model2)
hist(r2)
plot(model2, which = 2)
#normality
shapiro.test(r2) ##pass
#homoscedasticity
bptest(model2) ##pass
#multicollinearity
vif(model2) ##no multicollinearity existed

####
#fifth model assumption check
completeD5 = complete(imp, 5)
y5 = logit(completeD5$DiabetesP2024)
model5 = lm(y5~completeD5$GDP + completeD5$HealthEx + completeD5$PIRate2022 + completeD5$MAge2024 + completeD5$DietScore, data = completeD5)
summary(model5)
r5 = resid(model5)
hist(r5)
plot(model5, which = 2)
#normality
shapiro.test(r5) ##pass
#homoscedasticity
bptest(model5) ##pass
#multicollinearity
vif(model5) ##no multicollinearity existed

#tenth model assumption check
completeD10 = complete(imp, 10)
y10 = logit(completeD10$DiabetesP2024)
model10 = lm(y10~completeD10$GDP + completeD10$HealthEx + completeD10$PIRate2022 + completeD10$MAge2024 + completeD10$DietScore, data = completeD10)
summary(model10)
r10 = resid(model10)
hist(r10)
plot(model10, which = 2)
#normality
shapiro.test(r10) ##pass
#homoscedasticity
bptest(model10) ##pass
#multicollinearity
vif(model10) ##no multicollinearity existed

options(scipen = 0)
sumR=summary(result, conf.int = TRUE, conf.level = 0.95)

gt(summary(result, conf.int = TRUE))
colnames(sumR)
tableResult = sumR[, c(
  "term",
  "estimate",
  "conf.low",
  "conf.high",
  "p.value"
)]

name_map = c(
  "(Intercept)" = "Intercept",
  GDP = "GDP per capita",
  HealthEx = "Healthcare expenditure",
  PIRate2022 = "Physical inactivity rate",
  MAge2024 = "Median age",
  DietScore = "Diet quality score"
)


gt(tableResult) %>%
  text_transform(
    locations = cells_body(columns = term),
    fn = function(x) {
      name_map[x]
      }
  )%>%
  tab_header(
    title = md("**Pooled Multiple Linear Regression Results**"),
    subtitle = md("logit-transformed diabetes prevalence; estimates pooled across 10 multiple imputed datasets"
    )) %>%
  cols_label(
    term = md("**Predictor** "),
    estimate = md("**Coefficients**"),
    conf.low = md("**Lower**"),
    conf.high = md("**Upper**"),
    p.value = md("**p-value**")
  ) %>%
  tab_spanner(
    label = md("**95% Confident Interval**"),
    columns = c(conf.low, conf.high)
  ) %>%
  cols_align(
    align = "center",
    columns = c(conf.low, conf.high)
  )%>%
  cols_align(
    align = "center",
    columns = estimate
    )%>%
  cols_align(
    align = "left",
    columns = term)%>%
  fmt_number(
    columns = c(estimate, conf.low, conf.high),
    decimals = 3
  ) %>%
  fmt(
    columns = p.value,
    fns = function(x){
      ifelse(
        x < 0.001,
        "<0.001",
        sprintf("%.3f", x)
      )
    }
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = p.value,
      rows = p.value < 0.05 & term != "(Intercept)"
    )
  )

hist(dt$DiabetesP2024)

p1 = completeD5 %>%
  ggplot(aes(PIRate2022*100, DiabetesP2024*100, colour = Region)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(aes(group = 1),
              method = lm, na.rm = T, colour = "red") + 
  theme_bw() + 
  labs(title = "Physical Inactivity Rate and Diabetes Prevalence",
       x = "Physical Inactivity Rate (%)",
       y = "Diabetes prevalence (%)")

p2 = completeD5 %>%
  ggplot(aes(DietScore, DiabetesP2024*100, colour = Region)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = lm, na.rm = T, colour = "yellow") + 
  theme_bw() + 
  labs(title = "Diet Score and Diabetes Prevalence",
       x = "Diet Score",
       y = "Diabetes prevalence (%)")
p1+p2
