# the goal of this study is to measure whether more 
# foreign workers are certified to obtain H1B will 
# actually decrease the US national wage in a given state by estimating β
library(readxl)
library(stargazer)
library(car)
library(lmtest)
library(metafor)

# Import Excel Data into R Dataframe
df = data.frame(read_excel("Group project_group 9(ver.2) .xlsx"))

# Wages are in different units (yearly, monthly, etc). Convert them to hourly wages.
df[df$Year_or_hour=='Year','prevailing_wage'] = df[df$Year_or_hour=='Year','prevailing_wage'] / (52 * 40) # 52 weeks and 40 hours per week
df[df$Year_or_hour=='Month','prevailing_wage'] = df[df$Year_or_hour=='Month','prevailing_wage'] / (4 * 40) # 4 weeks and 40 hours per week
df[df$Year_or_hour=='Bi-weekly','prevailing_wage'] = df[df$Year_or_hour=='Bi-weekly','prevailing_wage'] / (2 * 40) # 2 weeks and 40 hours per week
df[df$Year_or_hour=='Week','prevailing_wage'] = df[df$Year_or_hour=='Week','prevailing_wage'] / (40) # 40 hours per week

df[df$Year_or_hour=='Year','foreign_wage_level'] = df[df$Year_or_hour=='Year','foreign_wage_level'] / (52 * 40) # 52 weeks and 40 hours per week
df[df$Year_or_hour=='Month','foreign_wage_level'] = df[df$Year_or_hour=='Month','foreign_wage_level'] / (4 * 40) # 4 weeks and 40 hours per week
df[df$Year_or_hour=='Bi-weekly','foreign_wage_level'] = df[df$Year_or_hour=='Bi-weekly','foreign_wage_level'] / (2 * 40) # 2 weeks and 40 hours per week
df[df$Year_or_hour=='Week','foreign_wage_level'] = df[df$Year_or_hour=='Week','foreign_wage_level'] / (40) # 40 hours per week

# Drop outliers in wages. They're likely errors in the data.
summary(df$prevailing_wage)
summary(df$foreign_wage_level)
df = df[df$prevailing_wage>=2.13,] # Keep above minimum wage
df = df[df$prevailing_wage<=200.0,] # Drop unrealistically high wages

# Select the variables you're going to use in the regressions
df = df[,c('location','decision_date','certi_dummy','total_positions','prevailing_wage','foreign_wage_level','policy_dummy', "category")]

# Aggregate data to the state X month level
library(dplyr)
df1 = df %>% group_by(location, decision_date) %>% summarise(
  certi_dummy = mean(certi_dummy,na.rm=TRUE),
  total_positions = mean(total_positions,na.rm=TRUE),
  prevailing_wage = mean(prevailing_wage,na.rm=TRUE),
  foreign_wage_level = mean(foreign_wage_level,na.rm=TRUE),
  policy_dummy = mean(policy_dummy,na.rm=)
  )
df1 = as.data.frame(df1)
df1

## tests panel or OLS for this data
pbgtest(df1)

# State level panel regression
library(plm)
library(lmtest)

# Create lagged certified numbers
df1 = pdata.frame(df1,index=c('location','decision_date'))
df1$lag_certi_dummy = plm::lag(df1$certi_dummy)

# df with category:
df2 = df %>% group_by(location, decision_date, category) %>% summarise(
  certi_dummy = mean(certi_dummy,na.rm=TRUE),
  total_positions = mean(total_positions,na.rm=TRUE),
  prevailing_wage = mean(prevailing_wage,na.rm=TRUE),
  foreign_wage_level = mean(foreign_wage_level,na.rm=TRUE),
  policy_dummy = mean(policy_dummy,na.rm=)
)
df2 = as.data.frame(df2)

df2 = pdata.frame(df2,index=c("location","category", 'decision_date' ))
df2$lag_certi_dummy = plm::lag(df2$certi_dummy)

df2$lage_actual_wage = plm::lag(df2$foreign_wage_level)

df3 = df %>% group_by(location) %>% summarise(
  certi_dummy = mean(certi_dummy,na.rm=TRUE),
  total_positions = mean(total_positions,na.rm=TRUE),
  prevailing_wage = mean(prevailing_wage,na.rm=TRUE),
  foreign_wage_level = mean(foreign_wage_level,na.rm=TRUE),
  policy_dummy = mean(policy_dummy,na.rm=)
)
df3 = as.data.frame(df1)

df3 = pdata.frame(df3,index=c('location'))
df3$lag_certi_dummy = plm::lag(df3$certi_dummy)

colnames(df3)

## let's do OLS 
OLS = plm(log(prevailing_wage)~log(foreign_wage_level)+certi_dummy+total_positions+policy_dummy,data=df2, model = "pooling")
summary(OLS)

stargazer(OLS, type="text")

# Create a table of the regression results
table <- stargazer(OLS, title = "Regression Results", align = TRUE)
writeLines(table, "my_regression_table.tex")

##run the Breushch-Pagan test
bptest(OLS) # rejected this test, we need a panel data model with FE or RE is better
bp_test = bptest(OLS)
library(xtable)

bp_table <- xtable(bp_test, caption = "Breusch-Pagan Test for Individual-Specific Effects")
print(bp_table, caption.placement = "top", include.rownames = FALSE, booktabs = TRUE)

# Now, Let's run a fixed effects model and a random effect model
fe = plm(log(prevailing_wage)~log(foreign_wage_level)+certi_dummy+total_positions+policy_dummy, data=df2, model="within", index=c("decision_date"))
summary(fe)

## create table in latex for all the models
table <- stargazer(OLS, fe, re1, re2_result, re3_result, title = "Results", align = TRUE)
writeLines(table, "fe_regression_table.tex")

stargazer(OLS, fe, re1, re2_result, re3_result, type= "html",title = "Results")

##testing for heteroscedasticity
##bptest(fe)

## The F test, Used between pools and OLS to see which one is better
## null is OLS is better
## alternative is individual effects not captured by OLS, and FE better
pFtest(fe, OLS)

## The Breush-Pagan Languange Multiplier test: used between RE and Pooled OLS
# null is vriances between eneities are zero, meaning no panel effects
# alternative: 
plmtest(OLS, type="bp")

## with fixed on category and location
fe2 = plm(log(prevailing_wage)~log(foreign_wage_level)+certi_dummy+total_positions+policy_dummy, data=df2, model="within", index=c("category", "location"))
summary(fe2)

stargazer(OLS, fe, type="text")

## random effects model with gls
re1 = plm(log(prevailing_wage)~log(foreign_wage_level)+certi_dummy+total_positions+policy_dummy, data=df2, model="random")
coeftest(re1,vcovHC(re1,type='HC1',cluster='group')) # Cluster standard errors by group = location
summary(re1)

re2 = plm(log(prevailing_wage)~log(foreign_wage_level)+certi_dummy+total_positions+policy_dummy, data=df2, model="random")
re2_result = coeftest(re2,vcovHC(re2,type='HC1',cluster='group')) # Cluster standard errors by group = location
summary(re2)

## exporting the random effect model
stargazer(re, re2, type="text")

bptest(re3, studentize=FALSE)

re3 = plm(log(prevailing_wage)~log(foreign_wage_level)+certi_dummy+total_positions+policy_dummy|log(foreign_wage_level)+lag_certi_dummy+total_positions+policy_dummy, data=df2, model="random", method="gls")
re3_result = coeftest(re3,vcovHC(re3,type='HC1',cluster='group')) # Cluster standard errors by group = location
summary(re3)

stargazer(re3_result, type="text")

## run some tests to see wich model is best
# hausman
phtest(fe, re1) ## really singnificant, so stick to fixed effect model

## breush pagan LM for RE 
plmtest(re1, type="bp")

stargazer(fe, re1,type="text")
## check the validity of this model

# Fit an IV regression model using ivreg
library(AER)
iv_model <- ivreg(log(prevailing_wage) ~ certi_dummy + total_positions + policy_dummy | 
                    lag_certi_dummy + total_positions + policy_dummy, 
                  data = df2)
# Perform the over-identification test
bptest(re1)
bptest(re2)

## wooldridge test  for autocorrelation in panel 
pbgtest(re3)

## breusch-Pagan test for heterescedasticity
# Breusch-Pagan test
bptest(re1)




