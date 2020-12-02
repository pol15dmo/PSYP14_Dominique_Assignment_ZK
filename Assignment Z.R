# ASSIGNMENT 1

library(psych) 
library(car) 
library(lmtest)
library(sandwich) 
library(boot)
library(lmboot) 
library(tidyverse)
library(gridExtra)
library(dplyr)
library(car)
library(lsr)
library(sciplot)
library(lm.beta)

coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}

# MODEL 1

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")  

data_sample_1 = data_sample_1 %>% 
  mutate(sex = factor(sex))

fig_1 = data_sample_1 %>%
  ggplot() +
  aes(x = sex, y = pain) +
  geom_boxplot()+
  ylim(c(0, 10))
fig_2 = 
  data_sample_1 %>%
  ggplot() +
  aes(x = age, y = pain) +
  geom_point() +
  geom_smooth(method = "lm", se = F)
grid.arrange(fig_1, fig_2, nrow=1)

mod_age_sex_pain = lm(pain ~ age + sex, data = data_sample_1) 

summary(mod_age_sex_pain)

standardCoefs(mod_age_sex_pain)

confint(mod_age_sex_pain)

AIC(mod_age_sex_pain)

# MODEL 2

fig_8 = data_sample_1 %>%
  ggplot() + aes(x = age, y = pain) + geom_point() + geom_smooth(method = "lm")

fig_9 = data_sample_1 %>%
  ggplot() + aes(x = sex, y = pain) + geom_point() + geom_smooth(method = "lm")

fig_10 = data_sample_1 %>%
  ggplot() + aes(x = STAI_trait, y = pain) + geom_point() + geom_smooth(method = "lm")

fig_11 = data_sample_1 %>%
  ggplot() + aes(x = pain_cat, y = pain) + geom_point() + geom_smooth(method = "lm")

fig_12 = data_sample_1 %>%
  ggplot() + aes(x = cortisol_serum, y = pain) + geom_point() + geom_smooth(method = "lm")

fig_13 = data_sample_1 %>%
  ggplot() + aes(x = cortisol_saliva, y = pain) + geom_point() + geom_smooth(method = "lm")

fig_14 = data_sample_1 %>%
  ggplot() + aes(x = mindfulness, y = pain) + geom_point() + geom_smooth(method = "lm")

grid.arrange(fig_8, fig_9, fig_10, fig_11, fig_12, fig_13, fig_14, ncol=2) 

model_2 = lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_1)

summary(model_2)

AIC(model_2)

model_2 %>% plot(which = 5)

model_2 %>% plot(which = 4)

model_2_without_4_93_114 = lm(formula = pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_1, subset = -c(93, 114, 150)) 

summary(model_2_without_4_93_114)

standardCoefs(model_2_without_4_93_114)

confint(model_2_without_4_93_114)

AIC(model_2_without_4_93_114)

describe(residuals(model_2_without_4_93_114)) 

model_2_without_4_93_114 %>% plot(which = 2)

model_2_without_4_93_114 %>% residualPlots()

model_2_without_4_93_114 %>% plot(which = 3)

model_2_without_4_93_114 %>% ncvTest()

model_2_without_4_93_114 %>% bptest()

vif(model_2_without_4_93_114)

data_sample_1 %>% select(pain, cortisol_serum, cortisol_saliva) %>% pairs.panels(col = "red", lm = T)

model_2_no_saliva = lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_1)

model_2_final = lm(formula = pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_1, subset = -c(93, 100, 114))

describe(residuals(model_2_without_4_93_114))

model_2_final %>% plot(which = 2)

model_2_final %>% residualPlots()

model_2_without_4_93_114 %>% plot(which = 3)

model_2_final %>% ncvTest()

model_2_final %>% bptest()

model_2_final %>% vif()

summary(model_2_final)

standardCoefs(model_2_final)

confint(model_2_final)

# MODEL COMPARSION

mod_age_sex_pain_final = lm(pain ~ age + sex, data = data_sample_1, subset = -c(93, 110, 114))

AIC(mod_age_sex_pain_final)

anova(mod_age_sex_pain_final, model_2_final)

coef_table(model_2_final)

coef_table(mod_age_sex_pain_final)

# ASSIGNMENT 2

data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")

theory_based_model = lm(formula = pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income, data = data_sample_1, subset = -c(93, 100, 114))

fig_10 = theory_based_model %>%
  ggplot() + aes(x = age, y = pain) + geom_point() + geom_smooth(method = "lm")

fig_11 = theory_based_model %>%
  ggplot() + aes(x = sex, y = pain) + geom_point() + geom_smooth(method = "lm")

fig_12 = theory_based_model %>%
  ggplot() + aes(x = STAI_trait, y = pain) + geom_point() + geom_smooth(method = "lm")

fig_13 = theory_based_model %>%
  ggplot() + aes(x = pain_cat, y = pain) + geom_point() + geom_smooth(method = "lm")

fig_14 = theory_based_model %>%
  ggplot() + aes(x = cortisol_serum, y = pain) + geom_point() + geom_smooth(method = "lm")

fig_15 = theory_based_model %>%
  ggplot() + aes(x = mindfulness, y = pain) + geom_point() + geom_smooth(method = "lm")

fig_16 = theory_based_model %>%
  ggplot() + aes(x = weight, y = pain) + geom_point() + geom_smooth(method = "lm")

fig_17 = theory_based_model %>%
  ggplot() + aes(x = IQ, y = pain) + geom_point() + geom_smooth(method = "lm")

fig_18 = theory_based_model %>%
  ggplot() + aes(x = household_income, y = pain) + geom_point() + geom_smooth(method = "lm")

grid.arrange(fig_10, fig_11, fig_12, fig_13, fig_14, fig_15, fig_16, ncol=2) 

summary(theory_based_model)

AIC(theory_based_model)

confint(theory_based_model)

standardCoefs(theory_based_model)

model_2 %>% plot(which = 5) 

model_2 %>% plot(which = 4)

describe(residuals(theory_based_model))

theory_based_model %>% plot(which = 2)

theory_based_model %>% residualPlots()

theory_based_model %>% plot(which = 3)

theory_based_model %>% ncvTest()

theory_based_model %>% bptest()

vif(theory_based_model)

step(object = theory_based_model, direction = "backward")

backward_model = lm(formula = pain ~ sex + age + pain_cat + cortisol_serum + mindfulness + household_income, data = data_sample_1, subset = -c(93, 100,114))

summary(backward_model)

AIC(backward_model)

confint(backward_model)

standardCoefs(backward_model)

theory_based_model_predicted = predict(theory_based_model, data_sample_2)

backward_model_predicted = predict(backward_model, data_sample_2)

theory_based_model_predicted = predict(theory_based_model, data_sample_2)

backward_model_predicted = predict(backward_model, data_sample_2)

sum(theory_based_model_predicted)

coef_table(theory_based_model)

coef_table(backward_model)

# ASSIGNMENT 3

library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest) 
library(MuMIn)
library(optimx)

stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}

data_sample_3 = read_csv("https://tinyurl.com/ha-dataset3")

data_sample_4 = read_csv("https://tinyurl.com/ha-dataset4")

view(data_sample_3)

view(data_sample_4)

data_sample_3 = data_sample_3 %>% 
  mutate(ID = factor(ID), sex = factor(sex), hospital = factor(hospital))

data_sample_4 = data_sample_4 %>% 
  mutate(ID = factor(ID), sex = factor(sex), hospital = factor(hospital))

data_sample_3 = data_sample_3 %>% mutate(hospital = recode(hospital, "hospital_1" = 1, "hospital_2" = 2, "hospital_3" = 3, "hospital_4" = 4, "hospital_5" = 5, "hospital_6" = 6, "hospital_7" = 7, "hospital_8" = 8, "hospital_9" = 9, "hospital_10" = 10)) 

data_sample_3_random_intercept = lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1|hospital), data = data_sample_3)

data_sample_3_withpreds = data_sample_3
data_sample_3_withpreds$pred_int = predict(data_sample_3_random_intercept)

summary(data_sample_3_random_intercept)

confint(data_sample_3_random_intercept)

r2beta(data_sample_3_random_intercept, method = "nsj", data = data_sample_3)

r.squaredGLMM(data_sample_3_random_intercept) 

cAIC(data_sample_3_random_intercept)$caic

stdCoef.merMod(data_sample_3_random_intercept)

summary(model_2_final)

confint(model_2_final)

data_sample_4 = data_sample_4 %>% mutate(hospital = recode(hospital, "hospital_11" = 1, "hospital_12" = 2, "hospital_13" = 3, "hospital_14" = 4, "hospital_15" = 5, "hospital_16" = 6, "hospital_17" = 7, "hospital_18" = 8, "hospital_19" = 9, "hospital_20" = 10)) 

prediction_data = predict(data_sample_3_random_intercept, data_sample_4)

RSS = sum((data_sample_4[, "pain"] - prediction_data)^2)

mod_mean <- lm(pain ~ 1, data = data_sample_4)

TSS = sum((data_sample_4[, "pain"] - predict(mod_mean))^2)

1-(RSS/TSS)

data_sample_3_new_random_intercept = lmer(pain ~ age + cortisol_serum + mindfulness + (1|hospital), data = data_sample_3)

data_sample_3_new_random_slope = lmer(pain ~ hospital + age + cortisol_serum + mindfulness + (cortisol_serum + mindfulness|hospital), data = data_sample_3)

data_sample_3_new_random_slope_opt = lmer(pain ~ hospital + age + cortisol_serum + mindfulness + (cortisol_serum + mindfulness|hospital), control = lmerControl(optimizer = "Nelder_Mead"), data = data_sample_3)

data_sample_3 = data_sample_3 %>%
  mutate(pred_int = predict(data_sample_3_new_random_intercept),
         pred_slope = predict(data_sample_3_new_random_slope_opt))

data_sample_3 %>%
  ggplot() +
  aes(y = pain, x = cortisol_serum, group = hospital)+
  geom_point(aes(color = hospital), size = 4) +
  geom_line(color='red', aes(y=pred_int, x=cortisol_serum))+
  facet_wrap( ~ hospital, ncol = 2)

data_sample_3 %>%
  ggplot() +
  aes(y = pain, x = age, group = hospital)+
  geom_point(aes(color = hospital), size = 4) +
  geom_line(color='red', aes(y=pred_int, x=age))+
  facet_wrap( ~ hospital, ncol = 2)

data_sample_3 %>%
  ggplot() +
  aes(y = pain, x = mindfulness, group = hospital)+
  geom_point(aes(color = hospital), size = 4) +
  geom_line(color='red', aes(y=pred_int, x=mindfulness))+
  facet_wrap( ~ hospital, ncol = 2)

