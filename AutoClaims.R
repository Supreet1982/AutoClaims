str(AutoClaims)

library(tidyverse)
library(e1071)
library(GGally)
library(effects)
library(MASS)
library(caret)
library(car)
library(Metrics)

#Factorize the categorical variables

AutoClaims$Gender <- as.factor(AutoClaims$Gender)
AutoClaims$VehicleType <- as.factor(AutoClaims$VehicleType)
AutoClaims$Region <- as.factor(AutoClaims$Region)
AutoClaims$PolicyType <- as.factor(AutoClaims$PolicyType)

str(AutoClaims)

#Univariate analysis

#Target variable follows Gamma distribution

AutoClaims %>%
  ggplot(aes(ClaimAmount)) +
  geom_histogram(bins = 100)

#Predictor variables analysis

cat_vars <- c('Gender', 'VehicleType', 'Region', 'PolicyType')

#Relevel categorical predictors

for (i in cat_vars) {
  table2 <- as.data.frame(table(AutoClaims[,i]))
  max <- which.max(table2[,2])
  level_name <- as.character(table2[max, 1])
  AutoClaims[,i] <- relevel(AutoClaims[, i], ref = level_name)
}

summary(AutoClaims[,cat_vars])

#Numeric variables

skewness(AutoClaims$DrivingExperience) #e1071

AutoClaims %>%
  ggplot(aes(DrivingExperience)) +
  geom_histogram()

#Bivariate analysis

#Numeric/Numeric

num_vars <- names(AutoClaims)[sapply(AutoClaims, is.numeric)]

num_df <- AutoClaims[sapply(AutoClaims, is.numeric)]

pairs(num_df)
ggpairs(num_df) #GGally

#Numeric/Categorical

AutoClaims %>%
  group_by(PolicyType) %>%
  summarize(
    mean = mean(ClaimAmount),
    median = median(ClaimAmount),
    n = n()
)

AutoClaims %>%
  ggplot(aes(VehicleAge, ClaimAmount)) +
  geom_point() +
  facet_wrap(~VehicleType)

AutoClaims %>%
  ggplot(aes(Region, ClaimAmount)) +
  geom_boxplot() +
  facet_wrap(~PolicyType)

#Interactions

AutoClaims %>%
  ggplot(aes(VehicleAge, ClaimAmount)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = 'glm', se = FALSE, method.args = list(family = 
                                                    Gamma(link = 'log'))) +
  facet_wrap(~VehicleType)

glm1 <- glm(ClaimAmount ~ VehicleType*PolicyType,data = AutoClaims, 
            family = Gamma(link = 'log'))

plot(allEffects(glm1)) #effects

glm0 <- glm(ClaimAmount ~ VehicleType + PolicyType,data = AutoClaims, 
            family = Gamma(link = 'log'))

anova(glm0, glm1, test = 'Chisq')
AIC(glm0, glm1)

#Check whether target variable follows Gamma distribution

fit <- fitdistr(AutoClaims$ClaimAmount, densfun = 'gamma') #MASS
fit$estimate
shape_hat <- fit$estimate['shape']
rate_hat <- fit$estimate['rate']
mean_hat <- fit$estimate['mean']
sd_hat <- fit$estimate['sd']
l_hat <- fit$estimate['lambda']
m_hat <- fit$estimate['meanlog']
s_hat <- fit$estimate['sdlog']
  
ks.test(AutoClaims$ClaimAmount, 'pgamma', shape = shape_hat,
        rate = rate_hat)

#Model Development

#Train-test split

set.seed(42)

partition <- createDataPartition(AutoClaims$ClaimAmount, p = 0.7, list = FALSE)

AutoClaims_train <- AutoClaims[partition,]
AutoClaims_test <- AutoClaims[-partition,]

mean(AutoClaims_train$ClaimAmount)
mean(AutoClaims_test$ClaimAmount)

#Model development

glm2 <- glm(ClaimAmount ~ ., data = AutoClaims_train, family = Gamma(
  link = 'log'
))

summary(glm2)

glm3 <- glm(ClaimAmount ~ Age + ClaimHistory, family = Gamma(link = 'log'),
            data = AutoClaims_train)

summary(glm3)

#Model Diagnosis

res_dev <- residuals(glm2, type = 'deviance')
res_pearson <- residuals(glm2, type = 'pearson')
fitted_vals <- fitted(glm2)

plot(fitted_vals, res_dev, xlab = 'Fitted Value', ylab = 'Deviance residuals',
     main = 'Fitted vs Deviance Residual')
abline(h = 0, col = 'red')


res_dev2 <- residuals(glm3, type = 'deviance')
res_pearson2 <- residuals(glm3, type = 'pearson')
fitted_vals2 <- fitted(glm3)

plot(fitted_vals2, res_dev2, xlab = 'Fitted Value', ylab = 'Deviance residuals',
     main = 'Fitted vs Deviance Residual')
abline(h = 0, col = 'red')

qqnorm(res_dev)
qqline(res_dev, col='red')

cooksD <- cooks.distance(glm2)
hatsvalues <- hatvalues(glm2)

plot(cooksD, type = 'h', main = "Cook's Distance", ylab = 'Distance')
abline(h = 4/length(cooksD), col = 'red', lty = 2)

#RMSE

pred <- predict(glm2, newdata = AutoClaims_test, type = 'response')
actual <- AutoClaims_test$ClaimAmount

rmse <- sqrt(mean((pred - actual)^2))
rmse
rmse(pred, actual)

################################################################################

#Confidence Interval

pred_link <- predict(glm2, newdata = AutoClaims_test, se.fit = TRUE,
                     type = 'link')

z <- 1.96

fit_link <- pred_link$fit
se_link <- pred_link$se.fit

lower_link <- fit_link - z * se_link
upper_link <- fit_link + z * se_link

pred_response <- exp(fit_link)
lower_res <- exp(lower_link)
upper_rep <- exp(upper_link)

plot_data <- data.frame(
  Age = AutoClaims_test$Age,
  Predicted = pred_response,
  upper = upper_rep,
  lower = lower_res
)

plot_data %>%
  ggplot(aes(Age, Predicted)) +
  geom_point(color = 'blue', alpha = 0.6) +
  geom_line(color = 'darkblue') +
  geom_ribbon(aes(ymin = lower, ymax = upper, color = 'lightblue', 
                  alpha = 0.3)) + 
  theme(legend.position = 'none') +
  geom_smooth(aes(y=Predicted), method = 'loess', se = FALSE)

plot_data2 <- data.frame(
  CH = AutoClaims_test$ClaimHistory,
  Predicted = pred_response,
  upper = upper_rep,
  lower = lower_res
)

plot_data2 %>%
  ggplot(aes(CH, Predicted)) +
  geom_point(color = 'blue', alpha = 0.6) +
  geom_line(color = 'darkblue') +
  geom_ribbon(aes(ymin = lower, ymax = upper, color = 'lightblue', 
                  alpha = 0.3)) + 
  theme(legend.position = 'none') +
  geom_smooth(aes(y=Predicted), method = 'loess', se = FALSE)

################################################################################

#Prediction Interval

mu_hat <- predict(glm2, newdata = AutoClaims_test, type = 'response')
phi_hat <- summary(glm2)$dispersion

shape_hat <- 1 / phi_hat
scale_hat <- mu_hat * phi_hat

lower_pi <- qgamma(0.025, shape = shape_hat, scale = scale_hat)
upper_pi <- qgamma(0.975, shape = shape_hat, scale = scale_hat)

pi_data <- data.frame(
  Age = AutoClaims_test$Age,
  Predicted = mu_hat,
  lowerPI = lower_pi,
  upperPI = upper_pi
)

pi_data <- pi_data[order(pi_data$Age),]

pi_data %>%
  ggplot(aes(Age, Predicted)) +
  geom_point(color = 'blue', alpha = 0.6) +
  geom_ribbon(aes(ymin=lowerPI, ymax=upperPI), fill = 'lightpink', 
              alpha = 0.3) +
  geom_smooth(aes(y=Predicted), method = 'loess', se = TRUE)

################################################################################








