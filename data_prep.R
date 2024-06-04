########################################
#                                      #
#               Libraries              #
#                                      #
########################################

# setwd("advanced econometric\\projekt\\projekt2\\final_econometrics")
# setwd("/Users/sergiocarcamo/Dev/econometrics")

library("readr")
library("dplyr")
library("sandwich")
library("lmtest")
library("MASS")
#install.packages("mfx")
library("mfx")
#install.packages("LogisticDx") # error
library("LogisticDx")
library("aod")
#install.packages("logistf")
library("logistf")
library("car")
library("stargazer")
#install.packages("aods3")
library("aods3")
library("DescTools")
library("ggplot2")
library("AER")
library("car")
library("gridExtra")

data <- read.csv("data/ecomerce_data.csv", sep=";")

glimpse(data)
colnames(data)
table(data$Churn)
########################################
#                                      #
#         Data Visualization           #
#                                      #
########################################
# QUANTITATIVE VARIABLES
# tenure and WarehouseToHome
tenure_plot <- ggplot(data, aes(y = factor(1), x = Tenure)) + 
  geom_violin(fill = "#354f52", trim = F) + 
  labs(title = "Tenure", x = "", y = "") +
  theme(panel.background = element_rect(fill = "white"))

WarehouseToHome_plot <- ggplot(data, aes(y = factor(1), x = WarehouseToHome)) + 
  geom_violin(fill = "#2f3e46", trim = F) + 
  labs(title = "Warehouse To Home", x = "", y = "") +
  theme(panel.background = element_rect(fill = "white"))

# Arrange the plots on the same canvas
grid.arrange(tenure_plot, WarehouseToHome_plot, ncol = 2)

# cashback and order amount
CashbackAmount_plot <- ggplot(data, aes(y = factor(1), x = CashbackAmount)) + 
  geom_violin(fill = "#354f52", trim = F) + 
  labs(title = "Cashback Amount", x = "", y = "") +
  theme(panel.background = element_rect(fill = "white"))

OrderAmount_plot <- ggplot(data, aes(y = factor(1), x = OrderAmountHikeFromlastYear)) + 
  geom_violin(fill = "#2f3e46", trim = F) + 
  labs(title = "Order Amount Hike From last Year", x = "", y = "") +
  theme(panel.background = element_rect(fill = "white"))

# Arrange the plots on the same canvas
grid.arrange(CashbackAmount_plot, OrderAmount_plot, ncol = 2)

# Number of address and satisfaction score
NumberOfAddress_plot <- ggplot(data, aes(y = factor(1), x = NumberOfAddress)) + 
  geom_violin(fill = "#354f52", trim = F) + 
  labs(title = "Number Of Address", x = "", y = "") +
  theme(panel.background = element_rect(fill = "white"))

SatisfactionScore_plot <- ggplot(data, aes(y = factor(1), x = SatisfactionScore)) + 
  geom_violin(fill = "#2f3e46", trim = F) + 
  labs(title = "Satisfaction Score", x = "", y = "") +
  theme(panel.background = element_rect(fill = "white"))

# Arrange the plots on the same canvas
grid.arrange(NumberOfAddress_plot, SatisfactionScore_plot, ncol = 2)

# OrderCount and DaySinceLastOrder
OrderCount_plot <- ggplot(data, aes(y = factor(1), x = OrderCount)) + 
  geom_violin(fill = "#354f52", trim = F) + 
  labs(title = "Order Count", x = "", y = "") +
  theme(panel.background = element_rect(fill = "white"))

DaySinceLastOrder_plot <- ggplot(data, aes(y = factor(1), x = DaySinceLastOrder)) + 
  geom_violin(fill = "#2f3e46", trim = F) + 
  labs(title = "Day Since Last Order", x = "", y = "") +
  theme(panel.background = element_rect(fill = "white"))

# Arrange the plots on the same canvas
grid.arrange(OrderCount_plot, DaySinceLastOrder_plot, ncol = 2)

# HourSpendOnApp and DaySinceLastOrder
HourSpendOnApp_plot <- ggplot(data, aes(y = factor(1), x = HourSpendOnApp)) + 
  geom_violin(fill = "#354f52", trim = F) + 
  labs(title = "Hour Spend On App", x = "", y = "") +
  theme(panel.background = element_rect(fill = "white"))

NumberOfDeviceRegistered_plot <- ggplot(data, aes(y = factor(1), x = NumberOfDeviceRegistered)) + 
  geom_violin(fill = "#2f3e46", trim = F) + 
  labs(title = "Number Of Device Registered", x = "", y = "") +
  theme(panel.background = element_rect(fill = "white"))

# Arrange the plots on the same canvas
grid.arrange(HourSpendOnApp_plot, NumberOfDeviceRegistered_plot, ncol = 2)

# QUALITATIVE VARIABLES
# plot all categorical vairables on one canvas
colors = c("#cad2c5", "#84a98c", "#52796f", "#354f52", "#2f3e46")
variables <- c("PreferredLoginDevice", "CityTier", "MaritalStatus", "Complain", "Gender", "PreferredPaymentMode", "PreferedOrderCat", "Complain", "Churn")
plots <- list()
for (i in 1:length(variables)) {
  plots[[i]] <- ggplot(data, aes_string(x = variables[i])) + 
    geom_bar(fill = colors[i %% length(colors) + 1]) + 
    labs(title = variables[i], x = "", y = "") +
    theme(panel.background = element_rect(fill = "white"), legend.position = "none")
}

do.call("grid.arrange", c(plots, ncol = 2))

# logit vs probit cumulative distribution functions
x_values <- data.frame(x = seq(-8, 8, length.out = 100))
ggplot(data = x_values, aes(x = x)) +
  stat_function(fun = plogis, aes(color = "Logit"), size = 1.2, linetype = 1) +
  stat_function(fun = pnorm, aes(color = "Probit"), size = 1.2, linetype = 2) +
  scale_color_manual(name = "Function",
                     values = c("Logit" = "#84a98c", "Probit" = "#354f52")) +
  labs(x = "") +
  theme_minimal()

########################################
#                                      #
#               Data Prep              #
#                                      #
########################################

#remove id column
data <- subset(data, select = -CustomerID)

# count of all unique values
unique_value_counts <- data %>%
  summarise(across(everything(), ~n_distinct(.)))
glimpse(unique_value_counts)

# Gender
table(data$Gender)
data$Gender <- ifelse(data$Gender == "Male", 1, 0)

# PreferredLoginDevice
table(data$PreferredLoginDevice)
data$LoginDevice_Computer <- ifelse(data$PreferredLoginDevice == "Computer", 1, 0)
data$LoginDevice_MPhone <- ifelse(data$PreferredLoginDevice == "Mobile Phone", 1, 0)
data$LoginDevice_Phone <- ifelse(data$PreferredLoginDevice == "Phone", 1, 0)
data <- subset(data, select = -PreferredLoginDevice)
glimpse(data)

# PreferredPaymentMode
table(data$PreferredPaymentMode)
data$PreferredPaymentMode[data$PreferredPaymentMode == "Cash on Delivery"] <- "Cash"
data$PreferredPaymentMode[data$PreferredPaymentMode == "COD"] <- "Cash"
data$PreferredPaymentMode[data$PreferredPaymentMode == "CC"] <- "Credit card"
data$PreferredPaymentMode[data$PreferredPaymentMode == "Credit Card"] <- "Credit card"
data$PaymentMode_Cash <- ifelse(data$PreferredPaymentMode == "Cash", 1, 0)
data$PaymentMode_CCard <- ifelse(data$PreferredPaymentMode == "Credit card", 1, 0)
data$PaymentMode_DCard <- ifelse(data$PreferredPaymentMode == "Debit Card", 1, 0)
data$PaymentMode_EWallet <- ifelse(data$PreferredPaymentMode == "E wallet", 1, 0)
data$PaymentMode_UPI <- ifelse(data$PreferredPaymentMode == "UPI", 1, 0)
data <- subset(data, select = -PreferredPaymentMode)
glimpse(data)

# CityTier
table(data$CityTier)
data$CityTier1 <- ifelse(data$CityTier == 1, 1, 0)
data$CityTier2 <- ifelse(data$CityTier == 2, 1, 0)
data$CityTier3 <- ifelse(data$CityTier == 3, 1, 0)
data <- subset(data, select = -CityTier)
glimpse(data)

# PreferedOrderCat
table(data$PreferedOrderCat)
data$PreferedOrderCat[data$PreferedOrderCat == "Mobile"] <- "Mobile"
data$PreferedOrderCat[data$PreferedOrderCat == "Mobile Phone"] <- "Mobile"
data$OrderCat_Fashion <- ifelse(data$PreferedOrderCat == "Fashion", 1, 0)
data$OrderCat_Grocery <- ifelse(data$PreferedOrderCat == "Grocery", 1, 0)
data$OrderCat_Laptop <- ifelse(data$PreferedOrderCat == "Laptop & Accessory", 1, 0)
data$OrderCat_Mobile <- ifelse(data$PreferedOrderCat == "Mobile", 1, 0)
data$OrderCat_Other <- ifelse(data$PreferedOrderCat == "Others", 1, 0)
data <- subset(data, select = -PreferedOrderCat)
glimpse(data)

# MaritalStatus
table(data$MaritalStatus)
data$Martial_Divorced <- ifelse(data$MaritalStatus == "Divorced", 1, 0)
data$Martial_Married <- ifelse(data$MaritalStatus == "Married", 1, 0)
data$Martial_Single <- ifelse(data$MaritalStatus == "Single", 1, 0)
data <- subset(data, select = -MaritalStatus)
glimpse(data)

# count unique values in each columns
unique_value_counts <- data %>%
  summarise(across(everything(), ~n_distinct(.)))
glimpse(unique_value_counts)

# count Na values in each columns
colSums(is.na(data)) %>% 
  sort()

colnames(data)

########################################
#                                      #
#               Model                  #
#                                      #
########################################
# remove one level from all variable levels
# treat them as base level
final_data <- subset(data, select = -c(LoginDevice_Computer, PaymentMode_Cash, CityTier1, OrderCat_Fashion, Martial_Single))

# logit general model
logit_model <- glm(Churn ~ Tenure + 
                     WarehouseToHome + 
                     Gender + 
                     HourSpendOnApp + 
                     NumberOfDeviceRegistered + 
                     SatisfactionScore + 
                     NumberOfAddress + 
                     Complain + 
                     OrderAmountHikeFromlastYear + 
                     CouponUsed + 
                     OrderCount + 
                     DaySinceLastOrder + 
                     CashbackAmount + 
                     LoginDevice_MPhone + 
                     LoginDevice_Phone + 
                     PaymentMode_CCard + 
                     PaymentMode_DCard + 
                     PaymentMode_EWallet + 
                     PaymentMode_UPI + 
                     CityTier2 + 
                     CityTier3 + 
                     OrderCat_Grocery + 
                     OrderCat_Laptop + 
                     OrderCat_Mobile + 
                     OrderCat_Other + 
                     Martial_Divorced + 
                     Martial_Married + 
                     Gender * Complain +
                     OrderCount * DaySinceLastOrder +
                     NumberOfDeviceRegistered * SatisfactionScore,
                   data = final_data, 
                   family=binomial(link="logit"))
summary(logit_model)

# probit general model
probit_model <- glm(Churn ~ Tenure + 
                     WarehouseToHome + 
                     Gender + 
                     HourSpendOnApp + 
                     NumberOfDeviceRegistered + 
                     SatisfactionScore + 
                     NumberOfAddress + 
                     Complain + 
                     OrderAmountHikeFromlastYear + 
                     CouponUsed + 
                     OrderCount + 
                     DaySinceLastOrder + 
                     CashbackAmount + 
                     LoginDevice_MPhone + 
                     LoginDevice_Phone + 
                     PaymentMode_CCard + 
                     PaymentMode_DCard + 
                     PaymentMode_EWallet + 
                     PaymentMode_UPI + 
                     CityTier2 + 
                     CityTier3 + 
                     OrderCat_Grocery + 
                     OrderCat_Laptop + 
                     OrderCat_Mobile + 
                     OrderCat_Other + 
                     Martial_Divorced + 
                     Martial_Married + 
                     Gender * Complain +
                     OrderCount * DaySinceLastOrder +
                     NumberOfDeviceRegistered * SatisfactionScore,
                   data = final_data, 
                   family=binomial(link="probit"))
summary(probit_model)

model_list <- list(logit_model, probit_model)

# quality table
model_names <- c("Logit Model", "Probit Model")
stargazer(model_list, type = "text", title = "Regression Model Comparison",
          align = TRUE, column.labels = model_names, out = "models.txt")

general <- logit_model

# likelihood ratio test
# the general model is better than a model only with intercept
null_probit = glm(Churn~1, data=data, family=binomial(link="logit"))
lr_test <- lrtest(general, null_probit)
stargazer(lr_test, type = "text", summary = FALSE, title = "Likelihood Ratio Test Results", out="lrtest.txt")

# Remove insignificant variables from the model
# Step 1 - check if all insignificant variables are jointly significant
insignificant_model_1 <- glm(Churn ~ Gender +
                               HourSpendOnApp +
                               OrderAmountHikeFromlastYear +
                               CouponUsed +
                               PaymentMode_EWallet +
                               OrderCat_Grocery,
                             data = final_data,
                             family = binomial(link = "logit"))
anova_results <- anova(general, insignificant_model_1, test = "LRT")
anova_results
stargazer(anova_results, type = "text", summary = FALSE, title = "ANOVA Likelihood Ratio Test Results", 
          rownames = TRUE, digits = 3, out = "anov_insig.txt")
# the variables are jointly significant so we have to take the General-to-specific approach

# Step 2 - remove HourSpendOnApp (p-value: 0.97)
reduced_model_1 <- glm(Churn ~ Tenure + 
                     WarehouseToHome + 
                     Gender + 
                     NumberOfDeviceRegistered + 
                     SatisfactionScore + 
                     NumberOfAddress + 
                     Complain + 
                     OrderAmountHikeFromlastYear + 
                     CouponUsed + 
                     OrderCount + 
                     DaySinceLastOrder + 
                     CashbackAmount + 
                     LoginDevice_MPhone + 
                     LoginDevice_Phone + 
                     PaymentMode_CCard + 
                     PaymentMode_DCard + 
                     PaymentMode_EWallet + 
                     PaymentMode_UPI + 
                     CityTier2 + 
                     CityTier3 + 
                     OrderCat_Grocery + 
                     OrderCat_Laptop + 
                     OrderCat_Mobile + 
                     OrderCat_Other + 
                     Martial_Divorced + 
                     Martial_Married + 
                     Gender * Complain +
                     OrderCount * DaySinceLastOrder +
                     NumberOfDeviceRegistered * SatisfactionScore,
                   data = final_data, 
                   family=binomial(link="logit"))
summary(reduced_model_1)
anova(general, reduced_model_1, test = "LRT")
# there are still insignificant variables in reduced_model_1 
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 3
# let's drop "the most insignificant" - PaymentMode_EWallet (p-value: 0.67)
reduced_model_2 <- glm(Churn ~ Tenure + 
                         WarehouseToHome + 
                         Gender + 
                         NumberOfDeviceRegistered + 
                         SatisfactionScore + 
                         NumberOfAddress + 
                         Complain + 
                         OrderAmountHikeFromlastYear + 
                         CouponUsed + 
                         OrderCount + 
                         DaySinceLastOrder + 
                         CashbackAmount + 
                         LoginDevice_MPhone + 
                         LoginDevice_Phone + 
                         PaymentMode_CCard + 
                         PaymentMode_DCard + 
                         PaymentMode_UPI + 
                         CityTier2 + 
                         CityTier3 + 
                         OrderCat_Grocery + 
                         OrderCat_Laptop + 
                         OrderCat_Mobile + 
                         OrderCat_Other + 
                         Martial_Divorced + 
                         Martial_Married + 
                         Gender * Complain +
                         OrderCount * DaySinceLastOrder +
                         NumberOfDeviceRegistered * SatisfactionScore,
                       data = final_data, 
                       family=binomial(link="logit"))
summary(reduced_model_2)
anova(general, reduced_model_2, test = "LRT")
# there are still insignificant variables in reduced_model_2
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 4
# let's drop "the most insignificant" - OrderAmountHikeFromlastYear (p-value: 0.63)
reduced_model_3 <- glm(Churn ~ Tenure + 
                         WarehouseToHome + 
                         Gender + 
                         NumberOfDeviceRegistered + 
                         SatisfactionScore + 
                         NumberOfAddress + 
                         Complain + 
                         CouponUsed + 
                         OrderCount + 
                         DaySinceLastOrder + 
                         CashbackAmount + 
                         LoginDevice_MPhone + 
                         LoginDevice_Phone + 
                         PaymentMode_CCard + 
                         PaymentMode_DCard + 
                         PaymentMode_UPI + 
                         CityTier2 + 
                         CityTier3 + 
                         OrderCat_Grocery + 
                         OrderCat_Laptop + 
                         OrderCat_Mobile + 
                         OrderCat_Other + 
                         Martial_Divorced + 
                         Martial_Married + 
                         Gender * Complain +
                         OrderCount * DaySinceLastOrder +
                         NumberOfDeviceRegistered * SatisfactionScore,
                       data = final_data, 
                       family=binomial(link="logit"))
summary(reduced_model_3)
anova(general, reduced_model_3, test = "LRT")
# there are still insignificant variables in reduced_model_3
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 5
# let's drop "the most insignificant" - Gender (p-value: 0.21)
reduced_model_4 <- glm(Churn ~ Tenure + 
                         WarehouseToHome + 
                         NumberOfDeviceRegistered + 
                         SatisfactionScore + 
                         NumberOfAddress + 
                         Complain + 
                         CouponUsed + 
                         OrderCount + 
                         DaySinceLastOrder + 
                         CashbackAmount + 
                         LoginDevice_MPhone + 
                         LoginDevice_Phone + 
                         PaymentMode_CCard + 
                         PaymentMode_DCard + 
                         PaymentMode_UPI + 
                         CityTier2 + 
                         CityTier3 + 
                         OrderCat_Grocery + 
                         OrderCat_Laptop + 
                         OrderCat_Mobile + 
                         OrderCat_Other + 
                         Martial_Divorced + 
                         Martial_Married + 
                         Gender * Complain +
                         OrderCount * DaySinceLastOrder +
                         NumberOfDeviceRegistered * SatisfactionScore,
                       data = final_data, 
                       family=binomial(link="logit"))
summary(reduced_model_4)
anova(general, reduced_model_4, test = "LRT")
# there are still insignificant variables in reduced_model_4
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 6
# let's drop "the most insignificant" - OrderCount (p-value: 0.12)
reduced_model_5 <- glm(Churn ~ Tenure + 
                         WarehouseToHome + 
                         NumberOfDeviceRegistered + 
                         SatisfactionScore + 
                         NumberOfAddress + 
                         Complain + 
                         CouponUsed + 
                         DaySinceLastOrder + 
                         CashbackAmount + 
                         LoginDevice_MPhone + 
                         LoginDevice_Phone + 
                         PaymentMode_CCard + 
                         PaymentMode_DCard + 
                         PaymentMode_UPI + 
                         CityTier2 + 
                         CityTier3 + 
                         OrderCat_Grocery + 
                         OrderCat_Laptop + 
                         OrderCat_Mobile + 
                         OrderCat_Other + 
                         Martial_Divorced + 
                         Martial_Married + 
                         Gender * Complain +
                         OrderCount * DaySinceLastOrder +
                         NumberOfDeviceRegistered * SatisfactionScore,
                       data = final_data, 
                       family=binomial(link="logit"))
summary(reduced_model_5)
anova(general, reduced_model_5, test = "LRT")
# there are still insignificant variables in reduced_model_5
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 7
# let's drop "the most insignificant" - CouponUsed (p-value: 0.07)
reduced_model_6 <- glm(Churn ~ Tenure + 
                         WarehouseToHome + 
                         NumberOfDeviceRegistered + 
                         SatisfactionScore + 
                         NumberOfAddress + 
                         Complain + 
                         DaySinceLastOrder + 
                         CashbackAmount + 
                         LoginDevice_MPhone + 
                         LoginDevice_Phone + 
                         PaymentMode_CCard + 
                         PaymentMode_DCard + 
                         PaymentMode_UPI + 
                         CityTier2 + 
                         CityTier3 + 
                         OrderCat_Grocery + 
                         OrderCat_Laptop + 
                         OrderCat_Mobile + 
                         OrderCat_Other + 
                         Martial_Divorced + 
                         Martial_Married + 
                         Gender * Complain +
                         OrderCount * DaySinceLastOrder +
                         NumberOfDeviceRegistered * SatisfactionScore,
                       data = final_data, 
                       family=binomial(link="logit"))
summary(reduced_model_6)
anova(general, reduced_model_6, test = "LRT")
# there are still insignificant variables in reduced_model_6
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 8
# let's drop "the most insignificant" - OrderCat_Grocery (p-value: 0.09)
reduced_model_7 <- glm(Churn ~ Tenure + 
                         WarehouseToHome + 
                         NumberOfDeviceRegistered + 
                         SatisfactionScore + 
                         NumberOfAddress + 
                         Complain + 
                         DaySinceLastOrder + 
                         CashbackAmount + 
                         LoginDevice_MPhone + 
                         LoginDevice_Phone + 
                         PaymentMode_CCard + 
                         PaymentMode_DCard + 
                         PaymentMode_UPI + 
                         CityTier2 + 
                         CityTier3 + 
                         OrderCat_Laptop + 
                         OrderCat_Mobile + 
                         OrderCat_Other + 
                         Martial_Divorced + 
                         Martial_Married + 
                         Gender * Complain +
                         OrderCount * DaySinceLastOrder +
                         NumberOfDeviceRegistered * SatisfactionScore,
                       data = final_data, 
                       family=binomial(link="logit"))
summary(reduced_model_7)
anova(general, reduced_model_7, test = "LRT")
# there is no insignificant variables anymore
# we can stop with the General to specific approach now

########################################
#                                      #
#               Final Model            #
#                                      #
########################################
# present final model in quality table
final_model <- reduced_model_7
stargazer(final_model, type = "text", title = "final model", align=T, out = "final.txt")

# quality table presenting general and final model
model_list <- list(general, final_model)
model_names <- c("General Model", "Final Model")
stargazer(model_list, type = "text", title = "Regression Model Comparison",
          align = TRUE, column.labels = model_names, out = "final_general.txt")

# marginal effects for average characteristics
probitmfx(formula=Churn ~ Tenure + 
            WarehouseToHome + 
            NumberOfDeviceRegistered + 
            SatisfactionScore + 
            NumberOfAddress + 
            Complain + 
            DaySinceLastOrder + 
            CashbackAmount + 
            LoginDevice_MPhone + 
            LoginDevice_Phone + 
            PaymentMode_CCard + 
            PaymentMode_DCard + 
            PaymentMode_UPI + 
            CityTier2 + 
            CityTier3 + 
            OrderCat_Laptop + 
            OrderCat_Mobile + 
            OrderCat_Other + 
            Martial_Divorced + 
            Martial_Married + 
            Gender * Complain +
            OrderCount * DaySinceLastOrder +
            NumberOfDeviceRegistered * SatisfactionScore
          , data = final_data
          , atmean = T)

# average marginal effects
probitmfx(formula=Churn ~ Tenure + 
            WarehouseToHome + 
            NumberOfDeviceRegistered + 
            SatisfactionScore + 
            NumberOfAddress + 
            Complain + 
            DaySinceLastOrder + 
            CashbackAmount + 
            LoginDevice_MPhone + 
            LoginDevice_Phone + 
            PaymentMode_CCard + 
            PaymentMode_DCard + 
            PaymentMode_UPI + 
            CityTier2 + 
            CityTier3 + 
            OrderCat_Laptop + 
            OrderCat_Mobile + 
            OrderCat_Other + 
            Martial_Divorced + 
            Martial_Married + 
            Gender * Complain +
            OrderCount * DaySinceLastOrder +
            NumberOfDeviceRegistered * SatisfactionScore
          , data = final_data
          , atmean = F)

# marginal effects for user defined characteristics
source("functions/marginaleffects.R")
user.def.obs <- c(
  1,            # Intercept
  12,           # Tenure
  15,           # WarehouseToHome
  3,            # NumberOfDeviceRegistered
  4,            # SatisfactionScore
  2,            # NumberOfAddress
  1,            # Complain
  8,            # DaySinceLastOrder
  50,           # CashbackAmount
  1,            # LoginDevice_MPhone
  0,            # LoginDevice_Phone
  1,            # PaymentMode_CCard
  0,            # PaymentMode_DCard
  0,            # PaymentMode_UPI
  0,            # CityTier2
  1,            # CityTier3
  0,            # OrderCat_Laptop
  1,            # OrderCat_Mobile
  0,            # OrderCat_Other
  0,            # Martial_Divorced
  1,            # Martial_Married
  0,            # Gender
  5,            # OrderCount
  0 * 1,        # Gender * Complain
  5 * 8,        # OrderCount * DaySinceLastOrder
  3 * 4         # NumberOfDeviceRegistered * SatisfactionScore
)

marginaleffects(final_model, user.def.obs)
# linktest
source("functions/linktest.R")
linktest_result <- linktest(final_model)

# R-Squared 
PseudoR2(final_model,c("Tjur","McKelveyZavoina"))

# count r2
predicted_probabilities <- predict(final_model, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)
actual_classes <- final_model$y
correct_predictions <- sum(predicted_classes == actual_classes)
total_observations <- length(actual_classes)
count_R2 <- correct_predictions / total_observations
count_R2

# adj count r2
most_frequent_outcome_count <- max(table(actual_classes))
adjusted_count_R2 <- (correct_predictions - most_frequent_outcome_count) / (total_observations - most_frequent_outcome_count)
adjusted_count_R2

# Hosmer-Lemeshow and Osius-Rojekt test 
# Goodness-of-fit test used in logistic regression models
gof.results <- LogisticDx::gof(final_model)
gof.results$gof


# Hypothesis verification
# use wald.test 
final_model_with_omited <- glm(Churn ~ OrderAmountHikeFromlastYear + # ommited variable
                                 Tenure + 
                                 WarehouseToHome + 
                                 NumberOfDeviceRegistered + 
                                 SatisfactionScore + 
                                 NumberOfAddress + 
                                 Complain + 
                                 DaySinceLastOrder + 
                                 CashbackAmount + 
                                 LoginDevice_MPhone + 
                                 LoginDevice_Phone + 
                                 PaymentMode_CCard + 
                                 PaymentMode_DCard + 
                                 PaymentMode_UPI + 
                                 CityTier2 + 
                                 CityTier3 + 
                                 OrderCat_Laptop + 
                                 OrderCat_Mobile + 
                                 OrderCat_Other + 
                                 Martial_Divorced + 
                                 Martial_Married + 
                                 Gender * Complain +
                                 OrderCount * DaySinceLastOrder +
                                 NumberOfDeviceRegistered * SatisfactionScore
                               , data = final_data
                               , family = binomial(link = "logit"))
waldtest(final_model, final_model_with_omited, test="F")
# p-value of wald test is > 0.05 so we fail to reject H0, and can conclude
# that the coefficients of the omitted variables are significantly different from zero.
