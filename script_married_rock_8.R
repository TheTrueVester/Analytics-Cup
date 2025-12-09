#################################
##########Analytics Cup##########
#################################

##########libraries##########
library(tidyverse)
library(lubridate)
library(summarytools)
library(ggmap)
library(tidymodels)
library(ranger)

##########global settings##########
options(dplyr.width = Inf)
theme_set(theme_minimal())
set.seed(2022)

##########data##########
#trans <- read.csv("transactions.csv", encoding = "utf-8")
transactions <- read_csv("transactions.csv")
geo <- read_csv("geo.csv")
customers <- read_csv("customers.csv")

##########data preprocessing##########

#cleansing transactions
df_t <- transactions %>%
  mutate(OFFER_STATUS = tolower(OFFER_STATUS),
         OFFER_STATUS = if_else((OFFER_STATUS == "win" | OFFER_STATUS == "won"), 1, 0),
         OFFER_STATUS = as_factor(OFFER_STATUS),
         MO_CREATED_DATE = substring(MO_CREATED_DATE, 1, 10),
         MO_CREATED_DATE = if_else(PRICE_LIST != "Tarif public", format(strptime(MO_CREATED_DATE, "%d.%m.%Y"), "%Y-%m-%d"), MO_CREATED_DATE),
         MO_CREATED_DATE = as.Date(MO_CREATED_DATE),
         SO_CREATED_DATE = substring(SO_CREATED_DATE, 1, 10),
         SO_CREATED_DATE = if_else(PRICE_LIST != "Tarif public", format(strptime(SO_CREATED_DATE, "%d.%m.%Y"), "%Y-%m-%d"), SO_CREATED_DATE),
         SO_CREATED_DATE = as.Date(SO_CREATED_DATE),
         PRICE_LIST = as_factor(PRICE_LIST),
         OFFER_TYPE = as_factor(OFFER_TYPE),
         BUSINESS_TYPE = as_factor(BUSINESS_TYPE),
         CUSTOMER = gsub("[^0-9]", "", CUSTOMER),
         CUSTOMER = as.numeric(CUSTOMER),
         END_CUSTOMER_ID = case_when(END_CUSTOMER == "Yes" ~ as.character(CUSTOMER),
                                     END_CUSTOMER == "No" ~ NA_character_,
                                     TRUE ~ END_CUSTOMER),
         END_CUSTOMER_TF = case_when(END_CUSTOMER == "Yes" ~ 1,
                                     END_CUSTOMER == "No" ~ 0,
                                     (END_CUSTOMER != "Yes" & END_CUSTOMER != "No" & !is.na(END_CUSTOMER)) ~ 1)) %>%
  select(-END_CUSTOMER)
summary(df_t)
sample_n(df_t, 15)

#cleansing geo
df_geo <- geo %>% 
  filter(!is.na(SALES_LOCATION))
summary(df_geo)

sample_n(df_geo, 15)

#joining transactions and geo
df_t_geo <- left_join(df_t, df_geo) %>%
  mutate(COUNTRY = if_else(COUNTRY == "CH", "Switzerland", "France"))
summary(df_t_geo)
sample_n(df_t_geo, 15)

#cleansing customers
df_customers <- customers %>% mutate(REV_CURRENT_YEAR = gsub("[^0-9.]", "", REV_CURRENT_YEAR),
                                     REV_CURRENT_YEAR = as.numeric(REV_CURRENT_YEAR),
                                     CREATION_YEAR = paste0(substring(CREATION_YEAR, 7), "-1-1"),
                                     CREATION_YEAR = as_date(CREATION_YEAR))
summary(df_customers)
sample_n(df_customers, 15)

#exchange rates (as per 01th Dec. 2021)
#https://www1.oanda.com/lang/de/currency/converter/
euro_per_usd <- 0.88288
euro_per_pound <- 1.1755
euro_per_yuan <- 0.13855

#joining transactions, geo, and customers
df <- left_join(df_t_geo, df_customers, by=c("CUSTOMER", "COUNTRY"))

#normalizing revenue to USD & individualizing customer
df <- df %>% mutate(REV_CURRENT_YEAR.1 = if_else(CURRENCY == "US Dollar", REV_CURRENT_YEAR.1 * euro_per_usd, REV_CURRENT_YEAR.1),
                    REV_CURRENT_YEAR.1 = if_else(CURRENCY == "Pound Sterling", REV_CURRENT_YEAR.1 * euro_per_pound, REV_CURRENT_YEAR.1),
                    REV_CURRENT_YEAR.1 = if_else(CURRENCY == "Chinese Yuan", REV_CURRENT_YEAR.1 * euro_per_yuan, REV_CURRENT_YEAR.1),
                    REV_CURRENT_YEAR.2 = if_else(CURRENCY == "US Dollar", REV_CURRENT_YEAR.2 * euro_per_usd, REV_CURRENT_YEAR.2),
                    REV_CURRENT_YEAR.2 = if_else(CURRENCY == "Pound Sterling", REV_CURRENT_YEAR.2 * euro_per_pound, REV_CURRENT_YEAR.2),
                    REV_CURRENT_YEAR.2 = if_else(CURRENCY == "Chinese Yuan", REV_CURRENT_YEAR.2 * euro_per_yuan, REV_CURRENT_YEAR.2),
                    CUSTOMER = if_else(is.na(CUSTOMER), as.character(CUSTOMER), if_else(COUNTRY == "France", paste0("F", CUSTOMER), paste0("S", CUSTOMER))),
                    SALES_LOCATION = if_else((endsWith(SALES_LOCATION, "on Centre-Est") & startsWith(SALES_LOCATION, "Besan")), "Besanjon Centre-Est", SALES_LOCATION)) %>%
  select(-REV_CURRENT_YEAR)

#final df
summary(df)
sample_n(df, 15)

##########data inspection##########

offers_won_rate <- count(filter(df, OFFER_STATUS == 1)) / (count(filter(df, OFFER_STATUS == 1)) + count(filter(df, OFFER_STATUS == 0)))
offers_won_rate

#plotting won_rate against some factors

##########data finalization##########

#enriching data
#final preprocessing & variable selection

##########model training###########

df_final <- df %>% select(-END_CUSTOMER_ID) %>% 
  mutate(END_CUSTOMER_TF = if_else(is.na(END_CUSTOMER_TF), 2, END_CUSTOMER_TF),
         END_CUSTOMER_TF = as_factor(END_CUSTOMER_TF),
         ISIC = if_else(is.na(ISIC), 10000, ISIC),
         ISIC = as_factor(ISIC),
         TECH = as_factor(TECH),
         CUSTOMER = if_else(is.na(CUSTOMER), "new", CUSTOMER),
         CUSTOMER = as_factor(CUSTOMER),
         COUNTRY = if_else(is.na(COUNTRY), "new", COUNTRY),
         COUNTRY = as_factor(COUNTRY),
         SALES_OFFICE = if_else(is.na(SALES_OFFICE), "new", SALES_OFFICE),
         SALES_OFFICE = as_factor(SALES_OFFICE),
         SALES_BRANCH = if_else(is.na(SALES_BRANCH), "new", SALES_BRANCH),
         SALES_BRANCH = as_factor(SALES_BRANCH),
         OWNERSHIP = if_else(is.na(OWNERSHIP), "new", OWNERSHIP),
         OWNERSHIP = as_factor(OWNERSHIP),
         CURRENCY = if_else(is.na(CURRENCY), "new", CURRENCY),
         CURRENCY = as_factor(CURRENCY)) %>%
  select(-MO_ID, -SO_ID, -SALES_LOCATION)
summary(df_final)
sample_n(df_final, 15)

train_all <- df_final %>% filter(is.na(TEST_SET_ID))
test <- df_final %>% anti_join(train_all) %>% select(-OFFER_STATUS)
train_all <- train_all %>% mutate(TEST_SET_ID = 999999) #%>% na.omit

train_neg <- train_all %>% filter(OFFER_STATUS == 0)
n_pos <- as.integer(count(train_neg)*55/45)
train_pos <- train_all %>% filter(OFFER_STATUS == 1)
train_pos_fourth <- sample_n(train_pos, n_pos) 

train_50 <- rbind(train_pos_fourth, train_neg)
pos_rate <- count(train_50 %>% filter(OFFER_STATUS == 1)) / count(train_50)
pos_rate

summary(train_50)
sample_n(train_50, 15)

summary(test)
sample_n(test, 15)

##########start with model##########

rec <- recipe(OFFER_STATUS ~ ., data = train_50) %>%
  update_role(TEST_SET_ID, new_role = "ID") %>%
  step_mutate_at(where(is.Date), fn=decimal_date) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_zv(all_predictors())

rec
rec <- prep(rec, training = train_50)
processed <- bake(rec, test)
sample_n(processed, 18)

folds <- train_50 %>% vfold_cv(v=10, strata = COUNTRY)

model <- rand_forest(
  mode = 'classification',
  trees = 1000,
  #additional hyperparameters
) %>% set_engine("ranger",
                 #additional ranger-specific params
                 importance = "impurity" #needed for feature importance plot below
)
model

training_workflow <- 
  workflow() %>%
  add_recipe(rec) %>%
  add_model(model)
training_workflow

cv_fits <- 
  training_workflow %>% 
  fit_resamples(folds,
                metrics = metric_set(yardstick::bal_accuracy))
cv_fits %>% collect_metrics()

best_config <- cv_fits %>%
  select_best('bal_accuracy')

final_workflow <- training_workflow %>% 
  finalize_workflow(best_config)
final_workflow

trained_model <- final_workflow %>% 
  fit(data=train_50)
trained_model

train_set_with_predictions <-
  bind_cols(
    train_50,
    trained_model %>% predict(train_50)
  )
train_set_with_predictions

PN <- count(train_set_with_predictions %>% filter(.pred_class == 0))
PP <- count(train_set_with_predictions %>% filter(.pred_class == 1))
RN <- count(train_set_with_predictions %>% filter(OFFER_STATUS == 0))
RP <- count(train_set_with_predictions %>% filter(OFFER_STATUS == 1))

TP <- count(train_set_with_predictions %>% filter(.pred_class == 1 & OFFER_STATUS == 1))
TN <- count(train_set_with_predictions %>% filter(.pred_class == 0 & OFFER_STATUS == 0))
FP <- count(train_set_with_predictions %>% filter(.pred_class == 1 & OFFER_STATUS == 0))
FN <- count(train_set_with_predictions %>% filter(.pred_class == 0 & OFFER_STATUS == 1))

actual_negative <- c(FP, TN)
actual_positive <- c(TP, FN)

rows <- c("pred. positive", "pred. negative")
pred_table <- data.frame(actual_neg = c(FP, TN), actual_positive = c(TP, FN))
pred_table

sensitivity <- TP / (TP + FN)
specificity <- TN / (FP + TN)
bac <- (sensitivity + specificity) / 2

print(paste0("Sensitivity: ", sensitivity))
print(paste0("Specificity: ", specificity))
print(paste0("BAC: ", bac))

##########submission file##########

preds <- trained_model %>% predict(test)
preds
preds %>% count(.pred_class==1)

submission <- bind_cols(
  test %>% select(TEST_SET_ID),
  trained_model %>% predict(test)) %>%
  rename(id = TEST_SET_ID,
         prediction = .pred_class) %>%
  arrange(id)

write_csv(submission, "predictions_married_rock_8.csv")
