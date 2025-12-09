#install.packages("summarytools")

# data preprocessing
library(tidyverse)
library(lubridate)
# data exploration
library(summarytools) # for user-friendly html summaries of data
library(ggmap) # for plotting data on a map
# for meta-ml
library(tidymodels)
library(dplyr)
library(randomForest)
library(ROSE)


options(dplyr.width = Inf)
theme_set(theme_minimal())
set.seed(2022)

#############################################################
######## deal with classification and customer table ########
#############################################################
# prepare classification table
classification <- read_csv("classification.csv",
                           col_types = cols(Reseller = col_character(),
                                            Test_set_id = col_character()))
classification <- classification %>% mutate(Reseller = factor(Reseller,
                                                              labels = c("0","1")))
summary(classification)

# prepare customer table
customer <- read_csv("customers.csv", 
                     col_types = cols(Customer_ID = col_character()))
customer <- customer %>% mutate(Type = factor(Type)) %>% 
  group_by(Customer_ID)
summary(customer)

######################################################################
######## deal with sales_orders_header and sales_orders table ########
######################################################################
# prepare sales_orders_header table
sales_orders_header <- read_csv("sales_orders_header.csv",
                                col_types = cols(Sales_Order = col_character()))
summary(sales_orders_header)

sales_orders_header$Creation_Date <- as.Date(sales_orders_header$Creation_Date)
sales_orders_header$Release_Date <- as.Date(sales_orders_header$Release_Date)
sales_orders_header[is.na(sales_orders_header)] = as.Date("2022-02-01")
date_interval = interval(sales_orders_header$Creation_Date,sales_orders_header$Release_Date)

sales_orders_header <- sales_orders_header %>% mutate(time_interval = time_length(date_interval,unit = 'day'))
sales_orders_header <- sales_orders_header %>% mutate(Sales_Organization = factor(Sales_Organization),
                                                      Document_Type = factor(Document_Type),
                                                      Delivery = factor(Delivery))
summary(sales_orders_header)

# prepare sales_orders table
sales_orders <- read_csv("sales_orders.csv",
                           col_types = cols(Material_Class = col_character(),
                                            Cost_Center = col_character(),
                                            Item_Position = col_character()))
summary(sales_orders)
sales_orders_prepared <- sales_orders %>% group_by(Sales_Order) %>% 
  summarize(item_count = n(), avg_item = mean(Num_Items), sum_item = sum(Num_Items), 
            avg_value = mean(Net_Value), sum_value = sum(Net_Value))

summary(sales_orders_prepared)

# inner join sales_order_header and sales_order
orders = inner_join(sales_orders_prepared,sales_orders_header,by = c("Sales_Order" = "Sales_Order"))
summary(orders)

################################################################################
#################         merge orders with customers         ##################
################################################################################
customer_order <- inner_join(customer,orders, by = c("Sales_Order" = "Sales_Order"))
summary(customer_order)

################################################################################
#################          use aggregate functions        ######################
################################################################################

# get number of orders
order_count <- customer_order %>% group_by(Customer_ID) %>% 
  summarise(order_count = n())
summary(order_count)

# get number of different types of orders
type_num <- customer_order %>% select(Customer_ID,Type) %>% group_by(Customer_ID,Type) %>% summarise(type_count = n())
type_num <- pivot_wider(type_num, names_from = Type, values_from = type_count)
type_num[is.na(type_num)] = 0
summary(type_num)

# get the sum and mean of items ordered by a customer
item <- customer_order %>% group_by(Customer_ID) %>% 
  summarise(avg_item = mean(avg_item), sum_item = mean(sum_item), sum_kinds = sum(item_count),
            avg_net_value = mean(Net_Value),sum_net_value = sum(Net_Value),interval = mean(time_interval))
summary(item)

# get number of different sales organizations of orders
org_count <- customer_order %>% select(Customer_ID,Sales_Organization) %>% 
  group_by(Customer_ID,Sales_Organization) %>% summarise(org_count = n()) %>% 
  pivot_wider(names_from = Sales_Organization, values_from = org_count)
org_count[is.na(org_count)] = 0
summary(org_count)

# get number of different document types of orders
document_type <- customer_order %>% select(Customer_ID,Document_Type) %>% 
  group_by(Customer_ID,Document_Type) %>% summarise(document_type = n()) %>% 
  pivot_wider(names_from = Document_Type, values_from = document_type)
document_type[is.na(document_type)] = 0
document_type <- document_type %>% rename(Credit_memo = `Credit memo`, Order_charge = `Order w/o charge`)

summary(document_type)

# get number of different delivery types of orders
delivery_type <- customer_order %>% select(Customer_ID,Delivery) %>% 
  group_by(Customer_ID,Delivery) %>% summarise(delivery_type = n()) %>% 
  pivot_wider(names_from = Delivery, values_from = delivery_type)
delivery_type[is.na(delivery_type)] = 0
delivery_type <- delivery_type %>% rename(Not_relevant = `Not relevant`, Completely_processed = `Completely processed`,
                                          Not_yet_processed = `Not yet processed`, Partially_processed = `Partially processed`)
summary(delivery_type)

################################################################################
############          merge tables to get final dataframe        ###############
################################################################################
df <- inner_join(order_count, type_num, by = c("Customer_ID" = "Customer_ID")) %>%
  inner_join(item, by = c("Customer_ID" = "Customer_ID")) %>% 
  inner_join(org_count, by = c("Customer_ID" = "Customer_ID")) %>%
  inner_join(document_type, by = c("Customer_ID" = "Customer_ID")) %>%
  inner_join(delivery_type, by = c("Customer_ID" = "Customer_ID")) %>%
  right_join(classification, by = c("Customer_ID" = "Customer_ID"))
summary(df)


################################################################################
#prepare business_units table
business_units <- read_csv("business_units.csv",
                           col_types = cols(YHKOKRS = col_character(),
                                            Cost_Center = col_character()))
summary(business_units)

#prepare service_map table
service_map <- read_csv("service_map.csv",
                        col_types = cols(MATKL_service = col_character()))
################################################################################


################################################################################
############################   Data set definition  ############################
################################################################################

# get labeled data which can be used to train and validate
df_test <- df %>% filter(is.na(Reseller))
summary(df_test)
df <- df %>% filter(!is.na(Reseller)) %>% filter(order_count < 500) %>%select(-Test_set_id)
#df <- df %>% filter(!is.na(Reseller)) %>%select(-Test_set_id)
summary(df)
sample(df,15)





train_neg <- df %>% filter(Reseller == "0")
train_pos <- df %>% filter(Reseller == "1")
n_neg <- as.integer(count(train_pos)*2)

summary(train_pos)
summary(train_neg)
summary(n_neg)

train_neg_fourth <- sample_n(train_neg, n_neg) 

train_50 <- rbind(train_neg_fourth, train_pos)
pos_rate <- count(train_50 %>% filter(Reseller == "1")) / count(train_50)
pos_rate

summary(train_50)
sample_n(train_50, 15)

#summary(test)
#sample_n(test, 15)

train_set <- train_50 %>% select(Customer_ID, order_count, avg_item, sum_item, sum_kinds, avg_net_value, Order,
                                  Contract, Credit_memo, Returns, Reseller)

summary(train_set)







################################################################################
#############################   Model Selection  ###############################
################################################################################

# define a recipe
rec <- recipe(
  #specify predictors, target and data. Necessary to enable code auto-completion
  Reseller ~ ., data = train_set) %>% 
  # tell tidymodels that `id` is an ID and should not be used in any model
  update_role(Customer_ID, new_role = "ID") %>% 
  # turn dates into decimals, e.g. 2019-07-01 becomes 2019.5
  step_mutate_at(where(is.Date), fn=decimal_date) %>% 
  # remove columns with high correlation
  step_corr(all_numeric(), threshold = 0.7) %>%
  
  step_impute_mean(all_numeric_predictors()) %>% 
  #Centrality
  step_center(all_numeric()) %>%
  # remove constant columns
  step_zv(all_predictors())

#rec <- prep(rec, training = train_set)
#rec
#processed_train_set <- bake(rec,train_set)
#rec %>% prep() %>% bake(new_data=val_set)
#sample_n(processed_train_set, 18)
#sample_n(processed_val_set, 18)

folds <- train_set %>% vfold_cv(v=5, strata = Reseller)

#model <- randomForest(train_set$Reseller~.,data=train_set,
#                      mtry=5,importance=T,proximity=T,ntree=1000)
#model

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
  fit(data=train_set)
trained_model

threshold = c()

train_set_with_predictions <-
  bind_cols(
    train_set,
    trained_model %>% predict(train_set)
    #trained_model %>% predict(train_set, threshold(0.4, 1))
    #as.factor(ifelse(predict(trained_model, train_set, type="response")>0.4,"Y","N"))
    #as.factor(ifelse(predict(trained_model, train_set)>0.4,"Y","N"))
    #predict(trained_model, train_set, threshold(0.4))
  )
train_set_with_predictions

PN <- count(train_set_with_predictions %>% filter(.pred_class == "0"))
PP <- count(train_set_with_predictions %>% filter(.pred_class == "1"))
RN <- count(train_set_with_predictions %>% filter(Reseller == "0"))
RP <- count(train_set_with_predictions %>% filter(Reseller == "1"))

TP <- count(train_set_with_predictions %>% filter(.pred_class == "Y" & Reseller == "1"))
TP
TN <- count(train_set_with_predictions %>% filter(.pred_class == "N" & Reseller == "0"))
TN
FP <- count(train_set_with_predictions %>% filter(.pred_class == "Y" & Reseller == "0"))
FP
FN <- count(train_set_with_predictions %>% filter(.pred_class == "N" & Reseller == "1"))
FN

actual_negative <- c(FP, TN)
actual_positive <- c(TP, FN)

rows <- c("pred. positive", "pred. negative")
pred_table <- data.frame(actual_neg = c(FP, TN), actual_positive = c(TP, FN))
pred_table

sensitivity <- TP / (TP + FN)
sensitivity
specificity <- TN / (FP + TN)
specificity
bac <- (sensitivity + specificity) / 2
bac

print(paste0("Sensitivity: ", sensitivity))
print(paste0("Specificity: ", specificity))
print(paste0("BAC: ", bac))

trained_model %>% extract_fit_parsnip() %>% vip::vip()

##########validation test##########
preds <- trained_model %>% predict(val_set)
preds
val_preds <- preds %>% bind_cols(val_set)
val_preds

PN <- count(val_preds %>% filter(.pred_class == "0"))
PP <- count(val_preds %>% filter(.pred_class == "1"))
RN <- count(val_preds %>% filter(Reseller == "0"))
RP <- count(val_preds %>% filter(Reseller == "1"))

TP <- count(val_preds %>% filter(.pred_class == "1" & Reseller == "1"))
TP
TN <- count(val_preds %>% filter(.pred_class == "0" & Reseller == "0"))
TN
FP <- count(val_preds %>% filter(.pred_class == "1" & Reseller == "0"))
FP
FN <- count(val_preds %>% filter(.pred_class == "0" & Reseller == "1"))
FN
sensitivity <- TP / (TP + FN)
sensitivity
specificity <- TN / (FP + TN)
specificity
bac <- (sensitivity + specificity) / 2
bac

##########submission file##########

preds <- trained_model %>% predict(df_test)
preds
preds %>% count(.pred_class==1)

submission <- bind_cols(
  df_test %>% select(Test_set_id),
  trained_model %>% predict(df_test)) %>%
  rename(id = Test_set_id, prediction = .pred_class) %>%
  arrange(id)

#test_dataset <- read_csv("pub_K8PzhiD.csv")

#test_dataset <- test_dataset %>% mutate(id = as.character(id))

#?mutate

#inner_join(test_dataset, submission, by = c("id" = "id"))




#submission[submission == "N"] <- "0"  
#submission[submission == "Y"] <- "1"  

write_csv(submission, "predictions_Hanuta_1.csv")
