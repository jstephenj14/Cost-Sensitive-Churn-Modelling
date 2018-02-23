
overview <- all_events %>%
dplyr::filter(event == "purchase") %>%
filter(price < 250) %>%
dplyr::group_by(cust_id) %>% 
dplyr::summarise(total_bought = sum(price))

summary(overview$total_bought)

train_active  <- all_events %>%
filter(timestamp < '2014-08-01' &  
timestamp >= '2014-04-01' & 
event == "purchase") %>%
group_by(cust_id) %>% 
summarise(total_bought = sum(price)) %>% 
filter(total_bought >= 35) 

train <- train_active %>% left_join(all_events %>% 
filter(timestamp >= "2014-08-01" &
timestamp < "2014-12-01" &
event == "purchase") %>% 
select(cust_id) %>% 
unique() %>%
mutate(still_there = 1),by=c("cust_id")) %>%
mutate(target = ifelse(is.na(still_there) == TRUE,0,1)) 

long_term <- 
  all_events %>% 
  filter(timestamp <  '2014-08-01' &
           timestamp >= '2013-12-01') %>%
  group_by(cust_id) %>% 
  summarise(
    products_viewed =  sum(!is.na(product_id)),
    distinct_products = n_distinct(product_id),
    distinct_prod_types= n_distinct(prod_type),
    distinct_merchants = n_distinct(merchant_id),
    amount_bought = sum(price[event == "purchase"],na.rm=TRUE),
    products_bought = sum(event == "purchase",na.rm=TRUE)) %>% 
  ungroup()

names(long_term) = map_chr(names(long_term), function(x) paste(x,"LT",sep ="_"))

short_term <- all_events %>% 
  filter(timestamp <  '2014-08-01' &
           timestamp >= '2014-07-01') %>%
  group_by(cust_id) %>% 
  summarise(
    products_viewed =  sum(!is.na(product_id)),
    distinct_products = n_distinct(product_id),
    distinct_prod_types= n_distinct(prod_type),
    distinct_merchants = n_distinct(merchant_id),
    amount_bought = sum(price[event == "purchase"],na.rm=TRUE),
    products_bought = sum(event == "purchase",na.rm=TRUE)) %>% 
  ungroup()

names(short_term) = map_chr(names(short_term), function(x) paste(x,"ST",sep ="_"))

lifetime <- all_events %>%
filter(timestamp < '2014-08-01') %>%
group_by(cust_id) %>% 
summarise(amount_bought = sum(price[event == "purchase"],na.rm=TRUE),
products_bought = sum(event == "purchase",na.rm=TRUE),
active_time = difftime('2014-08-01',min(timestamp))) 

names(lifetime) = map_chr(names(lifetime), function(x) paste(x,"LFTIME",sep ="_"))

train <- train %>% left_join(long_term,by=c("cust_id"="cust_id_LT")) %>%
left_join(short_term,by=c("cust_id"="cust_id_ST")) %>%
left_join(lifetime,by=c("cust_id"="cust_id_LFTIME")) 

train["active_time_LFTIME"] = as.numeric(train$active_time_LFTIME)
train[is.na(train)] <- 0

final_train <- train %>% mutate(
ratio_products_viewed = products_viewed_ST/products_viewed_LT,
ratio_distinct_products = distinct_products_ST/distinct_products_LT,
ratio_distinct_prod_types = distinct_products_ST/distinct_products_LT,
ratio_distinct_merchants = distinct_merchants_ST/distinct_merchants_LT,
ratio_amount_bought = amount_bought_ST/amount_bought_LT,
ratio_products_bought=products_bought_ST/products_bought_LT,
amount_per_time = amount_bought_LFTIME/active_time_LFTIME,
product_per_time  = products_bought_LFTIME/active_time_LFTIME)

test_active  <- all_events %>%
dplyr::filter(timestamp < '2014-12-01' &
timestamp >= '2014-08-01' &
event == "purchase") %>%
dplyr::group_by(cust_id) %>% 
dplyr::summarise(total_bought = sum(price,na.rm=TRUE)) %>% 
dplyr::filter(total_bought >= 30) 

test <- test_active %>% 
left_join(all_events %>% 
filter(  timestamp >= "2014-12-01" &
timestamp < "2015-03-01" &
event == "purchase") %>% 
select(cust_id) %>% 
unique() %>%
mutate(still_there = 1),by=c("cust_id")) %>%
mutate(target = ifelse(is.na(still_there) == TRUE,0,1)) 

long_term_test <- 
all_events %>% 
filter(timestamp <  '2014-12-01' &
timestamp >= '2014-04-01') %>%
group_by(cust_id) %>% 
summarise(
products_viewed =  sum(!is.na(product_id)),
distinct_products = n_distinct(product_id),
distinct_prod_types= n_distinct(prod_type),
distinct_merchants = n_distinct(merchant_id),
amount_bought = sum(price[event == "purchase"],na.rm=TRUE),
products_bought = sum(event == "purchase",na.rm=TRUE)) %>% 
ungroup()

names(long_term_test) = map_chr(names(long_term_test), function(x) paste(x,"LT",sep ="_"))

short_term_test <- all_events %>% 
filter(timestamp <  '2014-12-01' &
timestamp >= '2014-11-01') %>%
group_by(cust_id) %>% 
summarise(
products_viewed =  sum(!is.na(product_id)),
distinct_products = n_distinct(product_id),
distinct_prod_types= n_distinct(prod_type),
distinct_merchants = n_distinct(merchant_id),
amount_bought = sum(price[event == "purchase"],na.rm=TRUE),
products_bought = sum(event == "purchase",na.rm=TRUE)) %>% 
ungroup()

names(short_term_test) = map_chr(names(short_term_test), function(x) paste(x,"ST",sep ="_"))

lifetime_test <- all_events %>%
filter(timestamp < '2014-12-01') %>%
group_by(cust_id) %>% 
summarise(amount_bought = sum(price[event=="purchase"],na.rm=TRUE),
products_bought = sum(event == "purchase",na.rm=TRUE),
active_time = difftime('2014-12-01',min(timestamp))) 

names(lifetime_test) = map_chr(names(lifetime_test), function(x) paste(x,"LFTIME",sep ="_"))

final_test <- test %>% 
left_join(long_term_test,by=c("cust_id"="cust_id_LT")) %>%
left_join(short_term_test,by=c("cust_id"="cust_id_ST")) %>%
left_join(lifetime_test,by=c("cust_id"="cust_id_LFTIME")) 

final_test["active_time_LFTIME"] = as.numeric(final_test$active_time_LFTIME)
final_test[is.na(final_test)] <- 0

final_test <- final_test %>% mutate(
ratio_products_viewed = products_viewed_ST/products_viewed_LT,
ratio_distinct_products = distinct_products_ST/distinct_products_LT,
ratio_distinct_prod_types = distinct_products_ST/distinct_products_LT,
ratio_distinct_merchants = distinct_merchants_ST/distinct_merchants_LT,
ratio_amount_bought = amount_bought_ST/amount_bought_LT,
ratio_products_bought=products_bought_ST/products_bought_LT,
amount_per_time = amount_bought_LFTIME/active_time_LFTIME,
product_per_time  = products_bought_LFTIME/active_time_LFTIME)

valuable_customers <- all_events %>% 
dplyr::filter(event == "purchase") %>%
dplyr::group_by(cust_id) %>% 
dplyr::summarise(total_bought = sum(price),count = n()) %>% 
dplyr::filter(total_bought >= 35) %>% select(cust_id)

purchases_only <- all_events %>% 
filter(event == "purchase") %>% 
filter(price < 250) %>%
filter(cust_id %in% valuable_customers$cust_id)

distinct_userids_n <- length(unique(purchases_only$cust_id))

average_order_value <- sum(purchases_only$price,na.rm=TRUE)/nrow(purchases_only)
purchase_freqeuncy <- nrow(purchases_only)/distinct_userids_n
customer_value <- average_order_value*purchase_freqeuncy

days_of_observation <- difftime(max(purchases_only$timestamp,na.rm=TRUE),
min(purchases_only$timestamp,na.rm=TRUE)) %>% as.numeric()

LV_Data <- all_events %>% 
filter(cust_id %in% valuable_customers$cust_id) %>%
group_by(cust_id) %>%
summarise(max= max(timestamp,na.rm = TRUE),
min = min(timestamp,na.rm = TRUE),
lifetime = difftime(max(timestamp,na.rm = TRUE),
min(timestamp,na.rm = TRUE)),
no_of_appearances=n()) %>% 
ungroup()

average_lifetime <- mean(LV_Data$lifetime,na.rm=TRUE) %>% as.numeric()
customer_lifetime_value <- average_lifetime*customer_value/(days_of_observation)
customer_lifetime_value

final_train <- as.data.frame(lapply(final_train,as.numeric))
final_test <- as.data.frame(lapply(final_test,as.numeric))

final_train1_manual <- final_train[c("target","products_viewed_ST","distinct_products_ST","distinct_prod_types_ST","distinct_merchants_ST","amount_bought_ST","products_bought_ST","ratio_products_viewed","ratio_distinct_products","ratio_distinct_prod_types","ratio_distinct_merchants","ratio_amount_bought","ratio_products_bought","amount_per_time","product_per_time","amount_bought_LFTIME","products_bought_LFTIME","active_time_LFTIME")]

final_test1_manual <- final_test[c("target","products_viewed_ST","distinct_products_ST", "distinct_prod_types_ST","distinct_merchants_ST","amount_bought_ST","products_bought_ST", "ratio_products_viewed","ratio_distinct_products","ratio_distinct_prod_types","ratio_distinct_merchants","ratio_amount_bought","ratio_products_bought","amount_per_time","product_per_time","amount_bought_LFTIME","products_bought_LFTIME","active_time_LFTIME")]


final_train1_manual["target"] =  as.factor(final_train1_manual$target)
final_test1_manual["target"] =  as.factor(final_test1_manual$target)

train_task <- makeClassifTask(data = final_train1_manual, target = "target")
test_task <- makeClassifTask(data = final_test1_manual, target = "target")

costs = matrix(c(0, 29,12,-17),2)
colnames(costs) = rownames(costs) = getTaskClassLevels(train_task)

target.costs = makeCostMeasure(id = "target.costs", 
name = "Target costs", 
costs = costs,
best = -17, 
worst = 29)

xgb_lrn <- makeLearner(cl = "classif.xgboost", predict.type = "prob")

#Grid search parameters
params <- makeParamSet(
makeIntegerParam("max_depth",lower = 3L,upper = 5L),
makeIntegerParam("min_child_weight",lower = 1L,upper = 2L),
makeDiscreteParam("subsample",values = seq(0.5,1,0.1)),
makeDiscreteParam("colsample_bytree",values = seq(0.5,1,0.1)),
makeDiscreteParam("eta", values = seq(0.01,0.1,0.01)),
makeDiscreteParam("nrounds", values = seq(100L, 300L, by = 100L)))

rdesc <- makeResampleDesc("CV", iters = 3)
ctrl <- makeTuneControlRandom(maxit = 25L)

set.seed(42)

invisible(mytune <- tuneParams(learner = xgb_lrn,
task = train_task,
resampling = rdesc,
measures = target.costs,
par.set = params,
control = ctrl,
show.info = FALSE))

lrn_tuned <- setHyperPars(xgb_lrn,par.vals = mytune$x)

invisible(xgmodel <- train(learner = lrn_tuned,
task = train_task))

xgpred <- predict(xgmodel,test_task)

pred <- prediction(xgpred$data$prob.1,xgpred$data$truth)
perf <- performance(pred,"tpr","fpr")

plot(perf,col="black",lty=3, lwd=3)

auc <- performance(pred,"auc")
auc <- unlist(slot(auc, "y.values"))
auc

mlr::performance(xgpred, measures = list(acc,target.costs))

getConfMatrix(xgpred)

d = generateThreshVsPerfData(xgpred, measures = list(target.costs, acc))
plotThreshVsPerf(d)

xgpred = setThreshold(xgpred, c(s = 0.7))

mlr::performance(xgpred, measures = list(acc,target.costs))
getConfMatrix(xgpred)

