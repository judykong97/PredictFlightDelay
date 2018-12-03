set.seed(1)

f15 = read.csv("flights2015.csv")
f16 = read.csv("flights2016.csv")
full.data = rbind(f15, f16)
f17 = read.csv("flights2017.csv")
f17.guess = read.csv("flights2017_guess.csv")
f17.visible = read.csv("flights2017_visible.csv")

to.rm = "DEP_TIME, DEP_DELAY, DEP_DELAY_NEW, DEP_DELAY_GROUP, DEP_TIME_BLK, TAXI_OUT, WHEELS_OFF, WHEELS_ON, TAXI_IN, ARR_TIME, ARR_DELAY, ARR_DELAY_NEW, ARR_DEL15, ARR_DELAY_GROUP, ARR_TIME_BLK, CANCELLED, CANCELLATION_CODE, DIVERTED, ACTUAL_ELAPSED_TIME, AIR_TIME, CARRIER_DELAY, WEATHER_DELAY, NAS_DELAY, SECURITY_DELAY, LATE_AIRCRAFT_DELAY, FIRST_DEP_TIME, TOTAL_ADD_GTIME, LONGEST_ADD_GTIME"
to.rm = strsplit(to.rm, ", ")[[1]]
to.rm = !(colnames(full.data) %in% to.rm)
full.data = full.data[,to.rm]
na.rows = which(rowSums(is.na(full.data)) > 0)
full.data = full.data[-na.rows,]
manual.rm = c("FL_DATE", "UNIQUE_CARRIER", "TAIL_NUM", "AIRLINE_ID", "ORIGIN_AIRPORT_ID", "ORIGIN_AIRPORT_SEQ_ID", "ORIGIN_CITY_MARKET_ID", "ORIGIN_CITY_NAME", "ORIGIN_STATE_ABR", "ORIGIN_STATE_FIPS", "ORIGIN_STATE_NM", "ORIGIN_WAC", "DEST_AIRPORT_ID", "DEST_AIRPORT_SEQ_ID", "DEST_CITY_MARKET_ID", "DEST_CITY_NAME", "DEST_STATE_NM", "DEST_WAC", "FLIGHTS")
manual.rm = !(colnames(full.data) %in% manual.rm)

full.data = full.data[,manual.rm]

train.ind = sample(1:nrow(full.data), 0.8*nrow(full.data))
train = full.data[train.ind,]
test = full.data[-train.ind,]


save(full.data, train.ind, file = "split.Rdata")


setwd("/Users/eugene/Desktop/github/462/data/")
load("split.Rdata")
library(glmnet)

full.data[, c("QUARTER", "MONTH", "DAY_OF_MONTH", "DAY_OF_WEEK", "DISTANCE_GROUP")] = lapply(full.data[,c("QUARTER", "MONTH", "DAY_OF_MONTH", "DAY_OF_WEEK", "DISTANCE_GROUP")] , factor)

train = full.data[train.ind,]
test = full.data[-train.ind,]

temp = glm(DEP_DEL15 ~ QUARTER + MONTH + DAY_OF_MONTH + DAY_OF_WEEK + CARRIER + ORIGIN + DEST + DISTANCE, family = "binomial", data = train)


x = model.matrix(DEP_DEL15 ~ QUARTER + MONTH + DAY_OF_MONTH + DAY_OF_WEEK + CARRIER + ORIGIN + DEST + DISTANCE, family = "binomial", data = train)
y = train$DEP_DEL15
cv_glm = cv.glmnet(x, y, family='binomial')


#EDA
table(full.data$DAY_OF_WEEK, full.data$DEP_DEL15)/rowSums(table(full.data$DAY_OF_WEEK, full.data$DEP_DEL15))
table(full.data$MONTH, full.data$DEP_DEL15)/rowSums(table(full.data$MONTH, full.data$DEP_DEL15))

# Logistic regression demonstrated no improvement from the base rate

saveRDS(cv_glm, file = "cvglm.RData")


preds = predict(temp, s = cv_glm$lambda.1se, type = "response")
pred = preds$fit > 0.5

roc()

lasso.rm = names(which(is.na(coef(temp, s = cv_glm$lambda.1se))))
x = x[,-which(colnames(x) %in% lasso.rm)]
