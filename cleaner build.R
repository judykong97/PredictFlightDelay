set.seed(1)

f15 = read.csv("flights2015.csv")
f16 = read.csv("flights2016.csv")
full.data = rbind(f15, f16)

to.rm = "DEP_TIME, DEP_DELAY, DEP_DELAY_NEW, DEP_DELAY_GROUP, DEP_TIME_BLK, TAXI_OUT, WHEELS_OFF, WHEELS_ON, TAXI_IN, ARR_TIME, ARR_DELAY, ARR_DELAY_NEW, ARR_DEL15, ARR_DELAY_GROUP, ARR_TIME_BLK, CANCELLED, CANCELLATION_CODE, DIVERTED, ACTUAL_ELAPSED_TIME, AIR_TIME, CARRIER_DELAY, WEATHER_DELAY, NAS_DELAY, SECURITY_DELAY, LATE_AIRCRAFT_DELAY, FIRST_DEP_TIME, TOTAL_ADD_GTIME, LONGEST_ADD_GTIME"
to.rm = strsplit(to.rm, ", ")[[1]]
to.rm = !(colnames(full.data) %in% to.rm)

full.data = full.data[,to.rm]
na.rows = which(rowSums(is.na(full.data)) > 0)
full.data = full.data[-na.rows,]
manual.rm = c("FL_DATE", "UNIQUE_CARRIER", "TAIL_NUM", "AIRLINE_ID", "ORIGIN_AIRPORT_ID", "ORIGIN_AIRPORT_SEQ_ID", "ORIGIN_CITY_MARKET_ID", "ORIGIN_CITY_NAME", "ORIGIN_STATE_ABR", "ORIGIN_STATE_FIPS", "ORIGIN_STATE_NM", "ORIGIN_WAC", "DEST_AIRPORT_ID", "DEST_AIRPORT_SEQ_ID", "DEST_CITY_MARKET_ID", "DEST_CITY_NAME", "DEST_STATE_NM", "DEST_WAC", "FLIGHTS", "DEST_STATE_ABR")
manual.rm = !(colnames(full.data) %in% manual.rm)
full.data = full.data[,manual.rm]

onlyPIT = full.data[full.data$ORIGIN == "PIT",]
onlyPIT[, c("QUARTER", "MONTH", "DAY_OF_MONTH", "DAY_OF_WEEK", "DISTANCE_GROUP", "DEP_DEL15")] = lapply(onlyPIT[,c("QUARTER", "MONTH", "DAY_OF_MONTH", "DAY_OF_WEEK", "DISTANCE_GROUP", "DEP_DEL15")] , factor)


lasso = glm(DEP_DEL15 ~ QUARTER + MONTH + DAY_OF_MONTH + DAY_OF_WEEK + CARRIER + DEST + DISTANCE + CRS_DEP_TIME, family = "binomial", data = onlyPIT)
x = model.matrix(DEP_DEL15 ~ QUARTER + MONTH + DAY_OF_MONTH + DAY_OF_WEEK + CARRIER + DEST + DISTANCE + CRS_DEP_TIME, family = "binomial", data = onlyPIT)
y = onlyPIT$DEP_DEL15
cv_glm = cv.glmnet(x, y, family='binomial')

lasso.rm = names(which(is.na(coef(lasso, s = cv_glm$lambda.1se))))
x = x[,-which(colnames(x) %in% lasso.rm)]


bal.rf = randomForest(x, y, strata = y, sampsize = c(sum(y == 1), sum(y == 1)))

#87% and 13%
table(onlyPIT$DEP_DEL15)/sum(table(onlyPIT$DEP_DEL15))


preds = as.integer(predict(bal.rf, x, type = "prob")[,2] > 0.5)
table(preds == y)
confusionMatrix(as.factor(preds), y)
plot(roc(y, preds))
roc(y, preds)$auc


f17.visible = read.csv("flights2017_visible.csv")
f17.visible[, c("QUARTER", "MONTH", "DAY_OF_MONTH", "DAY_OF_WEEK", "DISTANCE_GROUP")] = lapply(f17.visible[,c("QUARTER", "MONTH", "DAY_OF_MONTH", "DAY_OF_WEEK", "DISTANCE_GROUP")] , factor)
f17.visible = f17.visible[, c("DEP_DEL15", "QUARTER", "MONTH", "DAY_OF_MONTH", "DAY_OF_WEEK", "CARRIER", "ORIGIN", "DEST", "DISTANCE", "CRS_DEP_TIME")]
f17.visible = f17.visible[-which(rowSums(is.na(f17.visible)) > 0), ]

x.vis = model.matrix(DEP_DEL15 ~ QUARTER + MONTH + DAY_OF_MONTH + DAY_OF_WEEK + CARRIER + DEST + DISTANCE + CRS_DEP_TIME, family = "binomial", data = f17.visible)
x.vis = x.vis[,-which(colnames(x.vis) %in% lasso.rm)]
y.vis = as.factor(f17.visible$DEP_DEL15)


add.cols = setdiff(colnames(x), colnames(x.vis))
x.vis = cbind(x.vis, matrix(0, nrow = nrow(x.vis), ncol = length(add.cols), dimnames = list(NULL, add.cols)))

preds.vis = as.integer(predict(bal.rf, x.vis, type = "prob")[,2] > 0.5)
plot(roc(y.vis, preds.vis))
roc(y.vis, preds.vis)$auc

f17.guess = read.csv("flights2017_guess.csv")
x.guess = model.matrix( ~ QUARTER + MONTH + DAY_OF_MONTH + DAY_OF_WEEK + CARRIER + DEST + DISTANCE + CRS_DEP_TIME, family = "binomial", data = f17.guess)
x.guess = x.guess[,-which(colnames(x.guess) %in% lasso.rm)]
add.cols = setdiff(colnames(x), colnames(x.guess))
x.guess = cbind(x.guess, matrix(0, nrow = nrow(x.guess), ncol = length(add.cols), dimnames = list(NULL, add.cols)))
preds.guess = predict(bal.rf, x.guess, type = "prob")[,2]

delay.guesses = as.numeric(preds.guess)
auc.guess = 0.5931
team.name = "Galois"
save(list=c("delay.guesses", "auc.guess", "team.name"),file="stat462final.RData")
