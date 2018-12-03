set.seed(1)

f15 = read.csv("flights2015.csv")
f16 = read.csv("flights2016.csv")
f17 = read.csv("flights2017.csv")
f17.guess = read.csv("flights2017_guess.csv")
f17.visible = read.csv("flights2017_visible.csv")

to.rm = "DEP_TIME, DEP_DELAY, DEP_DELAY_NEW, DEP_DELAY_GROUP, DEP_TIME_BLK, TAXI_OUT, WHEELS_OFF, WHEELS_ON, TAXI_IN, ARR_TIME, ARR_DELAY, ARR_DELAY_NEW, ARR_DEL15, ARR_DELAY_GROUP, ARR_TIME_BLK, CANCELLED, CANCELLATION_CODE, DIVERTED, ACTUAL_ELAPSED_TIME, AIR_TIME, CARRIER_DELAY, WEATHER_DELAY, NAS_DELAY, SECURITY_DELAY, LATE_AIRCRAFT_DELAY, FIRST_DEP_TIME, TOTAL_ADD_GTIME, LONGEST_ADD_GTIME"
to.rm = strsplit(to.rm, ", ")[[1]]
to.rm = !(colnames(f15) %in% to.rm)
f15.temp = f15[,to.rm]

train.ind = sample(1:nrow(f15), 0.7*nrow(f15))
f15.train = f15.temp[train.ind,]
f15.test = f15.temp[-train.ind,]


library(randomForest)



