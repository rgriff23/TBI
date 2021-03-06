########################################################################################################################
# PREPARATIONS
########################################################################################################################

# Read data into R
data <- read.csv("~/Desktop/TBI/PietrobonTBIRegistry_DATA_2015-09-17_1418.csv", header=TRUE, stringsAsFactors=FALSE)

########################################################################################################################
# CHECK TIME DATA
########################################################################################################################

# How many total rows of data?
nrow(data)	#1986

# How many non-missing entries for time variables?
sum(data$time_arrival != "")	#1961 (98.7%)
sum(data$cdmd_arrival != "")	#1959 (98.6%)
sum(data$labs_time != "")		#1677 (84.4%)
sum(data$skullxr_time != "")	#1512 (76.1%)
sum(data$fluids_time != "")		#693 (34.9%)
sum(data$time_tbisurg != "")	#417 (21%)
sum(data$time_surgtoicu != "")	#246 (12.4%)
sum(data$time_death != "")		#172 (8.7%)
sum(data$oxygen_time != "")		#79	(4%)
sum(data$mannitol_time != "")	#34 (1.7%)
sum(data$ctbrain_time != "")	#19 (1%)
sum(data$ccollar_time != "")	#16 (0.9%)

# Convert time variables to class POSIXct (fully de-identify data by uniformly fixing the date of arrival at 1/1/15)
data$time_arrival <- as.POSIXct(strptime(paste("01/01/2015", data$time_arrival), "%m/%d/%Y %H:%M"))
data$cdmd_arrival <- as.POSIXct(strptime(paste("01/01/2015", data$cdmd_arrival), "%m/%d/%Y %H:%M"))
data$oxygen_time <- as.POSIXct(strptime(paste("01/01/2015", data$oxygen_time), "%m/%d/%Y %H:%M"))
data$fluids_time <- as.POSIXct(strptime(paste("01/01/2015", data$fluids_time), "%m/%d/%Y %H:%M"))
data$labs_time <- as.POSIXct(strptime(paste("01/01/2015", data$labs_time), "%m/%d/%Y %H:%M"))
data$skullxr_time <- as.POSIXct(strptime(paste("01/01/2015", data$skullxr_time), "%m/%d/%Y %H:%M"))
data$ctbrain_time <- as.POSIXct(strptime(paste("01/01/2015", data$ctbrain_time), "%m/%d/%Y %H:%M"))
data$mannitol_time <- as.POSIXct(strptime(paste("01/01/2015", data$mannitol_time), "%m/%d/%Y %H:%M"))
data$ccollar_time <- as.POSIXct(strptime(paste("01/01/2015", data$ccollar_time), "%m/%d/%Y %H:%M"))
data$time_tbisurg <- as.POSIXct(strptime(paste("01/01/2015", data$time_tbisurg), "%m/%d/%Y %H:%M"))
data$time_surgtoicu <- as.POSIXct(strptime(paste("01/01/2015", data$time_surgtoicu), "%m/%d/%Y %H:%M"))
data$time_death <- as.POSIXct(strptime(paste("01/01/2015", data$time_death), "%m/%d/%Y %H:%M"))

# Check for times that don't make sense
length(which((data$cdmd_arrival-data$time_arrival)<0))	# 19 cdmd arrivals 'before' patient arrival
data[which((data$cdmd_arrival-data$time_arrival)<0),c("study_id", "time_arrival", "cdmd_arrival")]
length(which((data$oxygen_time-data$time_arrival)<0))	# 3 oxygen 'before' patient arrival
data[which((data$oxygen_time-data$time_arrival)<0),c("study_id", "time_arrival", "oxygen_time")]
length(which((data$fluids_time-data$time_arrival)<0))	# 42 fluids 'before' patient arrival
data[which((data$fluids_time-data$time_arrival)<0),c("study_id", "time_arrival", "fluids_time")]
length(which((data$labs_time-data$time_arrival)<0))	# 202 labs 'before' patient arrival
data[which((data$labs_time-data$time_arrival)<0),c("study_id", "time_arrival", "labs_time")]
length(which((data$skullxr_time-data$time_arrival)<0))	# 175 x-rays 'before' patient arrival
data[which((data$skullxr_time-data$time_arrival)<0),c("study_id", "time_arrival", "skullxr_time")]
length(which((data$ctbrain_time-data$time_arrival)<0))	# 4 ct scans 'before' patient arrival
data[which((data$ctbrain_time-data$time_arrival)<0),c("study_id", "time_arrival", "date_arrival", "ctbrain_time", "ctbrain_day")]
length(which((data$mannitol_time-data$time_arrival)<0))	# 3 mannitol 'before' patient arrival
data[which((data$mannitol_time-data$time_arrival)<0),c("study_id", "time_arrival", "mannitol_time")]
length(which((data$ccollar_time-data$time_arrival)<0))	# 0 collar 'before' patient arrival

# For ctbrain/surgery/icu/death, the date may be different (later) than the date of patient arrival
# Check for dates that don't make sense
length(which(difftime(strptime(data$date_tbisurg, "%m/%d/%Y"), strptime(data$date_arrival, "%m/%d/%Y"), units="days")<0)) # 51 surgeries 'before' patient arrival
data[which(difftime(strptime(data$date_tbisurg, "%m/%d/%Y"), strptime(data$date_arrival, "%m/%d/%Y"), units="days")<0),c("study_id", "date_arrival", "date_tbisurg")]
length(which(difftime(strptime(data$ctbrain_day, "%m/%d/%Y"), strptime(data$date_arrival, "%m/%d/%Y"), units="days")<0)) # 0 ct scans 'before' patient arrival
length(which(difftime(strptime(data$date_surgtoicu, "%m/%d/%Y"), strptime(data$date_arrival, "%m/%d/%Y"), units="days")<0)) # 22 icu 'before' patient arrival
data[which(difftime(strptime(data$date_surgtoicu, "%m/%d/%Y"), strptime(data$date_arrival, "%m/%d/%Y"), units="days")<0),c("study_id", "date_arrival", "date_surgtoicu")]
length(which(difftime(strptime(data$date_surgtoicu, "%m/%d/%Y"), strptime(data$date_tbisurg, "%m/%d/%Y"), units="days")<0)) # 22 icu 'before' surgery
data[which(difftime(strptime(data$date_surgtoicu, "%m/%d/%Y"), strptime(data$date_tbisurg, "%m/%d/%Y"), units="days")<0),c("study_id", "date_surgtoicu", "date_tbisurg")]
length(which(difftime(strptime(data$date_death, "%m/%d/%Y"), strptime(data$date_arrival, "%m/%d/%Y"), units="days")<0)) # 5 death 'before' patient arrival
data[which(difftime(strptime(data$date_death, "%m/%d/%Y"), strptime(data$date_arrival, "%m/%d/%Y"), units="days")<0),c("study_id", "date_death", "date_arrival")]

########################################################################################################################
# FIX TIME DATA
########################################################################################################################

# Check for patient care times that appear to occur 'before' patient arrival times
# Make patient care dates 01/02/15 in cases where that would lead to a time difference
# of <10 hours and remove suspect rows. Also remove large outliers.
data[which((data$cdmd_arrival-data$time_arrival)<0),c("study_id", "time_arrival", "cdmd_arrival")]
cdmd_keep <- c(213, 941, 981, 1021, 1088, 1096, 1478, 1671, 1683, 1689, 1771, 1827, 1922, 1945)
cdmd_drop <- c(25, 131, 703, 913, 1518)
data[cdmd_keep, "cdmd_arrival"] <- as.POSIXct(strptime(paste("01/02/2015", strftime(data[cdmd_keep, "cdmd_arrival"], "%H:%M")), "%m/%d/%Y %H:%M"))
data <- data[-cdmd_drop,]
rownames(data) <- 1:nrow(data)
data[which((data$oxygen_time-data$time_arrival)<0),c("study_id", "time_arrival", "oxygen_time")]
oxygen_drop <- c(856, 1431, 40) # 40 is a positive outlier, took 3 hours to get patient oxygen
data <- data[-oxygen_drop,]
rownames(data) <- 1:nrow(data)
data[which((data$fluids_time-data$time_arrival)<0),c("study_id", "time_arrival", "fluids_time")]
fluids_keep <- c(459, 491, 603, 608, 635, 650, 656, 907, 1015, 1043, 1082, 1098, 1281, 1348, 1352, 1430, 1470, 1471, 1485, 1499, 1587, 1596, 1659, 1681, 1691, 1705, 1718, 1722, 1763, 1819, 1914, 1937, 1953, 1976)
fluids_drop <- c(844, 1169, 1242, 1317, 1357, 1476, 1530, 1715)
data[fluids_keep, "fluids_time"] <- as.POSIXct(strptime(paste("01/02/2015", strftime(data[fluids_keep, "fluids_time"], "%H:%M")), "%m/%d/%Y %H:%M"))
data <- data[-fluids_drop,]
rownames(data) <- 1:nrow(data)
fluids_drop2 <- c(3, 495, 1067, 1348)	# positive outliers (>8 hours)
data <- data[-fluids_drop2,]
rownames(data) <- 1:nrow(data)

########################################################################################################################
# DEFINE NEW WAITING TIME VARIABLES		
#######################################################################################################################

# Define new waiting time variables and add to dataframe
cdmd_wait <- c(data$cdmd_arrival - data$time_arrival) #secs 
oxygen_wait <- c(data$oxygen_time - data$time_arrival)  #mins
fluids_wait <- c(data$fluids_time - data$time_arrival)  #secs
data <- cbind(data, cdmd_wait, oxygen_wait, fluids_wait)

########################################################################################################################
# DEFINE NEW CATEGORICAL VARIABLES FOR gcs_tot, sys_bp, pulse_ox, AND fluids
########################################################################################################################

# Define three broad gcs categories: mild, moderate, and severe
gcs_cat <- rep(0, nrow(data))
gcs_cat[data$gcs_tot < 9] <- "severe"
gcs_cat[data$gcs_tot > 8 & data$gcs_tot < 14] <- "moderate"
gcs_cat[data$gcs_tot > 13] <- "mild"
gcs_cat[gcs_cat=="0"] <- NA
data <- cbind(data, gcs_cat)

# Add gcs factor variable
gcs <- as.factor(data$gcs_tot)
data <- cbind(data, gcs)

# Hypotension = 1
bp_low <- ifelse(data$sys_bp < 90, 1, 0)

# Hypertension = 1
bp_high <- ifelse(data$sys_bp > 159, 1, 0)

# Hypoxic = 1
ox_low <- ifelse(data$pulse_ox < 92, 1, 0)

# Fluids given = 1
fluids0 <- ifelse(data$fluids > 0, 1, 0)

# Add new binary variables to dataframe
data <- cbind(data, bp_low, bp_high, ox_low, fluids0)

########################################################################################################################
# DELETE UNWANTED COLUMNS AND WRITE NEW DATA FILE FOR FINAL ANALYSIS
########################################################################################################################

write.csv(data, "~/Desktop/GitHub/TBI/TBI final data.csv", row.names=FALSE)

########################################################################################################################
########################################################################################################################
# VISUALIZATIONS 
########################################################################################################################
########################################################################################################################

############################################################
### waiting time distributions / predictors of waiting time
############################################################

# Histograms of waiting times (how are waiting times distributed)
quartz()
layout(matrix(1:3, 1, 3))
hist(cdmd_wait/60/60, main="", xlab="Time to MD arrival (hours)", col="gray")
abline(v=median(cdmd_wait/60/60, na.rm=T), col="red") #10mins
mtext("median = 10 minutes", line=1, cex=0.6)
hist(oxygen_wait, main="", xlab="Time to oxygen (minutes)", ylab="", col="gray")
abline(v=median(oxygen_wait, na.rm=T), col="red") #10mins
mtext("median = 10 minutes", line=1, cex=0.6)
hist(fluids_wait/60/60, main="", xlab="Time to fluids (hours)", ylab="", col="gray")
abline(v=median(fluids_wait/60/60, na.rm=T), col="red") #20 mins
mtext("median = 20 minutes", line=1, cex=0.6)

# Histograms of log1p waiting times (how does log transformation affect the wait time variables)
quartz()
layout(matrix(1:3, 1, 3))
hist(log1p(cdmd_wait), main="", xlab="Time to MD arrival (log1p secs)", col="gray")
abline(v=mean(log1p(cdmd_wait), na.rm=T), col="red") #10.8mins
mtext("mean = 10.8 minutes", line=1, cex=0.6)
hist(log1p(oxygen_wait), main="", xlab="Time to oxygen (log1p minutes)", ylab="", col="gray")
abline(v=mean(log1p(oxygen_wait), na.rm=T), col="red") #10.3mins
mtext("mean = 10.3 minutes", line=1, cex=0.6)
hist(log1p(fluids_wait), main="", xlab="Time to fluids (log1p secs)", ylab="", col="gray")
abline(v=mean(log1p(fluids_wait), na.rm=T), col="red") #22 mins
mtext("mean = 22 minutes", line=1, cex=0.6)

# Boxplots of log MD waiting times across different GCS (are sicker patients being seen more quickly?)
quartz()
layout(matrix(1,1,1))
boxplot(log1p(cdmd_wait)~gcs, col="gray", main="Log MD wait times across incoming GCS", ylab="Log1p MD wait time", xlab="Incoming patient GCS")
abline(h=mean(log1p(cdmd_wait), na.rm=T), col="red")

# Boxplots of log MD waiting times across different times of day (does time of day affect how quickly patients are seen?)
quartz()
layout(matrix(1,1,1))
hour <- as.factor(format(data$time_arrival, format='%H'))
boxplot(log1p(cdmd_wait)~hour, col="gray", main="Log MD wait times across hours of the day", ylab="Log1p MD wait time", xlab="Hour of patient arrival")
abline(h=mean(log1p(cdmd_wait), na.rm=T), col="red")
# Linear regression of MD waiting times as a function of time of day
summary(lm(log1p(cdmd_wait)~hour))

# Are hypoxic/hypotensive patients given oxygen/fluids more quickly?
quartz()
layout(matrix(1:2,1,2))
boxplot(log1p(oxygen_wait)~ox_low, data=data, col="skyblue", names=c("Normal ox", "Low ox"), main="Oxygen wait times", ylab="log1p(Oxygen wait time)")
boxplot(log1p(fluids_wait)~bp_low, data=data, col="pink", names=c("Normal/high bp", "Low bp"), main="Fluid wait times", ylab="log1p(Fluids wait time)")
summary(lm(log1p(oxygen_wait)~ox_low)) # not significant
summary(lm(log1p(fluids_wait)~bp_low)) # p < 0.05, low blood pressure leads to faster fluids time
# Does GCS predict waiting times to oxygen/fluids?
summary(lm(log1p(oxygen_wait)~ox_low+gcs_cat)) 
summary(lm(log1p(fluids_wait)~bp_low+gcs_cat))

#########################################
### treatment of hypoxia and hypotension
#########################################

# Distribution of pulse oxygen and blood pressure
quartz()
layout(matrix(1:2,1,2))
hist(data$pulse_ox, col="gray", ylab="Pulse oxygen", xlab="Frequency", main="")
abline(v=92, col="red", xpd=FALSE)
hist(data$sys_bp, col="gray", ylab="Systolic blood pressure", xlab="Frequency", main="")
abline(v=90, col="red", xpd=FALSE)
abline(v=159, col="red", xpd=FALSE)

# Oxygen given to hypoxic vs non-hypoxic patients
ox <- table(data[,c("oxygen", "ox_low")], exclude=NULL)[2:1,1:3]
# Frequencies
quartz()
layout(matrix(1:2,1,2))
barplot(ox, beside=T, names.arg=c("Normal", "Low", "NA"), main="Frequency of patients receiving oxygen", col=c("skyblue", "lightgray"), xlab="Pulse oxygen")
legend("bottomleft", xpd=TRUE, inset=c(-0.2,-0.25),  legend=c("Oxygen", "No oxygen"), fill=c("skyblue","lightgray"), cex=0.75)
# Proportions
err.upper <- c()
err.lower <- c()
for (i in 1:ncol(ox)) {
  test <- prop.test(ox[1,i], colSums(ox)[i])
  err.upper[i] <- test$conf.int[2]
  err.lower[i] <- test$conf.int[1]
}
bplot <- barplot(ox[1,]/colSums(ox), ylim=c(0,1), names.arg=c("Normal", "Low", "NA"), main="Proportion of patients receiving oxygen", col="skyblue", xlab="Pulse oxygen")
arrows(x0=c(bplot), y0=err.lower, x1=c(bplot), y1=err.upper, length=0.07, angle=90, code=3)

# Fluids given to patients with different blood pressures
fl0 <- table(data.frame(data$fluids0, data$bp_low+data$bp_high))[,1]  #normal
fl1 <- table(data[,c("fluids0", "bp_low")])[,2] #low
fl2 <- table(data[,c("fluids0", "bp_high")], exclude=NULL)[1:2,2:3]  #high, NA
fl <- cbind("norm"=fl0, "low"=fl1, "2"=fl2)[2:1,c(2,1,3,4)]
# Frequencies
quartz()
layout(matrix(1:2,1,2))
barplot(fl, beside=T, names.arg=c("Low", "Normal", "High", "NA"), main="Frequency of patients receiving fluids", col=c("pink","lightgray"), xlab="Blood pressure")
legend("bottomleft", xpd=TRUE, inset=c(-0.2,-0.25),  legend=c("Fluids", "No fluids"), fill=c("pink","lightgray"), cex=0.75)
# Proportions
err.upper <- c()
err.lower <- c()
for (i in 1:ncol(fl)) {
  test <- prop.test(fl[1,i], colSums(fl)[i])
  err.upper[i] <- test$conf.int[2]
  err.lower[i] <- test$conf.int[1]
}
bplot <- barplot(fl[1,]/colSums(fl), ylim=c(0,1), names.arg=c("Low", "Normal", "High", "NA"), main="Proportion of patients receiving fluids", col="pink", xlab="Blood pressure")
arrows(x0=c(bplot), y0=err.lower, x1=c(bplot), y1=err.upper, length=0.07, angle=90, code=3)


# Appropriate administration of oxygen at different GCS levels
quartz()
layout(matrix(1:4, 2, 2, byrow=T))
# Proportion hypoxic who get oxygen
hypox_ox <- table(data$gcs_cat[data$ox_low==1 & data$oxygen==1])/table(data$gcs_cat[data$ox_low==1])
hypox_ox.upper <- c()
hypox_ox.lower <- c()
for (i in 1:length(hypox_ox)) {
  test <- prop.test(table(data$gcs_cat[data$ox_low==1 & data$oxygen==1])[i], table(data$gcs_cat[data$ox_low==1])[i])
  hypox_ox.upper[i] <- test$conf.int[2]
  hypox_ox.lower[i] <- test$conf.int[1]
}
# Proportion hypoxic who do NOT get oxygen
hypox_nox <- table(data$gcs_cat[data$ox_low==1 & data$oxygen==0])/table(data$gcs_cat[data$ox_low==1])
hypox_nox.upper <- c()
hypox_nox.lower <- c()
for (i in 1:length(hypox_nox)) {
  test <- prop.test(table(data$gcs_cat[data$ox_low==1 & data$oxygen==0])[i], table(data$gcs_cat[data$ox_low==1])[i])
  hypox_nox.upper[i] <- test$conf.int[2]
  hypox_nox.lower[i] <- test$conf.int[1]
}
# Proportion not hypoxic who get oxygen
nhypox_ox <- table(data$gcs_cat[data$ox_low==0 & data$oxygen==1])/table(data$gcs_cat[data$ox_low==0])
nhypox_ox.upper <- c()
nhypox_ox.lower <- c()
for (i in 1:length(nhypox_ox)) {
  test <- prop.test(table(data$gcs_cat[data$ox_low==0 & data$oxygen==1])[i], table(data$gcs_cat[data$ox_low==0])[i])
  nhypox_ox.upper[i] <- test$conf.int[2]
  nhypox_ox.lower[i] <- test$conf.int[1]
}
# Proportion not hypoxic who don't get oxygen
nhypox_nox <- table(data$gcs_cat[data$ox_low==0 & data$oxygen==0])/table(data$gcs_cat[data$ox_low==0])
nhypox_nox.upper <- c()
nhypox_nox.lower <- c()
for (i in 1:length(nhypox_ox)) {
  test <- prop.test(table(data$gcs_cat[data$ox_low==0 & data$oxygen==0])[i], table(data$gcs_cat[data$ox_low==0])[i])
  nhypox_nox.upper[i] <- test$conf.int[2]
  nhypox_nox.lower[i] <- test$conf.int[1]
}
# Proportion hypotensive who get fluids
hypot_fl <- table(data$gcs_cat[data$bp_low==1 & data$fluids0==1])/table(data$gcs_cat[data$bp_low==1])
hypot_fl.upper <- c()
hypot_fl.lower <- c()
for (i in 1:length(hypot_fl)) {
  test <- prop.test(table(data$gcs_cat[data$bp_low==1 & data$fluids0==1])[i], table(data$gcs_cat[data$bp_low==1])[i])
  hypot_fl.upper[i] <- test$conf.int[2]
  hypot_fl.lower[i] <- test$conf.int[1]
}
# Proportion hypotensive who do NOT get fluids
hypot_nfl <- table(data$gcs_cat[data$bp_low==1 & data$fluids0==0])/table(data$gcs_cat[data$bp_low==1])
hypot_nfl.upper <- c()
hypot_nfl.lower <- c()
for (i in 1:length(hypot_nfl)) {
  test <- prop.test(table(data$gcs_cat[data$bp_low==1 & data$fluids0==0])[i], table(data$gcs_cat[data$bp_low==1])[i])
  hypot_nfl.upper[i] <- test$conf.int[2]
  hypot_nfl.lower[i] <- test$conf.int[1]
}
# Proportion not hypotensive who get fluids
nhypot_fl <- table(data$gcs_cat[data$bp_low==0 & data$fluids0==1])/table(data$gcs_cat[data$bp_low==0])
nhypot_fl.upper <- c()
nhypot_fl.lower <- c()
for (i in 1:length(nhypot_fl)) {
  test <- prop.test(table(data$gcs_cat[data$bp_low==0 & data$fluids0==1])[i], table(data$gcs_cat[data$bp_low==0])[i])
  nhypot_fl.upper[i] <- test$conf.int[2]
  nhypot_fl.lower[i] <- test$conf.int[1]
}
# Proportion not hypotensive who don't get fluids
nhypot_nfl <- table(data$gcs_cat[data$bp_low==0 & data$fluids0==0])/table(data$gcs_cat[data$bp_low==0])
nhypot_nfl.upper <- c()
nhypot_nfl.lower <- c()
for (i in 1:length(nhypot_nfl)) {
  test <- prop.test(table(data$gcs_cat[data$bp_low==0 & data$fluids0==0])[i], table(data$gcs_cat[data$bp_low==0])[i])
  nhypot_nfl.upper[i] <- test$conf.int[2]
  nhypot_nfl.lower[i] <- test$conf.int[1]
}
# Needed treatments given
bplot <- barplot(rbind(hypox_ox, hypot_fl), ylim=c(0,1), beside=T, main="Needed treatment was given", col=c("skyblue", "pink"), ylab="Frequency")
arrows(x0=c(bplot), y0=c(rbind(hypox_ox.lower, hypot_fl.lower)), x1=c(bplot), y1=c(rbind(hypox_ox.upper, hypot_fl.upper)), length=0.07, angle=90, code=3)
# Needed treatments not given
bplot <- barplot(rbind(hypox_nox, hypot_nfl), ylim=c(0,1), beside=T, main="Needed treatment NOT given", col=c("skyblue", "pink"))
arrows(x0=c(bplot), y0=c(rbind(hypox_nox.lower, hypot_nfl.lower)), x1=c(bplot), y1=c(rbind(hypox_nox.upper, hypot_nfl.upper)), length=0.07, angle=90, code=3)
# Unneeded treatments given
bplot <- barplot(rbind(nhypox_ox, nhypot_fl), ylim=c(0,1), beside=T, main="Unneeded treatment was given", col=c("skyblue", "pink"), xlab="TBI category", ylab="Frequency")
arrows(x0=c(bplot), y0=c(rbind(nhypox_ox.lower, nhypot_fl.lower)), x1=c(bplot), y1=c(rbind(nhypox_ox.upper, nhypot_fl.upper)), length=0.07, angle=90, code=3)
legend("bottomleft", xpd=TRUE, inset=c(-0.25,-0.5),  legend=c("Oxygen", "Fluids"), fill=c("skyblue","pink"), cex=0.75)
# Unneeded treatment not given
bplot <- barplot(rbind(nhypox_nox, nhypot_nfl), ylim=c(0,1), beside=T, main="Unneeded treatment NOT given", col=c("skyblue", "pink"), xlab="TBI category")
arrows(x0=c(bplot), y0=c(rbind(nhypox_nox.lower, nhypot_nfl.lower)), x1=c(bplot), y1=c(rbind(nhypox_nox.upper, nhypot_nfl.upper)), length=0.07, angle=90, code=3)

# Logistic regression model predicting patient's receiving oxygen or fluids
summary(glm(oxygen~ox_low+gcs_cat, data=data, family="binomial"))
summary(glm(fluids0~bp_low+gcs_cat, data=data, family="binomial"))
# Models for patients who did not need treatment
summary(glm(oxygen~gcs_cat, data=data[data$ox_low==0,], family="binomial"))
summary(glm(fluids0~gcs_cat, data=data[data$bp_low==0,], family="binomial")) #***
# Models for patients who needed treatment
summary(glm(oxygen~gcs_cat, data=data[data$ox_low==1,], family="binomial"))  #***
summary(glm(fluids0~gcs_cat, data=data[data$bp_low==1,], family="binomial"))

# Logistic regression for patient survival as predicted by abnormal oxygen, blood pressure, and gcs
summary(glm(death~ox_low+bp_low+bp_high+gcs_cat, data=data, family="binomial"))
summary(glm(death~ox_low+bp_low+bp_high+gcs_tot, data=data, family="binomial"))

summary(glm(death~ox_low+bp_low+bp_high+gcs_cat, data=data[round(seq(1, 1966, length.out=1966/2)),], family="binomial"))
summary(glm(death~ox_low+bp_low+bp_high+gcs_tot, data=data[round(seq(1, 1966, length.out=1966/2)),], family="binomial"))
round(seq(1, 1966, length.out=1966/2))

########################
### predictors of death
########################

# Comparison of death rates among different incoming GCS
quartz()
layout(matrix(1, 1, 1))
freq <- table(data[,c("death", "gcs_tot")])
# Frequencies
barplot(freq, beside=TRUE, col=c("skyblue", "darkred"), xlab="GCS", ylab="Frequency", main="Death and Recovery vs. Incoming GCS")
legend("topleft", xpd=TRUE, inset=c(-0.1,-0.2), legend=c("Death", "Recovery"), fill=c("darkred", "skyblue"))
# Proportions
err.upper <- c()
err.lower <- c()
for (i in 1:ncol(freq)) {
  test <- prop.test(freq[2,i], colSums(freq)[i])
  err.upper[i] <- test$conf.int[2]
  err.lower[i] <- test$conf.int[1]
}
quartz()
layout(matrix(1, 1, 1))
bplot <- barplot(freq[2,]/colSums(freq), beside=FALSE, col="darkred", xlab="GCS", ylab="Probability of death", main="Death rate vs. Incoming GCS", ylim=c(0,1))
arrows(x0=c(bplot), y0=err.lower, x1=c(bplot), y1=err.upper, length=0.07, angle=90, code=3)

# Linear regression of MD waiting times as a function of incoming GCS
summary(lm(log1p(cdmd_wait)~as.factor(data$gcs_tot)))
summary(lm(log1p(cdmd_wait)~data$gcs_tot))
summary(lm(log1p(cdmd_wait)~data$gcs_cat))

# Comparison of oxygen and blood pressure levels among patients who lived and died
quartz()
layout(matrix(1:2,1,2))
# Pulse oxygen
boxplot(pulse_ox~death+gcs_cat, data=data, boxwex=0.25, cex=0.5, ylim=c(10,110), at=c(0.1, 0.5, 1.1, 1.5, 2.1, 2.5), col=c("skyblue", "darkred", "skyblue", "darkred", "skyblue", "darkred"), xaxt="n", main="Oxygen levels vs Survival", ylab="Pulse oxygen", xlab="TBI category")
axis(1, at=c(0.3, 1.3, 2.3), labels=c("Mild", "Moderate", "Severe"))
abline(h=90, col="red", xpd=FALSE)
legend("bottomleft", xpd=TRUE, cex=0.6, inset=c(-0.2,-0.25),  legend=c("Death", "Recovery"), fill=c("darkred", "skyblue"))
# Blood pressure
boxplot(sys_bp~death+gcs_cat, data=data, boxwex=0.25, cex=0.5, ylim=c(20,270), at=c(0.1, 0.5, 1.1, 1.5, 2.1, 2.5), col=c("skyblue", "darkred", "skyblue", "darkred", "skyblue", "darkred"), xaxt="n", main="Blood pressure vs Survival", ylab="Systolic blood pressure", xlab="TBI category")
axis(1, at=c(0.3, 1.3, 2.3), labels=c("Mild", "Moderate", "Severe"))
abline(h=159, col="red", xpd=FALSE)
abline(h=90, col="red", xpd=FALSE)

# Does waiting time to MD predict death?
summary(glm(death ~ log1p(cdmd_wait) + gcs, data=data, family="binomial"))
summary(glm(death ~ log1p(cdmd_wait) + gcs_cat, data=data, family="binomial"))
summary(glm(death ~ log1p(cdmd_wait), data=data[data$gcs_cat=="mild",], family="binomial"))
summary(glm(death ~ log1p(cdmd_wait), data=data[data$gcs_cat=="moderate",], family="binomial"))
summary(glm(death ~ log1p(cdmd_wait), data=data[data$gcs_cat=="severe",], family="binomial"))

summary(glm(death ~ log1p(cdmd_wait) + rti, data=data, family="binomial"))
summary(glm(death ~ log1p(cdmd_wait), data=data[data$rti==0,], family="binomial"))
summary(glm(death ~ log1p(cdmd_wait), data=data[data$rti==1,], family="binomial"))
summary(glm(death ~ log1p(cdmd_wait), data=data[data$rti==2,], family="binomial"))

library(lme4)
summary(glmer(death ~ log1p(cdmd_wait) + (1|gcs), data=data, family="binomial"))

########################################################################################################################
########################################################################################################################
# CHECK WHETHER MISSING DATA IS NON-RANDOM WITH RESPECT TO PATIENT DEATH
########################################################################################################################
########################################################################################################################

# How many non-NA entries for variables of interest?
sum(!is.na(data$time_arrival))	#1941
sum(!is.na(data$cdmd_arrival))	#1939
sum(!is.na(data$oxygen_time))	#75
sum(!is.na(data$fluids_time))	#680
sum(!is.na(data$sys_bp))	#1732
sum(!is.na(data$pulse_ox))	#1687
sum(!is.na(data$fluids))	#1954
sum(!is.na(data$oxygen))	#1957
sum(!is.na(data$male))	#1966
sum(!is.na(data$age))	#1945
sum(!is.na(data$gcs_tot))	#1956

# Proportion of non-NA entries for variables of interest?
sum(!is.na(data$time_arrival))/nrow(data)	#0.987
sum(!is.na(data$cdmd_arrival))/nrow(data)	#0.986
sum(!is.na(data$oxygen_time))/nrow(data)	#0.0381
sum(!is.na(data$fluids_time))/nrow(data)	#0.346
sum(!is.na(data$sys_bp))/nrow(data)	#0.881
sum(!is.na(data$pulse_ox))/nrow(data)	#0.858
sum(!is.na(data$fluids))/nrow(data)	#0.994
sum(!is.na(data$oxygen))/nrow(data)	#0.995
sum(!is.na(data$male))/nrow(data)	#1
sum(!is.na(data$age))/nrow(data)	#0.989
sum(!is.na(data$gcs_tot))/nrow(data)	#0.995

########################################################################################################################
########################################################################################################################
# GLMs
########################################################################################################################
########################################################################################################################

# All data
# looks only at whether fluids and oxygen were given (n = 1348)
mod = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + fluids0 + oxygen, data=data, family="binomial")
summary(mod) # gcs, fluids (+), oxygen (+)
# fluids time (n = 512)
mod2 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + log1p(fluids_wait) + oxygen, data=data, family="binomial")
summary(mod2)	# gcs, oxygen
# oxygen time (n = 60)
mod3 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + log1p(oxygen_wait) + fluids0, data=data, family="binomial")
summary(mod3)	# gcs

# Data for gcs 3-8
data2 = data[which(data$gcs_tot<9),]
# looks only at whether fluids and oxygen were given (n = 178)
mod38.1 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + fluids0 + oxygen, data=data2, family="binomial")
summary(mod38.1)	# gcs, oxygen
# fluids time (n = 83)
mod38.2 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + log1p(fluids_wait) + oxygen, data=data2, family="binomial")
summary(mod38.2)	# gcs
# oxygen time (n = 42)
mod38.3 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + log1p(oxygen_wait) + fluids0, data=data2, family="binomial")
summary(mod38.3)

# Data for gcs 9-13
data3 = data[which(data$gcs_tot>8 & data$gcs_tot<14),]
# looks only at whether fluids and oxygen were given (299 df)
mod913.1 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + fluids0 + oxygen, data=data3, family="binomial")
summary(mod913.1)	# gcs, cdmd, fluids, oxygen
# fluids time (n = 123)
#mod913.2 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + log1p(fluids_wait) + oxygen, data=data3, family="binomial")
#summary(mod913.2)	# Oxygen
# oxygen time (n = 8)
#mod914.3 = glm(death ~ gcs_tot + male + age + bp_low + log1p(cdmd_wait) + log1p(oxygen_wait) + fluids0, data=data3, family="binomial")
#summary(mod914.3)

# Data for gcs 14-15
data4 = data[which(data$gcs_tot>13),]
# looks only at whether fluids and oxygen were given ()
mod15.1 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + fluids0 + oxygen, data=data4, family="binomial")
summary(mod15.1)	# age, ox_low
# fluids time ()
mod15.2 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + log1p(fluids_wait) + oxygen, data=data4, family="binomial")
summary(mod15.2)	# NA
#mod15.3 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + log1p(oxygen_wait) + fluids0, data=data4, family="binomial")
#summary(mod15.3)	# NA

# Analysis of only stage 2 and 3 hypertensive patients (68 df)
data.bp = data[which(data$sys_bp>159),]
ox_low = data.bp$pulse_ox
ox_low[which(ox_low < 92)] = 1; ox_low[which(ox_low > 91)] = 0
data.bp = cbind(data.bp, ox_low)
mod.bp = glm(death ~ gcs_tot + sys_bp + ox_low + fluids + oxygen, data=data.bp, family="binomial")
summary(mod.bp)	# gcs, fluids

# Analysis of low blood pressure patients (56 df)
data.bp2 = data[which(data$sys_bp<90),]
ox_low = data.bp2$pulse_ox
ox_low[which(ox_low < 92)] = 1; ox_low[which(ox_low > 91)] = 0
data.bp2 = cbind(data.bp2, ox_low)
mod.bp2 = glm(death ~ gcs_tot + sys_bp + ox_low + fluids + oxygen, data=data.bp2, family="binomial")
summary(mod.bp2)	# gcs


########################################################################################################################
########################################################################################################################
# END 
########################################################################################################################
########################################################################################################################





