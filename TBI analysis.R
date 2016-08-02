
# Read data into R
data <- read.csv("~/Desktop/GitHub/TBI/DATA_Aug1_2016.csv", header=TRUE, stringsAsFactors=FALSE)

library(plyr)

########################################################################################################################
########################################################################################################################
# CLEAN DATA
########################################################################################################################
########################################################################################################################

# Convert time variables to class POSIXct (arbitrarily make date 1/1/15, deidentifies final data, which can go on github)
data$time_arrival <- as.POSIXct(strptime(paste("01/01/2015", data$time_arrival), "%m/%d/%Y %H:%M"))
data$cdmd_arrival <- as.POSIXct(strptime(paste("01/01/2015", data$cdmd_arrival), "%m/%d/%Y %H:%M"))
data$oxygen_time <- as.POSIXct(strptime(paste("01/01/2015", data$oxygen_time), "%m/%d/%Y %H:%M"))
data$fluids_time <- as.POSIXct(strptime(paste("01/01/2015", data$fluids_time), "%m/%d/%Y %H:%M"))

# Drop known problem cases
drop <- c(755, 987, 1441, 1483, 1564)
drop <- which(data$study_id %in% drop)
data <- data[-drop,]
rm(drop)

# Identify cases where treatment time occurs the day after patient arrival, and change the date to 1/2/2015
change <- which((data$cdmd_arrival-data$time_arrival)<0)
data[change,"cdmd_arrival"] <- as.POSIXct(strptime(paste("01/02/2015", strftime(data[change, "cdmd_arrival"], "%H:%M")), "%m/%d/%Y %H:%M"))
change <- which((data$fluids_time-data$time_arrival)<0)
data[change,"fluids_time"] <- as.POSIXct(strptime(paste("01/02/2015", strftime(data[change, "fluids_time"], "%H:%M")), "%m/%d/%Y %H:%M"))
rm(change)

# Drop outliers, which are probably errors (>10 hour wait time for any treatment)
data <- data[-which(difftime(data$cdmd_arrival, data$time_arrival, units="hours")>10),]
data <- data[-which(difftime(data$fluids_time, data$time_arrival, units="hours")>10),]

# Define new waiting time variables and add to dataframe
cdmd_wait <- as.numeric(difftime(data$cdmd_arrival, data$time_arrival, units="secs")) #secs 
oxygen_wait <- as.numeric(difftime(data$oxygen_time, data$time_arrival, units="secs"))  #secs
fluids_wait <- as.numeric(difftime(data$fluids_time, data$time_arrival, units="secs"))  #secs
data <- cbind(data, cdmd_wait, oxygen_wait, fluids_wait)

########################################################################################################################
########################################################################################################################
# DEFINE NEW VARIABLES FOR hypotension, hypoxia, fluids given, GCS category, CTAS level, and SATS level
########################################################################################################################
########################################################################################################################

# Hypotension = 1
bp_low <- ifelse(data$sys_bp < 90, 1, 0)

# Hypoxic = 1
ox_low <- ifelse(data$pulse_ox < 92, 1, 0)

# Fluids given = 1
fluids0 <- ifelse(data$fluids > 0, 1, 0)

# Define GCS categories: mild, moderate, and severe
gcs_cat <- rep(NA, nrow(data))
gcs_cat[data$gcs_tot < 9] <- "severe"
gcs_cat[data$gcs_tot > 8 & data$gcs_tot < 14] <- "moderate"
gcs_cat[data$gcs_tot > 13] <- "mild"
gcs_cat <- factor(gcs_cat, levels=c("moderate", "mild", "severe"))

# Define CTAS levels
CTAS <- rep(NA, nrow(data))
CTAS[data$gcs_tot < 9] <- "1" # < 1 min
CTAS[data$gcs_tot > 8 & data$gcs_tot < 14] <- "2" # < 15 mins
CTAS[data$gcs_tot > 13] <- "3-5" # < 120 mins

# Define binary variables for CTAS_ontime (on time = 1)
CTAS_ontime <- rep(NA, nrow(data))
CTAS_ontime[CTAS=="1" & !is.na(CTAS)] <- ifelse(data[CTAS=="1" & !is.na(CTAS),"cdmd_wait"]<61, 1, 0)
CTAS_ontime[CTAS=="2" & !is.na(CTAS)] <- ifelse(data[CTAS=="2" & !is.na(CTAS),"cdmd_wait"]<901, 1, 0)
CTAS_ontime[CTAS=="3-5" & !is.na(CTAS)] <- ifelse(data[CTAS=="3-5" & !is.na(CTAS),"cdmd_wait"]<7201, 1, 0)

# Define SATS levels

# Add new variables to dataframe
data <- cbind(data, bp_low, ox_low, fluids0, gcs_cat, CTAS, CTAS_ontime)

########################################################################################################################
########################################################################################################################
# DESCRIPTIVE STATISTICS 
########################################################################################################################
########################################################################################################################

# Patient enrollment and sex
nrow(data) # 2216 patients enrolled
table(data$male) # 400 females, 1808 males

# Data completeness
sum(!is.na(data$time_arrival))/nrow(data) # 2189 patient arrival times (98.8%)
sum(!is.na(data$gcs_tot))/nrow(data) # 2202 recorded GCS (99.4%)
sum(!is.na(data$cdmd_arrival))/nrow(data) #2186 physician arrival times (98.6%)
sum(!is.na(data$fluids0))/nrow(data) # 2201 recorded whether fluids given (99.3%)
sum(!is.na(data$fluids_time))/sum(data$fluids0, na.rm=T)	# 776 fluids delivery times (74.9%)
sum(!is.na(data$oxygen))/nrow(data) # 2204 recorded whether oxygen given (99.5%)
sum(!is.na(data$oxygen_time))/sum(data$oxygen, na.rm=T)	# 87 oxygen delivery times	(100%)
sum(!is.na(data$death))/nrow(data)  # 1946 recorded whether patient died (87.8%)

# Proportion of hypotension, hypoxia, GCS mild, GCS moderate, GCS severe, and death
table(data$bp_low, exclude=NULL)[2]/sum(table(data$bp_low)) # 4.1% hypotensive
table(data$ox_low, exclude=NULL)[2]/sum(table(data$ox_low)) # 11.5% hypoxic
table(data$gcs_cat, exclude=NULL)[1]/sum(table(data$gcs_cat)) # 75.8% GCS mild
table(data$gcs_cat, exclude=NULL)[2]/sum(table(data$gcs_cat)) # 12.0% GCS moderate
table(data$gcs_cat, exclude=NULL)[3]/sum(table(data$gcs_cat)) # 12.2% GCS severe
table(data$death, exclude=NULL)[2]/sum(table(data$death)) # 10.3% died

# Proportion of hypotension receiving fluids and hypoxic receiving oxygen
sum(data$bp_low & data$fluids0, na.rm=T)/sum(data$bp_low, na.rm=T) # 75.0% of hypotensive patients got fluids
sum(data$ox_low & data$oxygen, na.rm=T)/sum(data$ox_low, na.rm=T) # 29.2% of hypoxic patients got oxygen

# Proportion of CTAS levels seen by MD on time
sum(data$CTAS=="1" & data$cdmd_wait<=60, na.rm=T)/sum(data$CTAS=="1", na.rm=T) # 3.7% seen in < 1 minute
sum(data$CTAS=="2" & data$cdmd_wait<=900, na.rm=T)/sum(data$CTAS=="2", na.rm=T) # 71.7% seen in < 15 minutes
sum(data$CTAS=="3-5" & data$cdmd_wait<=7200, na.rm=T)/sum(data$CTAS=="3-5", na.rm=T)  # 95.6% seen in < 2 hours

# Median and range of time to oxygen, time to fluids, and time to physician arrival
median(data$cdmd_wait, na.rm=T)/60 # 10 minutes
range(data$cdmd_wait, na.rm=T)/60/60 # min = 0 minutes, max = 10 hours
median(data$fluids_wait, na.rm=T)/60 # 20 minutes
range(data$fluids_wait, na.rm=T)/60/60 # min = 0 minutes, max = 9.93 hours
median(data$oxygen_wait, na.rm=T)/60 # 10 minutes
range(data$oxygen_wait, na.rm=T)/60 # min = 2 minutes, max = 3.13 hours

########################################################################################################################
########################################################################################################################
# ANALYSIS OF TREATMENT 
########################################################################################################################
########################################################################################################################

# Gaussian regression of wait times ~ time of day
hour <- as.factor(format(data$time_arrival, format='%H'))
data <- cbind(data, hour)
lm.timeday <- lm(log1p(cdmd_wait)~hour)
summary(lm.timeday)
lm.timeday2 <- lm(log1p(fluids_wait)~hour)
summary(lm.timeday2)
lm.timeday3 <- lm(log1p(oxygen_wait)~hour)
summary(lm.timeday3)
# Median, min and max wait times for each period
ddply(data, .(hour), function(x) {min(x$cdmd_wait, na.rm=T)/60})
ddply(data, .(hour), function(x) {median(x$cdmd_wait, na.rm=T)/60})
ddply(data, .(hour), function(x) {max(x$cdmd_wait, na.rm=T)/60})


# Add new time variable
peakhours <- ifelse(hour %in% 6:17, 1, 0)
data <- cbind(data, peakhours)

# Logistic regression of fluids administration ~ hypotension + GCS
lm.fl <- glm(fluids0 ~ bp_low + gcs_cat, data=data, family="binomial")
summary(lm.fl)

# Logistic regression of oxygen administration ~ hypoxia + GCS
lm.ox <- glm(oxygen ~ ox_low + gcs_cat, data=data, family="binomial")
summary(lm.ox)

# Gaussian regression of fluids wait time ~ hypotension + GCS
lm.fl_wait <- lm(log1p(fluids_wait) ~ bp_low + gcs_cat, data=data)
summary(lm.fl_wait)

# Gaussian regression of oxygen wait time ~ hypoxia + GCS
lm.ox_wait <- lm(log1p(oxygen_wait) ~ ox_low + gcs_cat, data=data)
summary(lm.ox_wait)

# Gaussian regression of MD wait time ~ GCS
lm.cdmd_wait <- lm(log1p(cdmd_wait) ~ gcs_cat + peakhours, data=data)
summary(lm.cdmd_wait)

########################################################################################################################
########################################################################################################################
# ANALYSIS OF PATIENT OUTCOMES 
########################################################################################################################
########################################################################################################################

# Logistic regression of death ~ hypoxia + hypotension + GCS
lm.x <- glm(death ~ ox_low + bp_low + gcs_cat, data=data, family="binomial")
summary(lm.x)

# Exact tests for death ~ ontime
tab1 <- table(data$death[data$CTAS=="1"], data$CTAS_ontime[data$CTAS=="1"])
tab2 <- table(data$death[data$CTAS=="2"], data$CTAS_ontime[data$CTAS=="2"])
tab3 <- table(data$death[data$CTAS=="3-5"], data$CTAS_ontime[data$CTAS=="3-5"])
dimnames(tab1) <- dimnames(tab2) <- dimnames(tab3) <- list(death=c("N", "Y"), ontime=c("N", "Y"))
fisher.test(tab1) # Level 1
fisher.test(tab2) # Level 2
# Don't do test for Level 3 because we can't distinguish 3-5 and there is a cell with 0

########################################################################################################################
########################################################################################################################
# VISUALIZATIONS 
########################################################################################################################
########################################################################################################################


############################################################
### waiting time distributions / predictors of waiting time
############################################################

# NOT FOR MANUSCRIPT
# Boxplots of log MD waiting times across different GCS (are sicker patients being seen more quickly?)
quartz()
layout(matrix(1,1,1))
gcs <- as.factor(data$gcs_tot)
data <- cbind(data, gcs)
boxplot(cdmd_wait~gcs, col="gray", main="MD wait times across incoming GCS", ylab="MD wait time", xlab="Incoming patient GCS")
abline(h=median(cdmd_wait), na.rm=T), col="red")

# NOT FOR MANUSCRIPT
# Boxplots of log MD waiting times across different times of day (does time of day affect how quickly patients are seen?)
quartz()
layout(matrix(1,1,1))
hour <- as.factor(format(data$time_arrival, format='%H'))
boxplot(cdmd_wait~hour, col="gray", main="MD wait times across hours of the day", ylab="MD wait time", xlab="Hour of patient arrival")
abline(h=median(cdmd_wait), na.rm=T), col="red")
# Linear regression of MD waiting times as a function of time of day
summary(lm(log1p(cdmd_wait)~hour))

# Are hypoxic/hypotensive patients given oxygen/fluids more quickly?
quartz()
layout(matrix(1:2,1,2))
boxplot(log1p(oxygen_wait)~ox_low, data=data, col="skyblue", names=c("Normal ox", "Low ox"), main="Oxygen wait times", ylab="log1p(Oxygen wait time)")
boxplot(log1p(fluids_wait)~bp_low, data=data, col="pink", names=c("Normal/high bp", "Low bp"), main="Fluid wait times", ylab="log1p(Fluids wait time)")
summary(glm(log1p(oxygen_wait)~ox_low)) #
summary(glm(log1p(fluids_wait)~bp_low)) #*

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
barplot(ox[1,]/colSums(ox), ylim=c(0,1), names.arg=c("Normal", "Low", "NA"), main="Proportion of patients receiving oxygen", col="skyblue", xlab="Pulse oxygen")

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
barplot(fl[1,]/colSums(fl), ylim=c(0,1), names.arg=c("Low", "Normal", "High", "NA"), main="Proportion of patients receiving fluids", col="pink", xlab="Blood pressure")

# Appropriate administration of oxygen at different GCS levels
quartz()
layout(matrix(1:4, 2, 2, byrow=T))
# Proportion hypoxic who get oxygen
hypox_ox <- table(data$gcs_cat[data$ox_low==1 & data$oxygen==1])/table(data$gcs_cat[data$ox_low==1])
# Proportion hypoxic who do NOT get oxygen
hypox_nox <- table(data$gcs_cat[data$ox_low==1 & data$oxygen==0])/table(data$gcs_cat[data$ox_low==1])
# Proportion not hypoxic who get oxygen
nhypox_ox <- table(data$gcs_cat[data$ox_low==0 & data$oxygen==1])/table(data$gcs_cat[data$ox_low==0])
# Proportion not hypoxic who don't get oxygen
nhypox_nox <- table(data$gcs_cat[data$ox_low==0 & data$oxygen==0])/table(data$gcs_cat[data$ox_low==0])
# Proportion hypotensive who get fluids
hypot_fl <- table(data$gcs_cat[data$bp_low==1 & data$fluids0==1])/table(data$gcs_cat[data$bp_low==1])
# Proportion hypotensive who do NOT get fluids
hypot_nfl <- table(data$gcs_cat[data$bp_low==1 & data$fluids0==0])/table(data$gcs_cat[data$bp_low==1])
# Proportion not hypotensive who get fluids
nhypot_fl <- table(data$gcs_cat[data$bp_low==0 & data$fluids0==1])/table(data$gcs_cat[data$bp_low==0])
# Proportion not hypotensive who don't get fluids
nhypot_nfl <- table(data$gcs_cat[data$bp_low==0 & data$fluids0==0])/table(data$gcs_cat[data$bp_low==0])
# Needed treatments given
barplot(rbind(hypox_ox, hypot_fl), ylim=c(0,1), beside=T, main="Needed treatment was given", col=c("skyblue", "pink"), ylab="Frequency")
# Needed treatments not given
barplot(rbind(hypox_nox, hypot_nfl), ylim=c(0,1), beside=T, main="Needed treatment NOT given", col=c("skyblue", "pink"))
# Unneeded treatments given
barplot(rbind(nhypox_ox, nhypot_fl), ylim=c(0,1), beside=T, main="Unneeded treatment was given", col=c("skyblue", "pink"), xlab="TBI category", ylab="Frequency")
legend("bottomleft", xpd=TRUE, inset=c(-0.25,-0.5),  legend=c("Oxygen", "Fluids"), fill=c("skyblue","pink"), cex=0.75)
# Unneeded treatment not given
barplot(rbind(nhypox_nox, nhypot_nfl), ylim=c(0,1), beside=T, main="Unneeded treatment NOT given", col=c("skyblue", "pink"), xlab="TBI category")

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
quartz()
layout(matrix(1, 1, 1))
barplot(t(t(freq)/colSums(freq)), beside=FALSE, col=c("skyblue", "darkred"), xlab="GCS", ylab="Proportions", main="Death and Recovery vs. Incoming GCS")
legend("topleft", xpd=TRUE, inset=c(-0.1,-0.2),  legend=c("Death", "Recovery"), fill=c("darkred", "skyblue"))

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
mod = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + fluids0 + oxygen, data=data2, family="binomial")
summary(mod) # gcs, fluids (+), oxygen (+)
# fluids time (n = 512)
mod2 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + log1p(fluids_wait) + oxygen, data=data2, family="binomial")
summary(mod2)	# gcs, oxygen
# oxygen time (n = 60)
mod3 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + log1p(oxygen_wait) + fluids0, data=data2, family="binomial")
summary(mod3)	# gcs

# Data for gcs 3-8
data3 = data2[which(data$gcs_tot<9),]
# looks only at whether fluids and oxygen were given (n = 178)
mod38.1 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + fluids0 + oxygen, data=data3, family="binomial")
summary(mod38.1)	# gcs, oxygen
# fluids time (n = 83)
mod38.2 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + log1p(fluids_wait) + oxygen, data=data3, family="binomial")
summary(mod38.2)	# gcs
# oxygen time (n = 42)
mod38.3 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + log1p(oxygen_wait) + fluids0, data=data3, family="binomial")
summary(mod38.3)

# Data for gcs 9-13
data4 = data2[which(data2$gcs_tot>8 & data2$gcs_tot<14),]
# looks only at whether fluids and oxygen were given (299 df)
mod913.1 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + fluids0 + oxygen, data=data4, family="binomial")
summary(mod913.1)	# gcs, cdmd, fluids, oxygen
# fluids time (n = 123)
mod913.2 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + log1p(fluids_wait) + oxygen, data=data4, family="binomial")
summary(mod913.2)	# Oxygen
# oxygen time (n = 8)
#mod914.3 = glm(death ~ gcs_tot + male + age + bp_low + log1p(cdmd_wait) + log1p(oxygen_wait) + fluids0, data=data4, family="binomial")
#summary(mod914.3)

# Data for gcs 14-15
data5 = data2[which(data2$gcs_tot>13),]
# looks only at whether fluids and oxygen were given ()
mod15.1 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + fluids0 + oxygen, data=data5, family="binomial")
summary(mod15.1)	# age, ox_low
# fluids time ()
mod15.2 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + log1p(fluids_wait) + oxygen, data=data5, family="binomial")
summary(mod15.2)	# NA
mod15.3 = glm(death ~ gcs_tot + male + age + bp_low + ox_low + log1p(cdmd_wait) + log1p(oxygen_wait) + fluids0, data=data5, family="binomial")
summary(mod15.3)	# NA

# Analysis of only stage 2 and 3 hypertensive patients (68 df)
data.bp = data[which(data$sys_bp>159),]
ox_low = data.bp$pulse_ox
ox_low[which(ox_low < 92)] = 1; ox_low[which(ox_low > 91)] = 0
cbind(data.bp, ox_low)
mod.bp = glm(death ~ gcs_tot + sys_bp + ox_low + fluids + oxygen, data=data.bp, family="binomial")
summary(mod.bp)	# gcs, fluids

# Analysis of low blood pressure patients (56 df)
data.bp2 = data[which(data$sys_bp<90),]
ox_low = data.bp2$pulse_ox
ox_low[which(ox_low < 92)] = 1; ox_low[which(ox_low > 91)] = 0
cbind(data.bp, ox_low)
mod.bp2 = glm(death ~ gcs_tot + sys_bp + ox_low + fluids + oxygen, data=data.bp2, family="binomial")
summary(mod.bp2)	# gcs


########################################################################################################################
########################################################################################################################
# END 
########################################################################################################################
########################################################################################################################





