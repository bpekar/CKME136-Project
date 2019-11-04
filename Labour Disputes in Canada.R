install.packages("curl")
install.packages("sqldf")
install.packages("randomForest")
install.packages("AER")
library(curl)
library(sqldf)
library(randomForest)
library(AER)

download.file(url = "https://www150.statcan.gc.ca/n1/en/tbl/csv/14100121-eng.zip?st=1eq49VM9",
              destfile = "hours_lost.zip",
              method = "curl")
download.file(url = "https://www150.statcan.gc.ca/n1/en/tbl/csv/36100103-eng.zip?st=06jRTwkc",
              destfile = "gdp.zip",
              method = "curl")
download.file(url = "https://www150.statcan.gc.ca/n1/en/tbl/csv/14100017-eng.zip?st=PqrOQSRY",
              destfile = "unemp.zip",
              method = "curl")



hours_lost <- read.csv(unz("hours_lost.zip", "14100121.csv"))

i <- sapply(hours_lost, is.factor)
hours_lost[i] <- lapply(hours_lost[i], as.character)

reduced <- data.frame(hours_lost$ï..REF_DATE,
                      hours_lost$GEO,
                      hours_lost$Hours.lost,
                      hours_lost$Reason.of.absence,
                      hours_lost$Sex,
                      hours_lost$VALUE)
names(reduced) <- c("date", "geo", "hours_ees", "reason", "sex", "value")
reduced$value[is.na(reduced$value)] <- 0
full_weeks <- reduced[reduced$reason == "Labour dispute (strike or lockout), away full week",]
part_weeks <- reduced[reduced$reason == "Labour dispute (strike or lockout), away part week",]
combined <- data.frame(full_weeks$date,
                       full_weeks$geo,
                       full_weeks$hours_ees,
                       full_weeks$sex,
                       1000*(full_weeks$value+part_weeks$value))
names(combined) <- c("date", "geo", "hours_ees", "sex", "value")
i <- sapply(combined, is.factor)
combined[i] <- lapply(combined[i], as.character)

hours_clean <- combined[which(!startsWith(combined$date, "1976") &
                                !startsWith(combined$date, "1977") &
                                !startsWith(combined$date, "1978") &
                                !startsWith(combined$date, "2019") &
                                combined$hours_ees == "Hours lost by employees"),
                        c(1,2,4,5)]

rm(i, combined, full_weeks, hours_lost, part_weeks, reduced)



gdp <- read.csv(unz("gdp.zip", "36100103.csv"))

i <- sapply(gdp, is.factor)
gdp[i] <- lapply(gdp[i], as.character)

gdp <- gdp[which(gdp$Estimates == "Gross domestic product at market prices" &
                   gdp$Seasonal.adjustment == "Seasonally adjusted at annual rates"),]

reduced <- data.frame(gdp$ï..REF_DATE,
                      gdp$VALUE)
names(reduced) <- c("date", "value")
reduced$date <- as.character(reduced$date)
reduced <- reduced[which(as.numeric(substr(reduced$date,1,4))>1978 &
                           as.numeric(substr(reduced$date,1,4))<2019),]

prevRow <- function(x) {
  r <- (1L-1):(length(x)-1)
  r[r<1] <- NA
  return(x[r])
}

reduced$prev <- prevRow(reduced$value)
reduced$lower <- reduced$value<reduced$prev
reduced$lower2 <- prevRow(reduced$lower)
reduced$recession <- reduced$lower & reduced$lower2
reduced$recession[1] <- F

reduced$month <- as.integer(substr(reduced$date,6,7))

insert1 <- transform(reduced, date=paste(substr(reduced$date,1,5),ifelse(month<9,"0",""),month+1,sep=""))
insert2 <- transform(reduced, date=paste(substr(reduced$date,1,5),ifelse(month<9,"0",""),month+2,sep=""))

reduced <- rbind(reduced,insert1,insert2)

gdp_clean <- reduced[,c(1,2,6)]
rm(i, prevRow, reduced, gdp, insert1, insert2)



unemp <- read.csv(unz("unemp.zip", "14100017.csv"))

emp <- unemp[which(unemp$Labour.force.characteristics == "Labour force" &
                     unemp$Age.group == "15 years and over"),]
rate <- unemp[which(unemp$Labour.force.characteristics == "Unemployment rate" &
                       unemp$Age.group == "15 years and over"),]

colnames(emp)[13] <- "emp"
colnames(rate)[13] <- "rate"

reduced <- data.frame(emp$ï..REF_DATE,
                      emp$GEO,
                      emp$Sex,
                      emp$emp,
                      rate$rate)
names(reduced) <- c("date", "geo", "sex", "emp", "value")
unemp_clean <- reduced[which(as.numeric(substr(reduced$date,1,4))>1978 &
                           as.numeric(substr(reduced$date,1,4))<2019),]

i <- sapply(unemp_clean, is.factor)
unemp_clean[i] <- lapply(unemp_clean[i], as.character)

rm(i, reduced, unemp, emp, rate)



df <- sqldf("SELECT h.date, h.geo, h.sex, h.value, g.recession
             FROM gdp_clean g
             JOIN hours_clean h
             ON h.date = g.date
            ")
names(df)[4] <- "hours"
df <- sqldf("SELECT d.date, d.geo, d.sex, d.hours, d.recession, u.emp, u.value
             FROM unemp_clean u
             JOIN df d
             ON d.date = u.date
             AND d.geo = u.geo
             AND d.sex = u.sex
            ")
names(df)[7] <- "unemp"

df$date <- 12*as.integer(substr(df$date,1,4)) + as.integer(substr(df$date,6,7))
df$hours <- df$hours/df$emp

females <- df[df$sex=="Females",6]
df <- df[df$sex=="Both sexes",c(1,2,4,5,6,7)]
df$sex <- females/df$emp
rm(females)

outliers <- boxplot(df$hours, plot=F)$out
df <- df[-which(df$hours %in% outliers),]
rm(outliers)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))}
df$hours <- normalize(df$hours)
df$unemp <- normalize(df$unemp)
df$date <- normalize(df$date)
df$sex <- normalize(df$sex)
rm(normalize)


plot(df[,c("hours", "unemp", "date", "sex")])


lin_model <- lm(df$hours ~ df$geo + df$sex + df$recession + df$unemp + df$date)
summary(lin_model)
plot(predict(lin_model),df$hours)


df_fac<-df
df_fac$geo<-as.factor(df_fac$geo)
set.seed(0)
rf_model <- randomForest(hours ~ geo + sex + recession + unemp + date, data=df_fac, importance=T)
rf_model$importance
plot(predict(rf_model),df$hours)
rm(df_fac)


ivr_model <- ivreg(df$hours ~ df$geo + df$sex + df$recession + df$unemp + df$date)
ivr_model
plot(predict(ivr_model),df$hours)


sum(residuals(lin_model)^2)
sum((df$hours-predict(rf_model))^2)
sum(residuals(ivr_model)^2)