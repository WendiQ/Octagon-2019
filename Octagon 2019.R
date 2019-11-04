# Octagon 2019

# Import datasets
screen <- read.csv("Screen.csv")
colnames(screen)[colnames(screen)=="X18."] <- "18up"
screen <- screen[, 1:10]
dem1 <- read.csv("Demographics1.csv")
dem2 <- read.csv("Demographics2.csv")
medhis1 <- read.csv("Medhistory1.csv")
medhis2 <- read.csv("Medhistory2.csv")
acq <- read.csv("ACQ.csv")
acq <- acq[-is.na(acq$id)==0,] # Get rid of rows of NA values

# Dataset insepction
## medhis2
## Transform date from numeric to datetime
medhis2$assess <- as.Date(medhis2$assess, origin="1899-12-30")

# Change column names that appear in more than 1 dataset
colnames(screen)[colnames(screen)=="date"] <- "date_screen"
colnames(acq)[colnames(acq)=="date"] <- "date1"
colnames(medhis2)[colnames(medhis2)=="assess"] <- "assess_mh"
colnames(dem2)[colnames(dem2)=="assess"] <- "assess_dem"

# Merge dataset
## Check if id is unique in "screen"
length(unique(screen$id))==nrow(screen) # outputs TRUE

## Concatenate 2 demographics datasets
dem <- dplyr::bind_rows(dem1, dem2)
dem <- dem[is.na(dem$id)==0, ] # Drop null rows

## Concatenate 2 medical history datasets
### Notice: medhis1 has a "co.arthritis" column and medhis2 has a "arthritis"
### column. Rename the first one so that they can be merged into 1 column.
colnames(medhis1)[colnames(medhis1)=="co.arthritis"] <- "arthritis"
medhis <- dplyr::bind_rows(medhis1, medhis2)
medhis <- medhis[is.na(medhis$id)==0, ] # Drop null rows

## Merge screen with dem, medhis, and acq
scr_dem <- merge(screen, dem, by="id")
scr_mh <- merge(scr_dem, medhis, by="id")
master <- merge(scr_mh, acq, by="id", all.x = TRUE) # master is the merged dataset

# Clean dataset
## Demographics
### Race & ethinicity
#### Changing Canadian race to their ethnicity
for (i in 1:nrow(master)) {
  if (master$race[i] == "Canadian" && nchar(as.character(master$ethnicity[i])) > 1) {
    master$race[i] <- as.character(master$ethnicity[i])
  }
}

#### Standardize categories
master$race[master$race == "White" | master$race == "white (caucasian)"] <- "white"
master$race[master$race == "arab"] <- "arab/west asian"
master$race[master$race == "Japanese"] <- "japanese"
master$race[master$race =="latin american"] <- "south american"
master$race_group[master$race== "Indian" | master$race=="south asian"] <- "south asian"
master$race_group[master$race== "japanese" | master$race =="chinese" |
                    master$race=="korean" | master$race=="south east asian" |
                    master$race =="filipino"] <- "east/southeast asian"
master$race_group[master$race=="asian"] <- "asian (unspecified)"
master$race_group[master$race=="aboriginal"|master$race=="Cree"] <- "aboriginal"
master$race_group[master$race=="white"|master$race=="Syria/Serbia"|
                    master$race=="Dutch"|master$race=="Nunavut"|
                    master$race=="Canadian"] <- "white"
master$race_group[master$race=="arab/west asian"] <- "west asian"
master$race_group[master$race=="south american"] <- "south american"
master$race_group[master$race=="south african"|master$race=="black"] <- "black"
master$race_group[is.na(master$race_group)==1] <- "other"

### Gender/Sex
for (i in 1:nrow(master)) {
  if (is.na(master$sex[i])==1 & (master$gender[i]==1 | master$gender[i]==0)) {
    master$sex[i] <- master$gender[i]
  } else if (is.na(master$sex[i])==1 & (master$gender[i]==2 | master$gender[i]==3)) {
    master$sex[i] <- 2  # 2 is trans or unknown
  }
}

for (i in 1:nrow(master)) {
  if (is.na(master$gender[i])==1) {
    master$gender[i] <- master$sex[i]
  }
}

### Age
#### Calculate the dem2 assess age
ass_date <- as.Date(as.character(master$assess_dem), "%d-%m-%Y")
master$assess_dem <- as.Date(as.character(master$assess_dem), "%d-%m-%Y")
ass_year <- c(as.numeric(format(ass_date, "%Y")))
birth_year <- as.numeric(master$birthyear)
master$demo_age <- ass_year - birth_year

#### Calculate the screening age
ACQ_date <- as.Date(as.character(master$date_screen), "%d-%m-%Y")
ACQ_year <- c(as.numeric(format(ACQ_date, "%Y")))
birth_year <- as.numeric(master$birthyear)
master$ACQ_age <- ACQ_year - birth_year

#### Calculate the age of medhistory assess
mh_date <- as.Date(as.character(master$assess_mh), "%Y-%m-%d")
mh_year <- c(as.numeric(format(mh_date, "%Y")))
birth_year <- as.numeric(master$birthyear)
master$mh_age <- mh_year - birth_year

### Work Status
# 0 - unemployed, 1 - working, 2 - retired
for (i in 1:nrow(master)) {
  if (master$work[i]==0 & master$retire[i]==1) {
    master$work[i] <- 2
  }
}

## ACQ
### Eliminate entry with false dates
master$date1 = as.Date(as.character(master$date1), format = "%d-%m-%Y")
master$date2 = as.Date(as.character(master$date2), format = "%d-%m-%Y")
master$date3 = as.Date(as.character(master$date3), format = "%d-%m-%Y")
master$date4 = as.Date(as.character(master$date4), format = "%d-%m-%Y")
master$date5 = as.Date(as.character(master$date5), format = "%d-%m-%Y")
master$date_screen = as.Date(as.character(master$date_screen), format = "%d-%m-%Y")

master$date1[master$date1 <= "2016-12-31"] = NA
master$date1[master$date1 >= "2019-10-31"] = NA

master$date2[master$date2 <= "2016-12-31"] = NA
master$date2[master$date2 >= "2019-10-31"] = NA


master$date3[master$date3 <= "2016-12-31"] = NA
master$date3[master$date3 >= "2019-10-31"] = NA

master$date4[master$date4 <= "2016-12-31"] = NA
master$date4[master$date4 >= "2019-10-31"] = NA

master$date5[master$date5 <= "2016-12-31"] = NA
master$date5[master$date5 >= "2019-10-31"] = NA

### To check whether screen date is before acq date
table(master$date_screen <= master$date1)
table(master$date_screen <= master$date2)
table(master$date_screen <= master$date3)
table(master$date_screen <= master$date4)
table(master$date_screen <= master$date5)

### Set acq.screen > 6 to NA
#master$acq.screen[master$acq.screen > 6] = NA

### Set result of n to 0, and blank to 1
master$result =as.numeric(master$result)
master$result[master$result == 2] = 0

### Change column variable types
#### Categorical variables -> integer
master$sex <- as.integer(master$sex)

#### Continuous variables -> numerical
master$height <- as.numeric(master$height)
master$weight <- as.numeric(master$weight)

## Missing data
### Missing value overview
missing <- function(df) {
  # Returns column name, missing value %, datatype
  for (column in colnames(df)) {
    print(sprintf("%s | %.2f | %s",
                  column, sum(is.na(df[[column]]))/length(df[[column]]), class(df[[column]])))
  }
}

(missing(master))

### Missing 18up
master$ACQ_age[master[["18up"]]==""] # Everyone is over 18

### Impute missing values

# Numerical values are imputed using median
# Categorical values are imputed using the observed % among categories

columns <- c("smoking", "weight", "height", "polyps",
             "arthritis", "CVD", "allergy", "vaccine")

for (colname in columns) {
  if (class(master[[colname]])=="numeric") {
    master[[colname]][is.na(master[[colname]])] <- median(master[[colname]], na.rm = TRUE)
  } else if (class(master[[colname]])=="integer") {
    percent <- table(master[[colname]])[["1"]]/nrow(master)
    num_na <- length(master[[colname]][is.na(master[[colname]])])
    master[[colname]][is.na(master[[colname]])] <- rbinom(num_na, 1, percent)
  }
}

## Drop columns/rows
### Delete infconsent = blank
master <- subset(master, infconsent=="y")

### Drop ethnicity column
master <- subset(master, select= -c(infconsent, ethnicity))

### Drop columns with only NA
master <- master[ ,colSums(is.na(master)) != nrow(master)]

#Calculate BMI using weight and height
wt_lb <- master$weight * 0.453592

ht_m <- master$height
height_m <- numeric(0)
for (i in 1:nrow(master)) {
  digits <- ht_m[i]%%10
  tens <- (ht_m[i] - digits) / 10
  ht <- digits * 0.0254 + tens * 0.3048
  height_m <- c(height_m, ht)
}
BMI_data <- wt_lb / (height_m)^2
master$BMI <- BMI_data


# Analysis
## Calculate ACQ score fluctuation
date_names <- c("date1", "date2", "date3", "date4", "date5")
acq_names <- c("acq", "acq2", "acq3", "acq4", "acq5")

for (p in 1:nrow(master)) {
  dates <- c()
  acqs <- numeric()
  fluc <- numeric()

  i <- 1
  while(i < length(date_names)+1) {
    if (is.na(master[[date_names[i]]][p]) != 1 && is.na(master[[acq_names[i]]][p]) != 1) {
      dates[i] <- master[[date_names[i]]][p]
      acqs[i] <- master[[acq_names[i]]][p]
    } else if (length(dates) >= 1) {
      dates[i] <- dates[i-1]
      acqs[i] <- acqs[i-1]
    } else {
      dates[i] <- NA
      acqs[i] <- NA
    }
    if (length(dates) > 1 && is.na(dates[i-1]) != 1) {
      acq_diff <- acqs[i] - acqs[i-1]
      date_diff <- dates[i] - dates[i-1]
      fluc <- c(fluc, acq_diff * date_diff)
    }
    i <- i + 1
  }
  if (length(fluc) > 0) {
    fluc_rate <- sum(fluc)*30/(max(dates, na.rm = TRUE)-min(dates, na.rm = TRUE))
    master$fluctuate[p] <- round(fluc_rate, 4)  # 4 decimal places
  } else {
    master$fluctuate[p] <- NA
  }
}

## Split people into those who had clinics before, during, and after screening
### Pick out earliest and latest clinic dates

for (i in 1:nrow(master)) {
  dates <- c()
  for (name in date_names) {
    dates <- c(dates, master[[name]][i])
  }
  master$first_clinic[i] <- min(dates, na.rm = TRUE)
  master$last_clinic[i] <- max(dates, na.rm = TRUE)
  if (is.infinite(master$first_clinic[i])) {
    master$first_clinic[i] <- NA
    master$last_clinic[i] <- NA
  }
}

master$first_clinic <- as.Date(master$first_clinic, origin = "1970-01-01")
master$last_clinic <- as.Date(master$last_clinic, origin = "1970-01-01")

### 0 - clinic before screening
### 1 - clinic after screening
### 2 - screening during clinic
### 3 - no clinic
for (i in 1:nrow(master)) {
  if (is.na(master$first_clinic[i])!= 1) {
    if (master$first_clinic[i] > master$date_screen[i]) {
      master$clinic_time[i] <- 1  # clinic after screening
    } else if (master$first_clinic[i] < master$date_screen[i] &
               master$last_clinic[i] > master$date_screen[i]) {
      master$clinic_time[i] <- 2  # screening during clinics
    } else {
      master$clinic_time[i] <- 0  # clinic before screening
    }
  } else {
    master$clinic_time[i] <- 3 # no follow up
  }
}

submas <- subset(master, select = c(id, date_screen, result, clinic_time,
                                    first_clinic, last_clinic, date1, acq,
                                    date2, acq2, date3, acq3, date4, acq4,
                                    date5, acq5, fluctuate))




