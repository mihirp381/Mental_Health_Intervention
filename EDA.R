library(readr)
mhcld_puf_2020_csv <- read_csv("mhcld-puf-2020.csv", 
                               col_types = cols(YEAR = col_date(format = "%Y")))
View(mhcld_puf_2020_csv)                                                                            
data <- mhcld_puf_2020_csv

# There are a total of 6945521 observations and 40 variables in the raw dataset

summary(data)

# Variables to be removed : Year, TRAUSTREFLG, CONDUCTFLG, DELIRDEMFLG, BIPOLARFLG, ODDFLG, 
# PDDFLG, PERSONFLG, SCHIZOFLG, ALCSUBFLG, OTHERDISFLG, VETERAN
toremove <- c("YEAR", "TRAUSTREFLG", "CONDUCTFLG", "DELIRDEMFLG", "BIPOLARFLG", "ODDFLG", 
              "PDDFLG", "PERSONFLG", "SCHIZOFLG", "ALCSUBFLG", "OTHERDISFLG", "VETERAN")

data.sub <- data[, !(names(data) %in% toremove)]
data.sub

# Since we are only focusing on age groups from 0-17, we will eliminate other records
# Data dictionary suggests that age groups 0-11, 12-14, 15-17 are coded as 1,2,3 respectively

data.sub <- data.sub[(data.sub$AGE %in% c(1,2,3)), ] # Removed about 4.5 million rows

# We now have the dataset ready to be pushed to the next phase for pre-processing/cleaning
colnames(data.sub)
#Checking the missing rows first

for (name in colnames(data.sub)){
  print(paste(name,"  ",sum(is.na(data.sub[[name]]))))
}
par(mfrow = c(1, 3))
hist(data.sub$ANXIETYFLG, xlab = "ANXIETY FLAG", main = "Histogram for ANXIETY FLAG")
hist(data.sub$ADHDFLG, xlab = "ADHD FLAG", main = "Histogram for ADHD FLAG")
hist(data.sub$DEPRESSFLG, xlab = "DEPRESSION FLAG", main = "Histogram for DEPRESSION FLAG")

# Isn't recognized. This is because the values -9 in all variables are encoded as : 
# Missing/unknown/not collected/invalid/no or deferred diagnosis

count_equal_to_minus9 <- colSums(data.sub == -9)
write.csv(data.frame(count_equal_to_minus9), "NULLS.csv",row.names = row.names(data.frame(count_equal_to_minus9)))
View(data.frame(count_equal_to_minus9))
row.names(data.frame(count_equal_to_minus9))
# Gender has a about 5352 null rows and hence could be eliminated from the dataset as it is less than 1% of records

data.sub <- data.sub[data.sub$GENDER != -9, ]

View(data.frame(colSums(data.sub == -9)))

# We will now look at the imputation techniques to be used for each variable with extremely high frequency of missing rows

nonnulleduc <- data.sub[data.sub$EDUC != -9, c("EDUC", "AGE", "EMPLOY")]
hist(nonnulleduc$EDUC)

# Comparing it with AGE

par(mfrow = c(1, 2))
hist(nonnulleduc$EDUC)
hist(data.sub$AGE)

# Hence, the highest number of records in Education is for 0-8 years in education
# Highest number of records in Age is for 0-11 years
# To further confirm this, we can draw a count matrix of two columns

library(reshape2)

df <- data.frame(
  nonnulleduc$EDUC,
  nonnulleduc$AGE
)

count_matrix <- dcast(df, nonnulleduc.EDUC ~ nonnulleduc.AGE, fun.aggregate = length, value.var = "nonnulleduc.EDUC")

count_matrix[is.na(count_matrix)] <- 0 # Filling in missing combinations with 0

rownames(count_matrix) <- count_matrix$nonnulleduc.EDUC
count_matrix$nonnulleduc.EDUC <- NULL

print(count_matrix)

# Hence, it is evident that Individuals in the age group 0-11 have 0-8 years of education
# Individuals from the age group 12-14 also have 0-8 years of education
# While, individuals from age group 14-17 have 9-11 years of education. 
# This is what one would expect. 

# However, the age group 14-17 still has significant amount of individuals with 0-8 years 
# of education. Maybe employement status is playing a role. Lets check!

# Isolating age group 14-17

nonnulleduc.1417 <- data.sub[data.sub$EDUC != -9, c("EDUC", "AGE", "EMPLOY")]
nonnulleduc.1417 <- nonnulleduc.1417[AGE = 3]

df <- data.frame(
  nonnulleduc.1417$EDUC,
  nonnulleduc.1417$AGE,
  nonnulleduc.1417$EMPLOY
)

count_matrix <- dcast(df, nonnulleduc.1417.AGE + nonnulleduc.1417.EMPLOY ~ nonnulleduc.1417.EDUC, fun.aggregate = length, value.var = "nonnulleduc.1417.AGE")

count_matrix[is.na(count_matrix)] <- 0

rownames(count_matrix) <- paste(count_matrix$nonnulleduc.1417.AGE, count_matrix$nonnulleduc.1417.EMPLOY, sep = "_")
count_matrix$nonnulleduc.1417.AGE <- count_matrix$nonnulleduc.1417.EMPLOY <- NULL

print(count_matrix)

# As expected, no employement for age groups 0-11, 12-14

# For age group 14-17, Majority fall in the education groups 0-8 years and 9-11 years

# However, no clear difference can be observed. 

# Hence, we will go ahead with the initial results for imputing Education 0-8 years 
# for the age groups 0-11 and 12-14, while Education 9-11 for the age group 14-17

data.sub$EDUC[data.sub$AGE %in% c(1,2) & data.sub$EDUC == -9] <- 2
data.sub$EDUC[data.sub$AGE == 3 & data.sub$EDUC == -9] <- 3

# Confirming for no null values in Education : 

count_equal_to_minus9 <- colSums(data.sub == -9)

View(data.frame(count_equal_to_minus9))

# Perfect. Next up, ETHNIC

nonnullethnic <- data.sub[data.sub$ETHNIC != -9, c("ETHNIC")]
hist(nonnullethnic$ETHNIC)
table(nonnullethnic$ETHNIC)

# Hence, mode is 4 : Not of Hispanic or Latino origin. But 3 : Other Hispanic or Latino origin, also has significant number of rows.

# Maybe we can use Region to find if we can find any causation since similar ethnicities reside in similar locations

nonnullethnic <- data.sub[data.sub$ETHNIC != -9, c("REGION", "ETHNIC")]

df <- data.frame(
  nonnullethnic$REGION,
  nonnullethnic$ETHNIC
)

count_matrix <- dcast(df, nonnullethnic.REGION ~ nonnullethnic.ETHNIC, fun.aggregate = length, value.var = "nonnullethnic.REGION")

count_matrix[is.na(count_matrix)] <- 0

rownames(count_matrix) <- count_matrix$nonnullethnic.REGION
count_matrix$nonnullethnic.REGION <- NULL

print(count_matrix)

# Clearly, ETHNIC people 3 : Other Hispanic or Latino origin come from Region 4 : West
# Hence, rather than imputing 4 for all rows, we can say if people come from region 4
# ethicity is 3 otherwise its 4. 

data.sub$ETHNIC[data.sub$REGION == 4 & data.sub$ETHNIC == -9] <- 3
data.sub$ETHNIC[data.sub$REGION != 4 & data.sub$ETHNIC == -9] <- 4

# Perfect we removed nulls from ETHNICITY as well! Next up, Race. 

count_equal_to_minus9 <- colSums(data.sub == -9)

View(data.frame(count_equal_to_minus9))

# Checking the distribution of Race

nonnullrace <- data.sub[data.sub$RACE != -9, c("RACE", "REGION")]
hist(nonnullrace$RACE)
table(nonnullrace$RACE)

# The mode is clearly race 5 : White. However, lets consider other common factors such as region and 

df <- data.frame(
  nonnullrace$RACE,
  nonnullrace$REGION
)

count_matrix <- dcast(df, nonnullrace.RACE ~ nonnullrace.REGION, fun.aggregate = length, value.var = "nonnullrace.RACE")

count_matrix[is.na(count_matrix)] <- 0

rownames(count_matrix) <- count_matrix$nonnullrace.RACE
count_matrix$nonnullrace.RACE <- NULL

print(count_matrix)

# No clear division, the mode is still the race WHITE for all regions

# Lets try DIVISION or STATEFIP 

nonnullrace <- data.sub[data.sub$RACE != -9, c("RACE", "DIVISION")]

df <- data.frame(
  nonnullrace$RACE,
  nonnullrace$DIVISION
)

count_matrix <- dcast(df, nonnullrace.RACE ~ nonnullrace.DIVISION, fun.aggregate = length, value.var = "nonnullrace.RACE")

count_matrix[is.na(count_matrix)] <- 0

rownames(count_matrix) <- count_matrix$nonnullrace.RACE
count_matrix$nonnullrace.RACE <- NULL

print(count_matrix)

# There is still no major difference except for division 9: Pacific which has people from race 6 : Some other race alone/two or more races

nonnullrace <- data.sub[data.sub$RACE != -9, c("RACE", "STATEFIP")]

df <- data.frame(
  nonnullrace$RACE,
  nonnullrace$STATEFIP
)

count_matrix <- dcast(df, nonnullrace.RACE ~ nonnullrace.STATEFIP, fun.aggregate = length, value.var = "nonnullrace.RACE")

count_matrix[is.na(count_matrix)] <- 0

rownames(count_matrix) <- count_matrix$nonnullrace.RACE
count_matrix$nonnullrace.RACE <- NULL

View(count_matrix)

# Now we can see the clear difference in race from statefips. 

state.race <- t(count_matrix)
state.race$major.race <- colnames(state.race)[max.col(state.race,ties.method="first")]

race.imput <- rbind(count_matrix, as.list(state.race$major.race))

mapping <- as.list(race.imput[7, ])

data.sub$RACE <- mapping[as.character(data.sub$STATEFIP)]

# Confirming no null values for RACE

count_equal_to_minus9 <- colSums(data.sub == -9)

View(data.frame(count_equal_to_minus9))

# Next up, the variable MH1 : Mental Health diagnostic 1

nonnullmh1 <- data.sub[data.sub$MH1 != -9, c("MH1", "MH2", "MH3", "SUB")]
hist(nonnullmh1$MH1)
table(nonnullmh1$MH1)

# There is no clear mode, Classes 1, 2, 3, 7, 13 have significantly greater values 

nulls.MH1 <- data.sub[data.sub$MH1 == -9,]

count_equal_to_minus9 <- colSums(nulls.MH1 == -9)

View(data.frame(count_equal_to_minus9))


# Hence, clearly, the records where MH1 is not collected, MH2, MH3 are also 
# not collected either. Also, 98% of the rows have SUB missing as well for such records. 
# Interesting!! Will be back later to this. 

# Next up, MARSTAT

nonnullmarstat <- data.sub[data.sub$MH1 != -9, c("MARSTAT")]
hist(nonnullmarstat$MARSTAT)
table(nonnullmarstat$MARSTAT)

# Hence, as expected, it is rare that individuals are married by the age of 17
# Maybe, ones that are married are from a particular race or ethnicity

marriedonce <- data.sub[data.sub$MARSTAT %in% c(2, 3, 4), c("MARSTAT", "ETHNIC", "RACE")]

df <- data.frame(
  marriedonce$MARSTAT,
  marriedonce$ETHNIC
)

count_matrix <- dcast(df, marriedonce.MARSTAT ~ marriedonce.ETHNIC, fun.aggregate = length, value.var = "marriedonce.MARSTAT")

count_matrix[is.na(count_matrix)] <- 0

rownames(count_matrix) <- count_matrix$marriedonce.MARSTAT
count_matrix$marriedonce.MARSTAT <- NULL

View(count_matrix)

# NOTHING SIGNIFICANT. HENCE, WE WILL SIMPLY IMPUTE BY 1 : NOT MARRIED

data.sub$MARSTAT[data.sub$MARSTAT == -9] <- 1

# Confirming no nulls

count_equal_to_minus9 <- colSums(data.sub == -9)

View(data.frame(count_equal_to_minus9))
data.sub[,c('MH1', 'MH2', 'MH3', 'SUB')]

# The -9s in MH diagnosis variables are not actually nulls. They are instances of unidentified or 
# negative results. That is no MH problem was diagnosed. 

# Next up, EMPLOY, DETNLF. 

nullemp <- data.sub[data.sub$EMPLOY == -9, c("EMPLOY", "DETNLF")]

sum(is.na(nullemp[nullemp$EMPLOY == -9 && nullemp$DETNLF == -9,])
)

# Hence, confirmed that employ and detnlf have the same NULLs

# We will remove these variables
nrow(nullemp[nullemp$EMPLOY == -9 && nullemp$DETNLF == -9,])

data.sub <- data.sub[, !(names(data.sub) %in% c('EMPLOY', 'DETNLF'))]

names(data.sub)



# Counting the number of nulls again
# Initialize an empty vector to store counts
count_neg9 <- numeric(ncol(data.sub))

# Loop through each column and count the number of -9 values
for(i in 1:ncol(data.sub)){
  count_neg9[i] <- sum(data.sub[[i]] == -9, na.rm = TRUE)
}

# Create a named vector to easily identify each variable
names(count_neg9) <- colnames(data.sub)

# Display the counts
count_neg9

# Next up, LIVARAG

# After inspecting the data dictionary, it seems like the absence of an address is itself an 
# indication of some factor which may lead to mental health. To confirm this, we will compare,
# the number of observations with each of the flags where there is a record of any form of address 
# vs where the LIVARAG is -9

livaragn9 <- data.sub[(data.sub$LIVARAG != -9),]
livarag9 <- data.sub[(data.sub$LIVARAG == -9),]

table(livaragn9$ANXIETYFLG)
table(livaragn9$ADHDFLG)
table(livaragn9$DEPRESSFLG)

table(livarag9$ANXIETYFLG)
table(livarag9$ADHDFLG)
table(livarag9$DEPRESSFLG)

# Contrary to the hypothesis. We will encode it as undetected. 

70974/nrow(data.sub)

# Since it is less than 5% of the number of rows, we will remove records with SMISED = -9

data.sub<- data.sub[(data.sub$SMISED != -9),]


count_neg9 <- numeric(ncol(data.sub))

# Loop through each column and count the number of -9 values
for(i in 1:ncol(data.sub)){
  count_neg9[i] <- sum(data.sub[[i]] == -9, na.rm = TRUE)
}

# Create a named vector to easily identify each variable
names(count_neg9) <- colnames(data.sub)

# Display the counts
count_neg9

# For the variables MH1, MH2, MH3, SUB, SAP, LIVARAG we will now create dummies 

library(fastDummies)

MH1_dummies <- dummy_cols(data.sub, select_columns = "MH1", remove_selected_columns = TRUE)

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "MH2", remove_selected_columns = TRUE)

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "MH3", remove_selected_columns = TRUE)

names(MH1_dummies)

# We will rename MH1_-9 as NOMH, and remove MH2_-9, MH3_-9
colnames(MH1_dummies)[colnames(MH1_dummies) == "MH1_-9"] <- "NOMH"

names(MH1_dummies)


MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('MH2_-9', 'MH3_-9'))]

# Creating dummies for LIVARAG

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "LIVARAG", remove_selected_columns = TRUE)

names(MH1_dummies)

# Removing LIVARAG_-9

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('LIVARAG_-9'))]

# Dropping CASEID, dummies for AGE, EDUCATION, RACE, ETHNIC, Gender

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "AGE", remove_selected_columns = TRUE)

names(MH1_dummies)

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('AGE_1', 'CASEID'))]

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "EDUC", remove_selected_columns = TRUE)

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('EDUC_1'))]

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "ETHNIC", remove_selected_columns = TRUE)

names(MH1_dummies)

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('ETHNIC_1'))]

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "RACE", remove_selected_columns = TRUE)

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('RACE_1'))]

names(MH1_dummies)

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "GENDER", remove_selected_columns = TRUE)

names(MH1_dummies)

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('GENDER_1'))]

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "SPHSERVICE", remove_selected_columns = TRUE)

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('SPHSERVICE_1'))]

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "CMPSERVICE", remove_selected_columns = TRUE)

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('CMPSERVICE_1'))]

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "OPISERVICE", remove_selected_columns = TRUE)

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('OPISERVICE_1'))]

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "RTCSERVICE", remove_selected_columns = TRUE)

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('RTCSERVICE_1'))]

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "IJSSERVICE", remove_selected_columns = TRUE)

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('IJSSERVICE_1'))]

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "SUB", remove_selected_columns = TRUE)

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('SUB_1'))]

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "MARSTAT", remove_selected_columns = TRUE)

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('MARSTAT_1'))]

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "SMISED", remove_selected_columns = TRUE)

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('SMISED_1'))]

names(MH1_dummies)

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "SAP", remove_selected_columns = TRUE)

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('SAP_-9'))]

MH1_dummies <- dummy_cols(MH1_dummies, select_columns = "NUMMHS", remove_selected_columns = TRUE)

# We will drop NUMMHS as it is directly correlated with MH1, MH2, MH3

MH1_dummies <- MH1_dummies[, !(names(MH1_dummies) %in% c('NUMMHS_0', 'NUMMHS_1', 'NUMMHS_2', 'NUMMHS_3'))]

count_neg9 <- numeric(ncol(MH1_dummies))

# Loop through each column and count the number of -9 values
for(i in 1:ncol(MH1_dummies)){
  count_neg9[i] <- sum(MH1_dummies[[i]] == -9, na.rm = TRUE)
}

# Create a named vector to easily identify each variable
names(count_neg9) <- colnames(MH1_dummies)

# Display the counts
count_neg9

# We now have a cleaned dataset. Saving it to a CSV:

write.csv(MH1_dummies, "Cleaned_data.csv", row.names = FALSE)

# Next steps will be to understand the correlations between each variable with the response variables

df <- read.csv('Cleaned_data.csv')

View(df)
nrow(df)
table(df$ANXIETYFLG)
table(df$DEPRESSFLG)
table(df$ADHDFLG)


df <- df[, !(names(df) %in% c('MARSTAT_2', 'MARSTAT_3', 'MARSTAT_4', 'STATEFIP', 'REGION', 'DIVISION'))]

# Stratified Sampling 80% : Train, 20% : Test
library(caret)
set.seed(123) # for reproducibility

# Create stratified partitions
trainIndex <- createDataPartition(df$ANXIETYFLG, p = 0.8, list = FALSE, times = 1)

# Create the training and test sets
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

response <- trainData$ANXIETYFLG
predictors <- trainData[, !(names(trainData) %in% c("ANXIETYFLG", "ADHDFLG", "DEPRESSFLG"))]




library(randomForest)

rf_model <- randomForest(response1 ~ ., data = predictors, ntree = 500)








