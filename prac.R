#getting and setting working directory
getwd()
setwd("C:/Users/pande/OneDrive/Desktop/house-prices-advanced-regression-techniques")

#loading train and test data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#reading data
head(train)
str(train)  #no factors yet

#hypothesis 1- Sale price depends on MSSubClass 
#hypothesis 2- Sale price depends on LotArea 
#hypothesis 3- Sale price depends on Utilities
#hypothesis 4- Sale price depends on Neighbourhood
#hypothesis 5- Sale price depends on Condition1, Condition2
#hypothesis 6- Sale price depends on BldgType
#hypothesis 7- Sale price depends on HouseStyle 
#hypothesis 8- Sale price depends on OverallQual, OverallCond
#hypothesis 9- Sale price depends on YearBuilt, YearRemodAdd
#hypothesis 10- Sale price depends on Exterior1st,2nd
#hypothesis 11- Sale price depends on ExterQual, ExterCond
#hypothesis 12- Sale price depends on BsmtQual, BsmtCond
#hypothesis 13- Sale price depends on TotalBSMTSF
#hypothesis 14- Sale price depends on HeatingQC, CentralAir
#hypothesis 15- Sale price depends on 1stFlrSF, 2nd
#hypothesis 16- Sale price depends on LowQualSF
#hypothesis 17- Sale price depends on baths...
#hypothesis 18- Sale price depends on Bedroom, Kitchen 
#hypothesis 19- Sale price depends on KitchenQual
#hypothesis 20- Sale price depends on TotRmsAbvGrd
#hypothesis 21- Sale price depends on GarageType, GarageQual, GarageCond 
#hypothesis 22- Sale price depends on GarageCars, GarageArea
#hypothesis 23- Sale price depends on WoodDeck to PoolArea
#hypothesis 24- Sale price depends on PoolQual, Fence
#hypothesis 25- Sale price depends on MiscFeature, MiscVal
#hypothesis 26- Sale price depends on YrSold 
#hypothesis 27- Sale price depends on SaleType, SaleCond 

#binding both data sets
test$SalePrice <- NA
full <- rbind(train,test)

#printing out number of na values in each column 
sapply(full, function(x)(sum(is.na(x))))

#filling in missing/na values for:
#MSZoning
#printing the column
full$MSZoning

#printing a table of all possible values
table(full$MSZoning)
#printing percentages
table(full$MSZoning)/2919

#counting number of na values
sum(is.na(full$MSZoning))   #4 na values

#since most values are "RL" (~78%) and "RM" (~16%) I think it's safe to assume 3 to be "RL" 
#and 1 to be "RM"

#print rows which have NA and compare with rows which have "RL"
full[is.na(full$MSZoning),]
zoning_rl <- full[full$MSZoning=="RL",]

#looking at another variable
table(zoning_rl$MSSubClass)
table(full$MSSubClass)

#since most values are "RL" (~78%) and "RM" (~16%) I think it's safe to assume 3 to be "RL" 
#and 1 to be "RM"
full$MSZoning[1916] <- "RL"
full$MSZoning[2217] <- "RL"
full$MSZoning[2251] <- "RM"
full$MSZoning[2905] <- "RL"

#counting number of na values
sum(is.na(full$MSZoning))   #0 na values now

#LotFrontage
#printing the column
full$LotFrontage

#printing a table of all possible values
table(full$LotFrontage)
#printing percentages
table(full$LotFrontage)/2919
#printing max percentage to figure out most common value
max(table(full$LotFrontage)/2919)   #max is less than 10% 

#counting number of na values
sum(is.na(full$LotFrontage))   #486 na values

#printing out number of na values in each column 
sapply(full, function(x)(sum(is.na(x)))/2919)   #16% is na values for this variable

#best thing to predict na values is probably to find mean/median
#plotting graph
library(ggplot2)
ggplot(data=full, aes(x= LotFrontage))+geom_histogram()
#mean looks to be around 60-70 units

#calculating mean and median to determine what value to substitute
?sum
?nrow
mean_lot <- sum(full$LotFrontage, na.rm = TRUE)/(2919-486)
?median
median_lot <- median(full$LotFrontage, na.rm = TRUE)
#only difference of 1 around mean and median

#i'll choose to substitute na values with the rounded mean (69)
#this is probably not the best method
full$LotFrontage[is.na(full$LotFrontage)] <- 69

#counting number of na values
sum(is.na(full$LotFrontage))   #0 na values

#Alley
#printing the column
full$Alley

#printing a table of all possible values
table(full$Alley)
#printing percentages
table(full$Alley)/2919

#counting number of na values
sum(is.na(full$Alley))   #2721 na values

#printing out number of na values in each column 
sapply(full, function(x)(sum(is.na(x)))/2919)   #93% is na values for this variable

#na in this case means there is no alley access (assuming all nas mean this)

#i will substitute na values with something else (na values are ignored for most things)
#will substitute NA values with "None"
full$Alley[is.na(full$Alley)] <- "None"

#counting number of na values
sum(is.na(full$Alley))   #0 na values

#Utilities
#printing the column
full$Utilities

#printing a table of all possible values
table(full$Utilities)
#printing percentages
table(full$Utilities)/2919

#counting number of na values
sum(is.na(full$Utilities))   #2 na values

#printing out number of na values in each column 
sapply(full, function(x)(sum(is.na(x)))/2919)   #0.7% is na values for this variable

#since "AllPub" is the most used value, the 2 nas will be substituted by allpub
full$Utilities[is.na(full$Utilities)] <- "AllPub"

#Exterior1st
#printing the column
full$Exterior1st

#printing a table of all possible values
table(full$Exterior1st)
#printing percentages
table(full$Exterior1st)/2919
#printing max percentage to figure out most common value
max(table(full$Exterior1st)/2919)   #max is ~35%

#counting number of na values
sum(is.na(full$Exterior1st))   #1 na value

#printing out number of na values in each column 
sapply(full, function(x)(sum(is.na(x)))/2919)   #0.3% is na values for this variable

#since "VinylSd" is the most used value, the 1 na will be substituted by "VinylSd"
#probably not the most accurate assumption but 1 value won't change much
full$Exterior1st[is.na(full$Exterior1st)] <- "VinylSd"

#counting number of na values
sum(is.na(full$Exterior1st))   #0 na value

#Exterior2nd
#printing the column
full$Exterior2nd

#printing a table of all possible values
table(full$Exterior2nd)
#printing percentages
table(full$Exterior2nd)/2919
#printing max percentage to figure out most common value
max(table(full$Exterior2nd)/2919)   #max is ~35%

#counting number of na values
sum(is.na(full$Exterior2nd))   #1 na value

#printing out number of na values in each column 
sapply(full, function(x)(sum(is.na(x)))/2919)   #0.3% is na values for this variable

#since "VinylSd" is the most used value, the 1 na will be substituted by "VinylSd"
#probably not the most accurate assumption but 1 value won't change much
full$Exterior2nd[is.na(full$Exterior2nd)] <- "VinylSd"

#counting number of na values
sum(is.na(full$Exterior2nd))   #0 na value

#MasVnrType
#printing the column
full$MasVnrType

#printing a table of all possible values
table(full$MasVnrType)
#printing percentages
table(full$MasVnrType)/2919

#counting number of na values
sum(is.na(full$MasVnrType))   #24 na values

24/2919 #about .8%

#printing out number of na values in each column 
sapply(full, function(x)(sum(is.na(x)))/2919)   

#i'll substitute 8 values with "BrkFace" and 16 with "None"
#print rows which have NA and compare with rows which have "RL"
#variable to store indices of na values for MasVnrType
na_MasVnrType <- full$Id[is.na(full$MasVnrType)]

#randomly picked 8 indices
full$MasVnrType[235] <- "BrkFace"
full$MasVnrType[1279] <- "BrkFace"
full$MasVnrType[2005] <- "BrkFace"
full$MasVnrType[2350] <- "BrkFace"
full$MasVnrType[2611] <- "BrkFace"
full$MasVnrType[937] <- "BrkFace"
full$MasVnrType[1883] <- "BrkFace"
full$MasVnrType[651] <- "BrkFace"
full$MasVnrType[is.na(full$MasVnrType)] <- "None"

#counting number of na values
sum(is.na(full$MasVnrType))   #0 na value

#MasVnrArea
#printing the column
full$MasVnrArea

#printing a table of all possible values
table(full$MasVnrArea)
#printing percentages
table(full$MasVnrArea)/2919 #0 has ~60%

#counting number of na values
sum(is.na(full$MasVnrArea))   #23 na values

23/2919 #about .8%

#printing out number of na values in each column 
sapply(full, function(x)(sum(is.na(x)))/2919)   

#best thing to predict na values is probably to find mean/median
#plotting graph
library(ggplot2)
ggplot(data=full, aes(x= MasVnrArea))+geom_histogram()

#calculating mean and median to determine what value to substitute
?sum
?nrow
mean_masVnr <- sum(full$MasVnrArea, na.rm = TRUE)/(2919-23)
?median
median_masVnr <- median(full$MasVnrArea, na.rm = TRUE)
#mean= 102 and median=0

#gonna find mean and median of "BrkFace" type
#resetting values of masVnrArea in full (coz i made a mistake)
full$MasVnrArea <- full_backup$MasVnrArea
masVnrType_Brk <- full[full$MasVnrType=="BrkFace",]
nrow(masVnrType_Brk)  #887 rows
?mean
mean_brk <- mean(masVnrType_Brk$MasVnrArea, na.rm=TRUE)
median_brk <-median(masVnrType_Brk$MasVnrArea, na.rm=TRUE)
#mean is 261.6 while median is 203 for brk type

#if MasVnrType= none then MasVnrArea=0 too
#variable to store indices of na values for MasVnrType
na_MasVnrArea <- full[is.na(full$MasVnrArea),]

#randomly picked 8 indices ans substitute rounded mean with it
full$MasVnrArea[235] <- 262
full$MasVnrArea[1279] <- 262
full$MasVnrArea[2005] <- 262
full$MasVnrArea[2350] <- 262
full$MasVnrArea[2611] <- 262
full$MasVnrArea[937] <- 262
full$MasVnrArea[1883] <- 262
full$MasVnrArea[651] <- 262
full$MasVnrArea[is.na(full$MasVnrArea)] <- 0

#counting number of na values
sum(is.na(full$MasVnrArea))   #0 na value

#BsmtQual
#printing the column
full$BsmtQual

#printing a table of all possible values
table(full$BsmtQual)
#printing percentages
table(full$BsmtQual)/2919 #Gd and TA both have above 40%

#counting number of na values
sum(is.na(full$BsmtQual))   #81 na values

81/2919 #about 2.8%

#in this case na means no basement
#will change value of na to something else too

#BsmtCond
#printing the column
full$BsmtCond

#printing a table of all possible values
table(full$BsmtCond)
#printing percentages
table(full$BsmtCond)/2919 #Gd and TA both have above 40%

#counting number of na values
sum(is.na(full$BsmtCond))   #82 na values

82/2919 #about 2.8%

#in this case na means no basement
#might change value of na to something else too

#BsmtExposure
#printing the column
full$BsmtExposure

#printing a table of all possible values
table(full$BsmtExposure)
#printing percentages
table(full$BsmtExposure)/2919 #Gd and TA both have above 40%

#counting number of na values
sum(is.na(full$BsmtExposure))   #82 na values

82/2919 #about 2.8%

#in this case na means no basement
#might change value of na to something else too

#BsmtFinType1
#printing the column
full$BsmtFinType1

#printing a table of all possible values
table(full$BsmtFinType1)
#printing percentages
table(full$BsmtFinType1)/2919 #Gd and TA both have above 40%

#counting number of na values
sum(is.na(full$BsmtFinType1))   #79 na values

79/2919 #about 2.8%

#in this case na means no basement
#might change value of na to something else too
#slightly different number of na values so might need to substitute a bit

#BsmtFinSF1
#printing the column
full$BsmtFinSF1

#printing a table of all possible values
table(full$BsmtFinSF1)
#printing percentages
table(full$BsmtFinSF1)/2919 #Gd and TA both have above 40%

#counting number of na values
sum(is.na(full$BsmtFinSF1))   #1 na value

#best thing to predict na values is probably to find mean/median
#plotting graph
library(ggplot2)
ggplot(data=full, aes(x= BsmtFinSF1))+geom_histogram()
#mean looks to be around 500ish units

#calculating mean and median to determine what value to substitute
?sum
?nrow
mean_bsmtFin <- sum(full$BsmtFinSF1, na.rm = TRUE)/(2919-1)
?median
median_bsmtFin <- median(full$BsmtFinSF1, na.rm = TRUE)
#mean= 441 and median=368.5

#if BsmtFinType1= na then BsmtFinSF1=0
full_backup <- rbind(train,test)
index<- full_backup$Id[is.na(full_backup$BsmtFinSF1)]
full[index,"BsmtFinType1"]  #value is indeed na
full$BsmtFinSF1[2121] <- 0

#counting number of na values
sum(is.na(full$BsmtFinSF1))   #0 na value

#BsmtFinType2
#printing the column
full$BsmtFinType2

#printing a table of all possible values
table(full$BsmtFinType2)
#printing percentages
table(full$BsmtFinType2)/2919 #Gd and TA both have above 40%

#counting number of na values
sum(is.na(full$BsmtFinType2))   #80 na values

80/2919 #about 2.8%

#in this case na means no basement
#might change value of na to something else too
#slightly different number of na values so might need to substitute a bit

#BsmtFinSF2
#printing the column
full$BsmtFinSF2

#printing a table of all possible values
table(full$BsmtFinSF2)
#printing percentages
table(full$BsmtFinSF2)/2919 #Gd and TA both have above 40%

#counting number of na values
sum(is.na(full$BsmtFinSF2))   #1 na value

#best thing to predict na values is probably to find mean/median
#plotting graph
#library(ggplot2)
#ggplot(data=full, aes(x= BsmtFinSF1))+geom_histogram()
#mean looks to be around 500ish units

#calculating mean and median to determine what value to substitute
#?sum
#?nrow
#mean_bsmtFin <- sum(full$BsmtFinSF1, na.rm = TRUE)/(2919-1)
#?median
#median_bsmtFin <- median(full$BsmtFinSF1, na.rm = TRUE)
#mean= 441 and median=368.5

#if BsmtFinType2= na then BsmtFinSF2=0
index<- full$Id[is.na(full$BsmtFinSF2)]
full[index,"BsmtFinType2"]  #value is indeed na
full$BsmtFinSF2[2121] <- 0

#counting number of na values
sum(is.na(full$BsmtFinSF2))   #0 na value

#have to make sure same ids have na values for all bsmt-qualities
NaBsmtQual <- full[is.na(full$BsmtQual),1]
NaBsmtCond <- full[is.na(full$BsmtCond),1]

#2041, #2186, 2525 id is there in NaBsmtCond but not in NaBsmtQual
#2218,2219 vice versa
NaBsmtExposure <- full[is.na(full$BsmtExposure),1]

#checking if 3rd int array(NaBsmtExposure) has same elements as 2nd one
NaBsmtCond == NaBsmtExposure
#949,1488,2349 in NaBsmtExposure but not in NaBsmtCond
#2041,2186,2525 vice versa
#949,1488,2349 in NaBsmtExposure but not NaBsmtQual
#2218,2219 vice-versa

NaBsmtFinType1 <- full[is.na(full$BsmtFinType1),1]
#949,1488,2349 in NaBsmtExposure but not NaBsmtFinType1
#2041,2186,2525 in NaBsmtCond but not NaBsmtFinType1
#2218,2219 in NaBsmtQual but not NaBsmtFinType1

NaBsmtFinType2 <- full[is.na(full$BsmtFinType2),1]
#333 in NaBsmtFinType2 but not NaBsmtFinType1
#333 in NaBsmtFinType2 but not NaBsmtExposure
#949,1488,2349 vice-versa
#333 in NaBsmtFinType2 but not NaBsmtCond
#2041,2186,2525 vice versa
#333 in NaBsmtFinType2 but not NaBsmtQual
#2218,2219 vice versa

#checking bsmt-qualities of Id #333
full$BsmtQual[333]  #Good and not NA as expected
full$BsmtCond[333]  #TA (Typical)
full$BsmtExposure[333]  #No exposure
full$BsmtFinType1[333]  #GLQ (Good living quarters)
full$BsmtFinType2[333]  #NA which shouldn't be NA since BsmtFinSF2 is not 0

table(full$BsmtFinType2)
table(full$BsmtFinType2)/2919 #85% is "Unf"
#since it is only 1 value i can assume "Unf" in this case
full$BsmtFinType2[333] <- "Unf"

sum(is.na(full$BsmtFinType2))

#checking bsmt-qualities of Id #949, 1488, 2349, 2041, 2186, 2525, 2218, 2219
full$BsmtQual[949]  #Good and not NA as expected
full$BsmtCond[949]  #TA (Typical)
full$BsmtExposure[949]  #NA which should be something else
full$BsmtFinType1[949]  #Unf
full$BsmtFinType2[949]  #Unf

full$BsmtQual[1488]  #Good and not NA as expected
full$BsmtCond[1488]  #TA (Typical)
full$BsmtExposure[1488]  #NA which should be something else
full$BsmtFinType1[1488]  #Unf
full$BsmtFinType2[1488]  #Unf

full$BsmtQual[2349]  #Good and not NA as expected
full$BsmtCond[2349]  #TA (Typical)
full$BsmtExposure[2349]  #NA which should be something else
full$BsmtFinType1[2349]  #Unf
full$BsmtFinType2[2349]  #Unf

table(full$BsmtExposure)
table(full$BsmtExposure)/2919 #65% is "No", 14% is "Av" etc

#gonna find out most common BsmtExposure value for unfinished BsmtFinType1 and BsmtFinType2
table(full$BsmtExposure[full$BsmtFinType1=="Unf"])
table(full$BsmtExposure[full$BsmtFinType1=="Unf"])/(686+57+22+83)
#81% for no

table(full$BsmtExposure[full$BsmtFinType2=="Unf"])
table(full$BsmtExposure[full$BsmtFinType2=="Unf"])/(1698+198+233+362)
#68% for no

table(full$BsmtExposure[full$BsmtQual=="Gd"])
table(full$BsmtExposure[full$BsmtQual=="Gd"])/(694+112+139+261)
#57.5% for no, 21.6 for "Av"

table(full$BsmtExposure[full$BsmtCond=="TA"])
table(full$BsmtExposure[full$BsmtCond=="TA"])/(1743+216+251+393)
#67% for no, 15% for "Av"

table(full$BsmtExposure[full$BsmtFinType2=="Unf" & full$BsmtFinType1=="Unf" & full$BsmtCond=="TA" & full$BsmtQual=="Gd"]) 
table(full$BsmtExposure[full$BsmtFinType2=="Unf" & full$BsmtFinType1=="Unf" & full$BsmtCond=="TA" & full$BsmtQual=="Gd"])/(255+22+10+58)
#73.9% no, 16.8% Av

#since it is only 3 values i can assume "No" in all cases for simplicity
full$BsmtExposure[949] <- "No"
full$BsmtExposure[1488] <- "No"
full$BsmtExposure[2349] <- "No"

sum(is.na(full$BsmtExposure))

#checking bsmt-qualities of Id #2041, 2186, 2525, 2218, 2219
full$BsmtQual[2041]  #Good and not NA as expected
full$BsmtCond[2041]  #NA which should be something else
full$BsmtExposure[2041]  #Mn
full$BsmtFinType1[2041]  #GLQ
full$BsmtFinType2[2041]  #Rec

full$BsmtQual[2186]  #TA
full$BsmtCond[2186]  #NA
full$BsmtExposure[2186]  #No
full$BsmtFinType1[2186]  #BLQ
full$BsmtFinType2[2186]  #Unf

full$BsmtQual[2525]  #TA
full$BsmtCond[2525]  #NA
full$BsmtExposure[2525]  #Av
full$BsmtFinType1[2525]  #ALQ
full$BsmtFinType2[2525]  #Unf

table(full$BsmtCond)
table(full$BsmtCond)/2919 #89% is "TA"

#since it is only 3 values i can assume "TA" in all cases for simplicity
full$BsmtCond[2041] <- "TA"
full$BsmtCond[2186] <- "TA"
full$BsmtCond[2525] <- "TA"

sum(is.na(full$BsmtCond))

#checking bsmt-qualities of Id #2218, 2219
full$BsmtQual[2218]  #NA
full$BsmtCond[2218]  #Fa
full$BsmtExposure[2218]  #No
full$BsmtFinType1[2218]  #Unf
full$BsmtFinType2[2218]  #Unf

full$BsmtQual[2219]  #NA
full$BsmtCond[2219]  #TA
full$BsmtExposure[2219]  #No
full$BsmtFinType1[2219]  #Unf
full$BsmtFinType2[2219]  #Unf

table(full$BsmtQual)
table(full$BsmtQual)/2919 #44% is "TA", 41% is Gd

table(full$BsmtQual[full$BsmtFinType2=="Unf" & full$BsmtFinType1=="Unf" & full$BsmtExposure=="No"]) 
table(full$BsmtQual[full$BsmtFinType2=="Unf" & full$BsmtFinType1=="Unf" & full$BsmtExposure=="No"])/(26+54+269+338)
#49% TA, 39% Gd

table(full$BsmtQual[full$BsmtFinType2=="Unf" & full$BsmtFinType1=="Unf" & full$BsmtExposure=="No" & full$BsmtCond=="Fa"])
table(full$BsmtQual[full$BsmtFinType2=="Unf" & full$BsmtFinType1=="Unf" & full$BsmtExposure=="No" & full$BsmtCond=="Fa"])/(13+2+31)
#67% TA

table(full$BsmtQual[full$BsmtFinType2=="Unf" & full$BsmtFinType1=="Unf" & full$BsmtExposure=="No" & full$BsmtCond=="TA"])
table(full$BsmtQual[full$BsmtFinType2=="Unf" & full$BsmtFinType1=="Unf" & full$BsmtExposure=="No" & full$BsmtCond=="TA"])/(20+38+258+304)
#49% TA, 42% Gd

#since it is only 2 values i can assume "TA" in 1 case, "Gd" in 1 case  for simplicity
full$BsmtQual[2218] <- "TA"
full$BsmtQual[2219] <- "Gd"

sum(is.na(full$BsmtQual))

#comparing na values again
NaBsmtQual <- full[is.na(full$BsmtQual),1]
NaBsmtCond <- full[is.na(full$BsmtCond),1]
NaBsmtExposure <- full[is.na(full$BsmtExposure),1]
NaBsmtFinType1 <- full[is.na(full$BsmtFinType1),1]
NaBsmtFinType2 <- full[is.na(full$BsmtFinType2),1]

#checking if arrays are equal
NaBsmtQual==NaBsmtCond
NaBsmtQual==NaBsmtExposure
NaBsmtQual==NaBsmtFinType1
NaBsmtQual==NaBsmtFinType2
#all of na ids match now

#replacing na values with others
full$BsmtQual[is.na(full$BsmtQual)] <- "None"
full$BsmtCond[is.na(full$BsmtCond)] <- "None"
full$BsmtExposure[is.na(full$BsmtExposure)] <- "None"
full$BsmtFinType1[is.na(full$BsmtFinType1)] <- "None"
full$BsmtFinType2[is.na(full$BsmtFinType2)] <- "None"

#printing out number of na values in each column 
sapply(full, function(x)(sum(is.na(x))))

#BsmtUnfSF
#printing the column
full$BsmtUnfSF

#printing a table of all possible values
table(full$BsmtUnfSF)
#printing percentages
table(full$BsmtUnfSF)/2919 

#counting number of na values
sum(is.na(full$BsmtUnfSF))   #1 na value

#if there is no basement then BsmtUnfSF=0
index<- full$Id[is.na(full$BsmtUnfSF)]
full[index,]
full[index,"BsmtQual"]  #value is na
full$BsmtUnfSF[2121] <- 0

#counting number of na values
sum(is.na(full$BsmtUnfSF))   #0 na value

#TotalBsmtSF
#printing the column
full$TotalBsmtSF

#printing a table of all possible values
table(full$TotalBsmtSF)
#printing percentages
table(full$TotalBsmtSF)/2919 

#counting number of na values
sum(is.na(full$TotalBsmtSF))   #1 na value

#if there is no basement then BsmtUnfSF=0
index<- full$Id[is.na(full$TotalBsmtSF)]
full[index,]
full[index,"BsmtQual"]  #value is na
full$TotalBsmtSF[2121] <- 0

#counting number of na values
sum(is.na(full$TotalBsmtSF))   #0 na value

#Electrical
#printing the column
full$Electrical

#printing a table of all possible values
table(full$Electrical)
#printing percentages
table(full$Electrical)/2919 #sbrkr has 91.5%

#counting number of na values
sum(is.na(full$Electrical))   #1 na value

#printing out number of na values in each column 
sapply(full, function(x)(sum(is.na(x)))/2919)   

#variable to store index of na values for Electrical
index <- full$Id[is.na(full$Electrical)]

#since it is only 1 value i'll assume it is SBrkr	
full[index,]
full$Electrical[is.na(full$Electrical)] <- "SBrkr"

#counting number of na values
sum(is.na(full$Electrical))   #0 na value

#BsmtFullBath
#printing the column
full$BsmtFullBath

#printing a table of all possible values
table(full$BsmtFullBath)
#printing percentages
table(full$BsmtFullBath)/2919 #0 has 58% and 1 has 40%

#counting number of na values
sum(is.na(full$BsmtFullBath))   #2 na values

#if there is no basement then BsmtUnfSF=0
index<- full$Id[is.na(full$BsmtFullBath)]
full[index,]
full[index,"BsmtQual"] #values are na
full$BsmtFullBath[2121] <- 0
full$BsmtFullBath[2189] <- 0

#counting number of na values
sum(is.na(full$BsmtFullBath))   #0 na value

#BsmtHalfBath
#printing the column
full$BsmtHalfBath

#printing a table of all possible values
table(full$BsmtHalfBath)
#printing percentages
table(full$BsmtHalfBath)/2919 #0 has 94%

#counting number of na values
sum(is.na(full$BsmtHalfBath))   #2 na values

#if there is no basement then BsmtUnfSF=0
index<- full$Id[is.na(full$BsmtHalfBath)] #same na values as last one
full[index,]
full[index,"BsmtQual"]  #values are na
full$BsmtHalfBath[index] <- 0

#counting number of na values
sum(is.na(full$BsmtHalfBath))   #0 na value

#KitchenQual
#printing the column
full$KitchenQual

#printing a table of all possible values
table(full$KitchenQual)
#printing percentages
table(full$KitchenQual)/2919 #ta has 51% and Gd has 39%

#counting number of na values
sum(is.na(full$KitchenQual))   #1 na value

#printing out number of na values in each column 
sapply(full, function(x)(sum(is.na(x)))/2919)   

#variable to store index of na values for Electrical
index <- full$Id[is.na(full$KitchenQual)]

#if kitchen=0 then quality=poor
full$KitchenAbvGr[index]  #seems to have 1 kitchen
full[index,]

#other qual values (of that index) range between good and fair so i'll do ta (typical/average)	
full$KitchenQual[is.na(full$KitchenQual)] <- "TA"

#counting number of na values
sum(is.na(full$KitchenQual))   #0 na value

#Functional
#printing the column
full$Functional

#printing a table of all possible values
table(full$Functional)
#printing percentages
table(full$Functional)/2919 #typ has 93%

#counting number of na values
sum(is.na(full$Functional))   #2 na values

#printing out number of na values in each column 
sapply(full, function(x)(sum(is.na(x)))/2919)   

#variable to store index of na values for Electrical
index <- full$Id[is.na(full$Functional)]

#says to assume typ in data_description (unless deductions are warranted)
full[index,]

#salecondition is abnormal for both (that might be linked)
#since it's only 2 na values i'll assume typ	
full$Functional[is.na(full$Functional)] <- "Typ"

#counting number of na values
sum(is.na(full$Functional))   #0 na value

#FireplaceQu
#printing the column
full$FireplaceQu

#printing a table of all possible values
table(full$FireplaceQu)
#printing percentages
table(full$FireplaceQu)/2919 

#counting number of na values
sum(is.na(full$FireplaceQu))   #1420 na values

#in this case na means no fireplace
#might change value of na to something else too

#check if number of Fireplaces==0 is 1420
table(full$Fireplaces)  #yes it is 

full$Fireplaces[is.na(full$FireplaceQu)] != 0

#replacing it with "None"
full$FireplaceQu[is.na(full$FireplaceQu)] <- "None"

#counting number of na values
sum(is.na(full$FireplaceQu))   #0 na value

#GarageType
#printing the column
full$GarageType

#printing a table of all possible values
table(full$GarageType)
#printing percentages
table(full$GarageType)/2919 

#counting number of na values
sum(is.na(full$GarageType))   #157 na values

#in this case na means no garage
#might change value of na to something else too
#should check if other garage-related variables also have 157nas

#GarageYrBlt
#printing the column
full$GarageYrBlt

#printing a table of all possible values
table(full$GarageYrBlt)
#printing percentages
table(full$GarageYrBlt)/2919 

#counting number of na values
sum(is.na(full$GarageYrBlt))   #159 na values

#in this case na means no garage
#might change value of na to something else too
#2 more nas than previous one which means 2 values need to have yrbuilt which dont

#GarageFinish
#printing the column
full$GarageFinish

#printing a table of all possible values
table(full$GarageFinish)
#printing percentages
table(full$GarageFinish)/2919 

#counting number of na values
sum(is.na(full$GarageFinish))   #159 na values

#in this case na means no garage
#might change value of na to something else too
#2 more nas than previous one which means 2 values need to have garagefinish which dont

#GarageCars
#printing the column
full$GarageCars

#printing a table of all possible values
table(full$GarageCars)
#printing percentages
table(full$GarageCars)/2919 

#counting number of na values
sum(is.na(full$GarageCars))   #1 na value

#if garagetype and garagefinish==na then no. of cars=0
index <- full$Id[is.na(full$GarageCars)]
full[2577,]

#garagetype is detached but every other value=na 
#gotta find more values like this and then substitute other values

#GarageArea
#printing the column
full$GarageArea

#printing a table of all possible values
table(full$GarageArea)
#printing percentages
table(full$GarageArea)/2919 

#counting number of na values
sum(is.na(full$GarageArea))   #1 na value

#if garagetype and garagefinish==na then no. of cars=0
index <- full$Id[is.na(full$GarageArea)]
full[2577,]

#garagetype is detached but every other value=na 
#same index as before
#gotta find more values like this and then substitute other values

#GarageQual
#printing the column
full$GarageQual

#printing a table of all possible values
table(full$GarageQual)
#printing percentages
table(full$GarageQual)/2919 

#counting number of na values
sum(is.na(full$GarageQual))   #159 na values

#in this case na means no garage
#might change value of na to something else too
#2 more nas than previous one which means 2 values need to have garagefinish which dont

#GarageCond
#printing the column
full$GarageCond

#printing a table of all possible values
table(full$GarageCond)
#printing percentages
table(full$GarageCond)/2919 

#counting number of na values
sum(is.na(full$GarageCond))   #159 na values

#in this case na means no garage
#might change value of na to something else too
#2 more nas than previous one which means 2 values need to have garagefinish which dont

#have to make sure same ids have na values for all bsmt-qualities
NaGarageType <- full[is.na(full$GarageType),1]
NaGarageYrBlt <- full[is.na(full$GarageYrBlt),1]

#2127,2577 in 2nd but not 1st

NaGarageFinish <- full[is.na(full$GarageFinish),1]

#checking if 3rd int array(NaBsmtExposure) has same elements as 2nd one
NaGarageFinish == NaGarageYrBlt
#3rd is same as 2nd
#2127,2577 in 3rd but not 2nd

NaGarageQual <- full[is.na(full$GarageQual),1]

#checking if 4th int array(NaBsmtExposure) has same elements as 3rd one
NaGarageQual == NaGarageYrBlt
#4th is same as 3rd
#2127,2577 in 4th but not 1st

NaGarageCond <- full[is.na(full$GarageCond),1]
#checking if 4th int array(NaBsmtExposure) has same elements as 3rd one
NaGarageCond == NaGarageYrBlt
#5th is same as 4th
#2127,2577 in 5th but not 1st

#checking garage-qualities of Id #2127,2577
full$GarageType[2127]  #Detchd
full$GarageYrBlt[2127]  #NA
full$GarageFinish[2127]  #NA
full$GarageQual[2127]  #NA
full$GarageCond[2127]  #NA

full$GarageType[2577]  #Detchd
full$GarageYrBlt[2577]  #NA
full$GarageFinish[2577]  #NA
full$GarageQual[2577]  #NA
full$GarageCond[2577]  #NA

#display columns 60,61,64,65
table(full[full$GarageType=="Detchd",61])
table(full[full$GarageType=="Detchd",61])/(24+34+719)
#92% Unf so will assume both are Unf

full$GarageFinish[2127] <- "Unf"
full$GarageFinish[2577] <- "Unf"

sum(is.na(full$GarageFinish)) #157 na values

NaGarageFinish <- full[is.na(full$GarageFinish),1]
#checking if 3rd int array(NaBsmtExposure) has same elements as 1st one
NaGarageFinish == NaGarageType

#display columns 60,64,65
table(full[full$GarageType=="Detchd",64])
table(full[full$GarageType=="Detchd",64])/(3+97+5+5+667)
#86% TA so will assume both are TA

full$GarageQual[2127] <- "TA"
full$GarageQual[2577] <- "TA"

sum(is.na(full$GarageQual)) #157 na values

NaGarageQual <- full[is.na(full$GarageQual),1]
#checking if 4th int array(NaBsmtExposure) has same elements as 1st one
NaGarageQual == NaGarageType

#display columns 60,65
table(full[full$GarageType=="Detchd",65])
table(full[full$GarageType=="Detchd",65])/(3+67+3+12+692)
#89% TA so will assume both are TA

full$GarageCond[2127] <- "TA"
full$GarageCond[2577] <- "TA"

sum(is.na(full$GarageCond)) #157 na values

NaGarageCond <- full[is.na(full$GarageCond),1]
#checking if 5th int array(NaBsmtExposure) has same elements as 1st one
NaGarageCond == NaGarageType

#display columns 60
table(full[full$GarageType=="Detchd",60])
table(full[full$GarageType=="Detchd",60])/(sum(full$GarageType=="Detchd",na.rm = TRUE))
max(table(full[full$GarageType=="Detchd",60])/(sum(full$GarageType=="Detchd",na.rm = TRUE)))
#max is 3.7% so avg or median might be better method

#finding mean and median
mean_yr <- sum(full[full$GarageType=="Detchd",60], na.rm = TRUE)/(sum(full$GarageType=="Detchd",na.rm = TRUE))
median_yr <- median(full[full$GarageType=="Detchd",60], na.rm = TRUE)

#mean is 1956 and median is 1962- pretty close
#i'll substitute it with mean

full$GarageYrBlt[2127] <- 1956
full$GarageYrBlt[2577] <- 1956

sum(is.na(full$GarageYrBlt)) #157 na values

NaGarageYrBlt <- full[is.na(full$GarageYrBlt),1]
#checking if 5th int array(NaBsmtExposure) has same elements as 1st one
NaGarageYrBlt == NaGarageType

#checking if arrays are equal
NaGarageType==NaGarageFinish
NaGarageType==NaGarageQual
NaGarageType==NaGarageCond
NaGarageType==NaGarageYrBlt
#all of na ids match now

#replacing na values with others
full$GarageType[is.na(full$GarageType)] <- "None"
full$GarageFinish[is.na(full$GarageFinish)] <- "None"
full$GarageQual[is.na(full$GarageQual)] <- "None"
full$GarageCond[is.na(full$GarageCond)] <- "None"
full$GarageYrBlt[is.na(full$GarageYrBlt)] <- 0

#printing out number of na values in each column 
sapply(full, function(x)(sum(is.na(x))))

#check if GarageCars and GarageArea have same na values
index <- full$Id[is.na(full$GarageCars)]

full[2577,]
#same na values

#display both variables in table form
table(full$GarageCars)  #max is 2 then 1 then 3
table(full$GarageCars)/(157+776+1594+374+16+1 )
#55% 2, 26% 1, 13% 3

#maybe area will help- more area=more cars
table(full$GarageArea)  #max is 2 then 1 then 3
table(full$GarageArea)/(157+776+1594+374+16+1 )
#finding mean and median will be better in this case

#finding mean and median
mean_yr <- mean(full$GarageArea, na.rm=TRUE)
median_yr <- median(full$GarageArea, na.rm = TRUE)
#mean is 473 and median is 480

#finding number of cars for ~480 area
full$GarageCars[full$GarageArea==480]
table(full$GarageCars[full$GarageArea==480])
#51 have 2 cars, 2 have 1 car, 1 has 4 cars
table(full$GarageCars[full$GarageArea==480])/(51+2+1)
#94% have 2 cars so safe to assume 2 cars

#will verify with area around 470

#finding number of cars for ~470 area
full$GarageCars[full$GarageArea>=470 & full$GarageArea<=480]
table(full$GarageCars[full$GarageArea>=470 & full$GarageArea<=480])
#118 have 2 cars, 2 have 1 car, 1 has 3 cars, 1 has 4 cars
table(full$GarageCars[full$GarageArea>=470 & full$GarageArea<=480])/(118+2+1+1)
#97% have 2 cars so safe to assume 2 cars

full$GarageCars[2577] <- 2

#counting number of na values
sum(is.na(full$GarageCars))   #0 na value

#i'll assume median of 480 for area

full$GarageArea[2577] <- 480

#counting number of na values
sum(is.na(full$GarageArea))   #0 na value

#PoolQC
#printing the column
full$PoolQC

#printing a table of all possible values
table(full$PoolQC)
#printing percentages
table(full$PoolQC)/2919 

#counting number of na values
sum(is.na(full$PoolQC))   #2909 na values

#in this case na means no pool
#will change value of na to something else too

#check if 2909 rows have poolarea=0
table(full$PoolArea)  #2906 do which means 3 should not have na values
condition<- (full$PoolArea==0) && (is.na(full$PoolQC))
index <- full$Id[condition]

#will find efficient way to do this

# creating variable to store miscfeature=na and miscval=0
NaPoolQC <- full$Id[is.na(full$PoolQC) & full$PoolArea==0]
#2906 values

#since 2906 variables seem to be rightly na & 0 the na can be substituted by none
full$PoolQC[is.na(full$PoolQC) & full$PoolArea==0] <- "None"

#checking what's up with other na value
NaPoolQC <- full$Id[is.na(full$PoolQC)]
#2814 values

#checking values at NaPoolQC indices
full$PoolArea[NaPoolQC]
#can't be none since value is 368, 444, and 561

#table of PoolArea
table(full$PoolArea)
#these values are in lower end
full$PoolQC[full$PoolArea>0]
#not very useful

#printing table of PoolQC
table(full$PoolQC) #most are none, Gd-4, Fa-2, Ex-4 (very few values)

#displaying PoolArea and PoolQC
full[full$PoolArea>0,c(72,73)]

#area less than 368 have Ex and more than 444 has Gd
#hard to predict from just this

#since there are 3 na values and all 3 options are almost-equally likely i'll substitute it with TA
#seems to be the safest option
full$PoolQC[NaPoolQC] <- "TA"

#counting number of na values
sum(is.na(full$PoolQC))   #0 na value

#Fence
#printing the column
full$Fence

#printing a table of all possible values
table(full$Fence)
#printing percentages
table(full$Fence)/2919 

#counting number of na values
sum(is.na(full$Fence))   #2348 na values

#in this case na means no fence
#will change value of na to something else too
#replacing it with "None"
full$Fence[is.na(full$Fence)] <- "None"

#counting number of na values
sum(is.na(full$Fence))   #0 na value

#MiscFeature
#printing the column
full$MiscFeature

#printing a table of all possible values
table(full$MiscFeature)
#printing percentages
table(full$MiscFeature)/2919 

#counting number of na values
sum(is.na(full$MiscFeature))   #2814 na values

#in this case na means none
#might change value of na to something else too

#should check if no of na in miscfeature= 2814
table(full$MiscVal) #has 2816 which is 2 more (2 mnww might be worth 0)

# creating variable to store miscfeature=na and miscval=0
NaMiscFeature <- full$Id[is.na(full$MiscFeature) & full$MiscVal==0]
#2813 values

#since 2813 variables seem to be rightly na & 0 the na can be substituted by none
full$MiscFeature[is.na(full$MiscFeature) & full$MiscVal==0] <- "None"

#checking what's up with other na value
NaMiscFeature2 <- full$Id[is.na(full$MiscFeature)]
#2814 values
NaMiscFeature3 <- full$Id[full$MiscVal==0]
#2816 values

#checking values at NaMiscFeature2 index
full[NaMiscFeature2,]
#can't be none since value is 17000

#table of MiscVal
table(full$MiscVal)
#17000 is the highest- maybe looking at other high values will give hint
full$MiscFeature[full$MiscVal>6000]
#last 5 have 3 Gar2, 1 Othr, 1 NA

#printing table of miscfeature
table(full$MiscFeature) #most are none, then shed

#displaying miscfeature and mscvalue
full[full$MiscVal>5000,c(75,76)]

#since 2nd, 3rd, 4th highest have Gar2 I'll assume this one is Gar2 too
full$MiscFeature[NaMiscFeature2] <- "Gar2"

#counting number of na values
sum(is.na(full$MiscFeature))   #0 na value

#SaleType
#printing the column
full$SaleType

#printing a table of all possible values
table(full$SaleType)
#printing percentages
table(full$SaleType)/2919   #wd is 86.5%

#counting number of na values
sum(is.na(full$SaleType))   #1 na value

#variable to store index of na values for Electrical
index <- full$Id[is.na(full$SaleType)]

#says to assume typ in data_description (unless deductions are warranted)
full[2490,]

#salecondition is abnormal for both (that might be linked)
#since it's only 1 na value i'll assume wd (cant be new since it has been remodelled 
#in a few decades)	
full$SaleType[is.na(full$SaleType)] <- "WD"

#counting number of na values
sum(is.na(full$SaleType))   #0 na value

#printing out number of missing values in each column 
sapply(full, function(x)(sum(x==""))) #none


#1. go through all of it again

str(full)

#printing out number of na values in each column 
sapply(full, function(x)(sum(is.na(x))))
#2. no. of nas in all garage(done), basement (done) etc should be the same
#done with this step finally

#making few graphs
#hypothesis 1- Sale price depends on MSSubClass 
table(full$MSSubClass)
library(ggplot2)
#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(MSSubClass))) + geom_bar()

#, y=SalePrice

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(MSSubClass), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(MSSubClass), y=SalePrice)) + geom_boxplot()

#MSSubClass of 60, 120 etc seem to have more price
#newer seems to have more price

?ggplot

#hypothesis 2- Sale price depends on LotArea 
table(full$LotArea)

#making point graph
ggplot(data=full[1:1460,], aes(x=factor(LotArea), y=SalePrice)) + geom_point()

#directly proportional

#hypothesis 3- Sale price depends on Utilities
table(full$Utilities)
#pointless

#hypothesis 4- Sale price depends on Neighborhood
table(full$Neighborhood)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(Neighborhood))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(Neighborhood), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(Neighborhood), y=SalePrice)) + geom_boxplot()

#can see good amt of disparity

#hypothesis 5- Sale price depends on Condition1, Condition2
table(full$Condition1)
table(full$Condition2)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(Condition1))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(Condition1), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(Condition1), y=SalePrice)) + geom_boxplot()

#a bit of difference can be seen

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(Condition2))) + geom_bar()

#most are in Norm so pointless

#hypothesis 6- Sale price depends on BldgType
table(full$BldgType)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(BldgType))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(BldgType), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(BldgType), y=SalePrice)) + geom_boxplot()

#not too much diff

#hypothesis 7- Sale price depends on HouseStyle 
table(full$HouseStyle)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(HouseStyle))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(HouseStyle), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(HouseStyle), y=SalePrice)) + geom_boxplot()

#some diff

#hypothesis 8- Sale price depends on OverallQual, OverallCond
table(full$OverallQual)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(OverallQual))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(OverallQual), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(OverallQual), y=SalePrice)) + geom_boxplot()

#most difference yet- directly proportional- one of most imp properties!

table(full$OverallCond)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(OverallCond))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(OverallCond), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(OverallCond), y=SalePrice)) + geom_boxplot()

#some diff but not as much as previous one

#hypothesis 9- Sale price depends on YearBuilt, YearRemodAdd
table(full$YearBuilt)

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=YearBuilt, y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(YearBuilt), y=SalePrice)) + geom_boxplot()

#good amt of diff- mostly directly proportional

table(full$YearRemodAdd)

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=YearRemodAdd, y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(YearRemodAdd), y=SalePrice)) + geom_boxplot()

#good amt of diff- mostly directly proportional like previous one

#hypothesis 10- Sale price depends on Exterior1st,2nd
table(full$Exterior1st)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(Exterior1st))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(Exterior1st), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(Exterior1st), y=SalePrice)) + geom_boxplot()

#might be imp too

table(full$Exterior2nd)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(Exterior2nd))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(Exterior2nd), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(Exterior2nd), y=SalePrice)) + geom_boxplot()

#shows abt same difference than previous one

#hypothesis 11- Sale price depends on ExterQual, ExterCond
table(full$ExterQual)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(ExterQual))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(ExterQual), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(ExterQual), y=SalePrice)) + geom_boxplot()

#seems very imp(probably due to low number of factors)!

table(full$ExterCond)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(ExterCond))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(ExterCond), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(ExterQual), y=SalePrice)) + geom_boxplot()

#same as previous one

#hypothesis 12- Sale price depends on BsmtQual, BsmtCond
table(full$BsmtQual)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(BsmtQual))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(BsmtQual), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(BsmtQual), y=SalePrice)) + geom_boxplot()

#might be imp- Ex has more than others

table(full$BsmtCond)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(BsmtCond))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(BsmtCond), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(BsmtCond), y=SalePrice)) + geom_boxplot()

#some diff

#hypothesis 13- Sale price depends on TotalBSMTSF
table(full$TotalBsmtSF)

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(TotalBsmtSF), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(TotalBsmtSF), y=SalePrice)) + geom_boxplot()

#directly proportional

#hypothesis 14- Sale price depends on HeatingQC, CentralAir
table(full$HeatingQC)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(HeatingQC))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(HeatingQC), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(HeatingQC), y=SalePrice)) + geom_boxplot()

#a bit of diff

table(full$CentralAir)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(CentralAir))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(CentralAir), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(CentralAir), y=SalePrice)) + geom_boxplot()

#some diff- might be imp

#hypothesis 15- Sale price depends on 1stFlrSF, 2nd
table(full$X1stFlrSF)

#making point graph and boxplot
#point graph seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(X1stFlrSF), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(X1stFlrSF), y=SalePrice)) + geom_boxplot()

#directly proportional

table(full$X2ndFlrSF)

#making point graph and boxplot
#point graph seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(X2ndFlrSF), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(X2ndFlrSF), y=SalePrice)) + geom_boxplot()

#directly proportional

#hypothesis 16- Sale price depends on LowQualFinSF
table(full$LowQualFinSF)

#not enough to make reliable graph

#hypothesis 17- Sale price depends on baths...
table(full$BsmtFullBath)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(BsmtFullBath))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(BsmtFullBath), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(BsmtFullBath), y=SalePrice)) + geom_boxplot()

#not much diff

table(full$BsmtHalfBath)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(BsmtHalfBath))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(BsmtHalfBath), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(BsmtHalfBath), y=SalePrice)) + geom_boxplot()

#no diff

table(full$FullBath)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(FullBath))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(FullBath), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(FullBath), y=SalePrice)) + geom_boxplot()

#good amt of diff- seems directly prop

table(full$HalfBath)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(HalfBath))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(HalfBath), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(HalfBath), y=SalePrice)) + geom_boxplot()

#some diff

#hypothesis 18- Sale price depends on Bedroom, Kitchen
table(full$BedroomAbvGr)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(BedroomAbvGr))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(BedroomAbvGr), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(BedroomAbvGr), y=SalePrice)) + geom_boxplot()

#a bit of diff

table(full$KitchenAbvGr)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(KitchenAbvGr))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(KitchenAbvGr), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(KitchenAbvGr), y=SalePrice)) + geom_boxplot()

#not enough to compare well but some diff

#hypothesis 19- Sale price depends on KitchenQual
table(full$KitchenQual)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(KitchenQual))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(KitchenQual), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(KitchenQual), y=SalePrice)) + geom_boxplot()

#imp quality!!

#hypothesis 20- Sale price depends on TotRmsAbvGrd
table(full$TotRmsAbvGrd)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(TotRmsAbvGrd))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(TotRmsAbvGrd), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(TotRmsAbvGrd), y=SalePrice)) + geom_boxplot()

#mostly directly proportional

#hypothesis 21- Sale price depends on GarageType, GarageQual, GarageCond
table(full$GarageType)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(GarageType))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(GarageType), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(GarageType), y=SalePrice)) + geom_boxplot()

#some diff

table(full$GarageQual)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(GarageQual))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(GarageQual), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(GarageQual), y=SalePrice)) + geom_boxplot()

#some diff

table(full$GarageCond)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(GarageCond))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(GarageCond), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(GarageCond), y=SalePrice)) + geom_boxplot()

#bit of diff- might not use

#hypothesis 22- Sale price depends on GarageCars, GarageArea
table(full$GarageCars)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(GarageCars))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(GarageCars), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(GarageCars), y=SalePrice)) + geom_boxplot()

#imp- mostly directly proportional

table(full$GarageArea)

#making point graph and boxplot
#point graph seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=GarageArea, y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(GarageArea), y=SalePrice)) + geom_boxplot()

#directly proportional

#hypothesis 23- Sale price depends on WoodDeck to PoolArea
table(full$WoodDeckSF)

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=WoodDeckSF, y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(WoodDeckSF), y=SalePrice)) + geom_boxplot()

#a bit directly prop but not too much

table(full$OpenPorchSF)

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(OpenPorchSF), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(OpenPorchSF), y=SalePrice)) + geom_boxplot()

#not much diff

table(full$EnclosedPorch)

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=EnclosedPorch, y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(EnclosedPorch), y=SalePrice)) + geom_boxplot()

#kinda directly prop

table(full$X3SsnPorch)
#not enough variable data

table(full$ScreenPorch)

#making point graph and boxplot
#point graph seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=ScreenPorch, y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(ScreenPorch), y=SalePrice)) + geom_boxplot()

#not much diff

table(full$PoolArea)
#not enough variables

#hypothesis 24- Sale price depends on PoolQual, Fence
table(full$PoolQC)
#not enough to graph

table(full$Fence)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(Fence))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(Fence), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(Fence), y=SalePrice)) + geom_boxplot()

#a bit of diff

#hypothesis 25- Sale price depends on MiscFeature, MiscVal
table(full$MiscFeature)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(MiscFeature))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(MiscFeature), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(MiscFeature), y=SalePrice)) + geom_boxplot()

#might be imp

#hypothesis 26- Sale price depends on YrSold 
table(full$YrSold)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(YrSold))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(YrSold), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(YrSold), y=SalePrice)) + geom_boxplot()

#no diff

#hypothesis 27- Sale price depends on SaleType, SaleCond 
table(full$SaleType)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(SaleType))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(SaleType), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(SaleType), y=SalePrice)) + geom_boxplot()

#imp

table(full$SaleCondition)

#making simple bar graph
ggplot(data=full[1:1460,], aes(x=factor(SaleCondition))) + geom_bar()

#making point graph and boxplot
#box plot seems to be best for this type of info
ggplot(data=full[1:1460,], aes(x=factor(SaleCondition), y=SalePrice)) + geom_point()
ggplot(data=full[1:1460,], aes(x=factor(SaleCondition), y=SalePrice)) + geom_boxplot()

#might be imp

#1. make some variables into factors
str(full)
full$MSZoning <- as.factor(full$MSZoning)
full$Street <- as.factor(full$Street)
full$Alley <- as.factor(full$Alley)
full$LotShape <- as.factor(full$LotShape)
full$LandContour <- as.factor(full$LandContour)
full$Utilities <- as.factor(full$Utilities)
full$LotConfig <- as.factor(full$LotConfig)
full$LandSlope <- as.factor(full$LandSlope)
full$Condition1 <- as.factor(full$Condition1)
full$Condition2 <- as.factor(full$Condition2)
full$BldgType <- as.factor(full$BldgType)
full$HouseStyle <- as.factor(full$HouseStyle)
full$OverallQual <- as.factor(full$OverallQual)
full$OverallCond <- as.factor(full$OverallCond)
full$RoofStyle <- as.factor(full$RoofStyle)
full$RoofMatl <- as.factor(full$RoofMatl)
full$MasVnrType <- as.factor(full$MasVnrType)
full$ExterQual <- as.factor(full$ExterQual)
full$ExterCond <- as.factor(full$ExterCond)
full$Foundation <- as.factor(full$Foundation)
full$BsmtQual <- as.factor(full$BsmtQual)
full$BsmtCond <- as.factor(full$BsmtCond)
full$BsmtExposure <- as.factor(full$BsmtExposure)
full$BsmtFinType1 <- as.factor(full$BsmtFinType1)
full$BsmtFinType2 <- as.factor(full$BsmtFinType2)
full$Heating <- as.factor(full$Heating)
full$HeatingQC <- as.factor(full$HeatingQC)
full$CentralAir <- as.factor(full$CentralAir)
full$Electrical <- as.factor(full$Electrical)
full$KitchenQual <- as.factor(full$KitchenQual)
full$Functional <- as.factor(full$Functional)
full$FireplaceQu <- as.factor(full$FireplaceQu)
full$GarageType <- as.factor(full$GarageType)
full$GarageFinish <- as.factor(full$GarageFinish)
full$GarageQual <- as.factor(full$GarageQual)
full$GarageCond <- as.factor(full$GarageCond)
full$PavedDrive <- as.factor(full$PavedDrive)
full$PoolQC <- as.factor(full$PoolQC)
full$Fence <- as.factor(full$Fence)
full$MiscFeature <- as.factor(full$MiscFeature)
full$SaleType <- as.factor(full$SaleType)
full$SaleCondition <- as.factor(full$SaleCondition)

#idk if these variables should be made into factors but there's no harm in doing it
full$MSSubClass <- as.factor(full$MSSubClass)
full$Neighborhood <- as.factor(full$Neighborhood)
full$Exterior1st <- as.factor(full$Exterior1st)
full$Exterior2nd <- as.factor(full$Exterior2nd)
full$BsmtFullBath <- as.factor(full$BsmtFullBath)
full$BsmtHalfBath <- as.factor(full$BsmtHalfBath)
full$FullBath <- as.factor(full$FullBath)
full$HalfBath <- as.factor(full$HalfBath)
full$BedroomAbvGr <- as.factor(full$BedroomAbvGr)
full$KitchenAbvGr <- as.factor(full$KitchenAbvGr)
full$TotRmsAbvGrd <- as.factor(full$TotRmsAbvGrd)
full$Fireplaces <- as.factor(full$Fireplaces)
full$GarageCars <- as.factor(full$GarageCars)
full$MoSold <- as.factor(full$MoSold)
full$YrSold <- as.factor(full$YrSold)

str(full)
table(full$YrSold)

#done

#splitting data into test and train
train <- full[1:1460,]
test <- full[1461:2919,]

#2. svm model
library("e1071")

#making an svm model
set.seed(123)
svm_model <- svm(SalePrice ~ ., data = train, scale = FALSE, kernel = "radial", cost = 5)
summary(svm_model)

#plotting model
#idk how to do this yet- error
plot.svm(svm_model)

#making prediction with test data
#predicting for train data instead of test data for some reason
#will try again later
?predict
train_pre_svm<- predict(svm_model, data=test, type="response")
train_pre_svm
table(train_pre_svm)

#not much accuracy but thats expected
mean(train_pre_svm==train$SalePrice)

#converting MasVnrArea from chr to numeric
table(full$MasVnrArea)
full$MasVnrArea <- as.numeric(full$MasVnrArea)

#values of train being predicted only
?predict
svm_prediction<- predict(svm_model, newdata=test[-81], type="response")
#1459 same predictions of 163000
svm_prediction
table(svm_prediction)

#making a submission
svm_solution <- data.frame(test$Id, svm_prediction)

colnames(svm_solution)

#renaming columns
names(svm_solution)[names(svm_solution) == "test.Id"] <- "Id"
names(svm_solution)[names(svm_solution) == "SaleCondition"] <- "SalePrice"

#creating solution file
write.csv(svm_solution, file="svm_solution.csv", row.names = F)

#0.41657- way less than expected 
#will try to reduce number of variables

#will be choosing these factors- MSSubClass, LotArea, Neighborhood, HouseStyle, OverallCond, OverallQual, YearBuilt, YearRemodAdd
#, Exterior1st, Exterior2nd, ExterQual, ExterCond, BsmtQual, TotalBamtSF, CentralAir
#, X1stFlrSF, X2nsFlrSF, FullBath, HalfBath, KitchenQual, TotRmsAbvGrd, GarageType
#, GarageQual, GarageCars, GarageArea, WoodDeck, EnclosedPorch, MiscFeature, SaleType
#, SaleCond

#making an svm model
set.seed(123)
svm_model2 <- svm(SalePrice ~ MSSubClass+ LotArea+ Neighborhood+ HouseStyle+ OverallCond+ OverallQual+ YearBuilt+ YearRemodAdd
                 + Exterior1st+ Exterior2nd+ ExterQual+ ExterCond+ BsmtQual+ TotalBsmtSF+ CentralAir
                 + X1stFlrSF+ X2ndFlrSF+ FullBath+ HalfBath+ KitchenQual+ TotRmsAbvGrd+ GarageType
                 + GarageQual+ GarageCars+ GarageArea+ WoodDeckSF+ EnclosedPorch+ MiscFeature+ SaleType
                 + SaleCondition, data = train)
summary(svm_model2)

#plotting model
#idk how to do this yet- error
plot.svm(svm_model2)

#making prediction with test data
#predicting for train data instead of test data for some reason
#will try again later
?predict
train_pre_svm2<- predict(svm_model2, data=test, type="response")
train_pre_svm2
table(train_pre_svm2)

#not much accuracy but thats expected
mean(train_pre_svm2==train$SalePrice)

#values of train being predicted only
?predict
svm_prediction2<- predict(svm_model2, newdata=test[-81], type="response")
#diff values for each this time
svm_prediction2
table(svm_prediction2)

#making a submission
svm_solution2 <- data.frame(test$Id, svm_prediction2)

?data.frame
colnames(svm_solution2)

#renaming columns
names(svm_solution2)[names(svm_solution2) == "test.Id"] <- "Id"
names(svm_solution2)[names(svm_solution2) == "svm_prediction2"] <- "SalePrice"

#creating solution file
write.csv(svm_solution2, file="svm_solution2.csv", row.names = F)

#much better- score of 0.15134

#removing more variables might help
#will be choosing these factors- MSSubClass, LotArea, Neighborhood, OverallCond, OverallQual, YearBuilt, YearRemodAdd
#, ExterQual, ExterCond, TotalBamtSF, CentralAir
#, X1stFlrSF, X2nsFlrSF, FullBath, KitchenQual, TotRmsAbvGrd,
#, GarageCars, GarageArea, MiscFeature, SaleType
#, SaleCond

#making an svm model
set.seed(123)
svm_model3 <- svm(SalePrice ~ MSSubClass+ LotArea+ Neighborhood+ OverallCond+ OverallQual+ YearBuilt+ YearRemodAdd
                  + ExterQual+ ExterCond+ TotalBsmtSF+ CentralAir
                  + X1stFlrSF+ X2ndFlrSF+ FullBath+ KitchenQual+ TotRmsAbvGrd
                  + GarageCars+ GarageArea+ MiscFeature+ SaleType
                  + SaleCondition, data = train)
summary(svm_model3)

#plotting model
#idk how to do this yet- error
plot.svm(svm_model3)

#making prediction with test data
#predicting for train data instead of test data for some reason
#will try again later
?predict
train_pre_svm3<- predict(svm_model3, data=test, type="response")
train_pre_svm3
table(train_pre_svm3)

#not much accuracy but thats expected
mean(train_pre_svm3==train$SalePrice)

#values of train being predicted only
?predict
svm_prediction3<- predict(svm_model3, newdata=test[-81], type="response")
svm_prediction3
table(svm_prediction3)

#making a submission
svm_solution3 <- data.frame(test$Id, svm_prediction3)

?data.frame
colnames(svm_solution3)

#renaming columns
names(svm_solution3)[names(svm_solution3) == "test.Id"] <- "Id"
names(svm_solution3)[names(svm_solution3) == "svm_prediction3"] <- "SalePrice"

#creating solution file
write.csv(svm_solution3, file="svm_solution3.csv", row.names = F)

#score improved a bit- 0.14754

#should try changing the features more once i figure out what the imp variables are

#making an svm model

set.seed(123)
svm_model4 <- svm(SalePrice ~ MSSubClass+MSZoning+ LotArea+Street+LotShape+Utilities
                              +LotConfig+LandSlope+Neighborhood+Condition1+Condition2+OverallQual
                              +YearBuilt+RoofStyle+RoofMatl+MasVnrArea+Foundation+BsmtQual+BsmtExposure
                              +BsmtFinType1+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF, data = train)
summary(svm_model4)

#plotting model
#idk how to do this yet- error
plot.svm(svm_model3)

#making prediction with test data
#predicting for train data instead of test data for some reason
#will try again later
?predict
train_pre_svm4<- predict(svm_model4, data=test, type="response")
train_pre_svm4
table(train_pre_svm4)

#not much accuracy but thats expected
mean(train_pre_svm4==train$SalePrice)

#values of train being predicted only
?predict
svm_prediction4<- predict(svm_model4, newdata=test[-81], type="response")
svm_prediction4
table(svm_prediction4)

#making a submission
svm_solution4 <- data.frame(test$Id, svm_prediction4)

?data.frame
colnames(svm_solution4)

#renaming columns
names(svm_solution4)[names(svm_solution4) == "test.Id"] <- "Id"
names(svm_solution4)[names(svm_solution4) == "svm_prediction4"] <- "SalePrice"

#creating solution file
write.csv(svm_solution4, file="svm_solution4.csv", row.names = F)

#worse score- 0.18813

#try again with variables from varImpPlot
set.seed(123)
svm_model5 <- svm(SalePrice ~ OverallQual+Neighborhood+GrLivArea+GarageCars+ExterQual
                  +TotalBsmtSF+X1stFlrSF+GarageArea+KitchenQual+X2ndFlrSF+BsmtQual
                  +BsmtFinSF1+TotRmsAbvGrd+YearBuilt+LotArea+FullBath+MoSold+MSSubClass
                  +GarageYrBlt+Exterior2nd+YearRemodAdd+FireplaceQu+Exterior1st
                  +MasVnrArea+GarageFinish+GarageType+LotFrontage+BsmtUnfSF+Fireplaces
                  +OverallCond, data = train)
summary(svm_model5)

#plotting model
#idk how to do this yet- error
plot.svm(svm_model5)

#making prediction with test data
#predicting for train data instead of test data for some reason
#will try again later
?predict
train_pre_svm5<- predict(svm_model5, data=test, type="response")
train_pre_svm5
table(train_pre_svm5)

#not much accuracy but thats expected
mean(train_pre_svm5==train$SalePrice)

#values of train being predicted only
?predict
svm_prediction5<- predict(svm_model5, newdata=test[-81], type="response")
svm_prediction5
table(svm_prediction5)

#making a submission
svm_solution5 <- data.frame(test$Id, svm_prediction5)

?data.frame
colnames(svm_solution5)

#renaming columns
names(svm_solution5)[names(svm_solution5) == "test.Id"] <- "Id"
names(svm_solution5)[names(svm_solution5) == "svm_prediction5"] <- "SalePrice"

#creating solution file
write.csv(svm_solution5, file="svm_solution5.csv", row.names = F)

#better score- 0.14637

#gonna reduce num of variables
set.seed(123)
svm_model6 <- svm(SalePrice ~ OverallQual+Neighborhood+GrLivArea+GarageCars+ExterQual
                  +TotalBsmtSF+X1stFlrSF+GarageArea+KitchenQual+X2ndFlrSF+BsmtQual
                  +BsmtFinSF1+TotRmsAbvGrd+YearBuilt+LotArea+FullBath+MoSold+MSSubClass, 
                  data = train)
summary(svm_model6)

#plotting model
#idk how to do this yet- error
plot.svm(svm_model6)

#making prediction with test data
#predicting for train data instead of test data for some reason
#will try again later
?predict
train_pre_svm6<- predict(svm_model6, data=test, type="response")
train_pre_svm6
table(train_pre_svm6)

#not much accuracy but thats expected
mean(train_pre_svm6==train$SalePrice)

#values of train being predicted only
?predict
svm_prediction6<- predict(svm_model6, newdata=test[-81], type="response")
svm_prediction6
table(svm_prediction6)

#making a submission
svm_solution6 <- data.frame(test$Id, svm_prediction6)

?data.frame
colnames(svm_solution6)

#renaming columns
names(svm_solution6)[names(svm_solution6) == "test.Id"] <- "Id"
names(svm_solution6)[names(svm_solution6) == "svm_prediction6"] <- "SalePrice"

#creating solution file
write.csv(svm_solution6, file="svm_solution6.csv", row.names = F)

#score decreased- 0.15362

#try again with variables from varImpPlot2
set.seed(123)
svm_model7 <- svm(SalePrice ~ OverallQual+Neighborhood+ExterQual+GarageCars+X1stFlrSF
                  +TotalBsmtSF+GarageArea+X2ndFlrSF+KitchenQual+BsmtQual+TotRmsAbvGrd
                  +YearBuilt+LotArea+FullBath+BsmtQual+MSSubClass+YearRemodAdd
                  +Exterior2nd+Exterior1st+GarageType+WoodDeckSF+OverallCond
                  +HouseStyle+SaleCondition+HalfBath+SaleType+CentralAir+GarageQual
                  +EnclosedPorch+ExterCond+MiscFeature, data = train)
summary(svm_model7)

#plotting model
#idk how to do this yet- error
plot.svm(svm_model7)

#making prediction with test data
#predicting for train data instead of test data for some reason
#will try again later
?predict
train_pre_svm7<- predict(svm_model7, data=test, type="response")
train_pre_svm7
table(train_pre_svm7)

#not much accuracy but thats expected
mean(train_pre_svm7==train$SalePrice)

#values of train being predicted only
?predict
svm_prediction7<- predict(svm_model7, newdata=test[-81], type="response")
svm_prediction7
table(svm_prediction7)

#making a submission
svm_solution7 <- data.frame(test$Id, svm_prediction7)

?data.frame
colnames(svm_solution7)

#renaming columns
names(svm_solution7)[names(svm_solution7) == "test.Id"] <- "Id"
names(svm_solution7)[names(svm_solution7) == "svm_prediction7"] <- "SalePrice"

#creating solution file
write.csv(svm_solution7, file="svm_solution7.csv", row.names = F)

#eh score- 0.15094

#gonna reduce num of variables
set.seed(123)
svm_model8 <- svm(SalePrice ~ OverallQual+Neighborhood+ExterQual+GarageCars+X1stFlrSF
                  +TotalBsmtSF+GarageArea+X2ndFlrSF+KitchenQual+BsmtQual+TotRmsAbvGrd
                  +YearBuilt+LotArea+FullBath+BsmtQual+MSSubClass, data = train)
summary(svm_model8)

#plotting model
#idk how to do this yet- error
plot.svm(svm_model8)

#making prediction with test data
#predicting for train data instead of test data for some reason
#will try again later
?predict
train_pre_svm8<- predict(svm_model8, data=test, type="response")
train_pre_svm8
table(train_pre_svm8)

#not much accuracy but thats expected
mean(train_pre_svm6==train$SalePrice)

#values of train being predicted only
?predict
svm_prediction8<- predict(svm_model8, newdata=test[-81], type="response")
svm_prediction8
table(svm_prediction8)

#making a submission
svm_solution8 <- data.frame(test$Id, svm_prediction8)

?data.frame
colnames(svm_solution8)

#renaming columns
names(svm_solution8)[names(svm_solution8) == "test.Id"] <- "Id"
names(svm_solution8)[names(svm_solution8) == "svm_prediction8"] <- "SalePrice"

#creating solution file
write.csv(svm_solution8, file="svm_solution8.csv", row.names = F)

#score decreased- 0.15848

#try again with variables from varImpPlot3
set.seed(123)
svm_model9 <- svm(SalePrice ~ OverallQual+Neighborhood+GarageCars+ExterQual+TotalBsmtSF
                  +X1stFlrSF+GarageArea+X2ndFlrSF+TotRmsAbvGrd+LotArea+YearBuilt
                  +KitchenQual+FullBath+MSSubClass+YearRemodAdd+OverallCond+SaleCondition
                  +SaleType+CentralAir+ExterCond+MiscFeature, data = train)
summary(svm_model9)

#plotting model
#idk how to do this yet- error
plot.svm(svm_model7)

#making prediction with test data
#predicting for train data instead of test data for some reason
#will try again later
?predict
train_pre_svm9<- predict(svm_model9, data=test, type="response")
train_pre_svm9
table(train_pre_svm9)

#not much accuracy but thats expected
mean(train_pre_svm9==train$SalePrice)

#values of train being predicted only
?predict
svm_prediction9<- predict(svm_model9, newdata=test[-81], type="response")
svm_prediction9
table(svm_prediction9)

#making a submission
svm_solution9 <- data.frame(test$Id, svm_prediction9)

?data.frame
colnames(svm_solution9)

#renaming columns
names(svm_solution9)[names(svm_solution9) == "test.Id"] <- "Id"
names(svm_solution9)[names(svm_solution9) == "svm_prediction9"] <- "SalePrice"

#creating solution file
write.csv(svm_solution9, file="svm_solution9.csv", row.names = F)

#promising score- 0.14741

#gonna reduce num of variables
set.seed(123)
svm_model10 <- svm(SalePrice ~ OverallQual+Neighborhood+GarageCars+ExterQual+TotalBsmtSF
                  +X1stFlrSF+GarageArea+X2ndFlrSF+TotRmsAbvGrd+LotArea+YearBuilt
                  +KitchenQual+FullBath+MSSubClass, data = train)
summary(svm_model10)

#plotting model
#idk how to do this yet- error
plot.svm(svm_model10)

#making prediction with test data
#predicting for train data instead of test data for some reason
#will try again later
?predict
train_pre_svm10<- predict(svm_model10, data=test, type="response")
train_pre_svm10
table(train_pre_svm10)

#not much accuracy but thats expected
mean(train_pre_svm6==train$SalePrice)

#values of train being predicted only
?predict
svm_prediction10<- predict(svm_model10, newdata=test[-81], type="response")
svm_prediction10
table(svm_prediction10)

#making a submission
svm_solution10 <- data.frame(test$Id, svm_prediction10)

?data.frame
colnames(svm_solution10)

#renaming columns
names(svm_solution10)[names(svm_solution10) == "test.Id"] <- "Id"
names(svm_solution10)[names(svm_solution10) == "svm_prediction10"] <- "SalePrice"

#creating solution file
write.csv(svm_solution10, file="svm_solution10.csv", row.names = F)

#score decreased- 0.15884

#3. glm model/logit

#making linear model
?glm

#had to exclude variables coz of error mssgs
logit_model<- glm(SalePrice ~ ., data=train[-c(2,50,55,57,62,73)])
summary(logit_model)

str(full)

#making a prediction and displaying it
train_pre_logit <- predict(logit_model, data=train,type =  "response")
table(train_pre_logit)

#predicting with test data
test_pre_logit <- predict(logit_model, newdata=test,type =  "response")

table(test$PassengerId)
#idk what this is supposed to do since we dont know survived
table(test_pre_logit)

#making a submission
logit_solution <- data.frame(test$Id, test_pre_logit)

colnames(logit_solution)

#renaming columns
names(logit_solution)[names(logit_solution) == "test.Id"] <- "Id"
names(logit_solution)[names(logit_solution) == "test_pre_logit"] <- "SalePrice"

#creating solution file
write.csv(logit_solution, file="logit_solution.csv", row.names = F)

#worse score- 0.20068

#will try with lesser variables
#making linear model
?glm

#had to exclude variables coz of error mssgs
logit_model2<- glm(SalePrice ~ LotArea+ Neighborhood+ HouseStyle+ OverallCond+ OverallQual+ YearBuilt+ YearRemodAdd
                  + Exterior1st+ Exterior2nd+ ExterQual+ ExterCond+ BsmtQual+ TotalBsmtSF+ CentralAir
                  + X1stFlrSF+ X2ndFlrSF+ HalfBath+ KitchenQual+ GarageType
                  + GarageQual+ GarageArea+ WoodDeckSF+ EnclosedPorch+ MiscFeature+ SaleType
                  + SaleCondition, data=train[-c(2,50,55,57,62,73)])
summary(logit_model2)

str(full)

#making a prediction and displaying it
train_pre_logit2 <- predict(logit_model2, data=train,type =  "response")
table(train_pre_logit2)

#predicting with test data
test_pre_logit2 <- predict(logit_model2, newdata=test,type =  "response")

table(test$Id)
#idk what this is supposed to do since we dont know survived
table(test_pre_logit2)

#making a submission
logit_solution2 <- data.frame(test$Id, test_pre_logit2)

colnames(logit_solution2)

#renaming columns
names(logit_solution2)[names(logit_solution2) == "test.Id"] <- "Id"
names(logit_solution2)[names(logit_solution2) == "test_pre_logit2"] <- "SalePrice"

#creating solution file
write.csv(logit_solution2, file="logit_solution2.csv", row.names = F)

#improved a bit- 0.15267

#making linear model
?glm

#had to exclude variables coz of error mssgs
logit_model3<- glm(SalePrice ~ LotArea+ Neighborhood+ OverallCond+ OverallQual+ YearBuilt+ YearRemodAdd
                   + ExterQual+ ExterCond+ TotalBsmtSF+ CentralAir
                   + X1stFlrSF+ X2ndFlrSF+ KitchenQual
                   + GarageArea+ MiscFeature+ SaleType
                   + SaleCondition, data=train[-c(2,50,55,57,62,73)])
summary(logit_model3)

str(full)

#making a prediction and displaying it
train_pre_logit3 <- predict(logit_model3, data=train,type =  "response")
table(train_pre_logit3)

#predicting with test data
test_pre_logit3 <- predict(logit_model3, newdata=test,type =  "response")

table(test$Id)
#idk what this is supposed to do since we dont know survived
table(test_pre_logit3)

#making a submission
logit_solution3 <- data.frame(test$Id, test_pre_logit3)

colnames(logit_solution3)

#renaming columns
names(logit_solution3)[names(logit_solution3) == "test.Id"] <- "Id"
names(logit_solution3)[names(logit_solution3) == "test_pre_logit3"] <- "SalePrice"

#creating solution file
write.csv(logit_solution3, file="logit_solution3.csv", row.names = F)

#improved a tiny bit- 0.15124

#have to find a way to use the dropped variables and then check

#can infer imp variables from summary-should do this later

#making linear model
?glm

#had to exclude variables coz of error mssgs
#excluding MSSubClass
logit_model4<- glm(SalePrice ~ MSZoning+ LotArea+Street+LotShape+Utilities
                  +LotConfig+LandSlope+Neighborhood+Condition1+Condition2+OverallQual
                  +YearBuilt+RoofStyle+RoofMatl+MasVnrArea+Foundation+BsmtQual+BsmtExposure
                  +BsmtFinType1+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF, data=train[-c(2,50,55,57,62,73)])
summary(logit_model4)

str(full)
full$Street

#making a prediction and displaying it
train_pre_logit4 <- predict(logit_model4, data=train,type =  "response")
table(train_pre_logit4)

#predicting with test data
test_pre_logit4 <- predict(logit_model4, newdata=test,type =  "response")

table(test$PassengerId)
#idk what this is supposed to do since we dont know survived
table(test_pre_logit)

#making a submission
logit_solution4 <- data.frame(test$Id, test_pre_logit4)

colnames(logit_solution4)

#renaming columns
names(logit_solution4)[names(logit_solution4) == "test.Id"] <- "Id"
names(logit_solution4)[names(logit_solution4) == "test_pre_logit4"] <- "SalePrice"

#creating solution file
write.csv(logit_solution4, file="logit_solution4.csv", row.names = F)

#worse score than expected- 0.19448


#4. decision tree- check for min xerror

set.seed(123)
#making model from decision tree
#install and load required packages first
library("rpart")

?rpart
#creating model
dt_model <- rpart(SalePrice ~ ., data=train)

#packages to plot model
install.packages("rpart.plot")
library("rpart.plot")

?rpart.plot

#plotting model
rpart.plot(dt_model, fallen.leaves = T, box.palette = "blue")
#title, pclass, fare, embarked r imp

#package for confusion matrix
install.packages("caret")
#for some reason it is not loading
library("caret")

#predicting train data to see accuracy
pre_dtree <- predict(dt_model, data=train)

?confusionMatrix

#confusion matrix with accuracy of 0.8384%
confusionMatrix(pre_dtree, train$SalePrice)

#trying to find minxerror
#can find min xerror from this table
printcp(dt_model)

#This function returns the optimal cp value associated with the minimum error.
dt_model$cptable[which.min(dt_model$cptable[,"xerror"]),"CP"]

#plotting to find min error cp
plotcp(dt_model)

?prune
ptree<- prune(dt_model, cp=dt_model$cptable[which.min(dt_model$cptable[,"xerror"]),"CP"])

#installing package to plot more aesthetically
install.packages("rattle")
library("rattle")
fancyRpartPlot(ptree, uniform=TRUE, main="Pruned Classification Tree")
#we've pruned the tree now

#predicting with test data
test_pre_dtree <- predict(ptree, newdata=test)

#generalized solution
table(test_pre_dtree)

#making a submission
dtree_solution <- data.frame(test$Id, test_pre_dtree)

colnames(dtree_solution)

#renaming columns
names(dtree_solution)[names(dtree_solution) == "test.Id"] <- "Id"
names(dtree_solution)[names(dtree_solution) == "test_pre_dtree"] <- "SalePrice"

#creating solution file
write.csv(dtree_solution, file="dtree_solution.csv", row.names = F)

#not as bad as expected for generalized solution- 0.23963

#5. random forest

#setting seed
set.seed(4242)
library("randomForest")

#making first model
rf_model <- randomForest(SalePrice ~ , data= train)
?randomForest

#plotting model to find out optimal number of trees
plot(rf_model)
?plot

#apparently can find out importance of variables too
#will do this later
importance    <- importance(rf_model)

test_pre_rf <- predict(rf_model, test)

#make csv file with prediction
rf_solution <- data.frame(test$Id, test_pre_rf)

#more accurate without SibSp + Parch
varImpPlot(rf_model)
varImpPlot(rf_model2)
varImpPlot(rf_model3)

colnames(rf_solution)

#renaming columns
names(rf_solution)[names(rf_solution) == "test.Id"] <- "Id"
names(rf_solution)[names(rf_solution) == "test_pre_rf"] <- "SalePrice"

#creating solution file
write.csv(rf_solution, file="rf_solution.csv", row.names = F)

#ok score- 0.15258

#will try with lesser variables
set.seed(4242)
library("randomForest")

#making second model
rf_model2 <- randomForest(SalePrice ~ MSSubClass+ LotArea+ Neighborhood+ HouseStyle+ OverallCond+ OverallQual+ YearBuilt+ YearRemodAdd
                         + Exterior1st+ Exterior2nd+ ExterQual+ ExterCond+ BsmtQual+ TotalBsmtSF+ CentralAir
                         + X1stFlrSF+ X2ndFlrSF+ FullBath+ HalfBath+ KitchenQual+ TotRmsAbvGrd+ GarageType
                         + GarageQual+ GarageCars+ GarageArea+ WoodDeckSF+ EnclosedPorch+ MiscFeature+ SaleType
                         + SaleCondition, data= train)
?randomForest

#plotting model to find out optimal number of trees
plot(rf_model2)
?plot

#apparently can find out importance of variables too
#will do this later
importance2    <- importance(rf_model2)

test_pre_rf2 <- predict(rf_model2, test)

#make csv file with prediction
rf_solution2 <- data.frame(test$Id, test_pre_rf2)

colnames(rf_solution2)

#renaming columns
names(rf_solution2)[names(rf_solution2) == "test.Id"] <- "Id"
names(rf_solution2)[names(rf_solution2) == "test_pre_rf2"] <- "SalePrice"

#creating solution file
write.csv(rf_solution2, file="rf_solution2.csv", row.names = F)

#surprisingly score got worse- 0.15785

#will try with lesser variables
set.seed(4242)
library("randomForest")

#making second model
rf_model3 <- randomForest(SalePrice ~ MSSubClass+ LotArea+ Neighborhood+ OverallCond+ OverallQual+ YearBuilt+ YearRemodAdd
                          + ExterQual+ ExterCond+ TotalBsmtSF+ CentralAir
                          + X1stFlrSF+ X2ndFlrSF+ FullBath+ KitchenQual+ TotRmsAbvGrd
                          + GarageCars+ GarageArea+ MiscFeature+ SaleType
                          + SaleCondition, data= train)
?randomForest

#plotting model to find out optimal number of trees
plot(rf_model3)
?plot

#apparently can find out importance of variables too
#will do this later
importance3    <- importance(rf_model3)

test_pre_rf3 <- predict(rf_model3, test)

#make csv file with prediction
rf_solution3 <- data.frame(test$Id, test_pre_rf3)

colnames(rf_solution3)

#renaming columns
names(rf_solution3)[names(rf_solution3) == "test.Id"] <- "Id"
names(rf_solution3)[names(rf_solution3) == "test_pre_rf3"] <- "SalePrice"

#creating solution file
write.csv(rf_solution3, file="rf_solution3.csv", row.names = F)

#surprisingly score got worse- 0.15663

#combining 2 solutions by finding average
#svm_solution6 and 9

#finding mean of 2 predictions and storing it in variable
x1 <- data.frame(svm_prediction6, svm_prediction9)

#findind mean of each row and storing it in variable
prediction_combined69 <- data.frame(rowMeans(x1))
?mean

#printing out prediction column
prediction_combined69[,1]

#combining Id and prediction
svm_combined69 <- data.frame(Id=test[,1],SalePrice=prediction_combined69[,1])
colnames(svm_combined69)

#creating solution file
write.csv(svm_combined69, file="svm_combined69.csv", row.names = F)

#score= 0.14696

#svm_solution5 and 9

#finding mean of 2 predictions and storing it in variable
x2 <- data.frame(svm_prediction5, svm_prediction9)

#findind mean of each row and storing it in variable
prediction_combined59 <- data.frame(rowMeans(x2))
?mean

#printing out prediction column
prediction_combined59[,1]

#combining Id and prediction
svm_combined59 <- data.frame(Id=test[,1],SalePrice=prediction_combined59[,1])
colnames(svm_combined59)

#creating solution file
write.csv(svm_combined59, file="svm_combined59.csv", row.names = F)

#lowest score yet= 0.14402

#svm_solution5 and 3

#finding mean of 2 predictions and storing it in variable
x3 <- data.frame(svm_prediction5, svm_prediction3)

#findind mean of each row and storing it in variable
prediction_combined35 <- data.frame(rowMeans(x3))
?mean

#printing out prediction column
prediction_combined35[,1]

#combining Id and prediction
svm_combined35 <- data.frame(Id=test[,1],SalePrice=prediction_combined35[,1])
colnames(svm_combined35)

#creating solution file
write.csv(svm_combined35, file="svm_combined35.csv", row.names = F)

#low score= 0.14408

#svm_solution5, 9 and 3

#finding mean of 2 predictions and storing it in variable
x4 <- data.frame(svm_prediction5, svm_prediction3, svm_prediction9)

#findind mean of each row and storing it in variable
prediction_combined359 <- data.frame(rowMeans(x4))
?mean

#printing out prediction column
prediction_combined359[,1]

#combining Id and prediction
svm_combined359 <- data.frame(Id=test[,1],SalePrice=prediction_combined359[,1])
colnames(svm_combined359)

#creating solution file
write.csv(svm_combined359, file="svm_combined359.csv", row.names = F)

#score= 0.14455



