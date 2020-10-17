# set dir to source file - the lazy way
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen = 999) # disable scientific notation
##
# load data
chicken <- read.csv("chickendata.csv",header = TRUE)
##
#remove column X, as i assume it just represent line number per data entry and we dont care
chicken <- subset(chicken, select = -c(X))
##
#check if we got empty data
sum(is.na(chicken))
# [1] 0 means no missing values
##
#look at data summary
summary(chicken)
##

#subset into different groups!
#per PDF: G1 has Chickens ID 1 to 20, G2 has 20 to 30, G3 has 30 to 40 and G4 has 40 to 50.
G1 <- subset(chicken, Chick..ID. <= 20)
G2 <- subset(chicken, Chick..ID. > 20 &  Chick..ID. <= 30)
G3 <- subset(chicken, Chick..ID. > 30 &  Chick..ID. <= 40)
G4 <- subset(chicken, Chick..ID. > 40 &  Chick..ID. <= 50)
##
# Calculate diet costs huehuehuehue
G1$cost <- (2.2*10^-5)/G1$weight..g.
G2$cost <- (2.8*10^-5)/G2$weight..g.
G3$cost <- (3.0*10^-5)/G3$weight..g.
G4$cost <- (3.2*10^-5)/G4$weight..g.
##
# calculate runningcosts
# summarize all weights
SumWTG1 <- aggregate(weight..g.~Time..days., data=G1, FUN=sum) 
SumWTG2 <- aggregate(weight..g.~Time..days., data=G2, FUN=sum) 
SumWTG3 <- aggregate(weight..g.~Time..days., data=G3, FUN=sum) 
SumWTG4 <- aggregate(weight..g.~Time..days., data=G4, FUN=sum)
# calc mean per day
SumWTG1$mean <- aggregate(weight..g.~Time..days., data=G1, FUN=mean) 
SumWTG2$mean <- aggregate(weight..g.~Time..days., data=G2, FUN=mean) 
SumWTG3$mean <- aggregate(weight..g.~Time..days., data=G3, FUN=mean) 
SumWTG4$mean <- aggregate(weight..g.~Time..days., data=G4, FUN=mean)

#########################
#################################
#get the mean weight per diet
getMeansofData <- tapply(chicken$weight..g., chicken$Diet..ID., mean)
print(getMeansofData)
###
#get the mean days chickens been in dataset
getChickendays <- tapply(chicken$Time..days., chicken$Diet..ID., mean)
print(getChickendays)
###
#we know all chickens alive got weighed last day
finalday = tapply(chicken$weight..g., chicken$Diet..ID., max)
print(finalday)
###