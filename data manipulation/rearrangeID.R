# GOAL: take a data set from a crossover study with n = 50 individuals such that
# ID '1' is first individual and ID '51' is first individual after crossover
# currently sorted by ID but would like to sort by individual such that data set results
# in ID 1, 51, 2, 52, etc...

#clear any workspace
rm(list = ls())

#create 'fake' data for one hundred ID's with associated information
data <- data.frame("ID" = seq(1,100, 1), 
                    "RACE" = rep(c("asian", "white", "black", "hispanic", "indian", 
                            "asian", "white", "black", "hispanic", "indian"), each = 10), 
                    "ISM" = rep(c(0,1), each=5,length.out = 100))


#split into two cohorts as if individual '1' and '51' are same person in cross-over
firstcohort <- subset(data, data$ID <= 50)
#another way of subsetting
secondcohort <- data[data$ID >50,]


# initialize new data frame with empty vectors 
# must set stringsAsFactors = FALSE or doesn't initalize properly
sorted <- data.frame(ID = character(), RACE = character(), ISM = character(), stringsAsFactors = FALSE)


#initialize count variable
j = 1
firstcohort[1,]
#loop through to pull same row from first and second cohort
# i is row number from first/second cohort subsets
# j is row in new data frame
for(i in 1:nrow(firstcohort)) {
  sorted[j, ] <- firstcohort[i,]
  j = j + 1
  sorted[j,] <- secondcohort[i,]
  j = j+1  
}

sorted

    