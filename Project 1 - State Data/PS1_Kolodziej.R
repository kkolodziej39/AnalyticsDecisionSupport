# Kyle Kolodziej
# Last edited: 9/9/2021
# CS 5357 Problem Set 1

StateData = read.csv("StateData.csv") # Read in the csv file

# Part A: Scatter plot of all the states
partA = plot(StateData$Longitude, StateData$Latitude, xlab = "Longitude", 
             ylab = "Latitude", main = "Latitude v. Longitude")
jpeg("Part A Latitude vs Longitude.jpg")
plot(StateData$Longitude, StateData$Latitude, xlab = "Longitude", 
             ylab = "Latitude", main = "Latitude v. Longitude")
dev.off()

# Part B: Scatter plot of two variables to create a linear relationship
# with the horizontal axis having Latitude. Put Frost on vertical axis
jpeg("Part B Latitude vs Frost.jpg")
partB = plot(StateData$Latitude, StateData$Frost, xlab = "Latitude", 
             ylab = "Frost", main = "Latitude v. Frost")
dev.off()

# Part C: Compute the number of populations in each region

# (i): Using the which function
partCWhichSouth = sum(StateData$Population[which(StateData$Region == "South")])
partCWhichWest = sum(StateData$Population[which(StateData$Region == "West")])
partCWhichNortheast = sum(StateData$Population[which(StateData$Region == "Northeast")])
partCWhichNorthCentral = sum(StateData$Population[which(StateData$Region == "North Central")])

# (i): Using the which function but without hard coding the region values
df = data.frame(StateData)
uniqueRegionNames = unique(df[c("Region")]) # Get all of the region names
regionVect = c()
for(region in uniqueRegionNames){
  print(region)
  regionVect = append(regionVect, region)
  combined = paste(region, "Sub")
  newCombined = str_remove_all(combined, " ")
}

partCWhichArray = c()
for(region in regionVect){
  toAdd = c(region, sum(StateData$Population[which(StateData$Region == region)]))
  partCWhichArray = append(partCWhichArray, toAdd)
}


# (i): Using the tapply function
partCtApply = tapply(StateData$Population, StateData$Region, sum)

# Part D: Box plot of Murders for Each region

# Doing it with each region hard coded first
southSub = subset(StateData, StateData$Region == "South")
jpeg("Murders in South Region.jpg")
partDSouth = boxplot(southSub$Murder, xlab = "South", ylab = "Murder", main = "Murders in South Region")
dev.off()

westSub = subset(StateData, StateData$Region == "West")
partDWest = boxplot(westSub$Murder, xlab = "West", ylab = "Murder", main = "Murders in West Region")

northeastSub = subset(StateData, StateData$Region == "Northeast")
partDNortheast = boxplot(northeastSub$Murder, xlab = "Northeast", ylab = "Murder", main = "Murders in Northeast Region")

northCentralSub = subset(StateData, StateData$Region == "North Central")
partDNorthCentral = boxplot(northeastSub$Murder, xlab = "North Central", ylab = "Murder", main = "Murders in North Central Region")

# Now making the boxplots using a vector of the regions with this for loop
for(region in regionVect){
  regionSubset = subset(StateData, StateData$Region == region)
  jpegTitle = paste("Murders in", region, "Region.jpg")
  subData = subset(StateData, StateData$Region == region)
  mainTitle = paste("Murders in", region, "Region")
  jpeg(jpegTitle)
  boxplot(subData$Murder, xlab = region, ylab = "Murder", main = mainTitle)
  dev.off()
}

# Now making one plot with 4 seperate boxplots for each region
jpeg("Murders for each Region.jpg")
partDCombined = boxplot(StateData$Murder ~ StateData$Region,
                        xlab = "Region", 
                        ylab = "Murder", 
                        main = "Murders for each Region")
dev.off()

# Part E: box plot of distribution of income of...

# i) states that have >55% high school graduation rate
jpeg("Income Distribution Greater than 55.jpg")
partEGreater = boxplot(StateData$Income[which(StateData$HighSchoolGrad > 55)] ~ StateData$Region[which(StateData$HighSchoolGrad > 55)],
                        xlab = "Region", 
                        ylab = "Income",
                        main = "Income Distribution for States with HS Graduation Rate Above 55%")
dev.off()

# ii) states that have <55% high school graduation rate
jpeg("Income Distribution Less than 55.jpg")
partELess = boxplot(StateData$Income[which(StateData$HighSchoolGrad < 55)] ~ StateData$Region[which(StateData$HighSchoolGrad < 55)],
                       xlab = "Region", 
                       ylab = "Income",
                       main = "Income Distribution for States with HS Graduation Rate Below 55%")
dev.off()


# Part F: using table function to get the number of states that the average income exceeds $4,500 for each region
stateIncomeCount = table(StateData$Region, StateData$Income > 4500)

# Part G: scatter plots with LifeExp as the y-axis and trying to find an x-axis that would
# have a negative relationship on life expectancy

# Going to try murder rate
jpeg("Life Expectancy vs Murder Rate.jpg")
plot(StateData$Murder, StateData$LifeExp,
     xlab = "Murder",
     ylab = "Life Expectancy",
     main = "Life Expectancy vs Murder Rate")
dev.off() # Looks like it has a negative relationship

# Going to try high school graduation rate now
jpeg("Life Expectancy vs High School Graduation Rate.jpg")
plot(StateData$HighSchoolGrad, StateData$LifeExp,
     xlab = "High School Graduation Rate",
     ylab = "Life Expectancy",
     main = "Life Expectancy vs High School Graduation Rate")
dev.off() # Looks like it has a positive relationship

# Going to try population
jpeg("Life Expectancy vs Population.jpg")
plot(StateData$Population, StateData$LifeExp,
     xlab = "Population",
     ylab = "Life Expectancy",
     main = "Life Expectancy vs Population")
dev.off() # Looks like it does not have much of a relationship

# Going to try illiteracy rate
jpeg("Life Expectancy vs Illiteracy Rate.jpg")
plot(StateData$Illiteracy, StateData$LifeExp,
     xlab = "Illiteracy",
     ylab = "Life Expectancy",
     main = "Life Expectancy vs Illiteracy Rate")
dev.off() # Looks like it also had a negative relationship


