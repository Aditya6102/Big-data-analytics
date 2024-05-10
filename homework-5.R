#ASSIGNMENT-5
# reading a csv file
stats <- read.csv(file.choose())
stats
#1:Show the data of France
stats[stats$Country.Name =="France",]
stats
#2: Showing the countries with birthrate >20 and High income
data<- stats[stats$Birth.rate > 20 & stats$Income.Group == "High income", ]
print(data)

#3:displaying data of country w/ highest internet users
max_internet_user_country <- stats[stats$Internet.users == max(stats$Internet.users,na.rm = TRUE), ]
print(max_internet_user_country)

#4:avg birth Rate
Avg_birthrate <- mean(stats$Birth.rate, na.rm = TRUE)
print(Avg_birthrate)

#5:standard Deviation of birthrate
SD_birthrate <- sd(stats$Birth.rate, na.rm = TRUE)
print(SD_birthrate)

#6: Countries birth rate is two SDs above the mean
countries_above_2SDs <- stats[stats$Birth.rate > (Avg_birthrate + 2*SD_birthrate), ]
print(countries_above_2SDs)

#7: IQR of birthrate 
Iqr_birthrate <- IQR(stats$Birth.rate, na.rm = TRUE)
print(Iqr_birthrate)