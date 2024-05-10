# read a csv file
# method 1 : select manually
stats <- read.csv(file.choose())
stats
#functions used to explore data of .csv file 
nrow(stats) # no.rows
ncol(stats) # no.of columns
head(stats) # first 6 rows
tail(stats) # last 6 rows
str(stats) #structure, Factor W/4 means 4 different values 1,2,3,4
#used to factorize a column by using factor()
summary(stats)
#using "$" for extracting column vector
stats[3,3] # row 3 and col 3
stats[3,3, drop=F]
stats[3, "Birth.rate"] #rows dont contain names in data frame
stats$Internet.users #used to extract column vector
stats$Internet.users[3] # Angola
levels(stats$Income.Group)
summary(stats$Birth.rate)

stats[1:10,]
stats[2:7,]
stats[c(4,16,100),]
stats$Birth.rate * stats$Internet.users # meaningless, just testing math
stats$Birth.rate + stats$Internet.users # meaningless, just testing math
# Adding a column
stats$MyCalc <- stats$Birth.rate * stats$Internet.users
stats
stats$xyz <- 1:5 # add another column
stats

#data filtering
#filtering data frames
#to make it Slow way of doing it
filter <- stats$Internet.users < 2 # which one less than 2 percent
stats[filter,]
#optimal way for filtering
stats[stats$Birth.rate > 40 & stats$Internet.users < 2,] # one step
stats[stats$Income.Group == "High income", ]
stats[stats$Country.Name == "Egypt", ]
#quick plotting
?qplot
# One variable
library(ggplot2) #calling for visualization
qplot(data = stats, x = Internet.users, bins = 10)
# Two variables
qplot(data=stats, x = Income.Group, y= Birth.rate)
qplot(data=stats, x = Income.Group, y= Internet.users)
# Testing the different sizes
qplot(data=stats, x = Income.Group, y= Birth.rate, size=I(10)) #default size is 1
# Testing different Colors for plotting
qplot(data=stats, x = Income.Group, y= Birth.rate, size=I(6), color=I("purple"))

# Testing the different charts qplot can draw
qplot(data=stats, x = Income.Group, y= Birth.rate, geom="boxplot")
qplot(data=stats, x = Income.Group, y= Birth.rate, geom="path")
qplot(data=stats, x = Income.Group, y= Birth.rate, geom="point")

#plotting functions for three variables
qplot(data=stats, x=Internet.users, y=Birth.rate)
qplot(data=stats, x=Internet.users, y=Birth.rate, size=I(4))
qplot(data=stats, x=Internet.users, y=Birth.rate, color=I("orange"))
qplot(data=stats, x=Internet.users, y=Birth.rate, color= Income.Group)
qplot(data=stats, x=Internet.users, y=Birth.rate, color= Income.Group, size=I(4))
qplot(data=stats, y=Internet.users, x=Birth.rate, color= Income.Group, size=I(2))

#continent.R
#Execute below code to generate three new vectors
Countries_2012_Dataset <- c("Aruba","Afghanistan","Angola","Albania","United Arab Emirates","Argentina","Armenia","Antigua and Barbuda","Australia","Austria","Azerbaijan","Burundi","Belgium","Benin","Burkina Faso","Bangladesh","Bulgaria","Bahrain","Bahamas, The","Bosnia and Herzegovina","Belarus","Belize","Bermuda","Bolivia","Brazil","Barbados","Brunei Darussalam","Bhutan","Botswana","Central African Republic","Canada","Switzerland","Chile","China","Cote d'Ivoire","Cameroon","Congo, Rep.","Colombia","Comoros","Cabo Verde","Costa Rica","Cuba","Cayman Islands","Cyprus","Czech Republic","Germany","Djibouti","Denmark","Dominican Republic","Algeria","Ecuador","Egypt, Arab Rep.","Eritrea","Spain","Estonia","Ethiopia","Finland","Fiji","France","Micronesia, Fed. Sts.","Gabon","United Kingdom","Georgia","Ghana","Guinea","Gambia, The","Guinea-Bissau","Equatorial Guinea","Greece","Grenada","Greenland","Guatemala","Guam","Guyana","Hong Kong SAR, China","Honduras","Croatia","Haiti","Hungary","Indonesia","India","Ireland","Iran, Islamic Rep.","Iraq","Iceland","Israel","Italy","Jamaica","Jordan","Japan","Kazakhstan","Kenya","Kyrgyz Republic","Cambodia","Kiribati","Korea, Rep.","Kuwait","Lao PDR","Lebanon","Liberia","Libya","St. Lucia","Liechtenstein","Sri Lanka","Lesotho","Lithuania","Luxembourg","Latvia","Macao SAR, China","Morocco","Moldova","Madagascar","Maldives","Mexico","Macedonia, FYR","Mali","Malta","Myanmar","Montenegro","Mongolia","Mozambique","Mauritania","Mauritius","Malawi","Malaysia","Namibia","New Caledonia","Niger","Nigeria","Nicaragua","Netherlands","Norway","Nepal","New Zealand","Oman","Pakistan","Panama","Peru","Philippines","Papua New Guinea","Poland","Puerto Rico","Portugal","Paraguay","French Polynesia","Qatar","Romania","Russian Federation","Rwanda","Saudi Arabia","Sudan","Senegal","Singapore","Solomon Islands","Sierra Leone","El Salvador","Somalia","Serbia","South Sudan","Sao Tome and Principe","Suriname","Slovak Republic","Slovenia","Sweden","Swaziland","Seychelles","Syrian Arab Republic","Chad","Togo","Thailand","Tajikistan","Turkmenistan","Timor-Leste","Tonga","Trinidad and Tobago","Tunisia","Turkey","Tanzania","Uganda","Ukraine","Uruguay","United States","Uzbekistan","St. Vincent and the Grenadines","Venezuela, RB","Virgin Islands (U.S.)","Vietnam","Vanuatu","West Bank and Gaza","Samoa","Yemen, Rep.","South Africa","Congo, Dem. Rep.","Zambia","Zimbabwe")
Codes_2012_Dataset <- c("ABW","AFG","AGO","ALB","ARE","ARG","ARM","ATG","AUS","AUT","AZE","BDI","BEL","BEN","BFA","BGD","BGR","BHR","BHS","BIH","BLR","BLZ","BMU","BOL","BRA","BRB","BRN","BTN","BWA","CAF","CAN","CHE","CHL","CHN","CIV","CMR","COG","COL","COM","CPV","CRI","CUB","CYM","CYP","CZE","DEU","DJI","DNK","DOM","DZA","ECU","EGY","ERI","ESP","EST","ETH","FIN","FJI","FRA","FSM","GAB","GBR","GEO","GHA","GIN","GMB","GNB","GNQ","GRC","GRD","GRL","GTM","GUM","GUY","HKG","HND","HRV","HTI","HUN","IDN","IND","IRL","IRN","IRQ","ISL","ISR","ITA","JAM","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR","KOR","KWT","LAO","LBN","LBR","LBY","LCA","LIE","LKA","LSO","LTU","LUX","LVA","MAC","MAR","MDA","MDG","MDV","MEX","MKD","MLI","MLT","MMR","MNE","MNG","MOZ","MRT","MUS","MWI","MYS","NAM","NCL","NER","NGA","NIC","NLD","NOR","NPL","NZL","OMN","PAK","PAN","PER","PHL","PNG","POL","PRI","PRT","PRY","PYF","QAT","ROU","RUS","RWA","SAU","SDN","SEN","SGP","SLB","SLE","SLV","SOM","SRB","SSD","STP","SUR","SVK","SVN","SWE","SWZ","SYC","SYR","TCD","TGO","THA","TJK","TKM","TLS","TON","TTO","TUN","TUR","TZA","UGA","UKR","URY","USA","UZB","VCT","VEN","VIR","VNM","VUT","PSE","WSM","YEM","ZAF","COD","ZMB","ZWE")
Regions_2012_Dataset <- c("The Americas","Asia","Africa","Europe","Middle East","The Americas","Asia","The Americas","Oceania","Europe","Asia","Africa","Europe","Africa","Africa","Asia","Europe","Middle East","The Americas","Europe","Europe","The Americas","The Americas","The Americas","The Americas","The Americas","Asia","Asia","Africa","Africa","The Americas","Europe","The Americas","Asia","Africa","Africa","Africa","The Americas","Africa","Africa","The Americas","The Americas","The Americas","Europe","Europe","Europe","Africa","Europe","The Americas","Africa","The Americas","Africa","Africa","Europe","Europe","Africa","Europe","Oceania","Europe","Oceania","Africa","Europe","Asia","Africa","Africa","Africa","Africa","Africa","Europe","The Americas","The Americas","The Americas","Oceania","The Americas","Asia","The Americas","Europe","The Americas","Europe","Asia","Asia","Europe","Middle East","Middle East","Europe","Middle East","Europe","The Americas","Middle East","Asia","Asia","Africa","Asia","Asia","Oceania","Asia","Middle East","Asia","Middle East","Africa","Africa","The Americas","Europe","Asia","Africa","Europe","Europe","Europe","Asia","Africa","Europe","Africa","Asia","The Americas","Europe","Africa","Europe","Asia","Europe","Asia","Africa","Africa","Africa","Africa","Asia","Africa","Oceania","Africa","Africa","The Americas","Europe","Europe","Asia","Oceania","Middle East","Asia","The Americas","The Americas","Asia","Oceania","Europe","The Americas","Europe","The Americas","Oceania","Middle East","Europe","Europe","Africa","Middle East","Africa","Africa","Asia","Oceania","Africa","The Americas","Africa","Europe","Africa","Africa","The Americas","Europe","Europe","Europe","Africa","Africa","Middle East","Africa","Africa","Asia","Asia","Asia","Asia","Oceania","The Americas","Africa","Europe","Africa","Africa","Europe","The Americas","The Americas","Asia","The Americas","The Americas","The Americas","Asia","Oceania","Middle East","Oceania","Middle East","Africa","Africa","Africa","Africa")


#---------------------creating data frames

mydf <-data.frame(Countries_2012_Dataset,Codes_2012_Dataset,Regions_2012_Dataset)
mydf
head(mydf)
colnames(mydf) <- c("Country", "Code", "Region")
head(mydf)

# another way to do it, one step
rm(mydf)
mydf <-data.frame(Country= Countries_2012_Dataset,Code= Codes_2012_Dataset,Region=Regions_2012_Dataset)
head(mydf)

tot <- cbind(stats, mydf) # assuming same ordering
head(tot)
qplot(data= tot, x= Internet.users, y=Birth.rate, color=Region)

#better merging

merged <- merge(stats, mydf, by.x= "Country.Code", by.y="Code")
head(merged)
merged$Country <- NULL
head(merged)
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region)

#shapes  0-25
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(17))
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(2))
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(15))
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(23))

# Transparency  0---1

qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(19), alpha=I(0.7))
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(19), alpha=I(0.3))
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(19), alpha=I(0.5))
# All four variables
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=Income.Group, alpha=I(0.5))
# Add a title

qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(19), alpha=I(0.7),
      main="Birth rate vs Internet Users")