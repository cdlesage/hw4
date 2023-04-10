library(tidyverse)
library(tidycensus)

# Reading in data
data = read_csv("airport_pairs.csv")

#Question 1
# Filtering for data of flights in or out of RDU
RDUdata = filter(data, origin == "RDU" | dest == "RDU")

# Sorting by passenger count
RDUdataarranged = arrange(RDUdata, passengers)
# Filtering for more than 10000 passengers
filtered = filter(RDUdataarranged, passengers >= 10000)
# ATL is most popular nonstop destination                 


# Question 2
# Loading ACS variables and saving as a CSV file to open in Excel
acs_data = load_variables(2021, "acs5")
write_csv(acs_data, "acsdata.csv")

# Making a table renamed for origin population
census_origin = get_acs(
  geography="cbsa",  # could be tract, block group, etc.
  variables=c(
    "household_income_origin"="B19013_001",
    "food_stamps_origin"="B22003_001",
    "atorabove_povertylevel_origin"="B17001_031",
    "origin_pop"="B01003_001"
  ),
  year=2021,
  survey="acs5",
  output="wide"
)
census_origin = census_origin %>%
  rename("origin_cbsa"="GEOID")

# Making a table renamed for destination population
census_dest = get_acs(
  geography="cbsa",  
  variables=c(
    "household_income_dest"="B19013_001",
    "food_stamps_dest"="B22003_001",
    "atorabove_povertylevel_dest"="B17001_031",
    "dest_pop"="B01003_001"
  ),
  year=2021,
  survey="acs5",
  output="wide"
)
census_dest = census_dest %>%
  rename("dest_cbsa"="GEOID")

# Merging all the data
join1 = merge(data, census_origin, by="origin_cbsa")
join2 = merge(join1, census_dest, by="dest_cbsa")

# Scatterplot of origin population vs total passengers
ggplot(join2, aes(x=origin_popE, y=passengers)) +
  geom_point(size=1) 


# Scatterplot of destination population vs total passengers
ggplot(join2, aes(x=dest_popE, y=passengers)) +
  geom_point(size=1)

# Scatterplot of flight distance vs total passengers
ggplot(join2, aes(x=distancemiles, y=passengers)) +
  geom_point(size=1) +
  geom_smooth(method = "loess", formula = y ~ x)

# Scatterplots of poverty status variable 
ggplot(join2, aes(x=atorabove_povertylevel_originE, y=passengers)) +
  geom_point(size=1)

ggplot(join2, aes(x=atorabove_povertylevel_destE, y=passengers)) +
  geom_point(size=1)

# Question 3
# Regression model of the variables 

multiple_variable_regression = lm(passengers~origin_popE+dest_popE+distancemiles+atorabove_povertylevel_originE+atorabove_povertylevel_destE, join2)
summary(multiple_variable_regression)


# Question 4

# Creating a new data frame
join2simple = subset(join2, select = c(origin, dest, passengers, distancemiles, dest_popE, origin_popE, atorabove_povertylevel_originE, atorabove_povertylevel_destE))
route1 = filter(join2simple, origin == "RDU" | dest == "RDU" | origin == "PDX" | dest == "PDX")
route2 = filter(join2simple, origin == "RDU" | dest == "RDU" | origin == "ELP" | dest == "ELP")
route3 = filter(join2simple, origin == "RDU" | dest == "RDU" | origin == "TLH" | dest == "TLH")
route4 = filter(join2simple, origin == "RDU" | dest == "RDU" | origin == "SAN" | dest == "SAN")
# Using predict for route 1
route1$prediction = predict(multiple_variable_regression, route1)
route2$prediction = predict(multiple_variable_regression, route2)
route3$prediction = predict(multiple_variable_regression, route3)
route4$prediction = predict(multiple_variable_regression, route4)

mean(route1$prediction)
mean(route2$prediction)
mean(route3$prediction)
mean(route4$prediction)




