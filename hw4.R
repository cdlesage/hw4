library(tidyverse)

# Reading in data
data = read_csv("airport_pairs.csv")

#Question 1
# Filtering for data of flights in or out of RDU
RDUdata = filter(data, origin == "RDU" | dest == "RDU")
# Selecting the columns we need
RDUdataselected = subset(RDUdata, select = c(origin, dest, passengers))
# Sorting by passenger count
q1dataarranged = arrange(RDUdataselected, passengers)
# Filtering for more than 10000 passengers
q1filtered = filter(q1dataarranged, passengers >= 10000)
# ATL is most popular nonstop destination                 