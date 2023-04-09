library(tidyverse)
library(tidycensus)

# Reading in data
data = read_csv("airport_pairs.csv")

#Question 1
# Filtering for data of flights in or out of RDU
RDUdata = filter(data, origin == "RDU" | dest == "RDU")
# Selecting the columns we need
RDUdataselected = subset(RDUdata, select = c(origin, dest, passengers))
# Sorting by passenger count
RDUdataarranged = arrange(RDUdataselected, passengers)
# Filtering for more than 10000 passengers
filtered = filter(RDUdataarranged, passengers >= 10000)
# ATL is most popular nonstop destination                 


# Question 2
acs_data = load_variables(2021, "acs5")
write_csv(acs_data, "acsdata.csv")

census = get_acs(
  geography="cbsa",  # could be tract, block group, etc.
  variables=c(
    "total_mode"="B08301_001",
    "drove_alone"="B08301_003",
    "transit"="B08301_010",
    "taxi"="B08301_016",
    "motorcycle"="B08301_017",
    "bicycle"="B08301_018",
    "walk"="B08301_019",
    "other"="B08301_020",
    "wfh"="B08301_021",
    "total_ttime"="B08303_001",
    "ttime_60_89"="B08303_012",
    "ttime_90plus"="B08303_013"
  ),
  year=2021,
  state="NC",
  survey="acs5",
  output="wide"
)