########### Take Home Exercises ############

# flights dataset

data("flights")

# combine the destination coordinates with a precision of two decimals in a single variable
flights$combined_destination <- sprintf("%.2f:%.2f", flights$dest_lat, flights$dest_long)
head(flights)

# obtain the total number of filghts that arrived at each airport
arrived_at_each_airport <- table(flights$combined_destination)
View(arrived_at_each_airport)

# order the variable in descending order
# here, order() returns the index of ordered list
arrived_at_each_airport <- arrived_at_each_airport[order(arrived_at_each_airport, decreasing = T)]
View(arrived_at_each_airport)

# get the top ten airports
top_ten <- arrived_at_each_airport[1:10]
View(top_ten)

# now obtain the origin information (lat, lon) of the flights that travelled to the top 10 destinations
top_ten_airports <- names(top_ten)
top_ten_airports
top_10_flights <- subset(flights, combined_destination %in% top_ten_airports)
View(top_10_flights)

# create a data frame that includes the origin and destination of all the flights to the top 10 destinations
colnames(flights)
top_10_flights <- top_10_flights[, 1:4]
View(top_10_flights)

# plot the data to visualize results
globejs(arcs = top_10_flights,
        arcsColor = "green",
        arcsLwd = 2,
        arcsOpacity = 0.7,
        atmosphere = T)
