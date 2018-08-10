#Convert character vectors to factor vector based on the formula:

unique(df$characterVector) / nrow(df$characterVector)

#If the ratio of unique values to total values is lower than .5, then convert vector to factor.
#Ideally we would expect this ratio to be very low... <.1 is ideal.
unique(df$characterVector) / nrow(df$characterVector) < .5
