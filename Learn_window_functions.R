# replicate from dplyr vignettes
# https://cran.r-project.org/web/packages/dplyr/vignettes/window-functions.html
library(Lahman)
batting <- select(tbl_df(Batting), playerID, yearID, teamID, G, AB:H) # select certain columns for building new dataframe
batting <- arrange(batting, playerID, yearID, teamID)  # order the rows 
players <- group_by(batting, playerID)  # you can store the grouped dataframe, look the same as the original df but it's different

# For each player, find the two years with most hits, H column
View(filter(players, min_rank(desc(H)) <= 2 & H > 0))
# Within each player, rank each year by the number of games played
View(mutate(players, G_rank = min_rank(G)))  # created a new column indicating the rank based on G within each player

# For each player, find every year that was better than the previous year
View(filter(players, G > lag(G)))
# For each player, compute avg change in games played per year
View(mutate(players, G_change = (G - lag(G)) / (yearID - lag(yearID))))

# For each player, find all where they played more games than average
View(filter(players, G > mean(G)))
# For each player, compute a z score based on number of games played
mutate(players, G_z = (G - mean(G))/sd(G))



# There are five main families of window functions.
# Two families are unrelated to aggregation functions: 
# 1. Ranking and ordering functions: row_number(), min_rank(), dense_rank(), cume_dist(), percent_rank(), and ntile()
# these functions take a vector to order by, and return various types of ranks
# 2. Offsets lead() and lag() allow you to access the previous and next values in a vector, making it easy to compute differences and trends

# The other three families are variations on familiar aggregate functions:
# 1. Cummulative aggregates: cumsum(), cummin(), cummax(), and cumall(), cumany(), and cummean()
# 2. Rolling aggregates operate in a fixed width window. 
# 3. Recycled aggregates, when an aggregate is repeated to match the length of the input. 

# Ranking functions
x <- c(1,1,2,2,2)

row_number(x)
min_rank(x)
dense_rank(x)
cume_dist(x)
percent_rank(c(1,1,2,3,4,5))

# Select best two years
View(filter(players, min_rank(desc(G)) <= 2))
# Select best 10% of years
filter(players, cume_dist(desc(G)) < 0.1)

# ntile() divides the data up into n evenly sized buckets. 
# we could use ntile() to divide the players within a team into four ranked groups, and calculate the average number of games within each group.
by_team_player <- group_by(batting, teamID, playerID) # groups: teamID, playerID
by_team <- summarise(by_team_player, G = sum(G)) # groups: teamID
by_team_quartile <- group_by(by_team, quartile = ntile(G, 4)) # groups: quartile
summarise(by_team_quartile, mean(G))

# Lead and lag
x <- 1:5
lead(x)
lag(x)
# use them to compute differences or percent changes, find out when a value changes
# Find when a player changed teams
filter(players, teamID != lag(teamID))

# use order_by argument when needed to
df <- data.frame(year=2000:2005, value=(0:5)^2)
scrambled <- df[sample(nrow(df)),] 

wrong <- mutate(scrambled, running = cumsum(value))
arrange(wrong, year)

right <- mutate(scrambled, running = order_by(year, cumsum(value)))
arrange(right, year)


# Cumulative aggregates
# cumany() and cumall() are useful for selecting all rows up to, or all rows after, a condition is true for the first (or last) time
# find all records for a player after they played a year with 150 games
View(filter(players, cumany(G > 150)))  # any after 150


View(filter(players, cumall(G > 150))) # after 150 

x <- 1:10
y <- 10:1
cumsum(x)
order_by(y, cumsum(x)) # Don't understand this

# Recycled aggregates
filter(players, G > mean(G))
filter(players, G < median(G))



ID <- c(1,1,1,1,1,1,2,2,2,2,2,2)
hour <- c(0, 1, 3.5, 4, 5, 6, 1, 1.2, 2.6, 3.7, 4.2, 8)
df <- data.frame(ID, hour)
by_ID <- group_by(df, ID)
by_ID_2 <- mutate(by_ID, hour2 = hour)
filter(by_ID_2, hour2 - hour >=3)









