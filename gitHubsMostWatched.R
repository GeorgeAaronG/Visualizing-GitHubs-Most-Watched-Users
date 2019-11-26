# GitHub's Most Watched Users
#
# 1. Create an app to access the GitHub API.
# 2. Collect data on a list of the 25 most "watched" users and their repositories.
# 2.1. Create a bar chart of the frequency count of the programming languages used in the repositories.
# 2.2. Create a bar chart of the top 15 most frequently used programming languages in the repositories in descending order.
# 2.3. Create two separate pie charts of the repositories by has_wiki and by has_downloads separately.
# 2.4. Create a trend plot of the last 100 rows of data based on created_at.
# 2.5. Create a heat map of key user statistics by user

#################
# 1. Create an app to access the GitHub API.

# Libraries, authentication, and data
library(github)
library(ggplot2)
library(data.table)
library(sqldf)
library(gplots)

client.id <- "3e25d0c15a9375141be5"
client.secret <- "fd532a83689e8de383e7326b3dacfda76608b332"
ctx <- interactive.login(client.id, client.secret)


#################
# 2. Collect data on a list of the 25 most "watched" users and their repositories.

activeusers <- read.csv("mostWatchedUsers2.csv")

# Change date to format supported by R
format.git.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%SZ", tz = "GMT")
}

# Updating the column with new date format
activeusers$created_at <- format.git.date(activeusers$created_at)
activeusers$updated_at <- format.git.date(activeusers$updated_at)
activeusers$pushed_at <- format.git.date(activeusers$pushed_at)
View(activeusers)

# Selecting 23 subset columns from 66 active user traits
colnames(activeusers)
ausersubset <- activeusers[,c("id","name","full_name","private","description","fork","created_at","updated_at","pushed_at","homepage","size","stargazers_count","watchers_count","language","has_issues","has_downloads","has_wiki","has_pages","forks_count","open_issues_count","forks","open_issues","watchers")]
View(ausersubset)

# Replace True and False with 1 and 0
ausersubset$private <- as.integer(ausersubset$private)
ausersubset$fork <- as.integer(ausersubset$fork)
ausersubset$has_issues <- as.integer(ausersubset$has_issues)
ausersubset$has_downloads <- as.integer(ausersubset$has_downloads)
ausersubset$has_wiki <- as.integer(ausersubset$has_wiki)
ausersubset$has_pages <- as.integer(ausersubset$has_pages)

# Return the user name ONLY under 'full name'
ausersubset$full_name <- sapply(strsplit(as.character(ausersubset$full_name), split='/', fixed=TRUE), function(x) (x[1]))
head(ausersubset$full_name)

# Flag for presence of website/webpage and add column to subset
ausersubset$has_web <- as.numeric(grepl(".", ausersubset$homepage))

# Length of the description added to subset
ausersubset$desclen <- nchar(as.character(ausersubset$description))

# Finding the number of days that have passed and adding to the subset
ausersubset$dayscreated <- as.integer(difftime(Sys.Date(),ausersubset$created_at , units = c("days")))
ausersubset$daysupdated <- as.integer(difftime(Sys.Date(),ausersubset$updated_at , units = c("days")))
ausersubset$dayspushed <- as.integer(difftime(Sys.Date(),ausersubset$pushed_at , units = c("days")))

# There are a total of 28 features now.



##########################
# 2.1. Create a bar chart of the frequency count of the programming languages used in the repositories.

lang <- table(ausersubset$language)
lang <- data.frame(lang)
order(lang$Freq)
lang[order(-lang$Freq),]
barplot(lang$Freq,  names.arg = lang$language, main="Histogram for Language", xlab="Language", ylab="Frequency")


#########################
# 2.2. Create a bar chart of the top used programming languages in the repositories in descending order.

a <- table(ausersubset$language)
a <- as.data.frame(a)
a <- a[with(a, order(-Freq)), ]
colnames(a) <- c("Language","Count")
barplot(toplang$Count,  names.arg = toplang$Language, main="Histogram for Language", xlab="Language", ylab="Frequency", las=2, cex.names=.8)



#############################
# 2.3. Create two separate pie charts of the repositories by has_wiki and by has_downloads separately.

pie1 <- ggplot(ausersubset, aes(x = factor(1), fill = factor(ausersubset$has_wiki))) + geom_bar(width = 1) 
pie1 + coord_polar(theta = "y")

pie2 <- ggplot(ausersubset, aes(x = factor(1), fill = factor(ausersubset$has_downloads))) + geom_bar(width = 1) 
pie2 + coord_polar(theta = "y")



##############################
# 2.4. Create a trend plot of the last 100 rows of data based on created_at.

# Transform to date type
trenddata <- ausersubset[c("created_at", "id")]
trenddata$created_at <- as.POSIXct(strptime(trenddata$created_at, "%Y-%m-%d"))


tdata <- table(trenddata$created_at)
tdata <- as.data.frame(tdata)
head(tdata)
colnames(tdata) <- c("Date","Repositories")
tdata$Date <- as.Date(tdata$Date)

# Returns last 100 rows
tdata1 <- tail(tdata, 100)

q <- ggplot(data=tdata1, aes(x=Date, y=Repositories, group=1)) +
     geom_line() + 
     geom_point()

q + theme(axis.text.x = element_text(angle = 90, hjust = 1))



####################################
# 2.5. Create a heat map of key user statistics by user

# Display column names
colnames(ausersubset)

# Create a subset of data with selected columns
newdata <- ausersubset[c("id","full_name","size","watchers_count", "forks_count", "open_issues_count", "desclen", "dayscreated", "daysupdated", "dayspushed")]

# Executes SQL query on data frame
sd <- sqldf("select full_name, count(id), avg(size),  sum(watchers_count), sum(forks_count), sum(open_issues_count), avg(desclen), avg(dayscreated), avg(daysupdated), avg(dayspushed) from newdata group by full_name")
colnames(sd) <- c("Name", "Repositories", "AverageSize", "Watchers", "Forks", "Issues", "Avg_desc_length", "Avg_days_since_created", "Avg_days_since_updated", "Avg_days_since_pushed")

# Set row name to Name of user in data frame
row.names(sd) <- sd$Name

# Sort by Repositories in descending order and remove names
sd <- sd[order(-sd$Repositories),] 
sd <- sd[,2:10]
View(sd)

# Convert to a matrix
sdmat <- as.matrix(sd)

# Create heatmap without color key using heatmap function
sd_heatmap <- heatmap(sdmat, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,8))

# With color key
# Create headmap with color key using heatmap.2 function
sd_heatmap <- heatmap.2(sdmat, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(8,8), key=TRUE)
