## Install and load packages
#install.packages("XML")
library(XML)
#install.packages("RCurl")
library(RCurl)
#install.packages("igraph")
library(igraph)
#install.packages("sfsmisc")
library(sfsmisc)
#install.packages("ggplot2")
library(ggplot2)


#Disable scientific notation
options(scipen=10000)

## Scrape Wikipedia
#Give path
path<-"https://de.wikipedia.org/wiki/Liste_der_Gro%C3%9F-_und_Mittelst%C3%A4dte_in_Deutschland"
# Umlaute umcodiert
# ä: %C3%A4
# ß: %C3%9F

#Scrape website and format
webpage <- getURL(path)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
data    <- readHTMLTable(webpage)
data    <- data[[2]]

## Prepare data
data$`2014`    <- as.numeric(gsub("\\.", "", data$`2014`))
data           <- data[, c("Name", "2014")]
colnames(data) <- c("name", "size")

## Figures
# Histogram of city sizes
#Generate histogram
ggplot(data, aes(size)) + geom_histogram(binwidth = 10000) + xlab("City size") + ylab("Number of cites N(s)")

# Logged density plot
bins      <- seq(0, max(data$size)+10000, 10000) #define bins
counts    <- hist(data$size, breaks = bins)$counts #get counts
data.freq <- data.frame(bins=head(bins, -1), counts=counts) #get data set
data.freq <- data.freq[!(data.freq$counts==0),,] #Subset to disregard sizes with count = 0
#Generate logged scatterplot
ggplot(data.freq, aes(x=bins, y=counts)) + geom_point(shape=1) + scale_x_log10() + scale_y_log10() + xlab("City size s (log)") + ylab("Counts of cities of size s in Germany N(s) (log)")

## Estimate the dimension of German cities
#Estimating the dimension d using MLE
d   <- 1+length(data$size)*1/sum(log(data$size/(min(data$size))))
d
