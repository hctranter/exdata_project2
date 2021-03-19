# Download and read data sets.

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

library(downloader)

if(!(file.exists("Source_Classification_Code.rds")
     &file.exists("Source_Classification_Code.rds"))){
        download(url, "./exdata_data_NEI_data.zip")
        unzip("exdata_data_NEI_data.zip",exdir = "./")
        unlink("exdata_data_NEI_data.zip")}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Collect relevant data and plot.
library(dplyr)

totalbalt <- NEI %>%
        filter(fips == "24510") %>%
        group_by(year) %>%
        summarise(totalemissions = sum(Emissions)) %>%
        print()

png("plot2.png")
totalbalt <- transform(totalbalt, year = factor(year))
barplot(totalemissions~year, totalbalt, space = 0.7, col = "#FF6633",
        xlab = "Year", ylab = "Emissions of PM2.5 (tons)", 
        main="Total Emissions of PM2.5 from all sources in Baltimore City")
dev.off()
