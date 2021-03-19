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

# Create table of total emissions by year and plot.

library(dplyr)
totalem <- NEI %>%
        group_by(year) %>%
        summarise(totalemissions = sum(Emissions)) %>%
        mutate(totalemissionsmt = totalemissions/1000000)%>%
        print()

png("plot1.png")
totalem <- transform(totalem, year = factor(year))
barplot(totalemissionsmt~year, totalem, space = 0.7, col = "#FF9999",
        xlab = "Year", ylab = "Emissions of PM2.5 (in million tons)", 
        main="Total Emissions of PM2.5 from all sources in the US")
dev.off()

