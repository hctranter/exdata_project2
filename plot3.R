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

# Gathering relevant data and plotting. 
library(dplyr)
library(ggplot2)

typebolt <- NEI %>%
        filter(fips == "24510") %>%
        group_by(type, year) %>%
        summarise(totalemissions = sum(Emissions)) %>%
        print()

png("plot3.png")
typebolt$year <- as.factor(typebolt$year)

ggplot(typebolt, aes(fill=type, y=totalemissions, x=year)) + 
    
        geom_bar(stat = "identity", position = position_dodge(0.8),width=0.7) +
        
        labs(y = "Emissions of PM2.5 (tons)", x = "Year",
             title = "Total Emissions of PM2.5 in Baltimore City") +
        
        scale_fill_discrete(name = "Type of Source") +
        
        theme_bw(base_family = "Times") +
       
         theme(plot.title = element_text(hjust=0.5)) 
dev.off()


