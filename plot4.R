# Download and read data.
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

library(downloader)

if(!(file.exists("Source_Classification_Code.rds")&file.exists("Source_Classification_Code.rds"))){
        download(url, "./exdata_data_NEI_data.zip")
        unzip("exdata_data_NEI_data.zip",exdir = "./")
        unlink("exdata_data_NEI_data.zip")}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Use the EI.Sector variable to identify SCC codes with keywords "combustion" 
# and "coal".
SCCcodes <- SCC$SCC[grep(".*\\bComb\\b.*\\bCoal\\b",SCC$EI.Sector)]

# Recode NA's in SCC column in NEI data set, 
NEI[NEI == "   NA"] <- NA

# Calculate the emissions from coal combustion for each state in each year. 
library(dplyr)
coalstate <- NEI %>%
        filter(SCC %in% SCCcodes) %>%
        mutate(state = str_sub(fips,1,2)) %>%
        group_by(year,state) %>%
        summarise(coalemissions = sum(Emissions)) %>%
        print()

#Sina plot with box plot layer.

library(ggplot2)
library(ggforce)

png("plot4.png")
coalstate$year <- as.factor(coalstate$year)
ggplot(coalstate, aes(x = year, y= coalemissions)) + 
        geom_sina(method = "stack", color = "blue",alpha = 0.5, maxwidth = 0.9) + 
        geom_boxplot(width = 0.1, color = "red", alpha = 0.5, outlier.shape = NA) +
        labs(x = "Year", y = "Emissions of PM2.5 (tons)", 
             title = "Distribution of Emissions of PM2.5 from \nCoal Combustion-related sources across US States") + 
        theme_bw(base_family = "Times", ) + 
        theme(plot.title = element_text(hjust=0.5)) +
        theme(axis.text = element_text(size = 12), axis.title = element_text(size =12))
dev.off()



