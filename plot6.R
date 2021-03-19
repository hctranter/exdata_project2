# Download and read data.
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

library(downloader)

if(!(file.exists("Source_Classification_Code.rds")
     &file.exists("Source_Classification_Code.rds"))){
        download(url, "./exdata_data_NEI_data.zip")
        unzip("exdata_data_NEI_data.zip",exdir = "./")
        unlink("exdata_data_NEI_data.zip")}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


## Getting SCC codes for on road vehicle Sub group.
lightgas <- SCC$SCC[SCC$EI.Sector == 
                            "Mobile - On-Road Gasoline Light Duty Vehicles"]
heavygas <-SCC$SCC[SCC$EI.Sector == 
                           "Mobile - On-Road Gasoline Heavy Duty Vehicles"]
lightdie <- SCC$SCC[SCC$EI.Sector == 
                            "Mobile - On-Road Diesel Light Duty Vehicles"]
heavydie <- SCC$SCC[SCC$EI.Sector == 
                            "Mobile - On-Road Diesel Heavy Duty Vehicles"]

## Emissions for each vehicle sub group for each year in Baltimore City. 
finalbalt <- NEI %>% 
        filter((fips == "24510")&(type == "ON-ROAD")) %>%
        mutate(vehiclefuel = case_when(SCC %in% lightgas ~ "Light Gas", 
                                       SCC %in% heavygas ~ "Heavy Gas",
                                       SCC %in% lightdie ~ "Light Diesel",
                                       SCC %in% heavydie ~ "Heavy Diesel"))%>%
        group_by(year, vehiclefuel)%>%
        summarise(totalemissions = sum(Emissions))%>%
        mutate(county = "Baltimore City")


## Emissions for each vehicle sub group for each year in LA county. 
finalla <- NEI %>% 
        filter((fips == "06037")&(type == "ON-ROAD")) %>%
        mutate(vehiclefuel = case_when(SCC %in% lightgas ~ "Light Gas", 
                                       SCC %in% heavygas ~ "Heavy Gas",
                                       SCC %in% lightdie ~ "Light Diesel",
                                       SCC %in% heavydie ~ "Heavy Diesel"))%>%
        group_by(year, vehiclefuel)%>%
        summarise(totalemissions = sum(Emissions))%>%
        mutate(county = "Los Angeles County")

## Combine data frames for LA county and Baltimore.
labalt <- rbind(finalbalt, finalla)

## Plotting.
png("plot6.png")
labalt$year<- as.factor(labalt$year)
ggplot(labalt, aes(fill=vehiclefuel, y=totalemissions, x=year)) +
        geom_bar(stat = "identity", position = position_dodge(0.8),width=0.7) + 
        facet_grid(county~.,scales = "free") +
        labs(x = "Year", 
             y = "Emissions of PM2.5 (tons)", 
             title = "Emissions of PM2.5 from Motor Vehicles") +
        scale_fill_discrete(name = "Vehicle and Fuel Type") +
        theme_bw(base_family = "Times") +
        theme(plot.title = element_text(hjust=0.5)) +
        theme(strip.text.y = element_text(size = 12),
              axis.text = element_text(size = 12),
              axis.title = element_text(size =12))
dev.off()

