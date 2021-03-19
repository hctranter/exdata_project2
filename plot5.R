# Download and read data.
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

library(downloader)

if(!(file.exists("Source_Classification_Code.rds")&
     file.exists("Source_Classification_Code.rds"))){
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


## Emissions for each sub group for each year. 
finalbalt <- NEI %>% 
        filter((fips == "24510")&(type == "ON-ROAD")) %>%
        mutate(vehiclefuel = case_when(SCC %in% lightgas ~ "Light Gas", 
                               SCC %in% heavygas ~ "Heavy Gas",
                               SCC %in% lightdie ~ "Light Diesel",
                                SCC %in% heavydie ~ "Heavy Diesel"))%>%
        group_by(year, vehiclefuel)%>%
        summarise(totalemissions = sum(Emissions))%>%
        mutate(county = "Baltimore City")
       
finalbalt$year <- as.factor(finalbalt$year)

png("plot5.png")
ggplot(finalbalt, aes(fill=vehiclefuel, y=totalemissions, x=year)) + 
        geom_bar(stat = "identity", position = position_dodge(0.8),width=0.7) +
        labs(x = "Year", 
        y = "Emissions of PM2.5 (tons)", 
        title = "Emissions of PM2.5 from Motor Vehicles\n in Baltimore City") +
        scale_fill_discrete(name = "Vehicle and Fuel type") +
        theme_bw(base_family = "Times") +
        theme(plot.title = element_text(hjust=0.5)) +
        theme(axis.text = element_text(size = 12), 
              axis.title = element_text(size =12))
dev.off()
