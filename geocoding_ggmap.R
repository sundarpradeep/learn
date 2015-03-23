install.packages("xlsx")
install.packages("gdata")
install.packages("dplyr")

install.packages("ggmap")
install.packages("ggplot2")

library("xlsx")
library("gdata")
library("ggmap")
library("ggplot2")

setwd("./dev/data/NYC421Data")

# step - 1 : Read Files 

allfiles <- dir(pattern="*.xls")
consolidated <- data.frame()

for(i in 1:length(allfiles)){
  addressfile <- NULL 
  addressfile <- read.xlsx(allfiles[i],  
                           1,
                           startRow=5,
                           #                      rowIndex=5:9,
                           header=TRUE,
                           colIndex=c(8,9)
  )
  addressfile$fileId <- i 
  if (length(consolidated) == 0 ){
    consolidated <- addressfile 
  }
  else {
    consolidated <- rbind(consolidated,addressfile)
  }            
}

consolidated$full.address <- paste(trim(consolidated$ADDRESS),consolidated$ZIP.CODE,"NEW YORK USA",sep=" ") 
consolidated <- tbl_df(consolidated)
consolidated.unique <- distinct(select(consolidated, fileId,full.address))

# Step : 2 - Geocode File

class(locations)
options(BingMapsKey='AgyqIM0pl1I8dJr8Tfb7efnQAChYXjdaSi73yGP1XVDITzldbFAaJKzTfZR3QzJX')

coordinates<-sapply(consolidated.unique$full.address,function(x) taRifx.geo::geocode(x,service='bing',returntype='coordinates'))

coordinates.final<- as.data.frame(t(as.data.frame(coordinates)))

write.csv(coordinates.final,file="./all.421.coordinates.csv")

# Step : 4 - Plot the geocodes in ggmap..

nyc.map <- get_map('New York City',zoom=10,maptype="roadmap")

pdf("./nycplot.pdf")

ggmap(nyc.map, extent = 'panel') +
  geom_point(aes(x = V2, y = V1), data = coordinates.final, alpha = .75,color="darkred",size=1)

dev.off()




#Create animation in HTML file
saveHTML({
  for (i in 1:nrow(countryLatLonDF)) 
    myMap <- ggmap(get_map(location = c(lat=0, lon=0), color="color",source="google", maptype="terrain", zoom=2))
{     #Get the map
      myMap <- myMap + geom_point(data = countryLatLonDF[i,], aes(x = lon, y = lat, color = countryname, alpha = 0.5, fill = "red"), size = 5, shape = 21) + geom_text(data = countryLatLonDF[i,], aes(x = lon, y = lat, label = countryname), size = 3, vjust = 0, hjust = -0.1, color = "blue") + scale_colour_discrete(name  = "countryname")
      print(myMap)
    } 

}, img.name = "anim_plot", imgdir = "anim_dir", htmlfile = "anim.html", autobrowse = FALSE, title = "Country animation", verbose =FALSE, interval = 2)

graphics.off()

