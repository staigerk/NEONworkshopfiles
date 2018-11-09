#####NEON Set-up
  ## RUN 
#install.packages("devtools")
library("devtools", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
#install.packages("raster")
library("raster", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

source("http://bioconductor.org/biocLite.R")

install_github("NEONScience/NEON-utilities/neonUtilities")
install_github("NEONScience/NEON-geolocation/geoNEON")
biocLite("rhdf5")



###NEON Workshop Day 1
#Data for PAR for 2 sites, 2 months already downloaded from portal)

#load packages
library(neonUtilities)
library(geoNEON)
library(raster)
library(rhdf5)

#fix string handling
options(stringsAsFactors = FALSE)

# stack data from portal  (ie, stack across site/month in to csvs so you're not dealing with like 40 files...)
stackByTable("~/Downloads/NEON_par.zip")  #unless computer autounzips and doesn't retain zip, inwhichcase: folder=TRUE, remove .zip from filepath

# download some observational data using zipsByProduct() in neon utils
  #its a wrapper for the API
zipsByProduct(dpID="DP1.10098.001", site="WREF",
              package="expanded",check.size = TRUE, 
              savepath = "~/Downloads")      #savepath is file path to save into; default is into current wd

# and stack
stackByTable("~/Downloads/filesToStack10098", folder=TRUE)  #folder=true cause its a folder not a zip


#download remote sensing data... (AOP=airborne observations platform data)
#download by tile
byTileAOP(dpID = "DP3.30015.001", site="WREF",year="2017",
          easting=580000, northing = 5075000, savepath = "~/Downloads")


##Reading data in- PAR data
par30=read.delim("~/Downloads/NEON_par/stackedFiles/PARPAR_30min.csv", sep=",")
parvar=read.delim("~/Downloads/NEON_par/stackedFiles/variables.csv", sep=",")

#plot some stuff
#time series, convert time format
par30$startDateTime = as.POSIXct(par30$startDateTime,
                                 format="%Y-%m-%d T %H:%M:%S Z",tz="GMT")


#plot mean par at highest tower level
plot(PARMean~startDateTime, 
     data=par30[which(par30$verticalPosition==80),],
     type="l")


#veg structure data
#read in mapping and tagging table...
vegmap= read.delim("~/Downloads/filesToStack10098/stackedfiles/vst_mappingandtagging.csv",sep=",")
vegind= read.delim("~/Downloads/filesToStack10098/stackedfiles/vst_apparentindividual.csv", sep=",")


##geoNEON helps to spatially reference these babies so you can actually know where the trees are! nice!
vegmap= geoNEON::def.calc.geo.os(vegmap, "vst_mappingandtagging") 

#making a litte stem map!
veg=merge(vegind, vegmap, by=c("individualID","namedLocation","domainID","siteID","plotID"))

symbols(veg$adjEasting[which(veg$plotID =="WREF_080")],
        veg$adjNorthing[which(veg$plotID =="WREF_080")],
        circles=veg$stemDiameter[which(veg$plotID =="WREF_080")]/100, xlab="Easting",ylab="Northing",
        inches=FALSE)

#color in the living ones, leave the dead ones blank.
symbols(veg$adjEasting[which(veg$individualID %in% live & veg$plotID =="WREF_080")],
        veg$adjNorthing[which(veg$individualID %in% live & veg$plotID =="WREF_080")],
        circles=veg$stemDiameter[which(veg$individualID %in% live & veg$plotID =="WREF_080")]/100, xlab="Easting",ylab="Northing",
        inches=FALSE, add=TRUE, fg=par("red"), bg="red")


# AOP data
chm= raster("~/Downloads/DP3.30015.001/2017/FullSite/D16/2017_WREF_1/L3/DiscreteLidar/CanopyHeightModelGtif/NEON_D16_WREF_DP3_580000_5075000_CHM.tif/")
plot(chm, col=topo.colors(6))



##playing with NEON data:
##Goal: get distance to nearest neighbor

coords=cbind(as.numeric(veg$adjEasting),as.numeric(veg$adjNorthing))
coords=na.exclude(coords)
indivdists=pointDistance(coords, lonlat=FALSE, allpairs = TRUE)

#gets dist to nn, but not id of nn... for now
indivdists[indivdists %in% 0]=NA
nearest=apply(indivdists,1,min, na.rm=TRUE)
dbh=veg$stemDiameter[!veg$adjEasting %in% NA]

##does DBH predict distance to NN?
dbh=veg$stemDiameter[!veg$adjEasting %in% NA]
plot(dbh,nearest, log="y")

##does height?
height=veg$height[!veg$adjEasting %in% NA]
plot(height,nearest, log="y")

live=cbind(veg$individualID[grep("Live",veg$plantStatus)],veg$stemDiameter[grep("Live",veg$plantStatus)],veg$height[grep("Live",veg$plantStatus)])

###plot living / dead
symbols(veg$adjEasting[which(veg$plotID =="WREF_080")],
        veg$adjNorthing[which(veg$plotID =="WREF_080")],
        circles=veg$stemDiameter[which(veg$plotID =="WREF_080")]/100, xlab="Easting",ylab="Northing",
        inches=FALSE)

#color in the living ones, leave the dead ones blank.
symbols(veg$adjEasting[which(veg$individualID %in% live & veg$plotID =="WREF_080")],
        veg$adjNorthing[which(veg$individualID %in% live & veg$plotID =="WREF_080")],
        circles=veg$stemDiameter[which(veg$individualID %in% live & veg$plotID =="WREF_080")]/100, xlab="Easting",ylab="Northing",
        inches=FALSE, add=TRUE, fg=par("red"), bg="red")



####OK back to work: API tutorial
  ##useful to query data in an automated way, or get spatial data, etc.

#tutorial at: https://www.neonscience.org/neon-api-usage

#load some packages...
install.packages("httr")
install.packages("jsonlite")
install.packages("downloader")

library(httr)
library(jsonlite)
library(downloader)

req= GET("http://data.neonscience.org/api/v0/products/DP1.10003.001")
 #this just kinda tells u that the page exists...
req.content=content(req,as="parsed")  #get the content from the request-- still kinda shitty to look at tho.

available=fromJSON(content(req, as="text"))
available$data$siteCodes  #which sites which months have data

bird.urls= unlist(available$data$siteCodes$availableDataUrls)  #the urls where all the avialable data is at

#figured out what's available, start to get the data...
bird=GET(bird.urls[grep("WOOD/2015-07", bird.urls)])
bird.files=fromJSON(content(bird, as="text"))

bird.count=read.delim(bird.files$data$files$url[intersect(grep("countdata",bird.files$data$files$name),grep("basic",bird.files$data$files$name))], sep=",")

##taxonomy endpoint from API

loon.req= GET("http://data.neonscience.org/api/v0/taxonomy/?family=Gaviidae")
loon.list=fromJSON(content(loon.req, as="text"))


##or:
loon.req= GET("http://data.neonscience.org/api/v0/taxonomy/?family=Gaviidae&verbose=TRUE")
loon.list=fromJSON(content(loon.req, as="text"))


###Day 2 NEON:




