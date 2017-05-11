





GBIF_MAPS_COUNTY_COUNT<-function(CSV_FILE,SHAPEFILE,TAXONOMY,out_dir,input_dir){
  #####################################################################################  
  options(java.home="C:\\Program Files\\Java\\jre1.8.0_101")
  
  require(rgdal);require(raster);require(shapefiles)
  require(ffbase);require(dplyr);require(xlsx)
  




################################################################
options(java.home="C:\\Program Files\\Java\\jre1.8.0_101")

require(rgdal);require(raster);require(shapefiles)
require(ffbase);require(dplyr);require(xlsx)

#####################################################################################  

cat("     ","\n") 
cat("MORE FREQUENT TAXON PER COUNTY SCRIPT BEGINNING","\n")
cat("     ","\n")

#####################################################################################  
cat("Reading USA Census.gv  counties shapefile","\n")

USA_SHP<-shapefile(paste0(input_dir,"/",SHAPEFILE))

gc()
#####################################################################################
cat("Reading GBIF file","\n")

USA_CSV <- read.csv.ffdf(file=paste0(input_dir,"/",CSV_FILE),
                         encoding="UTF-8",sep = "\t",
                         VERBOSE = TRUE,na.strings="",first.rows = 50000,
                         next.rows = 50000,quote="",header=T,colClasses="factor")

gc()

x<-as.data.frame(cbind(as.numeric(as.character(USA_CSV$decimallongitude[])),as.numeric(as.character(USA_CSV$decimallatitude[]))))
x<-x[complete.cases(x),]
colnames(x)<-c("lon","lat")
x$lon<-as.numeric(as.character(x$lon))
x$lat<-as.numeric(as.character(x$lat))
gc()
# #####################################################################################
cat("Overlapping coordinates and counties","\n")

x2<-SpatialPointsDataFrame(x[,1:2],as.data.frame(x))
projection(x2)<-CRS("+proj=longlat +datum=WGS84")

CRS.new <- crs(USA_SHP) # (@mdsumner points out that

sp_new <- spTransform(x2, CRS.new)

x3<-over(sp_new,USA_SHP)

x4<-cbind(x,x3)

gc()
#####################################################################################

cat("Making counties list","\n")


counties<-unique(x4$AFFGEOID)
counties<-counties[!is.na(counties)]
fin<-matrix(nrow =length(counties),ncol =2)
colnames(fin)<-c("AFFGEOID","COUNT")

#####################################################################################
cat("Making pivot table for "," count"," level","\n")


for(i in 1:length(counties)){
  y<-x4[which(x4$AFFGEOID==counties[i]),]

      count<-as.data.frame(table(y$AFFGEOID)) 

  
  count<-count[which(count$Freq>0),]
  count<-count[order(-count$Freq),]
  
  if(nrow(count)>0){
    fin[i,1]<-counties[i]
    fin[i,2]<-as.character(count[1,2])
    
  }else{
    fin[i,1]<-counties[i]
    fin[i,2]<-NA

  }
};rm(i,y,count)

gc()

#####################################################################################
fin<-as.data.frame(fin)
fin<-fin[which(!is.na(fin[,2])),]
#####################################################################################
write.xlsx(fin,paste0(out_dir,"/","COUNT_",Sys.Date(),".xls"),row.names=F,showNA = F,sheetName="COUNT")
#############################################################


}
SHAPEFILE<-"cb_2015_us_county_500k.shp"
CSV_FILE<-"0010885-160910150852091.csv"
input_dir<-"E:/ADMIN/cb_2015_500K"
out_dir<-"E:/ADMIN/cb_2015_500K"  


x<-GBIF_MAPS_COUNTY_COUNT(CSV_FILE,SHAPEFILE,TAXONOMY,out_dir,input_dir)