
GBIF_MAPS_COUNTY<-function(CSV_FILE,SHAPEFILE,TAXONOMY,out_dir,input_dir){
  #####################################################################################  
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
#####################################################################################

cat("Processing for ",TAXONOMY," level","\n")

if(TAXONOMY=="species"){
  x<-as.data.frame(cbind(as.character(USA_CSV$scientificname[]),as.numeric(as.character(USA_CSV$decimallongitude[])),as.numeric(as.character(USA_CSV$decimallatitude[]))))
}else if(TAXONOMY=="genus"){
x<-as.data.frame(cbind(as.character(USA_CSV$genus[]),as.numeric(as.character(USA_CSV$decimallongitude[])),as.numeric(as.character(USA_CSV$decimallatitude[]))))
}else if(TAXONOMY=="family"){
x<-as.data.frame(cbind(as.character(USA_CSV$family[]),as.numeric(as.character(USA_CSV$decimallongitude[])),as.numeric(as.character(USA_CSV$decimallatitude[]))))
}

x<-x[complete.cases(x),]

gc()

if(TAXONOMY=="species"){
  colnames(x)<-c("taxon_final","lon","lat")
}else if(TAXONOMY=="genus"){
  colnames(x)<-c("genus","lon","lat")
}else if(TAXONOMY=="family"){
  colnames(x)<-c("family","lon","lat")
}

x$lon<-as.numeric(as.character(x$lon))
x$lat<-as.numeric(as.character(x$lat))

gc()
# #####################################################################################
cat("Overlapping coordinates and counties","\n")

x2<-SpatialPointsDataFrame(x[,2:3],as.data.frame(x))
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
fin<-matrix(nrow =length(counties),ncol =7)


if(TAXONOMY=="species"){
  colnames(fin)<-c("AFFGEOID","taxon_1","taxon_2","taxon_3","freq_1","freq_2","freq_3")  
}else if(TAXONOMY=="genus"){
  colnames(fin)<-c("AFFGEOID","genus_1","genus_2","genus_3","freq_1","freq_2","freq_3")
}else if(TAXONOMY=="family"){
  colnames(fin)<-c("AFFGEOID","family_1","family_2","family_3","freq_1","freq_2","freq_3")
}

gc()
#####################################################################################
cat("Making pivot table for ",TAXONOMY," level","\n")


for(i in 1:length(counties)){
  y<-x4[which(x4$AFFGEOID==counties[i]),]
  if(TAXONOMY=="species"){
    count<-as.data.frame(table(y$taxon_final)) 
  }else if(TAXONOMY=="genus"){
    count<-as.data.frame(table(y$genus))
    }else if(TAXONOMY=="family"){
      count<-as.data.frame(table(y$family))    
  }
 
  count<-count[which(count$Freq>0),]
  count<-count[order(-count$Freq),]
  
if(nrow(count)>0){
fin[i,1]<-counties[i]
fin[i,2]<-as.character(count[1,1])
fin[i,3]<-as.character(count[2,1])
fin[i,4]<-as.character(count[3,1])
fin[i,5]<-count[1,2]
fin[i,6]<-count[2,2]
fin[i,7]<-count[3,2]
}else{
  fin[i,1]<-counties[i]
  fin[i,2]<-NA
  fin[i,3]<-NA
  fin[i,4]<-NA
  fin[i,5]<-NA
  fin[i,6]<-NA
  fin[i,7]<-NA  
  
  }
};rm(i,y,count)

gc()

#####################################################################################

fin<-as.data.frame(fin)
fin<-fin[which(!is.na(fin[,2])),]

cat("Writing excel file for ",TAXONOMY," level","\n")


if(TAXONOMY=="species"){
  write.xlsx(fin,paste0(out_dir,"/","taxon_final_",Sys.Date(),".xls"),row.names=F,showNA = F,sheetName="taxon_final")
}else if(TAXONOMY=="genus"){
  write.xlsx(fin,paste0(out_dir,"/","genus_",Sys.Date(),".xls"),row.names=F,showNA = F,sheetName="genus")
}else if(TAXONOMY=="family"){
  write.xlsx(fin,paste0(out_dir,"/","family_",Sys.Date(),".xls"),row.names=F,showNA = F,sheetName="family")
}
gc()
#####################################################################################
cat("     ","\n")
cat("#####","\n")
cat("DONE!","\n")
cat("#####","\n")
cat("     ","\n")
}

SHAPEFILE<-"cb_2015_us_county_500k.shp"
CSV_FILE<-"0010885-160910150852091.csv"
input_dir<-"E:/ADMIN/cb_2015_500K"
out_dir<-"E:/ADMIN/cb_2015_500K"  

TAXONOMIES<-c("species","genus","family")

lapply(1:length(TAXONOMIES),function(i){
TAXONOMY<-TAXONOMIES[[i]]
  xy<-GBIF_MAPS_COUNTY(CSV_FILE,SHAPEFILE,TAXONOMY,out_dir,input_dir)
  
})
