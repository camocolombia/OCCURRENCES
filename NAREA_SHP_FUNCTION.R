
NAREA_FUNCTION<-function(shp_path,layer_name,narea_path,spName){

  #######################################################################################################
  ###LOADING PACKAGES
  #######################################################################################################
 require(shapefiles);require(raster);library(rgeos);require(rgdal)
  
  setwd(narea_path) 
  
  cat("Reading shapefile","\n")
  
  #######################################################################################################
  ###LOADING SHAPEFILES
  #######################################################################################################  
  
STATE<-readOGR(dsn=path.expand(shp_path), layer=layer_name)
COUNTY<-readOGR(dsn=path.expand(shp_path2), layer=layer_name2)
gc()

  #######################################################################################################
  ###LOADING CSV FILE
  #######################################################################################################
cat("Reading csv file with GRIN species Native areas","\n")

list_NA<-read.csv(paste0(narea_path,"/","NAREAS.csv"),header = T,sep = "|",na.strings = "")
list_NA<-list_NA[which(!is.na(list_NA$full_geo_id_USA)),]

  #######################################################################################################
  ###TAXA FIXES
  #######################################################################################################


spName2<-sub(" ","_",spName)
spName2<-sub(" ","_",spName2)
spName2<-sub("var. ","var._",spName2)
spName2<-sub("subsp. ","subsp._",spName2)
spName2<-sub("f. ","f._",spName2)
spName2<-sub("nothosubsp. ","nothosubsp._",spName2)

  #######################################################################################################
  ###MATCHING AND SEARCHING STATUS
  #######################################################################################################

cat("Matching ",spName, "with Shapefile","\n")

list_SP<-list_NA[which(list_NA$Taxon==spName),]
list_SP<-list_SP[which(list_SP$STATUS==1),]
list_SP<-list_SP[which(!is.na((list_SP$full_geo_id_USA))),]

list_approach<-as.character(unique(list_SP$LIST))



  #######################################################################################################
  ###EXTRACTING COUNTIES OR STATES
  #######################################################################################################

counties<-as.character(unique(list_SP$full_geo_id_USA))
counties<-factor(counties)


  #######################################################################################################
  ###USING COUNTIES OR STATES
  #######################################################################################################


        #######################################################################################################
        ###USING COUNTIES
        #######################################################################################################


if(any(list_approach=="COUNTY/GRIN"|list_approach=="COUNTY",na.rm = T)){
  
 cat("Using COUNTY APPROACH","\n")########
  
shp<-COUNTY
  
if(nrow(list_SP)>0){
  
  cat("Subsetting  ",spName,"\n")
  
  #######################################################################################################
  ###SUBSETTING COUNTIES
  
  shp_NA3 <- subset(shp, AFFGEOID %in% counties)
  
  #######################################################################################################
  ###WRITING SHAPEFILE
  
  cat("Writing shapefile for ",spName,"\n")
  
  setwd(output_dir) 
  
  writeOGR(obj=shp_NA3, dsn=spName2, layer="narea", driver="ESRI Shapefile") # this is in geographical projection
  
  #######################################################################################################
  ###WRITING PNG FILES
  
  
  cat("Writing png image for ",spName,"\n")
  
  png(filename=paste0(output_dir,"/",spName2,"_COUNTY.png"),
      width = 800, height = 800,unit="px")
  plot(shp,col=c("gray"))
  plot(shp_NA3,col="red",add=T)
  title(spName)
  dev.off()
  gc()
  
}else{
  cat("Ommiting ",spName,"\n")
    }


cat("#########","\n")
cat(" #DONE!# ","\n")
cat("#########","\n")
  


      }else{
 
  
      #######################################################################################################
      ###USING STATES
      #######################################################################################################
  
   
  cat("Using STATE APPROACH","\n")########
  
  shp<-STATE######## 
  
  if(nrow(list_SP)>0){
    
    #######################################################################################################
    ###SUBSETTING STATES
    
    cat("Subsetting  ",spName,"\n")
    
    shp_NA3 <- subset(shp, ADM1_CODE %in% counties)
    
    #######################################################################################################
    ###WRITING SHAPEFILE
    
    cat("Writing shapefile for ",spName,"\n") 
    
    setwd(output_dir) 
    
    writeOGR(obj=shp_NA3, dsn=spName2, layer="narea", driver="ESRI Shapefile") # this is in geographical projection
    
    
    
    #######################################################################################################
    ###WRITING PNG FILES
    
    cat("Writing png image for ",spName,"\n")
    
    png(filename=paste0(output_dir,"/",spName2,"_STATE.png"),
        width = 800, height = 800,unit="px")
    plot(shp,col=c("gray"))
    plot(shp_NA3,col="red",add=T)
    title(spName)
    dev.off()
    gc()
    
  }else{
    cat("Ommiting ",spName,"\n")
    } 
  }
gc()
}



#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################


shp_path<-"D:/BU/ADMIN"
shp_path2<-"D:/Dropbox/Dropbox/NPGS georeferencing project/COUNTIES/cb_2015_500K - Copy/SHP"
layer_name<-"GAUL_2014_NA2"
layer_name2<-"GAUL_US_CA_MEX_3"

narea_path<-"D:/BU/NAREA"
output_dir<-"D:/BU/NAREA/OUTCOME_2"
#spName<-"Actaea racemosa"

taxa<-c(
  # "Coreopsis auriculata",
  # "Coreopsis delphiniifolia",
  # "Coreopsis gladiata",
  # "Coreopsis lanceolata",
  # "Coreopsis leavenworthii",
  # "Coreopsis major",
  # "Coreopsis nuecensis",
  # "Coreopsis pubescens",
  # "Coreopsis pulchra",
  # "Coreopsis tinctoria",
  # "Coreopsis tripteris",
  # "Coreopsis verticillata",
  # "Coreopsis wrightii",
  # "Rudbeckia fulgida",
  # "Rudbeckia fulgida var. fulgida",
  # "Rudbeckia fulgida var. speciosa",
  # "Rudbeckia graminifolia",
  # "Rudbeckia grandiflora",
  # "Rudbeckia hirta",
  # "Rudbeckia laciniata",
  # "Rudbeckia maxima",
  # "Rudbeckia missouriensis",
  # "Rudbeckia mohrii",
  # "Rudbeckia nitida",
  # "Rudbeckia scabrifolia",
  # "Rudbeckia texana",
  # "Rudbeckia triloba",
  # "Phlox amplifolia",
  # "Phlox buckleyi",
  # "Phlox floridana",
  # "Phlox paniculata",
  # "Phlox_stolonifera"
"Amaranthus wrightii",
"Zizania aquatica",
"Zizania aquatica var. aquatica",
"Zizania aquatica var. brevis",
"Zizania palustris",
"Zizania palustris var. interior",
"Zizania palustris var. palustris",
"Zizania texana"
)




lapply(1:length(taxa),function(i){
spName<-taxa[[i]]
  x<-NAREA_FUNCTION(shp_path,layer_name,narea_path,spName)
})
#spName<-taxa[[1]]

