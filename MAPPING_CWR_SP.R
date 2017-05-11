
#taxon<-"" #taxon to do the query 
# open_dir<-""#folder csv file
# filename=""# csv filename
#out_dir="" #output folder


require(ffbase)
MAPPING_CWR_FUNCTION<-function(taxon,open_dir,filename,out_dir){
  
  
  
  #require(Rcpp);
  require(rgeos);
  require(rgdal)#;
  #require(maptools); 
  require(ffbase)#;
  require(sp);
  require(ggplot2)#;
  require(ggmap);
  #require(maps)
  #require(plyr)
  #require(mapdata)
  #require(mapproj)
  #require(raster)
  #library(data.table)
  library(RgoogleMaps)
  #library(RColorBrewer)
  #library(loa)
  #library(MASS)
  #require(stringi)
  
  
  ###############################
  #                             #
  #     LOADING POINTS DATA     #             
  #                             #
  ###############################
  

  
  ###############################
  #                             #
  #     QUERY   AND FILTER      #             
  #                             #
  ###############################
  
  taxon<-taxon
  taxon2<-taxon
  taxon3<-gsub("_", " ", taxon)
  
  ####################
  cat("filtering: ", as.character(taxon),"\n")
  
  
  
  #genus_filt<-subset.ffdf(genus_gbif,taxon_final==taxon)
  expression_tax<-ffwhich(genus_gbif,taxon_final==taxon)
  
  if(is.null(expression_tax)){
    cat("ommiting ",taxon,"\n")
  }else{
    
  #  is.na(any(genus_filt$final_cult_stat[]=="wild")) 
  genus_filt<-genus_gbif[expression_tax,]
  #genus_filt<-as.ffdf(genus_filt);gc()
  #write.table.ffdf(genus_filt,paste0("D:/",taxon,".csv"),sep="|")
    cat("filtering wild and NA records for final_cult_stat ",taxon,"\n")  
    
  #   expression_cult<-ffwhich(genus_filt,is.na(final_cult_stat))
  # deals0 <-   genus_filt[expression_cult,]
    idx <- is.na(genus_filt$final_cult_stat) ## This uses is.na.ff_vector from ffbase
    idx <- ffwhich(idx, idx == TRUE) ## Is part of ffbase

    if(is.null(idx)){
      genus_filt<-genus_filt
    }  else{
      
      deals0<-genus_filt[idx,] #NAs
    #if(is.na(any(genus_filt$final_cult_stat[]=="wild"))){
      
      #genus_filt<-genus_filt
   if(!is.na(any(genus_filt$final_cult_stat[]=="wild")) & !is.na(any(unique(genus_filt$final_cult_stat[])=="wild"))){
        
        deals2<-subset.ffdf(genus_filt,final_cult_stat=="wild") 
        
        genus_filt<-ffdfappend(deals0, deals2, adjustvmode=F)    
        
    #   }else if(!is.na(any(genus_filt$final_cult_stat[]=="cultivated"))){
    #     deals2<-subset.ffdf(genus_filt,final_cult_stat=="wild")
    #     
    #     if(exists("deals2")){
    #       genus_filt<-ffdfappend(deals0, deals2, adjustvmode=F)    
    #       
    #     }else{
    #       
    #       genus_filt<-deals0
    #     }
    #     
    # }else if(!is.na(any(genus_filt$final_cult_stat[]=="weedy"))){
    #   deals2<-subset.ffdf(genus_filt,final_cult_stat=="wild")
    #   
    #   if(exists("deals2")){
    #   genus_filt<-ffdfappend(deals0, deals2, adjustvmode=F)    
    #   
    #   }else{
    #     
    #     genus_filt<-deals0
    #   }
    #   
    # }else if(is.na(any(genus_filt$final_cult_stat[]=="weedy"))){
    #   
    #   
    } else if(nrow(deals0)==nrow(genus_filt)){
        
        cat("No wild status in final_cult_stat...using NAs","\n")  
      genus_filt<-deals0
    }else if(length(length(unique(genus_filt$final_cult_stat[]=="wild"))==1)){
      
      genus_filt<-genus_filt
      cat("only wild status in final_cult_stat...using it","\n")  
      
      }
    }    
  #     
  #     
  #   
  #   if(nrow(deals0)==nrow(genus_filt)){
  #    # genus_filt<-as.ffdf(genus_filt);gc()        
  # 
  #   }else{
  # 
  #     if(any(unique(genus_filt$final_cult_stat)=="cultivated")){
  #       
  #       cat("searching final_cult_stat=cultivated","\n")
  #       
  #      expression_cult<-ffwhich(genus_filt,final_cult_stat=="cultivated")
  #      deals1 <- genus_filt[expression_cult,]
  #      
  #           if(nrow(deals1)+nrow(deals0)!=nrow(genus_filt)){
  #        cat("final_cult_stat=wild was founded using wild and NA's values","\n")
  #        
  #        deals2<-subset.ffdf(genus_filt,final_cult_stat=="wild") 
  #        genus_filt<-ffdfappend(deals0, deals2, adjustvmode=F)
  #           }else{
  #             
  #             genus_filt<-ffdfappend(deals0, deals2, adjustvmode=F)
  #             
  #           }
  #         }else if(any(unique(genus_filt$final_cult_stat)=="weedy")){
  #        cat("only final_cult_stat=cultivated was founded using NAs","\n")
  #        genus_filt<-deals0
  #       }
  #    
  #    }
  # #    genus_filt<-subset.ffdf(genus_filt,final_cult_stat=="wild")
  # 
  # }    
  # 
    genus_filt<-as.ffdf(genus_filt);gc()  
    
   
#  unique((deals1$final_cult_stat))
  
 

  
  ###############################
  #                             #
  #  LOADING LON LAT IN A FF    #             
  #                             #
  ###############################
  temp_dt <- ff(dim=c(nrow(genus_filt),2),vmode="double")
  
  for( i in 1:nrow(genus_filt)){
    temp_dt[i,1] <- as.numeric(as.character(genus_filt$final_lon[i]))
    temp_dt[i,2]   <- as.numeric(as.character(genus_filt$final_lat[i]))
  };rm(i)
  colnames(temp_dt)<-c("long", "lat")
  
  ######################################
  #                                    #
  # DELETING DUPLICATES AND PROJECTING #             
  #                                    #
  ######################################
  
  cat("deleting duplicates from ", as.character(taxon3),"\n")
  
  temp_dt4<-temp_dt[!is.na(temp_dt[,1] ),]
  if(!is.null(nrow(temp_dt4))){
    temp_dt4<-temp_dt4[!duplicated(temp_dt4[,1]), ]   
  }else{
    temp_dt4<-temp_dt4
    }

  if(is.null(nrow(temp_dt4))){
    cat("ommiting ",as.character(taxon),"\n")
  }else{
  temp_dt<-as.ff(temp_dt)
  temp_dt<-as.ffdf(temp_dt)
  temp_dt<-as.data.frame.ffdf(temp_dt)
  #temp_dt$taxon<-taxon
  # WGS_S<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  # 
  # 
  # coordinates(temp_dt)<-c("long", "lat")
  # proj4string(temp_dt)<-WGS_S
  
  cat("transforming coordinates in a dataframe for  ", as.character(taxon3),"\n")
  
  temp_dt2<-as.data.frame(matrix(nrow=nrow(temp_dt),ncol=2))
  colnames(temp_dt2)<-c("long", "lat")
  
  temp_dt2[,1]<-temp_dt[,1]
  
  temp_dt2[,2]<-temp_dt[,2]
  
  gc()
  
  write.table(genus_filt,paste0(out_dir,"/","CSV","/",taxon,"_",Sys.Date(),".csv"),sep="|",row.names = F)
  ######################################
  #                                    #
  # RANGING COORDINATES TO GET MAP     #             
  #                                    #
  ######################################
  
  
  if(nrow(temp_dt2)>1 & !is.null(nrow(temp_dt4))){
  
  
  xmin<-min(temp_dt2[,1],na.rm=T)
  xmax<-max(temp_dt2[,1],na.rm=T)
  
  ymin<-min(temp_dt2[,2],na.rm=T)
  ymax<-max(temp_dt2[,2],na.rm=T)
  
  }else if(is.null(nrow(temp_dt4))){
    
    xmin<-min(temp_dt4[1],na.rm=T)-3
    xmax<-max(temp_dt4[1],na.rm=T)+3
    
    ymin<-min(temp_dt4[2],na.rm=T)-3
    ymax<-max(temp_dt4[2],na.rm=T)+3 
  }else if(nrow(temp_dt4)==1){
    
    xmin<-min(temp_dt4[1],na.rm=T)-3
    xmax<-max(temp_dt4[1],na.rm=T)+3
    
    ymin<-min(temp_dt4[2],na.rm=T)-3
    ymax<-max(temp_dt4[2],na.rm=T)+3 
  }  else if(nrow(temp_dt2)==1){
    min<-min(temp_dt2[,1],na.rm=T)-3
    xmax<-max(temp_dt2[,1],na.rm=T)+3
    
    ymin<-min(temp_dt2[,2],na.rm=T)-3
    ymax<-max(temp_dt2[,2],na.rm=T)+3 
    
  }
  
  rm(temp_dt);gc()
  
  
  #temp_dt2<-as.data.frame(temp_dt);gc()
  
  ###############################
  #                             #
  #     GETTING  TAXON MAP      #             
  #                             #
  ###############################
  
  cat("Obtaining Google map for  ", as.character(taxon3),"\n")
  
  MaxZoom2<-MaxZoom(c(ymin,ymax),c(xmin,xmax))
  
  if(MaxZoom2<3){
    MaxZoom2=3
  }
  
  center <- rev(sapply(as.data.frame(temp_dt2[,1:2]), mean,na.rm=T))
  center<-rev(center)
  
  ss<-get_map(location=center, zoom = MaxZoom2, maptype='road', source='google',crop=TRUE,color='bw')
  
  
  
  
  ###############################
  #                             #
  #     TESTING RANKING         #             
  #                             #
  ###############################
  
  cat("Testing ranking such as:  subsp. ,var., f. for ", as.character(taxon3),"\n")
  
  
  split_text <- unlist(strsplit(taxon3, split=' '))
  cond <- c('subsp.', 'var.', 'f.')
  
  selected_text <- split_text[split_text %in% cond]
  
  
  cat("Savig file for ", as.character(taxon3),"\n")
  
  
  if(length(selected_text)==0){
    
    NAME<-substitute(expr=italic(taxon3),env=list(taxon3=taxon3))
  }else{
    a<-split_text[[1]]
    b<-split_text[[2]]
    c<-split_text[[4]]
    
    NAME<-substitute(expr=paste(italic(a)," ",italic(b)," ",selected_text," ",italic(c)),
                     env=list(a=a,b=b,selected_text=selected_text,c=c))
  }
  #setwd(out_dir)
  
  #  tiff(paste0(out_dir,"/",taxon2,"_",Sys.Date(),".tif"), res=600, compression = "lzw", height=4.8, width=4, units="in")
  
  map1 <- ggmap(ss, extent='panel')#, base_layer=ggplot(data=temp_dt2,aes(x=long, y=lat)))
  # map1<-map1+geom_point(color="black",size=0.5)
  map1<-map1+geom_point(aes(x=long, y=lat,col="Taxon"),size=2,stroke = 1,data=temp_dt2, show.legend = TRUE,shape=17)#Alt + 9733	
  map1<-map1+labs(x ="Longitude", y="Latitude")
  map1<-map1+ggtitle(NAME)
  map1<-map1+ theme(axis.text=element_text(size=10),axis.title=element_text(size=10),legend.position="bottom",legend.title=element_blank())+
    #scale_fill_manual(name="Taxon occurrences",values="black") # no title) 
    scale_colour_manual(name="Taxon occurrences",values="black")
  
  #map1<-map1+scale_color_manual(values = c("Occurrences" = 'black')) + 
  #  scale_shape_manual(values = c(taxon3 = 17))
  
  
  
  # ggsave(map1,)
  ggsave(paste0(out_dir,"/","PDF","/",taxon2,"_",Sys.Date(),".pdf"),units="in",width=4,height=4.8,scale=2,dpi=600)
  
  #  print(map1)
  #dev.off()
    }
  }
  gc()
}

##########################################################################333
##########################################################################333
##########################################################################333

open_dir<-"D:/CWR_OCC_VALIDATION/NORTH_AMERICA_2016_07_19"#folder csv file

#filename<-"United_States.csv"# csv filename
#filename<-"Query_2016-08-23.csv"# csv filename ##whole dataset
#filename<-"raw_CIAT_Phaseolusdb_2012_ff_2016_8_24.csv"# csv filename
#filename<-"sunflower_all_2016_08_24.csv"
#filename<-"ipomoea_all_2016_08_25.csv"
#filename<-"Lactuca-2016_08_25.csv"
#filename<-"Query_GBIF_2016-08-25.csv"
#filename<-"Chenopodium_berlandieri_JFM_2016-08-24.csv"
#filename<-"Chenopodium_berlandieri_AMJ_2016-08-24.csv"
#filename<-"Chenopodium_berlandieri_JAS_2016-08-24.csv"
#filename<-"Chenopodium_berlandieri_OND_2016-08-24.csv"
#filename<-"Actaea_racemosa_2016-08-25 - Copy.csv"
#filename<-"Query_2016-08-26_2.csv"
#filename<-"Query_2016-08-27.csv"
#filename<-"Query_GBIF_2016-08-29.csv"
#filename<-"Nicotiana_Query_JMNedits_2016_08_29.csv"
#filename<-"Thurberi_2016_09_02.csv"
#filename<-"Query_2016-09-09.csv"
#filename<-"Query_Cberlandieri_2016-09-21_MAMJ.csv"
#filename<-"Amaranthus_2016_09_21.csv"
#filename<-"Query_2016-10-03_CUCURBITA.csv"
#filename<-"Query_2016-10-13_LOA.csv"
#filename<-"Query_GBIF_2016-10-13.csv"
#filename<-"A_TUCSONENSIS_2016_10_17.csv"
#filename<-"Query_2016-11-03.csv"
#filename<-"Query_2016-11-09.csv"
filename<-"Query_2016-11-14.csv"
##########################################################################333

out_dir<-"D:/CWR_OCC_VALIDATION/NORTH_AMERICA_2016_07_19/OUT"

##########################################################################333
cat("loading filename: ", as.character(filename),"\n")
#setwd(open_dir)
genus_gbif <- read.csv2.ffdf(file=paste0(open_dir,"/",filename),encoding="UTF-8",sep = "|",
                             VERBOSE = TRUE,na.strings="",first.rows = 50000,
                             next.rows = 50000,quote="",header=T,colClasses="factor");gc()

#write.table(unique(genus_gbif$taxon_final[]),"D:/CWR_OCC_VALIDATION/query_sp_2016_08_26.CSV",sep="|",row.names=F)

 taxa<-c(
   # "Phaseolus_maculatus",
   # "Phaseolus_maculatus_subsp._maculatus",
   # "Phaseolus_maculatus_subsp._ritensis",
   # "Phaseolus_polystachios_subsp._polystachios",
   # "Phaseolus_polystachios_subsp._sinuatus",
   # "Phaseolus_polystachios_subsp._smilacifolius",
   # "Phaseolus_polystachios_var._polystachios"
   "Phaseolus_polystachios"
   
   )

lapply(1:length(taxa),function(i){
  cat(i,"\n")
  x<-MAPPING_CWR_FUNCTION(taxon=taxa[[i]],open_dir=open_dir,filename=filename,out_dir=out_dir)

})
 # taxon<-"Amaranthus_tucsoniensis"
 # x<-MAPPING_CWR_FUNCTION(taxon=taxon,open_dir=open_dir,filename=filename,out_dir=out_dir)
