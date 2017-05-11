#devtools::install_github("ropensci/rgbif")

#https://cran.r-project.org/web/packages/rgbif/vignettes/rgbif_vignette.html

rGBIF_DOWNLOAD<-function(taxon,outdir,fuzzy,countries,coordinates){
  
  require(rgbif);require(plyr);require(ff);require(stringr)
  
  ###COUNT WORDS FUNCTION###  
  nwords <- function(string, pseudo=F){
    ifelse( pseudo, 
            pattern <- "\\S+", 
            pattern <- "[[:alpha:]]+" 
    )
    str_count(string, pattern)
  }
  
  if(fuzzy==TRUE){
    
    ###FUZZY OPTIONS ACCORDLY nwords FUNCTION RESULTS###  
    if(nwords(taxon)==1){
      key <- name_suggest(q=taxon, rank="genus")$key[1]
      cat("Finding key number for ", as.character(taxon),"...genus","\n") 
    }else if(nwords(taxon)==2){
      key <- name_suggest(q=taxon, rank="species")$key[1]
      cat("Finding key number for ", as.character(taxon),"...species","\n") 
    }else if(nwords(taxon)==3){
      key <- name_suggest(q=taxon, rank="species")$key[1]
      cat("Finding key number for ", as.character(taxon),"...species","\n") 
    }else if(nwords(taxon)==4){
      if(grep("var.",taxon, value=F)==1){
        key <- name_suggest(q=taxon, rank="variety")$key[1]
        cat("Finding key number for ", as.character(taxon),"...variety","\n") 
      }else if(grep("subsp.",taxon, value=F)==1){
        key <- name_suggest(q=taxon, rank="subspecies")$key[1]
        cat("Finding key number for ", as.character(taxon),"...subspecies","\n") 
      }  
    }
    
    ###################
    
    cat("Doing a query for ", as.character(taxon),"\n") 
    ###################
    s<-occ_search(taxonKey=key,country=countries,hasCoordinate=coordinates,limit=200000,fields= 'minimal')
    gc()
  }else{
    ###SCIENTIFIC NAMEOPTION###  
    
    s<-occ_search(scientificName =taxon,country=countries,hasCoordinate=coordinates,limit=200000,fields= 'minimal')
    gc()
  }
  ###################
  
  ###TESTING
  
  cat("testing null rows for ", as.character(taxon),"\n")
  
  S_COUNTRIES<-list()
  
  for(i in 1:length(s)){
    
    if(is.null(nrow(s[[i]]$data))){S_COUNTRIES[[i]]<--100}else{S_COUNTRIES[[i]]<-100000}
    
    
  };rm(i)
  
  ##if SUM <0 there are not records for any country
  
  if(sum(unlist(S_COUNTRIES),na.rm=T)>0){
    
    # a<-if(is.null(nrow(s$US$data))){a<-10 }else{a<-100}
    # b<-if(is.null(nrow(s$MX$data))){b<-10 }else{b<-100}
    # c<-if(is.null(nrow(s$CA$data))){c<-10 }else{c<-100}
    #((a+b+c)!=30){
    
    ##### 
    
    cat("saving files for ", as.character(taxon),"\n")
    
    ##### 
    
    ###SAVING csv FILES USING A COUNTRIES LIST###
    
    lapply(1:length(countries),function(j){
      cat("Writing csv files for ", as.character(taxon),"\n")  
      
      if(!is.null(nrow(s[[j]]$data))){
        write.table(s[[j]]$data,paste0(outdir,"/",as.character(taxon),"_",names(s[j]),"_",Sys.Date(),".csv"),sep="|",row.names = F)
        gc()
      } else{
        cat("ommiting csv files for ", as.character(taxon),"...",names(s[j]),"\n")  
        
      }
    })
    
  }else{
    
    cat("Skipping ", as.character(taxon),"\n")
    
  }
}


outdir<-"D:/Dropbox/Dropbox/OUT_POINT_MAPS/GBIF"


countries<-c("US","MX","CA")

 taxa<-c(
   "Phlox_amplifolia"
   # "Phlox_floridana",
   # "Phlox_paniculata",
   # "Phlox_stolonifera"
   )

lapply(1:length(taxa),function(i){
  cat(as.character(i),"\n")
  x<-rGBIF_DOWNLOAD(taxon=taxa[[i]],outdir=outdir,fuzzy=FALSE,countries=countries,coordinates=TRUE) 
})
# i=1
# taxon<-taxa[[i]]
