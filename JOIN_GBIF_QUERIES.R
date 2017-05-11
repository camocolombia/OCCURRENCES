

left = function (string,char){
  substr(string,1,char)
}

input_dir<-"D:/Dropbox/Dropbox/OUT_POINT_MAPS/GBIF"
files<-list.files(input_dir,pattern=".csv")
files<-sub("_2016-10-13.csv","",files)
files<-sub("CA","",files)
files<-sub("MX","",files)
files<-sub("US","",files)
files<-sub("__","",files)
files<-left(files,nchar(files)-1)
taxa<-unique(files)


#i=1


lapply(1:length(taxa),function(i){
  
  f<-list.files(input_dir,pattern=taxa[[i]],full.names = T)  
  f<-lapply(1:length(f),function(j){
    f2<-read.csv(f[[j]],sep="|") 
    return(f2)
  })
f<-do.call(rbind,f)

for(k in 1:nrow(f)){
  if(f$decimalLatitude[[k]]==f$decimalLongitude[[k]]){
    f$decimalLatitude[[k]]<-NA;f$decimalLongitude[[k]]<-NA
  }

}

colnames(f)<-c("taxon_final","key","final_lat","final_lon","issues")

write.table(f,paste0(input_dir,"/","final","/",as.character(taxa[[i]]),"_",Sys.Date(),".csv"),sep="|",row.names=F)
})
  
final<-list.files(paste0(input_dir,"/","final"),full.names = T)
final<-lapply(1:length(final),function(j){
  f2<-read.csv(final[[j]],sep="|") 
  return(f2)
})
final<-do.call(rbind,final)
write.table(final,paste0(input_dir,"/","final","/","Query_GBIF","_",Sys.Date(),".csv"),sep="|",row.names=F)


#g<-list.files(paste0(input_dir,"/","final"),pattern=".csv",full.names = T)

