
###Figure 1 -----

library(sp)
library(rgdal)
library(tmap)

nfhs4_combined <- readRDS("data/nfhs4/nfhs4_combined_t_o.RDS")
pregnant_anaemia <- nfhs4_combined[nfhs4_combined$District.ITEMID=="V78",c("censuscode","Total")]
pregnant_ifa <- nfhs4_combined[nfhs4_combined$District.ITEMID=="V32",c("censuscode","Total")]

all_women_anaemia <- nfhs4_combined[nfhs4_combined$District.ITEMID=="V79",c("censuscode","Total")]
all_women_highbs <- nfhs4_combined[nfhs4_combined$District.ITEMID=="V81",c("censuscode","Total")]

child_immunized <- nfhs4_combined[nfhs4_combined$District.ITEMID=="V47",c("censuscode","Total")]
child_stunted <- nfhs4_combined[nfhs4_combined$District.ITEMID=="V71",c("censuscode","Total")]

df=child_stunted
value_var = "Total"
id_var="censuscode"
district=TRUE
title="Title"
state="India"
legend="Percentage (%)"
method="tmap"
palette="Oranges"
direction=1

palette <- ifelse(direction==-1,paste0("-",palette),palette)

#Enter path to shape files
# https://github.com/datameet/maps


path_shape_files <- "~/shape_files"
df[,"id2"]     <- df[,id_var]
df[,"value"]  <- df[,value_var]

if(is.null(district)|district==FALSE){
  
  shape_df <-  readOGR(paste0(path_shape_files,"/IND_adm1"),"IND_adm1")
  if(!("India" %in% state)){
    shape_df <- shape_df[shape_df$NAME_1 %in% state,]
    bound_df <- bound_df[bound_df$NAME_1 %in% state,]
  }
  
  shape_df2 <- sp::merge(shape_df,df[,c("id2","value")],by.y="id2",by.x="ID_1",all.x=TRUE)
  bound_df <-  readOGR(paste0(path_shape_files,"/Census_2011"),"2011_Dist")
  
  tm_shape(bound_df) + tm_borders(col="black") + 
    tm_shape(shape_df2) + tm_borders() + tm_fill(col="value",style="fixed",breaks=c(0,1,2,3),
                                                 palette=paste0("-",palette)) + 
    tm_text(text="NAME_1",size=0.3,col="darkgrey",remove.overlap = FALSE)
  
}

if(district==TRUE){
  shape_df <-  readOGR(paste0(path_shape_files,"/Census_2011"),"2011_Dist")
  bound_df <- readOGR(paste0(path_shape_files,"/IND_adm1"),"IND_adm1")
  
  if(!("India" %in% state)){
    shape_df <- shape_df[shape_df$ST_NM %in% state,]
    bound_df <- bound_df[bound_df$NAME_1 %in% state,]
  }
  shape_df2 <- sp::merge(shape_df,df,by.x="censuscode",by.y="id2",all.x=TRUE)
  a <- tm_shape(shape_df2) + 
    tm_borders() + tm_fill(title= "Percentage (%)",
                           col="value",palette=paste0(palette),
                           breaks=c(0,15,30,45,60,100)) + 
    tm_shape(bound_df) + tm_borders(col="black") + 
    tm_text(text="NAME_1",col="black",size=0.5,remove.overlap = TRUE)+
    tm_legend(legend.position = c("right","top"),
              legend.outside=FALSE,
              legend.just=c("left","top"))+ 
    tm_xlab("Longitude") +
    tm_ylab("Latitude")
  
  save_tmap(a,'test.png',height=2300/300)
  
}




###Figure 3 ----
path_shape_files <- "C:/Analysis/data/india/shape_files"
path_rapid_ifa <- "C:/Analysis/writing/rapid_ifa"


india_adm3 <- readOGR(paste0(path_shape_files,"/IND_Adm"),"IND_adm3")
sirohi <- india_adm3[india_adm3$NAME_2=="Sirohi",]

p <- fortify(sirohi)

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
circle1 <- circleFun(c(72.65,24.5),0.1,npoints = 1000)
circle2 <- circleFun(c(72.6,24.9),0.1,npoints = 1000)


tiff(paste0(path_rapid_ifa,"/Figure3_3_adm3.tiff"), units="px", width=2800, height=2000, res=300)
p1 <- ggplot() + theme_bw() + 
  geom_polygon(data=p,aes(x=long,y=lat,group=group),col="grey",fill="khaki2") + 
  geom_point(data= map_ifa,aes(x=lon,y=lat,shape=type,size=population,colour=ifa,group=id),lwd=3,stroke=2) 

p1 <- p1 + geom_line(data= map_ifa,aes(x=lon,y=lat,size=population,group=id,lty="dotted"),size=1) 
p1 <- p1 + geom_path(data= circle1,aes(x,y,lty="continuous"),size=1) 
p1 <- p1 + geom_path(data= circle2,aes(x,y,lty="continuous"),size=1)
p1 <- p1 + geom_text(aes(x=72.6,y=24.55,label="1"),size=12,col="darkorange")
p1 <- p1 + geom_text(aes(x=72.55,y=24.95,label="2"),size=12,col="darkorange")
p1 <- p1 + xlab("Longitude") + ylab("Latitude")
p1 <- p1 + scale_shape_manual(name="Facilities",
                              labels=c("PHC or CHC","Sub Centre","ASHA or AWW"),
                              values=c(0,1,2))
p1 <- p1 +  scale_size(range(50,400),name="Population")
p1 <- p1 + scale_colour_manual(name="IFA Status",
                               values=c("red","darkgreen"),labels=c("Absent","Present"))
p1 <- p1 + scale_linetype_manual(name="Line Type",
                                 values=c("dotted"=3,"continuous"=1),
                                 labels=c("Case","Administrative Link"))
print(p1)
dev.off()

map_ifa$population <- NULL
write.csv(map_ifa,paste0(path_rapid_ifa,"/Figure3_map_public.csv"))
