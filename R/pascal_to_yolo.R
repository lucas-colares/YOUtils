# pascal_to_yolo
#' @details
#' EN: Convert annotations in .xml files from PASCAL VOC (xmin, xmax, ymin, ymax) to YOLO format (normalized x_center,y_center,bbox_width,bbox_height), resize images accordingly and saves the images, annotations a Label Map in .yaml format.
#' PT: Converte anotações em arquivos .xml do PASCAL VOC (xmin, xmax, ymin, ymax) para o formato YOLO (normalized x_center,y_center,bbox_width,bbox_height), redimensiona as imagens de acordo e salva as imagens, anotações em txt e um Label Map no formato .yaml.
#'
#' @param ann Um vetor com os caminhos dos arquivos de anotações em PASCAL VOC (.xml)
#' @param img Um vetor com o caminho para as imagens, na mesma ordem e tamanho do vetor de anotações
#' @param resize Booleano (TRUE ou FALSE) indicando se as imagens e anotações devem ser redimensionadas
#' @param size Um número (em pixels) indicando o tamanho para redimensionar as imagens
#' @param flat Booleano (TRUE ou FALSE) indicando se as imagens devem ser achatadas para caber em uma proporção 1:1 
#' @param save_ann Booleano (TRUE ou FALSE) indicando se as anotações devem ser salvas em formato .txt
#' @param save_img Booleano (TRUE ou FALSE) indicando se as imagens devem ser salvas em arquivo .jpg
#' @param savePath Um caracter indicando o caminho para salvar as imagens, anotações e label map
#' 
#' @return Um lista com 4 matrizes. Uma com as coordenadas originais das anotações em PASCAL VOC (xmin,xmax,ymin,ymax). Uma com as coordenadas após redimensionamento em PASCAL VOC (xmin,xmax,ymin,ymax). Uma terceira com as anotações em formato YOLO (normalized x_center,y_center,bbox_width,bbox_height). E uma quarta final com o label map.
#' 
#'
#' @examples
#' pascal_to_yolo(ann="C://Path//to//file.xml", img="C://Path//to//image.jpg", resize=TRUE, size=640, flat=TRUE,save_ann=TRUE,save_img=TRUE,savePath="C://Path//to//folder//to//save")

##### pascal_to_yolo ----
pascal_to_yolo<-function(ann, img, resize=TRUE,size=640,flat=TRUE,save_ann=FALSE,save_img=FALSE,savePath=getwd()){
  if(save_img|save_ann){
    print("Resizing annotations and saving images...")
  } else {
    print("Resizing annotations...")
  }
  
  final_anns<-pblapply(1:length(ann),function(x){
    XML::xmlTreeParse(ann[x])->xml
    XML::xmlRoot(xml)->xml
    grep("object",names.XMLNode(xml))->obj_pos
    if(!is.na(obj_pos[1])){
      coords<-{}
      orig_coords<-{}
      for(y in 1:length(obj_pos)){
        xml[[obj_pos[y]]]->obj_xml
        grep("name",names.XMLNode(obj_xml))->obj_name
        grep("bndbox",names.XMLNode(obj_xml))->obj_box
        
        obj_xml[[obj_box]]->obj_box_temp
        
        obj_xml[[obj_name]]->obj_name_temp
        xmlValue(obj_name_temp)->obj_name_temp
        
        data.frame(ID=obj_name_temp,
                   xmin=as.numeric(xmlValue(obj_box_temp[[grep("xmin",names.XMLNode(obj_box_temp))]])),
                   xmax=as.numeric(xmlValue(obj_box_temp[[grep("xmax",names.XMLNode(obj_box_temp))]])),
                   ymin=as.numeric(xmlValue(obj_box_temp[[grep("ymin",names.XMLNode(obj_box_temp))]])),
                   ymax=as.numeric(xmlValue(obj_box_temp[[grep("ymax",names.XMLNode(obj_box_temp))]])))->coords[[length(coords)+1]]  
      }
      do.call("rbind",coords)->coords
      
      
      image_read(img[x])->imgz
      image_info(imgz)->img_info
      
      orig_coords[[length(orig_coords)+1]]<-data.frame(coords,width=img_info$width,height=img_info$height,file=img[x])
      
      if(flat){
        size_x=size_y=size
      } else {
        
        if(img_info$width>img_info$height){
          size_x=size
          size_y=min(c(img_info$width,img_info$height))*size/max(c(img_info$width,img_info$height))
        } else {
          size_y=size
          size_x=min(c(img_info$width,img_info$height))*size/max(c(img_info$width,img_info$height))
        }
      }
      
      
      if(resize){
        img_re<-image_resize(imgz,paste0(size_x,"x",size_y,"!"))
      } else {
        img_re<-imgz
      }
      
      rm(imgz)
      
      if(save_img){
        file_name<-strsplit(img[x],"/")
        unlist(file_name)->file_name
        tail(file_name,1)->file_name
        image_write(img_re,path = file.path(savePath,file_name),format = "jpeg",quality = 100)
      }
      
      rm(img_re)
      gc()
      
      if(resize){
        x_scale=size_x/img_info$width
        y_scale=size_y/img_info$height
        coords$xmin=coords$xmin*x_scale
        coords$xmax=coords$xmax*x_scale
        coords$ymin=coords$ymin*y_scale
        coords$ymax=coords$ymax*y_scale
        coords$width=size_x
        coords$height=size_y
      }
      
      coords$file<-img[x]
      
      if(resize){
        return(list(Orig=orig_coords,Resized=coords))  
      } else {
        return(list(Orig=orig_coords))
      }
    }
  })
  if(resize){
    orig_anns<-lapply(final_anns,"[[",1)
    orig_anns2<-lapply(orig_anns,"[[",1)
    do.call("rbind",orig_anns2)->orig_anns
    
    final_anns<-lapply(final_anns,"[[",2)
    do.call("rbind",final_anns)->final_anns
  } else {
    final_anns<-lapply(final_anns,"[[",1)
    final_anns<-lapply(final_anns,"[[",1)
    do.call("rbind",final_anns)->final_anns
  }
  unique(final_anns$ID)[order(unique(final_anns$ID))]->labs
  data.frame(lab=labs,N=seq(0,(length(labs)-1)))->labs
  data.frame(ID=labs[match(final_anns$ID,labs$lab),]$N,x_yolo=NA,y_yolo=NA,width_yolo=NA,height_yolo=NA,file=final_anns$file)->yolo_anns
  
  yolo_anns$x_yolo<-colMeans(apply(final_anns[,c("xmin","xmax")],1,range))/final_anns$width
  yolo_anns$y_yolo<-colMeans(apply(final_anns[,c("ymin","ymax")],1,range))/final_anns$height
  
  yolo_anns$width_yolo<-as.numeric(diff(apply(final_anns[,c("xmin","xmax")],1,range))/final_anns$width)
  yolo_anns$height_yolo<-as.numeric(diff(apply(final_anns[,c("ymin","ymax")],1,range))/final_anns$height)
  
  if(save_ann){
    print("Saving annotations...")
    pblapply(unique(yolo_anns$file),function(o){
      yolo_anns[yolo_anns$file==o,]
      write.table(yolo_anns[yolo_anns$file==o,1:5],file = file.path(savePath,gsub(".jpg$|.jpeg$|.JPG$|.png$",".txt",tail(strsplit(o,"/")[[1]],1))),col.names = FALSE,row.names = FALSE)
    })
  }
  
  rbind("train: train/images", "val: val/images", "test: test/images","",paste0("nc: ",nrow(labs)),paste0("names: ['",paste0(labs$lab,collapse = "', '"),"']"))->labmap
  write.table(labmap,file = file.path(savePath,"data.yaml"),row.names = F,col.names = F,quote = FALSE)
  
  if(resize){
    return(list(Orig=orig_anns,Resized=final_anns,YOLO=yolo_anns,LabelMap=labs))
  }
  gc()
  return(list(Orig=final_anns,YOLO=yolo_anns,LabelMap=labs))
}
