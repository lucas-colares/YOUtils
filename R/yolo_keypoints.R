# yolo_keypoints
#' @details
#' Une anotações de caixas em PASCAL VOC e landmarks (em array de 3 dimensões com coordenadas x e y de cada landmark) e converte eles em formato YOLo, salvando as imagens, anotações e label map.
#'
#' @param xmls Um vetor com os caminhos dos arquivos de anotações das caixas em PASCAL VOC (.xml)
#' @param imgs Um vetor com o caminho para as imagens, na mesma ordem e tamanho do vetor de anotações
#' @param tps Um array de 3 dimensões, onde linhas são os landmarks, colunas são as coordenadas x e y, e a terceira dimensão é o individuo/imagem medido. A terceira dimensão precisa estar na mesma ordem que os arquivos de imagens e anotações.
#' @param resize Booleano (TRUE ou FALSE) indicando se as imagens e anotações devem ser redimensionadas
#' @param size Um número (em pixels) indicando o tamanho para redimensionar as imagens
#' @param flat Booleano (TRUE ou FALSE) indicando se as imagens devem ser achatadas para caber em uma proporção 1:1 
#' @param save_ann Booleano (TRUE ou FALSE) indicando se as anotações devem ser salvas em formato .txt
#' @param save_img Booleano (TRUE ou FALSE) indicando se as imagens devem ser salvas em arquivo .jpg
#' @param savePath Um caracter indicando o caminho para salvar as imagens, anotações e label map
#' 
#' @return Uma matriz com as coordenadas dos landmarks de cada imagem em formato YOLO, além de salvar um label map em formato .yaml
#' 
#'
#' @examples
#' yolo_keypoints(xmls="C://Path//to//file.xml", imgs="C://Path//to//image.jpg", tps=array_object,resize=TRUE, size=640, flat=TRUE,save_ann=TRUE,save_img=TRUE,savePath="C://Path//to//folder//to//save")

##### yolo_keypoints ----
yolo_keypoints<-function(xmls,imgs,tps,savePath,resize=FALSE,size=640,flat=FALSE,save_img=FALSE,save_ann=FALSE){
  pascal_to_yolo(ann = xmls,img = imgs,resize = resize,size = size,savePath = savePath,flat = flat,save_ann = save_ann,save_img = save_img)->yolo_ann_data
  
  unique(yolo_ann_data$YOLO$file)->filez
  rbind("train: train/images", "val: val/images", "test: test/images","",paste0("kpt_shape: [",dim(tps)[1],", 2]"),"","names: ",paste0("  ",yolo_ann_data$LabelMap$N,": ",yolo_ann_data$LabelMap$lab))->labs
  write.table(labs,file = file.path(savePath,"data.yaml"),row.names = F,col.names = F,quote = FALSE)
  
  for(x in 1:dim(tps)[3]){
    
    if(resize){
      
      if(yolo_ann_data$Resized$width[x]>yolo_ann_data$Resized$height[x]){
        size_x=size
        size_y=min(c(yolo_ann_data$Resized$width[x],yolo_ann_data$Resized$height[x]))*size/max(c(yolo_ann_data$Resized$width[x],yolo_ann_data$Resized$height[x]))
      } else {
        size_y=size
        size_x=min(c(yolo_ann_data$Resized$width[x],yolo_ann_data$Resized$height[x]))*size/max(c(yolo_ann_data$Resized$width[x],yolo_ann_data$Resized$height[x]))
      }
      
      x_scale=size_x/yolo_ann_data$Orig$width[x]
      y_scale=size_y/yolo_ann_data$Orig$height[x]
      tps[,1,x]=tps[,1,x]*x_scale
      tps[,2,x]=tps[,2,x]*y_scale
      
      tps[,1,x]<-tps[,1,x]/size_x
      #tps[,2,x]<-abs(tps[,2,x]-yolo_ann_data$Orig$height)
      tps[,2,x]<-tps[,2,x]/size_y 
      
    } else {
      tps[,1,x]<-tps[,1,x]/yolo_ann_data$Orig$width[x]
      #tps[,2,x]<-abs(tps[,2,x]-yolo_ann_data$Orig$height)
      tps[,2,x]<-tps[,2,x]/yolo_ann_data$Orig$height[x] 
    }
  }
  
  ann_keys<-lapply(1:dim(tps)[3],function(x){
    yolo_ann_data$YOLO[yolo_ann_data$YOLO$file==filez[x],]->sel_img
    ord_keys<-lapply(1:nrow(tps[,,x]),function(u){
      return(c(tps[u,,x][1],tps[u,,x][2]))
    })
    unlist(ord_keys)->ord_keys
    ann_keys<-paste(sel_img$ID,sel_img$x_yolo,sel_img$y_yolo,sel_img$width_yolo,sel_img$height_yolo,paste0(ord_keys,collapse = " "),sep = " ")  
    
    return(data.frame(ann_keys,file=filez[x]))
  })
  do.call("rbind",ann_keys)->ann_keys
  gc()
  return(ann_keys)
}
