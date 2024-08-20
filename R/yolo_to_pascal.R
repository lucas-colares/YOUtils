# yolo_to_pascal
#' @details
#' Converte anotações do formato de YOLO (.txt; x_center, ycenter, width_bbox, height_bbox) para PASCAL VOC (.xml; xmin, xmax, ymin, ymax).
#' 
#' @param ann Um vetor com os caminhos dos arquivos de anotações em formato YOLO (.txt)
#' @param img Um vetor com o caminho para as imagens, na mesma ordem e tamanho do vetor de anotações
#' @param labelmap Uma matriz de labelmap como retornada pela função "pascal_to_yolo", com duas colunas: (1) "lab", com o nome da categoria correspondente em texto; e (2) "N", com informação das categorias em números de 0 à +Inf. 
#' @param save Booleano (TRUE ou FALSE) indicando se as imagens e anotações devem ser redimensionadas
#' @param corr_overlap Booleano (TRUE ou FALSE) indicando se as identificações que se sobrepõem devem ser eliminadas, mantendo-se somente a predição com maior precisão entre as que se sobrepõem
#' @param IOU Um número entre 0 e 1 que indica o limite mínimo para se considerar uma sobreposição entre duas identificações
#' @param savePath Um caracter indicando o caminho para salvar as imagens, anotações e label map
#' 
#' @return Uma matriz com as anotações em formato PASCAL VOC (em .xml; xmin, xmax, ymin, ymax). Também salva as anotações e imagens, caso "save=TRUE".
#'
#' @examples
#' yolo_to_pascal(ann="C://Path//to//file.txt", img="C://Path//to//image.jpg",labelmap=data_frame, save=TRUE, corr_overlap=TRUE, IOU=0.2,savePath="C://Path//to//folder//to//save")

##### yolo_to_pascal ----
yolo_to_pascal<-function(ann,img,labelmap,save=TRUE, savePath,corr_overlap=FALSE,IOU=0.2){
  new_anns<-pbapply::pblapply(1:length(ann),function(y){
    read.csv(ann[y],header = F,sep = " ")->annLoad
    colnames(annLoad)=c("ID",'x_yolo', 'y_yolo', 'width_yolo','height_yolo','conf')
    img_file<-img[grepl(paste0(gsub(".txt$","",gsub("\\)","\\\\)",gsub("\\(","\\\\(",ann[y]))),"\\.\\D+"),img)]
    IMG<-image_read(img_file)
    annLoad$ID<-labelmap[match(annLoad$ID,labelmap[,1]),][,2]
    annLoad$width_yolo<-annLoad$width_yolo*image_info(IMG)$width
    annLoad$height_yolo<-annLoad$height_yolo*image_info(IMG)$height
    coords<-data.frame(matrix(NA,nrow = nrow(annLoad),ncol = 4))
    for(t in 1:nrow(annLoad)){
      x_min <- (annLoad$x_yolo[t]*image_info(IMG)$width)-(annLoad$width_yolo[t]/2)
      x_max <- (annLoad$x_yolo[t]*image_info(IMG)$width)+(annLoad$width_yolo[t]/2)
      y_min <- (annLoad$y_yolo[t]*image_info(IMG)$height)-(annLoad$height_yolo[t]/2)
      y_max <- (annLoad$y_yolo[t]*image_info(IMG)$height)+(annLoad$height_yolo[t]/2)
      colnames(coords)=c("xmin","ymin","xmax","ymax")
      coords$xmin[t]<-x_min
      coords$xmax[t]<-x_max
      coords$ymin[t]<-y_min
      coords$ymax[t]<-y_max
    }
    
    if(corr_overlap){
      {non_over_bbox<-{}
      dupl_yolo=data.frame(ID=annLoad$ID,coords,conf=annLoad$conf)
      if(nrow(dupl_yolo)>1){
        data.frame(matrix(NA,nrow = nrow(dupl_yolo),ncol = nrow(dupl_yolo)))->dist.mat
        rownames(dist.mat)=rownames(dupl_yolo)
        colnames(dist.mat)=rownames(dupl_yolo)
        for(r in 1:nrow(dist.mat)){
          dupl_yolo[rownames(dupl_yolo)==rownames(dist.mat)[r],]->sel.r
          for(c in 1:ncol(dist.mat)){
            dupl_yolo[rownames(dupl_yolo)==rownames(dist.mat)[c],]->sel.c
            x_dist<-min(sel.r$xmax,sel.c$xmax) - max(sel.c$xmin,sel.r$xmin)
            y_dist<-min(sel.r$ymax,sel.c$ymax) - max(sel.c$ymin,sel.r$ymin)
            area_over=x_dist*y_dist
            dist.mat[r,c]<-area_over/(((sel.r$xmax-sel.r$xmin)*(sel.r$ymax-sel.r$ymin))+((sel.c$xmax-sel.c$xmin)*(sel.c$ymax-sel.c$ymin))-area_over)
          }
        }
        pair_comp<-data.frame(expand.grid(rownames(dist.mat),colnames(dist.mat)),value=suppressMessages(try(reshape2::melt(dist.mat),silent = T)[,2]))
        pair_comp[!pair_comp$Var1==pair_comp$Var2,]->pair_comp
        
        non_over_temp<-{}
        for(u in 1:nrow(dupl_yolo)){
          sel_fis<-pair_comp[pair_comp$Var1==rownames(dupl_yolo[u,]),]
          sel_fis[sel_fis$value>=IOU,]->sel_fis
          if(nrow(sel_fis)>0){
            over_bbox<-unique(c(sel_fis$Var1,sel_fis$Var2))
            non_over_temp[[length(non_over_temp)+1]]<-dupl_yolo[grepl(paste0("^",over_bbox,"$",collapse = "|"),rownames(dupl_yolo)),][order(dupl_yolo[grepl(paste0("^",over_bbox,"$",collapse = "|"),rownames(dupl_yolo)),]$conf,decreasing = T),][1,]
          } else {
            dupl_yolo[u,]->non_over_temp[[length(non_over_temp)+1]]
          } 
        }
        
        if(is.null(non_over_temp)){
          non_over_bbox[[length(non_over_bbox)+1]]<-dupl_yolo
        } else {
          do.call("rbind",non_over_temp)->non_over_temp
          non_over_bbox[[length(non_over_bbox)+1]]<-non_over_temp[!duplicated(non_over_temp),]  
        }
      } else {
        non_over_bbox[[length(non_over_bbox)+1]]<-dupl_yolo
      }}
      
      non_over_bbox[[1]]->coords
    }
    
    if(save){
      doc = newXMLDoc()
      root = newXMLNode("annotation", doc = doc)
      fldNode = newXMLNode("folder", paste0(c(strsplit(ann[y],"/")[[1]][-length(strsplit(ann[y],"/")[[1]])]),collapse = "/"), parent = root)
      fileNode = newXMLNode("filename", sapply(strsplit(gsub(".txt",".jpg",lapply(strsplit(ann[y],"/"),tail,1)[[1]]),"/"), tail, 1), parent = root)
      pathNode = newXMLNode("path",gsub(".txt",".jpg",ann[y]), parent = root)
      srcNode = newXMLNode("source", parent = root)
      dataNode = newXMLNode("database","Unknown", parent = srcNode)
      sizeNode = newXMLNode("size", parent = root)
      widthNode = newXMLNode("width",image_info(IMG)$width, parent = sizeNode)
      heightNode = newXMLNode("height",image_info(IMG)$height, parent = sizeNode)
      depthNode = newXMLNode("depth","3", parent = sizeNode)
      segNode = newXMLNode("segmented","0", parent = root)
      
      for (k in 1:nrow(coords)) {
        objNode = newXMLNode("object", parent = root)
        namNode = newXMLNode("name", coords$ID[k], parent = objNode)
        poseNode = newXMLNode("pose", "Unspecified", parent = objNode)
        trnNode = newXMLNode("truncated", "0", parent = objNode)
        dffNode = newXMLNode("difficult", "0", parent = objNode)
        bbxNode = newXMLNode("bndbox", parent = objNode)
        xmiNode = newXMLNode("xmin",coords$xmin[k], parent = bbxNode)
        ymiNode = newXMLNode("ymin",coords$ymin[k], parent = bbxNode)
        xmxNode = newXMLNode("xmax",coords$xmax[k], parent = bbxNode)
        ymxNode = newXMLNode("ymax",coords$ymax[k], parent = bbxNode) 
        #}
      }
      saveXML(doc, file=file.path(savePath,gsub(".txt",".xml",lapply(strsplit(ann[y],"/"),tail,1)[[1]]))) 
    }
    
    return(data.frame(coords,file=ann[y]))
    
  }) 
  return(do.call("rbind",new_anns))
}
