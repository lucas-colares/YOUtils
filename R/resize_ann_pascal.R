# resize_ann_pascal
#' @details
#' EN: Resize annotations in .xml (PASCAL VOC) and their respective images
#' PT: Redimensiona anotações em .xml (PASCAL VOC) e suas respectivas imagens
#' 
#' @param ann A vector with paths to the .xml annotation files // Um vetor com caminhos para os arquivos de anotação .xml
#' @param img A vector with paths to the image files // Um vetor com caminhos para os arquivos de imagens
#' @param resize A number indicating the size (in pixels) of the new images and annotation // Um número indicando o tamanho das novas imagens e anotações em pixels
#' 
#' @return A matrix with the resize PASCAL VOC annotation (in xmin, xmax, ymin, ymax format) after resizing.
#'
#' @examples
#' resize_ann_pascal(ann="C://Path//to//file.xml", img="C://Path//to//image.jpg", resize=640)

##### resize_ann_pascal ----
resize_ann_pascal <- function(ann, img, resize=640){
  if(length(ann)!=length(img)){
    print("Annotations and images do not have the same dimensions!")
    break
  }
  final_anns<-pblapply(1:length(ann),function(x){
    XML::xmlTreeParse(ann[x])->xml
    XML::xmlRoot(xml)->xml
    grep("object",names.XMLNode(xml))->obj_pos
    coords<-{}
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
    x_scale=resize/img_info$width
    y_scale=resize/img_info$height
    
    coords$xmin=coords$xmin*x_scale
    coords$xmax=coords$xmax*x_scale
    coords$ymin=coords$ymin*y_scale
    coords$ymax=coords$ymax*y_scale
    
    coords$file<-img[x]
    return(coords)
  })
  return(final_anns)
}
