# save_label_map
#' @details
#' Salva um arquivo de label map em .yaml baseado no resultado da função "pascal_to_yolo".
#'
#' @param labmap Uma matriz com uma coluna chamada "lab" contendo as categorias do modelo em ordem alfabetica 
#' @param savePath Um caracter indicando o caminho para salvar o label map
#' 
#' @return Salva um arquivo de labelmap "data.yaml".
#' 
#'
#' @examples
#' save_label_map(labmap=data_frame, savePath="C://Path//to//folder//to//save")

save_label_map<-function(labmap,savePath){
  rbind("train: train/images", "val: val/images", "test: test/images","",paste0("nc: ",nrow(labmap)),paste0("names: ['",paste0(labmap$lab,collapse = "', '"),"']"))->labs
  write.table(labs,file = file.path(savePath,"data.yaml"),row.names = F,col.names = F,quote = FALSE)
}
