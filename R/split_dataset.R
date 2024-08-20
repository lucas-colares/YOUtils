# split_dataset
#' @details
#' Separa um banco de dados no formato YOLO em treino, validação e teste, criando pastas para cada subset de dados.
#'
#' @param ann Um vetor com os caminhos dos arquivos de anotações em formato YOLO (.txt)
#' @param img Um vetor com o caminho para as imagens, na mesma ordem e tamanho do vetor de anotações
#' @param train Um número entre 0 e 1 indicando a proporção do banco de dados que será usada para treinamento
#' @param val Um número entre 0 e 1 indicando a proporção do banco de dados que será usada para validação do modelo
#' @param test Um número entre 0 e 1 indicando a proporção do banco de dados que será usada para teste (opcional)
#' @param remove Booleano (TRUE ou FALSE) indicando se as imagens e anotações devem ser removidas da pasta original após a criação das novas pastas
#' @param savePath Um caracter indicando o caminho para salvar as imagens e anotações dos 3 subsets
#' 
#' @return Cria pastas de treino, validação e teste e insere as imagens e anotações dentro destas novas pastas 
#'
#' @examples
#' split_dataset(ann="C://Path//to//file.xml", imgs="C://Path//to//image.jpg", train=0.8, val=0.2, test=0, savePath="C://Path//to//folder//to//save", remove=TRUE)

##### split_dataset ----
split_dataset<-function(ann,imgs,train=0.8,val=0.2,test=0,savePath,remove=TRUE){
  data.frame(ann=ann,img=imgs)->yolo_data
  nrow(yolo_data)*train->ntrain
  sample(1:nrow(yolo_data),ntrain)->train_rows
  yolo_data[train_rows,]->train_set
  
  yolo_data[-train_rows,]->remaining_set
  sample(1:nrow(remaining_set),(nrow(yolo_data)*val))->valid_rows
  remaining_set[valid_rows,]->valid_set
  if(test!=0){
    remaining_set[-valid_rows,]->test_set
    
    dir.create(file.path(savePath,"test"))
    dir.create(file.path(savePath,"test","images"))
    dir.create(file.path(savePath,"test","labels"))
    
    file.copy(test_set$ann,file.path(savePath,"test","labels",unlist(lapply(strsplit(test_set$ann,"/"),tail,1))))
    file.copy(test_set$img,file.path(savePath,"test","images",unlist(lapply(strsplit(test_set$img,"/"),tail,1))))
  }
  
  dir.create(file.path(savePath,"train"))
  dir.create(file.path(savePath,"train","images"))
  dir.create(file.path(savePath,"train","labels"))
  
  dir.create(file.path(savePath,"val"))
  dir.create(file.path(savePath,"val","images"))
  dir.create(file.path(savePath,"val","labels"))
  
  file.copy(train_set$ann,file.path(savePath,"train","labels",unlist(lapply(strsplit(train_set$ann,"/"),tail,1))))
  file.copy(train_set$img,file.path(savePath,"train","images",unlist(lapply(strsplit(train_set$img,"/"),tail,1))))
  
  file.copy(valid_set$ann,file.path(savePath,"val","labels",unlist(lapply(strsplit(valid_set$ann,"/"),tail,1))))
  file.copy(valid_set$img,file.path(savePath,"val","images",unlist(lapply(strsplit(valid_set$img,"/"),tail,1))))
  
  if(test!=0){
    file.remove(c(train_set$ann,train_set$img,
                  valid_set$ann,valid_set$img,
                  test_set$ann,test_set$img))
  } else {
    file.remove(c(train_set$ann,train_set$img,
                  valid_set$ann,valid_set$img))
  }
}
