# loadTPS
#' @details
#' Carrega um arquivo de landmarks em .TPS como array 3d.
#' 
#' @param tps O caminho para um arquivo com extensão .TPS, geralmente criado no MorphoJ
#' 
#' @return Um array de 3 dimensões com landmarks nas linhas, coordenadas x e y nas colunas, e uma terceira dimensão que representa as diferentes imagens ou individuos.
#' 
#'
#' @examples
#' loadTPS(ann="C://Path//to//file.TPS")

##### loadTPS ----
loadTPS<-function(tps){
  read.table(tps,sep="\t",fileEncoding = "latin1")->TPSdata
  seq(0,nrow(TPSdata),12+2)->steps
  coords<-lapply(steps,function(x){
    if(x!=max(steps)){
      do.call("rbind",strsplit(TPSdata[(x+2):((x+1)+12),]," "))->temp_data
      data.frame(x=as.numeric(temp_data[,1]),y=as.numeric(temp_data[,2]))->temp_data
      return(temp_data)
    }
  })
  namez<-lapply(steps,function(x){
    if(x!=max(steps)){
      gsub(".*=| $","",TPSdata[x+12+2,])->file_name
      return(file_name)
    }
  })
  
  coords[!unlist(lapply(coords,is.null))]->coords
  namez[!unlist(lapply(namez,is.null))]->namez
  
  arr=array(unlist(coords),dim=c(12,2,length(coords)),dimnames = list(NULL,NULL,namez))
  return(arr)
}
