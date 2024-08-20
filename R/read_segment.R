# read_segment
#' @details
#' Carrega predições de modelos de segmentação do YOLO.
#' 
#' @param ann Um vetor com os caminhos dos arquivos de anotações em formato YOLO (.txt)
#'
#' @return Uma matriz com as predições de um modelo de segmentação em formato YOLO.
#'
#' @examples
#' read_segment(ann="C://Path//to//file.txt")

##### read_segment ----
read_segment<-function(ann){
  
  df_list<-pblapply(1:length(ann),function(x){
    readLines(ann[x])->data1
    strsplit(data1," ")->data1_split
    lapply(data1_split,as.data.frame)->apply1
    lapply(apply1,t)->apply2
    lapply(apply2,as.data.frame)->apply3
    do.call(rbind.fill,apply3)->df
    apply(df,2,as.numeric)->df
    return(as.data.frame(df))  
  })
  
  all_ans<-{}
  
  for(x in 1:length(df_list)){
    for(y in 1:nrow(df_list[[x]])){
      df_list[[x]][y,!is.na(df_list[[x]][y,])]->sel_data
      sel_data[,ncol(sel_data)]->conf
      sel_data[,1]->class
      all_ans[[length(all_ans)+1]]<-data.frame(class=class,ID=y,var=c("x","y"),coord=suppressMessages(melt(sel_data[,2:(ncol(sel_data)-1)]))[,2],conf=conf,file=ann[x])
    }
  }
  
  return(do.call("rbind",all_ans))
}
