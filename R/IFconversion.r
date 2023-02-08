
#' Aggregation of primary data into Intuitionistic Representation
#'
#' @description The \code{IFconversion} - Aggregation of primary data into Intuitionistic Representation. Reference describing the method: Jefmański (2020) \doi{10.1007/978-3-030-52348-0_4}
#' @param primary dataset with object names (not aggregated) in first column
#' @param u cut level
#' @param u_is_neutral if exact value of variable is equal to u (cut_level) the variable is treated as neutral (TRUE) or negative (FALSE)
#' @return \code{IFconversion} returns the decision matrix (\emph{m} x \emph{n*3}) with the values of the \emph{mi} \emph{ni} and \emph{pi} (three columns for each fuzzy representation), for the \emph{n} criteria
#' @references Jefmański Bartłomiej, Intuitionistic Fuzzy Synthetic Measure for Ordinal Data. in: Classification and Data Analysis: Theory and Applications / Jajuga Krzysztof, Batóg Jacek, Walesiak Marek (eds.), Studies in Classification, Data Analysis, and Knowledge Organization, 2020, Cham, Springer, 53-72. \doi{10.1007/978-3-030-52348-0_4}
#' @examples
#' set.seed(61222)
#' data<-sample(1:7,26*13*8,replace=TRUE)
#' dim(data)<-c(26*13,8)
#' nrColumns<-8
#' primary<-data.frame(name=rep(LETTERS,each=13),data)
#' inth<-IFconversion(primary)
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @export
IFconversion<-function(primary, # data frame with object names (not aggregated) in first column
                                                  u =round(mean(c(min(primary[,-1],na.rm = TRUE),max(primary[,-1],na.rm = TRUE)))),u_is_neutral =TRUE) #cut level
{
  if(!any(class(primary)=="data.frame")){
    primary<-as.data.frame(primary)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop(
      "Package \"dplyr\" must be installed to use this function.",
      call. = FALSE
    )
  }
  #primary=mutate(primary, across(everything(), ~if_na(.x, -1)))
  if (length(u)!=1 && length(u)!=ncol(primary)-1) {
    stop(
      "u parameter must have length equal to 1 or to number of variables",
      call. = FALSE
    )
  }
  if(length(u)==1){
    u=rep(u,ncol(primary)-1)
  }
  names<-unique(primary[,1])
  clnames<-colnames(primary[,-1])
  namecolumn<-colnames(primary)[1]
  fuzzy<-array(0,c(length(names),(ncol(primary)-1)*3))
  for(i in 1:length(names)){
    for(j in 1:(ncol(primary)-1)){
      t<-filter(primary,!!as.name(namecolumn)==names[i])%>%select(!!as.name(clnames[j]))
      N<-nrow(t)
      mi<-sum(t>u[j],na.rm = TRUE)/N
      if(u_is_neutral){
        ni<-sum(t<u[j],na.rm = TRUE)/N
      }
      else{
        ni<-sum(t<=u[j],na.rm = TRUE)/N
      }
      pi<-1-mi-ni
      # rounding accuracy
      if(pi<0){
        pi=0
      }
      fuzzy[i,3*j-2]<-mi
      fuzzy[i,3*j-1]<-ni
      fuzzy[i,3*j]<-pi
    }
  }
  rownames(fuzzy)<-names
  colnames(fuzzy)<-paste(rep(colnames(primary)[-1],each=3),c("mi","ni","pi"))
  fuzzy
}


#' The sample intuitionistic fuzzy dataset
#'
#' @description The sample intuitionistic fuzzy dataset
#' @docType data
#' @keywords data
#' @examples
#' set.seed(61222)
#' data(data_IF)
#' m<-IFSM(data_IF)
#' print(m)
"data_IF"


# set.seed(61222)
# data<-sample(1:7,26*13*8,replace=TRUE)
# dim(data)<-c(26*13,8)
# nrColumns<-8
# primary<-data.frame(name=rep(LETTERS,each=13),data)
# inth<-IFconversion(primary)

# data_IF<-matrix(c(0.400,0.400,0.200,0.700,0.100,0.200,0.500,0.300,0.200,0.300,0.700,0.000,0.600,0.200,0.200,0.700,0.100,0.200,0.500,0.400,0.100,0.600,0.200,0.200,0.600,0.200,0.200,0.400,0.500,0.100,0.500,0.300,0.200,0.700,0.200,0.100,0.700,0.200,0.100,0.700,0.100,0.200,0.300,0.600,0.100),5,9,byrow=T)
# dimnames(data_IF)[[1]]<-paste("A",1:5,sep="")
# dimnames(data_IF)[[2]]<-paste(paste("C",rep(1:3,each=3),sep=""),rep(c("mi","ni","pi"),3),sep="_")
# IFSM(data_IF)[1,2]
# save(data_IF,file="data/data_IF.rda")
