
#' Implementation of the Intuitionistic Fuzzy Synthetic Measure Method for Fuzzy Multi-Criteria Decision Making Problems
#'
#' @description The \code{IFSM} - Intuitionistic Fuzzy Synthetic Measure Method for Fuzzy Multi-Criteria Decision Making Problems. Reference describing the method: Jefmański, Roszkowska, Kusterka-Jefmańska (2021) \doi{10.3390/e23121636}
#' @param data The data matrix (\emph{m} x \emph{n*3}) with the values of  \emph{mi} \emph{ni} and \emph{pi} (three columns for each intuitionistic fuzzy representation of criteria for each alternative) where \emph{m} is the number of alternatives and \emph{n} is the number of criteria.
#' @param d Distance "euclidean" or "hamming".
#' @param w A vector of length \emph{n}, containing the crisp weights for the criteria (one value for intuitionistic fuzzy representation).
#' @param z A vector  of length \emph{n}, with preferences type for each criterion with "b" (benefit) and "c" (cost).
#' @param p Ideal point calculation type with one of two values: "dataBounds" – ideal point contains max and min values from the dataset – see details; "idealBounds" – ideal point contains 1 and 0’s - see details.
#' @return \code{IFSM} returns a data frame that contains the scores of the Intuitionistic Fuzzy Synthetic Measure (IFSM) and the ranking of the alternatives.
#' @references Jefmański B, Roszkowska E, Kusterka-Jefmańska M. Intuitionistic Fuzzy Synthetic Measure on the Basis of Survey Responses and Aggregated Ordinal Data. Entropy. 2021; 23(12):1636. \doi{10.3390/e23121636}
#'
#' Roszkowska E, Jefmański B, Kusterka-Jefmańska M. On Some Extension of Intuitionistic Fuzzy Synthetic Measures for Two Reference Points and Entropy Weights. Entropy. 2022; 24(8):1081. \doi{10.3390/e24081081}
#'
#' Xu, Z. Some Similarity Measures of Intuitionistic Fuzzy Sets and Their Applications to Multiple Attribute Decision Making. Fuzzy Optimization and Decision Making. 2007; 6: 109–121. \doi{10.1007/s10700-007-9004-z}
#'
#' @details For p="dataBounds"  the actual ideal point is calculated for benefits  as maximum from all values for \emph{mi} and min for \emph{ni} (\emph{pi} = 1- \emph{mi} - \emph{ni}); in the case of costs, minimal value for \emph{mi} and max for \emph{ni} (\emph{pi} = 1- \emph{mi} - \emph{ni}).
#' For p="idealBounds" for benefitss is 1 for \emph{mi} and 0 for \emph{ni} (\emph{pi} = 1- \emph{mi} - \emph{ni} ). In the case of costs it is 0 for \emph{mi} and 1 for \emph{ni} (\emph{pi} = 1- (\emph{mi} - \emph{ni}).
#' @examples
#' set.seed(823)
#' data<-sample(1:7,26*13*8,replace=TRUE)
#' dim(data)<-c(26*13,8)
#' nrColumns<-8
#' primary<-data.frame(name=rep(LETTERS,each=13),data)
#' f<-IFconversion(primary)
#' print(f)
#' m<-IFSM(f)
#' print(m)
#' @export
IFSM<- function(data, #matrix with all the alternatives
                                          d = "e", # distance "euclidean" or "hamming"
                                          w = rep(3/ncol(data),ncol(data)/3), # weights for each criterion (variable)
                                          z = rep("b",ncol(data)/3), # preferences vector for each variable with "b" (benefit) and "c" (cost) values
                                          p = "dataBounds") # Ideal Point  object vector or string with one of two values: "dataBounds" - ideal point object contains maximas and minimas from dataset, "idealBounds" - ideal point  object contains 1 and 0's -  see details.
{
  intuitionisticSyntheticMeasure(data,d,w,z,p,ap="dataBounds",type="SM")
}

#' Implementation of the Intuitionistic Fuzzy Technique for Order of Preference by Similarity to Ideal Solution for Fuzzy Multi-Criteria  Decision Making Problems
#'
#' @description The \code{IFTOPSIS} -  Intuitionistic Fuzzy Technique for Order of Preference by Similarity to Ideal Solution for Fuzzy Multi-Criteria  Decision Making. Reference describing the method: Roszkowska, Kusterka-Jefmańska, Jefmański (2021) \doi{10.3390/e23050563}
#' @param data The data matrix (\emph{m} x \emph{n*3}) with the values of  \emph{mi} \emph{ni} and \emph{pi} (three columns for each intuitionistic fuzzy representation of criteria for each alternative), where \emph{m} is the number of alternatives and \emph{n} is the number of criteria.
#' @param d Distance "euclidean" or "hamming".
#' @param w A vector of length \emph{n}, containing the crisp weights for the criteria (one value for intuitionistic fuzzy representation)
#' @param z A vector  of length \emph{n},  with preferences type for each criterion with "b" (benefit) and "c" (cost).
#' @param p Ideal point calculation type with one of two values: "dataBounds" – ideal point contains max and min values from the dataset – see details; "idealBounds" – ideal point contains 1 and 0’s - see details.
#' @param ap Anti-ideal point calculation type with one of two values: "dataBounds" – anti-ideal point contains min and max from the dataset – see details; "idealBounds" – anti-ideal point contains 0 and 1’s - see details.
#' @return \code{IFTOPSIS} returns a data frame that contains the scores of the Intuitionistic Fuzzy Technique for Order of Preference by Similarity to Ideal Solution (IFTOPSIS) and the ranking of the alternatives.
#' @details For p="dataBounds"  the actual ideal point is calculated for benefits  as maximum from all values for \emph{mi} and min for \emph{ni} (\emph{pi} = 1- \emph{mi} - \emph{ni}); in the case of costs, minimal value for \emph{mi} and max for \emph{ni} (\emph{pi} = 1- \emph{mi} - \emph{ni}).
#' For p="idealBounds" for benefitss is 1 for \emph{mi} and 0 for \emph{ni} (\emph{pi} = 1- \emph{mi} - \emph{ni} ). In the case of costs it is 0 for \emph{mi} and 1 for \emph{ni} (\emph{pi} = 1- (\emph{mi} - \emph{ni}).
#' For ap="dataBounds" the actual anti-ideal point is calculated for benefit criteria as minimum of all values for \emph{mi},  maximum of all values for \emph{ni} and \emph{pi} = 1- (\emph{mi} + \emph{ni}); in the case of cost criteria, maximum of all values for \emph{mi},
#' minimum of all values   for \emph{ni} and \emph{pi} = 1- (\emph{mi} + \emph{ni}).
#' For ap="idealBounds" in the case of benefit criteria it is 0 for \emph{mi},
#'  1 for \emph{ni}, 0 for \emph{pi}; in the case of cost criteria it is 1 for \emph{mi}, 0 for \emph{ni} and 0 for \emph{pi}.

#' @references Roszkowska E, Kusterka-Jefmańska M, Jefmański B. Intuitionistic Fuzzy TOPSIS as a Method for Assessing Socioeconomic Phenomena on the Basis of Survey Data. Entropy. 2021; 23(5):563. \doi{10.3390/e23050563}
#'
#' Xu, Z. Some Similarity Measures of Intuitionistic Fuzzy Sets and Their Applications to Multiple Attribute Decision Making. Fuzzy Optimization and Decision Making. 2007; 6: 109–121. \doi{10.1007/s10700-007-9004-z}
#'
#' @examples
#' set.seed(823)
#' data<-sample(1:7,26*13*8,replace=TRUE)
#' dim(data)<-c(26*13,8)
#' nrColumns<-8
#' primary<-data.frame(name=rep(LETTERS,each=13),data)
#' f<-IFconversion(primary)
#' m<-IFTOPSIS(f)
#' print(m)
#' @export
IFTOPSIS<- function(data, #matrix with all the alternatives
                d = "e", # distance "euclidean" or "hamming"
                w = rep(3/ncol(data),ncol(data)/3), # weights for each criterion (variable)
                z = rep("b",ncol(data)/3), # preferences vector for each variable with "b" (benefit) and "c" (cost) values
                p = "dataBounds", # Ideal point object vector or string with one of two values: "dataBounds" - ideal point  object contains maximas and minimas from dataset, "idealBounds" - ideal point object contains 1 and 0's -  see details.
                ap = "dataBounds") # Anti ideal point object vector or string with one of two values: "dataBounds" - anti ideal point  object contains minimas and maximas from dataset, "idealBounds" - anti ideal point object contains 0 and 1's -  see details.
{
  intuitionisticSyntheticMeasure(data,d,w,z,p,ap,type="TOPSIS")
}



#' @keywords internal
intuitionisticSyntheticMeasure<- function(data,d,w,z,p,ap,type)
{
  #on macos machines sometimes 0.5+0.5+0.0 isn't equal to 1
  tollerance=0.000001
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop(
      "Package \"dplyr\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if(ncol(data)%%3 !=0){
    stop ("For inthuitionistic data datasets should contain three columns for each variable")
  }
  if(any(data<0)) {
    stop ("For intuitionistic data all values should be in range 0-1")
  }
  if(any(data>1)) {
    stop ("For intuitionistic data all values should be in range 0-1")
  }
  for(i in 1:(ncol(data)%/%3)){
    if(any(abs(apply(cbind(data[,3*i-2],data[,3*i-1],data[,3*i]),1,sum)-1)>tollerance)){
      stop ("For intuitionistic fuzzy data mi,ni  and pi should sum to 1")
    }
  }
  if(length(w)!=ncol(data)%/%3) {
    stop ("length of weights vector w should be equal to number of criteria")
  }
  if(any(w<0)) {
    stop ("all weights in vector w should be in range 0-1")
  }
  if(any(w>1)) {
    stop ("all weights in vector w should be in range 0-1")
  }
  if(abs(sum(w)-1)>tollerance) {
    stop ("the weights in vector w should sum to 1")
  }
  if(!d %in% c("euclidean","e","h","hamming")){
    stop ("the distance parameter should be one of the following \"euclidean\" (\"e)\" or \"hamming\" (\"h\")")
  }
  if(length(z)!=ncol(data)%/%3) {
    stop ("length of preferences vector z should be equal to number of variables")
  }
  if(!all(z %in% c("b","c"))){
    stop ("the preferences vector z should contain values \"b\" or \"c\"")
  }
  if(length(p)==1){
    if(!p %in% c("dataBounds","idealBounds")){
      stop ("the ideal point parameter p should contain one of the following: \"dataBounds\", \"idealBounds\" or vector with ideal point coordinates ")
    }
  }
  else{
    if(length(p)!=ncol(data)){
      stop ("the ideal point parameter p should contain one of the following: \"dataBounds\", \"idealBounds\" or vector with ideal point coordinates ")
    }
    for(i in 1:(length(p)%/%3)){
      if(abs(p[,3*i-2]+p[,3*i-1]+p[,3*i]-1)>tollerance){
        stop ("For ideal point  object each three mi,ni,pi, should sum to 1")
      }
    }
  }
  if(length(ap)==1){
    if(!ap %in% c("dataBounds","idealBounds")){
      stop ("the anti ideal point parameter ap should contain one of the following: \"dataBounds\", \"idealBounds\" or vector with anti ideal point parameters ")
    }
  }else{
    if(length(ap)!=ncol(data)){
      stop ("the anti ideal point parameter ap should contain one of the following: \"dataBounds\", \"idealBounds\" or vector with anti ideal point parameters ")
    }
    for(i in 1:(length(ap)%/%3)){
      if(abs(ap[,3*i-2]+ap[,3*i-1]+ap[,3*i]-1)>tollerance){
        stop ("For anti ideal point object each three mi,ni,pi, should sum to 1")
      }
    }
  }
  if(length(p)==1){
    pattern<-NULL
    apattern<-NULL
    for(i in 1:(ncol(data)%/%3)){
      if(p=="dataBounds"){
        if(z[i]=="b"){
          mi=max(data[,3*i-2])
          ni=min(data[,3*i-1])
          pi=1-mi-ni
          ami=min(data[,3*i-2])
          ani=max(data[,3*i-1])
          api=1-ami-ani
        } else{
          mi=min(data[,3*i-2])
          ni=max(data[,3*i-1])
          pi=1-mi-ni
          ami=max(data[,3*i-2])
          ani=min(data[,3*i-1])
          api=1-ami-ani
        }
        pattern<-c(pattern,mi,ni,pi)
        apattern<-c(apattern,ami,ani,api)
      }
      else {
        if(z[i]=="b"){
          pattern<-c(pattern,1,0,0)
          apattern<-c(apattern,0,1,0)
        } else {
          pattern<-c(pattern,0,1,0)
          apattern<-c(apattern,1,0,0)
        }
      }
    }
  }
  adists<-dists<-array(0,c(nrow(data),1))
  rownames(adists)<-rownames(dists)<-rownames(data)
  # colnames(adists)<-colnames(adists)<-"Dio_plus"
  fseq<-seq(3,ncol(data),by=3)
  # print(fseq)
  for(i in 1:nrow(data)){
    if(substr(d,1,1)=="e" ){
      dists[i,]<-sqrt(1/2*sum(w*(data[i,fseq-2]-pattern[fseq-2])^2,
                            w*(data[i,fseq-1]-pattern[fseq-1])^2,
                            w*(data[i,fseq]-pattern[fseq])^2))
      adists[i,]<-sqrt(1/2*sum(w*(data[i,fseq-2]-apattern[fseq-2])^2,
                              w*(data[i,fseq-1]-apattern[fseq-1])^2,
                              w*(data[i,fseq]-apattern[fseq])^2))
    }
    else{
      dists[i,]<-1/2*sum(w*abs(data[i,fseq-2]-pattern[fseq-2]),
                               w*abs(data[i,fseq-1]-pattern[fseq-1]),
                               w*abs(data[i,fseq]-pattern[fseq]))
      adists[i,]<-1/2*sum(w*abs(data[i,fseq-2]-apattern[fseq-2]),
                         w*abs(data[i,fseq-1]-apattern[fseq-1]),
                         w*abs(data[i,fseq]-apattern[fseq]))

      }
  }
  final<-array(0,c(nrow(data),2))
  if(type=="SM"){ #synthetic
    d0mean<-mean(dists[,1])
    sd0<-sqrt(1/nrow(data)*sum((dists[,1]-d0mean)^2))
    rownames(final)<-rownames(data)
    colnames(final)<-c("IFSM","ranking")
    final[,1]<-1-dists/(d0mean+2*sd0)
  }
  if(type=="TOPSIS"){ #TOPSIS
    d0mean<-mean(dists[,1])
    sd0<-sqrt(1/nrow(data)*sum((dists[,1]-d0mean)^2))
    rownames(final)<-rownames(data)
    colnames(final)<-c("IFTOPSIS","ranking")
    final[,1]<-adists/(dists+adists)
  }
  final[,2]<-nrow(data)+1-as.vector(rank((final[,1]),ties.method = "max"))
  attr(final,"ideal-point")<-pattern
  if(type=="TOPSIS"){
    attr(final,"anti-ideal-point")<-apattern
  }
  attr(final,"dists")<-dists
  if(type=="TOPSIS"){
    attr(final,"adists")<-adists
  }
  final
}


# dane<-read.csv2("dane_Ewa.csv",row.names = 1)
# wynik<-IFTOPSIS(dane, d="h", w=c(0.4, 0.3, 0.3), z=c("c", "b", "c"),p="idealBounds")
# print(wynik)
# (rank(wynik[,1]))
# dane<-read.csv2("dane_Ewa.csv",header=TRUE,row.names=1)
# options(OutDec=",")
#
# # Obliczenie IFTOPSIS
# wynik<-IFSM(dane, d="h", w=c(0.4, 0.3, 0.3), z=c("b", "b", "c"),p="dataBounds")
# print(wynik)



