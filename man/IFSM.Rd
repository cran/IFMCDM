% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IFMCDM.r
\name{IFSM}
\alias{IFSM}
\title{Implementation of the Intuitionistic Fuzzy Synthetic Measure Method for Fuzzy Multi-Criteria Decision Making Problems}
\usage{
IFSM(
  data,
  d = "e",
  w = rep(3/ncol(data), ncol(data)/3),
  z = rep("b", ncol(data)/3),
  p = "dataBounds"
)
}
\arguments{
\item{data}{The data matrix (\emph{m} x \emph{n*3}) with the values of  \emph{mi} \emph{ni} and \emph{pi} (three columns for each intuitionistic fuzzy representation of criteria for each alternative) where \emph{m} is the number of alternatives and \emph{n} is the number of criteria.}

\item{d}{Distance "euclidean" or "hamming".}

\item{w}{A vector of length \emph{n}, containing the crisp weights for the criteria (one value for intuitionistic fuzzy representation).}

\item{z}{A vector  of length \emph{n}, with preferences type for each criterion with "b" (benefit) and "c" (cost).}

\item{p}{Ideal point calculation type with one of two values: "dataBounds" – ideal point contains max and min values from the dataset – see details; "idealBounds" – ideal point contains 1 and 0’s - see details.}
}
\value{
\code{IFSM} returns a data frame that contains the scores of the Intuitionistic Fuzzy Synthetic Measure (IFSM) and the ranking of the alternatives.
}
\description{
The \code{IFSM} - Intuitionistic Fuzzy Synthetic Measure Method for Fuzzy Multi-Criteria Decision Making Problems. Reference describing the method: Jefmański, Roszkowska, Kusterka-Jefmańska (2021) \doi{10.3390/e23121636}
}
\details{
For p="dataBounds"  the actual ideal point is calculated for benefits  as maximum from all values for \emph{mi} and min for \emph{ni} (\emph{pi} = 1- \emph{mi} - \emph{ni}); in the case of costs, minimal value for \emph{mi} and max for \emph{ni} (\emph{pi} = 1- \emph{mi} - \emph{ni}).
For p="idealBounds" for benefitss is 1 for \emph{mi} and 0 for \emph{ni} (\emph{pi} = 1- \emph{mi} - \emph{ni} ). In the case of costs it is 0 for \emph{mi} and 1 for \emph{ni} (\emph{pi} = 1- (\emph{mi} - \emph{ni}).
}
\examples{
set.seed(823)
data<-sample(1:7,26*13*8,replace=TRUE)
dim(data)<-c(26*13,8)
nrColumns<-8
primary<-data.frame(name=rep(LETTERS,each=13),data)
f<-IFconversion(primary)
print(f)
m<-IFSM(f)
print(m)
}
\references{
Jefmański B, Roszkowska E, Kusterka-Jefmańska M. Intuitionistic Fuzzy Synthetic Measure on the Basis of Survey Responses and Aggregated Ordinal Data. Entropy. 2021; 23(12):1636. \doi{10.3390/e23121636}

Roszkowska E, Jefmański B, Kusterka-Jefmańska M. On Some Extension of Intuitionistic Fuzzy Synthetic Measures for Two Reference Points and Entropy Weights. Entropy. 2022; 24(8):1081. \doi{10.3390/e24081081}

Xu, Z. Some Similarity Measures of Intuitionistic Fuzzy Sets and Their Applications to Multiple Attribute Decision Making. Fuzzy Optimization and Decision Making. 2007; 6: 109–121. \doi{10.1007/s10700-007-9004-z}
}
