\name{scheffe_contrast}
\alias{scheffe_contrast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scheffe_contrast(y,gr,coef1 = c(2,1,1), coef2 = c(1,1,1),
                             alpha = 0.05, conf.int = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{y}{Vector Of Data}

\item{gr}{Group Factors}

\item{coef1}{Coefficients in Linear Combinations}
\item{coef2}{Coefficients in Linear Combinations}

\item{alpha}{Significant test}

\item{conf.int}{TRUE or FALSE}


}



\examples{
library(multicon)
raa=c(1.53,1.61,3.75,2.89,3.26)
rab = c(3.89,3.68,5.70,5.62,5.79)
rac = c(8.18,5.64,7.36,8.82,7.10)
rabbit <- data.frame(raa,rab,rac)
st <- c(1.53,1.61,3.75,2.89,3.26,
        3.89,3.68,5.70,5.62,5.79,
        8.18,5.64,7.36,8.82,7.10)
gr <- factor(rep(c("A", "B", "C"), c(5,5,5)))
data_rabbit <- data.frame(st,gr)
colnames(data_rabbit) <- c("Treatment", "Stimulant")

contrast.ftest(y = st,gr,coef = c(2,-1,-1),alpha = 0.05,
                           conf.int = TRUE)

}

