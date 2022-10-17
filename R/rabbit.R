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


data(data_rabbit)
#' @title Data for Multiple Comparison Test
#' @name   rabbit
#' @docType data
#' @keywords datasets
#' @usage data(rabbit)
#' @description
#' Twelve homeowners are selected randomly to participate in an experiment with
#' a plant nursery. Each homeowner is asked to select four fairly identical
#' areas in his yard and to plant four different types of rabbites, one in each
#' area.
#'
#' Each of the 12 blocks consists of four fairly identical plots of land, each
#' receiving care of approximately the same degree of skill because the four
#' plots are presumably cared for by the same homeowern.
#'
#' @format
#' A \code{data.frame} with 48 observations on the following 3 variables.
#'
#'  @details
#'         \itemize{
#'         \item{\strong{judge}} judge
#'         \item{\strong{trt}} a factor with levels \code{t1}, \code{t2}, \code{t3}, and \code{t4}
#'         \item{\strong{evaluation}} evaluation
#'         }
#'
#' @references
#' Practical Nonparametrics Statistics. W.J. Conover, 1999
#'
#' @source
#' Book: Practical Nonparametrics Statistics, pag 372.
#'
#' @keywords datasets
#' @examples
#'
#' data(rabbit)
#' str(rabbit)
#'
