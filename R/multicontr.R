# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

contrast.ftest <- function(y,gr,coef = c(2,1,1),alpha = 0.05,
                           conf.int = TRUE){



  if(conf.int == TRUE){
    # With confidence interval

    ## Mean of Each Group
    rbm <- aggregate(y,
                     by = list(Category = gr),
                     FUN = mean
    )

    ## Length of each group
    lpg <- aggregate(y,
                     by = list(Category = gr),
                     FUN = length
    )

    ## ANOVA Model

    fit = aov(y~gr)

    ## Size Parameters

    a = length(lpg$x)
    N = length(y)
    n = lpg[,2]
    dfg = a-1
    dfy = N-a
    dft = N-1

    ## Mean Square Error
    mse = sum((fit$residuals)^2)/dfy

    ## Mean Estimates and Numerator
    sum.mean = round(sum(coef*rbm[,2]),digits = 2)
    sq.sum = (sum.mean)^2

    ## Mean of Squares of Coefficients and denominator
    sum.sqc = sum((coef^2)/n)

    ## Test Statistic

    fval = round(sq.sum/(mse*sum.sqc),digits = 2)

    ## Computed p-value (f-distribution)
    pvalue = pf(fval,1,dfy,lower.tail = F)

    ## (1-alpha)% Confidence Interval

    lower = sum.mean - (qt((alpha)/2,N-a,lower.tail = F))*(mse*sum.sqc)
    upper = sum.mean + (qt((alpha)/2,N-a,lower.tail = F))*(mse*sum.sqc)
    CI = c(Lowerlvl = lower, Upperlvl = upper)

    ## Summary

    test.summary = round(c(Estimate = sum.mean, MSE = mse,
                           n = n,
                           N = N, a = a, dfy = dfy, dfg = dfg,
                           Fstatistic = fval, pvalue=pvalue, CI),
                         digits = 2)
    return(test.summary)

  }
  else
    # Without confidence interval
  {

    ## ANOVA Model

    fit = aov(y~gr)

    ## Mean of each group

    rbm <- aggregate(y,
                     by = list(Category = gr),
                     FUN = mean
    )

    ## Length of each group

    lpg <- aggregate(y,
                     by = list(Category = gr),
                     FUN = length
    )

    ## Size Parameters

    a = length(lpg$x)
    N = length(y)
    n = lpg[,2]
    dfg = a-1
    dfy = N-a
    dft = N-1

    ## MSE

    mse = sum((fit$residuals)^2)/dfy

    ## Mean Estimates and Numerator

    sum.mean = round(sum(coef*rbm[,2]),digits = 2)
    sq.sum = (sum.mean)^2

    ## Mean of Squares of Coefficients and denominator

    sum.sqc = sum((coef^2)/n)

    ## Test Statistic

    fval = round(sq.sum/(mse*sum.sqc),digits = 2)

    ## Computed p-value

    pvalue = pf(fval,1,dfy,lower.tail = F)

    ## Summary

    test.summary = round(c(Estimate = sum.mean, MSE = mse,
                           n=n,
                           N = N, a = a, dfy = dfy, dfg = dfg,
                           Fstatistic = fval, pvalue=pvalue),
                         digits = 2)
    return(test.summary)
  }

}

contrast.ftest(y = data_rabbit$Treatment,gr = data_rabbit$Stimulant,
               coef = c(2,-1,-1),
               conf.int = TRUE)

contrast.ftest(y = data_rabbit$Treatment,gr = data_rabbit$Stimulant,
               coef = c(2,-1,-1),
               conf.int = F)


contrast.ftest(y = data_rabbit$Treatment,gr = data_rabbit$Stimulant,
               coef = c(0,-1,1),
               conf.int = TRUE)

contrast.ftest(y = data_rabbit$Treatment,gr = data_rabbit$Stimulant,
               coef = c(0,-1,1),
               conf.int = F)


#######################################################################

# Scheffe Method


scheffe.contrast <- function(y,gr,coef1 = c(2,1,1), coef2 = c(1,1,1),
                             alpha = 0.05, conf.int = TRUE){



  if(conf.int == TRUE){
    # With confidence interval

    ## Mean of each group
    rbm <- aggregate(y,
                     by = list(Category = gr),
                     FUN = mean
    )
    ## Length of each group
    lpg <- aggregate(y,
                     by = list(Category = gr),
                     FUN = length
    )

    ## ANOVA model

    fit = aov(y~gr)

    ## Size parameters
    a = length(lpg$x)
    N = length(y)
    n = lpg[,2]
    dfg = a-1
    dfy = N-a
    dft = N-1

    ## Mean Square Error
    mse = sum((fit$residuals)^2)/dfy

    ## Mean Estimates and Numerator for C1
    sum.mean1 = round(sum(coef1*rbm[,2]),digits = 2)

    sq.sum1 = (sum.mean1)^2

    ### Denominator

    sum.sqc1 = sum((coef1^2)/n)

    ## Mean Estimates and Numerator for C2
    sum.mean2 = round(sum(coef2*rbm[,2]),digits = 2)
    sq.sum2 = (sum.mean2)^2

    ### Denominator
    sum.sqc2 = sum((coef2^2)/n)

    ## Critical Value/Margin of Error

    sua1 = sqrt((a-1)*(qf((alpha),a-1,N-a,
                          lower.tail = F))*(mse*sum.sqc1)) # for C1
    sua2 = sqrt((a-1)*(qf((alpha),a-1,N-a,
                          lower.tail = F))*(mse*sum.sqc2)) # for C2

    ## (1-alpha)% Confidence Interval
    lower1 = sum.mean1 - sua1
    upper1 = sum.mean1 + sua1
    lower2 = sum.mean2 - sua2
    upper2 = sum.mean2 + sua2
    CI1 = c(Lowerlvl = lower1, Upperlvl = upper1)
    CI2 = c(Lowerlvl = lower2, Upperlvl = upper2)
    cu1 = abs(sum.mean1)
    cu2 = abs(sum.mean2)



    H01 = round(c(Estimate = sum.mean1, MSE = mse,
                  n = n,
                  N = N, a = a, dfy = dfy, dfg = dfg,
                  "|Cu|"= cu1,
                  "Sua" = sua1, CI1),digits = 2)
    H02 = round(c(Estimate = sum.mean2, MSE = mse,
                  n = n,
                  N = N, a = a, dfy = dfy, dfg = dfg,
                  "|Cu|"= cu2,
                  "Sua" = sua2, CI2),digits = 2)

    contrast.summary <- rbind(H01, H02)

    return(contrast.summary)
  }
  else
    # Without confidence interval
  {
    ## Mean of each group

    rbm <- aggregate(y,
                     by = list(Category = gr),
                     FUN = mean
    )

    ## Length of each group
    lpg <- aggregate(y,
                     by = list(Category = gr),
                     FUN = length
    )

    ## ANOVA model

    fit = aov(y~gr)


    ## Size parameters

    a = length(lpg$x)
    N = length(y)
    n = lpg[,2]
    dfg = a-1
    dfy = N-a
    dft = N-1

    ## MSE
    mse = sum((fit$residuals)^2)/dfy


    ## Mean Estimates and Numerator for C1
    sum.mean1 = round(sum(coef1*rbm[,2]),digits = 2)

    sq.sum1 = (sum.mean1)^2

    ### Denominator

    sum.sqc1 = sum((coef1^2)/n)

    ## Mean Estimates and Numerator for C2
    sum.mean2 = round(sum(coef2*rbm[,2]),digits = 2)
    sq.sum2 = (sum.mean2)^2

    ### Denominator
    sum.sqc2 = sum((coef2^2)/n)

    ## Critical Value/Margin of Error

    sua1 = sqrt((a-1)*(qf((alpha),a-1,N-a,
                          lower.tail = F))*(mse*sum.sqc1))
    sua2 = sqrt((a-1)*(qf((alpha),a-1,N-a,
                          lower.tail = F))*(mse*sum.sqc2))

    cu1 = abs(sum.mean1)
    cu2 = abs(sum.mean2)

    ## Estimates Compilation

    H01 = round(c(Estimate = sum.mean1, MSE = mse,
                  n = n,
                  N = N, a = a, dfy = dfy, dfg = dfg,
                  "|Cu|"= cu1,
                  "Sua" = sua1),digits = 2)
    H02 = round(c(Estimate = sum.mean2, MSE = mse,
                  n = n,
                  N = N, a = a, dfy = dfy, dfg = dfg,
                  "|Cu|"= cu2,
                  "Sua" = sua2),digits = 2)

    contrast.summary <- rbind(H01, H02)

    return(contrast.summary)
  }

}

scheffe.contrast(y = data_rabbit$Treatment,gr = data_rabbit$Stimulant,
                 coef1 = c(2,-1,-1), coef2 = c(3,-1,-2),
                 conf.int = TRUE
)

scheffe.contrast(y = data_rabbit$Treatment,gr = data_rabbit$Stimulant,
                 coef = c(2,-1,-1), coef2 = c(3,-1,-2),
                 conf.int = FALSE
)
