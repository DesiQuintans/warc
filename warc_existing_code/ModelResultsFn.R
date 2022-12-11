#________________________________________#
#ModelResultsFns.R
#Author: AVH
#Modified by: AVH
#Description: Functions to extract output values
#             from:
#             glm models
#             poisson (log-binomal) models for relative risk
#             Goodness of fit stats for glm models
#             coxph
#             coxme
#             glmer

#Input: NA
#Output:NA
#________________________________________#


#' Extract output from `lm`
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' @md
lm.summary <- function(x){
    # get the summary
    xs <- summary(x)
    # and the confidence intervals for the coefficients
    ci <- confint(x)
    # the table from the summary object
    coefTable <- coefficients(xs)
    # replace the Standard error / test statistic columns with the CI
    coefTable[,2:3] <- ci
    # rename appropriately
    colnames(coefTable)[2:3] <- colnames(ci)
    coefTable[,4]<- ifelse(coefTable[,4]<0.001,"<0.001",format(round(coefTable[,4],3),nsmall=3))
    coefTable[,1:3]<-format(round(as.numeric(coefTable[,1:3]),2),nsmall=2)
    # return the whole table....
    coefTable<- as.data.frame(coefTable,stringsAsFactors = FALSE)
}



#' Extract output from logistic regression (odds ratio)
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
OR.summary <- function(x){
    # get the summary
    xs <- summary(x)
    # and the confidence intervals for the coefficients
    ci = confint(x)
    # the table from the summary object
    coefTable <-coefficients(xs)
    # replace the Standard error / test statistic columns with the CI
    coefTable[,2:3] <- ci
    # rename appropriately
    colnames(coefTable)[2:3] <- colnames(ci)
    #format p-values
    coefTable[,4]<- ifelse(coefTable[,4]<0.001,"<0.001",format(round(coefTable[,4],3),nsmall=3))
    # exponentiate the appropriate columns
    coefTable[,1:3] <- format(round(exp(as.numeric(coefTable[,1:3])),2),nsmall=2)
    # return the whole table....
    coefTable<- as.data.frame(coefTable,stringsAsFactors=F)
    names(coefTable)<-c("Odds Ratio", "OR 95% CI Lower","OR 95% CI Upper","p-value")
    return(coefTable)
}



#' Extract output from Cox PH
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
cox_ph<- function(x){
    x <- summary(x)
    p.value<-x$wald["pvalue"]
    p.value<-ifelse(p.value<0.001,"<0.001",format(round(p.value,3),nsmall=3))
    beta<-round(x$coef[1], 2);#coeficient beta
    HR <-round(x$coef[2], 2) ; #exp(beta)
    HR.confint.lower <- round(x$conf.int[,"lower .95"], 2)
    HR.confint.upper <- round(x$conf.int[,"upper .95"],2)
    HR <- paste0(HR, " (",
                 HR.confint.lower, "-", HR.confint.upper, ")")
    res<-c(beta, HR, p.value)
    names(res)<-c("Coefficient", "HR (95% CI for HR)","p-value")
    return(res)
}



#' Extract output values from any logbin or logistic or poisson
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
p.summary <-function(x){
    # get the summary
    xs <-summary(x)
    # and the confidence intervals for the coefficients
    ci = confint(x)
    # the table from the summary object
    coefTable <-coefficients(xs)
    # replace the Standard error / test statistic columns with the CI
    coefTable[,2:3] <- ci
    # rename appropriately
    colnames(coefTable)[2:3] <- colnames(ci)
    # exponentiate the appropriate columns
    coefTable[,4]<- ifelse(coefTable[,4]<0.001,"<0.001",format(round(coefTable[,4],3),nsmall=3))
    coefTable[,1:3] <- format(round(exp(as.numeric(coefTable[,1:3])),2),nsmall=2)
    # return the whole table....
    coefTable<- as.data.frame(coefTable,stringsAsFactors = FALSE)
    coefTable1<- coefTable[which(!row.names(coefTable) %in% c("(Intercept)")),]
    names(coefTable1)<-c("Relative Risk", "RR 95% CI Lower","RR 95% CI Upper","p-value")
    return(coefTable1)
}



#' Extract output values from robust poisson sandwich est
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
pr.summary <-function(x){
    cov <- vcovHC(x, type="HC1") #(replicates STATA)
    std.err <- sqrt(diag(cov))
    coefTable <- cbind(Estimate= exp(coef(x)), "Robust SE" = std.err,
                       "Pr(>|z|)" = 2 * pnorm(abs(coef(x)/std.err), lower.tail=FALSE),
                       LL = exp(coef(x) - 1.96 * std.err),
                       UL = exp(coef(x) + 1.96 * std.err))

    # round the appropriate columns
    coefTable[,3]<- ifelse(coefTable[,3]<0.001,"<0.001",format(round(coefTable[,3],3),nsmall=3))
    coefTable[,c(1,4:5)] <- format(round(as.numeric(coefTable[,c(1,4:5)]),2),nsmall=2)
    coefTable<-coefTable[,c(1,4,5,3)]
    # return the whole table....
    coefTable<- as.data.frame(coefTable,stringsAsFactors = FALSE)
    coefTable1<- coefTable[which(!row.names(coefTable) %in% c("(Intercept)")),]
    names(coefTable1)<-c("Relative Risk", "RR 95% CI Lower","RR 95% CI Upper","p-value")
    return(coefTable1)
}



#' Calculate RR_raw_95%_CI_chisq
#'
#' @param df The dataset you want to use (can be fitered)
#' @param x The treatment factor with intervention as second level
#' @param y The variable of interest with level of interest as second level
#'
#' @return
#' @export
#'
#' @examples
#'
#' @md
RR_raw_95_CI_chisq<- function(df,x,y)
{
    #Pass fields into function so can be used by dplyr
    df$x <- eval(substitute(x), df)
    df$y <- eval(substitute(y), df)

    #get rr and CI
    rr_tab<-df %>% group_by(x,y) %>%
        summarise(n=n())
    a<-rr_tab[4,c("n")]
    b<- rr_tab[3,c("n")]
    c<-rr_tab[2,c("n")]
    d<-rr_tab[1,c("n")]
    rr<- (a/(a+b))/(c/(c+d))
    se<- sqrt((1/a)+(1/c)-(1/(a+b))-(1/(c+d)))
    rr_ci_l<- exp(log(rr)-1.96*se)
    rr_ci_u<- exp(log(rr)+1.96*se)

    rr_raw_95CI<-(sprintf("%.2f (%.2f, %.2f)",rr, rr_ci_l,
                          rr_ci_u))
    #get p-value from Chi-sq test
    p.value <-chisq.test(df$x,
                         df$y,
                         correct = FALSE)$p.value
    p.value<- ifelse(p.value<0.001,"<0.001",format(round(p.value,3),nsmall=3))
    rr_rawCI_chisq <-cbind(c(rr_raw_95CI),c(p.value))
    colnames(rr_rawCI_chisq)<- c("Raw Relative Risk (95% CI)"," Chi-sq P-value")
    return(rr_rawCI_chisq)
}



#' Calculate RR_raw_95%_CI_fish
#'
#' @param df The dataset you want to use (can be fitered)
#' @param x The treatment factor with intervention as second level
#' @param y The variable of interest with level of interest as second level
#'
#' @return
#' @export
#'
#' @examples
#'
#' @md
RR_raw_95_CI_fish<- function(df,x,y)
{
    #Pass fields into function so can be used by dplyr
    df$x <- eval(substitute(x), df)
    df$y <- eval(substitute(y), df)

    #get rr and CI
    rr_tab<-df %>% group_by(x,y) %>%
        summarise(n=n())
    a<-rr_tab[4,c("n")]
    b<- rr_tab[3,c("n")]
    c<-rr_tab[2,c("n")]
    d<-rr_tab[1,c("n")]
    rr<- (a/(a+b))/(c/(c+d))
    se<- sqrt((1/a)+(1/c)-(1/(a+b))-(1/(c+d)))
    rr_ci_l<- exp(log(rr)-1.96*se)
    rr_ci_u<- exp(log(rr)+1.96*se)

    rr_raw_95CI<-(sprintf("%.2f (%.2f, %.2f)",rr, rr_ci_l,
                          rr_ci_u))
    #get p-value from Chi-sq test
    p.value <-fisher.test(df$x,
                          df$y,
                          correct = FALSE)$p.value
    p.value<- ifelse(p.value<0.001,"<0.001",format(round(p.value,3),nsmall=3))
    rr_rawCI_chisq <-cbind(c(rr_raw_95CI),c(p.value))
    colnames(rr_rawCI_fish)<- c("Raw Relative Risk (95% CI)"," Fisher P-value")
    return(rr_rawCI_fish)
}



#' Extract Model GF stats
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
#' @md
AICBIC <- function(x,y) {
    ap<-  anova(x,y, test = "Chisq")
    l <-  ifelse(ap$`Pr(>Chi)`[[2]]<0.001,"<0.001",format(round(ap$`Pr(>Chi)`[[2]],3),nsmall=3))
    m<-round(mean(y$residuals^2),2)
    filled <- data.frame("LRT p-value"=l,
                         MSE=m,
                         AIC = round(AIC(y),0),
                         BIC = round(BIC(y),0))
    return(filled)
}



#' Coxme extract function
#'
#' @param object
#' @param parm
#' @param level
#' @param ...
#' @param more
#'
#' @return
#' @export
#'
#' @examples
confint.coxme <- function(object, parm=NULL, level=0.95, ..., more=FALSE){
    if(!is.null(parm)) warning("[confint.coxme] argument 'parm' doesn't do anything for this method")
    if(level != 0.95) warning("[confint.coxme] 'level' will be 0.95 regardless of what argument you give it. Ha!")
    co <- object$coef
    se <- sqrt(diag(stats::vcov(object)))
    m <- matrix(c(co - 2*se, co + 2*se), ncol=2)
    colnames(m) <- c("2.5 %", "97.5 %")
    rownames(m) <- names(co)
    if(more){
        p <- 2*stats::pnorm(abs(co/se), lower.tail=F)
        m <- cbind(m, co, p)
        #rownames(m)[3:4] <- c("coef", "p")
    }
    return (m)
}



#' GLMER extract
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
OR.glmer <- function(x){
    # get the summary
    co <- coef(summary(x))
    # and the confidence intervals for the coefficients
    ci = matrix(c(co[,1] - 2*co[,2], co[,1] + 2*co[,2]), ncol=2)
    # replace the Standard error / test statistic columns with the CI
    co[,2:3] <- ci
    #format p-values
    co[,4]<- ifelse(co[,4]<0.001,"<0.001",format(round(co[,4],3),nsmall=3))
    # exponentiate the appropriate columns
    co[,1:3] <- format(round(exp(as.numeric(co[,1:3])),2),nsmall=2)
    # return the whole table....
    coefTable<- as.data.frame(co,stringsAsFactors=F)
    names(coefTable)<-c("Odds Ratio", "OR 95% CI Lower","OR 95% CI Upper","p-value")
    return(coefTable)
}
