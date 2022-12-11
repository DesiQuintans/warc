#________________________________________#
#AncovaTable.R
#Author: AVH
#Modified by: AVH
#Description: Produces an ANCOVA table
#Input: NA
#Output:NA
#________________________________________#
#' Produces Ancova table for follow up variable between treatment groups,
#' where the CO (covariate) is the baseline measurement of this variable
#'
#' This function returns variable levels at baseline, follow up
#' and the change between baseline and follow up by treatment group.
#' An analysis of covariance (ANCOVA) adjusting for baseline variable is used to estimate
#' the adjusted difference between treatment groups of follow up variable.
#'
#' @param df The dataset you want to use (can be fitered)
#' @param x The variable at baseline expressed as var_name
#' @param y The variable at follow up expressed as var_name
#' Note that your tretament group variable needs to be called: treatment.factor
#' @examples
#' @export
AncovaTable<- function(df,x,y)
{
    #Pass fields into function so can be used by dplyr
    df$x <- eval(substitute(x), df)
    df$y <- eval(substitute(y), df)
    df$z <- df$y-df$x

    outcome<- df  %>% group_by(treatment.factor) %>%
        summarise(n_B=sum(!is.na(x)),
                  mean_B=mean(x,na.rm=TRUE),
                  sd_B=sd(x,na.rm=TRUE),
                  mean_sd_B=paste0(format(round(mean(x,na.rm=TRUE),1),nsmall=1)," (",format(round(sd(x,na.rm=TRUE),1),nsmall = 1),")"),
                  median_IQR_B=paste0(format(round(median(x,na.rm=TRUE),1),nsmall=1)," (",format(round(quantile(x, 1/4,na.rm=TRUE),1),nsmall=1)," - ",format(round(quantile(x, 3/4,na.rm=TRUE),1),nsmall=1),")"),
                  n_FU=sum(!is.na(y)),
                  mean_FU=mean(y,na.rm=TRUE),
                  sd_FU=sd(y,na.rm=TRUE),
                  mean_sd_FU=paste0(format(round(mean(y,na.rm=TRUE),1),nsmall=1)," (",format(round(sd(y,na.rm=TRUE),1),nsmall=1),")"),
                  median_IQR_FU=paste0(format(round(median(y,na.rm=TRUE),1),nsmall=1)," (",format(round(quantile(y, 1/4,na.rm=TRUE),1),nsmall=1)," - ",format(round(quantile(y, 3/4,na.rm=TRUE),1),nsmall=1),")"),
                  n_CHG=sum(!is.na(z)),
                  mean_CHG=mean(z,na.rm=TRUE),
                  sd_CHG=sd(z,na.rm=TRUE),
                  mean_sd_CHG=paste0(format(round(mean(z,na.rm=TRUE),1),nsmall=1)," (",format(round(sd(z,na.rm=TRUE),1),nsmall=1),")"),
                  median_IQR_CHG=paste0(format(round(median(z,na.rm=TRUE),1),nsmall=1)," (",format(round(quantile(z, 1/4,na.rm=TRUE),1),nsmall=1)," - ",format(round(quantile(z, 3/4,na.rm=TRUE),1),nsmall=1),")")
        ) %>%
        mutate(se_B=sd_B/sqrt(n_B),
               se_FU=sd_FU/sqrt(n_FU),
               se_CHG=sd_CHG/sqrt(n_CHG)
        ) %>%
        mutate(CI_B=paste0("(",format(round(mean_B-1.96*se_B,1),nsmall=1),", ",format(round(mean_B+1.96*se_B,1),nsmall=1),")"),
               CI_FU=paste0("(",format(round(mean_FU-1.96*se_FU,1),nsmall=1),", ",format(round(mean_FU+1.96*se_FU,1),nsmall=1),")"),
               CI_CHG=paste0("(",format(round(mean_CHG-1.96*se_CHG,1),nsmall=1),", ",format(round(mean_CHG+1.96*se_CHG,1),nsmall=1),")"))

    t_outcome<-as.data.frame(t(outcome),stringsAsFactors = FALSE)
    t_outcome <- t_outcome[-1,]
    colnames(t_outcome)<- c("control","treatment")
    t_outcome<-rownames_to_column(t_outcome)
    t_outcome$B_FU<- "at six months"
    t_outcome[grep("_B",t_outcome$rowname,value=FALSE),c("B_FU")]<-"at baseline"
    t_outcome[grep("_CHG",t_outcome$rowname,value=FALSE),c("B_FU")]<-"the change"
    pattern <- paste(c("_B","_FU","_CHG"), collapse = "|")
    t_outcome$rowname <- gsub(pattern, "", t_outcome$rowname)
    t_outcome<- t_outcome[order(t_outcome$B_FU),]
    t_outcome<- t_outcome[which(t_outcome$rowname %in% c("n","mean_sd","median_IQR","CI")),c(4,1,2,3)]

    #Model
    #Baseline
    lm_raw_b <- glm(x ~ treatment.factor, data = df, family = "gaussian" )
    lm.raw_b <- lm.summary(lm_raw_b)
    #Follow up
    lm_raw_fu <- glm(y ~ treatment.factor, data = df, family = "gaussian" )
    lm.raw_fu <- lm.summary(lm_raw_fu)
    #Change in
    lm_raw_chg <- glm(z ~ treatment.factor, data = df, family = "gaussian" )
    lm.raw_chg <- lm.summary(lm_raw_chg)

    #Adjusted with with SBP at baseline
    lm_adj <- glm(y ~ treatment.factor + x, data = df, family = "gaussian" )
    lm.adj <- lm.summary(lm_adj)

    #find mean, se and 95% CI for control and treatment
    newdata<-df[c(1:2),c("treatment.factor","x")]
    newdata$x=mean(df$x,na.rm=T)
    newdata$treatment.factor <- c("Control","Text messages")
    newdata$treatment.factor <- factor(newdata$treatment.factor,levels=levels(df$treatment.factor))
    newdata_c <- newdata[1,]
    newdata_t <- newdata[2,]

    c<- predict(lm_adj,newdata_c,se.fit=T)
    t<- predict(lm_adj,newdata_t,se.fit=T)
    mdf<- as.numeric(lm_adj[["df.residual"]])


    mean_se_c=paste0(format(round(c$fit,1),nsmall=1)," (",format(round(c$se.fit,1),nsmall=1),")")
    CI_adj_c=paste0("(",format(round(c$fit-(qt(0.975, mdf)*t$se.fit),1),nsmall=1),", ",format(round(c$fit+(qt(0.975, df=mdf)*c$se.fit),1),nsmall=1),")")
    mean_se_t=paste0(format(round(t$fit,1),nsmall=1)," (",format(round(t$se.fit,1),nsmall=1),")")
    CI_adj_t=paste0("(",format(round(t$fit-(qt(0.975, mdf)*t$se.fit),1),nsmall=1),", ",format(round(t$fit+(qt(0.975, df=mdf)*t$se.fit),1),nsmall=1),")")

    t_outcome <- rbind(t_outcome, data.frame("B_FU"=c("Adjusted","Adjusted"), "rowname"=c("",""),
                                             "control"=c(mean_se_c,CI_adj_c),"treatment"=c(mean_se_t,CI_adj_t)))
    t_outcome$rowname <- c("n","Mean (SD)","Median (IQR)","(95% CI)",
                           "n","Mean (SD)","Median (IQR)","(95% CI)",
                           "n","Mean (SD)","Median (IQR)","(95% CI)",
                           "Mean (SE)","(95% CI)")



    #Create mean difference column
    mean_diff <-as.data.frame(c("",lm.raw_b[2,1],"",paste0("(",lm.raw_b[2,2],", ",lm.raw_b[2,3],")"),
                                "",lm.raw_fu[2,1],"",paste0("(",lm.raw_fu[2,2],", ",lm.raw_fu[2,3],")"),
                                "",lm.raw_chg[2,1],"",paste0("(",lm.raw_chg[2,2],", ",lm.raw_chg[2,3],")"),
                                paste(lm.adj[2,1],"<sup>*</sup>"),paste0("(",lm.adj[2,2],", ",lm.adj[2,3],")")))
    colnames(mean_diff) <- "mean_difference"
    #Create p-value column
    p_value <- as.data.frame(c("","","",lm.raw_b[2,4],
                               "","","",lm.raw_fu[2,4],
                               "","","",lm.raw_chg[2,4],
                               "",lm.adj[2,4]))
    colnames(p_value) <- "p-value"

    a<- cbind(t_outcome,mean_diff,p_value)
    return(a)
}
