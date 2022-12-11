#________________________________________#
#MinMax.R
#Author: AvH
#Modified by: AVH
#Description: Function for tabes of minimum and maximum values
#             for selected continuous variables in a study
#Input: NA
#Output:NA
#
#________________________________________#

#' Make table of min amd max values
#'
#' A function that returns table of minimums and maximums for continuous variables by tretament
#' group and total
#'
#' @param df The dataset that you want the statistics for
#' @param vars List of variables you want plotted in the order you want them plotted
#' @param by Group by variable
#'
#' @examples
#'
#' @export
#' @md
MinMax<- function(df,varl,by)
{

    #Pass group by variable into function so can be used by dplyr
    df$by <- eval(substitute(by), df)

    #Get minmax by group
    df1<- df %>%
        group_by(by)  %>%                            # group by
        summarise_at(varl,list(min = min, max = max),na.rm=TRUE)

    #Get minmax for all
    df2<- df %>%
        summarise_at(varl,list(min = min, max = max),na.rm=TRUE)
    df2<- cbind("Total",df2)
    names(df2)[[1]]<-names(df1)[[1]]

    #Bind by group and all together
    outcome<- rbind(df1,df2)
    t_outcome<-as.data.frame(t(outcome),stringsAsFactors = FALSE)
    colnames(t_outcome)<- t_outcome[1,]
    t_outcome <- t_outcome[-1,]

    t_outcome<-rownames_to_column(t_outcome)
    t_outcome$Timing<- "At baseline"
    pattern <- paste(c("_FU","form2"), collapse = "|")
    t_outcome[grep(pattern,t_outcome$rowname,value=FALSE),c("Timing")]<-"At six months"
    t_outcome$MinMax<- "Min"
    t_outcome[grep("max",t_outcome$rowname,value=FALSE),c("MinMax")]<-"Max"
    pattern2 <- paste(c("_B","_FU","form1_","form2_","_min","_max"), collapse = "|")
    t_outcome$rowname <- gsub(pattern2, "", t_outcome$rowname)
    t_outcome<- t_outcome[order(t_outcome$rowname,t_outcome$Timing),]
    colnames(t_outcome)<- c("Measure",levels(df$by)[[1]],levels(df$by)[[2]],"Total","Timing","MinMax")
    t_outcome<-t_outcome[,c("Measure","Timing","MinMax",levels(df$by)[[1]],levels(df$by)[[2]],"Total")]
    t_outcome[,c(4:6)]<- lapply(t_outcome[,c(4:6)], function(x) {round(as.numeric(x),1)})

    return(t_outcome)
}

