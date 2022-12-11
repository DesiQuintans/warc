#' Calculate sufftime time for Active Australia Survey
#'
#' sufftime = walktime + modtime + (2 x vigtime)
#' walktime =(walkhrs x 60) + walkmins
#' vigtime =(vighrs x 60) + vigmins
#' modtime =(modhrs x 60) + modmins
#' ignore:   all gardtime = (gardhrs x 60) + gardmins
#' and the number of sessions variable  (e.g b_aaq_numwalk), because the hours are already total for the week
#' To avoid errors due to over-reporting:
#'  -	any times greater than 840 minutes (14 hours) for a single activity type are recoded to 840 minutes.
#'  -	total times in all activities that are greater than 1680 minutes (28 hours) are recoded to 1680 mins.
#'
#' @param df (Dataframe) A dataframe with 7 columns `c("record_id", "walkhrs", "walkmins", "vighrs", "vigmins", "modhrs", "modmins")``
#'
#' @return A dataframe with columns `c("record_id", "sufftime", "walktime", "vigtime", "modtime")`.
#'
#' @examples
#'
#' @author
#' Amy von Huben
#'
#' @md
#' @export
AAS <- function(df) {

    #df<-b_pa
    names(df) <- c("record_id", "walkhrs","walkmins","vighrs","vigmins","modhrs","modmins")

    #Remove all_na rows
    df <- df[rowSums(is.na(df)) != (ncol(df)-1), ]

    #make NA zeroes
    df[,-1] <- lapply(df[,-1],function(x) {ifelse(is.na(x),0,x)})

    df <- df %>% mutate(walktime = ifelse((walkhrs*60)+ walkmins > 840, 840, (walkhrs*60)+walkmins),
                        vigtime=ifelse((vighrs * 60) + vigmins >840 ,840,(vighrs * 60) + vigmins),
                        modtime=ifelse((modhrs * 60) + modmins >840 ,840,(modhrs * 60) + modmins))
    df <- df %>%  mutate(sufftime= ifelse(walktime+modtime+(2*vigtime)>1680,1680,walktime+modtime+(2*vigtime)))
    df <- df[, c("record_id","sufftime","walktime","vigtime","modtime")]
    return(df)
}
