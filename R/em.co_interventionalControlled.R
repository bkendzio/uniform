#' @title em.co_interventionalControlled
#'
#' @description Calculation of effect measures for continuous continuous outcome data given by interventional controlled studies and uniformed by the co.co function.
#'
#' @param study A vector with study labels (e.g., 1 for study 1, 2 for study 2, ...).
#'
#' @param group A vector with group labels (e.g., 1 for intervention group, 2 for control group).
#'
#' @param t A vector with time points of outcome measurement (e.g., 1 for baseline, 2 for postintervention, and 3 for change from baseline).
#'
#' @param n A vector with numbers of observations in groups at time points.
#'
#' @param mean A vector of means of the outcome in groups at time points.
#'
#' @param s A vector of standard deviations of the outcome in groups at time points.
#'
#' @param data An optional data frame containing the study information.
#'
#' @param groupIntervention A character string that specifies how the elements of the group vector that define the intervention groups
#'  are labeled (e.g., "1" for intervention groups).
#'
#' @param groupControl A character string that specifies how the elements of the group vector that define the control groups
#'  are labeled (e.g., "2" for control groups).
#'
#' @param tBaseline A character string that specifies how the elements of the time point vector that define baseline measurements
#'  are labeled (e.g., "1" for baseline measurements).
#'
#' @param tPost A character string that specifies how the elements of the time point vector that define postintervention measurements
#'  are labeled (e.g., "2" for postintervention measurements).
#'
#' @param tChange A character string that specifies how the elements of the time point vector that define change from baseline measurements
#'  are labeled (e.g., "3" for change from baseline measurements).
#'
#' @param em An optional character string indicating which summary measure is calculated. Either "md" for mean difference (default), or "smd" for Hedges g' as standardized mean difference.
#'
#' @param smdMethod An optional character string indicating if Hedges g' is adjusted to account for a positive bias for small sample sizes.
#' Either "hedgesAdjusted" for Hedges adjustd g', or "hedgesUnadjusted" for Hedges unadjusted g'.
#'
#' @param combineChangePost An optional logical indicating whether the method by Follmann et al. for obtaining mean change from baseline with standard deviation
#' from mean baseline, mean postintervention, mean baseline standard deviation, and mean postintervention standard deviation is applied when mean from baseline
#' with standard deviation is missing. Default is FALSE.
#'
#' @examples
#' data(data2)
#' coUniform<-co.co(data=data2)
#' em<-em.co_interventionalControlled(data=coUniform,groupIntervention=1,groupControl=2,tBaseline=1,tPost=2,tChange=3,em="smd",smdMethod="hedgesAdjusted",combineChangePost=TRUE)
#' @export

em.co_interventionalControlled<-function(study,group,t,n,mean,s,data=NULL,groupIntervention,groupControl,tBaseline,tPost,tChange,r=NULL,em="md",smdMethod="hedgesAdjusted",combineChangePost=FALSE)
{

  if(!is.null(data)){
    study<-data$study
    group<-data$group
    t<-data$t
    n<-data$n
    mean<-data$mean
    s<-data$s
  }

  data4<-data.frame(study,group,t,n,mean,s)
  data4$t[data4$t==tBaseline]<-1
  data4$t[data4$t==tPost]<-2
  data4$t[data4$t==tChange]<-3
  data4$group[data4$t==groupIntervention]<-1
  data4$group[data4$t==groupControl]<-2

  data4<-reshape(data, v.names=c("n","mean","s"), timevar="t", idvar=c("study", "group"),
                 direction="wide")


  # Obtaining Mean Change from Baseline with Standard Deviation from Mean Baseline, Mean Postintervention, Mean Baseline Standard Deviation, and Mean Postintervention Standard Deviation
  ## Method by Follmann et al.
  data4$mean.3<-ifelse(is.na(data4$mean.3)&!is.na(data4$mean.1)&!is.na(data4$mean.2),
                       data4$mean.2-data4$mean.1
                       ,data4$mean.3)
  if(is.null(r)&!is.na(mean(data4$s.1*data4$s.2*data4$s.3,na.rm=TRUE))){
    data5<-subset(data4,!is.na(s.1)&!is.na(s.2)&!is.na(s.3))
    r<-(data5$s.1^2+data5$s.2^2-data5$s.3^2)/(2*data5$s.1*data5$s.2)
    r<-sum(r*data5$n.3)/sum(data5$n.3)
  }
  data4$s.3<-ifelse(is.na(data4$s.3)&!is.na(data4$s.1)&!is.na(data4$s.2),
                    sqrt(data4$s.1^2+data4$s.2^2-(2*r*data4$s.1*data4$s.2))
                    ,data4$s.3)

  data6<-reshape(data4, v.names=c("n.1","mean.1","s.1","n.2","mean.2","s.2","n.3","mean.3","s.3"), timevar="group", idvar=c("study"),
                 direction="wide")

  if(em=="md"&isFALSE(combineChangePost)){
    data6$md<-data6$mean.3.1-data6$mean.3.2
    data6$s.3<-sqrt(((data6$n.3.1-1)*data6$s.3.1^2+(data6$n.3.2-1)*data6$s.3.2^2)/(data6$n.3.1+data6$n.3.2-2))
    data6$se<-ifelse(!is.na(data6$md),sqrt((data6$n.3.1+data6$n.3.2)/(data6$n.3.1*data6$n.3.2)*data6$s.3^2),NA)
    data6$nIntervention<-data6$n.3.1
    data6$nControl<-data6$n.3.2
    data7<-subset(data6,select=c(study,nIntervention,nControl,md,se))
  }

  if(em=="md"&isTRUE(combineChangePost)){
    data6$mean.3.1<-ifelse((is.na(data6$mean.3.1)|is.na(data6$s.3.1)|is.na(data6$mean.3.2)|is.na(data6$s.2.2))&!is.na(data6$mean.2.1)|!is.na(data6$s.2.1)|!is.na(data6$mean.2.2)|!is.na(data6$s.2.2),
                           data6$mean.2.1,data6$mean.3.1)
    data6$mean.3.2<-ifelse((is.na(data6$mean.3.1)|is.na(data6$s.3.1)|is.na(data6$mean.3.2)|is.na(data6$s.2.2))&!is.na(data6$mean.2.1)|!is.na(data6$s.2.1)|!is.na(data6$mean.2.2)|!is.na(data6$s.2.2),
                           data6$mean.2.2,data6$mean.3.2)
    data6$s.3.1<-ifelse((is.na(data6$mean.3.1)|is.na(data6$s.3.1)|is.na(data6$mean.3.2)|is.na(data6$s.2.2))&!is.na(data6$mean.2.1)|!is.na(data6$s.2.1)|!is.na(data6$mean.2.2)|!is.na(data6$s.2.2),
                        data6$s.2.1,data6$s.3.1)
    data6$s.3.2<-ifelse((is.na(data6$mean.3.1)|is.na(data6$s.3.1)|is.na(data6$mean.3.2)|is.na(data6$s.2.2))&!is.na(data6$mean.2.1)|!is.na(data6$s.2.1)|!is.na(data6$mean.2.2)|!is.na(data6$s.2.2),
                        data6$s.2.2,data6$s.3.2)
    data6$md<-data6$mean.3.1-data6$mean.3.2
    data6$s.3<-sqrt(((data6$n.3.1-1)*data6$s.3.1^2+(data6$n.3.2-1)*data6$s.3.2^2)/(data6$n.3.1+data6$n.3.2-2))
    data6$se<-ifelse(!is.na(data6$md),sqrt((data6$n.3.1+data6$n.3.2)/(data6$n.3.1*data6$n.3.2)*data6$s.3^2),NA)
    data6$nIntervention<-data6$n.3.1
    data6$nControl<-data6$n.3.2
    data7<-subset(data6,select=c(study,nIntervention,nControl,md,se))
  }

  if(em=="smd"&smdMethod=="hedgesAdjusted"){
    data6$md<-data6$mean.3.1-data6$mean.3.2
    data6$s.3<-sqrt(((data6$n.3.1-1)*data6$s.3.1^2+(data6$n.3.2-1)*data6$s.3.2^2)/(data6$n.3.1+data6$n.3.2-2))
    data6$smd<-data6$md/data6$s.3*(gamma(((data6$n.3.1+data6$n.3.2)-2)/2)/(sqrt(((data6$n.3.1+data6$n.3.2)-2)/2)*gamma((((data6$n.3.1+data6$n.3.2)-2)-1)/2)))
    data6$se<-ifelse(!is.na(data6$md),sqrt((data6$n.3.1+data6$n.3.2)/(data6$n.3.1*data6$n.3.2)+(data6$smd^2)/(2*(data6$n.3.1+data6$n.3.2))),NA)
    data6$nIntervention<-data6$n.3.1
    data6$nControl<-data6$n.3.2
    data7<-subset(data6,select=c(study,nIntervention,nControl,smd,se))
  }

  if(em=="smd"&smdMethod=="hedgesUnadjusted"){
    data6$md<-data6$mean.3.1-data6$mean.3.2
    data6$s.3<-sqrt(((data6$n.3.1-1)*data6$s.3.1^2+(data6$n.3.2-1)*data6$s.3.2^2)/(data6$n.3.1+data6$n.3.2-2))
    data6$smd<-data6$md/data6$s.3
    data6$se<-ifelse(!is.na(data6$md),sqrt((data6$n.3.1+data6$n.3.2)/(data6$n.3.1*data6$n.3.2)+(data6$smd^2)/(2*(data6$n.3.1+data6$n.3.2))),NA)
    data6$nIntervention<-data6$n.3.1
    data6$nControl<-data6$n.3.2
    data7<-subset(data6,select=c(study,nIntervention,nControl,smd,se))
  }

  return(data7)
}
