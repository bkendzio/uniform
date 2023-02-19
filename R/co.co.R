#' @title co.co
#'
#' @description Uniforms a differently or incompletely reported continuous outcome across studies included in a meta-analysis.
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
#' @param m A vector of medians of the outcome in groups at time points.
#'
#' @param s A vector of standard deviations of the outcome in groups at time points.
#'
#' @param se A vector of standard errors of the mean of the outcome in groups at time points.
#'
#' @param ll95 An optional vector of the lower limits of the 95 percent confidence interval of the mean outcome in groups at time points.
#'
#' @param ul95 An optional vector of the upper limits of the 95 percent confidence interval of the mean outcome in groups at time points.
#'
#' @param ll90 A vector of the lower limits of the 90 percent confidence interval of the mean outcome in groups at time points.
#'
#' @param ul90 A vector of the upper limits of the 90 percent confidence interval of the mean outcome in groups at time points.
#'
#' @param ll99 A vector of the lower limits of the 99 percent confidence interval of the mean outcome in groups at time points.
#'
#' @param ul99 A vector of the upper limits of the 99 percent confidence interval of the mean outcome in groups at time points.
#'
#' @param a A vector of minima of the outcome in groups at time points.
#'
#' @param b A vector of maxima of the outcome in groups at time points.
#'
#' @param lq A vector of lower quartils of the outcome in groups at time points.
#'
#' @param uq A vector of upper quartils of the outcome in groups at time points.
#'
#' @param data A data frame containing the study information.
#'
#' @param meanSd.meanAB An optional character string indicating which method is used for obtaining mean and standard deviation
#' from mean and range. Either "range" for the range method (default), or "walterYao" for the method by Walter and Yao.
#'
#' @param meanSd.mIqrRange An optional character string indicating which method is used for obtaining mean and standard deviation
#'  from median, interquartile range, and range. Either "bland" for the method by bland, or "wanEtal" for the method by Wan et al. (default).
#'
#' @param meanSd.mIqr An optional character string indicating which method is used for obtaining mean and standard deviation
#'  from median and interquartile range. Either "wanEtal" for the method by Wan et al. (default), or "cochrane" for Cochrane method.
#'
#' @param meanSd.mRange An optional character string indicating which method is used for obtaining mean and standard deviation
#'  from median and range. Either "hozoEtal" for the method by Hozo et al. or "wanEtal" for the method by Wan et al. (default).
#'
#' @param meanSd.mean An optional character string indicating which method is used for obtaining mean and standard deviation
#'  from mean and missing standard deviation by imputation. Either "furukawaEtal" for the method by Furukawa et al. (default), or "marinhoEtal" for the method by Marinho et al..
#'
#' @return A data frame object that contains study labels, group labels, time points of outcome measurement,
#' numbers of observations in groups at time points, means of outcome in groups at time points, standard deviations of outcome in groups at time points.
#'
#' @examples
#' data(dataCoRaw)
#' dataCoUniform<-co.co(data=dataCoRaw)
#' @export

co.co<-function(study,group,t,n,mean,m,s,se,ll95,ul95,ll90,ul90,ll99,ul99,a,b,lq,uq,data=NULL,
                meanSd.meanAB="range",
                meanSd.mIqrRange="wanEtal",
                meanSd.mIqr="wanEtal",
                meanSd.mRange="wanEtal",
                meanSd.mean="furukawaEtal")
{

  if(!is.null(data)){
    study<-data$study
    group<-data$group
    t<-data$t
    n<-data$n
    mean<-data$mean
    m<-data$m
    s<-data$s
    se<-data$se
    ll95<-data$ll95
    ul95<-data$ul95
    ll90<-data$ll90
    ul90<-data$ul90
    ll99<-data$ll99
    ul99<-data$ul99
    a<-data$a
    b<-data$b
    lq<-data$lq
    uq<-data$uq
  }

  imputationHelp1<-mean(s,na.rm=TRUE)
  imputationHelp2<-ifelse(!is.na(mean(s,na.rm=TRUE))&!is.na(mean(mean,na.rm=TRUE)),lm(log(s)~log(abs(mean)),na.action="na.exclude"),NA)

  #Obtaining Mean and Standard Deviation from Mean and Standard Error
  ##Algebraic Conversion Method
  mean<-ifelse(!is.na(mean)&is.na(s)&!is.na(se),
               mean
               ,mean)
  s<-ifelse(!is.na(mean)&is.na(s)&!is.na(se),
            se*sqrt(n)
            ,s)

  #Obtaining Mean and Standard Deviation from Mean and Confidence Interval
  ##Algebraic Conversion Method
  mean<-ifelse(!is.na(mean)&is.na(s)&!is.na(ll95)&!is.na(ul95),
               mean
               ,mean)
  s<-ifelse(!is.na(mean)&is.na(s)&!is.na(ll95)&!is.na(ul95),
            sqrt(n)*(ul95-ll95)/3.92
            ,s)
  mean<-ifelse(!is.na(mean)&is.na(s)&!is.na(ll90)&!is.na(ul90),
               mean
               ,mean)
  s<-ifelse(!is.na(mean)&is.na(s)&!is.na(ll90)&!is.na(ul90),
            sqrt(n)*(ul90-ll90)/3.29
            ,s)
  mean<-ifelse(!is.na(mean)&is.na(s)&!is.na(ll99)&!is.na(ul99),
               mean
               ,mean)
  s<-ifelse(!is.na(mean)&is.na(s)&!is.na(ll99)&!is.na(ul99),
            sqrt(n)*(ul90-ll90)/5.15
            ,s)

  #Obtaining Mean and Standard Deviation from Mean and Range
  ## Range Method
  mean<-ifelse(!is.na(mean)&is.na(s)&!is.na(a)&!is.na(b)&meanSd.meanAB=="range",
               mean
               ,mean)
  s<-ifelse(!is.na(mean)&is.na(s)&!is.na(a)&!is.na(b)&meanSd.meanAB=="range",
            (b-a)/4
            ,s)
  ## Method by Walter and Yao
  mean<-ifelse(!is.na(mean)&is.na(s)&!is.na(a)&!is.na(b)&meanSd.meanAB=="walterYao",
               mean
               ,mean)
  f=ifelse(n/2==10,0.325,
           ifelse(n/2==11,0.315,
                  ifelse(n/2==12,0.307,
                         ifelse(n/2==13,0.300,
                                ifelse(n/2==14,0.294,
                                       ifelse(n/2==15,0.288,
                                              ifelse(n/2==16,0.283,
                                                     ifelse(n/2==17,0.279,
                                                            ifelse(n/2==18,0.275,
                                                                   ifelse(n/2==19,0.271,
                                                                          ifelse(n/2>=20 & n/2<=22,0.268,
                                                                                 ifelse(n/2>=23 & n/2<=27,0.254,
                                                                                        ifelse(n/2>=28 & n/2<=34,0.245,
                                                                                               ifelse(n/2>=35 & n/2<=44,0.231,
                                                                                                      ifelse(n/2>=45 & n/2<=54,0.222,
                                                                                                             ifelse(n/2>=55 & n/2<=64,0.216,
                                                                                                                    ifelse(n/2>=65 & n/2<=74,0.210,
                                                                                                                           ifelse(n/2>=75 & n/2<=84,0.206,
                                                                                                                                  ifelse(n/2>=85 & n/2<=94,0.202,
                                                                                                                                         0.199)))))))))))))))))))
  s<-ifelse(!is.na(mean)&is.na(s)&!is.na(a)&!is.na(b)&meanSd.meanAB=="walterYao",
            f*(b-data$a)
            ,s)

  #Obtaining Mean and Standard Deviation from Median, Interquartile Range, and Range
  ## Method by Bland
  mean<-ifelse(is.na(mean)&is.na(s)&!is.na(m)&!is.na(lq)&!is.na(uq)&!is.na(a)&!is.na(b)&meanSd.mIqrRange=="bland",
               ((n+3)*a+2*(n)*(lq+m+uq)+(n+3)*b)/(8*n)
               ,mean)
  s<-ifelse(is.na(mean)&is.na(s)&!is.na(m)&!is.na(lq)&!is.na(uq)&!is.na(a)&!is.na(b)&meanSd.mIqrRange=="bland",
            sqrt(1/16*(a^2+2*lq^2+2*m^2+2*uq^2+b^2)+
                   1/8*(a*lq+lq*m+m*uq+uq*b)-
                   1/64*(a+2*lq+2*m+2*uq+b)^2)
            ,s)
  ## Method by Wan et al.
  mean<-ifelse(is.na(mean)&is.na(s)&!is.na(m)&!is.na(lq)&!is.na(uq)&!is.na(a)&!is.na(b)&meanSd.mIqrRange=="wanEtal",
               (a+2*lq+2*m+2*uq+b)/8
               ,mean)
  s<-ifelse(is.na(mean)&is.na(s)&!is.na(m)&!is.na(lq)&!is.na(uq)&!is.na(a)&!is.na(b)&meanSd.mIqrRange=="wanEtal",
            (b-a)/(4*qnorm((n-0.375)/(n+0.25)))+(uq-lq)/(4*qnorm((0.75*n-0.125)/(n+0.25)))
            ,s)

  #Obtaining Mean and Standard Deviation from Median and Interquartile Range
  ## Method by Wan et al.
  mean<-ifelse(is.na(mean)&is.na(s)&!is.na(m)&!is.na(lq)&!is.na(uq)&meanSd.mIqr=="wanEtal",
               (lq+m+uq)/3
               ,mean)
  s<-ifelse(is.na(mean)&is.na(s)&!is.na(m)&!is.na(lq)&!is.na(uq)&meanSd.mIqr=="wanEtal",
            (uq-lq)/(2*qnorm((0.75*n-0.125)/(n+0.25)))
            ,s)
  ## Cochrane Method
  mean<-ifelse(is.na(mean)&is.na(s)&!is.na(m)&!is.na(lq)&!is.na(uq)&meanSd.mIqr=="cochrane",
               m
               ,mean)
  s<-ifelse(is.na(mean)&is.na(s)&!is.na(m)&!is.na(lq)&!is.na(uq)&meanSd.mIqr=="cochrane",
            (uq-lq)/1.35
            ,s)

  #Obtaining Mean and Standard Deviation from Median and Range
  ## Method by Hozo et al.
  mean<-ifelse(is.na(mean)&is.na(s)&!is.na(m)&!is.na(a)&!is.na(b)&meanSd.mRange=="hozoEtal",
               ifelse(n<=25,(a+2*m+b)/4,m)
               ,mean)
  s<-ifelse(is.na(mean)&is.na(s)&!is.na(m)&!is.na(a)&!is.na(b)&meanSd.mRange=="hozoEtal",
            ifelse(n<=15,sqrt(1/12*(((a+2*m+b)^2/4)+(b-a)^2)),
                   ifelse(n>15 & n<=70,(b-a)/4,(b-a)/6))
            ,s)
  ## Method by Wan et al.
  mean<-ifelse(is.na(mean)&is.na(s)&!is.na(m)&!is.na(a)&!is.na(b)&meanSd.mRange=="wanEtal",
               (a+2*m+b)/4
               ,mean)
  s<-ifelse(is.na(mean)&is.na(s)&!is.na(m)&!is.na(a)&!is.na(b)&meanSd.mRange=="wanEtal",
            (b-a)/(2*qnorm((n-0.375)/(n+0.25)))
            ,s)

  #Obtaining Mean and Standard Deviation from Mean and Missing Standard Deviation by Imputation
  ## Method by Furukawa et al.
  mean<-ifelse(!is.na(mean)&is.na(s)&!is.na(imputationHelp1)&meanSd.mean=="furukawaEtal",
               mean
               ,mean)
  s<-ifelse(!is.na(mean)&is.na(s)&!is.na(imputationHelp1)&meanSd.mean=="furukawaEtal",
            imputationHelp1
            ,s)
  ## Method by Marinho et al.
  mean<-ifelse(!is.na(mean)&is.na(s)&!is.na(imputationHelp2)&meanSd.mean=="marinhoEtal",
               mean
               ,mean)
  s<-ifelse(!is.na(mean)&is.na(s)&!is.na(imputationHelp2)&meanSd.mean=="marinhoEtal",
            exp(imputationHelp2$coefficients[2]*log(abs(mean))+imputationHelp2$coefficients[1])
            ,s)

  data3<-data.frame(study,group,t,n,mean,s)
  return(data3)
}


