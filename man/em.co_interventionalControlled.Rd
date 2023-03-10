% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/em.co_interventionalControlled.R
\name{em.co_interventionalControlled}
\alias{em.co_interventionalControlled}
\title{em.co_interventionalControlled}
\usage{
em.co_interventionalControlled(
  study,
  group,
  t,
  n,
  mean,
  s,
  data = NULL,
  groupIntervention,
  groupControl,
  tBaseline,
  tPost,
  tChange,
  r = NULL,
  em = "md",
  smdMethod = "hedgesAdjusted",
  combineChangePost = FALSE
)
}
\arguments{
\item{study}{A vector with study labels (e.g., 1 for study 1, 2 for study 2, ...).}

\item{group}{A vector with group labels (e.g., 1 for intervention group, 2 for control group).}

\item{t}{A vector with time points of outcome measurement (e.g., 1 for baseline, 2 for postintervention, and 3 for change from baseline).}

\item{n}{A vector with numbers of observations in groups at time points.}

\item{mean}{A vector of means of the outcome in groups at time points.}

\item{s}{A vector of standard deviations of the outcome in groups at time points.}

\item{data}{An optional data frame containing the study information.}

\item{groupIntervention}{A character string that specifies how the elements of the group vector that define the intervention groups
are labeled (e.g., "1" for intervention groups).}

\item{groupControl}{A character string that specifies how the elements of the group vector that define the control groups
are labeled (e.g., "2" for control groups).}

\item{tBaseline}{A character string that specifies how the elements of the time point vector that define baseline measurements
are labeled (e.g., "1" for baseline measurements).}

\item{tPost}{A character string that specifies how the elements of the time point vector that define postintervention measurements
are labeled (e.g., "2" for postintervention measurements).}

\item{tChange}{A character string that specifies how the elements of the time point vector that define change from baseline measurements
are labeled (e.g., "3" for change from baseline measurements).}

\item{em}{An optional character string indicating which summary measure is calculated. Either "md" for mean difference (default), or "smd" for Hedges g' as standardized mean difference.}

\item{smdMethod}{An optional character string indicating if Hedges g' is adjusted to account for a positive bias for small sample sizes.
Either "hedgesAdjusted" for Hedges adjustd g', or "hedgesUnadjusted" for Hedges unadjusted g'.}

\item{combineChangePost}{An optional logical indicating whether postintervention measurements should be used instead of change from baseline measurements for studies for which
change from baseline measurements cannot be obtained and when the absolute mean difference is used as effect measure. Default is FALSE.}
}
\value{
A data frame object that contains study labels,
numbers of observations in the intervention groups, numbers of observations in the control groups, effect measures, and standard errors for studies.
}
\description{
Calculation of effect measures for continuous continuous outcome data given by interventional controlled studies and uniformed by the co.co function.
}
\examples{
data(data2)
coUniform<-co.co(data=data2)
em<-em.co_interventionalControlled(data=coUniform,groupIntervention=1,groupControl=2,tBaseline=1,tPost=2,tChange=3,em="smd",smdMethod="hedgesAdjusted",combineChangePost=TRUE)
}
