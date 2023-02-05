#' Continuous outcome data of studies included in a meta-analysis
#'
#' Artificially generated data set with data of 25 interventional controlled studies that are included in a meta-analysis. The studies measured a
#' continuous outcome at baseline and postintervention and calculated the change from baseline for the intervention and control group, respectively.
#' As it is common in reality, the studies reported the continuous outcome differently, with different measures of central tendency (e.g., mean or median)
#' and dispersion (e.g., standard deviation or interquartile range). In addition, some measurements were not reported. The data set provides
#' artificial information to illustrate the use of the uniform package.
#'
#' @docType data
#'
#' @usage data(dataCoRaw)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{study}{An integer vector with study labels.}
#'  \item{group}{An integer vector with group labels.}
#'  \item{t}{An integer vector with time points of outcome measurement.}
#'  \item{n}{An integer vector with numbers of observations in groups at time points.}
#'  \item{mean}{A vector of means of the outcome in groups at time points.}
#'  \item{m}{A vector of medians of the outcome in groups at time points.}
#'  \item{s}{A vector of standard deviations of the outcome in groups at time points.}
#'  \item{se}{A vector of standard errors of the mean of the outcome in groups at time points.}
#'  \item{ll95}{A vector of the lower limits of the 95 percent confidence interval of the mean outcome in groups at time points.}
#'  \item{ul95}{A vector of the upper limits of the 95 percent confidence interval of the mean outcome in groups at time points.}
#'  \item{ll95}{A vector of the lower limits of the 90 percent confidence interval of the mean outcome in groups at time points.}
#'  \item{ul95}{A vector of the upper limits of the 90 percent confidence interval of the mean outcome in groups at time points.}
#'  \item{ll99}{A vector of the lower limits of the 99 percent confidence interval of the mean outcome in groups at time points.}
#'  \item{ul95}{A vector of the upper limits of the 99 percent confidence interval of the mean outcome in groups at time points.}
#'  \item{a}{A vector of minima of the outcome in groups at time points.}
#'  \item{b}{A vector of maxima of the outcome in groups at time points.}
#'  \item{lq}{A vector of lower quartils of the outcome in groups at time points.}
#'  \item{uq}{A vector of upper quartils of the outcome in groups at time points.}
#' }
#' @references This data set was artificially created for the uniform package.
#' @keywords datasets
#' @examples
#'
#' data(dataCoRaw)
#' head(dataCoRaw)
#'
"dataCoRaw"
