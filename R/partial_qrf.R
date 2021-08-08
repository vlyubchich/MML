#' Partial Dependence Plot for Quantile Random Forest
#'
#' Get a data frame for partial dependence plots from a quantile random forest.
#' The plots can be obtained with a variety of plotting functions from other packages.
#'
#' @param object a \code{\link[ranger]{ranger}} quantile random forest object.
#' @param pred.var character string giving the names of the predictor variables of
#' interest (see \code{\link[pdp]{partial}}).
#' @param Q a numeric vector of probabilities for which the plot is desired.
#' @param ... other arguments passed to \code{\link[pdp]{partial}}.
#'
#' @return A \code{data.frame} with the following columns:
#' the predictor variables supplied in \code{pred.var},
#' response variable (the name is extracted from the \code{ranger} function call),
#' and, if \code{length(Q) > 1}, one more column named \code{"Quantile"}.
#'
#' @seealso \code{\link[pdp]{partial}}, \code{\link[ranger]{ranger}}
#'
#' @keywords quantile forest
#'
#' @importFrom pdp partial
#' @importFrom stats predict
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Swiss
#' library(dplyr)
#' library(ggplot2)
#'
#' # fit a quantile random forest
#' qrf <- ranger::ranger(Examination ~ ., data = swiss, quantreg = TRUE, num.trees = 50)
#'
#' # a plot for one quantile
#' partial_qrf(qrf, pred.var = "Agriculture", Q = 0.5) %>%
#'     ggplot(aes(Agriculture, Examination)) +
#'     geom_line()
#'
#' # a plot for several quantiles, with decorations
#' library(hrbrthemes)
#' library(viridis)
#' partial_qrf(qrf, pred.var = "Agriculture") %>%
#'     ggplot(aes(Agriculture, Examination, group = Quantile, color = Quantile)) +
#'     geom_line() +
#'     scale_color_viridis(discrete = TRUE) +
#'     theme_ipsum()
#'
#' # a plot for one quantile for 2 predictors (takes longer, so save as an object)
#' df <- partial_qrf(qrf, pred.var = c("Agriculture", "Catholic"), Q = 0.5)
#' ggplot(df, aes(Agriculture, Catholic)) +
#'     geom_tile(aes(fill = Examination)) +
#'     scale_fill_viridis() +
#'     theme_ipsum() +
#'     labs(fill = "Median\nexamination")
#'}
#'
partial_qrf <- function(object, pred.var
                        ,Q = c(0.05, 0.5, 0.95)
                        ,...)
{
    Q <- sort(unique(Q))
    predfun <- function(object, newdata) {
        qpred <- predict(object, newdata, type = "quantiles", quantiles = Q)$predictions
        tmp <- c(apply(qpred, 2, mean))
        names(tmp) <- paste0("q", Q*100)
        return(tmp)
    }
    pdpout <- pdp::partial(object
                           ,pred.var = pred.var
                           ,pred.fun = predfun
                           ,...)
    if ("call" %in% ls(object)) {
        response <- as.character(object$call)
        response <- base::strsplit(response[2], "\\ ")[[1]][1]
        colnames(pdpout)[length(pred.var) + 1] <- response
    }
    if (length(Q) > 1) {
        colnames(pdpout)[ncol(pdpout)] <- "Quantile"
    }
    return(pdpout)
}
