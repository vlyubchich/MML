#' Goodness-of-fit Measures for Quantile Regression
#'
#' Calculate goodness-of-fit measures for quantile regression as given in
#' \insertCite{Haupt_etal_2011;textual}{MML}. Specifically, for a given quantile \eqn{v}, let
#' \eqn{p_v(u) = (v - I_{u<0})u},
#' where \eqn{I} is an indicator function with outputs \eqn{\{0,1\}};
#' and let \eqn{y_i} be the observed values,
#' \eqn{\hat{y}_i(v)} fitted values for the sample of size \eqn{n} (\eqn{i = 1,\dots,n}),
#' and \eqn{y_v} be the observed \eqn{v}-quantile. Then, \eqn{R^1} -- an analogue of \eqn{R^2} --
#' is defined as
#' \deqn{R^1(v) = 1 - \frac{\sum_{i=1}^n p_v(y_i - \hat{y}_i(v))}{\sum_{i=1}^n p_v(y_i - y_v)}}
#' and average \eqn{v}-weighted absolute error (ATWE) is
#' \deqn{ATWE(v) = n^{-1}\sum_{i=1}^n p_v(y_i - \hat{y}_i(v)).}
#' Higher \eqn{R^1} and lower ATWE are preferred.
#'
#' @param obs a numeric vector or a column matrix of observed values.
#' @param pred a numeric vector or a matrix of predicted quantiles (columns represent different quantiles).
#' @param quantiles an optional parameter specifying the quantiles for which the predictions were made.
#' If \code{pred} is a vector, \code{length(quantiles)} must be 1;
#' if \code{pred} is a matrix, \code{length(quantiles)} must be \code{ncol(pred)}.
#' If not specified, the quantiles are guessed from the \code{colnames(pred)}
#' assuming \code{pred} are predictions from \code{\link[ranger]{ranger}}
#' or \code{\link[quantreg]{rq}} (see Example 1.1 below).
#'
#' @return A matrix with the following rows: \code{R1} and \code{ATWE}.
#' The number of columns in the output matches the number of columns in \code{pred}.
#'
#' @references
#' \insertAllCited{}
#'
#' @keywords goodness-of-fit quantile
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Swiss
#' # select 30% of data for testing
#' n <- nrow(swiss)
#' testindex <- sample(1:n, 0.3*n, replace = FALSE)
#' # desired quantiles
#' qs <- c(0.025, 0.5, 0.975)
#'
#' # 1.1) quantile random forest
#' # fit a model on the training set
#' qrf <- ranger::ranger(Examination ~ ., data = swiss[-testindex,], quantreg = TRUE)
#' # predict on the testing set
#' pred_qrf <- predict(qrf, swiss[testindex,], type = "quantiles", quantiles = qs)$predictions
#' # get a summary on the testing set
#' gof_qr(swiss[testindex, "Examination"], pred_qrf)
#'
#' # 1.2) quantile regression
#' # fit a model on the training set
#' qrm <- quantreg::rq(Examination ~ ., data = swiss[-testindex,], tau = qs)
#' # predict on the testing set
#' pred_qrm <- predict(qrm, newdata = swiss[testindex,])
#' # get a summary on the testing set
#' gof_qr(swiss[testindex, "Examination"], pred_qrm)
#' }
#'
gof_qr <- function(obs, pred, quantiles = NULL) {
    pred <- cbind(pred)
    if (is.null(quantiles)) {
        # if not specified, try to get quantiles from the pred names
        quantiles <- colnames(pred)
        quantiles <- sapply(base::strsplit(quantiles, "[= ]+"), function(x) x[2])
        quantiles <- as.numeric(quantiles)
    }
    if (!is.null(quantiles) && (length(quantiles) != ncol(pred))) {
        stop("number of columns in 'pred' should match length(quantiles). Try setting 'quantiles = NULL'")
    }
    # pv function as in Eq 2 of Haupt et al. (2011)
    pv <- function(v, u) {
        (v - (u < 0)) * u
    }
    out <- sapply(1:ncol(pred), function(i) {
        e <- obs - pred[,i]
        # R1 function as in Eq 10 of Haupt et al. (2011)
        R1 <- 1 - sum(pv(quantiles[i], e)) / sum(pv(quantiles[i], obs - stats::quantile(obs, probs = quantiles[i])))
        # ATME function as in Eq 11 of Haupt et al. (2011)
        ATWE <- mean(pv(quantiles[i], e))
        c(R1 = R1, ATWE = ATWE)
    })
    colnames(out) <- paste0("quantile= ", quantiles)
    out
}
