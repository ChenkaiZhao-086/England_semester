# Copyright 2014-2021 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# The CausalImpact package implements inference on the causal effect of an
# intervention on a time series. It uses a counterfactual-forecasting strategy
# based on a Bayesian structural time-series model.
#
# Literature:
#   Brodersen KH, Gallusser F, Koehler J, Remy N, Scott SL (under review).
#   Inferring causal impact using Bayesian structural time-series models.
#   http://research.google.com/pubs/pub41854.html
#
# Author: kbrodersen@google.com (Kay Brodersen)

# Specify defaults for <model.args>
# (should always be in sync with the documentation of CausalImpact())
.defaults <- list(niter = 1000,
                  standardize.data = TRUE,
                  prior.level.sd = 0.01,
                  nseasons = 1,
                  season.duration = 1,
                  dynamic.regression = FALSE,
                  max.flips = -1)

#' Check and format the \code{data} argument provided to \code{CausalImpact()}
#'
#' @param data A zoo object, a vector, a matrix, or a data frame.
#'
#' @return A correctly formatted zoo object.
FormatInputData <- function(data) {
  # Check if `data` is a valid data type: a zoo object, a numerical vector, a
  # matrix, or a data frame.
  assert_that(is.zoo(data) || is.data.frame(data) ||
                ((is.vector(data) || is.matrix(data)) && is.numeric(data)))
  
  # If `data` is a data frame and the first column is 'date', try to convert.
  if (is.data.frame(data) && tolower(names(data)[1]) %in% c("date", "time")) {
    if (class(data$date) == "Date") {
      data <- zoo(data[, -1], data$date)
    } else {
      warning(paste0("Did you mean: data = zoo(data[, -1], data$",
                     names(data)[1], ")"))
    }
  }
  
  # Try to convert to zoo object, and assert that data is numeric.
  data <- TryStop(as.zoo(data), "could not convert input data to zoo object")
  assert_that(is.numeric(data))
  
  # Ensure <data> is formatted in such a way that rows represent time points
  if (is.null(ncol(data))) {
    dim(data) <- c(length(data), 1)
  }
  
  # Must have at least 3 time points
  assert_that(nrow(data) > 3)
  
  # Must not have NA in covariates (if any)
  if (ncol(data) >= 2) {
    assert_that(!anyNA(data[, -1]), msg = "covariates must not have NA values")
  }
  
  # Convert data from integer to double if necessary; this avoids overflow
  # problems if data values are large (i.e., close to the maximum range of
  # integer values).
  if (is.integer(data)) {
    data.matrix <- coredata(data)
    coredata(data) <- matrix(as.numeric(data.matrix), nrow = nrow(data.matrix),
                             ncol = ncol(data.matrix),
                             dimnames = dimnames(data.matrix))
  }
  
  return(data)
}

FormatInputPrePostPeriod <- function(pre.period, post.period, data) {
  # Checks `pre.period` and `post.period` input arguments, and returns the
  # corresponding time series indices.
  #
  # Args:
  #   pre.period:  two-element vector of pre-period boundaries in the time unit
  #                of `time(data)`.
  #   post.period: two-element vector of post-period boundaries in the time unit
  #                of `time(data)`.
  #   data:        already-checked zoo object, for reference only.
  #
  # Returns:
  #   List with entries `pre.period` and `post.period`, containing indices of
  #   period boundaries (relative to `time(data)`).
  
  assert_that(!is.null(pre.period))
  assert_that(!is.null(post.period))
  assert_that(length(pre.period) == 2, length(post.period) == 2)
  assert_that(!anyNA(pre.period), !anyNA(post.period))
  assert_that(isTRUE(all.equal(class(time(data)), class(pre.period))) ||
                (is.numeric(time(data)) && is.numeric(pre.period)),
              msg = paste0("pre.period (", class(pre.period)[1], ") ",
                           "must have the same class as the time points in ",
                           "the data (", class(time(data))[1], ")"))
  assert_that(isTRUE(all.equal(class(time(data)), class(post.period))) ||
                (is.numeric(time(data)) && is.numeric(post.period)),
              msg = paste0("post.period (", class(post.period)[1], ") ",
                           "must have the same class as the time points in ",
                           "the data (", class(time(data))[1], ")"))
  if (pre.period[1] < start(data)) {
    warning(paste0("Setting pre.period[1] to start of data: ", start(data)))
  }
  if (pre.period[2] > end(data)) {
    warning(paste0("Setting pre.period[2] to end of data: ", end(data)))
  }
  if (post.period[2] > end(data)) {
    warning(paste0("Setting post.period[2] to end of data: ", end(data)))
  }
  
  period.indices <- list(
    pre.period = GetPeriodIndices(pre.period, time(data)),
    post.period = GetPeriodIndices(post.period, time(data)))
  assert_that(diff(period.indices$pre.period) >= 2,
              msg = "pre.period must span at least 3 time points")
  assert_that(period.indices$post.period[1] > period.indices$pre.period[2])
  
  return(period.indices)
}

FormatInputForCausalImpact <- function(data, pre.period, post.period,
                                       model.args, bsts.model,
                                       post.period.response, alpha) {
  # Checks and formats all input arguments supplied to CausalImpact(). See the
  # documentation of CausalImpact() for details.
  #
  # Args:
  #   data:                 zoo object or data frame
  #   pre.period:           beginning and end of pre-period
  #   post.period:          beginning and end of post-period
  #   model.args:           list of additional arguments for the model
  #   bsts.model:           fitted bsts model (instead of data)
  #   post.period.response: observed response in the post-period
  #   alpha:                tail-area for posterior intervals
  #
  # Returns:
  #   list of checked (and possibly reformatted) input arguments
  
  # Check that a consistent set of variables has been provided
  assert_that(
    xor(!is.null(data) && !is.null(pre.period) && !is.null(post.period) &&
          is.null(bsts.model) && is.null(post.period.response),
        is.null(data) && is.null(pre.period) && is.null(post.period) &&
          !is.null(bsts.model) && !is.null(post.period.response)),
    msg = paste0("must either provide data, pre.period, post.period, ",
                 "model.args; or bsts.model and post.period.response"))
  
  # Check <data> and convert to zoo, with rows representing time points
  if (!is.null(data)) {
    data <- FormatInputData(data)
  }
  
  # Check `pre.period` and `post.period`, and convert them to period indices.
  if (!is.null(data)) {
    checked <- FormatInputPrePostPeriod(pre.period, post.period, data)
    pre.period <- checked$pre.period
    post.period <- checked$post.period
  }
  
  # Parse <model.args>, fill gaps using <.defaults>
  model.args <- ParseArguments(model.args, .defaults)
  #
  # Check only those parts of <model.args> that are used in this file. The other
  # fields will be checked in FormatInputForConstructModel().
  #
  # Check <standardize.data>
  assert_that(is.scalar(model.args$standardize.data))
  assert_that(is.logical(model.args$standardize.data))
  assert_that(!is.na(model.args$standardize.data))
  
  # Check <bsts.model>
  if (!is.null(bsts.model)) {
    assert_that(class(bsts.model) == "bsts")
  }
  
  # Check <post.period.response>
  if (!is.null(bsts.model)) {
    assert_that(!is.null(post.period.response),
                is.vector(post.period.response),
                is.numeric(post.period.response))
  }
  
  # Check <alpha>
  assert_that(is.numeric(alpha))
  assert_that(is.scalar(alpha))
  assert_that(!is.na(alpha))
  assert_that(alpha > 0, alpha < 1)
  
  # Return updated arguments
  return(list(data = data, pre.period = pre.period, post.period = post.period,
              model.args = model.args, bsts.model = bsts.model,
              post.period.response = post.period.response, alpha = alpha))
}

CausalImpact <- function(data = NULL,
                         pre.period = NULL,
                         post.period = NULL,
                         model.args = NULL,
                         bsts.model = NULL,
                         post.period.response = NULL,
                         alpha = 0.05) {
  # CausalImpact() performs causal inference through counterfactual
  # predictions using a Bayesian structural time-series model.
  #
  # Detailed and up-to-date documentation is provided in
  # ../man/CausalImpact.Rd. Type ?CausalImpact to display the documentation.
  # For example code, see the package vignette
  # (http://google.github.io/CausalImpact/).
  #
  # Args:
  #   data:        Time series of response variable and any covariates. This can
  #                be a \code{zoo} object; a \code{vector}; a \code{matrix}; or
  #                a \code{data.frame}. In any of these cases, the response
  #                variable must be in the first column, and any covariates in
  #                subsequent columns. A \code{zoo} object is recommended, as
  #                its time indices will be used to format the x-axis in
  #                \code{plot()}.
  #
  #   pre.period:  A vector specifying the first and the last time point of the
  #                pre-intervention period in the response vector \code{y}. This
  #                period can be thought of as a training period, used to
  #                determine the relationship between the response variable and
  #                the covariates. If \code{data} is a \code{zoo} object with
  #                a \code{time} attribute, \code{pre.period} must be indicated
  #                using the same time scale (i.e. using the same class as
  #                \code{time(data)}, see examples). If \code{data} doesn't have
  #                a \code{time} attribute, \code{post.period} is indicated with
  #                indices.
  #
  #   post.period: A vector specifying the first and the last day of the
  #                post-intervention period we wish to study. This is the period
  #                after the intervention has begun whose effect we are
  #                interested in. The relationship between response variable and
  #                covariates, as determined during the pre-period, will be used
  #                to predict how the response variable should have evolved
  #                during the post-period had no intervention taken place. If
  #                \code{data} is a \code{zoo} object with a \code{time}
  #                attribute, \code{post.period} must be indicated using the
  #                same time scale. If \code{data} doesn't have a \code{time}
  #                attribute, \code{post.period} is indicated with indices.
  #
  #   model.args:  Optional arguments that can be used to adjust the default
  #                construction of the state-space model used for inference.
  #                For full control over the model, you can construct your own
  #                model using the \code{bsts} package and feed the fitted model
  #                into \code{CausalImpact()} (see examples).
  #
  #   bsts.model:  Instead of passing in \code{data} and having
  #                \code{CausalImpact()} construct a model, it is possible to
  #                construct a model yourself using the \code{bsts} package. In
  #                this case, omit \code{data}, \code{pre.period}, and
  #                \code{post.period}. Instead only pass in \code{bsts.model},
  #                \code{y.post}, and \code{alpha} (optional). The model must
  #                have been fitted on data where the response variable was set
  #                to \code{NA} during the post-treatment period. The actual
  #                observed data during this period must then be passed to the
  #                function in \code{y.post}.
  #
  #   post.period.response: Actual observed data during the post-intervention
  #                period. This is required if and only if a fitted
  #                \code{bsts.model} is passed instead of \code{data}.
  #
  #   alpha:       Desired tail-area probability for posterior intervals.
  #                Defaults to 0.05, which will produce central 95\% intervals.
  #
  # Returns:
  #   A CausalImpact object. This is a list of:
  #     series:  observed data, counterfactual, pointwise and cumulative impact
  #     summary: summary table
  #     report:  verbal description of the analysis
  #     model:   list with four elements \code{pre.period}, \code{post.period},
  #              \code{bsts.model} and \code{alpha}. \code{pre.period} and
  #              \code{post.period} indicate the first and last time point of
  #              the time series in the respective period, \code{bsts.model} is
  #              the fitted model returned by \code{bsts()}, and \code{alpha}
  #              is the user-specified tail-area probability.
  #
  # Optional arguments for model.args:
  #   niter:              number of MCMC iterations
  #   standardize.data:   whether to standardize the data over the
  #                       pre-intervention period before model fitting
  #   prior.level.sd:     standard deviation of the prior on the local level
  #   nseasons:           number of seasons in the seasonal component
  #   season.duration:    duration of each season
  #   dynamic.regression: whether to have dynamic instead of static coefficients
  #   max.flips:          number of variables to flip in and out of the model on
  #                       each iteration
  #
  # For more details on all of the above, see the package manual (?CausalImpact)
  # or the vignette.
  #
  # Examples:
  #   # Time series without dates:
  #   set.seed(1)
  #   x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
  #   y <- 1.2 * x1 + rnorm(100)
  #   y[71:100] <- y[71:100] + 10
  #   data <- cbind(y, x1)
  #   pre.period <- c(1, 70)
  #   post.period <- c(71, 100)
  #
  #   impact <- CausalImpact(data, pre.period, post.period)
  #
  #   summary(impact)
  #   summary(impact, "report")
  #   plot(impact)
  #
  #   # Daily time series:
  #   times <- seq.Date(as.Date("2015-01-01"), by = 1, length.out = 100)
  #   data <- zoo(cbind(y, x1), times)
  #
  #   impact <- CausalImpact(data, times[pre.period], times[post.period])
  #
  #   summary(impact)
  #   summary(impact, "report")
  #   plot(impact)
  #
  #   # Analysis based on a `bsts` model:
  #   post.period.response <- y[post.period[1] : post.period[2]]
  #   post.period.response[post.period[1] : post.period[2]] <- NA
  #   ss <- AddLocalLevel(list(), y)
  #   bsts.model <- bsts(y ~ x1, ss, niter = 1000)
  #   impact <- CausalImpact(bsts.model = bsts.model,
  #                          post.period.response = post.period.response)
  
  # Check input
  checked <- FormatInputForCausalImpact(data, pre.period, post.period,
                                        model.args, bsts.model,
                                        post.period.response, alpha)
  data <- checked$data
  pre.period <- checked$pre.period
  post.period <- checked$post.period
  model.args <- checked$model.args
  bsts.model <- checked$bsts.model
  post.period.response <- checked$post.period.response
  alpha <- checked$alpha
  
  # Depending on input, dispatch to the appropriate Run* method()
  if (!is.null(data)) {
    impact <- RunWithData(data, pre.period, post.period, model.args, alpha)
    # Return pre- and post-period in the time unit of the time series.
    times <- time(data)
    impact$model$pre.period <- times[pre.period]
    impact$model$post.period <- times[post.period]
  } else {
    impact <- RunWithBstsModel(bsts.model, post.period.response, alpha)
  }
  
  return(impact)
}

RunWithData <- function(data, pre.period, post.period, model.args, alpha) {
  # Runs an impact analysis on top of a fitted bsts model.
  #
  # Args:
  #   data:        zoo object of response variable and covariates
  #   pre.period:  two-element vector specifying the indices of the  pre-period
  #                limits.
  #   post.period: two-element vector specifying the indices of the post-period
  #                limits.
  #   model.args:  list of model arguments
  #   alpha:       tail-probabilities of posterior intervals
  #
  # Returns:
  #   See CausalImpact().
  
  # Remember original time indices, then clear
  # Note that this precludes us from benefitting from bsts's ability to fill
  # missing time points automatically (which can be important when specifying a
  # seasonal component). In the future, we should stick with the original time
  # indices and account for the fact that bsts's output may contain additional
  # time points.
  times <- time(data)
  time(data) <- seq_len(nrow(data))
  
  # Zoom in on data in modeling range, remove original time indices.
  pre.period[1] <- max(pre.period[1], which.max(!is.na(data[, 1])))
  data.modeling <- window(data, start = pre.period[1])
  times.modeling <- window(times, start = pre.period[1])
  if (is.null(ncol(data.modeling))) {
    dim(data.modeling) <- c(length(data.modeling), 1)
  }
  
  # Standardize all variables?
  UnStandardize <- identity
  if (model.args$standardize.data) {
    fit.range <- c(1, diff(pre.period) + 1)
    sd.results <- StandardizeAllVariables(data.modeling, fit.range)
    data.modeling <- sd.results$data
    UnStandardize <- sd.results$UnStandardize
  }
  
  # Set observed response after pre-period to NA.
  window(data.modeling[, 1], start = pre.period[2] + 1) <- NA
  
  # Construct model and perform inference
  bsts.model <- ConstructModel(data.modeling, model.args)
  
  # Compile posterior inferences
  if (!is.null(bsts.model)) {
    y.cf <- window(data[, 1], start = pre.period[2] + 1)
    # We need to adapt post-period indices for `CompilePosteriorInferences()` to
    # specify start and end of the post-period relative to pre.period[1], not
    # relative to the start of the time series; `CompilePosteriorInferences()`
    # only sees the data from the beginning of the pre-period.
    inferences <- CompilePosteriorInferences(bsts.model, y.cf,
                                             post.period - pre.period[1] + 1,
                                             alpha, UnStandardize)
  } else {
    inferences <- CompileNaInferences(data[, 1])
  }
  
  # Put original time indices back into `inferences$series`
  time(inferences$series) <- times.modeling
  
  # Extend <series> to cover original range (padding with NA as necessary)
  empty <- zoo(, times)
  inferences$series <- merge(inferences$series, empty, all = TRUE)
  assert_that(nrow(inferences$series) == nrow(data))
  
  # Replace <y.model> by full original response
  inferences$series[, 1] <- data[, 1]
  
  # Assign response-variable names
  names(inferences$series)[1] <- "response"
  names(inferences$series)[2] <- "cum.response"
  
  # Return 'CausalImpact' object
  model <- list(pre.period = times[pre.period],
                post.period = times[post.period],
                model.args = model.args,
                bsts.model = bsts.model,
                alpha = alpha)
  impact <- list(series = inferences$series,
                 summary = inferences$summary,
                 report = inferences$report,
                 model = model,
                 raw_mean = inferences$raw_mean,
                 raw_sample = inferences$raw_samples)
  class(impact) <- "CausalImpact"
  return(impact)
}

RunWithBstsModel <- function(bsts.model, post.period.response, alpha = 0.05) {
  # Runs an impact analysis on top of a fitted bsts model.
  #
  # Args:
  #   bsts.model:           fitted model, as returned by bsts(), in which the
  #                         data during the post-period was set to NA
  #   post.period.response: observed data during the post-intervention period
  #   alpha:                tail-probabilities of posterior intervals
  #
  # Returns:
  #   See CausalImpact().
  
  # Guess <pre.period> and <post.period> from the observation vector
  # These will be needed for plotting period boundaries in plot().
  y <- as.vector(bsts.model$original.series)
  indices <- TryStop(InferPeriodIndicesFromData(y),
                     paste0("bsts.model must have been fitted on data where ",
                            "the values in the post-intervention period have ",
                            "been set to NA"))
  if (is.integer(time(bsts.model$original.series))) {
    indices <- lapply(indices, as.integer)
  }
  
  # Compile posterior inferences
  inferences <- CompilePosteriorInferences(bsts.model = bsts.model,
                                           y.cf = post.period.response,
                                           post.period = indices$post.period,
                                           alpha = alpha)
  
  # Assign response-variable names
  # N.B. The modeling period comprises everything found in bsts, so the actual
  # observed data is equal to the data in the modeling period
  names(inferences$series)[1] <- "response"
  names(inferences$series)[2] <- "cum.response"
  
  # Return 'CausalImpact' object
  times <- time(bsts.model$original.series)
  model <- list(pre.period = times[indices$pre.period],
                post.period = times[indices$post.period],
                bsts.model = bsts.model,
                alpha = alpha)
  impact <- list(series = inferences$series,
                 summary = inferences$summary,
                 report = inferences$report,
                 model = model)
  class(impact) <- "CausalImpact"
  return(impact)
}

PrintSummary <- function(impact, digits = 2L) {
  # Prints a summary of the results. Both \code{print.CausalImpact()} and
  # \code{summary.CausalImpact()} point here.
  #
  # Args:
  #   impact: A \code{CausalImpact} results object, as returned by
  #           \code{CausalImpact()}.
  #
  #   digits: Number of significant digits to print for all numbers.
  
  # Check input
  assert_that(class(impact) == "CausalImpact")
  assert_that(is.numeric(digits), is.scalar(digits), as.integer(digits) > 0,
              msg = "<digits> must be a positive integer")
  summary <- impact$summary
  alpha <- impact$model$alpha
  assert_that(!is.null(alpha) && alpha > 0,
              msg = "invalid <alpha>; <impact> must be a CausalImpact object")
  
  # Print title
  cat("Posterior inference {CausalImpact}\n")
  if (is.null(summary)) {
    cat("(Inference aborted)\n")
    return()
  }
  
  # Define formatting helper functions
  FormatNumber <- function(x) format(x, digits = digits, trim = TRUE)
  FormatPercent <- function(x) {
    paste0(format(x * 100, digits = digits, trim = TRUE), "%")
  }
  FormatCI <- function(a, b) {
    paste0("[", format(a, digits = min(digits, 2), trim = TRUE),
           ", ", format(b, digits = min(digits, 2), trim = TRUE),
           "]")
  }
  FormatPercentCI <- function(a, b) {
    paste0("[", format(a * 100, digits = min(digits, 2), trim = TRUE),
           "%, ", format(b * 100, digits = min(digits, 2), trim = TRUE),
           "%]")
  }
  
  # Compile data frame with formatted numbers
  fsummary <- data.frame(
    Actual = FormatNumber(summary$Actual),
    Pred = paste0(FormatNumber(summary$Pred),
                  " (", FormatNumber(summary$Pred.sd), ")"),
    Pred.ci = FormatCI(summary$Pred.lower, summary$Pred.upper),
    Separator1 = c("", ""),
    AbsEffect = paste0(FormatNumber(summary$AbsEffect),
                       " (", FormatNumber(summary$AbsEffect.sd), ")"),
    AbsEffect.ci = FormatCI(summary$AbsEffect.lower, summary$AbsEffect.upper),
    Separator2 = c("", ""),
    RelEffect = paste0(FormatPercent(summary$RelEffect),
                       " (", FormatPercent(summary$RelEffect.sd), ")"),
    RelEffect.ci = FormatPercentCI(summary$RelEffect.lower,
                                   summary$RelEffect.upper))
  
  # Invert and format as table
  tsummary <- t(fsummary)
  colnames(tsummary) <- c("Average", "Cumulative")
  ci.label <- paste0(round((1 - alpha) * 100), "% CI")
  row.names(tsummary) <- c("Actual", "Prediction (s.d.)", ci.label,
                           " ",
                           "Absolute effect (s.d.)", paste(ci.label, ""),
                           "  ",
                           "Relative effect (s.d.)", paste(ci.label, " "))
  
  # Print formatted table
  cat("\n")
  print.default(tsummary, print.gap = 3L, quote = FALSE)
  cat("\n")
  
  # Print overall tail-area probability
  p <- summary$p[1]
  cat(paste0("Posterior tail-area probability p:   ", round(p, 5), "\n"))
  cat(paste0("Posterior prob. of a causal effect:  ",
             round((1 - p) * 100, ifelse(p < 0.01, 5, ifelse(p < 0.05, 3, 0))),
             "%\n"))
  cat("\n")
  cat(paste0("For more details, type: summary(impact, \"report\")\n"))
  cat("\n")
}

PrintReport <- function(impact, digits = 2L) {
  # Prints a detailed report of the individual steps carried out during the
  # analysis.
  #
  # Args:
  #   impact: A \code{CausalImpact} results object, as returned by
  #           \code{CausalImpact()}.
  #   digits: Number of digits to print for all numbers. Note that percentages
  #           are always rounded to whole numbers.
  
  assert_that(class(impact) == "CausalImpact")
  cat("Analysis report {CausalImpact}\n")
  if (is.null(impact$report)) {
    cat("(Report empty)")
  } else {
    cat(paste(InterpretSummaryTable(impact$summary, digits), collapse = " "),
        "\n")
  }
}

.summary.CausalImpact <- function(impact,
                                  output = c("summary", "report"),
                                  ...) {
  # Helper function for summary.CausalImpact(). The latter must adhere to the
  # S3 generic interface summary(x, ...).
  #
  # Args:
  #   impact: CausalImpact results object
  #   output: "summary" or "report"
  #   ...: additional arguments
  
  output <- tolower(match.arg(output))
  if (output == "summary") {
    PrintSummary(impact, ...)
  } else if (output == "report") {
    PrintReport(impact, ...)
  }
}

summary.CausalImpact <- function(object, ...) {
  # S3 method for printing a summary of analysis results.
  #
  # Args:
  #   object: A \code{CausalImpact} results object, as returned by
  #           \code{CausalImpact()}.
  #   ...:    Optional additional arguments, as described below. The first is
  #           \code{output}. You can specify the type of desired output using
  #           \code{summary(x, "summary")} (default) or \code{summary(x,
  #           "report")}. Partial matches are allowed. Furthermore,
  #           \code{digits} can be used to customize the precision of the
  #           output.
  #
  # Documentation:
  #   usage: summary(x, output = c("summary", "report"), ...)
  
  .summary.CausalImpact(object, ...)
}

print.CausalImpact <- function(x, ...) {
  # S3 method for printing a summary of analysis results.
  #
  # Args:
  #   x:   A \code{CausalImpact} results object, as returned by
  #        \code{CausalImpact()}.
  #   ...: Optional additional arguments, as described below. The first is
  #        \code{output}. You can specify the type of desired output using
  #        \code{summary(x, "summary")} (default) or \code{summary(x,
  #        "report")}. Partial matches are allowed. Furthermore, \code{digits}
  #        can be used to customize the precision of the output, e.g.:
  #        summary(impact, "summary", digits = 2).
  #
  # Documentation:
  #   usage: print(x, output = c("summary", "report"), ...)
  
  .summary.CausalImpact(x, ...)
}

as.CausalImpact <- function(x, ...) {
  # S3 method for allowing other packages to write a \code{as.CausalImpact.foo}
  # function that coerces an object of class \code{foo} into a
  # \code{CausalImpact} object.
  #
  # Args:
  #   x:   Any \code{R} object.
  #   ...: Additional arguments to be passed to the method.
  
  UseMethod("as.CausalImpact")
}

as.CausalImpact.default <- function(x, ...) {
  # Default method for \code{as.CausalImpact}.
  #
  # Args:
  #   x:   Any \code{R} object.
  #   ...: Additional arguments to be passed to the method.
  
  stop("No method available to coerce an object of class ", class(x)[1],
       " to CausalImpact")
}

# Copyright 2014-2021 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Bayesian dynamic diffusion-regression state-space model for computing
# counterfactual predictions in a time series. Uses an MCMC algorithm
# implemented in the \code{bsts} package to compute samples from the posterior
# (smoothing) densities over states and parameters.
#
# Author: kbrodersen@google.com (Kay Brodersen)

# Some model priors are fixed, others can be adjusted through model.args.
# For full flexibility, construct your own bsts model and feed it into
# CausalImpactForBsts().
kLocalLevelPriorSampleSize <- 32
kStaticRegressionExpectedModelSize <- 3
kStaticRegressionExpectedR2 <- 0.8
kStaticRegressionPriorDf <- 50
kDynamicRegressionPriorSampleSize <- 32

ObservationsAreIllConditioned <- function(y) {
  # Checks whether the response variable (i.e., the series of observations for
  # the dependent variable y) are ill-conditioned. For example, the series might
  # contain too few non-NA values. In such cases, inference will be aborted.
  #
  # Args:
  #   y: observed series (numeric vector or single zoo series)
  #
  # Returns:
  #   TRUE if something is wrong with the observations; FALSE otherwise.
  
  assert_that(!is.null(y), length(y) >= 1)
  ill.conditioned <- FALSE
  
  # All NA?
  if (all(is.na(y))) {
    warning("Aborting inference due to input series being all NA.")
    ill.conditioned <- TRUE
    
    # Fewer than 3 non-NA values?
  } else if (sum(!is.na(y)) < 3) {
    warning("Aborting inference due to fewer than 3 non-NA values in input")
    ill.conditioned <- TRUE
    
    # Constant series?
  } else if (sd(y, na.rm = TRUE) == 0) {
    warning(paste0("Aborting inference due to input series being constant: ",
                   y[!is.na(y)][1]))
    ill.conditioned <- TRUE
  }
  return(ill.conditioned)
}

FormatInputForConstructModel <- function(data, model.args) {
  # Checks the input arguments supplied to ConstructModel(). Missing arguments
  # in \code{model.args} will be filled using \code{.defaults} (see top of file
  # impact_analysis.R).
  #
  # Args:
  #   data: time series of response variable and covariates
  #   model.args: list of additional arguments
  #
  # Returns:
  #   list of checked and correctly formatted arguments
  
  # Check <data>
  assert_that(!is.null(data))
  data <- as.zoo(data)
  if (is.null(ncol(data))) {
    dim(data) <- c(length(data), 1)
  }
  assert_that(is.numeric(data))
  assert_that(nrow(data) > 0)
  
  # If <data> has no names, assign: y, x1, x2, ...
  if (is.null(names(data))) {
    if (ncol(data) == 1) {
      names(data)[1] <- "y"
    } else {
      names(data) <- c("y", paste0("x", 2:ncol(data) - 1))
    }
  }
  
  # Check covariates
  if (ncol(data) >= 2) {
    assert_that(all(!is.na(data[, -1])), msg = "covariates must not be NA")
  }
  
  # (Re-)parse <model.args>, fill gaps using <.defaults>
  # (defined in impact_analysis.R)
  model.args <- ParseArguments(model.args, .defaults)
  
  # Check those parts of <model.args> that are used in this file
  # Check <niter>
  assert_that(is.scalar(model.args$niter))
  assert_that(is.numeric(model.args$niter))
  assert_that(!is.na(model.args$niter))
  assert_that(is.wholenumber(model.args$niter))
  model.args$niter <- round(model.args$niter)
  assert_that(model.args$niter >= 10,
              msg = paste0("must draw, at the very least, 10 MCMC samples; ",
                           "recommending 1000"))
  if (model.args$niter < 1000) {
    warning("Results potentially inaccurate. Consider using more MCMC samples.")
  }
  
  # Check <prior.level.sd>
  assert_that(is.scalar(model.args$prior.level.sd))
  assert_that(is.numeric(model.args$prior.level.sd))
  assert_that(!is.na(model.args$prior.level.sd))
  assert_that(model.args$prior.level.sd > 0)
  
  # Check <nseasons>
  assert_that(is.scalar(model.args$nseasons))
  assert_that(is.numeric(model.args$nseasons))
  assert_that(!is.na(model.args$nseasons))
  assert_that(is.wholenumber(model.args$nseasons))
  assert_that(model.args$nseasons >= 1,
              msg = paste0("nseasons cannot be 0; use 1 in order not to have ",
                           "seaonsal components"))
  
  # Check <season.duration>
  assert_that(is.scalar(model.args$season.duration))
  assert_that(is.numeric(model.args$season.duration))
  assert_that(!is.na(model.args$season.duration))
  assert_that(is.wholenumber(model.args$season.duration))
  assert_that(model.args$season.duration >= 1)
  
  # Check <dynamic.regression>
  assert_that(is.scalar(model.args$dynamic.regression))
  assert_that(is.logical(model.args$dynamic.regression))
  assert_that(!is.na(model.args$dynamic.regression))
  
  # Check <max.flips>
  assert_that(is.scalar(model.args$max.flips))
  assert_that(is.numeric(model.args$max.flips))
  assert_that(!is.na(model.args$max.flips))
  assert_that(is.wholenumber(model.args$max.flips))
  assert_that(model.args$max.flips > 0 || model.args$max.flips == -1)
  
  # Return updated args
  return(list(data = data, model.args = model.args))
}

# Tell 'R CMD check' to treat `BstsOptions()` as global variable to avoid
# false positives as long as 'bsts' version 0.7.x is not published.
# TODO(alhauser): remove this when 'bsts' version 0.7.x is published.
if(getRversion() >= "2.15.1") {
  utils::globalVariables("BstsOptions")
}

ConstructModel <- function(data, model.args = NULL) {
  # Specifies the model and performs inference. Inference means using the data
  # to pass from a prior distribution over parameters and states to a posterior
  # distribution. In a Bayesian framework, estimating a model means to obtain
  # p(parameters | data) from p(data | parameters) and p(parameters). This
  # involves multiplying the prior with the likelihood and normalising the
  # resulting distribution using the marginal likelihood or model evidence,
  # p(data). Computing the evidence poses a virtually intractable
  # high-dimensional integration problem which can be turned into an easier
  # optimization problem using, for instance, an approximate stochastic
  # inference strategy. Here, we use a Markov chain Monte Carlo algorithm, as
  # implemented in the \code{bsts} package.
  #
  # Args:
  #   data: time series of response variable and optional covariates
  #   model.args: optional list of additional model arguments
  #
  # Returns:
  #   \code{bsts.model}, as returned by \code{bsts()}
  
  # Check and format input
  checked <- FormatInputForConstructModel(data, model.args)
  data <- checked$data
  model.args <- checked$model.args
  y <- data[, 1]
  
  # If the series is ill-conditioned, abort inference and return NULL
  if (ObservationsAreIllConditioned(y)) {
    return(NULL)
  }
  
  # Local level
  # sigma.guess: standard deviation of the random walk of the level
  sdy <- sd(y, na.rm = TRUE)
  ss <- list()
  sd.prior <- SdPrior(sigma.guess = model.args$prior.level.sd * sdy,
                      upper.limit = sdy,
                      sample.size = kLocalLevelPriorSampleSize)
  ss <- AddLocalLevel(ss, y, sigma.prior = sd.prior)
  
  # Add seasonal component?
  if (model.args$nseasons > 1) {
    ss <- AddSeasonal(ss, y,
                      nseasons = model.args$nseasons,
                      season.duration = model.args$season.duration)
  }
  
  # No regression?
  if (ncol(data) == 1) {
    bsts.model <- bsts(y, state.specification = ss, niter = model.args$niter,
                       seed = 1, ping = 0,
                       model.options =
                         BstsOptions(save.prediction.errors = TRUE),
                       max.flips = model.args$max.flips)
  } else {
    formula <- paste0(names(data)[1], " ~ .")
    
    # Static regression?
    if (!model.args$dynamic.regression) {
      bsts.model <- bsts(formula, data = data, state.specification = ss,
                         expected.model.size =
                           kStaticRegressionExpectedModelSize,
                         expected.r2 = kStaticRegressionExpectedR2,
                         prior.df = kStaticRegressionPriorDf,
                         niter = model.args$niter, seed = 1, ping = 0,
                         model.options =
                           BstsOptions(save.prediction.errors = TRUE),
                         max.flips = model.args$max.flips)
      time(bsts.model$original.series) <- time(data)
      
      # Dynamic regression?
    } else {
      # Since we have predictor variables in the model, we need to explicitly
      # make their coefficients time-varying using AddDynamicRegression(). In
      # bsts(), we are therefore not giving a formula but just the response
      # variable. We are then using SdPrior to only specify the prior on the
      # residual standard deviation.
      # prior.mean: precision of random walk of coefficients
      sdx <- apply(data[, -1, drop = FALSE], 2, function(x) sd(x, na.rm = TRUE))
      model.options <- DynamicRegressionRandomWalkOptions(sdx = sdx, sdy = sdy)
      ss <- AddDynamicRegression(ss, formula, data = data,
                                 model.options = model.options)
      sd.prior <- SdPrior(sigma.guess = model.args$prior.level.sd * sdy,
                          upper.limit = 0.1 * sdy,
                          sample.size = kDynamicRegressionPriorSampleSize)
      bsts.model <- bsts(y, state.specification = ss, niter = model.args$niter,
                         expected.model.size = 3, ping = 0, seed = 1,
                         prior = sd.prior, max.flips = model.args$max.flips)
    }
  }
  return(bsts.model)
}

# Copyright 2014 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Posterior inference for bsts models.
#
# Author: kbrodersen@google.com (Kay Brodersen)

GetPosteriorStateSamples <- function(bsts.model) {
  # Returns a matrix of simulated values from the marginal posterior
  # distribution for the sum of all state variables.
  #
  # Args:
  #   bsts.model: A fitted model returned by \code{bsts()}.
  #
  # Returns:
  #   matrix [number of post-burn-in MCMC samples] x [time points]
  
  # Get state contributions (e.g., 1000 samples x 2 states x 365 time pts),
  # discarding burn-in samples (=> 900 x 2 x 365)
  burn <- SuggestBurn(0.5, bsts.model)
  assert_that(burn > 0)
  state.contributions <- bsts.model$state.contributions[-seq_len(burn), , ,
                                                        drop = FALSE]
  
  # Sum across states, call it 'state.samples' (=> 900 x 365)
  state.samples <- rowSums(aperm(state.contributions, c(1, 3, 2)), dims = 2)
  return(state.samples)
}

ComputeResponseTrajectories <- function(bsts.model) {
  # Generates trajectories of the response variable. A trajectory is a simulated
  # time series drawn from the posterior predictive distribution over the data.
  # This function differs from GetPosteriorStateSamples(). The latter returns
  # the posterior mean of the response. This function returns the actual value
  # (posterior mean + observation noise).
  #
  # Args:
  #   bsts.model: A model object as returned by \code{bsts()}.
  #
  # Returns:
  #   matrix [number of post-burn-in MCMC samples] x [time points]
  
  # Get posterior state samples
  state.samples <- GetPosteriorStateSamples(bsts.model)
  
  # Get observation noise standard deviation samples
  burn <- SuggestBurn(0.5, bsts.model)
  assert_that(burn > 0)
  sigma.obs <- bsts.model$sigma.obs[-seq_len(burn)]  # e.g., 900
  
  # Sample from the posterior predictive density over data
  n.samples <- dim(state.samples)[1]  # e.g., 900 x 365
  obs.noise.samples <- matrix(rnorm(prod(dim(state.samples)), 0, sigma.obs),
                              nrow = n.samples)
  y.samples <- state.samples + obs.noise.samples
  return(y.samples)
}

ComputePointPredictions <- function(y.samples, state.samples, alpha = 0.05) {
  # Summarises a matrix of response trajectory samples (\code{y.samples}) in
  # terms of the mean and an interval of the posterior predictive density over
  # the data.
  #
  # Args:
  #   y.samples:     Matrix of simulated response samples.
  #   state.samples: Matrix of posterior state samples (needed for the mean).
  #   alpha:         The resulting coverage of the posterior intervals will be
  #                  \code{1 - alpha}.
  #
  # Returns:
  #   data frame with 3 columns:
  #     point.pred.mean:  posterior predictive expectation
  #     point.pred.lower: lower limit of a \code{(1 - alpha)*100}% interval
  #     point.pred.upper: upper limit
  
  # Expectation of data = expectation of state (because noise is centered)
  assert_that(identical(dim(y.samples), dim(state.samples)),
              msg = "inconsistent y.samples, state.samples")
  point.pred.mean <- colMeans(state.samples) # e.g., 365
  
  # Quantiles of the data = Quantiles of (state + observation noise)
  assert_that(is.scalar(alpha), alpha > 0, alpha < 1)
  prob.lower <- alpha / 2      # e.g., 0.025 when alpha = 0.05
  prob.upper <- 1 - alpha / 2  # e.g., 0.975 when alpha = 0.05
  point.pred.lower <- as.numeric(t(apply(y.samples, 2, quantile, prob.lower)))
  point.pred.upper <- as.numeric(t(apply(y.samples, 2, quantile, prob.upper)))
  point.pred <- data.frame(point.pred = point.pred.mean,
                           point.pred.lower, point.pred.upper)
  return(point.pred)
}

ComputeCumulativePredictions <- function(y.samples, point.pred, y,
                                         post.period.begin, alpha = 0.05) {
  # Computes summary statistics for the cumulative posterior predictions over
  # the unobserved data points in the post-intervention period.
  #
  # Args:
  #   y.samples:         Matrix of simulated response trajectories, as returned
  #                      by \code{ComputeResponseTrajectories()}.
  #   point.pred:        Data frame of point predictions, as returned by
  #                      \code{ComputePointPredictions()}.
  #   y:                 Actual observed response, from the beginning of the
  #                      pre-period to the end of the observed period.
  #   post.period.begin: Index of the first data point of the post-period.
  #   alpha:             The resulting coverage of the posterior intervals will
  #                      be \code{1 - alpha}.
  #
  # Returns:
  #   data frame with 3 columns:
  #     cum.pred:       posterior predictive expectation
  #     cum.pred.lower: lower limit of a \code{(1 - alpha)*100}% interval
  #     cum.pred.upper: upper limit
  
  # After pre-inference standardization of the response variable has been
  # undone, we can form cumulative time series of counterfactual predictions.
  # Note that we only explicitly compute these for the post-period. The
  # cumulative prediction for the pre-period and the gap between pre- and post-
  # period (if any) is forced to equal the (cumulative) observed response in the
  # pre-period. Thus, the posterior intervals of the cumulative predictions do
  # not inherit variance from the pre-period, which would be misleading when
  # subtracting cumulative predictions from the cumulative observed response to
  # obtain the cumulative impact, which is the main use case. Thus, the
  # cumulative impact will be zero by construction before the beginning of the
  # post-period.
  
  # Compute posterior mean
  is.post.period <- seq_along(y) >= post.period.begin
  cum.pred.mean.pre <- cumsum.na.rm(as.vector(y)[!is.post.period])
  non.na.indices <- which(!is.na(cum.pred.mean.pre))
  assert_that(length(non.na.indices) > 0)
  last.non.na.index <- max(non.na.indices)
  cum.pred.mean.post <- cumsum(point.pred$point.pred[is.post.period]) +
    cum.pred.mean.pre[last.non.na.index]
  cum.pred.mean <- c(cum.pred.mean.pre, cum.pred.mean.post)
  
  # Check for overflow
  assert_that(identical(which(is.na(cum.pred.mean)),
                        which(is.na(y[!is.post.period]))),
              msg = "unexpected NA found in cum.pred.mean")
  
  # Compute posterior interval
  cum.pred.lower.pre <- cum.pred.mean.pre
  cum.pred.upper.pre <- cum.pred.mean.pre
  y.samples.cum.post <- t(apply(y.samples[, is.post.period, drop = FALSE], 1,
                                cumsum)) +
    cum.pred.mean.pre[last.non.na.index]
  if (sum(is.post.period) == 1) {
    y.samples.cum.post <- t(y.samples.cum.post)
  }
  assert_that(is.scalar(alpha), alpha > 0, alpha < 1)
  prob.lower <- alpha / 2      # e.g., 0.025 when alpha = 0.05
  prob.upper <- 1 - alpha / 2  # e.g., 0.975 when alpha = 0.05
  cum.pred.lower.post <- as.numeric(t(apply(y.samples.cum.post, 2, quantile,
                                            prob.lower)))
  cum.pred.upper.post <- as.numeric(t(apply(y.samples.cum.post, 2, quantile,
                                            prob.upper)))
  cum.pred.lower <- c(cum.pred.lower.pre, cum.pred.lower.post)
  cum.pred.upper <- c(cum.pred.upper.pre, cum.pred.upper.post)
  
  # Put cumulative prediction together
  cum.pred <- data.frame(cum.pred = cum.pred.mean,
                         cum.pred.lower, cum.pred.upper)
  return(cum.pred)
}

# Tell R CMD check to treat columns of data frames used in `dplyr::mutate` as
# global variables; this avoids false positives of "no visible binding for
# global variable ..." during the check.
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("AbsEffect", "AbsEffect.lower", "AbsEffect.upper",
                           "AbsEffect.sd", "Pred"))
}

CompileSummaryTable <- function(y.post, y.samples.post,
                                point.pred.mean.post, alpha = 0.05) {
  # Creates a table of statistics that summarise the post-intervention period.
  # This will later be accessible through \code{impact$model$summary}.
  #
  # Args:
  #   y.post:               Actual observed response during the post-period.
  #   y.samples.post:       Matrix of sampled response trajectories for the
  #                         post-period.
  #   point.pred.mean.post: Posterior predictive mean for the post-period. Note
  #                         that colMeans(y.samples.post) = point.pred.mean.post
  #                         in expectation (i.e., in the limit of an infinite
  #                         number of MCMC iterations); but for any given finite
  #                         simulation, y.samples.post contains sampled
  #                         observation noise. Therefore, to obtain a summary of
  #                         the posterior mean series, we consider the mean of
  #                         the posterior predictive level, without additional
  #                         simulated (centered) observation noise.
  #   alpha:                The resulting coverage of the posterior intervals
  #                         will be \code{1 - alpha}.
  #
  # Returns:
  #   data frame of post-period summary statistics
  
  # Check input
  assert_that(ncol(y.samples.post) == length(y.post),
              msg = "inconsistent y.post")
  assert_that(length(point.pred.mean.post) == length(y.post),
              msg = "inconsistent y.post")
  
  # We will compare the matrix of predicted trajectories (e.g., 900 x 201)
  # with a matrix of replicated observations (e.g., 900 x 201)
  y.repmat.post <- matrix(y.post, nrow = nrow(y.samples.post),
                          ncol = length(y.post), byrow = TRUE)
  assert_that(all(dim(y.repmat.post) == dim(y.samples.post)))
  
  # Define quantiles
  assert_that(is.scalar(alpha), alpha > 0, alpha < 1)
  prob.lower <- alpha / 2      # e.g., 0.025 when alpha = 0.05
  prob.upper <- 1 - alpha / 2  # e.g., 0.975 when alpha = 0.05
  
  # Compile summary statistics
  summary <- data.frame(
    Actual = c(mean(y.post), sum(y.post)),
    Pred = c(mean(point.pred.mean.post), sum(point.pred.mean.post)),
    Pred.lower = c(quantile(rowMeans(y.samples.post), prob.lower),
                   quantile(rowSums(y.samples.post), prob.lower)),
    Pred.upper = c(quantile(rowMeans(y.samples.post), prob.upper),
                   quantile(rowSums(y.samples.post), prob.upper)),
    Pred.sd = c(sd(rowMeans(y.samples.post)),
                sd(rowSums(y.samples.post))),
    AbsEffect = c(mean(y.post) - mean(point.pred.mean.post),
                  sum(y.post) - sum(point.pred.mean.post)),
    AbsEffect.lower = c(quantile(rowMeans(y.repmat.post - y.samples.post),
                                 prob.lower),
                        quantile(rowSums(y.repmat.post - y.samples.post),
                                 prob.lower)),
    AbsEffect.upper = c(quantile(rowMeans(y.repmat.post - y.samples.post),
                                 prob.upper),
                        quantile(rowSums(y.repmat.post - y.samples.post),
                                 prob.upper)),
    AbsEffect.sd = c(sd(rowMeans(y.repmat.post - y.samples.post)),
                     sd(rowSums(y.repmat.post - y.samples.post))))
  summary <- dplyr::mutate(summary,
                           RelEffect = AbsEffect / Pred,
                           RelEffect.lower = AbsEffect.lower / Pred,
                           RelEffect.upper = AbsEffect.upper / Pred,
                           RelEffect.sd = AbsEffect.sd / Pred)
  rownames(summary) <- c("Average", "Cumulative")
  
  # Add interval coverage, defined by alpha
  summary$alpha <- alpha
  
  # Add one-sided tail-area probability of overall impact, p
  y.samples.post.sum <- rowSums(y.samples.post)
  y.post.sum <- sum(y.post)
  p <- min(sum(c(y.samples.post.sum, y.post.sum) >= y.post.sum),
           sum(c(y.samples.post.sum, y.post.sum) <= y.post.sum)) /
    (length(y.samples.post.sum) + 1)
  assert_that(p > 0, p < 1)
  summary$p <- p
  return(summary)
}

InterpretSummaryTable <- function(summary, digits = 2L) {
  # Composes a written interpretation of a given summary table.
  #
  # Args:
  #   summary: Data frame with summary statistics, as created within
  #            FitImpactModel().
  #   digits:  Number of digits to print for all numbers. Note that percentages
  #            are always rounded to whole numbers.
  #
  # Returns:
  #   A string summarizing the summary verbally as one would do in the Results
  #   section of a paper.
  
  # Prepare formatted numbers
  actual <- PrettifyNumber(summary$Actual, round.digits = digits)
  letter <- IdentifyNumberAbbreviation(actual)
  pred <- PrettifyNumber(summary$Pred, letter, 2)
  pred.lower <- PrettifyNumber(summary$Pred.lower, letter, digits)
  pred.upper <- PrettifyNumber(summary$Pred.upper, letter, digits)
  abs.effect <- PrettifyNumber(summary$AbsEffect, letter, digits)
  abs.effect.lower <- PrettifyNumber(summary$AbsEffect.lower, letter, digits)
  abs.effect.upper <- PrettifyNumber(summary$AbsEffect.upper, letter, digits)
  rel.effect <- PrettifyPercentage(summary$RelEffect)
  rel.effect.lower <- PrettifyPercentage(summary$RelEffect.lower)
  rel.effect.upper <- PrettifyPercentage(summary$RelEffect.upper)
  
  # Evaluate significance and direction of the effect (increase or decrease)
  sig <- (! ((summary$RelEffect.lower[1] < 0) &&
               (summary$RelEffect.upper[1] > 0)))
  pos <- summary$RelEffect[1] > 0
  p <- summary$p[1]
  
  # Interval name
  ci.coverage <- paste0(round((1 - summary$alpha[1]) * 100), "%")
  
  # Initialize statement
  stmt <- NULL
  
  # Summarize averages
  stmt <- paste0(stmt, "\n\nDuring the post-intervention period, the response ",
                 "variable had an average value of approx. ", actual[1],
                 ". ", if (sig) "By contrast, in " else "In ",
                 "the absence of an intervention, ",
                 "we would have expected an average response of ",
                 pred[1], ". The ", ci.coverage, " interval of this ",
                 "counterfactual prediction is [", pred.lower[1],
                 ", ", pred.upper[1], "]. Subtracting this ",
                 "prediction from the observed response yields an estimate ",
                 "of the causal effect the intervention had on the response ",
                 "variable. This effect is ", abs.effect[1], " with a ",
                 ci.coverage, " interval of [", abs.effect.lower[1], ", ",
                 abs.effect.upper[1], "]. For a discussion of ",
                 "the significance of this effect, see below.")
  
  # Summarize sums
  stmt <- paste0(stmt, "\n\nSumming up the individual data points during ",
                 "the post-intervention period (which can only sometimes be ",
                 "meaningfully interpreted), the response variable had an ",
                 "overall value of ", actual[2], ". ",
                 if (sig) "By contrast, had " else "Had ",
                 "the intervention not taken place, we would have expected ",
                 "a sum of ", pred[2], ". The ", ci.coverage, " interval of ",
                 "this prediction is [", pred.lower[2], ", ", pred.upper[2],
                 "].")
  
  # Summarize relative numbers (in which case row [1] = row [2])
  stmt <- paste0(stmt, "\n\nThe above results are given in terms of ",
                 "absolute numbers. In relative terms, the response variable ",
                 "showed ", if (pos) "an increase of " else "a decrease of ",
                 rel.effect[1], ". The ", ci.coverage, " interval of this ",
                 "percentage is [", rel.effect.lower[1], ", ",
                 rel.effect.upper[1], "].")
  
  # Comment on significance
  if (sig && pos) {
    stmt <- paste0(stmt, "\n\nThis means that the positive effect observed ",
                   "during the intervention period is statistically ",
                   "significant and unlikely to be due to random ",
                   "fluctuations. ",
                   "It should be noted, however, that the question of whether ",
                   "this increase also bears substantive significance can ",
                   "only be answered by comparing the absolute effect (",
                   abs.effect[1], ") to the original goal of ",
                   "the underlying intervention.")
  } else if (sig && !pos) {
    stmt <- paste0(stmt, "\n\nThis means that the negative effect observed ",
                   "during the intervention period is statistically ",
                   "significant. If the experimenter had expected a positive ",
                   "effect, it is recommended to ",
                   "double-check whether anomalies in the control variables ",
                   "may have caused an overly optimistic expectation of ",
                   "what should have happened in the response variable in the ",
                   "absence of the intervention.")
  } else if (!sig && pos) {
    stmt <- paste0(stmt, "\n\nThis means that, although the intervention ",
                   "appears to have caused a positive effect, this effect ",
                   "is not statistically significant when considering the ",
                   "entire post-intervention period as a whole. Individual ",
                   "days or shorter stretches within the intervention period ",
                   "may of course still have had a significant effect, as ",
                   "indicated whenever the lower limit of the impact ",
                   "time series (lower plot) was above zero.")
  } else if (!sig && !pos) {
    stmt <- paste0(stmt, "\n\nThis means that, although it may look as ",
                   "though the intervention has exerted a negative effect ",
                   "on the response variable when considering the ",
                   "intervention period as a whole, this effect is not ",
                   "statistically significant, and so cannot be ",
                   "meaningfully interpreted.")
  }
  if (!sig) {
    stmt <- paste0(stmt, " The apparent effect could be the result of ",
                   "random fluctuations that are unrelated to the ",
                   "intervention. This is often the case when the ",
                   "intervention period is very long and includes much ",
                   "of the time when the effect has already worn off. ",
                   "It can also be the case when the intervention period ",
                   "is too short to distinguish the signal from the noise. ",
                   "Finally, failing to find a significant effect can ",
                   "happen when there are not enough control variables or ",
                   "when these variables do not correlate well with ",
                   "the response variable during the learning period.")
  }
  if (p < summary$alpha[1]) {
    stmt <- paste0(stmt, "\n\nThe probability of obtaining this effect by ",
                   "chance is very small (Bayesian one-sided tail-area ",
                   "probability p = ", round(p, 3), "). This means the causal ",
                   "effect can be considered statistically significant.")
  } else {
    stmt <- paste0(stmt, "\n\nThe probability of obtaining this ",
                   "effect by chance is p = ", round(p, 3), ". This ",
                   "means the effect may be spurious and would generally ",
                   "not be considered statistically significant.")
  }
  return(stmt)
}

AssertCumulativePredictionsAreConsistent <- function(cum.pred, post.period,
                                                     summary) {
  # Asserts that <cum.pred> is consistent with <summary>.
  #
  # Args:
  #   cum.pred:          Data frame of: cum.pred, cum.pred.lower, cum.upper.
  #   post.period:       A vector of two indices specifying the first and the
  #                      last time point of the post-intervention period.
  #   summary:           Summary table, as created by
  #                      \code{CompileSummaryTable()}.
  
  # Auxiliary function checking if one column of `cum.pred` is consistent with
  # the corresponding number in `summary`.
  AssertCumulativePredictionIsConsistent <- function(cum.pred.col,
                                                     summary.entry,
                                                     description) {
    non.na.indices <- which(!is.na(cum.pred.col[seq_len(post.period[1] - 1)]))
    assert_that(length(non.na.indices) > 0)
    last.non.na.index <- max(non.na.indices)
    assert_that(
      is.numerically.equal(cum.pred.col[post.period[2]] -
                             cum.pred.col[last.non.na.index],
                           summary.entry[2]),
      msg = paste0("The calculated ", description, " of the cumulative ",
                   "effect is inconsistent with the previously calculated ",
                   "one. You might try to run CausalImpact on a shorter ",
                   "time series to avoid this problem."))
  }
  
  AssertCumulativePredictionIsConsistent(cum.pred$cum.pred, summary$Pred,
                                         "mean")
  AssertCumulativePredictionIsConsistent(cum.pred$cum.pred.lower,
                                         summary$Pred.lower, "lower bound")
  AssertCumulativePredictionIsConsistent(cum.pred$cum.pred.upper,
                                         summary$Pred.upper, "upper bound")
}

CheckInputForCompilePosteriorInferences <- function(bsts.model, y.cf,
                                                    post.period, alpha,
                                                    UnStandardize) {
  # Checks the input arguments for CompilePosteriorInferences().
  #
  # Args:
  #   bsts.model:    Model object created by bsts().
  #   y.cf:          Actual observed data in the counterfactual period, i.e.
  #                  after pre-intervention period (vector or zoo object).
  #   post.period:   A vector of two indices specifying the first and the last
  #                  time point of the post-intervention period.
  #   alpha:         Level for credible intervals.
  #   UnStandardize: Function for undoing any data standardization.
  #
  # Returns:
  #   list of checked arguments
  
  # Check <bsts.model>
  assert_that(!is.null(bsts.model))
  assert_that(class(bsts.model) == "bsts")
  assert_that(length(bsts.model$original.series) >= 2)
  
  # Check <post.period>
  assert_that(is.vector(post.period))
  assert_that(is.numeric(post.period))
  assert_that(length(post.period) == 2)
  assert_that(!anyNA(post.period))
  # Check that <post.period> lies within the range covered by <y.cf>
  cf.period.start <- length(bsts.model$original.series) - length(y.cf) + 1
  assert_that(cf.period.start <= post.period[1])
  assert_that(post.period[1] <= post.period[2])
  assert_that(post.period[2] <= length(bsts.model$original.series))
  
  # Check <y.cf>
  assert_that(is.zoo(y.cf) || is.vector(y.cf))
  y.cf <- as.vector(y.cf)
  assert_that(is.numeric(y.cf))
  assert_that(length(y.cf) >= 1)
  assert_that(!anyNA(y.cf[(post.period[1] : post.period[2]) -
                            cf.period.start + 1]),
              msg = "NA values in the post-period not currently supported")
  assert_that(all(is.na(tail(bsts.model$original.series, length(y.cf)))),
              msg = paste0("bsts.model$original.series must end on a stretch ",
                           "of NA at least as long as y.cf"))
  
  # Check <alpha>
  assert_that(is.numeric(alpha))
  assert_that(is.scalar(alpha))
  assert_that(!is.na(alpha))
  assert_that(alpha > 0, alpha < 1)
  
  # Check <UnStandardize>
  assert_that(is.function(UnStandardize))
  assert_that(is.scalar(UnStandardize(1)))
  assert_that(is.numeric(UnStandardize(1)))
  assert_that(length(UnStandardize(c(1, 2))) == 2)
  
  # Return arguments
  return(list(bsts.model = bsts.model,
              y.cf = y.cf,
              post.period = post.period,
              alpha = alpha,
              UnStandardize = UnStandardize))
}

CompilePosteriorInferences <- function(bsts.model, y.cf, post.period,
                                       alpha = 0.05, UnStandardize = identity) {
  # Takes in a fitted \code{bsts} model and computes the posterior predictive
  # distributions, over time, for the counterfactual response and the causal
  # effect.
  #
  # Args:
  #   bsts.model:    A model object created by \code{bsts()}.
  #   y.cf:          Actual observed data in the counterfactual period, i.e.
  #                  after pre-intervention period (vector or zoo object).
  #   post.period:   A vector of two indices specifying the first and the last
  #                  time point of the post-intervention period.
  #   alpha:         The resulting coverage of the posterior intervals will be
  #                  \code{1 - alpha}.
  #   UnStandardize: If \code{bsts()} was run on standardized data, this is the
  #                  function to undo that standardization. This is critical for
  #                  obtaining correct cumulative predictions.
  #
  # Returns:
  #   series:  zoo time-series object of: point.pred, point.pred.lower, ...
  #   summary: table of summary statistics
  #   report:  verbal description of the summary statistics
  
  # Check input
  checked <- CheckInputForCompilePosteriorInferences(bsts.model, y.cf,
                                                     post.period, alpha,
                                                     UnStandardize)
  bsts.model <- checked$bsts.model
  y.cf <- checked$y.cf
  post.period <- checked$post.period
  alpha <- checked$alpha
  UnStandardize <- checked$UnStandardize
  
  # Compute point predictions of counterfactual (in standardized space)
  y.samples <- ComputeResponseTrajectories(bsts.model)
  state.samples <- GetPosteriorStateSamples(bsts.model)
  point.pred <- ComputePointPredictions(y.samples, state.samples, alpha)
  
  # Undo standardization (if any)
  y.samples <- UnStandardize(y.samples)
  state.samples2 <- UnStandardize(state.samples)
  point.pred <- UnStandardize(point.pred)
  y.model <- UnStandardize(bsts.model$original.series)
  
  # Reconstruct full original series
  indices <- seq_along(y.model)
  is.cf.period <- (indices >= length(y.model) - length(y.cf) + 1)
  y.model[is.cf.period] <- y.cf
  
  # Compile summary statistics (in original space). Summary statistics consider
  # quantities in the post-period only, not in the whole counterfactual period.
  is.post.period <- (indices >= post.period[1]) & (indices <= post.period[2])
  y.samples.post <- y.samples[, is.post.period, drop = FALSE]
  point.pred.mean.post <- point.pred$point.pred[is.post.period]
  y.post <- y.cf[tail(is.post.period, length(y.cf))]
  summary <- CompileSummaryTable(y.post, y.samples.post, point.pred.mean.post,
                                 alpha)
  report <- InterpretSummaryTable(summary)
  
  # Compute cumulative predictions (in original space)
  cum.pred <- ComputeCumulativePredictions(y.samples, point.pred, y.model,
                                           post.period[1], alpha)
  
  # Check that <cum.pred> is consistent with <summary>
  AssertCumulativePredictionsAreConsistent(cum.pred, post.period, summary)
  
  # Create results series
  cum.y.model <- cumsum.na.rm(y.model)
  series <- zoo(data.frame(y.model, cum.y.model, point.pred, cum.pred),
                time(y.model))
  series$point.effect <- series$y.model - series$point.pred
  series$point.effect.lower <- series$y.model - series$point.pred.upper
  series$point.effect.upper <- series$y.model - series$point.pred.lower
  series$cum.effect <- series$cum.y.model - series$cum.pred
  series$cum.effect.lower <- series$cum.y.model - series$cum.pred.upper
  series$cum.effect.upper <- series$cum.y.model - series$cum.pred.lower
  assert_that(nrow(series) == length(bsts.model$original.series))
  
  # Set effects and cumulative effects to NA at time points not belonging to
  # pre- or post-period.
  # Note that since the time series is cut to the beginning of the pre-period
  # before being given to CompilePosteriorInferences, pre-period and
  # non-counterfactual period are identical here.
  effect.cols <- grep("(point|cum)\\.effect", names(series))
  series[is.cf.period & !is.post.period, effect.cols] <- NA
  
  # Return <series> and <summary>
  return(list(series = series,
              summary = summary,
              report = report,
              raw_mean = state.samples2,
              raw_samples = y.samples))
}

CompileNaInferences <- function(y.model) {
  # Creates a data frame of inferences that are all NA. We do this when the
  # response data were ill-conditioned (e.g., all constant).
  #
  # Args:
  #   y.model: actual observed response in the modeling period (zoo object)
  
  # Check input
  assert_that(is.zoo(y.model))
  assert_that(length(y.model) >= 1)
  
  # Create NA inferences
  vars <- c("point.pred", "point.pred.lower", "point.pred.upper",
            "cum.pred", "cum.pred.lower", "cum.pred.upper",
            "point.effect", "point.effect.lower", "point.effect.upper",
            "cum.effect", "cum.effect.lower", "cum.effect.upper")
  na.series <- matrix(NA_real_, nrow = length(y.model), ncol = 12)
  na.series <- zoo(na.series, time(y.model))
  names(na.series) <- vars
  
  # Insert observed data, as we do in CompilePosteriorInferences()
  cum.y.model <- cumsum(y.model)
  series <- zoo(cbind(y.model = y.model, cum.y.model = cum.y.model, na.series),
                time(y.model))
  
  # Return NA <series> and NULL <summary>
  return(list(series = series,
              summary = NULL,
              report = NULL))
}

# Copyright 2014-2021 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Utility functions used throughout the package.
#
# Authors: kbrodersen@google.com (Kay Brodersen)
#          gallusser@google.com (Fabian Gallusser)
#          alhauser@google.com (Alain Hauser)

is.wholenumber <- function(x, tolerance = .Machine$double.eps ^ 0.5) {
  # Checks whether a number is a whole number. This is not the same as
  # \code{is.integer()}, which tests the data type.
  #
  # Args:
  #   x:         input scalar or vector
  #   tolerance: tolerance
  #
  # Returns:
  #   boolean
  #
  # Examples:
  #   CausalImpact:::is.wholenumber(c(1, 1.0, 1.2))
  #   # [1]  TRUE  TRUE FALSE
  
  return(abs(x - round(x)) < tolerance)
}

cumsum.na.rm <- function(x) {
  # Cumulative sum, ignoring NA.
  #
  # Args:
  #   x: numeric vector
  #
  # Returns:
  #   cumulative sum of x, but NA values are ignored
  #
  # Examples:
  #   CausalImpact:::cumsum.na.rm(c(1, NA, 2))
  #   # [1]  1 NA 3
  #
  #   # Compare this to the conventional cumsum():
  #   cumsum(c(1, NA, 2))
  #   # [1]  1 NA NA
  
  if (is.null(x)) {
    return(x)
  }
  nas <- is.na(x)
  s <- cumsum(ifelse(nas, 0, x))
  s[nas] <- NA
  return(s)
}

is.numerically.equal <- function(x, y, tolerance = .Machine$double.eps ^ 0.5) {
  # Tests whether two numbers are 'numerically equal' by checking whether their
  # relative difference is smaller than a given tolerance. 'Relative difference'
  # is defined as the absolute difference of the values divided by the maximum
  # of their absolute values.
  #
  # The difference between this function and `all.equal` is that the latter
  # checks the absolute difference rather than the relative difference.
  #
  # Args:
  #   x:         one of the values to be compared.
  #   y:         one of the values to be compared.
  #   tolerance: tolerance for the relative difference of `x` and `y`.
  
  assert_that(is.numeric(x), is.scalar(x))
  assert_that(is.numeric(y), is.scalar(y))
  assert_that(is.numeric(tolerance), is.scalar(tolerance), tolerance > 0)
  
  if (x == 0 && y == 0) {
    return(TRUE)
  } else {
    relative.difference <- abs(x - y) / max(abs(x), abs(y))
    return(relative.difference <= tolerance)
  }
}

TryStop <- function(call, error.msg = NULL) {
  # Tries to evaluate \code{call} and return its return value. If the call
  # fails, throws an error. The error message can be overwritten by
  # \code{error.msg}.
  #
  # Args:
  #   call:      any statement, e.g., a variable, a function call, etc.
  #   error.msg: message to show if \code{call} fails
  #
  # Returns:
  #   The return value of \code{call}. On failure, throws an error.
  #
  # Examples:
  #   \dontrun{
  #   ts <- CausalImpact:::TryStop(as.zoo(data), "failed to convert input data")
  #   }
  
  if (is.null(error.msg)) {
    return(eval(call))
  } else {
    return(tryCatch(eval(call), error = function(e) stop(error.msg)))
  }
}

ParseArguments <- function(args, defaults, allow.extra.args = FALSE) {
  # Fills missing fields in \code{args} with \code{defaults}. This function is
  # similar to what \code{modifyList()} does; except it is not nested, it allows
  # extra flexibility for errors, and \code{NULL} values in \code{args} do not
  # override the defaults.
  #
  # Args:
  #   args:             A list of arguments of any type.
  #
  #   defaults:         A list of default values.
  #
  #   allow.extra.args: Whether to allow (and keep) additional arguments in
  #                     \code{args} that are not present in \code{defaults}.
  #
  # Returns:
  #   \code{defaults}, where any value that is present in \code{args} has been
  #   overridden.
  #
  # Examples:
  #   args <- list(a = 10)
  #   defaults <- list(a = 1, b = 2)
  #   args <- CausalImpact:::ParseArguments(args, defaults)
  #   # Result: a = 10, b = 2
  
  # Check input
  assert_that(!is.null(defaults))
  assert_that(is.list(defaults))
  assert_that(is.list(args) || is.null(args))
  
  # Merge
  if (is.null(args)) {
    args <- list()
  }
  for (arg in names(defaults)) {
    if (!(arg %in% names(args)) || (is.null(args[[arg]]))) {
      args[[arg]] <- defaults[[arg]]
    }
  }
  
  # Are extra args allowed?
  if (!allow.extra.args) {
    illegal.args <- setdiff(names(args), names(defaults))
    assert_that(length(illegal.args) == 0,
                msg = paste0("illegal extra args: '",
                             paste(illegal.args, collapse = "', '"), "'"))
  }
  
  # Return
  return(args)
}

Standardize <- function(y, fit.range = NULL) {
  # Standardizes a vector \code{y}. The resulting vector is a linear
  # transformation of the entire vector \code{y} which has mean 0 and standard
  # deviation 1 over the range of indices specified by \code{fit.range}; i.e.
  # the function transforms the entire vector, but uses only part of it to fit
  # the moments. The original vector can be restored using
  # \code{UnStandardize()}, which is a function that is supplied as part of the
  # return value.
  #
  # Args:
  #   y:         numeric vector (may contain \code{NA} values) to be
  #              standardized
  #   fit.range: vector with 2 entries specifying the first and last index
  #              of the range of \code{y} used to fit the moments. If
  #              \code{NULL}, the whole range of \code{y} is used.
  #
  # Returns:
  #   list of:
  #     y:             standardized input vector, i.e. linearly transformed
  #                    input having mean 0 and SD 1 over the range of indices
  #                    specified by \code{fit.range}.
  #     UnStandardize: function that restores the original data.
  #
  # Examples:
  #   x <- c(1, 2, 3, 4, 5)
  #   result <- CausalImpact:::Standardize(x, c(1, 3))
  #   y <- result$UnStandardize(result$y)
  #   stopifnot(isTRUE(all.equal(x, y)))
  
  assert_that(is.null(dim(y)))
  if (!is.null(fit.range)) {
    assert_that(is.numeric(fit.range), length(fit.range) == 2,
                !anyNA(fit.range), !is.unsorted(c(1, fit.range, length(y))))
  } else {
    fit.range <- c(1, length(y))
  }
  
  y.fit <- y[fit.range[1] : fit.range[2]]
  y.mu <- mean(y.fit, na.rm = TRUE)
  if (is.nan(y.mu)) {
    y.mu <- NA_real_
  }
  y.sd <- sd(y.fit, na.rm = TRUE)
  y <- y - y.mu
  if (!is.na(y.sd) && (y.sd > 0)) {
    y <- y / y.sd
  }
  UnStandardize <- function(y) {
    if (!is.na(y.sd) && (y.sd > 0)) {
      y <- y * y.sd
    }
    y <- y + y.mu
    return(y)
  }
  return(list(y = y, UnStandardize = UnStandardize))
}

StandardizeAllVariables <- function(data, fit.range = NULL) {
  # Standardizes all columns of a given time series. While it transforms entire
  # columns, it just uses the rows specified by \code{fit.range} to fit the
  # moments (mean and standard deviation).
  #
  # Args:
  #   data:      data frame or zoo object with one or more columns
  #   fit.range: vector with 2 entries specifying the first and last row
  #              of the range of \code{data} used to fit the moments. If
  #              \code{NULL}, all rows of \code{data} are used.
  #
  # Returns:
  #   list of:
  #     data: standardized data
  #     UnStandardize: function for undoing the transformation of the first
  #                    column in the provided data
  
  if (!is.null(ncol(data))) {
    for (j in ncol(data) : 1) {
      tmp <- Standardize(data[, j], fit.range)
      data[, j] <- tmp$y
      UnStandardize <- tmp$UnStandardize
    }
  } else {
    tmp <- Standardize(data, fit.range)
    data <- tmp$y
    UnStandardize <- tmp$UnStandardize
  }
  return(list(data = data, UnStandardize = UnStandardize))
}

GetPeriodIndices <- function(period, times) {
  # Computes indices belonging to a period in data.
  #
  # Args:
  #   period:  two-element vector specifying start and end of a period, having
  #            the same data type as `times. The range from `period[1]` to
  #            `period[2]` must have an intersect with `times`.
  #   times:   vector of time points; can be of integer or of POSIXct type.
  #
  # Returns:
  #   A two-element vector with the indices of the period start and end within
  #   `times`.
  
  # Check input
  assert_that(length(period) == 2)
  assert_that(!anyNA(times))
  assert_that(identical(class(period), class(times)) ||
                (is.numeric(period) && is.numeric(times)))
  # Check if period boundaries are in the right order, and if `period` has an
  # overlap with `times`.
  assert_that(period[1] <= period[2])
  assert_that(period[1] <= tail(times, 1), period[2] >= times[1])
  
  # Look up values of start and end of period in `times`; also works if the
  # period start and end time are not exactly present in the time series.
  indices <- seq_along(times)
  is.period <- (period[1] <= times) & (times <= period[2])
  # Make sure the period does match any time points.
  assert_that(any(is.period),
              msg = "The period must cover at least one data point")
  period.indices <- range(indices[is.period])
  return(period.indices)
}

InferPeriodIndicesFromData <- function(y) {
  # Takes in a vector of observations and guesses the beginning and end of the
  # pre-period and the post-period.
  #
  # Args:
  #   y: observation vector
  #
  # Returns:
  #   pre.period: beginning and end of pre-period
  #   post.period: beginning and end of post-period
  #
  # Examples:
  #   CausalImpact:::InferPeriodIndicesFromData(c(10, 20, 30, 40, NA, NA, NA))
  #   # $pre.period
  #   # [1] 1 4
  #   #
  #   # $post.period
  #   # [1] 5 7
  
  assert_that(is.numeric(y))
  assert_that(length(y) >= 2)
  assert_that(is.na(tail(y, 1)))
  pre.period <- rep(NA, 2)
  tmp <- which(!is.na(y))[1]
  assert_that(length(tmp) != 0)
  pre.period[1] <- tmp
  tmp <- tail(which(diff(is.na(y)) == 1), 1)
  assert_that(length(tmp) != 0)
  pre.period[2] <- tmp
  post.period <- rep(NA, 2)
  post.period[1] <- pre.period[2] + 1
  post.period[2] <- length(y)
  return(list(pre.period = pre.period, post.period = post.period))
}

PrettifyPercentage <- function(x, round.digits = 0L) {
  # Converts a number into a nicely formatted percentage.
  #
  # Args:
  #  x:            Input scalar or vector of type numeric.
  #  round.digits: Round resulting percentage to this number of decimal places.
  #
  # Returns:
  #   vector of characters (same length as <vector>) of percentages
  #
  # Examples:
  #   CausalImpact:::PrettifyPercentage(c(-0.125, 0.2), 2)
  #   # [1] "-12.50%" "+20.00%"
  
  # Check input
  assert_that(all(is.finite(x)))
  assert_that(is.numeric(round.digits), is.scalar(round.digits),
              round.digits >= 0)
  round.digits <- as.integer(round.digits)
  
  return(sprintf("%+0.*f%%", round.digits, 100 * x))
}

PrettifyNumber <- function(x, letter = "", round.digits = 1L) {
  # Converts a number into heavily rounded human-readible format.
  #
  # Args:
  #   x:            Input scalar or vector of type numeric.
  #   letter:       Thousand value to round to. Possible values: "" (automatic),
  #                 "B" (billion), "M" (million), "K" (thousand), "none" (1).
  #   round.digits: Round the result to this number of decimal places if abs(x)
  #                 is at least 1 or <letter> is specified. If abs(x) is less
  #                 than 1, and if no <letter> is specified, <round.digits> is
  #                 interpreted as the number of significant digits.
  #
  # Returns:
  #   string of formatted values
  #
  # Examples:
  #   CausalImpact:::PrettifyNumber(c(0.123, 123, 123456))
  #   # [1] "0.1"    "123.0"  "123.5K"
  #   CausalImpact:::PrettifyNumber(3995, letter = "K", round.digits = 2)
  #   # [1] "4.00K"
  #   CausalImpact:::PrettifyNumber(1.234e-3, round.digits = 2)
  #   # [1] "0.0012"
  
  # Check input
  assert_that(is.numeric(x))
  assert_that(is.character(letter))
  assert_that(all(letter %in% c("", "B", "M", "K", "none")))
  assert_that(is.numeric(round.digits), round.digits[1] >= 0)
  round.digits <- as.integer(round.digits[1])
  
  letter <- rep(letter, length.out = length(x))
  PrettifySingleNumber <- function(x, letter, round.digits) {
    if (is.na(x) && !is.nan(x)) {
      return("NA")
    } else if (!is.finite(x)) {
      return(as.character(x))
    } else if ((letter == "" && abs(x) >= 1e9) || letter == "B") {
      return(sprintf("%0.*fB", round.digits, x / 1e9))
    } else if ((letter == "" && abs(x) >= 1e6) || letter == "M") {
      return(sprintf("%0.*fM", round.digits, x / 1e6))
    } else if ((letter == "" && abs(x) >= 1e3) || letter == "K") {
      return(sprintf("%0.*fK", round.digits, x / 1e3))
    } else if (abs(x) >= 1 || x == 0) {
      return(sprintf("%0.*f", round.digits, x))
    } else {
      # Calculate position of first non-zero digit after the decimal point
      first.nonzero <- - floor(log10(abs(x)))
      return(sprintf("%0.*f", round.digits + first.nonzero - 1, x))
    }
  }
  output <- sapply(seq_along(x), function(index) {
    PrettifySingleNumber(x[index], letter[index], round.digits)})
  return(output)
}

IdentifyNumberAbbreviation <- function(abbreviated.number) {
  # Identifies the rounding thousand used in PrettifyNumber(). It is useful when
  # multiple numbers to be rounded at the same level.
  #
  # Args:
  #   abbreviated.number: Abbreviated number, as for example returned by
  #                       PrettifyNumber().
  #
  # Returns:
  #   "B", "M", "K", or "none"
  #
  # Examples:
  #   CausalImpact:::IdentifyNumberAbbreviation("123.5K")
  #   # [1] "K"
  
  letter <- substr(abbreviated.number, nchar(abbreviated.number),
                   nchar(abbreviated.number))
  letter[!is.element(letter, c("B", "M", "K"))] <- "none"
  return(letter)
}

# Copyright 2014 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Code for plotting the result of a CausalImpact analysis.
#
# Author: kbrodersen@google.com (Kay Brodersen)

CreateDataFrameForPlot <- function(impact) {
  # Creates a long-format data frame for CreateImpactPlot().
  #
  # Args:
  #   impact: \code{CausalImpact} results object
  #
  # Returns:
  #   data frame of: time, response, mean, lower, upper, metric
  
  # Check input
  assert_that((class(impact) == "CausalImpact"))
  assert_that(!isTRUE(all(is.na(impact$series[, -c(1, 2)]))),
              msg = "inference was aborted; cannot create plot")
  
  # Create data frame from zoo series
  data <- as.data.frame(impact$series)
  data <- cbind(time = time(impact$series), data)
  
  # Reshape data frame
  tmp1 <- data[, c("time", "response", "point.pred", "point.pred.lower",
                   "point.pred.upper"), drop = FALSE]
  names(tmp1) <- c("time", "response", "mean", "lower", "upper")
  tmp1$baseline <- NA
  tmp1$metric <- "original"
  tmp2 <- data[, c("time", "response", "point.effect", "point.effect.lower",
                   "point.effect.upper"), drop = FALSE]
  names(tmp2) <- c("time", "response", "mean", "lower", "upper")
  tmp2$baseline <- 0
  tmp2$metric <- "pointwise"
  tmp2$response <- NA
  tmp3 <- data[, c("time", "response", "cum.effect", "cum.effect.lower",
                   "cum.effect.upper"), drop = FALSE]
  names(tmp3) <- c("time", "response", "mean", "lower", "upper")
  tmp3$metric <- "cumulative"
  tmp3$baseline <- 0
  tmp3$response <- NA
  data <- rbind(tmp1, tmp2, tmp3)
  data$metric <- factor(data$metric, c("original", "pointwise", "cumulative"))
  rownames(data) <- NULL
  return(data)
}

CreatePeriodMarkers <- function(pre.period, post.period, times) {
  # Creates a vector of period markers to display.
  #
  # Args:
  #   pre.period:  vector of 2 time points that define the pre-period.
  #   post.period: vector of 2 time points that define the post-period.
  #   times:       vector of time points.
  #
  # Returns:
  #   Vector of period markers that should be displayed, generally depicting the
  #   first and last time points of pre- and post-period. The start of the pre-
  #   period is not shown if it coincides with the first time point of the time
  #   series; similarly, the last time point of the post-period is not shown if
  #   it coincides with the last time point of the series. If there is no gap
  #   between pre- and post-period, the start marker of the post-period is
  #   omitted.
  
  pre.period.indices <- GetPeriodIndices(pre.period, times)
  post.period.indices <- GetPeriodIndices(post.period, times)
  markers <- NULL
  if (pre.period.indices[1] > 1) {
    markers <- c(markers, times[pre.period.indices[1]])
  }
  markers <- c(markers, times[pre.period.indices[2]])
  if (pre.period.indices[2] < post.period.indices[1] - 1) {
    markers <- c(markers, times[post.period.indices[1]])
  }
  if (post.period.indices[2] < length(times)) {
    markers <- c(markers, times[post.period.indices[2]])
  }
  markers <- as.numeric(markers)
  return(markers)
}

# Tell R CMD check to treat columns of data frames used in `ggplot` functions
# as global variables; this avoids false positives of "no visible binding for
# global variable ..." during the check.
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("baseline", "lower", "response", "upper"))
}

CreateImpactPlot <- function(impact, metrics = c("original", "pointwise",
                                                 "cumulative")) {
  # Creates a plot of observed data and counterfactual predictions.
  #
  # Args:
  #   impact:  \code{CausalImpact} results object returned by
  #            \code{CausalImpact()}.
  #   metrics: Which metrics to include in the plot. Can be any combination of
  #            "original", "pointwise", and "cumulative".
  #
  # Returns:
  #   A ggplot2 object that can be plotted using plot().
  
  # Create data frame of: time, response, mean, lower, upper, metric
  data <- CreateDataFrameForPlot(impact)
  
  # Select metrics to display (and their order)
  assert_that(is.vector(metrics))
  metrics <- match.arg(metrics, several.ok = TRUE)
  data <- data[data$metric %in% metrics, , drop = FALSE]
  data$metric <- factor(data$metric, metrics)
  
  # Initialize plot
  q <- ggplot(data, aes(x = time)) + theme_bw(base_size = 15)
  q <- q + xlab("") + ylab("")
  if (length(metrics) > 1) {
    q <- q + facet_grid(metric ~ ., scales = "free_y")
  }
  
  # Add prediction intervals
  q <- q + geom_ribbon(aes(ymin = lower, ymax = upper),
                       data, fill = "slategray2")
  
  # Add pre-period markers
  xintercept <- CreatePeriodMarkers(impact$model$pre.period,
                                    impact$model$post.period,
                                    time(impact$series))
  q <- q + geom_vline(xintercept = xintercept,
                      colour = "darkgrey", size = 0.8, linetype = "dashed")
  
  # Add zero line to pointwise and cumulative plot
  q <- q + geom_line(aes(y = baseline),
                     colour = "darkgrey", size = 0.8, linetype = "solid", 
                     na.rm = TRUE)
  
  # Add point predictions
  q <- q + geom_line(aes(y = mean), data,
                     size = 0.6, colour = "darkblue", linetype = "dashed",
                     na.rm = TRUE)
  
  # Add observed data
  q <- q + geom_line(aes(y = response), size = 0.6,  na.rm = TRUE)
  return(q)
}

plot.CausalImpact <- function(x, ...) {
  # Creates a plot of observed data and counterfactual predictions.
  #
  # Args:
  #   x:   A \code{CausalImpact} results object, as returned by
  #        \code{CausalImpact()}.
  #   ...: Can be used to specify \code{metrics}, which determines which panels
  #        to include in the plot. The argument \code{metrics} can be any
  #        combination of "original", "pointwise", "cumulative". Partial matches
  #        are allowed.
  #
  # Returns:
  #   A ggplot2 object that can be plotted using plot().
  #
  # Examples:
  #   \dontrun{
  #   impact <- CausalImpact(...)
  #
  #   # Default plot:
  #   plot(impact)
  #
  #   # Customized plot:
  #   impact.plot <- plot(impact) + ylab("Sales")
  #   plot(impact.plot)
  #   }
  
  return(CreateImpactPlot(x, ...))
}
