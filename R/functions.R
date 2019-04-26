# private function to check vector of probabilities
check_prob <- function(prob) {
  if (!is.numeric(prob)) {
    stop("\n'prob' must be a numeric vector")
  }
  if (any(is.na(prob))) {
    stop("\n'prob' cannot contain missing values")
  }
  if (any(prob < 0) | any(prob > 1)) {
    stop("\n'prob' values must be between 0 and 1")
  }
  TRUE
}

# private function to check vector of trials
check_trials <- function(trial) {
  if (!is.numeric(trial) | any(floor(trial) != trial)) {
    stop("\n'trial' must be an integer vector")
  }
  if (any(is.na(trial))) {
    stop("\n'trial' cannot contain missing values")
  }
  if (any(trial < 0)) {
    stop("\n'trial' values must be positive")
  }
  TRUE
}

# private function to check vector of success
check_success <- function(suc, trial) {
  if (!is.numeric(suc) | any(floor(suc) != suc) | any(suc < 0)) {
    stop("\n'success' must be a non-negative integer vector")
  }
  if (any(is.na(suc))) {
    stop("\n'suc' cannot contain missing values")
  }
  if (any(suc > trial)) {
    stop("\n'success' cannot be greater than trials")
  }
  TRUE && check_trials(trial)
}


# private function to calculate mean
aux_mean <- function(trials, prob) {
  trials * prob
}

# private function to calculate variance
aux_variance <- function(trials, prob) {
  trials * prob * (1 - prob)
}

# private function to calculate mode
aux_mode <- function(trials, prob) {
  if (is.integer(trials * prob + prob)){
    c(trials * prob + prob, trials * prob + prob - 1)
  } else {
    floor(trials * prob + prob)
  }
}


# private function to calculate skewness
aux_skewness <- function(n, p) {
  (1-2*p) / sqrt(n*p*(1-p))
}

# private function to calculate kurtosis
aux_kurtosis <- function(n, p) {
  (1-6*p*(1-p)) / (n*p*(1-p))
}




#' @title bin_choose
#' @description calculate the number of combinations k successes occur in n trials
#' @param k number of successes
#' @param n number of trials
#' @return the number of combinations
#' @export
#' @examples
#' bin_choose(5, 0)
#'
#' bin_choose(n = 5, k = 2)

bin_choose <- function(n, k) {
  check_success(k, n)
  check_trials(n)

  factorial(n) / (factorial(k) * factorial(n-k))
}


#' @title bin_probability
#' @description calculate the probability of getting k successes with probability p in n trials
#' @param k number of successes
#' @param n number of trials
#' @param p probability of success
#' @return the probability of getting k successes with probability p in n trials
#' @export
#' @examples
#' bin_probability(k = 2, n = 5, p = 0.5)
#'
#' bin_probability(k = 0:2, n = 5, p = 0.5)

bin_probability <- function(k, n, p) {
  if (!check_success(k, n)) {
    stop ('invalid success value')
  }
  if (!check_trials(n)) {
    stop ('invalid trials value')
  }
  if (!check_prob(p)) {
    stop ('invalid probability value')
  }

  bin_choose(n, k) * p^k * (1-p)^(n-k)

}


#' @title bin_distribution
#' @description calculate the distribution of probabilities of getting different number of successes
#' @param n number of trials
#' @param p probability of success
#' @return a dataframe with two classes of the probability ditribution
#' @export
#' @examples
#' bin_distribution(n = 5, p = 0.5)

library(dplyr)
bin_distribution <- function(n, p) {
  if (!check_trials(n)) {
    stop ('invalid trials value')
  }
  if (!check_prob(p)) {
    stop ('invalid probability value')
  }

  output = data.frame(success = 0:n)

  output2 = output %>% mutate(probability = bin_probability(output$success, n, p))
  class(output2) = c("bindis", "data.frame")
  output2

}

library(ggplot2)
#' @export
plot.bindis = function(bindis) {
  success = bindis$success
  probability = bindis$probability
  ggplot(bindis, aes(x = success, y = probability)) +
    geom_bar(stat = "identity")
}



#' @title bin_cumulative
#' @description calculate the probability and cumulative distributions
#' @param n number of trials
#' @param p probability of success
#' @return a dataframe with two classes of the probability and cumulative ditribution
#' @export
#' @examples
#' bin_cumulative(5, 0.5)

bin_cumulative <- function(n, p) {
  if (!check_trials(n)) {
    stop ('invalid trials value')
  }
  if (!check_prob(p)) {
    stop ('invalid probability value')
  }

  output = bin_distribution(n, p)

  output2 = output %>% mutate(cumulative = cumsum(probability))


  class(output2) = c("bincum", "data.frame")
  output2

}


#' @export
plot.bincum = function(bincum) {

  success = bincum$success
  probability = bincum$cumulative

  ggplot(bincum, aes(x = success, y = cumulative)) + geom_line() + geom_point()
}


#' @title bin_variable
#' @description display the binomial random variable objects
#' @param n number of trials
#' @param p probability of success
#' @return a list with number of trials and probability of success
#' @export
#' @examples
#' bin_variable(10, 0.3)

bin_variable <- function(n, p) {
  if (!check_trials(n)) {
    stop ('invalid trials value')
  }
  if (!check_prob(p)) {
    stop ('invalid probability value')
  }

  output = list("trials" = n, "prob" = p)
  class(output) = c("binvar")
  output

}


#' @export
print.binvar = function(binvar) {
  cat('"Binomial variable"\n\n')
  cat("Parameters\n")
  cat("- number of trials:", binvar$trials, "\n")
  cat("- prob of success:", binvar$prob)
}



#' @export
summary.binvar = function(binvar) {
  n = binvar$trials
  p = binvar$prob
  output = list("trials" = n, "prob" = p,
                 "mean" = aux_mean(n,p), "variance" = aux_variance(n,p),
                 "mode" = aux_mode(n,p), "skewness" = aux_skewness(n,p),
                 "kurtosis" = aux_kurtosis(n,p))

  class(output) = c("summary.binvar")
  output
}

#' @export
print.summary.binvar = function(summary.binvar) {
  cat('"Summary Binomial"\n\n')
  cat("Parameters\n")
  cat("- number of trials:", summary.binvar$trials, "\n")
  cat("- prob of success:", summary.binvar$prob, "\n\n")
  cat("Measures\n")
  cat("- mean:", summary.binvar$mean, "\n")
  cat("- variance:", summary.binvar$variance, "\n")
  cat("- mode:", summary.binvar$mode, "\n")
  cat("- skewness:", summary.binvar$skewness, "\n")
  cat("- kurtosis:", summary.binvar$kurtosis, "\n")
}



#' @title bin_mean
#' @description calculate the mean
#' @param n number of trials
#' @param p probability of success
#' @return the mean
#' @export
#' @examples
#' bin_mean(10, 0.3)


bin_mean <- function(n, p) {
  if (!check_trials(n)) {
    stop ('invalid trials value')
  }
  if (!check_prob(p)) {
    stop ('invalid probability value')
  }
  aux_mean(n, p)
}


#' @title bin_variance
#' @description calculate the variance
#' @param n number of trials
#' @param p probability of success
#' @return the variance
#' @export
#' @examples
#' bin_variance(10, 0.3)


bin_variance <- function(n, p) {
  if (!check_trials(n)) {
    stop ('invalid trials value')
  }
  if (!check_prob(p)) {
    stop ('invalid probability value')
  }
  aux_variance(n, p)
}


#' @title bin_mode
#' @description calculate the mode
#' @param n number of trials
#' @param p probability of success
#' @return the mode
#' @export
#' @examples
#' bin_mode(10, 0.3)


bin_mode <- function(n, p) {
  if (!check_trials(n)) {
    stop ('invalid trials value')
  }
  if (!check_prob(p)) {
    stop ('invalid probability value')
  }
  aux_mode(n, p)
}

#' @title bin_skewness
#' @description calculate the skewness
#' @param n number of trials
#' @param p probability of success
#' @return the skewness
#' @export
#' @examples
#' bin_skewness(10, 0.3)


bin_skewness <- function(n, p) {
  if (!check_trials(n)) {
    stop ('invalid trials value')
  }
  if (!check_prob(p)) {
    stop ('invalid probability value')
  }
  aux_skewness(n, p)
}


#' @title bin_kurtosis
#' @description calculate the kurtosis
#' @param n number of trials
#' @param p probability of success
#' @return the kurtosis
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)


bin_kurtosis <- function(n, p) {
  if (!check_trials(n)) {
    stop ('invalid trials value')
  }
  if (!check_prob(p)) {
    stop ('invalid probability value')
  }
  aux_kurtosis(n, p)
}


