library(purrr)
library(devtools)
library(usethis)

distribuicoes <- function(param_list) {
  if(param_list$distribution == "bernoulli") {
    aux = rbinom(param_list$obs, 1, param_list$p)
  }
  else
    if(param_list$distribution == "poisson") {
      aux = rpois(param_list$obs, param_list$lambda)
    }
    else
      if(param_list$distribution == "normal") {
        aux = rnorm(param_list$obs, param_list$mu, param_list$sigma2)
      }
  return(aux)  
}
