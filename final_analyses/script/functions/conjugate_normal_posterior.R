####################################################################
#  R function for Bayesian analysis of normal mean, variance known #
#  Parameters included are:                                        #
#                                                                  #
#  Inputs:                                                         #
#                                                                  #
#   x = vector of data                                             #
#   prior.mean = prior mean                                        #
#   prior.var  = prior variance                                    #
#   data.var   = assumed known variance of data                    #
#                                                                  #
#  Outputs:                                                        #
#                                                                  #
#   post.mean = posterior mean                                     #
#   post.var  = posterior variance                                 #
#                                                                  #
#   Adapted from:                                                  #
#           Brophy, J. M. (2020). Bayesian Interpretation of       #
#           the EXCEL Trial and Other Randomized Clinical Trials   #
#           of Left Main Coronary Artery Revascularization.        #
#           JAMA Internal Medicine, 180(7), 986–992.               #
#                                                                  #
#           https://doi.org/10.1001/jamainternmed.2020.1647        #
#                                                                  #
####################################################################

post.normal.mean <- function(data.mean, data.var, prior.mean, prior.var)
{
  post.mean.numerator <- prior.mean/prior.var + data.mean/data.var
  post.mean.denominator <- 1/prior.var + 1/data.var
  post.mean <-  post.mean.numerator/post.mean.denominator
  post.var <- (1/(1/prior.var + 1/data.var))
  a <- "Post mean = "
  b <- "Post Var = "
  c <- "Post SD = "
  cat(a, post.mean, ",", b, post.var, ",", c, sqrt(post.var), "\n" )
  newlist <- tibble(post.mean, post.var, sqrt(post.var))
  return(newlist)
}