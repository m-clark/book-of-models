library(tidyverse)

set.seed(123)

l = 1            # for l, sigma_f, sigma_n, see note at covariance function
sigma_f = 1      
sigma_n = .25 
k_eps   = 1e-8   # see note at Kstarstar
n_prior = 5      # number of prior draws
n_post_pred = 5  # number of posterior predictive draws

X_train = 15 * (runif(20) - .5)  
n_train = length(X_train)

# kept sine function for comparison to noise free result
y_train = sin(X_train) + rnorm(n = n_train, sd = .1)  

X_test = seq(-7.5, 7.5, length = 200)
n_test = length(X_test)

gp_mu <- function(x) {
  map_dbl(x, function(x) x = 0)
}

gp_K <- function(
  x,
  y = NULL,
  l = 1,
  sigma_f = 1,
  sigma_n = .5
  ) {
  
  if(!is.null(y)){
    sigma_f * exp( -(1/(2 * l^2)) * as.matrix(dist(x, upper = TRUE, diag = TRUE) ^ 2) ) +
      sigma_n*diag(length(x))    
  }
  else{
    sigma_f * exp( -(1/(2 * l^2)) * as.matrix(dist(x, upper = TRUE, diag = TRUE) ^ 2) )
  }  
}

Ky = gp_K(
  x = X_train,
  y = y_train,
  l = l,
  sigma_f = sigma_f,
  sigma_n = sigma_n
)

# initial matrix
K_ = gp_K(
  c(X_train, X_test),
  l = l,
  sigma_f = sigma_f,
  sigma_n = sigma_n
)

Kstar  = K_[1:n_train, (n_train+1):ncol(K_)]                    # dim = N x N*
tKstar = t(Kstar)                                               # dim = N* x N
Kstarstar = K_[(n_train+1):nrow(K_), (n_train+1):ncol(K_)] +    # dim = N* x N*
  k_eps*diag(n_test)      # the k_eps part is for positive definiteness

Kyinv = solve(Ky)

post_mu = gp_mu(X_test) + tKstar %*% Kyinv %*% (y_train - gp_mu(X_train))
post_K  = Kstarstar - tKstar %*% Kyinv %*% Kstar
s2 = diag(post_K)

y_pp = data.frame(t(MASS::mvrnorm(n_post_pred, mu = post_mu, Sigma = post_K)))

pp_data = data.frame(
  x = X_test,
  y = y_pp,
  fmean = post_mu, 
  se_lower = post_mu - 2 * sqrt(s2),
  se_upper = post_mu + 2 * sqrt(s2)
) %>% 
  pivot_longer(starts_with('y'), names_to = 'variable')

gdat = data.frame(
  x = X_test,
  y = y_pp,
  fmean = post_mu,
  se_lower = post_mu - 2 * sqrt(s2),
  se_upper = post_mu + 2 * sqrt(s2)
) %>%
  gather(key = variable,
         value = value,
         -x,
         -fmean,
         -se_lower,
         -se_upper)

ggplot(aes(x = x, y = value), data = gdat) +
  geom_ribbon(aes(ymin = se_lower, ymax = se_upper, group = variable),
              fill = 'gray98') +
  geom_line(aes(group = variable), color = '#FF550080') +
  geom_line(aes(group = variable, y = fmean),
            color = '#d9edf7',
            size = 2) +
  geom_point(
    aes(x = X_train, y = y_train),
    size = 4,
    color = '#0085a1',
    alpha = .5,
    data = data.frame(X_train, y_train)
  ) +
  geom_point(
    aes(x = X_train, y = y_train),
    size = 2,
    color = '#0085a1',
    alpha = .25,
    data = data.frame(X_train, y_train)
  ) +
  theme_void()

# ggsave('img/gp.svg', width = 6, height = 4, dpi = 300)


# gplite could be used for the posterior mean plot

# X_train = 15 * (runif(20) - .5)  
# n_train = length(X_train)

# # kept sine function for comparison to noise free result
# y_train = sin(X_train) + rnorm(n = n_train, sd = .1)  

# X_test = seq(-7.5, 7.5, length = 200)
# n_test = length(X_test)

# # set up the gp model, and optimize the hyperparameters
# gp <- gp_init(cfs = cf_sexp(), lik = lik_gaussian())
# gp <- gp_optim(gp, X_train, y_train)

# # compute the predictive mean and variance in a grid of points
# # xt <- seq(-4, 4, len = 300)
# pred <- gp_pred(gp, X_test, var = T)

# # visualize
# mu <- pred$mean
# lb <- pred$mean - 2*sqrt(pred$var)
# ub <- pred$mean + 2*sqrt(pred$var)

# ggplot() +
#   geom_ribbon(aes(x = X_test, ymin = lb, ymax = ub), fill = 'gray98') +
#   geom_line(aes(x = X_test, y = mu), size = 1, color = '#ff550080') +
#   geom_point(
#     aes(x = X_train, y = y_train),
#     size = 4,
#     color = '#0085a1',
#     alpha = .5,
#     data = data.frame(X_train, y_train)
#   ) +
#   geom_point(
#     aes(x = X_train, y = y_train),
#     size = 2,
#     color = '#0085a1',
#     alpha = .25,
#     data = data.frame(X_train, y_train)
#   ) +
#   xlab('x') + ylab('y') + theme_void()

