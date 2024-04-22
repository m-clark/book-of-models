
# Preliminaries -----------------------------------------------------------

# summary of the function from chatgpt
# The function boost performs gradient boosting regression. It takes in
# num_iter (number of iterations, default = 100), X_train (training predictors),
# y_train (training response), depth (whether to use polynomial features up to
# cubic or linear, default = FALSE), lr (learning rate, default = .3), col_samp
# (proportion of columns to sample at each iteration, default = .5) as inputs.
# It outputs a list with two components: prediction (the final prediction) and
# loss (the root mean squared error at each iteration). The code implements
# gradient boosting by looping through num_iter iterations and adding a weighted
# version of the current prediction to the weighted sum of all previous
# predictions, with weight equal to the learning rate lr.


library(tidyverse)

set.seed(1240)
N = 500
p = 10
X = mvtnorm::rmvnorm(N*2, rep(0, p))
y = 10 + X[,1] - X[,2] + .5*X[,1]*X[,5] + X[,3] +  .5*X[,3]^2 + rnorm(N*2, sd = 1)

X_train = data.frame(X[1:N,])
# X_test  = X[(N+1):N,]
y_train = y[1:N]
# y_test  = y[(N+1):N]

num_iter = 500

# loss_test  = rep(0, num_iter)
# col_samp = .5

boost <- function(
    X,
    y,
    nrounds  = 100,
    depth    = 1,
    lr       = .3,
    col_samp = .5
) {
  
  # initialize
  loss = rep(NA, num_iter)
  
  nc = ncol(X)
  
  # formula wont' allow a power of 1
  depth = ifelse(depth < 2, '', paste0('^', depth))
  form = as.formula(glue::glue("yi ~ .{depth}"))
  
  for (i in 1:num_iter) {
    if (i == 1)
      yi = y
    
    Xi = X %>% select(sample(1:nc, round(col_samp*nc)))
    
    fit = lm(form, data = Xi)
        
    pred = predict(fit)
    
    if (i == 1)
      predictions = pred
    else
      predictions = predictions + lr*pred # simple shrinkage a la ElemStatLearn 10.12.1
    
    loss[i] = sqrt(mean((y - predictions)^2))
    
    yi = y - predictions
    
  }
  
  list(
    prediction = predictions,
    loss = loss
  )
}

boost_fit1 = boost(X = X_train, y = y_train, nrounds = num_iter)
boost_fit2 = boost(X = X_train, y = y_train, nrounds = num_iter, depth = 3)

# plot(diff(boost_fit1$loss))

# cbind(
#   y_train, 
#   boost_fit1 = boost_fit1$prediction,
#   boost_fit2 = boost_fit2$prediction
#   ) %>% head()

# cor(boost_fit1$prediction, y_train)
# cor(boost_fit2$prediction, y_train)


par = list(
  booster     = 'gblinear'
  , objective = 'reg:squarederror'
  , eta       = .3
  , updater   = 'coord_descent'
  , feature_selector = 'random'
  # , nthread     = 1
  , eval_metric = 'rmse'
)


library(xgboost)

xgb_fit = xgboost(
  params    =  par,
  data      = as.matrix(X_train), 
  label     = y_train,
  nrounds   = num_iter,
  callbacks = list(cb.gblinear.history()),
  verbose   = FALSE 
)

xgb_preds = predict(xgb_fit, xgb.DMatrix(as.matrix(X_train)))

cor(cbind(pred = boost_fit1$prediction,
          pred_depth = boost_fit2$prediction,
          xgb = xgb_preds),
    y = tibble(y_train))

lm_fit = lm(y_train~., data.frame(X_train))

# plot(boost_fit1$prediction, xgb_preds)
data.frame(
  y  = y_train,
  lm = fitted(lm_fit),
  pred = boost_fit1$prediction, 
  xgb_preds, 
  boost_xgb_diff = xgb_preds-boost_fit1$prediction) %>%
  head()


loss_df = tibble(
  iter = 1:num_iter,
  no_depth = boost_fit1$loss,
  depth = boost_fit2$loss,
  xgb = xgb_fit$evaluation_log$train_rmse
)  %>%
  pivot_longer(-iter, names_to = 'type', values_to = 'loss') 

loss_df %>%
  # filter(type != 'depth') %>% 
  ggplot(aes(iter, loss)) +
  geom_line(aes(color = type), linewidth = 1) +
  theme_minimal()


data.frame(
  lm    = summary(lm_fit)$sigma,
  boost = last(boost_fit1$loss), 
  depth = last(boost_fit2$loss), 
  xgb   = last(xgb_fit$evaluation_log$train_rmse)
)

# coef_path <- xgb.gblinear.history(xgb_fit)
# matplot(coef_path, type = 'l')
# tibble(
#   xgb = colMeans(coef_path),
#   lm  = coef(lm_fit)
# )
# 
# xgb.importance(model = xgb_fit)


pred_df = tibble(
  boost_fit1 = boost_fit1$prediction,
  boost_fit2 = boost_fit2$prediction,
  xgb_preds,
  lm_preds = predict(lm_fit),
  y_train
)

pred_df

cor(pred_df) %>% round(3)

library(mgcv)

RHS = paste(map_chr(colnames(data.frame(X)), \(x) glue::glue("s({x}, bs = 'gp')")), collapse = ' + ')

mod = gam(as.formula(paste('y_train ~ ',RHS)), data = data.frame(X_train))
summary(mod)
