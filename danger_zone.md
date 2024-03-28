# Danger Zone


The following are very brief thoughts on common pitfalls in modeling. This is not meant to be comprehensive, but rather a quick reference for common things we think you'd probably want to avoid. We're not going to tell you not to do these things, but we are going to tell you to be very careful if you do, and be ready to defend your choices.


LM:

- stepwise/best subset regression
- focus on stats while ignoring predictive/practical results
- forget to rely heavily on domain knowledge
- forget to rely heavily on visualizations
- use statistical tests for assumptions
- compare models with R2

GLM:

- Use pseudo-R2. These aren't very useful and will never really be the R^2^ you get from OLS.


Estimation:
- overly worry about distribution if primary goal is interpretation
- assume a bootstrap is good enough for inference

Data:
- forget to transform your features
- mean imputation (in most cases)
- make something binary when it naturally isn't and has notable variability
- Large data can get small very quickly imbalance, interactions

ML:
- use a single model without any baseline
- start with a complicated model
- simple .5 cutoff for binary classification
- use a single metric for assessment
- ignore uncertainty
- compare models on different datasets
- let training leak into test set
- assume variable importance is telling you what you think it is
- Assume that because your lasso dropped a feature it has no effect on the target

Causal:
- pretend a model can prove a causal relationship
