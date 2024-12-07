The latest R used in the 2024 version of the book was 4.4.1. Quarto notebooks and the book itself were based on quarto version 1.6.3 as of this writing.

Some related model package versions used are:

`lme4==1.1.35`
`quantreg==5.97`
`marginaleffects==0.18.0`
`DALEX==2.4.3`


For MLR stuff, we recommend updating to whatever the latest versions are available, as it is under regular and extensive development.

`mlr3==0.20.2.9000`
`mlr3learners==0.6.0`
`mlr3extralearners==0.9.0.9`
`mlr3measures==0.6.0.9000`
`mlr3pipelines==0.7.0`
`mlr3tuning==0.20.0`
`mlr3torch==0.1.2`

Note that you will also need the underlying model packages for some of the learners, such as `lightgbm` or `glmnet`.

All data processing and most visualization was done via tidyverse packages.