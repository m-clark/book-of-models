# as of 2024-02 the quarto extension 'include-code-files' does not appear to work to be able to import this.
# using include with a string instead of logical will fail, and their own examples are dated.

import pandas as pd
import numpy as np

np.set_printoptions(precision = 4, suppress=True, floatmode = 'maxprec') # suppress is for whether to use scientific notation for otherwise rounded to zero numbers; apparently is either ignored or quarto doesn't passs
pd.set_option('display.precision', 4) # n