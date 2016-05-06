# RVEA in R (Work in Progress)

This is a port of the RVEA (reference vector guided evolutionary algorithm) for many-objective optimization in R. The original algorithm is in MATLAB and can be found at [https://github.com/ranchengcn/RVEA_Matlab](https://github.com/ranchengcn/RVEA_Matlab)

TODO:
  - P_objective.R function can have a lot of bugs, verify
  - Remove rand_seed
  - Error in objective function.. 1:0 in R is [1 0] and not simply empty.. see DTLZ2 for solution