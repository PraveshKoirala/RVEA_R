# RVEA in R (Work in Progress)

This is a port of the RVEA (reference vector guided evolutionary algorithm) for many-objective optimization in R. The original algorithm is in MATLAB and can be found at [https://github.com/ranchengcn/RVEA_Matlab](https://github.com/ranchengcn/RVEA_Matlab)

TODO:

  - Use substr for all strings instead of the slice operator
  - Verify that each corresponding R function exists for the matlab
  - P_objective.R function can have a lot of bugs, verify
  - Use matrix in all cases.. verify whether row or column vectors are used
  - Check if there are any matrix multiplications.. and change the operators accordingly.