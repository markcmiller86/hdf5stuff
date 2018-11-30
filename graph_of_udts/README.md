The evolution of cases for this example is as follows:

1. Using (non-trivial) HDF groups to represent pointers to instances of user-defined types.<br/>
1a. Actual data written as scalars in the groups.<br/>
1b. Actual data written using compound datatypes in the groups.<br/>
2. Complete graph written as 1D array of compounds, one for each user-defined type, where the arrays are written one scalar at a time.
3. Complete graph written as 1D array of compounds, one for each user-defined type, where the arrays are written all at once.

Hopefully, we will be able to get this working for both c and fortran.


