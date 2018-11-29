The evolution of cases for this example is as follows:

1. Writing of all data elements in the graph as scalars in the root group.
2. Using (non-trivial) HDF groups to represent pointers to instances of user-defined types.<br/>
2a. Actual data written as scalars in the groups.<br/>
2b. Actual data written using compound datatypes in the groups.<br/>
3. Complete graph written as 1D array of compounds, one for each user-defined type.

Hopefully, we will be able to get this working for both c and fortran.



