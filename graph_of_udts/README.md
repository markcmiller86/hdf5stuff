This example involves a somewhat complex, pointer-linked (e.g. a graph-like thingy),
dynamically allocated data structure comprised of three different user defined types (UDTs).

The problem we wish to solve is how to write an instance of this data structure to HDF5. We
demonstrate this with both C and Fortran examples.

## C Example Progression

In the first example, `udt_graph1.c`, each single UDT instance in the graph is written to HDF5 

the goal is to make the software engineering as
simple as possible at the expense

## Fortran Example Progression



The evolution of cases for this example is as follows:

1. Using (non-trivial) HDF groups to represent pointers to instances of user-defined types.<br/>
1a. Actual data written as scalars in the groups.<br/>
1b. Actual data written using compound datatypes in the groups.<br/>
2. Complete graph written as 1D array of compounds, one for each user-defined type, where the arrays are written one component at a time.
3. Complete graph written as 1D array of compounds, one for each user-defined type, where the arrays are written all at once.

Hopefully, we will be able to get this working for both c and fortran.


