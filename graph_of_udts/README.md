This example involves a somewhat complex, pointer-linked (e.g. a graph-like thingy),
dynamically allocated data structure comprised of three different user defined types (UDTs).

The problem we wish to solve is how to write an instance of this data structure to HDF5. We
demonstrate this with both C and Fortran examples.

## C Example Progression

We demonstrate 3 progressively more advanced examples of using HDF5 to write this data.

### Naive C Example

In the first example, `udt_graph1.c`, each *node* in the graph of UDTs is realized as an
HDF5 *group*. The data associated with a specific UDT, sans its pointer members, is written
as a singleton *dataset* (e.g. a dataset with a single *point*) within the HDF5 *group*
repesenting the node in the graph of UDTs. The *pointers* which link individual UDT
instances together are realized as *containership* in HDF5's *groups*. If UDT instance
`A` points to UDT instance `B` the `B` should appear as a sub-group of `A`.

This is a very simple approach from a software engineering standpoint. However, HDF5 file
overheads are bad. Overheads for groups and datasets.

### Better way to use HDF5 for this case.

## Fortran Example Progression



The evolution of cases for this example is as follows:

1. Using (non-trivial) HDF groups to represent pointers to instances of user-defined types.<br/>
1a. Actual data written as scalars in the groups.<br/>
1b. Actual data written using compound datatypes in the groups.<br/>
2. Complete graph written as 1D array of compounds, one for each user-defined type, where the arrays are written one component at a time.
3. Complete graph written as 1D array of compounds, one for each user-defined type, where the arrays are written all at once.

Hopefully, we will be able to get this working for both c and fortran.


