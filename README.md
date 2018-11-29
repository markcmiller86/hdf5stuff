# hdf5stuff
Temporary place to collect various HDF5 example/test clients

* [testhdf5_serialize.c](./snipits/testhdf5_serialize.c)
  * Creates a random JSON object using JSON-CWX library and writes it, naively, to HDF5
  * Has a few command line arguments size of object and amounts of meta data vs raw data within it.

* [testhdf5.c](./snipits/testhdf5.c)
  * Extensive test of HDF5 meta data which can generate 1,2,3 or 4 levels of groups with tiny datasets within it
  * Tests time to write, amount of object overhead

* [test_hdf5_complicated_udts_groups.c](./snipits/test_hdf5_complicated_udts_groups.c)
  * Example of writing a pointer-linked data structure to HDF5 in a way that is really simple from the software
    engineering standpoint but does not use HDF5 in an optimal way.

* [test_hdf5_complicated_udts_compounds.c](./snipits/test_hdf5_complicated_udts_compounds.c)
  * Example of writing a pointer-linked data structure to HDF5 in a way that minimizes HDF5 object overheads
  * This should be last step in evolution of this use case from the naive approach of realizing each node
    in the data structure as an HDF5 group.

* [test_hdf5_complicated_udts_compounds.F90](./snipits/test_hdf5_complicated_udts_compounds.F90)
  * Same as above except as a fortran 90 example.

To add new examples, create a top-level directory and start developing the example, including any
markdown files associated with it in that directory



