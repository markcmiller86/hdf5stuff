* [gendata.c](./gendata.c)
  * Generates a binary disk file of 1, 2 or 3 dimensional array of doubles which are
    samples of a separable combination of smooth, transendental math functions.
  * You can specify the size for each dimension.
  * Default is 3D where sizes in each dimension are prime numbers.
  * You can *decoorrelate* any dimension (e.g. make the data along that dimension random/noisey rather
    than smooth) independently. This has the effect of randomly swapping entries along that dimension.
    For example, if you have a 3D volume, you can think of it as a 2D stack of slices. If you choose to
    decorrelate along the 3rd (stack) dimension, it would be equivalent to jumbling the slices, like
    shuffling a deck of cards.
  * You can also add noise by jittering the sample positions
  * `./gendata --hep` gives command-line argument help
* You can use command-line compression tools like gzip to test time to compress and compression ratio
  of the data produced by `gendata`
* [h5zip.c](./h5zip.c)
  * Simple HDF5 client that reads a binary disk file into memory and then writes it as an HDF5
    dataset to an HDF5 file with various compression features enabled.
  * The purpose is to compare time and compression of the data via HDF5 to standard command-line
    tools.
  * There is a set of arguments to `h5zip` that are the same as for `gendata` to specify the size
    and shape of the array in the binary disk file that it reads.
