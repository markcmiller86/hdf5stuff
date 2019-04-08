/*
This file was copied from H5Z-ZFP and modified for purposes here.

This tool will generate a binary data file of 2 or 3D arrays of
double precision floating point data with certain properties as
specified by the command-line arguments.
*/

#include <clargs.h>

#include <assert.h>
#include <fcntl.h>
#include <math.h>
#include <unistd.h>

#include "hdf5.h"

/* Generate a simple, 1D sinusioidal data array with some noise */
#define TYPINT 1
#define TYPDBL 2

static void *
read_binary_data_file(char const *fname, int typ, int ndims, int const *dims)
{
    int i, fd;
    int n = typ == TYPINT ? (int) sizeof(int): (int) sizeof(double);
    void *buf;

    /* compute size of binary read */
    for (i = 0; i < ndims; i++)
        n *= dims[i];

    if (0 == (buf = malloc(n))) ERROR(malloc);
    if (0 > (fd = open(fname, O_RDONLY))) ERROR(open);
    if (n != read(fd, buf, n)) ERROR(read);
    if (0 != close(fd)) ERROR(close);
    return buf;
}

static int
write_hdf5_file(char const *fname, void const *buf, int typ, int ndims, int const *dims, int z)
{
    hid_t fid, cpid, spid, dsid;
    hsize_t hdims[3] = {dims[0],dims[1],dims[2]};
    hid_t tid = typ == TYPINT ? H5T_NATIVE_INT : H5T_NATIVE_DOUBLE;

    /* create HDF5 file */
    if (0 > (fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT))) ERROR(H5Fcreate);
    if (0 > (spid = H5Screate_simple(ndims, hdims, 0))) ERROR(H5Screate_simple);

    /* setup dataset creation properties */
    if (0 > (cpid = H5Pcreate(H5P_DATASET_CREATE))) ERROR(H5Pcreate);
    if (z)
    {
        /* single whole dataset chunk */
        hdims[2] = 1;
        if (0 > H5Pset_chunk(cpid, ndims, hdims)) ERROR(H5Pset_chunk);
        if (0 > H5Pset_shuffle(cpid)) ERROR(H5Pset_shuffle);
        if (0 > H5Pset_deflate(cpid, 9)) ERROR(H5Pset_deflate);
    }

    /* write the data */
    if (0 > (dsid = H5Dcreate(fid, "data", tid, spid, H5P_DEFAULT, cpid, H5P_DEFAULT))) ERROR(H5Dcreate);
    if (0 > H5Dwrite(dsid, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)) ERROR(H5Dwrite);

    /* cleanup hdf5 ids */;
    if (0 > H5Pclose(cpid)) ERROR(H5Pclose);
    if (0 > H5Dclose(dsid)) ERROR(H5Dclose);
    if (0 > H5Sclose(spid)) ERROR(H5Sclose);
    if (0 > H5Fclose(fid)) ERROR(H5Fclose);

    return 0;
}


int main(int argc, char **argv)
{
    int i;

    /* filename variables */
    char *ofile = (char *) calloc(NAME_LEN,sizeof(char));
    char *ofilez = (char *) calloc(NAME_LEN,sizeof(char));
    char *ifile = (char *) calloc(NAME_LEN,sizeof(char));

    int ndims = 3, n0 = 251, n1 = 257, n2 = 263; /* primes near 256 */
    int c0=0, c1=0, c2=0;
    int dims[3], ucdims[3], nucdims;
    int dtyp = TYPDBL;
    int help = 0;

    int *ibuf = 0;
    double *dbuf = 0;

    /* file arguments */
    strcpy(ifile, "test_dbls.dat");
    strcpy(ofile, "test_dbls.h5");
    strcpy(ofilez, "test_dbls.h5z");

    /* dataset arguments */
    HANDLE_SEP(HDF5 Dataset Arguments)
    HANDLE_ARG(ifile,strndup(argv[i]+len2,NAME_LEN), "\"%s\"",set input filename);
    HANDLE_ARG(ofile,strndup(argv[i]+len2,NAME_LEN), "\"%s\"",set output filename);
    HANDLE_ARG(ndims,(int)strtol(argv[i]+len2,0,10), "%d",number dimensions in data);
    HANDLE_ARG(n0,(int)strtol(argv[i]+len2,0,10), "%d",size of dimension 0 (ndims>0));
    HANDLE_ARG(c0,(int)strtol(argv[i]+len2,0,10), "%d",de-coorelate dimension 0? (1/0=Y/n));
    HANDLE_ARG(n1,(int)strtol(argv[i]+len2,0,10), "%d",size of dimension 1 (ndims>1));
    HANDLE_ARG(c1,(int)strtol(argv[i]+len2,0,10), "%d",de-coorelate dimension 1? (1/0=Y/n));
    HANDLE_ARG(n2,(int)strtol(argv[i]+len2,0,10), "%d",size of dimension 2 (ndims>2));
    HANDLE_ARG(c2,(int)strtol(argv[i]+len2,0,10), "%d",de-coorelate dimension 2? (1/0=Y/n));
    HANDLE_ARG(dtyp,(int)strtol(argv[i]+len2,0,10), "%d",data type (1=int32, 2=flt64));
    HANDLE_ARG(help,(int)strtol(argv[i]+len2,0,10),"%d",this help message); /* must be last for help to work */

    i = 0;
    if (ndims > 0 && c0 != 0) ucdims[i++] = 0;
    if (ndims > 1 && c1 != 0) ucdims[i++] = 1;
    if (ndims > 2 && c2 != 0) ucdims[i++] = 2;
    nucdims = i;

    if (ndims > 0) dims[0] = n0;
    if (ndims > 1) dims[1] = n1;
    if (ndims > 2) dims[2] = n2;
    
    dbuf = read_binary_data_file(ifile, dtyp, ndims, dims);

    write_hdf5_file(ofile, dbuf, dtyp, ndims, dims, 1);

    free(dbuf);
    free(ofile);

    return 0;
}
