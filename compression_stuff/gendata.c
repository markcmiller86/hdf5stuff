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

#define TYPINT 1
#define TYPDBL 2

/* Populate the hyper-dimensional array with samples of a radially symmetric
   sinc() function but where certain sub-spaces are randomized through dimindx arrays */
static void
hyper_smooth_radial(void *b, int typ, int n, int ndims, int const *dims, int const *m,
    int const * const dimindx[10])
{
    int i;
    double hyper_radius = 0;
    const double amp = 10000;
    double val;

    for (i = ndims-1; i >= 0; i--)
    {
        int iar = n / m[i];
        iar = dimindx[i][iar]; /* allow for randomized shuffle of this axis */
        iar -= dims[i]/2;      /* ensure centering in middle of the array */
        n = n % m[i];
        hyper_radius += iar*iar;
    }
    hyper_radius = sqrt(hyper_radius);

    if (hyper_radius < 1e-15)
        val = amp;
    else
        val = amp * sin(0.4*hyper_radius) / (0.4*hyper_radius);

    if (typ == TYPINT)
    {
        int *pi = (int*) b;
        *pi = (int) val;
    }
    else
    {
        double *pd = (double*) b;
        *pd = val;
    }
}

static double func(int i, double arg)
{
    /* a random assortment of interesting, somewhat bounded, unary functions */
    double (*const funcs[])(double x) = {cos, j0, fabs, sin, cbrt, erf};
    int const nfuncs = sizeof(funcs)/sizeof(funcs[0]);
    return funcs[i%nfuncs](arg);
}

/* Populate the hyper-dimensional array with samples of set of seperable functions
   but where certain sub-spaces are randomized through dimindx arrays */
static void
hyper_smooth_separable(void *b, int typ, int n, int ndims, int const *dims, int const *m,
    int const * const dimindx[10], double jitter)
{
    int i;
    double val = 1;

    for (i = ndims-1; i >= 0; i--)
    {
        int iar = n / m[i];
        iar = dimindx[i][iar]; /* allow for randomized shuffle of this axis */
        iar -= dims[i]/2;      /* ensure centering in middle of the array */
        n = n % m[i];
        val *= func(i, (double) iar + jitter);
    }

    if (typ == TYPINT)
    {
        int *pi = (int*) b;
        *pi = (int) val;
    }
    else
    {
        double *pd = (double*) b;
        *pd = val;
    }
}

/* Produce multi-dimensional array test data with the property that it is random
   in the UNcorrelated dimensions but smooth in the correlated dimensions. This
   is achieved by randomized shuffling of the array indices used in specific
   dimensional axes of the array. */
static void *
gen_random_correlated_array(int typ, int ndims, int const *dims,
                            int nucdims, int const *ucdims, double jitter)
{
    int i, n;
    int nbyt = (int) (typ == TYPINT ? sizeof(int) : sizeof(double)); 
    unsigned char *buf, *buf0;
    int m[10]; /* subspace multipliers */
    int *dimindx[10];
   
    assert(ndims <= 10);

    /* Set up total size and sub-space multipliers */
    for (i=0, n=1; i < ndims; i++)
    {
        n *= dims[i];
        m[i] = i==0?1:m[i-1]*dims[i-1];
    }

    /* allocate buffer of suitable size (doubles or ints) */
    buf0 = buf = (unsigned char*) malloc(n * nbyt);
    
    /* set up dimension identity indexing (e.g. Idx[i]==i) so that
       we can randomize those dimenions we wish to have UNcorrelated */
    for (i = 0; i < ndims; i++)
    {
        int j;
        dimindx[i] = (int*) malloc(dims[i]*sizeof(int));
        for (j = 0; j < dims[i]; j++)
            dimindx[i][j] = j;
    }

    /* Randomize selected dimension indexing */
    srandom(0xDeadBeef);
    for (i = 0; i < nucdims; i++)
    {
        int j, ucdimi = ucdims[i];
        for (j = 0; j < dims[ucdimi]-1; j++)
        {
            int tmp, k = random() % (dims[ucdimi]-j);
            if (k == j) continue;
            tmp = dimindx[ucdimi][j];
            dimindx[ucdimi][j] = k;
            dimindx[ucdimi][k] = tmp;
        }
    }

    /* populate the array data */
    for (i = 0; i < n; i++)
    {
        hyper_smooth_separable(buf, typ, i, ndims, dims, m, (int const * const *) dimindx, jitter);
        buf += nbyt;
    }

    /* free dimension indexing */
    for (i = 0; i < ndims; i++)
        free(dimindx[i]);

    return buf0;
}

static void
modulate_by_time(void *data, int typ, int ndims, int const *dims, int t)
{
    int i, n;

    for (i = 0, n = 1; i < ndims; i++)
        n *= dims[i];

    if (typ == TYPINT)
    {
        int *p = (int *) data;
        for (i = 0; i < n; i++, p++)
        {
            double val = *p;
            val *= exp(0.1*t*sin(t/9.0*2*M_PI));
            *p = val;
        }
    }
    else
    {
        double *p = (double *) data;
        for (i = 0; i < n; i++, p++)
        {
            double val = *p;
            val *= exp(0.1*t*sin(t/9.0*2*M_PI));
            *p = val;
        }
    }
}

static void
buffer_time_step(void *tbuf, void *data, int typ, int ndims, int const *dims, int t)
{
    int i, n;
    int k = t % 4;
    int nbyt = (int) (typ == TYPINT ? sizeof(int) : sizeof(double)); 

    for (i = 0, n = 1; i < ndims; i++)
        n *= dims[i];

    memcpy((char*)tbuf+k*n*nbyt, data, n*nbyt);
}

static void
copy_to_tmp(unsigned char *buf, int i, int n, int nbytes, unsigned char *tmp)
{
    int k;
    int kmax = nbytes < n ? nbytes : n;
    for (k = 0; k < kmax; k++)
    {
#if 0
        printf("copying %d bytes from buf[%d] to tmp[%d]\n", nbytes, k*n+i*nbytes, k*nbytes);
#endif
        memcpy(&tmp[k*nbytes], &buf[k*n+i*nbytes], nbytes);
    }
}

static void
scatter_bytes(unsigned char *buf, int i, int n, int nbytes, unsigned char *tmp)
{
    int k; /* which of the nbytes datums is in play */
    int kmax = nbytes < n ? nbytes : n;
    for (k = 0; k < kmax; k++)
    {
        int j; /* which of the bytes of the datum is in play */
        for (j = 0; j < nbytes; j++)
        {
#if 0
            printf("copying 1 byte, %c, from tmp[%d] to buf[%d](%c)\n", (char) tmp[k*nbytes+j], k*nbytes+j, j*n+i*nbytes+k, (char)buf[j*n+i*nbytes+k]);
#endif
            buf[j*n+i*nbytes+k] = tmp[k*nbytes+j];
        }
    }
}

static void
shuffle_bytes_in_place_like_hdf5(int dtyp, int ndims, int const *dims, void *buf)
{
    int nbytes = (int) (dtyp == TYPINT ? sizeof(int) : sizeof(double));
    int i,n;
    unsigned char *tmp = malloc(nbytes * nbytes);

    /* compute number of entries */
    for (i = 0, n = 1; i < ndims; i++)
        n *= dims[i];

    for (i = 0; i < n / nbytes + (n%nbytes?1:0); i++)
    {
        copy_to_tmp((unsigned char *)buf, i, n, nbytes, tmp);
        scatter_bytes((unsigned char *)buf, i, n, nbytes, tmp);
    }

    free(tmp);

}

static int
write_binary_data_file(char const *fname, void const *buf,
                       int typ, int ndims, int const *dims)
{
    int i, fd;
    int n = typ == TYPINT ? (int) sizeof(int): (int) sizeof(double);

    /* compute size of binary write */
    for (i = 0; i < ndims; i++)
        n *= dims[i];

    if (0 > (fd = open(fname, O_CREAT|O_TRUNC|O_WRONLY, S_IRUSR|S_IWUSR|S_IRGRP))) ERROR(open);
    if (n != write(fd, buf, n)) ERROR(write);
    if (0 != close(fd)) ERROR(close);
    return 0;
}

int main(int argc, char **argv)
{
    int i;

    /* filename variables */
    char *ofile = (char *) calloc(NAME_LEN,sizeof(char));

    /* sinusoid data generation variables */
    int ndims = 3, n0 = 251, n1 = 257, n2 = 263; /* primes near 256 */
    int c0=0, c1=0, c2=0;
    int dims[3], ucdims[3], nucdims;
    int dtyp = TYPDBL;
    double jitter = 0.000;
    int h5shuffle = 0;
    int help = 0;

    int *ibuf = 0;
    double *dbuf = 0;

    /* file arguments */
    strcpy(ofile, "test_dbls.dat");

    /* dataset arguments */
    HANDLE_SEP(Data Generation Arguments)
    HANDLE_ARG(ofile,strndup(argv[i]+len2,NAME_LEN), "\"%s\"",set output filename);
    HANDLE_ARG(ndims,(int)strtol(argv[i]+len2,0,10), "%d",number dimensions in data);
    HANDLE_ARG(n0,(int)strtol(argv[i]+len2,0,10), "%d",size of dimension 0 (ndims>0));
    HANDLE_ARG(c0,(int)strtol(argv[i]+len2,0,10), "%d",de-coorelate dimension 0? (1/0=Y/n));
    HANDLE_ARG(n1,(int)strtol(argv[i]+len2,0,10), "%d",size of dimension 1 (ndims>1));
    HANDLE_ARG(c1,(int)strtol(argv[i]+len2,0,10), "%d",de-coorelate dimension 1? (1/0=Y/n));
    HANDLE_ARG(n2,(int)strtol(argv[i]+len2,0,10), "%d",size of dimension 2 (ndims>2));
    HANDLE_ARG(c2,(int)strtol(argv[i]+len2,0,10), "%d",de-coorelate dimension 2? (1/0=Y/n));
    HANDLE_ARG(dtyp,(int)strtol(argv[i]+len2,0,10), "%d",data type (1=int32, 2=flt64));
    HANDLE_ARG(jitter,(double) strtod(argv[i]+len2,0),"%g",jitter abscissa to separable funcs);
    HANDLE_ARG(h5shuffle,(int)strtol(argv[i]+len2,0,10), "%d",shuffle bytes like HDF5 shuffle filter);
    HANDLE_ARG(help,(int)strtol(argv[i]+len2,0,10),"%d",this help message);
    assert(dtyp==2);
    if (!strncmp(ofile, "test_dbls.dat", 13) && dtyp == TYPINT)
        strcpy(ofile, "test_ints.dat");

    i = 0;
    if (ndims > 0 && c0 != 0) ucdims[i++] = 0;
    if (ndims > 1 && c1 != 0) ucdims[i++] = 1;
    if (ndims > 2 && c2 != 0) ucdims[i++] = 2;
    nucdims = i;

    if (ndims > 0) dims[0] = n0;
    if (ndims > 1) dims[1] = n1;
    if (ndims > 2) dims[2] = n2;
    
    dbuf = gen_random_correlated_array(dtyp, ndims, dims, nucdims, ucdims, jitter);
#if 0
{
    char *buf = (char *) dbuf;
    for (i = 0; i < n0*8; i++)
        buf[i] = 'a' + i%8;
}
#endif

    if (h5shuffle)
        shuffle_bytes_in_place_like_hdf5(dtyp, ndims, dims, dbuf);

    write_binary_data_file(ofile, dbuf, dtyp, ndims, dims);

    free(dbuf);
    free(ofile);

    return 0;
}
