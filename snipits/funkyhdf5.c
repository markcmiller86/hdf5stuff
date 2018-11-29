#include <fcntl.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "hdf5.h"

/* store nhex X nhex X nhex mesh of hexahedra with materials and other variables to
   HDF5 in a bit of a funky way and demonstrate partial I/O on the result.

   We write the data to HDF5 a few different ways; opaque, unsigned chars, doubles
   and a structure of arrays. We demonstrate partial I/O on these various cases.
*/

/* tags for sub-buffers */
typedef enum _storage_tags_t {
    FIRST = 0,
    CONN = FIRST, /* connectivities */
    MAT,      /* material ids */
    DAM,      /* damage */
    ZFLDA,    /* first zonal field */
    ZFLDB,    /* second zonal field */
    ZFLDV,    /* zonal vector field */
    COORD,    /* nodal coords */
    TAG,      /* nodal tag */
    NFLDA,    /* nodal vector field */
    LAST
} storage_tags_t;

void compute_storage(int nhex, int *_n, int *offsets, hid_t *hdftypes, int *ncomps, char **names)
{
    int n = 0;
    int nzones = nhex * nhex * nhex;
    int nnodes = (nhex+1)*(nhex+1)*(nhex+1);

    /* per-zone storage */
    offsets[CONN] = n;
    hdftypes[CONN] = H5T_NATIVE_INT;
    ncomps[CONN] = 8;
    names[CONN] = strdup("connects");
    n += nzones * 8 * sizeof(int); /* connectivities and ints */

    offsets[MAT] = n;
    hdftypes[MAT] = H5T_NATIVE_LONG;
    ncomps[MAT] = 1;
    names[MAT] = strdup("material");
    n += nzones * sizeof(long); /* zonal material ids as longs */

    offsets[DAM] = n;
    hdftypes[DAM] = H5T_NATIVE_UCHAR;
    ncomps[DAM] = 1;
    names[DAM] = strdup("damage");
    n += nzones * sizeof(unsigned char); /* zonal damage flags as chars */

    offsets[ZFLDA] = n;
    hdftypes[ZFLDA] = H5T_NATIVE_DOUBLE;
    ncomps[ZFLDA] = 1;
    names[ZFLDA] = strdup("zflda");
    offsets[ZFLDB] = n + nzones * sizeof(double);
    hdftypes[ZFLDB] = H5T_NATIVE_DOUBLE;
    ncomps[ZFLDB] = 1;
    names[ZFLDB] = strdup("zfldb");
    n += nzones * 2 * sizeof(double); /* 2 zonal fields as doubles */

    offsets[ZFLDV] = n;
    hdftypes[ZFLDV] = H5T_NATIVE_FLOAT;
    ncomps[ZFLDV] = 3;
    names[ZFLDV] = strdup("zfldvec");
    n += nzones * 3 * sizeof(float); /* 1 zonal vector field as float */

    /* per node storage */
    offsets[COORD] = n;
    hdftypes[COORD] = H5T_NATIVE_FLOAT;
    ncomps[COORD] = 3;
    names[COORD] = strdup("coords");
    n += nnodes * 3 * sizeof(float); /* nodal coordinates as float */

    offsets[TAG] = n;
    hdftypes[TAG] = H5T_NATIVE_INT;
    ncomps[TAG] = 1;
    names[TAG] = strdup("tag");
    n += nnodes * 1 * sizeof(int); /* 1 nodal tag field */

    offsets[NFLDA] = n;
    hdftypes[NFLDA] = H5T_NATIVE_DOUBLE;
    ncomps[NFLDA] = 1;
    names[NFLDA] = strdup("nflda");
    n += nnodes * 1 * sizeof(double); /* 1 nodal field as double */

    offsets[LAST] = n;
    *_n = n;
}

void fill_storage_with_coords(int nnode, void *storage, int offset)
{
    int i,j,k;
    float *p = (float *) ((char*) storage + offset);

    for (k = 0; k < nnode; k++)
        for (j = 0; j < nnode; j++)
            for (i = 0; i < nnode; *p++=(float)i, *p++=(float)j, *p++=(float)k, i++);
}

void fill_storage_with_tag(int nnode, void *storage, int offset)
{
    int i,j,k;
    int *p = (int *) ((char*) storage + offset);

    for (k = 0; k < nnode; k++)
        for (j = 0; j < nnode; j++)
            for (i = 0; i < nnode; i++, *p++=17);
}

void fill_storage_with_nodal_field(int nnode, void *storage, int offset)
{
    int i,j,k;
    double *p = (double *) ((char*) storage + offset);

    for (k = 0; k < nnode; k++)
        for (j = 0; j < nnode; j++)
            for (i = 0; i < nnode; i++, *p++=3.1415926);
}

void fill_storage_with_mats(int nhex, void *storage, int offset)
{
    int i,j,k;
    long *p = (long *) ((char*) storage + offset);

    for (k = 0; k < nhex; k++)
        for (j = 0; j < nhex; j++)
            for (i = 0; i < nhex; i++, *p++=(long)i);
}

void fill_storage_with_damage(int nhex, void *storage, int offset)
{
    int i,j,k;
    char *p = (char *) ((char*) storage + offset);

    for (k = 0; k < nhex; k++)
        for (j = 0; j < nhex; j++)
            for (i = 0; i < nhex; i++, *p++='a'+i%26);
}

void fill_storage_with_connects(int nhex, void *storage, int offset)
{
    int i,j,k;
    int n1 = nhex+1;
    int n2 = (nhex+1)*(nhex+1);
    int *p = (int *) ((char*) storage + offset);

    for (k = 0; k < nhex; k++) {
        for (j = 0; j < nhex; j++) {
            for (i = 0; i < nhex; i++) {
                *p++ = (k+0)*n2+(j+0)*n1+(i+0);
                *p++ = (k+0)*n2+(j+0)*n1+(i+1);
                *p++ = (k+0)*n2+(j+1)*n1+(i+1);
                *p++ = (k+0)*n2+(j+1)*n1+(i+0);
                *p++ = (k+1)*n2+(j+0)*n1+(i+0);
                *p++ = (k+1)*n2+(j+0)*n1+(i+1);
                *p++ = (k+1)*n2+(j+1)*n1+(i+1);
                *p++ = (k+1)*n2+(j+1)*n1+(i+0);
            }
        }
    }
}

void fill_storage_with_zonal_field(int nhex, void *storage, int offset)
{
    int i,j,k;
    double *p = (double *) ((char*) storage + offset);

    for (k = 0; k < nhex; k++)
        for (j = 0; j < nhex; j++)
            for (i = 0; i < nhex; i++, *p++=sqrt((double)i*i+j*j+k*k));
}

void fill_storage_with_zonal_vector_field(int nhex, void *storage, int offset)
{
    int i,j,k;
    float *p = (float *) ((char*) storage + offset);

    for (k = 0; k < nhex; k++) {
        for (j = 0; j < nhex; j++) {
            for (i = 0; i < nhex; i++) {
                *p++=(float)k*nhex*nhex+j*nhex+i;
                *p++=(float)i;
                *p++=(float)j;
            }
        }
    }
}

int main(int argc, char **argv)
{
    int i;
    int nhex = 2; /* 2x2x2 */
    int nbytes, offsets[16], ncomps[16];
    hid_t htypes[16];
    char *names[16];
    void *storage;
    int *tagbuf; /* a target buffer for partial read */
    storage_tags_t stag;
    hid_t fid, opaque_tid, dsid, space_id, struct_tid, arrtid, facprops;
    hid_t dsoffset; 
    hsize_t size;
    int nzones, nnodes;
    float *coord_buf;

    for (i = 1; i < argc; i++)
    {
        if (!strncmp("nhex=",argv[i],5))
            nhex = strtol(argv[i]+5,0,10);
        else
        {
            printf("unrecognized command-line option\n");
            exit(1);
        }
    }
    nzones = nhex * nhex * nhex;
    nnodes = (nhex+1)*(nhex+1)*(nhex+1);

    /* compute total storage needed */
    compute_storage(nhex, &nbytes, offsets, htypes, ncomps, names);

    /* allocate storage */
    storage = malloc(nbytes);

    /* fill storage zonals */
    fill_storage_with_connects(nhex, storage, offsets[CONN]);
    fill_storage_with_mats(nhex, storage, offsets[MAT]);
    fill_storage_with_damage(nhex, storage, offsets[DAM]);
    fill_storage_with_zonal_field(nhex, storage, offsets[ZFLDA]);
    fill_storage_with_zonal_field(nhex, storage, offsets[ZFLDB]);
    fill_storage_with_zonal_vector_field(nhex, storage, offsets[ZFLDV]);

    /* fill storage nodals */
    fill_storage_with_coords(nhex+1, storage, offsets[COORD]);
    fill_storage_with_tag(nhex+1, storage, offsets[TAG]);
    fill_storage_with_nodal_field(nhex+1, storage, offsets[NFLDA]);

    fid = H5Fcreate("funky.hdf5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* write this data as opaque data to HDF5 but create other datasets that 
       reference each of its segments as "external" data */
    opaque_tid  = H5Tcreate(H5T_OPAQUE, 1);
    size = (hsize_t) nbytes;
    space_id = H5Screate_simple(1, &size, &size);
    dsid = H5Dcreate(fid, "opaque", opaque_tid, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dsid, opaque_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, storage);
    H5Dclose(dsid);
    H5Sclose(space_id);
    H5Tclose(opaque_tid);

    /* get opaque dataset offset in the file */
    dsid = H5Dopen(fid, "opaque", H5P_DEFAULT);
    dsoffset = H5Dget_offset(dsid);
    H5Dclose(dsid);

    for (stag = FIRST; stag < LAST; stag++)
    {
        char tmpnm[256];
        hid_t dscrprops = H5Pcreate(H5P_DATASET_CREATE);
        int nents = stag<COORD?nzones:nnodes;
        hsize_t size = nents;
        hid_t stype = H5Tcreate(H5T_COMPOUND, ncomps[stag]*H5Tget_size(htypes[stag]));
        int k;

        for (k = 0; k < ncomps[stag]; k++)
        {
            snprintf(tmpnm, sizeof(tmpnm), "%s_comp_%d", names[stag], k);
            H5Tinsert(stype, tmpnm, k*H5Tget_size(htypes[stag]), htypes[stag]);
        }

        /* create an "external" dataset object pointing to the opaque data in this file */
        H5Pset_external(dscrprops, "funky.hdf5", dsoffset+offsets[stag],size*H5Tget_size(stype));
        space_id = H5Screate_simple(1, &size, &size);
        snprintf(tmpnm, sizeof(tmpnm), "opaque_%s", names[stag]);
        dsid = H5Dcreate(fid, tmpnm, stype, space_id, H5P_DEFAULT, dscrprops, H5P_DEFAULT);
        H5Tclose(stype);
        H5Dclose(dsid);
        H5Sclose(space_id);
    }

    /* close the file */
    H5Fclose(fid);

    free(storage);

    /* affect a partial read of the second coordinate component */
    fid = H5Fopen("funky.hdf5", H5F_ACC_RDONLY, H5P_DEFAULT);
    struct_tid = H5Tcreate(H5T_COMPOUND, H5Tget_size(htypes[COORD]));
    H5Tinsert(struct_tid, "coords_comp_0", 0, htypes[COORD]);
    dsid = H5Dopen(fid, "opaque_coords", H5P_DEFAULT);
    coord_buf = (float *) malloc(nnodes * sizeof(float));
    H5Dread(dsid, struct_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, coord_buf);
    for (i = 0; i < nnodes; i++)
        printf("coord_buf[%d] = %f\n", i, coord_buf[i]);
    H5Dclose(dsid);
    H5Tclose(struct_tid);
    H5Fclose(fid);
    free(coord_buf);
}
