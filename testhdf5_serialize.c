#include <json-cwx/json.h>

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hdf5.h"

/* useful macro for comparing HDF5 versions */
#define HDF5_VERSION_GE(Maj,Min,Rel)  \
        (((H5_VERS_MAJOR==Maj) && (H5_VERS_MINOR==Min) && (H5_VERS_RELEASE>=Rel)) || \
         ((H5_VERS_MAJOR==Maj) && (H5_VERS_MINOR>Min)) || \
         (H5_VERS_MAJOR>Maj))

#define ADD_ARG(TYPE,NAME,DEFVAL,SPEC,HELPSTR)                  \
    TYPE NAME = DEFVAL;                                         \
    {                                                           \
        char tmpstr[128];                                       \
        int len = strlen(#NAME)+1;                              \
        int i;                                                  \
        int _help = 0;                                          \
        for (i = 1; i < argc; i++)                              \
        {                                                       \
            if (strcasestr(argv[i], "help"))                    \
            {                                                   \
                if (i < argc - 1)                               \
                {                                               \
                    char *tmp = argv[argc-1];                   \
                    argv[argc-1] = argv[i];                     \
                    argv[i] = tmp;                              \
                }                                               \
                argc--;                                         \
                break;                                          \
            }                                                   \
        }                                                       \
        for (i = 1; i < argc; i++)                              \
        {                                                       \
            if (!strncmp(argv[i], #NAME"=", len))               \
            {                                                   \
                if (strchr(#TYPE,'*'))                          \
                    NAME=(TYPE) (argv[i]+len);                  \
                else                                            \
                    sscanf(argv[i]+len,SPEC,&NAME);             \
                if (i < argc - 1)                               \
                {                                               \
                    char *tmp = argv[argc-1];                   \
                    argv[argc-1] = argv[i];                     \
                    argv[i] = tmp;                              \
                }                                               \
                argc--;                                         \
                break;                                          \
            }                                                   \
        }                                                       \
        len = snprintf(tmpstr, sizeof(tmpstr), "%s="SPEC, #NAME, NAME);\
        printf("    %s%*s\n",tmpstr,80-len,HELPSTR);            \
        if (!strncmp(#NAME, "help", 4))                         \
        {                                                       \
            if (_help && argc == 1) return 0;                   \
            if (argc > 1)                                       \
            {                                                   \
                printf("unrecognized arguments...\n");          \
                for (i = 1; i < argc; i++)                      \
                    printf("    \"%s\"\n", argv[i]);            \
                return 1;                                       \
            }                                                   \
        }                                                       \
    }

static int best_2d_factors(
    int val,
    int *x,
    int *y
)
{
    int root = (int) sqrt((double)val);
    while (root)
    {
        if (!(val % root))
        {
            *x = root;
            *y = val / root;
            return 0;
        }
        root--;
    }
    return 1;
}

static json_object *
make_random_bool(int *nused)
{
    *nused = 1;
    if (random() % 2)
        return json_object_new_boolean(JSON_C_TRUE);
    else
        return json_object_new_boolean(JSON_C_FALSE);
}

static json_object *
make_random_int(int *nused)
{
    *nused = 4;
    return json_object_new_int(random() % 100000);
}

static json_object *
make_random_double(int *nused)
{
     *nused = 8;
     return json_object_new_double(
         (double) (random() % 100000) / (random() % 100000 + 1));
}

static json_object *
make_random_string(int nbytes, unsigned maxds, int *nused)
{
    char const *chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-";
    int const charslen = strlen(chars);
    int i;
    int strsize;

    if (maxds)
        strsize = 2 + random() % maxds;
    else if (nbytes < 8)
        strsize = nbytes;
    else if (nbytes < 248)
        strsize = random() % nbytes + 8;
    else
        strsize = random() % 248 + 8;

    *nused = strsize;
    char *rval = (char *) malloc(strsize);
    json_object *retval;

    for (i = 0; i < strsize-1; i++)
        rval[i] = *(chars + random() % charslen);
    rval[strsize-1] = '\0';
    retval = json_object_new_string(rval);
    free(rval);

    return retval;
}

/* Makes a random array of fixed type (bool, int, double or string).
   In particular, no array will have member sub-objects. */
static json_object *
make_random_array(int nmeta, unsigned maxds, int *umeta)
{
    int rval = random() % 100; /* selects type of datums in array */
    int rbyt = 1 + random() % nmeta;
    int rmax = 0;
    int i = 0;

    if (maxds)
        rmax = 1 + random() % maxds;

    *umeta = 0;
    json_object *retval = json_object_new_array();
    while (*umeta < rbyt)
    {
        int n;
        if (rval < 5) /* 5% bools */
            json_object_array_add(retval, make_random_bool(&n));
        else if (rval < 45) /* 40% ints */
            json_object_array_add(retval, make_random_int(&n));
        else if (rval < 85) /* 40% doubles */
            json_object_array_add(retval, make_random_double(&n));
        else /* 15% strings */
            json_object_array_add(retval, make_random_string(rbyt-*umeta,maxds,&n));
        *umeta += n;
        i++;
        if (maxds && i >= rmax) break;
    }
    return retval;
}

static json_object *
make_random_extarr(int nraw, unsigned maxds, int *uraw)
{
    
    int dims[2], ndims = random() % 2 + 1;
    int rval = random() % 100;
    int valsize = rval < 33 ? sizeof(int) : sizeof(double);
    int nvals;
    json_extarr_type dtype;
    void *data;

    if (maxds)
        nvals = 1 + random() % (maxds-1);
    else
        nvals = random() % (nraw / valsize) + 8;

    dims[0] = nvals;
    if (ndims == 2)
        best_2d_factors(nvals, &dims[0], &dims[1]);

    if (rval < 33) /* favor double arrays over int arrays 2:1 */
    {
        int i, *vals = (int *) malloc(nvals * sizeof(int));
        for (i = 0; i < nvals; i++)
            vals[i] = (i % 11) ? i : (random() % nvals);
        dtype = json_extarr_type_int32;
        data = vals;
    }
    else
    {
        int i;
        double *vals = (double *) malloc(nvals * sizeof(double));
        for (i = 0; i < nvals ; i++)
            vals[i] = (i % 11) ? i : (double) (random() % nvals) / nvals;
        dtype = json_extarr_type_flt64;
        data = vals;
    }

    *uraw = nvals * valsize;
    return json_object_new_extarr(data, dtype, ndims, dims);
}

/* A terminal is a json_object that will NOT have sub-objects */
static json_object *
make_random_terminal(int nraw, int nmeta, unsigned maxds, int *uraw, int *umeta)
{
    int rval = random() % 100;
    int tbool = 0, tint = 0, tdbl = 0, tstr = 0, tarr = 0;
    int nsum = nraw + nmeta ? nraw + nmeta : 1;
    int tval = 100 * nmeta / nsum;
    int dometa = rval < tval;

    if (nraw < 0)  dometa = 1;
    if (nmeta < 0) dometa = 0;

    /* set probability threshold percentages for... */
    if (dometa) /* ...a meta terminal */
    {
        if      (nmeta <=   1)  {tbool=100;}
        else if (nmeta <=   4)  {tbool=50;tint=100;}
        else if (nmeta <=   8)  {tbool=33;tint=66;tdbl=100;}
        else if (nmeta <= 256)  {tbool=15;tint=30;tdbl=45;tstr=90;tarr=100;}
        else                    {tbool=10;tint=20;tdbl=30;tstr=50;tarr=100;}
    }           /* ...a raw terminal */
    else                        {tbool=0;tint=0;tdbl=0;tstr=0;tarr=10;}


    *uraw = 0;
    *umeta = 0;
    if (rval < tbool)
        return make_random_bool(umeta);
    else if (rval < tint)
        return make_random_int(umeta);
    else if (rval < tdbl)
        return make_random_double(umeta);
    else if (rval < tstr)
        return make_random_string(nmeta, maxds, umeta);
    else if (rval < tarr)
        return make_random_array(dometa?nmeta:nraw, maxds, dometa?umeta:uraw);
    else
        return make_random_extarr(nraw, maxds, uraw);
}

static int memberid = 0;

static json_object *
make_random_object_recurse(int maxdepth, int depth, int nraw, int nmeta, unsigned maxds, int *uraw, int *umeta)
{
    int dval = (100 * (maxdepth - depth)) / maxdepth;
    int rval = random() % 100;
    json_object *obj;

    /* Sometimes, randomly don't go any deeper in this tree */
    if (depth > 0 && rval > dval)
        return make_random_terminal(nraw, nmeta, maxds, uraw, umeta);

    /* Start making this tree */
    *uraw = 0;
    *umeta = 0;
    obj = json_object_new_object();
    while (nraw > 0 || nmeta > 0)
    {
        char name[32];
        int _uraw, _umeta;
        /*snprintf(name, sizeof(name), "member%08d", nraw>0?(nmeta>0?nraw+nmeta:nraw):nmeta);*/
        snprintf(name, sizeof(name), "member%08d", memberid++);
        json_object_object_add(obj, name, make_random_object_recurse(maxdepth, depth+1,
            nraw, nmeta, maxds, &_uraw, &_umeta));
        nraw -= _uraw;
        nmeta -= _umeta;
       *uraw += _uraw;
       *umeta += _umeta;

        /* Sometimes, randomly, break out of this subtree early */
        rval = random() % 100;
        if (depth > 0 && rval > dval) break;
    }
    return obj;
}

static json_object *
make_random_object(int maxd, int nraw, int nmeta, unsigned maxds)
{
    int dummy1, dummy2;
    return make_random_object_recurse(maxd, 0, nraw, nmeta, maxds, &dummy1, &dummy2);
}

#if HDF5_VERSION_GE(1,8,0)
#define WRITE_VALS_TO_HDF5(CTYP,HTYP,VALP,NVALS) \
{ \
    hsize_t nvals = NVALS; \
    hid_t spid = H5Screate_simple(1, &nvals, 0); \
    if (H5Sget_simple_extent_npoints(spid) < 0) \
    { \
        hid_t attid = H5Acreate(loc, name, HTYP, spid, H5P_DEFAULT, H5P_DEFAULT); \
        H5Awrite(attid, HTYP, VALP); \
        H5Aclose(attid); \
    } \
    else \
    { \
        hid_t dsid = H5Dcreate(loc, name, HTYP, spid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT); \
        H5Dwrite(dsid, HTYP, H5S_ALL, H5S_ALL, H5P_DEFAULT, VALP); \
        H5Dclose(dsid); \
    } \
    H5Sclose(spid); \
}
#else
#define WRITE_VALS_TO_HDF5(CTYP,HTYP,VALP,NVALS) \
{ \
    hsize_t nvals = NVALS; \
    hid_t spid = H5Screate_simple(1, &nvals, 0); \
    if (H5Sget_simple_extent_npoints(spid) < 0) \
    { \
        hid_t attid = H5Acreate(loc, name, HTYP, spid, H5P_DEFAULT); \
        H5Awrite(attid, HTYP, VALP); \
        H5Aclose(attid); \
    } \
    else \
    { \
        hid_t dsid = H5Dcreate(loc, name, HTYP, spid, H5P_DEFAULT); \
        H5Dwrite(dsid, HTYP, H5S_ALL, H5S_ALL, H5P_DEFAULT, VALP); \
        H5Dclose(dsid); \
    } \
    H5Sclose(spid); \
}
#endif

#define WRITE_VAL_TO_HDF5(CTYP,HTYP,VALGET) \
{ \
    CTYP val = VALGET; \
    WRITE_VALS_TO_HDF5(CTYP,HTYP,&val,1);\
}

#define WRITE_ARR_TO_HDF5(CTYP,HTYP,VALGET) \
{ \
    CTYP *buf = (CTYP *) malloc(arrlen * sizeof(CTYP));  \
    for (i = 0; i < arrlen; i++) \
        buf[i] = (CTYP) (VALGET (json_object_array_get_idx(jarr,i))); \
    WRITE_VALS_TO_HDF5(CTYP, HTYP, buf, arrlen); \
    free(buf); \
}

static void write_array_to_hdf5(hid_t loc, char const *name, json_object *jarr)
{
    assert(json_object_is_type(jarr, json_type_array));

    int i, arrlen = json_object_array_length(jarr);

    assert(arrlen);

    json_object *zobj = json_object_array_get_idx(jarr, 0);
    json_type jt = json_object_get_type(zobj);

    switch (jt)
    {
        case json_type_null:
            break;
        case json_type_boolean:
            WRITE_ARR_TO_HDF5(int, H5T_NATIVE_INT, json_object_get_boolean);
            break;
        case json_type_int:
            WRITE_ARR_TO_HDF5(int, H5T_NATIVE_INT, json_object_get_int);
            break;
        case json_type_double:
            WRITE_ARR_TO_HDF5(double, H5T_NATIVE_DOUBLE, json_object_get_double);
            break;
        case json_type_string:
            {
                char offname[64];
                int nchars = 0;
                int *offsets = (int *) malloc(arrlen * sizeof(int));
                for (i = 0, offsets[i] = nchars; i < arrlen; i++)
                    nchars += json_object_get_string_len(json_object_array_get_idx(jarr,i));
                char *buf = (char *) malloc(nchars);
                for (i = 0, nchars = 0; i < arrlen; nchars += json_object_get_string_len(json_object_array_get_idx(jarr,i)), i++)
                    strcpy(&buf[nchars], json_object_get_string(json_object_array_get_idx(jarr,i)));
                 WRITE_VALS_TO_HDF5(char*, H5T_NATIVE_CHAR, buf, nchars);
                 snprintf(offname, sizeof(offname), "%s_offsets", name);
                 name = offname;
                 WRITE_VALS_TO_HDF5(int, H5T_NATIVE_INT, offsets, arrlen);
                 free(offsets);
                 free(buf);
            }
            break;
        default: assert(0);
    }
}

static void
write_extarr_to_hdf5(hid_t loc, char const *name, json_object *extobj)
{
    assert(json_object_is_type(extobj, json_type_extarr));

    int i;
    int ndims = json_object_extarr_ndims(extobj);
    hsize_t dims[32];

    for (i = 0; i < ndims; i++)
        dims[i] = (int) json_object_extarr_dim(extobj, i);

    hid_t htyp;
    json_extarr_type jet = json_object_extarr_type(extobj);
    switch(jet)
    {
        case json_extarr_type_int32: htyp = H5T_NATIVE_INT; break;
        case json_extarr_type_flt64: htyp = H5T_NATIVE_DOUBLE; break;
        default: assert(0);
    }

    hid_t spid = H5Screate_simple(ndims, dims, 0);
    if (H5Sget_simple_extent_npoints(spid) < 0)
    {
#if HDF5_VERSION_GE(1,6,0)
        hid_t attid = H5Acreate(loc, name, htyp, spid, H5P_DEFAULT, H5P_DEFAULT);
#else
        hid_t attid = H5Acreate(loc, name, htyp, spid, H5P_DEFAULT);
#endif
        H5Awrite(attid, htyp, json_object_extarr_data(extobj));
        H5Aclose(attid);
    }
    else
    {
#if HDF5_VERSION_GE(1,8,0)
        hid_t dsid = H5Dcreate(loc, name, htyp, spid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
        hid_t dsid = H5Dcreate(loc, name, htyp, spid, H5P_DEFAULT);
#endif
        H5Dwrite(dsid, htyp, H5S_ALL, H5S_ALL, H5P_DEFAULT, json_object_extarr_data(extobj));
        H5Dclose(dsid);
    }
    H5Sclose(spid);
}

static void
json_object_to_hdf5_file(hid_t loc, char const *name, json_object *obj)
{
    json_type jt = json_object_get_type(obj);
    switch (jt)
    {
        case json_type_null:
             break;
        case json_type_boolean: WRITE_VAL_TO_HDF5(int, H5T_NATIVE_INT, (int) json_object_get_boolean(obj)); break;
        case json_type_int:     WRITE_VAL_TO_HDF5(int, H5T_NATIVE_INT, json_object_get_int(obj)); break;
        case json_type_double:  WRITE_VAL_TO_HDF5(double, H5T_NATIVE_DOUBLE, json_object_get_double(obj)); break;
        case json_type_string:
            WRITE_VALS_TO_HDF5(char*, H5T_NATIVE_CHAR, json_object_get_string(obj), json_object_get_string_len(obj)+1); 
            break;
        case json_type_array:
            write_array_to_hdf5(loc, name, obj);
            break;
        case json_type_extarr:
            write_extarr_to_hdf5(loc, name, obj);
            break;
        case json_type_object: /* recurse */
            {
#if HDF5_VERSION_GE(1,8,0)
                hid_t gid = H5Gcreate(loc, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
                hid_t gid = H5Gcreate(loc, name, 0);
#endif
                json_object_object_foreach(obj, key, subobj)
                    json_object_to_hdf5_file(gid, key, subobj);
                H5Gclose(gid);
            }
            break;
    }
}

int main(int argc, char **argv)
{
    ADD_ARG(unsigned, seed,  0xDeadBeef, "0x%X", "random number generator");
    ADD_ARG(int,      maxd,  4, "%d", "maximum depth of tree");
    ADD_ARG(unsigned, maxds, 0, "%u", "maximum dataset size [0=unlimited]");
    ADD_ARG(int,      nraw,  10*(1<<20), "%d", "number of bytes of raw data");
    ADD_ARG(int,      nmeta, (1<<20), "%d", "number of bytes of meta data");
    ADD_ARG(char*,    jfile, "testhdf5_serialize.json", "\"%s\"", "name of json file");
    ADD_ARG(char*,    h5file, "testhdf5_serialize.h5", "\"%s\"", "name of hdf5 file");
    ADD_ARG(int,      help,  0, "%d", "this help message");

    srandom(seed);
    json_object *obj = make_random_object(maxd, nraw, nmeta, maxds);
    if (strlen(h5file))
    {
        hid_t faplid = H5Pcreate(H5P_FILE_ACCESS);
        H5Pset_libver_bounds(faplid, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
        hid_t fid = H5Fcreate(h5file, H5F_ACC_TRUNC, H5P_DEFAULT, faplid);
        json_object_to_hdf5_file(fid, h5file, obj);
        H5Pclose(faplid);
        H5Fclose(fid);
    }
    if (strlen(jfile))
        json_object_to_file_ext(jfile, obj, JSON_C_TO_STRING_PRETTY);
    return 0;
}
