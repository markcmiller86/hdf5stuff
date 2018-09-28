// Hdf5TypePathLeak
// 2016/3/28
//
// This test program demonstrates unbounded growth in the HDF5 type conversion path table when
// reading a file that contains a vlen datatype.
//
// Note that for this program to display the number of paths in the path table, you must add a
// symbol to H5T.c after the declaration of H5T_g to export the address of the allocated paths
// counter:
// H5_DLLVAR int* H5Tnpaths = &H5T_g.npaths;
// In my copy of the source I put it on line #523, but I guess that may vary by version.
//
// The underlying problems are three-fold:
// 1. Type conversion paths for compound and vlen datatypes are cached in a global table, but they
//    are never removed.
// 2. Type conversion attempts to treat vlen datatypes as unique if they are defined in different
//    files.
// 3. The file association of a vlen datatype is saved as a pointer in the type information, which
//    is allowed to dangle when the file is closed.
//
// My application, which is user-interactive, may open hundreds or thousands of hdf5 files during
// the lifetime of the process. Each of these files may contain one or more datasets contianing
// compound types with vlen fields. Because of (1), each time I open a new file and read its
// contents, new entries are added to the type conversion table. (The table contents are tracked by
// the H5T_g static struct defined in H5T.c.)
//
// Because of (2), the conversions for the types in a file go stale as soon as that file is closed.
// This is checked in H5T_cmp() in the following block (H5T.c:4173):
//
// /* Don't allow VL types in different files to compare as equal */
// if (dt1->shared->u.vlen.f < dt2->shared->u.vlen.f)
//    HGOTO_DONE(-1);
// if (dt1->shared->u.vlen.f > dt2->shared->u.vlen.f)
//    HGOTO_DONE(1);
//
// I tried to find a way to manually clear out entries from this table. There is a function
// provided for that purpose, H5Tunregister(pers, name, src_id, dst_id, func), but it does not work
// for compound types because of the way that the pers parameter is handled. If I pass
// H5T_PERS_HARD, no compound type conversions are removed because H5T_path_t.is_hard is set to
// false by H5T_path_find() when it falls back on the compound->compound soft conversion and
// generates a new path. Alternately, if I pass H5T_PERS_DONTCARE or H5T_PERS_SOFT, H5Tunregister()
// removes the default compound->compound soft conversion and I can't read any more datasets
// because the library can't create conversion paths for them.
//
// (1) and (2) together are enough to create unbounded memory growth, but diagnosing the problem is
// made harder by (3). If you consider dt1->shared->u.vlen.f in the code snippet above, the field
// f is a raw H5F_t*. This pointer is assigned in H5T__vlen_set_loc(), which is called when the
// dataset is opened. When the file is closed, the pointer dangles. The dangling pointer doesn't
// cause a crash (at least not in this case), because H5T_cmp() uses the pointer value as an
// identity but does not dereference it.
//
// However, the dangling pointer does make it difficult to diagnose the problem, because you would
// expect a trivial reproduction to look like this:
// A. Open a file with a suitable dataset.
// B. Open the dataset.
// C. Read the dataset (causing the conversion path to be allocated).
// D. Close the datset.
// E. Close the file.
// F. Look at the number of cached conversion paths.
// G. Repeat A-F and watch the number of conversion paths grow.
//
// The trivial reproduction doesn't work because when the file is closed, the H5F_t* is put on a
// free list and re-used for the next opened file. Because the memory allocation was re-used, the
// pointer check in H5T_cmp() passes.
//
// Therefore, to prove the path leak, I need to add two more things:
// * Call H5set_free_list_limits(0, 0, 0, 0, 0, 0) to disable the free lists.
// * Allocate sizeof(H5F_t) after closing the file on each iteration. This prevents the system
//   allocator from giving back the same pointer for subsequent requests, simulating the use of
//   the library in a larger program in which the allocator will be servicing other requests.
//   (On Windows, at least, malloc() is backed by the default process heap.)

#include <stdio.h>
#include <inttypes.h>
#include <signal.h>
#include <time.h>

#define H5_BUILT_AS_DYNAMIC_LIB 1
#include <hdf5.h>

// This must be added to the library to export the size of the path table; see above comments.
H5_DLLVAR int* H5Tnpaths;

// I just need this defined to get its size.
struct H5F_t {
    char *open_name;
    char *actual_name;
    char *extpath;
    void *shared;
    unsigned nopen_objs;
    void *obj_count;
    hid_t file_id;
    hbool_t closing;
    struct H5F_t *parent;
    unsigned nmounts;
};

void WriteFile(char *filename, char *datasetname, char *fieldname, hvl_t *data)
{
    hid_t file = H5Fcreate(filename, H5F_ACC_EXCL, H5P_DEFAULT, H5P_DEFAULT);

    hid_t vlenFileType = H5Tvlen_create(H5T_STD_I64LE);
    hid_t vlenMemoryType = H5Tvlen_create(H5T_NATIVE_INT64);

    hid_t compoundFileType = H5Tcreate(H5T_COMPOUND, sizeof(hvl_t));
    H5Tinsert(compoundFileType, fieldname, 0, vlenFileType);

    hid_t compoundMemoryType = H5Tcreate(H5T_COMPOUND, sizeof(hvl_t));
    H5Tinsert(compoundMemoryType, fieldname, 0, vlenMemoryType);

    hsize_t dimensions[1] = { 1 };
    hid_t space = H5Screate_simple(1, dimensions, dimensions);
    hid_t dataset = H5Dcreate(
        file,
        datasetname,
        compoundFileType,
        space,
        H5P_DEFAULT,
        H5P_DEFAULT,
        H5P_DEFAULT);
    H5Sselect_all(space);

    H5Dwrite(dataset,
        compoundMemoryType,
        space,
        space,
        H5P_DEFAULT,
        data);

    H5Dclose(dataset);
    H5Sclose(space);
    H5Tclose(compoundMemoryType);
    H5Tclose(compoundFileType);
    H5Tclose(vlenMemoryType);
    H5Tclose(vlenFileType);
    H5Fclose(file);
}

void CheckRead(hid_t dataset, char *fieldname, hvl_t* written)
{
    hid_t space = H5Dget_space(dataset);
    H5Sselect_all(space);

    hid_t vlenMemoryType = H5Tvlen_create(H5T_NATIVE_INT64);
    hid_t compoundMemoryType = H5Tcreate(H5T_COMPOUND, sizeof(hvl_t));
    H5Tinsert(compoundMemoryType, fieldname, 0, vlenMemoryType);

    hvl_t read;
    H5Dread(dataset, compoundMemoryType, space, space, H5P_DEFAULT, &read);
    if (written->len != read.len)
    {
        printf(
            "Wrote %" PRIu32 " integers; read %" PRIu32 ".\n",
            written->len,
            read.len);
    }
    for (size_t i = 0; i < read.len; ++i)
    {
        int64_t expected = ((int64_t*)written->p)[i];
        int64_t found = ((int64_t*)read.p)[i];
        if (expected != found)
        {
            printf(
                "Wrote %" PRId64 " at index %" PRIu32 "; read %" PRId64 ".\n",
                expected,
                i,
                found);
        }
    }

    H5Dvlen_reclaim(compoundMemoryType, space, H5P_DEFAULT, &read);
    H5Tclose(compoundMemoryType);
    H5Tclose(vlenMemoryType);
    H5Sclose(space);
}

// Let the main loop terminate cleanly when the program is interrupted.
volatile sig_atomic_t terminated;
void SignalHandler(int signo) { terminated = 1; }

int main(int argc, char *argv[])
{
    signal(SIGINT, SignalHandler);
    H5set_free_list_limits(0, 0, 0, 0, 0, 0);

    char *filename = argv[1];
    remove(filename);

    char *datasetname = "compound";
    char *fieldname = "integers";

    int64_t integers[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

    hvl_t written;
    written.p = integers;
    written.len = 10;
    WriteFile(filename, datasetname, fieldname, &written);

    time_t then, now;
    time(&then);

    while (!terminated)
    {
        hid_t file = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT);
        hid_t dataset = H5Dopen(file, datasetname, H5P_DEFAULT);
        CheckRead(dataset, fieldname, &written);
        H5Dclose(dataset);
        H5Fclose(file);
        H5allocate_memory(sizeof(struct H5F_t), 0);

        // I want to rate limit this to 1 Hz, otherwise it just spams the console.
        time(&now);
        if (now != then)
        {
            printf("Type conversion path table size: %i\n", *H5Tnpaths);
            then = now;
        }
    }

    return 0;
}
