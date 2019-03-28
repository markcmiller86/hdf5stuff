typedef struct __h5mount_info_t {
    char *mount_path;
    int flags;
} _h5mount_info_t;

int _h5mount(const char *type, const char *dir, int flags, void *data)
{
    /* if parallel, all tasks need to do this collectively */
    /* check if dir exists */
    /* H5Fcreate or H5Fopen */

    /* allocate resources for the mappings */
}

int _h5unmount(const char *dir, int flags)
{
}

int _h5mount_auto(char const *path)
{
}

int _h5mkdir(const char *path, mode_t mode)
{
    char const *h5path = 0;
    if (is_hdf5_fsiaf_path(path, &h5path))
    {
        H5Gcreate();
    }
    else
    {
        return mkdir();
    }
}

int _h5chdir(const char *path)
{
    char const *h5path = 0;
    if (is_hdf5_fsiaf_path(path, &h5path))
    {
    }
    else
    {
        return chdir();
    }
}

int _h5open(const char *path, int oflag, ...)
{
    char const *h5path = 0;
    if (is_hdf5_fsiaf_path(path, &h5path))
    {
        /* acquire int "fd" to map this dataset to */
        /* extendible, 1D, 4k chunks */
        fdmap[fd] = H5Dopen(h5path...);
        return fd;
    }
    else
    {
        /* check if O_CREAT and need mode flags */
        /* make call to 2-arg open or 3-arg open */
    }
}

FILE * _h5fopen(const char * restrict path, const char * restrict mode)
{
    char const *h5path = 0;
    if (is_hdf5_fsiaf_path(path, &h5path))
    {
        /* acquire int "fd" to map this dataset to */
        /* extendible, 1D, 4k chunks */
        fdmap[fd] = H5Dopen(h5path...);
        return fd;
    }
    else
    {
        /* check if O_CREAT and need mode flags */
        /* make call to 2-arg open or 3-arg open */
    }
}

int _h5write(int fd, void *buf, size_t nbytes)
{
    if (is_hdf5_fsiaf_fd(fd))
    {
        H5Dwrite(fdmap[fd], );
    }
    else
    {
        return write(fd, buf, nbytes);
    }
}

int _h5close(int fd)
{
    if (is_hdf5_fsiaf_fd(fd))
    {
        H5Dclose(fdmap[fd]);
        /* free up fd slot */
    }
    else
    {
        return close(fd);
    }
}
