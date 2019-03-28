#define open(P,F,...) _h5fs_open(P,F,__VA_ARGS__)
#define close(FD) _h5fs_close(FD)
#define write(FD,BUF,NBYTE) _h5fs_write(FD,BUF,NBYTE)
#define read(FD,BUF,NBYTE) _h5fs_read(FD,BUF,NBYTE)

     int
     mount(const char *type, const char *dir, int flags, void *data);

     int
     fmount(const char *type, int fd, int flags, void *data);

     int
     unmount(const char *dir, int flags);

     int
     chdir(const char *path);

     int
     fchdir(int fildes);



     int
     mkdir(const char *path, mode_t mode);

     int
     mkdirat(int fd, const char *path, mode_t mode);

     char *
     getcwd(char *buf, size_t size);

     char *
     getwd(char *buf);

     int
     dup(int fildes);

     int
     dup2(int fildes, int fildes2);

     int
     fgetpos(FILE *restrict stream, fpos_t *restrict pos);

     int
     fseek(FILE *stream, long offset, int whence);

     int
     fseeko(FILE *stream, off_t offset, int whence);

     int
     fsetpos(FILE *stream, const fpos_t *pos);

     long
     ftell(FILE *stream);

     off_t
     ftello(FILE *stream);

     void
     rewind(FILE *stream);

     off_t
     lseek(int fildes, off_t offset, int whence);


     int
     creat(const char *path, mode_t mode);

     FILE *
     fopen(const char * restrict path, const char * restrict mode);

     FILE *
     fdopen(int fildes, const char *mode);

     FILE *
     freopen(const char *path, const char *mode, FILE *stream);

     FILE *
     fmemopen(void *restrict *buf, size_t size, const char * restrict mode);


     int
     fstat(int fildes, struct stat *buf);

     int
     lstat(const char *restrict path, struct stat *restrict buf);

     int
     stat(const char *restrict path, struct stat *restrict buf);

     int
     fstatat(int fd, const char *path, struct stat *buf, int flag);

TRANSITIIONAL SYNOPSIS (NOW DEPRECATED)
     int
     fstat64(int fildes, struct stat64 *buf);

     int
     lstat64(const char *restrict path, struct stat64 *restrict buf);

     int
     stat64(const char *restrict path, struct stat64 *restrict buf);
