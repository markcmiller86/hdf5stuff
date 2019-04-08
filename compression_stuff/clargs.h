#ifndef CLARGS_H
#define CLARGS_H
/*
This file was copied from H5Z-ZFP and modified for purposes here.

This tool will generate a binary data file of 2 or 3D arrays of
double precision floating point data with certain properties as
specified by the command-line arguments.
*/

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Increase or decrease for terminal width */
#define HELP_WIDTH 72

/* convenience macro to handle command-line args and help */
#define HANDLE_SEP(SEPSTR)                                      \
{                                                               \
    char tmpstr[HELP_WIDTH];                                    \
    int len = snprintf(tmpstr, sizeof(tmpstr), "\n%s...", #SEPSTR);\
    printf("    %*s\n",(int)sizeof(tmpstr)-len,tmpstr);         \
}

#define HANDLE_ARG(A,PARSEA,PRINTA,HELPSTR)                     \
{                                                               \
    int i;                                                      \
    char tmpstr[HELP_WIDTH];                                    \
    int len;                                                    \
    int len2 = strlen(#A)+1;                                    \
    for (i = 1; i < argc; i++)                                  \
    {                                                           \
        if (strcasestr(argv[i],"help") &&                       \
            !strncasecmp(#A,"help",4))                          \
        {                                                       \
            return 0;                                           \
        }                                                       \
        else if (!strncmp(argv[i], #A"=", len2))                \
        {                                                       \
            A = PARSEA;                                         \
            break;                                              \
        }                                                       \
    }                                                           \
    len = snprintf(tmpstr, sizeof(tmpstr), "%s=" PRINTA, #A, A);\
    printf("    %s%*s\n",tmpstr,(int)sizeof(tmpstr)-len,#HELPSTR);\
}


/* convenience macro to handle errors */
#define ERROR(FNAME)                                              \
do {                                                              \
    int _errno = errno;                                           \
    fprintf(stderr, #FNAME " failed at line %d, errno=%d (%s)\n", \
        __LINE__, _errno, _errno?strerror(_errno):"ok");          \
    return 1;                                                     \
} while(0)

#define NAME_LEN 256

/* Examples of use of these command-line argument macros */
#if 0
    /* initialize default values for vars which hold command-line args */
    char *ofile = (char *) calloc(NAME_LEN,sizeof(char));
    int ndims = 3, n0 = 251;
    double jitter = 0.000;
    int help = 0;

    /* default values for string arguments */
    strcpy(ofile, "default_name.dat");

    /* dataset arguments */
    HANDLE_SEP(Data Generation Arguments)
    HANDLE_ARG(ofile,strndup(argv[i]+len2,NAME_LEN), "\"%s\"",set output filename);
    HANDLE_ARG(ndims,(int)strtol(argv[i]+len2,0,10), "%d",number dimensions in data);
    HANDLE_ARG(n0,(int)strtol(argv[i]+len2,0,10), "%d",size of dimension 0 (ndims>0));
    HANDLE_ARG(jitter,(double) strtod(argv[i]+len2,0),"%g",jitter abscissa to separable funcs);
    HANDLE_ARG(help,(int)strtol(argv[i]+len2,0,10),"%d",this help message); /* must be last for help to work */
#endif
#endif
