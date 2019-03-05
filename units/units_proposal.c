/*#include <hdf5.h>*/
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAGIC "h5UnIt"
#define VERSION "0.1"

/* Primal quantities and their default units are implied */

/* from numbers to string */
char *encode_unit(char const *units,
    unsigned int const num[7], unsigned int const den[7],
    double mult, double offset, double logbase, double logcoeff)
{
    /* format of string...
        units\0n0:n1:n2:n3:n4:n5:n6:d0:d1:d2:d3:d4:d5:d6:mult:offset:lb:lc:vno:magic
        3 chars each for num/den + ':' sep = 42
        4 doubles @ 18 chars/dbl           = 72
        version number                     =  3
        magic                              =  6
        units len                          =  variable
        embedded null                      =  1 */
    int n, n1;
    char *retval = 0;
    if (!units) return 0;

    n = strlen(units)+126;
    retval = malloc(n);
    assert(retval);
    n1 = snprintf(retval, n, "%s%c%6s:%3s"
        ":%02d:%02d:%02d:%02d:%02d:%02d:%02d"
        ":%02d:%02d:%02d:%02d:%02d:%02d:%02d"
        ":% 17g:% 17g:% 17g:% 17g",
        units, '\0', MAGIC, VERSION,
        num[0], num[1], num[2], num[3], num[4], num[5], num[6],
        den[0], den[1], den[2], den[3], den[4], den[5], den[6],
        mult, offset, logbase, logcoeff);
    assert(n1+1 == n);
    return retval;
}

/* from string to numbers */
void decode_unit(char const *unit_str,
    unsigned int num[7], unsigned int den[7],
    double *mult, double *offset, double *logbase, double *logcoeff)
{
    int n = strlen(unit_str);
    assert(!strncmp(&unit_str[n+1],MAGIC,6));
    n = sscanf(&unit_str[n+11],
        ":%d:%d:%d:%d:%d:%d:%d"
        ":%d:%d:%d:%d:%d:%d:%d"
        ":%lg:%lg:%lg:%lg",
        &num[0], &num[1], &num[2], &num[3], &num[4], &num[5], &num[6],
        &den[0], &den[1], &den[2], &den[3], &den[4], &den[5], &den[6],
        mult, offset, logbase, logcoeff);
    assert(n==18);
}

double xform_unit(double val, char const *src_units, char const *dst_units)
{
    int i;
    unsigned int const zeros[7] = {0,0,0,0,0,0,0};
    unsigned int src_num[7], src_den[7], dst_num[7], dst_den[7];
    unsigned int src_red[7], dst_red[7];
    double src_mult, src_offset, src_logbase, src_logcoeff;
    double dst_mult, dst_offset, dst_logbase, dst_logcoeff;

    /* decode src units */
    decode_unit(src_units, src_num, src_den, &src_mult, &src_offset, &src_logbase, &src_logcoeff);

    /* decode dst units */
    decode_unit(dst_units, dst_num, dst_den, &dst_mult, &dst_offset, &dst_logbase, &dst_logcoeff);

    /* reduce numerator and denominator of src and dst */
    for (i = 0; i < sizeof(src_num)/sizeof(src_num[0]); i++)
    {
        src_red[i] = src_num[i] - src_den[1];
        dst_red[i] = dst_num[i] - dst_den[1];
    }
    assert(!memcmp(src_red, dst_red, sizeof(src_red)));

#if 0
    /* Going from base is...
    val = dst_logcoeff * log(val) / log(dst_logbase) * dst_mult + dst_offset; */

    /* Going to base is reverse of above */

    /* convert from src units to standard units */
    val = src_logcoeff * pow(val, src_logbase) * src_mult + src_offset;

    /* convert from standard units to dst units */
    val = dst_logcoeff * log(val) / log(dst_logbase) * dst_mult + dst_offset;
#endif

#if 1
    if (src_logbase == 0 && dst_logbase == 0)
    {
        /* convert from src units to standard units */
        val = (val - src_offset) / src_mult;

        /* convert from standard units to dst units */
        val = val * dst_mult + dst_offset;
    }
    else
    {
        
    }
#endif

    return val;

}

#if 0
/* from string on group/dataset to numbers */
herr_t H5Dget_unit(hid_t locid);

/* from numbers to string on group/dataset */
/* how to handle compound type? */
herr_t H5Dput_unit(hid_t locid);

/* unit conversions on write */

/* unit conversions on read */

/* unit conversions in-situ */
#endif

int main()
{

    /* a velocity unit in meters/second */
   {
                           /* l,m,t,e,T,A,L */
       unsigned int num[7] = {1,0,0,0,0,0,0};
       unsigned int den[7] = {0,0,1,0,0,0,0};

       /* create some units */

       /* velocity in meters/sec */
       char *meps = encode_unit("meters/second", num, den, 1, 0, 0, 1);

       /* velocity in mph */
       char *mph = encode_unit("miles/hour", num, den, 2.23694, 0, 0, 1);
    }

    /* some temperature examples */
    {
                           /* l,m,t,e,T,A,L */
       unsigned int num[7] = {0,0,0,0,1,0,0};
       unsigned int den[7] = {0,0,0,0,0,0,0};
       double val = M_PI * 100; /* units of Kelvin */

       char *kelvin = encode_unit("Kelvin", num, den, 1, 0, 0, 1);
       char *farhenheit = encode_unit("Fahrenheit", num, den, (double)9/5, -459.67, 0, 1);
       printf("%g %s converts to %g %s\n", val, kelvin, xform_unit(val, kelvin, farhenheit), farhenheit);
    }

    /* planar angular values */
    {
                           /* l,m,t,e,T,A,L */
       unsigned int num[7] = {0,1,0,0,0,0,0};
       unsigned int den[7] = {0,1,0,0,0,0,0};
       double val = 90; /* units of degrees */

       char *rads = encode_unit("Radians", num, den, 1, 0, 0, 1);
       char *degs = encode_unit("Degrees", num, den, 180.0/M_PI, 0, 0, 1);
       printf("%g %s converts to %g %s\n", val, degs, xform_unit(val, degs, rads), rads);
    }

    /* solid angular values */
    {
                           /* l,m,t,e,T,A,L */
       unsigned int num[7] = {0,2,0,0,0,0,0};
       unsigned int den[7] = {0,2,0,0,0,0,0};
       double val = 10000; /* units of square degrees */

       char *strads = encode_unit("Steradians", num, den, 1, 0, 0, 1);
       char *sqdegs= encode_unit("Square Degrees", num, den, 180.0/M_PI*180.0/M_PI, 0, 0, 1);
       char *soldegs = encode_unit("Radians", num, den, acos(1.0-1/(2*M_PI)), 0, 0, 1);
       printf("%g %s converts to %g %s\n", val, sqdegs, xform_unit(val, sqdegs, strads), strads);
       printf("%g %s converts to %g %s\n", 3.04617, strads, xform_unit(3.04617, strads, sqdegs), sqdegs);
    }

    return 0;
}
