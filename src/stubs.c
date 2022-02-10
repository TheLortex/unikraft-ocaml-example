#include <sys/time.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>

CAMLprim 
value caml_gettimeofday() {
    struct timeval current_time;
    gettimeofday(&current_time, NULL);

    return caml_copy_double(((double)(current_time.tv_sec)) + (((double)current_time.tv_usec)/1000000.));
}

CAMLprim 
value caml_sleep(value secs) {
    double f = Double_val(secs);
    usleep((int)(1000000*f));
    return Val_unit;
}
