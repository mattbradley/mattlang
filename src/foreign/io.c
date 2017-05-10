#include "io.h"
#include <stdio.h>

mt_Value mt_puts_n(mt_Value n) {
    if (TYPEOF(n) == MT_INT) printf("%d\n", VALUE2INT(n));
    else printf("%#f\n", VALUE2DOUBLE(n));

    return NIL_VALUE;
}

mt_Value mt_puts_s(mt_Value s) {
    printf("%s\n", VALUE2STR(s));
    return NIL_VALUE;
}
