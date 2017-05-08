#include "kernel.h"

mt_Value mt_add_i(mt_Value a, mt_Value b) {
    return INT2VALUE(VALUE2INT(a) + VALUE2INT(b));
}

mt_Value mt_add_n(mt_Value a, mt_Value b) {
    return DOUBLE2VALUE(VALUE2NUM(a) + VALUE2NUM(b));
}

mt_Value mt_subtract_i(mt_Value a, mt_Value b) {
    return INT2VALUE(VALUE2INT(a) - VALUE2INT(b));
}

mt_Value mt_subtract_n(mt_Value a, mt_Value b) {
    return DOUBLE2VALUE(VALUE2NUM(a) - VALUE2NUM(b));
}

mt_Value mt_print_n(mt_Value n) {
    if (TYPEOF(n) == MT_INT) printf("%d", VALUE2INT(n));
    else printf("%#f", VALUE2DOUBLE(n));

    return MT_NIL_VALUE;
}

mt_Value mt_print_s(mt_Value s) {
    printf("%s", VALUE2STR(s));
    return MT_NIL_VALUE;
}
