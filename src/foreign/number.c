#include "number.h"

mt_Value mt_unary_plus(mt_Value n) {
  if (TYPEOF(n) == MT_INT) return INT2VALUE(+VALUE2INT(n));
  else return DOUBLE2VALUE(+VALUE2DOUBLE(n));
}

mt_Value mt_unary_minus(mt_Value n) {
  if (TYPEOF(n) == MT_INT) return INT2VALUE(-VALUE2INT(n));
  else return DOUBLE2VALUE(-VALUE2DOUBLE(n));
}

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

mt_Value mt_less_than(mt_Value a, mt_Value b) {
  return BOOL2VALUE(VALUE2NUM(a) < VALUE2NUM(b));
}

mt_Value mt_greater_than(mt_Value a, mt_Value b) {
  return BOOL2VALUE(VALUE2NUM(a) > VALUE2NUM(b));
}

mt_Value mt_less_than_equal(mt_Value a, mt_Value b) {
  return BOOL2VALUE(VALUE2NUM(a) <= VALUE2NUM(b));
}

mt_Value mt_greater_than_equal(mt_Value a, mt_Value b) {
  return BOOL2VALUE(VALUE2NUM(a) >= VALUE2NUM(b));
}
