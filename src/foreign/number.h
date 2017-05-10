#ifndef MT_NUMBER_H
#define MT_NUMBER_H 1

#include "kernel.h"

mt_Value mt_unary_plus(mt_Value);
mt_Value mt_unary_minus(mt_Value);
mt_Value mt_add_i(mt_Value, mt_Value);
mt_Value mt_add_n(mt_Value, mt_Value);
mt_Value mt_subtract_i(mt_Value, mt_Value);
mt_Value mt_subtract_n(mt_Value, mt_Value);
mt_Value mt_less_than(mt_Value, mt_Value);
mt_Value mt_greater_than(mt_Value, mt_Value);
mt_Value mt_less_than_equal(mt_Value, mt_Value);
mt_Value mt_greater_than_equal(mt_Value, mt_Value);

#endif // MT_NUMBER_H
