#include "kernel.h"

mt_Value mt_not(mt_Value v) {
  return BOOL2VALUE(!VALUE2BOOL(v));
}

mt_Value mt_and(mt_Value a, mt_Value b) {
  return BOOL2VALUE(VALUE2BOOL(a) && VALUE2BOOL(b));
}

mt_Value mt_or(mt_Value a, mt_Value b) {
  return BOOL2VALUE(VALUE2BOOL(a) || VALUE2BOOL(b));
}
