#include <stdint.h>
#include <stdio.h>
#include <math.h>

#ifndef MT_KERNEL_H
#define MT_KERNEL_H 1

#define MT_NIL (0)
#define MT_BOOL (1)
#define MT_INT (2)
#define MT_FLOAT (3)
#define MT_STRING (4)

#define MT_NIL_VALUE ((mt_Value){.type = MT_NIL, .data = (mt_Data){.i = 0}})

#define TYPEOF(val) (val.type)

#define VALUE2INT(val) (val.data.i)
#define VALUE2DOUBLE(val) (val.data.d)
#define VALUE2NUM(val) (TYPEOF(val) == MT_INT ? val.data.i : val.data.d)
#define VALUE2STR(val) (val.data.s)

#define INT2VALUE(num) ((mt_Value){.type = MT_INT, .data = (mt_Data){.i = num}})
#define DOUBLE2VALUE(num) ((mt_Value){.type = MT_FLOAT, .data = (mt_Data){.d = num}})
#define STR2VALUE(str) ((mt_Value){.type = MT_STRING, .data = (mt_Data){.s = str}})

typedef uint32_t mt_TypeId;

typedef union {
    int i;
    double d;
    char* s;
} mt_Data;

typedef struct {
    mt_TypeId type;
    mt_Data data;
} mt_Value;

mt_Value mt_add_i(mt_Value a, mt_Value b);
mt_Value mt_add_n(mt_Value a, mt_Value b);
mt_Value mt_subtract_i(mt_Value a, mt_Value b);
mt_Value mt_subtract_n(mt_Value a, mt_Value b);
mt_Value mt_print_n(mt_Value n);
mt_Value mt_print_s(mt_Value s);

#endif /* MT_KERNEL_H */
