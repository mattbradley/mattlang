#ifndef MT_KERNEL_H
#define MT_KERNEL_H 1

#include <stdlib.h>
#include <stdint.h>

#define MT_NIL (0)
#define MT_BOOL (1)
#define MT_INT (2)
#define MT_FLOAT (3)
#define MT_STRING (4)

#define TYPEOF(val) (val.type)

#define NIL_VALUE ((mt_Value){.type = MT_NIL, .data = (mt_Data){.i = 0}})

#define VALUE2BOOL(val) (val.data.i)
#define VALUE2INT(val) (val.data.i)
#define VALUE2DOUBLE(val) (val.data.d)
#define VALUE2NUM(val) (TYPEOF(val) == MT_INT ? val.data.i : val.data.d)
#define VALUE2STR(val) (val.data.s)
#define VALUE2FIELDS(val) (val.data.fields)

#define BOOL2VALUE(b) ((mt_Value){.type = MT_BOOL, .data = (mt_Data){.i = b}})
#define INT2VALUE(num) ((mt_Value){.type = MT_INT, .data = (mt_Data){.i = num}})
#define DOUBLE2VALUE(num) ((mt_Value){.type = MT_FLOAT, .data = (mt_Data){.d = num}})
#define STR2VALUE(str) ((mt_Value){.type = MT_STRING, .data = (mt_Data){.s = str}})

#define ALLOC_FIELDS(size) (malloc(sizeof(mt_Value) * size))
#define MAKE_VALUE(type_id, fields_ptr) ((mt_Value){.type = type_id, .data = (mt_Data){.fields = fields_ptr}})

typedef uint64_t mt_TypeId;

struct mt_ValueTag;

typedef union {
    int i;
    double d;
    char* s;
    struct mt_ValueTag* fields;
} mt_Data;

typedef struct mt_ValueTag {
    mt_TypeId type;
    mt_Data data;
} mt_Value;

mt_Value mt_not(mt_Value b);
mt_Value mt_and(mt_Value a, mt_Value b);
mt_Value mt_or(mt_Value a, mt_Value b);

#include "io.h"
#include "number.h"

#endif // MT_KERNEL_H
