#ifndef MT_TYPES_H
#define MT_TYPES_H 1

#include <stdlib.h>
#include <stdint.h>

typedef enum {
  MT_GROUND_TYPE,
  MT_TUPLE_TYPE,
  MT_RECORD_TYPE,
  MT_LAMBDA_TYPE,
  MT_UNION_TYPE,
  MT_INTERSECTION_TYPE
} mt_TypeKind;

struct mt_Type_t;

typedef struct mt_Type_t {
    mt_TypeKind kind;
    int num_inner_types;
    struct mt_Type_t* inner_types;
} mt_Type;

extern mt_Type mt_Int;
extern mt_Type mt_Float;

mt_Type mt_types_make_union(mt_Type* types);

#endif // MT_TYPES_H
