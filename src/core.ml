let core = fun x -> Printf.fprintf x "
#ifndef __CORE_H_
#define __CORE_H_
struct List;
struct Closure;
struct Value;
enum Type;
#include <string.h>
#include <stdlib.h>

typedef struct Value (*Lambda)();

struct Closure {
  Lambda lam;
  struct Value *env;
};

enum Type {
  INT,
  CHAR,
  LIST,
  PAIR%s
};

struct List {
  struct Value *list;
  int length;
};

struct Pair {
    struct Value *fst;
    struct Value *snd;
};

struct Value {
  union {
    double _float;
    char _char;
    struct Closure clo;
    struct List list;
    struct Pair pair;
    %s
  };
  struct Value *cell;
  enum Type t;
  int has_cell;
};

typedef struct Value Value;

Value
make_char (char c)
{
  Value v;
  v.t = CHAR;
  v._char = c;
  return v;
}

Value
make_closure(Lambda lam, Value *env, int env_len)
{
  Value v;
  v.clo.lam = lam;
  v.clo.env = malloc(env_len * sizeof(Value));
  memcpy(v.clo.env, env, env_len * sizeof(Value));
  return v;
}

Value
make_list(Value *l, int length)
{
  Value v;
  v.t = LIST;
  v.list.length = length;
  if (length)
    v.list.list = malloc(length * sizeof(Value));
  memcpy(v.list.list, l, length * sizeof(Value));
  return v;
}

Value
make_pair(Value fst, Value snd)
{
  Value v;
  v.t = PAIR;
  v.pair.fst = malloc(sizeof(Value));
  *(v.pair.fst) = fst;
  v.pair.snd = malloc(sizeof(Value));
  *(v.pair.snd) = snd;
  return v;
}


Value
make_int(double n)
{
  Value v;
  v.t = INT;
  v._float = n;
  return v;
}

Value
intern_eq (Value l1, Value l2)
{
  switch (l1.t) {
  case INT :
    if (l1._float != l2._float)
      return (make_int(0));
    break;
  case CHAR :
    if (l1._char != l2._char)
      return (make_int(0));
    break;
  case LIST :
    if ((l2.list.length) != (l1.list.length)) return (make_int(0));
    #pragma omp parallel for
      for (int i = 0; i < l2.list.length; i ++)
        if (!(intern_eq (*(l1.list.list + i), *(l2.list.list + i)))._float)
          return (make_int(0));
    break;
  case PAIR :
    if (!(intern_eq(*(l1.pair.fst), *(l2.pair.fst)))._float ||
        !(intern_eq(*(l1.pair.snd), *(l2.pair.snd)))._float)
        return (make_int(0));
    break;
    %s
  }
  return make_int(1);
}
%s

#endif
"
