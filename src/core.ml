let core_pre ="
#ifndef __CORE_H_
#define __CORE_H_

#include <string.h>
#include <stdlib.h>
struct List
struct Closure ;
union Value ;

typedef union Value (*Lambda)()  ;

struct Closure {
  Lambda lam;
  union Value *env;
};

struct List {
  union Value *list;
  int length;
};

union Value {
  int     _int;
"

let core_seq = "
  struct Closure clo;
  struct List list;
};

typedef union Value Value;

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
  v.list.list = malloc(length * sizeof(Value));
  memcpy(v.list.list, l, length * sizeof(Value));
  v.list.length = length;
  return v;
}

Value
make_int(int n)
{
  Value v;
  v._int = n;
  return v;
}

Value
lambda_sum (Value *env, Value n)
{
  return make_int((*env)._int + n._int);
}

Value
sum (Value *env, Value n, int len)
{
  Value *tenv = malloc((len + 1) * sizeof(Value));
  memcpy (tenv + 1, env, len);
  *tenv = n;
  return (make_closure(lambda_sum, tenv, len + 1));
}

Value
lambda_dif (Value *env, Value n)
{
  return make_int((*env)._int - n._int);
}

Value
dif (Value *env, Value n, int len)
{
  Value *tenv = malloc((len + 1) * sizeof(Value));
  memcpy (tenv + 1, env, len);
  *tenv = n;
  return (make_closure(lambda_dif, tenv, len + 1));
}

Value
lambda_div (Value *env, Value n)
{
    return make_int((*env)._int / n._int);
}

Value
dv (Value *env, Value n, int len)
{
    Value *tenv = malloc((len + 1) * sizeof(Value));
    memcpy (tenv + 1, env, len);
    *tenv = n;
    return (make_closure(lambda_div, tenv, len + 1));
}

Value
lambda_tim (Value *env, Value n)
{
    return make_int(((*env)._int) * (n._int));
}

Value
tim (Value *env, Value n, int len)
{
    Value *tenv = malloc((len + 1) * sizeof(Value));
    memcpy (tenv + 1, env, len);
    *tenv = n;
    return (make_closure(lambda_tim, tenv, len + 1));
}

Value
lambda_cons(Value *env, Value n)
{
    Value v;
    v.list.list = malloc((n.list.length + 1) * sizeof(Value));
    memcpy(v.list.list, env, sizeof(Value));
    memcpy(v.list.list + 1, n.list.list, n.list.length);
    return v;
}

Value
cons (Value *env, Value n, int len)
{
    Value *tenv = malloc((len + 1) * sizeof(Value));
    memcpy (tenv + 1, env, len);
    *tenv = n;
    return (make_closure(lambda_cons, tenv, len + 1));
}

Value
lambda_union()

Value
octo_union (Value *env, Value n, int len)
{
    Value *tenv = malloc((len + 1) * sizeof(Value));
    memcpy (tenv + 1, env, len);
    *tenv = n;
    return (make_closure(lambda_union, tenv, len + 1));
}

#endif // __CORE_H_
"
