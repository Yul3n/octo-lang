
#ifndef __CORE_H_
#define __CORE_H_

#include <string.h>
#include <stdlib.h>
struct Int ;
struct Closure ;
union Value ;

enum Tag { INT, CLOSURE } ;

typedef union Value (*Lambda)()  ;

struct Int {
  enum Tag t ;
  int value ;
} ;

struct Closure {
  enum Tag t;
  Lambda lam;
  union Value *env;
};

union Value {
  enum   Tag t;
  int     _int;

  struct Closure clo;
};

typedef union Value Value ;

Value
make_closure(Lambda lam, Value *env, int env_len)
{
  Value v;
  v.clo.t = CLOSURE;
  v.clo.lam = lam;
  v.clo.env = malloc(env_len * sizeof(Value));
  memcpy(v.clo.env, env, env_len * sizeof(Value));
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
lambda_sum(Value *env, Value n)
{
  return make_int((*env)._int + n._int);
}

Value
sum (Value *env, Value n, int len)
{
  Value *tenv = malloc((sizeof(env) + 1) * sizeof(Value));
  memcpy (tenv + 1, env, len);
  *tenv = n;
  return (make_closure(lambda_sum, tenv, len + 1));
}

Value
lambda_dif(Value *env, Value n)
{
  return make_int((*env)._int - n._int);
}

Value
dif (Value *env, Value n, int len)
{
  Value *tenv = malloc((sizeof(env) + 1) * sizeof(Value));
  memcpy (tenv + 1, env, len);
  *tenv = n;
  return (make_closure(lambda_dif, tenv, len + 1));
}

Value
lambda_div(Value *env, Value n)
{
  return make_int((*env)._int / n._int);
}

Value
dv (Value *env, Value n, int len)
{
  Value *tenv = malloc((sizeof(env) + 1) * sizeof(Value));
  memcpy (tenv + 1, env, len);
  *tenv = n;
  return (make_closure(lambda_div, tenv, len + 1));
}

Value
lambda_tim(Value *env, Value n)
{
  return make_int(((*env)._int) * (n._int));
}

Value
tim (Value *env, Value n, int len)
{
  Value *tenv = malloc((sizeof(env) + 1) * sizeof(Value));
  memcpy (tenv + 1, env, len);
  *tenv = n;
  return (make_closure(lambda_tim, tenv, len + 1));
}

#endif // __CORE_H_

