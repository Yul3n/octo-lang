let core = fun x -> Printf.fprintf x "
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
struct List;
struct Closure;
struct Value;
enum Type;

typedef struct Value (*Lambda)();

struct Closure {
  Lambda lam;
  struct Value *env;
};

enum Type {
  INT,
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
    long     _int;
    struct Closure clo;
    struct List list;
    struct Pair pair;
    %s
  };
  struct Value *cell;
  enum Type t;
};

typedef struct Value Value;

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
make_int(long n)
{
  Value v;
  v.t = INT;
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
lambda_mod (Value *env, Value n)
{
    return make_int((*env)._int %% n._int);
}

Value
mod (Value *env, Value n, int len)
{
    Value *tenv = malloc((len + 1) * sizeof(Value));
    memcpy (tenv + 1, env, len);
    *tenv = n;
    return (make_closure(lambda_mod, tenv, len + 1));
}

Value
lambda_cons(Value *env, Value n)
{
    Value v;
    v.t = LIST;
    v.list.list = malloc((n.list.length + 1) * sizeof(Value));
    memcpy(v.list.list, env, sizeof(Value));
    memcpy(v.list.list + 1, n.list.list, n.list.length * sizeof(Value));
    v.list.length = n.list.length + 1;
    return v;
}

Value
cons (Value *env, Value n, int len)
{
    Value *tenv = malloc((len + 1) * sizeof(Value));
    memcpy (tenv + 1, env, len * sizeof(Value));
    *tenv = n;
    return (make_closure(lambda_cons, tenv, len + 1));
}

Value
lambda_union(Value *env, Value n)
{
  Value v;
  v.t = LIST;
  v.list.list = malloc ((n.list.length + ((*(env)).list.length)) * sizeof(Value));
  memcpy(v.list.list, ((*(env)).list.list), ((*(env)).list.length) * sizeof(Value));
  memcpy(v.list.list + ((*(env)).list.length), n.list.list, n.list.length * sizeof(Value));
  v.list.length = (*(env)).list.length + n.list.length;
  return v;
}

Value
octo_union (Value *env, Value n, int len)
{
    Value *tenv = malloc((len + 1) * sizeof(Value));
    memcpy (tenv + 1, env, len * sizeof(Value));
    *tenv = n;
    return (make_closure(lambda_union, tenv, len + 1));
}

Value
lambda_index (Value *env, Value n)
{
  if ((n._int >= ((*(env)).list.length)) || (n._int < 0)){
    printf (\"Error: invalid array index. Size of the array %%li, index: %%d.\\n\",
    n._int, ((*(env)).list.length));
    exit (1);
  } else return *((*(env)).list.list + n._int);
}

Value
ind (Value *env, Value n, int len)
{
    Value *tenv = malloc((len + 1) * sizeof(Value));
    memcpy (tenv + 1, env, len * sizeof(Value));
    *tenv = n;
    return (make_closure(lambda_index, tenv, len + 1));
}

Value
octo_head (Value *env, Value n, int len)
{
  if (n.list.length == 0) {
    puts(\"Using head on an empty list.\");
    exit (1);
  }
  return(*(n.list.list));
}

Value
octo_tail (Value *env, Value n, int len)
{
  if (n.list.length == 0) {
    puts(\"Using tail on an empty list.\");
    exit (1);
  }
  if (n.list.length == 1)
    return (make_list(NULL, 0));
  return(make_list(n.list.list + 1, n.list.length - 1));
}


Value
octo_fst (Value *env, Value n, int len)
{
    return(*(n.pair.fst));
}

Value
octo_snd (Value *env, Value n, int len)
{
    return(*(n.pair.snd));
}


Value
intern_eq (Value l1, Value l2)
{
  switch (l1.t) {
  case INT :
    if (l1._int != l2._int)
      return (make_int(0));
    break;
  case LIST :
    if ((l2.list.length) != (l1.list.length)) return (make_int(0));
    for (int i = 0; i < l2.list.length; i ++)
      if (!(intern_eq (*(l1.list.list + i), *(l2.list.list + i)))._int)
        return (make_int(0));
    break;
  case PAIR :
    if (!(intern_eq(*(l1.pair.fst), *(l2.pair.fst)))._int ||
        !(intern_eq(*(l1.pair.snd), *(l2.pair.snd)))._int)
        return (make_int(0));
    break;
    %s
  }
  return make_int(1);
}
%s
"
