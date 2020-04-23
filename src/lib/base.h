#include "../core.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <omp.h>
#include <math.h>

char*
octo_str_to_c_str (Value s)
{
  char *str = malloc((s.list.length) * sizeof(char));
  for (int i = 0; i < s.list.length; i ++)
    *(str + i) = (*(s.list.list + i))._char;
  return str;
}

Value
c_str_to_octo_str (char *s)
{
  Value *str = malloc(strlen(s) * sizeof(Value));
  for (int i = 0; i < strlen(s); i ++)
    *(str + i) = make_char(*(s+i));
  return make_list(str, strlen(s));
}

Value
lambda_sum (Value *env, Value n)
{
  return make_int((*env)._float + n._float);
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
lambda_ddiv (Value *env, Value n)
{
  return make_int(((int) (*env)._float) / ((int) n._float));
}

Value
ddiv (Value *env, Value n, int len)
{
  Value *tenv = malloc((len + 1) * sizeof(Value));
  memcpy (tenv + 1, env, len);
  *tenv = n;
  return (make_closure(lambda_ddiv, tenv, len + 1));
}



Value
lambda_dif (Value *env, Value n)
{
  return make_int((*env)._float - n._float);
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
    return make_int((*env)._float / n._float);
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
    return make_int(((*env)._float) * (n._float));
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
    return make_int(fmod((*env)._float, n._float));
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
lambda_map (Value *env, Value n, int len)
{
    Value *l = malloc (n.list.length * sizeof(Value));
    #pragma omp parallel for
      for (int i = 0; i < n.list.length; i ++)
        *(l + i) = (*(env)).clo.lam((*(env)).clo.env, *(n.list.list + i), len + 1);
    return make_list(l, n.list.length);
}

Value
octo_map (Value *env, Value n, int len)
{
    Value *tenv = malloc((len + 1) * sizeof(Value));
    memcpy (tenv + 1, env, len);
    *tenv = n;
    return (make_closure(lambda_map, tenv, len + 1));
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
  if ((n._float >= ((*(env)).list.length)) || (n._float < 0)){
    printf ("Error: invalid array index. Size of the array %lf, index: %d.\n",
    n._float, ((*(env)).list.length));
    exit (1);
  } else return *((*(env)).list.list + (int) n._float);
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
    puts("Using head on an empty list.");
    exit (1);
  }
  return(*(n.list.list));
}

Value
octo_tail (Value *env, Value n, int len)
{
  if (n.list.length == 0) {
    puts("Using tail on an empty list.");
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
get_body (Value *env, Value n, int len)
{
    return (*(n.cell));
}

Value suml;
Value difl;
Value divl;
Value timl;
Value modl;
Value conl;
Value unil;
Value indl;
Value ddivl;
Value _head, _tail, _fst, _snd, get_b, _map, _char_code;

Value
char_code (Value *env, Value n, int len)
{
    return make_int(n._char);
}


Value
char_chr (Value *env, Value n, int len)
{
  if (n._float < 255)
    return make_char(n._float);
  else {
    puts("Invalid argument for char chr.");
    exit(1);
  }
}


Value _char_code;
Value _char_chr;

void
base_init ()
{
    difl = make_closure(dif, NULL, 0);
    modl = make_closure(mod, NULL, 0);
    divl = make_closure(dv, NULL, 0);
    timl = make_closure(tim, NULL, 0);
    suml = make_closure(sum, NULL, 0);
    unil = make_closure(octo_union, NULL, 0);
    conl = make_closure(cons, NULL, 0);
    indl = make_closure(ind, NULL, 0);
    ddivl = make_closure(ddiv, NULL, 0);
    get_b = make_closure(get_body, NULL, 0);
    _map = make_closure(octo_map, NULL, 0);
    _tail = make_closure(octo_tail, NULL, 0);
    _head = make_closure(octo_head, NULL, 0);
    _fst = make_closure(octo_fst, NULL, 0);
    _snd = make_closure(octo_snd, NULL, 0);
    _char_code = make_closure(char_code, NULL, 0);
}
