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
  enum Tag t ;
  Lambda lam ;
  int *env ;
} ;

union Value {
  enum Tag t ;
  struct Int n ;
  struct Closure clo ;
} ;

typedef union Value Value ;

static Value make_closure(Lambda lam, int *env, int env_len) {
  Value v ;
  v.clo.t = CLOSURE ;
  v.clo.lam = lam ;
  v.clo.env = malloc(sizeof(env) * sizeof(int));
  memcpy(v.clo.env, env, env_len);
  return v ;
}

static Value make_int(int n) {
  Value v ;
  v.n.t = INT ;
  v.n.value = n ;
  return v ;
}

Value lambda_sum(int *env, Value n){
  return make_int(*env + n.n.value);
}

static Value sum (int *env, Value n) {
  int *tmp_env = malloc((sizeof(env) + 1) * sizeof(int));
  int len = sizeof (env);
  memcpy (tmp_env + 1, env, len);
  *tmp_env = n.n.value;
  return (make_closure(lambda_sum, tmp_env, len + 1));
}
#endif // __CORE_H_
