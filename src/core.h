#ifndef __CORE_H_
#define __CORE_H_
struct Int ;
struct Closure ;
union Value ;

enum Tag { VOID, INT, BOOLEAN, CLOSURE, CELL, ENV } ;

typedef union Value (*Lambda)()  ;

struct Int {
  enum Tag t ;
  int value ;
} ;

struct Closure {
  enum Tag t ;
  Lambda lam ;
  void* env ;
} ;

struct Env {
  enum Tag t ;
  void* env ;
} ;

struct Cell {
  enum Tag t ;
  union Value* addr ;
} ;

union Value {
  enum Tag t ;
  struct Int z ;
  struct Closure clo ;
  struct Env env ;
} ;

typedef union Value Value ;

static Value make_closure(Lambda lam, Value env) {
  Value v ;
  v.clo.t = CLOSURE ;
  v.clo.lam = lam ;
  v.clo.env = env.env.env ;
  return v ;
}

static Value make_int(int n) {
  Value v ;
  v.z.t = INT ;
  v.z.value = n ;
  return v ;
}

static Value make_primitive(Lambda prim) {
  Value v ;
  v.clo.t = CLOSURE ;
  v.clo.lam = prim ;
  v.clo.env = NULL ;
  return v ;
}

static Value make_env(void* env) {
  Value v ;
  v.env.t = ENV ;
  v.env.env = env ;
  return v ;
}
#endif // __CORE_H_
