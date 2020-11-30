

* Delimited continuations
  - Prompts - how?
* Non-delimited continuations: is it sound?

* 

* C api with continuations
  - Lua-style:

```
int lua_pcallk(
    lua_State *L,
    int nargs,
    int nresults,
    int msgh,
    lua_KContext ctx,
    lua_KFunction k);

typedef intptr_t lua_KContext;
typedef int (*lua_KFunction)(lua_State *L, int status, lua_KContext ctx);
```
