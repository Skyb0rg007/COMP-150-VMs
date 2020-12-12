

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




PARSING:

`h` : string -> int
`counter` : int

getglobal 0 "foo"
  -> Lookup "foo" in h
     If it doesn't exist:
        Generate a fresh index by incrementing `counter`,
        insert it into `h`
    Set the index to `i`
    Generate the VM instruction:
        `getglobal 0 i`

VM:

`globals` : int -> value



$r0 := 2
setglobal["na"] := $r0
$r2 := getglobal["na"]
$r1 := 7
setglobal["na"] := $r1 



"na" -> 0


$r0 := 2
setglobal[0] := $r0
$r2 := getglobal[0]
$r1 := 7
setglobal[0] := $r1 

After parsing: `globals` is *still* uninitialized
