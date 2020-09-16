structure ObjectCode = struct
    datatype literal = INT of int
                     | REAL of real
                     | STRING of string
                     | BOOL of bool
                     | EMPTYLIST
                     | NIL

    type reg = int
    type operator = string

    datatype instr
      = REGS    of operator * reg list
      | REGSLIT of operator * reg list * literal
      | GOTO    of int             (* PC-relative branches *)
      | LOADFUNC of reg * int * instr list
              (* LOADFUNC (r, k, body) means:
                    - body describes a function
                      that expects k parameters
                    - capture those instructions and insert the function
                      into the literal pool
                    - emit an instruction to load that literal into register r
               *)

      | REGINT     of operator * reg * reg * int
end


structure ObjectUnparser :> sig
  (* emit on-disk loader language *)
  val unparse : ObjectCode.instr list -> string list 
     (* emits ".load module" with the right size *)
end
  =
struct
  structure L = ObjectCode
  val concatSp = String.concatWith " "
  val fixSign = String.map (fn #"~" => #"-" | c => c) 
  val int = fixSign o Int.toString

  fun parts (L.INT n) = [int n]
    | parts (L.REAL x) = [fixSign (Real.toString x)]
    | parts (L.BOOL b) = [if b then "true" else "false"]
    | parts (L.EMPTYLIST) = ["emptylist"]
    | parts (L.NIL) = ["nil"]
    | parts (L.STRING s) =
        let val codes = (map Char.ord o explode) s
        in  "string" :: int (length codes) :: map int codes
        end

  fun instr (L.REGS (opr, rs))   = concatSp (opr :: map int rs)
    | instr (L.REGSLIT (opr, rs, v))   = concatSp (opr :: map int rs @ parts v)
    | instr (L.GOTO offset) = concatSp ["goto", int offset]
    | instr (L.REGINT (opr, r1, r2, offset)) =
               concatSp [opr, int r1, int r2, int offset]
    | instr (L.LOADFUNC _) = Impossible.impossible "LOADFUNC reached instr"

  fun add (L.LOADFUNC (r, k, body), tail) =
        list (concatSp [".load", int r, "function", int k]) body tail
    | add (i, tail) = instr i :: tail
  and list prefix body tail =
        concatSp [prefix, int (length body)] :: foldr add tail body

  fun unparse code = list ".load module" code []

end
