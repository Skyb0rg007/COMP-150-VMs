(* Properties of primitives (SVM instructions) *)

(* You'll use the signature, but you don't need to know the
    implementation---except to be sure that every primitive
    has a corresponding VM instruction of the same name. *)

structure Primitive :> sig
  (* A primitive corresponds to an SVM instruction *)

  type base
  datatype primitive = SETS_REGISTER of base | HAS_EFFECT of base

    (* For SETS_REGISTER, the instruction takes the destination register
       as the first operand, and the actual parameters as remaining operands.

       For HAS_EFFECT, the instruction has no destination register;
       the operands are the arguments.
       
       A SETS_REGISTER primitive _must not_ have a side effect,
       because if one appears in an effectful context, it is discarded.
     *)

  (* these are the primitives that are available to source code *)
  val find : string -> primitive option
  val exposedNames : string list

  val name  : primitive -> string   (* used in assembly code & object code *)
  val arity : primitive -> int      (* used to make LAMBDAs on demand *)

  val throwsError : primitive -> bool
     (* primitive does not return, so is OK to use in a value context *)

  (* these are the primitives that are used inside the compiler *)
  val cons : primitive
  val setglobal : primitive
  val getglobal : primitive
  val check  : primitive
  val expect : primitive
  val check_assert : primitive
  val loadliteral : primitive
end
  =
struct

  val binary  = [ "+", "-", "*", "/", "<", ">", "cons", "=" ]
  val unary   = [ "boolean?", "null?", "number?", "pair?", "function?", "nil?"
                , "symbol?", "car", "cdr"
                ]
  val side_effecting = [ "print", "printu" ]
  val error = [ "error" ]
  val checky = [ "check", "expect" ] (* not sure if exposing these to source is smart *)

  type base = { name : string, arity : int }
  datatype primitive = SETS_REGISTER of base | HAS_EFFECT of base

  fun add arity ty names prims =
    foldl (fn (name, prims) => ty { name = name, arity = arity } :: prims)
          prims names

  infixr 0 $
  fun f $ x = f x

  val primitives : primitive list =
    add 2 SETS_REGISTER binary $
    add 1 SETS_REGISTER unary  $
    add 1 HAS_EFFECT side_effecting $
    add 1 HAS_EFFECT error $
    add 2 HAS_EFFECT checky $
    []

  fun base (SETS_REGISTER b) = b
    | base (HAS_EFFECT    b) = b

  val name  = #name  o base
  val arity = #arity o base
  
  fun throwsError p = #name (base p) = "error"

  fun find x = List.find (fn p => name p = x) primitives

  val exposedNames = map name primitives

  val cons         = SETS_REGISTER { name = "cons", arity = 2 }
  val setglobal    = HAS_EFFECT { name = "setglobal", arity = 2 }
  val getglobal    = SETS_REGISTER { name = "getglobal", arity = 1 }
  val check        = HAS_EFFECT { name = "check", arity = 2 }
  val expect       = HAS_EFFECT { name = "expect", arity = 2 }
  val check_assert = HAS_EFFECT { name = "check-assert", arity = 2 }
  val loadliteral  = SETS_REGISTER { name = "loadliteral", arity = 1 }

end



