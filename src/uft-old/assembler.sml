(* Translates abstract assembly code into abstract object code *)

(* You'll write this file *)

structure Assembler :>
  sig
    val translate : AssemblyCode.instr list -> ObjectCode.instr list Error.error
    (* the only thing than can go wrong here is an undefined label *)
  end
  =
struct

  structure A = AssemblyCode
  structure E = Env
  structure O = ObjectCode

  val (succeed, <*>, <$>) = (Error.succeed, Error.<*>, Error.<$>)
  infixr 4 <$>
  infix 3  <*>

  fun curry f x y = f (x, y)
  fun curry3 f x y z = f (x, y, z)
  fun flip f x y  = f y x
  fun cons x xs = x :: xs

  fun translate instrs =
    let fun cvt (A.OBJECT_CODE instr)       = Error.OK instr
          | cvt (A.LOADFUNC (r, k, instrs)) = curry3 O.LOADFUNC r k <$> translate instrs
          | cvt _                           = Error.ERROR "assembler not implemented"
    in  Error.list (map cvt instrs)
    end

end
