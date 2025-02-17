(* Interface to assembly language for the convenience of other translations *)

(* You'll use this interface periodically in modules 6 to 10:

    - You'll use the interface to write your code generator
    - You'll implement `copyreg` in a way that works for your SVM
    - In module 8, you'll add support for function calls
    - In module 10, you'll add support for closures 

 *)


structure AsmGen :> sig
  type reg = ObjectCode.reg          
  type instruction = AssemblyCode.instr
  type label

  type vmop = Primitive.primitive      (* identifies a VM operator, like + or / *)
  type literal = ObjectCode.literal    (* we care only about numbers and strings *)

  val newlabel : unit -> label  (* fresh every time *)

  (********* instructions that can be generated ******)

  (* simple instructions like cons, +, <; destination register on left *)
  val copyreg   : reg -> reg -> instruction   (* assignment/copy/move *)
  val setreg    : reg -> vmop -> reg list -> instruction
  val setregLit : reg -> vmop -> reg list -> literal -> instruction

  (* effects like setglobal, check, and expect *)
  val effect    : vmop -> reg list -> instruction
  val effectLit : vmop -> reg list -> literal -> instruction
    
  (* control-flow instructions *)
  val goto     : label -> instruction
  val ifgoto   : reg -> label -> instruction
  val deflabel : label -> instruction  (* labels the next instruction *)

  (* function loading *)
  val loadfunc : reg -> int -> instruction list -> instruction

  (* special cases of the code above *)
  val loadlit   : reg -> literal -> instruction   (* rX := LIT     *)
  val getglobal : reg -> literal -> instruction   (* rX := _G[LIT] *)
  val setglobal : literal -> reg -> instruction   (* _G[LIT] := rX *)

end
  =
struct
  structure A = AssemblyCode
  structure O = ObjectCode
  structure P = Primitive

  type reg = ObjectCode.reg
  type instruction = AssemblyCode.instr
  type label = string            
  type vmop = Primitive.primitive
  type literal = ObjectCode.literal


  (***** label generation ****)

  local
    val labelsSupplied = ref 0
  in
    fun newlabel () =
      let val n = 1 + !labelsSupplied
          val () = labelsSupplied := n
      in  "L" ^ (Int.toString n)
      end
  end

  exception InternalError of string
  fun fail s = raise InternalError s

  (***** sanity checking for correct use of primitives ****)

  fun asValue p =
    case p
      of P.HAS_EFFECT _ => fail ("primitive " ^ P.name p ^ " used for value")
       | P.SETS_REGISTER _ => P.name p

  fun asEffect p =
    case p
      of P.SETS_REGISTER _ => fail ("primitive " ^ P.name p ^ " used for effect")
       | P.HAS_EFFECT _ => P.name p


  fun i con arg = A.OBJECT_CODE (con arg)
  fun regs opr rs = i O.REGS (opr, rs)


  fun setreg    dest operator args   = i O.REGS    (asValue operator, dest::args)
  fun setregLit dest operator args v = i O.REGSLIT (asValue operator, dest::args, v)
  fun effect         operator args   = i O.REGS    (asEffect operator, args)
  fun effectLit      operator args v = i O.REGSLIT (asEffect operator, args, v)

  fun goto label = A.GOTO_LABEL label
  fun deflabel l = A.DEFLABEL l
  fun ifgoto reg l = A.IF_GOTO_LABEL (reg, l)

  fun loadfunc r k body = A.LOADFUNC (r, k, body)

  fun loadlit r v = setregLit r P.loadliteral [] v

  fun getglobal dest  name  = setregLit dest P.getglobal []    name
  fun setglobal name  reg   = effectLit      P.setglobal [reg] name
  
  fun copyreg dest src = Impossible.exercise "register-register move"

end
