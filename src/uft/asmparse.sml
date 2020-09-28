(* A parser for assembly language *)

(* You'll get a partially complete version of this file, 
  which you'll need to complete. *)

structure AsmParse :>
  sig
    val parse : AsmLex.token list -> AssemblyCode.instr Error.error
    val unparse1 : AssemblyCode.instr -> string 
    val unparse : AssemblyCode.instr list -> string list (* use me if there are functions *)
  end
  =
struct
  fun showTokens ts = "[" ^ String.concatWith "\194\183" (map AsmLex.unparse ts) ^ "]"

  structure P = MkListProducer (val species = "parser"
                                type input = AsmLex.token
                                val show = showTokens
                               )

  structure L = AsmLex
  structure A = AssemblyCode
  structure O = ObjectCode

  (* wishing for Modula-style FROM IMPORT here ... *)
  infix 3 <*>      val op <*> = P.<*>
  infixr 4 <$>     val op <$> = P.<$>
  infix 3 <~>      val op <~> = P.<~>
  infix 1 <|>      val op <|> = P.<|>
  infix 3 >>        val op >> = P.>>
  
  val succeed = P.succeed
  val curry = P.curry
  val curry3 = P.curry3
  val id = P.id
  val fst = P.fst
  val snd = P.snd
  val many = P.many
  val many1 = P.many1
  val sat = P.sat
  val one = P.one
  val notFollowedBy = P.notFollowedBy
  val eos = P.eos

  fun curry4 f w x y z = f (w, x, y, z)

  type 'a parser = 'a P.producer

  (****************************************************************************)

  (**** parsers for common tokens ****)

      (* These are your workhorse parsers---the analog of the `get`
         functions from the `tokens.h` interface in the SVM *)

  val int       = P.maybe (fn (L.INT   n)    => SOME n  | _ => NONE) one
  val name      = P.maybe (fn (L.NAME  n)    => SOME n  | _ => NONE) one
  val string    = P.maybe (fn (L.STRING s)   => SOME s  | _ => NONE) one
  val reg       = P.maybe (fn (L.REGISTER n) => SOME n  | _ => NONE) one

  fun the s =
    case AsmLex.tokenize s
      of Error.OK [t] => sat (P.eq t) one >> succeed ()
       | _ => Impossible.impossible "non-token in assembler parser"

  fun kw s = sat (P.eq s) name  (* keyword; example: kw "goto" *)

  (***** instruction-building functions for parsers ****)

  fun regs operator operands = A.OBJECT_CODE (O.REGS (operator, operands))
     (* curried instruction builder *)

  fun eR0 operator          = regs operator []
  fun eR1 operator r1       = regs operator [r1]
  fun eR2 operator r1 r2    = regs operator [r1, r2]
  fun eR3 operator r1 r2 r3 = regs operator [r1, r2, r3]


  (***** toy parser ****)

  val instr : A.instr P.producer 
     =  kw "@" >> regs <$> name <*> many int  (* "escape hatch" syntax *)
    <|> eR3 "+" <$> reg <~> the ":=" <*> reg <~> the "+" <*> reg
    <|> eR3 "-" <$> reg <~> the ":=" <*> reg <~> the "-" <*> reg

  val parse : L.token list -> A.instr Error.error = P.produce instr



  (*************************** unparsing *****************************)

  val int = Int.toString
  fun reg r = "r" ^ int r
  val spaceSep = String.concatWith " "
  val nlSep = String.concatWith "\n"

  (* fun unparse1 (A.OBJECT_CODE (O.REGS ("+", [x, y, z]))) = *)
        (* spaceSep [reg x, ":=", reg y, "+", reg z] *)
    (* | unparse1 _ = "an unknown assembly-code instruction" *)

  val unparse_object_code =
    fn O.REGS ("add", [x, y, z]) => spaceSep [reg x, ":=", reg y, "+", reg z]
     | O.REGS ("+", [x, y, z]) => spaceSep [reg x, ":=", reg y, "+", reg z]
     | O.REGS ("sub", [x, y, z]) => spaceSep [reg x, ":=", reg y, "-", reg z]
     | O.REGS ("-", [x, y, z]) => spaceSep [reg x, ":=", reg y, "-", reg z]
     | O.REGS ("mul", [x, y, z]) => spaceSep [reg x, ":=", reg y, "*", reg z]
     | O.REGS ("*", [x, y, z]) => spaceSep [reg x, ":=", reg y, "*", reg z]
     | O.REGS ("div", [x, y, z]) => spaceSep [reg x, ":=", reg y, "/", reg z]
     | O.REGS ("/", [x, y, z]) => spaceSep [reg x, ":=", reg y, "/", reg z]
     | O.REGS ("abs", [x, y])    => spaceSep [reg x, ":=", "abs", reg y]
     | O.REGS ("hash", [x, y])   => spaceSep [reg x, ":=", "hash", reg y]
     | O.REGS ("print", [x])     => spaceSep ["print", reg x]
     | O.REGS ("halt", [])       => "halt"
     (* | O.REGS ("goto", [x]) => "asdf" *)
     | O.REGSLIT ("loadliteral", [x], lit) => spaceSep [reg x, ":=", spaceSep (ObjectUnparser.literal lit)]
     | O.REGSLIT ("getglobal", [x], lit) => spaceSep [reg x, ":=", "G[" ^ spaceSep (ObjectUnparser.literal lit) ^ "]"]
     | O.REGSLIT ("setglobal", [x], lit) => spaceSep ["G[" ^ spaceSep (ObjectUnparser.literal lit) ^ "]", reg x]
     | O.GOTO n => spaceSep ["goto", int n]
     | O.LOADFUNC (reg, arity, instrs) =>
         nlSep [
           
         ]
     | p =>
         raise Fail ("unparse_object_code doesn't handle this case" ^ spaceSep (ObjectUnparser.program [p]))


  val unparse1 : AssemblyCode.instr -> string =
    fn A.OBJECT_CODE x => unparse_object_code x
     | A.DEFLABEL lbl => lbl ^ ":"
     | A.GOTO_LABEL lbl => spaceSep ["goto", lbl]
     | A.IF_GOTO_LABEL (r, lbl) => spaceSep ["if", reg r, "goto", lbl]
     | A.LOADFUNC _ => raise Fail "unparse1 should not be given LoadFunc"

  val unparse_loadfunc : AssemblyCode.instr -> string list =
    fn A.LOADFUNC (r, arity, instrs) =>
        reg r ^ " := function " ^ int arity ^ "{"
        :: map unparse1 instrs
        @ ["}"]
     | x => [ unparse1 x ]
  
  val unparse : AssemblyCode.instr list -> string list
    = List.concat o map unparse_loadfunc



end
