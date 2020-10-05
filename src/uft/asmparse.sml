(* A parser for assembly language *)

(* You'll get a partially complete version of this file, 
  which you'll need to complete. *)

structure AsmParse :>
  sig
    type line = string (* one line of assembly code *)
    val parse   : AsmLex.token list  list -> AssemblyCode.instr list Error.error
    val unparse : AssemblyCode.instr list -> line list
  end
  =
struct
  (* visualize list of tokens using Unicode middle dot as separator *)
  fun showTokens ts = "[" ^ String.concatWith "\194\183" (map AsmLex.unparse ts) ^ "]"

  structure P = MkListProducer (val species = "parser"
                                type input = AsmLex.token
                                val show = showTokens
                               )
      (* P for parser; builds a module that takes AsmLex.token as input *)

  structure L = AsmLex
  structure A = AssemblyCode
  structure O = ObjectCode

  type line = string (* one line of assembly code *)

  (* Operations on producers: Wishing for Modula-style FROM IMPORT here ... *)
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
  fun flip f x y = f y x

  (* utilities *)

  fun eprint s = TextIO.output (TextIO.stdErr, s)

  type 'a parser = 'a P.producer

  (* always-error parser; useful for messages *)
  fun expected what =
    let fun bads ts = Error.ERROR ("looking for " ^ what ^
                                   ", got this input: " ^ showTokens ts)
    in  P.check ( bads <$> many one )
    end

  (* always-succeed parser; prints msg when run *)
  fun debug msg =
      P.ofFunction (fn ts => (app eprint ["@> ", msg, "\n"]; SOME (Error.OK (), ts)))

  (* make another parser chatter on entry *)
  val verbose : string -> 'a parser -> 'a parser
   = (fn msg => fn p => debug msg >> p)

  val veryVerbose : string -> 'a parser -> 'a parser
      = (fn what => fn p =>
           let fun shout s = app eprint ["looking for ", what, s, "\n"]
           in  P.ofFunction (fn ts =>
                                let val _ = shout "..."
                                    val answer = P.asFunction p ts
                                    val _ =
                                        case answer
                                          of NONE => shout ": failed"
                                           | SOME (Error.ERROR _, _) => shout ": errored"
                                           | SOME (Error.OK _, _) => shout ": succeeded"
                                in  answer
                                end)
           end)

  (****************************************************************************)

  (**** parsers for common tokens ****)

      (* These are your workhorse parsers---the analog of the `get`
         functions from the `tokens.h` interface in the SVM *)

  val int       = P.maybe (fn (L.INT   n)    => SOME n  | _ => NONE) one
  val name      = P.maybe (fn (L.NAME  n)    => SOME n  | _ => NONE) one
  val string    = P.maybe (fn (L.STRING s)   => SOME s  | _ => NONE) one
  val reg       = P.maybe (fn (L.REGISTER n) => SOME n  | _ => NONE) one
  val eol       = P.maybe (fn (L.EOL)        => SOME () | _ => NONE) one

  (* turn any single-token string into a parser for that token *)
  fun the "\n" = eol
    | the s =
        case AsmLex.tokenize s
          of Error.OK [t, AsmLex.EOL] => sat (P.eq t) one >> succeed ()
           | _ => Impossible.impossible "non-token in assembler parser"

  fun kw s = sat (P.eq s) name  (* keyword; example: kw "goto" *)


  (***** instruction-building functions for parsers ****)

  fun regs operator operands = A.OBJECT_CODE (O.REGS (operator, operands))
     (* curried instruction builder *)

  fun eR0 operator          = regs operator []
  fun eR1 operator r1       = regs operator [r1]
  fun eR2 operator r1 r2    = regs operator [r1, r2]
  fun eR3 operator r1 r2 r3 = regs operator [r1, r2, r3]


  (***** toy parser for you to extend ****)

  (* parser to read an instruction /without/ reading end of line *)
  val one_line_instr : A.instr P.producer 
     =  kw "@" >> regs <$> name <*> many int  (* "escape hatch" syntax *)
    <|> eR3 "+" <$> reg <~> the ":=" <*> reg <~> the "+" <*> reg
    (* You add cases here.  Put cases with longer inputs first, e.g.,
          r1 := r2 + r3
       before
          r1 := r3
     *)    


   (**** recursive parser that handles end-of-line and function loading ****)

   (* Parsers for start and end of "load function", for you to write.
      Designing syntax so each one terminates with `eol` is recommended. *)

   fun loadfunc (reg, arity) body = A.LOADFUNC (reg, arity, body)
   val loadfunStart : (int * int) parser = (* fill in with (reg * arity) parser *)
         P.pzero <~> eol
   val loadfunEnd : unit parser =
         P.pzero <~> eol

   (* grammar :   <instruction> ::= <one_line_instruction> EOL
                                 | <loadfunStart> {<instruction>} <loadfunEnd> *)

   (* simple parser with no error detection *)
   val instruction : A.instr Error.error parser
     = Error.OK <$>
       P.fix (fn instruction : A.instr parser =>
                one_line_instr <~> eol
            <|> loadfunc <$> loadfunStart <*> many instruction <~> loadfunEnd
             )

   (* A better parser is juiced up with extra error detection *)

   fun badTokens ts = Error.ERROR ("unrecognized assembly line: " ^ showTokens ts)
   val nonEOL = sat (curry op <> L.EOL) one  (* any token except EOL *)

   val instruction : A.instr Error.error parser
     = P.fix (fn instruction =>
           Error.OK <$> one_line_instr <~> eol
       <|> Error.OK <$>
           (loadfunc <$> loadfunStart <*> 
                         P.check (Error.list <$> many instruction) <~>
                         loadfunEnd)
       <|> P.notFollowedBy loadfunEnd >>
           (* gobble to end of line, then succeed by producing error message: *)
           badTokens <$> many nonEOL <~> eol  
        )


  val parse = Error.join o P.produce (Error.list <$> many instruction) o List.concat
            

  (*************************** unparsing *****************************)

  val int = Int.toString
  fun reg r = "r" ^ int r
  val spaceSep = String.concatWith " "


  fun unparse1 (A.OBJECT_CODE (O.REGS ("+", [x, y, z]))) =
        spaceSep [reg x, ":=", reg y, "+", reg z]
    | unparse1 _ = "an unknown assembly-code instruction"

  
  val unparse : AssemblyCode.instr list -> string list
    = map unparse1  (* not good enough in presence of LOADFUNC *)



end
