(* Part of the Pretty Printer *)

(* You can ignore this *)

structure WppScheme :> sig
  val pp    : VScheme.def -> Wpp.doc
  val ppexp : VScheme.exp -> Wpp.doc
  val expString : VScheme.exp -> string  (* for use with check and expect *)
end
  =
struct
  structure S = VScheme
  structure P = Wpp

  val ++ = Wpp.^^
  infix 7 ++

  val nest = P.nest
  val te = P.text

  fun id x = x

  fun letkeyword S.LET     = "let"
    | letkeyword S.LETREC  = "letrec"

  fun value (S.SYM v)   = P.text v
    | value (S.NUM n)   = P.int n
    | value (S.BOOLV b) = P.text (if b then "#t" else "#f")
    | value (S.EMPTYLIST)     = P.text "()"
    | value (S.PAIR (car, cdr))  = 
        P.group (P.text "(" ++ P.seq P.line id (values (S.PAIR (car, cdr))) ++ P.text ")")
  and values (S.EMPTYLIST) = []
    | values (S.PAIR (car, cdr)) = value car :: values cdr
    | values v = [P.text ".", value v]

  fun wrap  docs = P.group (te "(" ++ P.seq P.line id docs ++ te ")")
  fun wraps docs = P.group (te "[" ++ P.seq P.line id docs ++ te "]")
  fun kw k docs = wrap (te k :: docs)

  fun exp e =
     let
         fun pplet thekw bs e =
           let val i = size thekw + 3
               fun binding (x, e) = wraps [te x, exp e]
               val bindings = P.seq P.line binding bs
           in  nest i (kw thekw [wrap [bindings], exp e])
           end

         fun nestedBindings (prefix', S.LETX (S.LET, [(x, e')], e)) =
               nestedBindings ((x, e') :: prefix', e)
           | nestedBindings (prefix', e) = (rev prefix', e)

     in  case e
           of S.LITERAL (v as S.NUM   _) => value v
            | S.LITERAL (v as S.BOOLV _) => value v
            | S.LITERAL v => te "'" ++ value v
            | S.VAR name => te name
            | S.SET (x, e) =>
                nest 3 (kw "set" [te x, exp e])
            | S.IFX (e1, e2, e3) =>
                nest 3 (kw "if" (map exp [e1, e2, e3]))
            | S.WHILEX (e1, e2) =>
                nest 3 (kw "while" (map exp [e1, e2]))
            | S.BEGIN es => 
                nest 3 (kw "begin" (map exp es))
            | S.APPLY (e, es) => 
                nest 3 (wrap (map exp (e::es)))
            | S.LETX (S.LET, bs as [(x, e')], e) =>
                let val (bs, e) = nestedBindings (bs, e)
                in  case bs
                      of [] => exp e
                       | [_] => pplet "let" bs e
                       | _ => pplet "let*" bs e
                end
            | S.LETX (lk, bs, e) => 
                let fun binding (x, e) = wraps [te x, exp e]
                    val bindings = P.seq P.line binding bs
                in  nest 3 (kw (letkeyword lk) [wrap [bindings], exp e])
                end
            | S.LAMBDA (xs, body) =>
                nest 3 (kw "lambda" [wrap (map te xs), exp body])
     end
 

   fun def d =
     case d
       of S.VAL (x, e) => nest 3 (kw "val" [te x, exp e])
        | S.DEFINE (f, (xs, e)) =>
            nest 3 (kw "define" [te f, wrap (map te xs), exp e])
        | S.CHECK_EXPECT (e, S.LITERAL (S.BOOLV true)) =>
            nest 3 (kw "check-assert" [exp e])
        | S.CHECK_EXPECT (e, e') =>
            nest 3 (kw "check-expect" [exp e, exp e'])
        | S.CHECK_ASSERT e =>
            nest 3 (kw "check-assert" [exp e])
        | S.EXP e => exp e


  val pp = def
  val ppexp = exp

  fun stripFinalNewline s =
    case rev (explode s)
      of #"\n" :: cs => implode (rev cs)
       | _ => s
  
  val expString = stripFinalNewline o Wpp.toString 60 o ppexp

end

