(* Parser for VScheme *)

(* You'll need to use the signature, 
    but don't need to know how it's implemented *)

structure VSchemeParsers :> sig
  val exp  : Sx.sx -> VScheme.exp Error.error
  val defs : Sx.sx -> VScheme.def list Error.error
end
  =
struct
  structure S = VScheme

  fun showTokens ts = "[" ^ String.concatWith "\194\183" (map SxUtil.whatis ts) ^ "]"

  structure P = MkListProducer (val species = "S-expression parser"
                                type input = Sx.sx
                                val show = showTokens
                               )

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

  fun id x = x

  fun letx k bs e = S.LETX (k, bs, e)

  fun letstar [] e            = e
    | letstar ((x, e')::bs) e = letx S.LET [(x, e')] (letstar bs e)

  fun sexp (Sx.SYM s) = S.SYM s
    | sexp (Sx.INT n) = S.NUM n
    | sexp (Sx.BOOL b) = S.BOOLV b
    | sexp (Sx.REAL y) = S.SYM (Real.toString y) (* urk! *)
    | sexp (Sx.LIST []) = S.EMPTYLIST
    | sexp (Sx.LIST (v::vs)) = S.PAIR (sexp v, sexp (Sx.LIST vs))

  val rwords =
    ["set", "if", "while", "begin", "let", "let*", "letrec", "lambda", "quote",
     "val", "define"]

  fun reserved s = List.exists (P.eq s) rwords

  val int       = P.maybe (fn (Sx.INT   n)    => SOME n  | _ => NONE) one
  val sym       = P.maybe (fn (Sx.SYM s)      => SOME s  | _ => NONE) one
  val list      = P.maybe (fn (Sx.LIST sxs)    => SOME sxs  | _ => NONE) one
  val bool      = P.maybe (fn (Sx.BOOL b)    => SOME b  | _ => NONE) one
  val sxreal    = P.maybe (fn (Sx.REAL x)    => SOME x  | _ => NONE) one

  fun kw word = P.sat (P.eq word) sym  (* keyword *)
  val name =  P.sat (not o reserved) sym
          <|> (sym >> P.perror "reserved word used as name")

  
  fun parseSucceeds (SOME (Error.OK a, [])) = SOME (Error.OK a)
    | parseSucceeds (SOME (Error.ERROR msg, _)) = SOME (Error.ERROR msg)
    | parseSucceeds _ = NONE

  fun oflist parser = P.check (P.maybe parseSucceeds (P.asFunction parser <$> list))

  type 'a parser = 'a P.producer

  val _ = oflist : 'a parser -> 'a parser

  fun bracket word parser =
    oflist (kw word >> (parser <~> P.eos <|> P.perror ("Bad " ^ word ^ " form")))

  val exp = P.fix' (fn expref =>
    let val exp = P.!! expref
        fun pair x y = (x, y)
        val binding  = oflist (pair <$> name <*> exp)
        val bindings = oflist (many binding)
    in     bracket "set"       (curry  S.SET <$> name <*> exp)
       <|> bracket "if"        (curry3 S.IFX <$> exp <*> exp <*> exp)
       <|> bracket "while"     (curry  S.WHILEX <$> exp <*> exp)
       <|> bracket "begin"     (       S.BEGIN <$> many exp)
       <|> bracket "break"     (Impossible.exercise <$> succeed "AST for break")
       <|> bracket "continue"  (Impossible.exercise <$> succeed "AST for continue")
       <|> bracket "let"       (letx S.LET    <$> bindings <*> exp)
       <|> bracket "let*"      (letstar       <$> bindings <*> exp)
       <|> bracket "letrec"    (letx S.LETREC <$> bindings <*> exp)
       <|> bracket "quote"     (      (S.LITERAL o sexp) <$> one)
       <|> bracket "lambda"    (curry S.LAMBDA <$> oflist (many name) <*> exp) 
       <|> oflist eos >> P.perror "empty list as Scheme expression"
       <|> S.LITERAL <$> (    kw "#t" >> P.succeed (S.BOOLV true)
                          <|> kw "#f" >> P.succeed (S.BOOLV false)
                          <|> S.BOOLV <$> bool
                          <|> S.NUM <$> int
                          <|> (S.SYM o Real.toString) <$> sxreal
                         )
       <|> S.VAR <$> name
       <|> oflist (curry S.APPLY <$> exp <*> many exp) 
                   
    end)

  fun unimp s _ = Impossible.unimp s

  fun define f xs e = S.DEFINE (f, (xs, e))

  fun single x = [x]

  fun l1 arityx K = arityx (single o K)

  local
    fun nullp x = S.APPLY(S.VAR "null?", [x])
    fun pairp x = S.APPLY(S.VAR "pair?", [x])
    val cons = fn (x, xs) => VSchemeUtils.cons x xs

        fun desugarRecord recname fieldnames =
              recordConstructor recname fieldnames ::
              recordPredicate recname fieldnames ::
              recordAccessors recname 0 fieldnames
        and recordConstructor recname fieldnames = 
              let val con = "make-" ^ recname
                  val formals = map (fn s => "the-" ^ s) fieldnames
                  val body = cons (S.LITERAL (S.SYM con), varlist formals)
              in  S.DEFINE (con, (formals, body))
              end
        and recordPredicate recname fieldnames =
              let val tag = S.SYM ("make-" ^ recname)
                  val predname = recname ^ "?"
                  val r = S.VAR "r"
                  val formals = ["r"]
                  val good_car = S.APPLY (S.VAR "=", [VSchemeUtils.car r, S.LITERAL tag])
                  fun good_cdr looking_at [] = nullp looking_at
                    | good_cdr looking_at (_ :: rest) =
                        and_also (pairp looking_at, good_cdr (VSchemeUtils.cdr looking_at) rest)
                  val body =
                    and_also (pairp r, and_also (good_car, good_cdr (VSchemeUtils.cdr r) fieldnames))
              in  S.DEFINE (predname, (formals, body))
              end
        and recordAccessors recname n [] = []
          | recordAccessors recname n (field::fields) =
              let val predname = recname ^ "?"
                  val accname = recname ^ "-" ^ field
                  val formals = ["r"]
                  val thefield = VSchemeUtils.car (cdrs (n+1, S.VAR "r"))
                  val body = S.IFX ( S.APPLY (S.VAR predname, [S.VAR "r"])
                                 , thefield
                                 , error (S.SYM (concat ["value-passed-to-"
                                               , accname
                                               , "-is-not-a-"
                                               , recname
                                               ])))
              in  S.DEFINE (accname, (formals, body)) ::
                  recordAccessors recname (n+1) fields
              end
        and and_also (p, q) = S.IFX (p, q, S.LITERAL (S.BOOLV false))
        and cdrs (0, xs) = xs
          | cdrs (n, xs) = VSchemeUtils.cdr (cdrs (n-1, xs))

        and list [] = S.LITERAL S.EMPTYLIST
          | list (v::vs) = cons (S.LITERAL v, list vs)
        and error x = S.APPLY (S.VAR "error", [S.LITERAL x])

        and varlist [] = S.LITERAL S.EMPTYLIST
          | varlist (x::xs) = cons (S.VAR x, varlist xs)
  in 
    val desugarRecord = desugarRecord
  end


  fun single x = [x]

  val def =
    single <$>
           (    bracket "val"    (curry S.VAL <$> name <*> exp)
            <|> bracket "define" (define <$> name <*> oflist (many name) <*> exp)
            <|> bracket "check-expect" (curry  S.CHECK_EXPECT <$> exp <*> exp)
            <|> bracket "check-assert" (       S.CHECK_ASSERT <$> exp)
            <|> bracket "check-error"  (unimp "check-error" <$> exp)
           )
    <|> bracket "record" (desugarRecord <$> name <*> oflist (many name))
    <|> single <$> S.EXP <$> exp

  fun transform parser sx = P.produce (parser <~> P.eos) [sx]

  val exp = transform exp
  val defs = transform def
end
