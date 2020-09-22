(* A lexer for assembly language. 
  This is in charge of tokenizing assembly code. *)

(* You'll need to understand what's going on here, and how it's used *)

structure AsmToken = struct
  datatype shape = ROUND | SQUARE | CURLY   (* bracket shape *)

  datatype token = LEFT of shape
                 | RIGHT of shape
                 | COMMA
                 | COLON
                 | EQUAL
                 | EQUALEQUAL (* == *)
                 | GETS   (* := *)
                 | REGISTER of int
                 | NAME   of string
                 | STRING of string
                 | INT of int
end

structure AsmLex :> sig
  datatype shape = datatype AsmToken.shape (* re-exports type _and constructors_ *)
  datatype token = datatype AsmToken.token
  val unparse : token -> string
  val tokenize : string -> token list Error.error
  val registerNum : string -> int Error.error

  val debug : string -> unit

end
  =    
struct
  datatype shape = datatype AsmToken.shape
  datatype token = datatype AsmToken.token

  val dq = #"\""   (* double quote *)

  fun quoteString s = str dq ^ String.toCString s ^ str dq
  fun ceeMinus #"~" = #"-"
    | ceeMinus c    = c

  val brackets = [(ROUND, "()"), (SQUARE, "[]"), (CURLY, "{}")]
    
  infixr 0 $       (* infix function application, from Haskell *)
  fun f $ x = f x

  fun curry f x y = f (x, y)
  fun bracketString shape = #2 $ valOf $ List.find (curry op = shape o #1) $ brackets


  fun unparse (LEFT shape)  = str $ String.sub (bracketString shape, 0)
    | unparse (RIGHT shape) = str $ String.sub (bracketString shape, 1)
    | unparse (COMMA)       = ","
    | unparse (COLON)       = ":"
    | unparse (EQUAL)       = "="
    | unparse (GETS)        = ":="
    | unparse (EQUALEQUAL)  = "=="
    | unparse (REGISTER n)  = "$r" ^ Int.toString n
    | unparse (NAME s)      = s  (* dodgy? *)
    | unparse (STRING s)    = quoteString s
    | unparse (INT s)       = String.map ceeMinus (Int.toString s)

  structure L = MkListProducer (val species = "lexer"
                                type input = char
                                val show = quoteString o implode
                               )




  (* wishing for Modula-style FROM IMPORT here ... *)
  infix 3 <*>      val op <*> = L.<*>
  infix 5 <~>      val op <~> = L.<~>
  infixr 4 <$>     val op <$> = L.<$>
  infix 1 <|>      val op <|> = L.<|>
  infix 6 <~> >>   val op <~> = L.<~>  val op >> = L.>>

  val succeed = L.succeed
  val curry = L.curry
  val id = L.id
  val fst = L.fst
  val snd = L.snd
  val many = L.many
  val many1 = L.many1
  val sat = L.sat
  val one = L.one
  val notFollowedBy = L.notFollowedBy
  val eos = L.eos

  type lexer = token L.producer

  fun isDelim c = Char.isSpace c orelse Char.contains "()[]{}:=;," c


  fun intFromChars (#"-" :: cs) =
        Error.map Int.~ (intFromChars cs)
    | intFromChars cs =
        (Error.OK o valOf o Int.fromString o implode) cs
        handle Overflow =>
          Error.ERROR "this interpreter can't read arbitrarily large integers"

  val minusSign = sat (L.eq #"-") one

  val intChars =
    (curry (op ::) <$> minusSign <|> succeed id) <*> many1 (sat Char.isDigit one) <~>
    notFollowedBy (sat (not o isDelim) one)

  val intToken = L.check (intFromChars <$> intChars)

  val comment = curry op :: <$> sat (L.eq #";") one <*> many one

  val whitespace =
    curry op @ <$> many (sat Char.isSpace one) <*> (comment <|> succeed [])

  fun registerNum s =
    let val prefixes = ["r", "$r"]
        fun regNum prefix s =
          if String.isPrefix prefix s then
            (Int.fromString o String.extract) (s, size prefix, NONE)
          else
            NONE
        fun get (p::ps) s =
              (case regNum p s of SOME n => SOME n | NONE => get ps s)
          | get [] s = NONE
    in  case get prefixes s of
          SOME n => if n >= 0 andalso n < 256 then 
                      Error.succeed n 
                    else 
                      Error.ERROR ("Register number out of range: " ^ Int.toString n)
        | NONE   => Error.ERROR ("Not a register number: " ^ s)
    end

  fun name s =
    case registerNum s
      of Error.OK r => REGISTER r
       | _ => NAME s

  fun char c = sat (curry op = c) one

  fun escape #"n" = Error.OK #"\n"
    | escape #"\"" = Error.OK #"\""
    | escape #"\\" = Error.OK #"\\"
    | escape c = Error.ERROR ("Escape code \\" ^ str c ^ " is undefined")

  val escapedChar =  char #"\\" >> L.check (escape <$> one)
                 <|> sat (curry op <> dq) one
         
  val escapedChar' = L.ofFunction
   (fn s =>
    let val answer = L.asFunction escapedChar s
        val results = case answer
                   of NONE => ["NONE"]
                    | SOME (Error.OK c, ts) => ["char ", str c, " with these left: ", implode ts]
                    | SOME (Error.ERROR s, ts) => ["ERROR ", s, " with these left: ", implode ts]
    in  app print results; print "\n"; answer
    end)
      


  fun bracketParser (shape, chars) =
        succeed (LEFT  shape) <~> char (String.sub (chars, 0))
    <|> succeed (RIGHT shape) <~> char (String.sub (chars, 1))

  val bracketParsers = foldl (fn (x, p) => bracketParser x <|> p) L.pzero brackets

  val token = 
        bracketParsers
    <|> succeed EQUALEQUAL <~> char #"=" <~> char #"="
    <|> succeed EQUAL      <~> char #"="
    <|> succeed GETS       <~> char #":" <~> char #"="
    <|> succeed COLON      <~> char #":"
    <|> succeed COMMA      <~> char #","
    <|> INT <$> intToken
    <|> char dq >> ((STRING o implode) <$> many escapedChar <~> char dq)
    <|> char dq >> L.perror "unterminated quoted string"
    <|> (name o implode) <$> many1 (sat (not o isDelim) one)

  val tokenize = L.produce (whitespace >> many (token <~> whitespace)) o explode
    : string -> token list Error.error



  fun debug s =
    case tokenize s
      of Error.OK tokens =>
           app print ["SUCCESS: ", String.concatWith " " (map unparse tokens), "\n"]
       | Error.ERROR s => app print ["ERROR: ", s, "\n"]

end