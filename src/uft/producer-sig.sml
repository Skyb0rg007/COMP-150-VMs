(* Producer interface [parsing combinators], developed in module 3 lab *)

(* You'll get a partially complete version of this file,
    which you'll need to complete *)

signature PRODUCER = sig
  type input
  type 'a error = 'a Error.error
  type 'a producer

  val asFunction : 'a producer -> input list -> ('a error * input list) option
  val ofFunction : (input list -> ('a error * input list) option) -> 'a producer

  val produce : 'a producer -> input list -> 'a error
    (* consumes the entire list to produce a single 'a, or errors *)


  (* main builders: applicative functor plus alternative *)
  val succeed : 'a -> 'a producer
  val <*> : ('a -> 'b) producer * 'a producer -> 'b producer
  val <$> : ('a -> 'b) * 'a producer -> 'b producer
  val <|> : 'a producer * 'a producer -> 'a producer

  (* shortcuts for parsing something and dropping the result *)
  val <~> : 'a producer * 'b producer -> 'a producer
  val >>  : 'a producer * 'b producer -> 'b producer

  (* conditional parsing *)
  val sat   : ('a -> bool)      -> 'a producer -> 'a producer
  val maybe : ('a -> 'b option) -> 'a producer -> 'b producer

  val eos : unit producer   (* end of stream *)
  val one : input producer  (* one token *)

  (* classic EBNF, plus "one or more" *)
  val optional : 'a producer -> 'a option producer
  val many  : 'a producer -> 'a list producer
  val many1 : 'a producer -> 'a list producer


  (* check for a semantic error, turn it into a syntax error *)
  val check : 'a error producer -> 'a producer


  (* occasionally useful *)
  val pzero : 'a producer (* always fails *)
  val perror : string -> 'a producer (* always errors *)

  (* for special things *)
  val notFollowedBy : 'a producer -> unit producer

  (* recursive parsers *)
  val fix : ('a producer -> 'a producer) -> 'a producer

  (* recursive parsers, efficiently *)
  type 'a pref 
  val ! : 'a pref -> 'a producer
  val fix' : ('a pref -> 'a producer) -> 'a producer
  

  (* useful for building semantic functions *)
  val id   : 'a -> 'a
  val fst  : 'a * 'b -> 'a
  val snd  : 'a * 'b -> 'b
  val pair : 'a -> 'b -> 'a * 'b
  val eq   : ''a -> ''a -> bool

  val curry  : ('a * 'b      -> 'c) -> ('a -> 'b -> 'c)
  val curry3 : ('a * 'b * 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)

end
