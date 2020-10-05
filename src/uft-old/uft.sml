(* This is the universal forward translator. As you build the different VScheme 
    representations and the translations between them, you'll chain together 
    these translations here. It implements the actual translations *)

(* You'll get a partially complete version of this file, 
    which you'll need to complete *)

structure UFT :> sig
  type language = Languages.language
  exception NotForward of language * language
  val translate : language * language -> TextIO.instream * TextIO.outstream -> unit Error.error
  exception NoTranslationTo of language
end
  =
struct

  (**** I/O functions and types ****)

  val lines  = IOUtil.lines
  val output = IOUtil.output
  val outln  = IOUtil.outln
  type instream = TextIO.instream


  (**** function composition, including errors ****)

  type 'a error = 'a Error.error

  infix 0 >>> >=>
  fun f >>> g = fn x => g (f x)         (* function composition, Elm style *)
  val op >=> = Error.>=>
  val ! = Error.map
  fun emap f = map f >>> Error.list

  (**** functions for reading code to be translated ****)

  val VS_of_file : instream -> AssemblyCode.instr list error =
    lines
    >>> map (AsmLex.tokenize  (* token list error *)
            >=>  AsmParse.parse       (* instr error *)
           )
    >>> Error.list


  (**** functions for emitting translated code ****)

  fun emitVO outfile = app (outln outfile) o ObjectUnparser.program
  fun emitVS outfile = app (outln outfile) o AsmParse.unparse

  (**** individual translations ***)

  exception Backward  

  datatype language = datatype Languages.language (* imports value constructors *)
  exception NoTranslationTo of language  

  fun VS_of VS   = VS_of_file
    | VS_of from = raise NoTranslationTo VS
  fun VO_of VO   = (fn _ => Error.ERROR "there is no reader for .vo")
    | VO_of from = VS_of from >=> Assembler.translate



  (**** the Universal Forward Translator ****)

  exception NotForward of language * language

  fun translate (from, to) (infile, outfile) =
    (case to
       of VO => VO_of      from >>> ! (emitVO outfile)
        | VS => VS_of      from >>> ! (emitVS outfile)
        | _  => raise NoTranslationTo to
    ) infile
    handle Backward => raise NotForward (from, to)
         | NoTranslationTo to => raise NotForward (from, to)
end
