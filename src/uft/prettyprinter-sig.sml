signature PRETTYPRINTER = sig
  val print : {width : int, print : string -> unit} -> PP.pretty -> unit
end
