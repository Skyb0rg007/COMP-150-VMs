(* Embeds KNormal-form Scheme into VScheme. This cannot fail. *)

(* You'll complete this file *)

structure EmbedKN :> sig
  val value : KNormalForm.literal -> VScheme.value
  val def   : VScheme.name KNormalForm.exp -> VScheme.def
end 
  = 
struct
  structure K  = KNormalForm
  structure S  = VScheme
  structure SU = VSchemeUtils
  structure P  = Primitive

  fun let' x e' e = S.LETX (S.LET, [(x, e')], e)   (* useful helper *)

  fun value _ = Impossible.unimp "embedding of KNF values"
  fun def   _ = Impossible.unimp "embedding of KNF expressions into definitions"

end
