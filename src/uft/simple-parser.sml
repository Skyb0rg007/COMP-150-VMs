
type reg = int
val reg    : reg producer             (* parses a register number *)
val int    : int producer             (* parses an integer literal *)
val string : string producer          (* parses a string literal *)
val name   : string producer          (* parses a name *)
val the    : string -> unit producer  (* one token, like a comma or bracket *)

type opcode = string
type instr (* instruction *)
val eR0 : opcode -> instr
val eR1 : opcode -> reg -> instr
val eR2 : opcode -> reg -> reg -> instr
val eR3 : opcode -> reg -> reg -> reg -> instr

(* Numbers *)

val parse_number : real producer =
    let
        val sign : (real -> real) producer =
            (fn _ => Real.negate) <$> string "-"
            <|>
            (fn _ => fn x => x) <$> string "+"
            <|>
            succeed (fn x => x)
        val part1 : real producer = 
            foldr (fn (x, acc) => acc * 10 + Int.toReal x) 0 <$> many1 int
        val part2 : real option producer =
            optional (string "." >> foldr (fn (x, acc) => acc / 10 + Int.toReal x) 0 <$> many1 int)
        fun combine f p1 p2 = f (p1 + Option.getOpt (p2, 0))
    in
        combine <$> sign <*> part1 <*> part2
    end



