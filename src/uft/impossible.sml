structure Impossible = struct
  exception Impossible of string
  fun impossible msg =
      let open TextIO
          fun shout s = (output (stdErr, s); flushOut stdErr) 
      in  app shout  ["Internal error: impossible ", msg, "\n"];
          raise (Impossible msg)
      end
  fun unimp msg = impossible (msg ^ " not implemented")
end
