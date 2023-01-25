type t =
  { kind: TokenKind.t
  ; lexeme: string  (* not quite sure why we need this, but keeping it consistent with the book *)
  ; line: int (* the line number the lexeme was encountered *)
  }
  [@@deriving show, eq]

