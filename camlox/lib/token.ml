type t =
  { kind: TokenKind.t
  ; col: int
  ; line: int (* the line number the lexeme was encountered *)
  }
  [@@deriving show, eq]

