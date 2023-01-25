type t =
  { line: int
  ; where: string
  ; message: string
  }
  [@@deriving show]

(* type 'a t = ('a, err) result *)

let report { line; where; message } =
  Format.printf "[line %d] Error%s: %s" line where message

