(** This module defines basic string operations to build LDAP filters *)

(** Format an operation into an LDAP filter *)
let format_filter op attr value = "(" ^ attr ^ op ^ value ^ ")"
let format_op op left right = "(" ^ op ^ left ^ right ^ ")"

(** Standard LDAP operators *)
let (=^) = format_filter "="
let (>=^) = format_filter ">="
let (<=^) = format_filter "<="
let (=~^) = format_filter "~="
let (&^) = format_op "&"
let (|^) = format_op "|"
let (!^) filter = "(!" ^ filter ^ ")"

(** N-ary operators *)
let all filters = "(&" ^ List.fold_left (^) "" filters ^ ")"
let one filters = "(|" ^ List.fold_left (^) "" filters ^ ")"

(** Combined operators *)
let (>^) left right = !^ (left <=^ right)
let (<^) left right = !^ (left >=^ right)
