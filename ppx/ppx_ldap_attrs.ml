(** This module defines a PPX to define LDAP attributes *)

open Ppxlib
open Ast_builder.Default

(** Build the bindings for LDAP attributes *)
let build_binding loc identifiers =
  let binding_of_identifier id =
    let loc = id.pexp_loc in
    match id.pexp_desc with
      | Pexp_ident {txt = Lident name; _} ->
          value_binding ~loc ~pat:(ppat_var ~loc {txt = name; loc} ) ~expr:(estring ~loc name)
      | _ ->
          Location.raise_errorf ~loc
                                "`ldap_filters' requires an identifier or a tuple of identifiers" in
  pstr_value ~loc Nonrecursive (List.map binding_of_identifier identifiers)

(** Rewrite the attribute declarations *)
let rewrite_ldap_attrs expr =
  let loc = expr.pexp_loc in
  build_binding loc (match expr.pexp_desc with
    | Pexp_ident _ -> [expr]
    | Pexp_tuple tup -> tup
    | _ -> Location.raise_errorf ~loc
                                 "`ldap_filters' requires an identifier or a tuple of identifiers")

(** Declare the extension *)
let mapper =
  Extension.declare "attrs" Extension.Context.structure_item
    Ast_pattern.(pstr (pstr_eval __ __ ^:: nil))
    (fun ~loc:_ ~path:_ expr _ -> rewrite_ldap_attrs expr)

(** Register the transformation *)
let () = Driver.register_transformation "ldap_attrs" ~extensions:[mapper]
