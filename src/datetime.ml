let lhs_regex = Str.regexp {|\(....\)\(..\)\(..\)\(..\)\(..\)\(..\)|}
let rhs_regex = Str.regexp {|\(..\)\(..\)|}
let rfc_regex = Str.regexp {|\(....\)-\(..\)-\(..\)T\(..\):\(..\):\(..\)|}
let colon_regex = Str.regexp ":"
let frac_regex = Str.regexp {|\.[0-9]*|}

let to_rfc3339 date =
  let lhs = Str.first_chars date 15 in
  let rhs = Str.string_after date 15 in
  Str.replace_first lhs_regex {|\1-\2-\3T\4:\5:\6|} lhs ^ Str.replace_first rhs_regex {|\1:\2|} rhs

let of_rfc3339 date =
  let lhs = Str.first_chars date 19 in
  let rhs = Str.string_after date 19 in
  Str.replace_first rfc_regex {|\1\2\3\4\5\6|} lhs ^ Str.global_replace colon_regex "" rhs
  |> Str.global_replace frac_regex ""

let to_ldap ptime_date = Ptime.to_rfc3339 ptime_date |> of_rfc3339

let of_ldap ldap_date =
  let (t, _, _) = to_rfc3339 ldap_date |> Ptime.of_rfc3339 |> Result.get_ok in
  t

let now () = Ptime_clock.now () |> to_ldap

let is_future date = Ptime.compare (of_ldap date) (Ptime_clock.now ()) > 0

let is_past date = Ptime.compare (of_ldap date) (Ptime_clock.now ()) < 0

let now_until date =
  Ptime.Span.sub (of_ldap date |> Ptime.to_span) (Ptime_clock.now () |> Ptime.to_span)
  |> Ptime.Span.to_int_s |> Option.get
