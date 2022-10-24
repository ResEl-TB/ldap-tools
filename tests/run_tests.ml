(** This module tests the PPX with Alcotest *)

open Alcotest
open Ldap_tools.Ldap_filter
open Ldap_tools.Datetime

[%%attrs objectClass, id, givenName]

(** A check for choice filters *)
let test_choices () =
  check string "=" "(objectClass=person)" (objectClass =^ "person");
  check string ">=" "(id>=100)" (id >=^ "100");
  check string "<=" "(id<=100)" (id <=^ "100");
  check string "~=" "(givenName~=John)" (givenName =~^ "John")

(** A check for nested filters *)
let test_nested () =
  check string "&" "(&(objectClass=person)(id=100))" ((objectClass =^ "person") &^ (id =^ "100"));
  check string "|" "(|(id=90)(id=100))" ((id =^ "90") |^ (id =^ "100"));
  check string "!" "(!(givenName=Admin))" (!^ (givenName =^ "Admin"));
  check string "!|" "(!(|(id=90)(id=100)))" (!^ ((id =^ "90") |^ (id =^ "100")))

(** A check for nested N-ary filters *)
let test_nary () =
  check string "all" "(&(objectClass=person)(id=100)(givenName=John))"
               (all [objectClass =^ "person"; id =^ "100"; givenName =^ "John"]);
  check string "one" "(|(id=80)(id=90)(id=100))" (one [id =^ "80"; id =^ "90"; id =^ "100"])

(** A check for complementary choice filters *)
let test_complem () =
  check string ">" "(!(id<=100))" (id >^ "100");
  check string "<" "(!(id>=100))" (id <^ "100")

let filter_tests = [
  ("choices", `Quick, test_choices);
  ("nested", `Quick, test_nested);
  ("nary", `Quick, test_nary);
  ("complem", `Quick, test_complem)
]

(** A check for datetime comparisons *)
let compare_dates () =
  check bool "same timezone" true (is_past "19700101000000Z")

(** A check for datetime conversions *)
let convert_dates () =
  check string "to_rfc3339" "2020-01-02T03:04:05+06:00" (to_rfc3339 "20200102030405+0600");
  check string "of_rfc3339" "20200102030405+0600" (of_rfc3339 "2020-01-02T03:04:05+06:00")

(** A check for datetime difference *)
let check_diff () =
  check bool "is close" true (now_until @@ now () |> abs |> (>=) 1);
  let before = now () in
  Unix.sleep 5;
  check bool "is close" true (now_until before + 5 |> abs |> (>=) 1)

let datetime_tests = [
  ("compare dates", `Quick, compare_dates);
  ("convert dates", `Quick, convert_dates);
  ("check diff", `Quick, check_diff)
]

let test_suites: unit test list = [
  "Filter", filter_tests;
  "Datetime", datetime_tests;
]

(** Run the test suites *)
let () = run "ldap_filter" test_suites
