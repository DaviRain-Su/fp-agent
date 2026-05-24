open! Base

let test_hello () = Alcotest.(check string) "same string" "hello" "hello"

let () =
  Alcotest.run "Scaffold"
    [ ("basic", [ Alcotest.test_case "hello" `Quick test_hello ]) ]
