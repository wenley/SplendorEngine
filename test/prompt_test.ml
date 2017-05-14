
let test_cases : (string * Action.action option) list = ("foo", None)
  :: ("B11", Some (Action.Buy (Board.One, 1)))
  :: ("2", None)
  :: ("2K", Some (Action.Two (Data.Black)))
  :: ("2KK", None)
  :: ("3KKK", Some (Action.Three (Data.Black, Data.Black, Data.Black)))
  :: ("3CMY", None)
  :: ("3", None)
  :: []

let print_failure input actual expected =
  Printf.printf "Test case failed: %s\n" input;
  Printf.printf "Expected: %s\n" expected;
  Printf.printf "  Actual: %s\n" actual

let print_compare (input:string) (expected:Action.action option) : unit =
  let actual = Prompt.parse_action input in
  let to_string action =
    match action with
    | None -> "None"
    | Some action -> Action.verbose_string_of_action action
  in
  match actual = expected with
  | true -> Printf.printf "Test case passed! %s\n" input
  | false -> print_failure input (to_string actual) (to_string expected)
;;

let run_tests () : unit =
  let run_test (test_case) =
    let input, expected = test_case in
    print_compare input expected
  in
  List.iter run_test test_cases
in run_tests ()
;;
