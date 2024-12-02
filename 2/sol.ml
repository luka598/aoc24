let load_file path =
  let rec load_lines channel acc = 
    match In_channel.input_line channel with
    | Some value -> load_lines channel (value :: acc);
    | None -> In_channel.close channel; List.rev acc;
  in

  let channel = In_channel.open_text path in
  load_lines channel []

let parse_lines lines =
(* (List.filter (fun x -> x <> "") (String.split_on_char ' ' value) *)
  let rec create_report lines acc = 
    match lines with
    | [] -> List.rev acc
    | h :: t ->
      create_report 
        t
        ((List.map (fun x -> int_of_string x) (List.filter (fun x -> x <> "") (String.split_on_char ' ' h))) :: acc)
    in
  
  create_report lines []

let test_report report =
  let increasing = (List.nth report 0) < (List.nth report 1) in
  let rec test values =
    match values with
    | [] -> true
    | [_] -> true
    | v1 :: v2 :: t when v1 > v2 && increasing -> false
    | v1 :: v2 :: t when v1 < v2 && not increasing -> false
    | v1 :: v2 :: t when (abs (v1 - v2) > 3) -> false
    | v1 :: v2 :: t when (abs (v1 - v2) < 1) -> false
    | v1 :: v2 :: t -> test (v2 :: t)
  in
    test report

let rec remove_nth n lst =
  List.filteri (fun i _ -> i <> n) lst

let sol1 reports = 
  List.fold_left (fun acc x -> acc + x) 0 (List.map (fun x -> Bool.to_int (test_report x)) reports)

let sol2 reports = 
  let test_report_any report = 
    let rec aux n safe = 
      match (n, safe) with
      | (-1, _) -> safe
      | (_, true) -> true
      | (_, false) -> aux (n - 1) (test_report (remove_nth n report) || safe)
    in
    aux (List.length report) (test_report report)
  in

  List.fold_left (fun acc x -> acc + x) 0 (List.map (fun x -> Bool.to_int (test_report_any x)) reports)



let () =
  if Array.length Sys.argv = 2 then
    let filename = Sys.argv.(1) in
    let lines = load_file filename in
    Printf.printf "Read %d lines from %s\n%!" (List.length lines) filename;
    let reports = parse_lines lines in
    Printf.printf "%d\n" (sol1 reports);
    Printf.printf "%d\n" (sol2 reports);

  else
    exit 1;
;;
