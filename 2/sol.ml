let time_it f =
  let start = Sys.time () in
  let res = f () in
  (res, (Sys.time ()) -. start)

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
  let parse_line line = 
    line
    |> String.split_on_char ' '
    |> List.filter_map (fun x -> int_of_string_opt x)
  in

  let rec create_report lines acc = 
    match lines with
    | [] -> List.rev acc
    | h :: t -> create_report t (parse_line h :: acc)
    in
  
  create_report lines []

let count_increasing lst = 
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | [_] -> acc
    | v1 :: v2 :: v when v1 > v2 -> aux (v2 :: v) (acc + 1)
    | v1 :: v2 :: v -> aux (v2 :: v) acc
  in
  aux lst 0

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
    let (lines, time) = time_it (fun () -> load_file filename) in
    Printf.printf "Read %d lines from %s in %fs\n%!" (List.length lines) filename time;
    let (reports, time) = time_it (fun () -> parse_lines lines) in
    Printf.printf "Parsed reports in %fs\n%!" time;
    let (sol1', time) = time_it (fun () -> sol1 reports) in
    Printf.printf "[1] %d in %fs\n%!" sol1' time;
    let (sol2', time) = time_it (fun () -> sol2 reports) in
    Printf.printf "[2] %d in %fs\n%!" sol2' time;

  else
    exit 1;
;;
