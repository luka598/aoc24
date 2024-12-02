let load_file path =
  let rec load_lines channel acc = 
    match In_channel.input_line channel with
    | Some value -> load_lines channel ((List.filter (fun x -> x <> "") (String.split_on_char ' ' value)) :: acc);
    | None -> In_channel.close channel; acc;
  in

  let channel = In_channel.open_text path in
  load_lines channel []

let lists lines =
  let rec create_lists lines left_acc right_acc = 
    match lines with
    | [] -> (left_acc, right_acc)
    | (l :: r :: _)::t ->
      create_lists t (int_of_string l :: left_acc) (int_of_string r :: right_acc)
    | _ -> exit 1;
    in
  
  create_lists lines [] []

let sol1 left right =
  let left = (List.sort (fun a b -> a - b) left) in
  let right = (List.sort (fun a b -> a - b) right) in
  let rec sol l r acc =
    match (l, r) with
    | (lh :: lt, rh :: rt) -> sol lt rt (acc + (abs (lh-rh)))
    | ([], []) -> acc
    | _ -> exit 2;
  in

  sol left right 0

let sol2 left right = 
  let rec sol l acc =
    match l with
    | [] -> acc
    | h :: t -> sol t (acc + h * List.length (List.filter (fun x -> x == h) right))
  in
  sol left 0

let () =
  if Array.length Sys.argv = 2 then
    let filename = Sys.argv.(1) in
    let lines = load_file filename in
    let (left, right) = lists lines in
    Printf.printf "%d\n" (sol1 left right);
    Printf.printf "%d\n" (sol2 left right);

  else
    exit 1;
;;