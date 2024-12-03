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

type token =
  | Number of int
  | Comma of unit
  | Char of unit
  | Mul of unit
  | Dont of unit
  | Do of unit
  | L_p of unit
  | R_p of unit


let tokenize prog =
  let prog = (List.of_seq (String.to_seq prog)) in
  let length = List.length prog in

  let get_c n =
    if n >= length then '\x00' else
    List.nth prog n
  in

  let rec capture_number h acc =
    if h >= length then (h, Number acc) else

    let c = get_c h in
    match c with
    | '0'..'9' -> capture_number (h+1) ((int_of_char c - 48) + (acc * 10))
    | _ -> (h, Number acc)
  in

  let rec aux h acc = 
    if h >= length then List.rev acc else

    let c = get_c h in
    match c with
    | '0'..'9' -> 
      let (new_h, s) = capture_number h 0 in
      aux new_h (s :: acc)

    | '(' -> 
      aux (h + 1) (L_p () :: acc)

    | ')' -> 
      aux (h + 1) (R_p () :: acc)

    | ',' -> 
      aux (h + 1) (Comma () :: acc)
    
    | 'm' when (get_c (h+1)) == 'u' && (get_c (h+2)) == 'l' ->
      aux (h+3) (Mul () :: acc)

    | 'd' when (get_c (h+1)) == 'o' && (get_c (h+2)) == 'n' && (get_c (h+3)) == '\'' && (get_c (h+4)) == 't' ->
      aux (h+5) (Dont () :: acc)

    | 'd' when (get_c (h+1)) == 'o' ->
      aux (h+2) (Do () :: acc)

    | _ ->
      aux (h+1) (Char () :: acc)

  in

  aux 0 []

let sol1 prog = 
  let rec do_mul tokens acc = 
    match tokens with
    | [] -> 
      acc

    | Mul _ :: L_p _ :: Number v1 :: Comma _ :: Number v2 :: R_p _ :: t ->
      do_mul t ((v1 * v2) + acc); 

    | h :: t -> 
      do_mul t acc
  in

  do_mul prog 0


let sol2 prog = 
  let rec do_mul acc x tokens = 
    match tokens with
    | [] -> 
      acc

    | Mul _ :: L_p _ :: Number v1 :: Comma _ :: Number v2 :: R_p _ :: t ->
      do_mul ((v1 * v2 * x) + acc) x t 
    
    | Do _ :: L_p _ :: R_p _ :: t ->
      do_mul acc 1 t

    | Dont _ :: L_p _ :: R_p _ :: t ->
      do_mul acc 0 t 

    | h :: t -> 
      do_mul acc x t
  in

  
  do_mul 0 1 prog

let () =
  if Array.length Sys.argv = 2 then
    let filename = Sys.argv.(1) in
    let (lines, time) = time_it (fun () -> load_file filename) in
    Printf.printf "Read 1 line from %s in %fs\n%!" filename time;

    let prog = List.fold_left (fun acc x -> acc ^ x) "" lines in

    let (tokens, time) = time_it (fun () -> tokenize prog) in
    Printf.printf "Tokenized in %fs\n%!"  time;

    let (sol1', time) = time_it (fun () -> sol1 tokens) in
    Printf.printf "[1] %d in %fs\n%!" sol1' time;

    let (sol2', time) = time_it (fun () -> sol2 tokens) in
    Printf.printf "[2] %d in %fs\n%!" sol2' time;

  else
    exit 1;
;;
