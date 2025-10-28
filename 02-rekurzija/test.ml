let rec reverse l =
  let rec aux l1 acc1 =
    match l1 with
    | [] -> acc1
    | x :: xs -> (
      aux xs (x::acc1)
    )
  in
  aux l []

  let apply_sequence f x n =
    if n < 0 then []
    else
      let rec aux x1 n1 acc1 =
        match n1 with
        | 0 -> List.rev acc1
        | _ -> aux (f x1) (n1 - 1) (x1 :: acc1)
      in
      aux x n []
  ;;
  