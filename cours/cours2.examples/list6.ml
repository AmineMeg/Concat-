let rec of_list = function
  | [] -> None
  | a::r -> (* insert a (of_list r) *) (* ou bien: *)
     let o = of_list r in
     let c = Some {info=a; prev=None; next=o} in
     match o with
     | None -> c
     | Some c' -> c'.prev <- c; c ;;
let rec toutagauche = function
  | None -> None
  | Some {prev=None} as c -> c
  | Some {prev=c} -> toutagauche c ;;
let to_list o =
  let rec build acc = function
    | None -> List.rev acc
    | Some {info=a;next=c} -> build (a::acc) c
  in build [] (toutagauche o) ;;
