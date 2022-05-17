open GraphFile;;
(*open AST;;*)



let () =
    print_int (Array.length Sys.argv );
    match Array.length Sys.argv with 
    | 3  -> 
        if (Sys.argv.(1)="genconcat") then
            create_prog_from Sys.argv.(2)
    | 4 ->
        if (Sys.argv.(1)="genconcat") then
            apply_file Sys.argv.(2) Sys.argv.(3)
    | _ -> raise (Invalid_argument "unknown parameters")
