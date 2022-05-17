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
            apply_file
    | _ -> raise (Invalid_argument "unknown parameters")
        
        (**
  match Sys.argv with 
    | [||] -> raise IllegalArgument ("No params")
    | h::t when h="genconcat" -> 
        if ((Array.length t) = 1) then
            create_prog_from Sys.argv.(0)
        else if ((Array.length t) =2 ) then 
            apply_file 
        else 
            raise IllegalArgument ("Trop de fichiers")
    | h::t -> raise IllegalArgument ("L'option n'existe pas")



    print_endline "Hello, World!";
    let file = "exemples/etape1/date_verbose"
    in
    print_file file;
    print_string "APRES DIJKSTRA \n";
    create_prog_from file ;;

    create_prog_from <fichier>
        apply_file <fichier entrer> <program>*)