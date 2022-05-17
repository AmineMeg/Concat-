open FileTreatment;;
open GraphFile;;
(*open AST;;*)

let () =
    print_endline "Hello, World!";
    let file_prog = "exemples/etape1/date_to_month"
    in
    let file_input = "exemples/etape1/input_date_to_month"
    in
    print_file file_prog;
    let prog = create_prog_from file_prog 
    in 
    print_string (apply_file file_input prog);;