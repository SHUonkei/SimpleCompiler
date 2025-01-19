let main () =
  (* ファイルを開く *)
  let cin =
    if Array.length Sys.argv > 1
    then open_in Sys.argv.(1)
    else stdin in
    let lexbuf = Lexing.from_channel cin in
    try
       (* 生成コード用ファイルtmp.sをオープン *)
       let file = open_out "tmp.s" in                           
          (* コード生成 *)
          let code = Emitter.trans_prog (Parser.prog Lexer.lexer lexbuf) in
             (* 生成コードの書出しとファイルのクローズ *)
             output_string file code; close_out file;     
             (* アセンブラとリンカの呼出し *)
             let _ = Unix.system "gcc tmp.s" in ()
    with
    | Parsing.Parse_error ->
        let pos = lexbuf.Lexing.lex_curr_p in
        Printf.eprintf "Syntax error at line %d, near token '%s'\n"
        (pos.Lexing.pos_lnum - 1)
        (Lexing.lexeme lexbuf);
        exit 1  
    | Table.No_such_symbol x -> print_string ("no such symbol: \""^x^"\"\n")
    | Semant.TypeErr s -> print_string (s^"\n")
    | Semant.Err s -> print_string (s^"\n")
    | Table.SymErr s -> print_string (s^"\n")

let _ = main ()