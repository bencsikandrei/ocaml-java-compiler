open Parser
open ExecuteProgramA
open CompileTree

(* run the program *)
let execute lexbuf fname verbose = 
  try 
    let ast = compilationUnit Lexer.token lexbuf in
    Log.debug verbose "successfull parsing";
    (* compile the AST with or without verbosity *)
    let jprog = CompileTree.compile_tree verbose ast fname in
    Log.debug verbose "successfull tree compilation";
    (* if verbose then AST.print_program ast; *)
    ExecuteProgramA.execute_code jprog;
    ()
  with 
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
