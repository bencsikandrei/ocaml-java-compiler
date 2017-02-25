open Parser
open ExecuteProgram
open CompileTree

(* run the program *)
let execute lexbuf fname verbose dorun = 
  try 
    let ast = compilationUnit Lexer.token lexbuf in
    Log.init verbose;
    Log.debug "successfull parsing";
    Typing.typing ast;
    (* compile the AST with or without verbosity *)
    if dorun then 
      (let jprog = CompileTree.compile_tree verbose ast fname in
      Log.debug "successfull tree compilation";
      (* if verbose then AST.print_program ast; *)
      ExecuteProgram.execute_code verbose jprog;
      );
     
    ()
  with 
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l

