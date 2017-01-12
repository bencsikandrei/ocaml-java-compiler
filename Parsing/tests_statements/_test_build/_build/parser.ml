
module Basics = struct
  
  exception Error
  
  type token = 
    | XOREQUAL
    | XOR
    | WHILE
    | VOLATILE
    | VOID
    | TRY
    | TRANSIENT
    | THROWS
    | THROW
    | THIS
    | SYNCHRONIZED
    | SWITCH
    | SUPER
    | STRLIT of (string)
    | STRICTFP
    | STATIC
    | SQ
    | SHORT
    | SEMI
    | RSHIFTEQUAL
    | RSHIFT
    | RPAR
    | RETURN
    | RCURL
    | RBRAC
    | RANG
    | QM
    | PUBLIC
    | PROTECTED
    | PRIVATE
    | PLUS
    | PEQUAL
    | PACKAGE
    | OREQUAL
    | OR
    | NULLLIT of (string)
    | NOT
    | NEW
    | NEQUAL
    | NATIVE
    | MULEQUAL
    | MUL
    | MODEQUAL
    | MOD
    | MINUSEQUAL
    | MINUS
    | LTHAN
    | LSHIFTEQUAL
    | LSHIFT
    | LPAR
    | LONG
    | LOGSHIFTEQUAL
    | LOGSHIFT
    | LETHAN
    | LCURL
    | LBRAC
    | LANG
    | INTLIT of (int)
    | INTERFACE
    | INT
    | INSTANCEOF
    | INCREMENT
    | IMPORT
    | IMPLEMENTS
    | IF
    | IDENTIFIER of (string)
    | GTHAN
    | GOTO
    | GETHAN
    | FOR
    | FLOATLIT of (float)
    | FLOAT
    | FINALLY
    | FINAL
    | EXTENDS
    | EQUAL
    | EOF
    | ENUM
    | ELSE
    | ELIPSIS
    | DQ
    | DOUBLELIT of (float)
    | DOUBLE
    | DOT
    | DO
    | DIVEQUAL
    | DIV
    | DIM
    | DEFAULT
    | DECREMENT
    | CONTINUE
    | CONST
    | COMM
    | COL
    | CLASS
    | CHARLIT of (char)
    | CHAR
    | CATCH
    | CASE
    | BYTE
    | BREAK
    | BOR
    | BOOLEANLIT of (bool)
    | BOOLEAN
    | BNOT
    | BAND
    | ASSIGN
    | ASSERT
    | ANOT
    | ANDEQUAL
    | AND
    | ABSTRACT
  
end

include Basics

let _eRR =
  Basics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState122
  | MenhirState116
  | MenhirState114
  | MenhirState108
  | MenhirState105
  | MenhirState104
  | MenhirState101
  | MenhirState94
  | MenhirState81
  | MenhirState65
  | MenhirState64
  | MenhirState57
  | MenhirState51
  | MenhirState48
  | MenhirState44
  | MenhirState39
  | MenhirState33
  | MenhirState29
  | MenhirState27
  | MenhirState23
  | MenhirState20
  | MenhirState18
  | MenhirState16
  | MenhirState14
  | MenhirState10
  | MenhirState8
  | MenhirState7
  | MenhirState6
  | MenhirState4
  | MenhirState2
  | MenhirState0
  
	open Printf
	open Lexing


let rec _menhir_goto_localVariableDeclStmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (lvds : (string)) = _v in
        let _v : (string) =                             ( lvds ) in
        _menhir_goto_forInit _menhir_env _menhir_stack _menhir_s _v
    | MenhirState122 | MenhirState2 | MenhirState101 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (lvd : (string)) = _v in
        let _v : (string) =                            ( lvd ) in
        _menhir_goto_localVariableDeclOrStmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_reduce1 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (string) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let ((_menhir_stack, _menhir_s), _, (lvds : (string))) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (string) =                                             ( "{\n"^lvds^"\n}\n" ) in
    _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_variableDeclarators : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMM ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (ts : (string))), _, (vd : (string))) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (string) =                                               ( "final "^ts^" "^vd^";" ) in
            _menhir_goto_localVariableDeclStmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMM ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (ts : (string))), _, (vd : (string))) = _menhir_stack in
            let _3 = () in
            let _v : (string) =                                       ( ts^vd^";" ) in
            _menhir_goto_localVariableDeclStmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_localVariableDeclAndStmts : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOLEAN ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | BREAK ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | BYTE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | CASE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | CHAR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | CONTINUE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | DEFAULT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | DO ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | DOUBLE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | FINAL ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | FLOAT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | FOR ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | IDENTIFIER _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | INT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LCURL ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LONG ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | RCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState101 in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) _menhir_s
        | RETURN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SEMI ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SHORT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SWITCH ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SYNCHRONIZED ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | THROW ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TRY ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | VOID ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | WHILE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOLEAN ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | BREAK ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | BYTE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | CASE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | CHAR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | CONTINUE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | DEFAULT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | DO ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | DOUBLE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | FINAL ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | FLOAT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | FOR ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | IDENTIFIER _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | INT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | LCURL ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | LONG ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | RCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState122 in
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) _menhir_s
        | RETURN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | SEMI ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | SHORT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | SWITCH ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | SYNCHRONIZED ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | THROW ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | TRY ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | VOID ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | WHILE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
    | _ ->
        _menhir_fail ()

and _menhir_goto_variableDeclarator : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (vd : (string)) = _v in
        let (_menhir_stack, _menhir_s, (vds : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string) =                                                       ( vds^" , "^vd ) in
        _menhir_goto_variableDeclarators _menhir_env _menhir_stack _menhir_s _v
    | MenhirState57 | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (vd : (string)) = _v in
        let _v : (string) =                         ( vd ) in
        _menhir_goto_variableDeclarators _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_localVariableDeclOrStmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState2 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (lvd : (string)) = _v in
        let _v : (string) =                              ( lvd ) in
        _menhir_goto_localVariableDeclAndStmts _menhir_env _menhir_stack _menhir_s _v
    | MenhirState122 | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (lvd : (string)) = _v in
        let (_menhir_stack, _menhir_s, (lvds : (string))) = _menhir_stack in
        let _v : (string) =                                                               ( lvds^lvd ) in
        _menhir_goto_localVariableDeclAndStmts _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_catches : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CATCH ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | FINALLY ->
        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | BOOLEAN | BREAK | BYTE | CASE | CHAR | CONTINUE | DEFAULT | DO | DOUBLE | ELSE | FINAL | FLOAT | FOR | IDENTIFIER _ | IF | INT | LCURL | LONG | RCURL | RETURN | SEMI | SHORT | SWITCH | SYNCHRONIZED | THROW | TRY | VOID | WHILE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (b : (string))), _, (c : (string))) = _menhir_stack in
        let _1 = () in
        let _v : (string) =                          ( "try "^b^c ) in
        _menhir_goto_guardingStmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114

and _menhir_goto_guardingStmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (gs : (string)) = _v in
    let _v : (string) =                    ( gs ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run105 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LCURL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run107 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOLEAN ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | BYTE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | CHAR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | DOUBLE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | FLOAT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | INT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | LONG ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | SHORT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | VOID ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_selectStmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (ss : (string)) = _v in
    let _v : (string) =                   ( ss ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_iterStmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (is : (string)) = _v in
    let _v : (string) =                ( is ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_varInitializer : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (vi : (string)) = _v in
    let (_menhir_stack, _menhir_s, (dn : (string))) = _menhir_stack in
    let _2 = () in
    let _v : (string) =                                               ( dn^" = "^vi ) in
    _menhir_goto_variableDeclarator _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_catchHeader : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LCURL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (string)) = _v in
    let _v : (string) =                ( id ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState51 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RCURL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _2 = () in
                let _1 = () in
                let _v : (string) =                ( "{ }" ) in
                _menhir_goto_varInitializer _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | COMM | SEMI ->
            _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | COMM | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (dn : (string))) = _menhir_stack in
        let _v : (string) =                    ( dn ) in
        _menhir_goto_variableDeclarator _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_compilationUnit : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (string)) = _v in
    Obj.magic _1

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (fi : (string))), (fe : (string))), _, (s : (string))) = _menhir_stack in
        let _5 = () in
        let _2 = () in
        let _1 = () in
        let _v : (string) =                                                    ( "for("^fi^fe^")"^s ) in
        _menhir_goto_iterStmt _menhir_env _menhir_stack _menhir_s _v
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | CASE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | CONTINUE ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | DEFAULT ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | DO ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | FOR ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | IDENTIFIER _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LCURL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | RETURN ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | SEMI ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | SWITCH ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | SYNCHRONIZED ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | THROW ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | TRY ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | WHILE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | BOOLEAN | BREAK | BYTE | CASE | CHAR | CONTINUE | DEFAULT | DO | DOUBLE | FINAL | FLOAT | FOR | IDENTIFIER _ | IF | INT | LCURL | LONG | RCURL | RETURN | SEMI | SHORT | SWITCH | SYNCHRONIZED | THROW | TRY | VOID | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (string))), _, (s : (string))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (string) =                                                            ( "if("^e^") "^s ) in
            _menhir_goto_selectStmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (e : (string))), _, (s1 : (string))), _, (s2 : (string))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (string) =                                                             ( "if("^e^") "^s1^"\nelse "^s2 ) in
        _menhir_goto_selectStmt _menhir_env _menhir_stack _menhir_s _v
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (e : (string))), _, (s : (string))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (string) =                                                  ( "synchronized ("^e^") "^s ) in
        _menhir_goto_guardingStmt _menhir_env _menhir_stack _menhir_s _v
    | MenhirState122 | MenhirState2 | MenhirState101 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (stmt : (string))) = _menhir_stack in
        let _v : (string) =                   ( stmt ) in
        _menhir_goto_localVariableDeclOrStmt _menhir_env _menhir_stack _menhir_s _v
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (e : (string))), _, (s : (string))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (string) =                                           ( "while("^e^")"^s ) in
        _menhir_goto_iterStmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_block : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (e : (string))), _, (b : (string))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (string) =                                          ( "switch ("^e^") "^b ) in
        _menhir_goto_selectStmt _menhir_env _menhir_stack _menhir_s _v
    | MenhirState2 | MenhirState122 | MenhirState6 | MenhirState8 | MenhirState101 | MenhirState16 | MenhirState29 | MenhirState94 | MenhirState64 | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (b : (string))) = _menhir_stack in
        let _v : (string) =            ( b ) in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CATCH ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | FINALLY ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (b : (string))) = _menhir_stack in
        let _1 = () in
        let _v : (string) =                  ( "finally "^b ) in
        (match _menhir_s with
        | MenhirState104 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (f : (string)) = _v in
            let ((_menhir_stack, _menhir_s), _, (b : (string))) = _menhir_stack in
            let _1 = () in
            let _v : (string) =                          ( "try "^b^f ) in
            _menhir_goto_guardingStmt _menhir_env _menhir_stack _menhir_s _v
        | MenhirState114 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (f : (string)) = _v in
            let (((_menhir_stack, _menhir_s), _, (b : (string))), _, (c : (string))) = _menhir_stack in
            let _1 = () in
            let _v : (string) =                                    ( "try "^b^c^f ) in
            _menhir_goto_guardingStmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            _menhir_fail ())
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (ch : (string))), _, (b : (string))) = _menhir_stack in
        let _v : (string) =                         ( ch^b ) in
        (match _menhir_s with
        | MenhirState114 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (c : (string)) = _v in
            let (_menhir_stack, _menhir_s, (cs : (string))) = _menhir_stack in
            let _v : (string) =                       ( cs^c ) in
            _menhir_goto_catches _menhir_env _menhir_stack _menhir_s _v
        | MenhirState104 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (c : (string)) = _v in
            let _v : (string) =          ( c ) in
            _menhir_goto_catches _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            _menhir_fail ())
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (s : (string))) = _menhir_stack in
        let _v : (string) =          ( s ) in
        _menhir_goto_compilationUnit _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (string) =  ( " |some expression| " ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | CASE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | CONTINUE ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | DEFAULT ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | DO ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | FOR ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | IDENTIFIER _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | LCURL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | RETURN ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | SEMI ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | SWITCH ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | SYNCHRONIZED ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | THROW ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | TRY ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | WHILE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (string))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (string) =                            ( "throw "^e^"; " ) in
            _menhir_goto_jumpStmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | CASE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | CONTINUE ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | DEFAULT ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | DO ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | FOR ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | IDENTIFIER _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | LCURL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | RETURN ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | SEMI ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | SWITCH ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | SYNCHRONIZED ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | THROW ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | TRY ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | WHILE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LCURL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (string))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (string) =                             ( "return "^e^"; "  ) in
            _menhir_goto_jumpStmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | CASE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | CONTINUE ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | DEFAULT ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | DO ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | FOR ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | IDENTIFIER _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LCURL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | RETURN ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | SEMI ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | SWITCH ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | SYNCHRONIZED ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | THROW ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | TRY ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | WHILE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (e : (string))) = _menhir_stack in
        let _v : (string) =               ( e ) in
        _menhir_goto_varInitializer _menhir_env _menhir_stack _menhir_s _v
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (s : (string))), _, (e : (string))) = _menhir_stack in
                let _7 = () in
                let _6 = () in
                let _4 = () in
                let _3 = () in
                let _1 = () in
                let _v : (string) =                                                     ( "do "^s^" while ("^e^"); ") in
                _menhir_goto_iterStmt _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_forInit : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (string) =       ( ";" ) in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | CASE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | CONTINUE ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | DEFAULT ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | DO ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | FOR ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | IDENTIFIER _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | LCURL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | RETURN ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | SEMI ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | SWITCH ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | SYNCHRONIZED ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | THROW ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | TRY ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | WHILE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_labelStmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (ls : (string)) = _v in
    let _v : (string) =                 ( ls ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_jumpStmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (js : (string)) = _v in
    let _v : (string) =                ( js ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_types : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
    | MenhirState122 | MenhirState2 | MenhirState101 | MenhirState8 | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (ts : (string))), (id : (string))) = _menhir_stack in
                let _5 = () in
                let _2 = () in
                let _1 = () in
                let _v : (string) =                                         ( "catch ( "^ts^id^" ) ") in
                _menhir_goto_catchHeader _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (ts : (string))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (string) =                             ( "catch ( "^ts^" ) " ) in
            _menhir_goto_catchHeader _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (string) =          ( " an error has occured\n" ) in
        _menhir_goto_compilationUnit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =         ( "void " ) in
    _menhir_goto_types _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LCURL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =          ( "short " ) in
    _menhir_goto_types _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =       ( ";" ) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (es : (string)) = _v in
    let _v : (string) =               ( es ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_reduce2 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _2 = () in
    let _1 = () in
    let _v : (string) =                ( "{\n \n}\n" ) in
    _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =         ( "long " ) in
    _menhir_goto_types _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEAN ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | BREAK ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | BYTE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | CASE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | CHAR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | CONTINUE ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | DEFAULT ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | DO ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | DOUBLE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | FINAL ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | FLOAT ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | FOR ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | INT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LCURL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LONG ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | RCURL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState8 in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | RETURN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | SEMI ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | SHORT ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | SWITCH ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | SYNCHRONIZED ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | THROW ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TRY ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | VOID ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | WHILE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =        ( "int " ) in
    _menhir_goto_types _menhir_env _menhir_stack _menhir_s _v

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string) =                    ( id^" : " ) in
        _menhir_goto_labelStmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOLEAN ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | BYTE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | CHAR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | DOUBLE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | FINAL ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | FLOAT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | INT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | LONG ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState33 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (string) =         ( ";" ) in
            _menhir_goto_forInit _menhir_env _menhir_stack _menhir_s _v
        | SHORT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | VOID ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =          ( "float " ) in
    _menhir_goto_types _menhir_env _menhir_stack _menhir_s _v

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEAN ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | BYTE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | CHAR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | DOUBLE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | FLOAT ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | INT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LONG ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | SHORT ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | VOID ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =           ( "double " ) in
    _menhir_goto_types _menhir_env _menhir_stack _menhir_s _v

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | CASE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | CONTINUE ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | DEFAULT ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | DO ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | FOR ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LCURL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | RETURN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | SEMI ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | SWITCH ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | SYNCHRONIZED ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | THROW ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TRY ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | WHILE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (string) =                ( "default : " ) in
        _menhir_goto_labelStmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (id : (string))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (string) =                                   ( "continue "^id^"; ") in
            _menhir_goto_jumpStmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (string) =                  ( "continue;") in
        _menhir_goto_jumpStmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =          ( "char " ) in
    _menhir_goto_types _menhir_env _menhir_stack _menhir_s _v

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (string) =  ( " |some constant expression| " ) in
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), (ce : (string))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (string) =                                   ( "case "^ce^": " ) in
        _menhir_goto_labelStmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =         ( "byte " ) in
    _menhir_goto_types _menhir_env _menhir_stack _menhir_s _v

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (id : (string))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (string) =                           ( "break "^id^"; " ) in
            _menhir_goto_jumpStmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (string) =               ( "break;" ) in
        _menhir_goto_jumpStmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =          ( "boolean " ) in
    _menhir_goto_types _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and compilationUnit : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (string) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LCURL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOLEAN ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | BREAK ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | BYTE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | CASE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | CHAR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | CONTINUE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DEFAULT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DO ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DOUBLE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | FINAL ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | FLOAT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | FOR ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | IDENTIFIER _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | INT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LCURL ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LONG ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | RCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState2 in
            _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) _menhir_s
        | RETURN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SEMI ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SHORT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SWITCH ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SYNCHRONIZED ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | THROW ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | TRY ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | VOID ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | WHILE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  
let parse_error s = 
	print_endline s;
	flush stdout
  

