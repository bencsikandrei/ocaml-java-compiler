
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
  | MenhirState447
  | MenhirState441
  | MenhirState439
  | MenhirState436
  | MenhirState435
  | MenhirState433
  | MenhirState428
  | MenhirState425
  | MenhirState424
  | MenhirState422
  | MenhirState420
  | MenhirState417
  | MenhirState414
  | MenhirState413
  | MenhirState412
  | MenhirState398
  | MenhirState395
  | MenhirState383
  | MenhirState377
  | MenhirState371
  | MenhirState368
  | MenhirState365
  | MenhirState364
  | MenhirState362
  | MenhirState346
  | MenhirState341
  | MenhirState338
  | MenhirState329
  | MenhirState328
  | MenhirState326
  | MenhirState324
  | MenhirState320
  | MenhirState318
  | MenhirState313
  | MenhirState309
  | MenhirState307
  | MenhirState306
  | MenhirState304
  | MenhirState302
  | MenhirState300
  | MenhirState296
  | MenhirState292
  | MenhirState290
  | MenhirState284
  | MenhirState281
  | MenhirState280
  | MenhirState279
  | MenhirState278
  | MenhirState276
  | MenhirState274
  | MenhirState273
  | MenhirState271
  | MenhirState265
  | MenhirState262
  | MenhirState259
  | MenhirState255
  | MenhirState254
  | MenhirState252
  | MenhirState248
  | MenhirState246
  | MenhirState243
  | MenhirState239
  | MenhirState233
  | MenhirState229
  | MenhirState225
  | MenhirState221
  | MenhirState220
  | MenhirState218
  | MenhirState216
  | MenhirState214
  | MenhirState210
  | MenhirState209
  | MenhirState208
  | MenhirState207
  | MenhirState202
  | MenhirState198
  | MenhirState194
  | MenhirState191
  | MenhirState188
  | MenhirState186
  | MenhirState183
  | MenhirState176
  | MenhirState169
  | MenhirState157
  | MenhirState153
  | MenhirState149
  | MenhirState147
  | MenhirState145
  | MenhirState143
  | MenhirState141
  | MenhirState138
  | MenhirState135
  | MenhirState134
  | MenhirState130
  | MenhirState128
  | MenhirState126
  | MenhirState124
  | MenhirState121
  | MenhirState118
  | MenhirState115
  | MenhirState113
  | MenhirState107
  | MenhirState105
  | MenhirState99
  | MenhirState97
  | MenhirState95
  | MenhirState93
  | MenhirState91
  | MenhirState89
  | MenhirState83
  | MenhirState82
  | MenhirState80
  | MenhirState77
  | MenhirState74
  | MenhirState61
  | MenhirState59
  | MenhirState57
  | MenhirState43
  | MenhirState42
  | MenhirState33
  | MenhirState30
  | MenhirState29
  | MenhirState27
  | MenhirState24
  | MenhirState23
  | MenhirState14
  | MenhirState5
  | MenhirState2
  | MenhirState0
  
	open Printf
	open Lexing
	open Expressions

let rec _menhir_goto_assignmentExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState2 | MenhirState447 | MenhirState5 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState214 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState246 | MenhirState248 | MenhirState420 | MenhirState412 | MenhirState413 | MenhirState417 | MenhirState414 | MenhirState271 | MenhirState273 | MenhirState395 | MenhirState398 | MenhirState274 | MenhirState383 | MenhirState276 | MenhirState278 | MenhirState300 | MenhirState302 | MenhirState318 | MenhirState320 | MenhirState377 | MenhirState362 | MenhirState364 | MenhirState371 | MenhirState368 | MenhirState365 | MenhirState326 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState338 | MenhirState341 | MenhirState313 | MenhirState304 | MenhirState296 | MenhirState262 | MenhirState239 | MenhirState218 | MenhirState210 | MenhirState198 | MenhirState24 | MenhirState27 | MenhirState30 | MenhirState43 | MenhirState169 | MenhirState77 | MenhirState89 | MenhirState105 | MenhirState157 | MenhirState107 | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ae : (string)) = _v in
        let _v : (string) =                          ( ae ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ae : (string)) = _v in
        let ((_menhir_stack, _menhir_s, (ue : (string))), (ass : (string))) = _menhir_stack in
        let _v : (string) =                                                                      ( ue^ass^ae ) in
        _menhir_goto_assignmentExpression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_conditionalExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ce : (string)) = _v in
        let ((_menhir_stack, _menhir_s, (cor : (string))), _, (ex : (string))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (string) =                                                                              ( cor^" ? "^ex^" : "^ce ) in
        _menhir_goto_conditionalExpression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState2 | MenhirState447 | MenhirState5 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState214 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState246 | MenhirState248 | MenhirState420 | MenhirState412 | MenhirState413 | MenhirState417 | MenhirState414 | MenhirState271 | MenhirState273 | MenhirState395 | MenhirState398 | MenhirState274 | MenhirState383 | MenhirState276 | MenhirState278 | MenhirState300 | MenhirState302 | MenhirState318 | MenhirState320 | MenhirState377 | MenhirState362 | MenhirState364 | MenhirState371 | MenhirState368 | MenhirState365 | MenhirState326 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState338 | MenhirState341 | MenhirState313 | MenhirState304 | MenhirState296 | MenhirState262 | MenhirState239 | MenhirState218 | MenhirState210 | MenhirState198 | MenhirState24 | MenhirState27 | MenhirState30 | MenhirState43 | MenhirState57 | MenhirState169 | MenhirState77 | MenhirState89 | MenhirState105 | MenhirState157 | MenhirState107 | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ce : (string)) = _v in
        let _v : (string) =                           ( ce ) in
        _menhir_goto_assignmentExpression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState225 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ce : (string)) = _v in
        let _v : (string) =                           ( ce ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (ce : (string))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (string) =                                 ( "case "^ce^" :" ) in
            _menhir_goto_switchLabel _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_conditionalOrExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
    | QM ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | COL | COMM | RBRAC | RCURL | RPAR | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (cor : (string))) = _menhir_stack in
        let _v : (string) =                              ( cor ) in
        _menhir_goto_conditionalExpression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run118 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run99 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_goto_conditionalAndExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 | MenhirState447 | MenhirState5 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState214 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState246 | MenhirState248 | MenhirState420 | MenhirState412 | MenhirState413 | MenhirState417 | MenhirState414 | MenhirState271 | MenhirState273 | MenhirState395 | MenhirState398 | MenhirState274 | MenhirState383 | MenhirState276 | MenhirState278 | MenhirState300 | MenhirState302 | MenhirState318 | MenhirState320 | MenhirState377 | MenhirState362 | MenhirState364 | MenhirState371 | MenhirState368 | MenhirState365 | MenhirState326 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState338 | MenhirState341 | MenhirState313 | MenhirState304 | MenhirState296 | MenhirState262 | MenhirState239 | MenhirState218 | MenhirState225 | MenhirState210 | MenhirState198 | MenhirState24 | MenhirState27 | MenhirState30 | MenhirState43 | MenhirState57 | MenhirState169 | MenhirState77 | MenhirState89 | MenhirState105 | MenhirState157 | MenhirState107 | MenhirState113 | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | COL | COMM | OR | QM | RBRAC | RCURL | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (cand : (string))) = _menhir_stack in
            let _v : (string) =                                ( cand ) in
            _menhir_goto_conditionalOrExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | COL | COMM | OR | QM | RBRAC | RCURL | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (cor : (string))), _, (cand : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                                 ( cor^" || "^cand ) in
            _menhir_goto_conditionalOrExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_goto_shiftExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState447 | MenhirState2 | MenhirState207 | MenhirState433 | MenhirState209 | MenhirState216 | MenhirState428 | MenhirState243 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState413 | MenhirState414 | MenhirState412 | MenhirState273 | MenhirState398 | MenhirState395 | MenhirState274 | MenhirState383 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState371 | MenhirState368 | MenhirState364 | MenhirState365 | MenhirState362 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState341 | MenhirState338 | MenhirState326 | MenhirState318 | MenhirState313 | MenhirState304 | MenhirState300 | MenhirState296 | MenhirState276 | MenhirState271 | MenhirState262 | MenhirState246 | MenhirState239 | MenhirState225 | MenhirState218 | MenhirState214 | MenhirState210 | MenhirState5 | MenhirState198 | MenhirState24 | MenhirState27 | MenhirState30 | MenhirState43 | MenhirState169 | MenhirState105 | MenhirState157 | MenhirState153 | MenhirState126 | MenhirState124 | MenhirState118 | MenhirState115 | MenhirState113 | MenhirState107 | MenhirState95 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState77 | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LOGSHIFT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | RSHIFT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | GETHAN | GTHAN | INSTANCEOF | LETHAN | LTHAN | NEQUAL | OR | QM | RBRAC | RCURL | RPAR | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (sh : (string))) = _menhir_stack in
            let _v : (string) =                     ( sh ) in
            _menhir_goto_relationalExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LOGSHIFT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | RSHIFT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | GETHAN | GTHAN | INSTANCEOF | LETHAN | LTHAN | NEQUAL | OR | QM | RBRAC | RCURL | RPAR | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (rel : (string))), _, (sh : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                      ( rel^" < "^sh ) in
            _menhir_goto_relationalExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LOGSHIFT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | RSHIFT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | GETHAN | GTHAN | INSTANCEOF | LETHAN | LTHAN | NEQUAL | OR | QM | RBRAC | RCURL | RPAR | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (rel : (string))), _, (sh : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                       ( rel^" <= "^sh ) in
            _menhir_goto_relationalExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LOGSHIFT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | RSHIFT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | GETHAN | GTHAN | INSTANCEOF | LETHAN | LTHAN | NEQUAL | OR | QM | RBRAC | RCURL | RPAR | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (rel : (string))), _, (sh : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                      ( rel^" > "^sh ) in
            _menhir_goto_relationalExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LOGSHIFT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | RSHIFT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | GETHAN | GTHAN | INSTANCEOF | LETHAN | LTHAN | NEQUAL | OR | QM | RBRAC | RCURL | RPAR | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (rel : (string))), _, (sh : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                       ( rel^" >= "^sh ) in
            _menhir_goto_relationalExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run141 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141

and _menhir_run147 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147

and _menhir_goto_inclusiveOrExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 | MenhirState447 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState413 | MenhirState414 | MenhirState412 | MenhirState273 | MenhirState398 | MenhirState395 | MenhirState274 | MenhirState383 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState371 | MenhirState368 | MenhirState364 | MenhirState365 | MenhirState362 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState341 | MenhirState338 | MenhirState326 | MenhirState318 | MenhirState313 | MenhirState304 | MenhirState300 | MenhirState296 | MenhirState276 | MenhirState271 | MenhirState262 | MenhirState246 | MenhirState239 | MenhirState225 | MenhirState218 | MenhirState214 | MenhirState210 | MenhirState5 | MenhirState198 | MenhirState24 | MenhirState27 | MenhirState30 | MenhirState43 | MenhirState57 | MenhirState169 | MenhirState77 | MenhirState105 | MenhirState157 | MenhirState153 | MenhirState115 | MenhirState113 | MenhirState107 | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | AND | COL | COMM | OR | QM | RBRAC | RCURL | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (ior : (string))) = _menhir_stack in
            let _v : (string) =                            ( ior ) in
            _menhir_goto_conditionalAndExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | AND | COL | COMM | OR | QM | RBRAC | RCURL | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (cand : (string))), _, (ior : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                                ( cand^"  && "^ior ) in
            _menhir_goto_conditionalAndExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run93 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_goto_additiveExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 | MenhirState447 | MenhirState5 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState214 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState246 | MenhirState248 | MenhirState420 | MenhirState412 | MenhirState413 | MenhirState417 | MenhirState414 | MenhirState271 | MenhirState273 | MenhirState395 | MenhirState398 | MenhirState274 | MenhirState383 | MenhirState276 | MenhirState278 | MenhirState300 | MenhirState302 | MenhirState318 | MenhirState320 | MenhirState377 | MenhirState362 | MenhirState364 | MenhirState371 | MenhirState368 | MenhirState365 | MenhirState326 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState338 | MenhirState341 | MenhirState313 | MenhirState304 | MenhirState296 | MenhirState262 | MenhirState239 | MenhirState218 | MenhirState225 | MenhirState210 | MenhirState198 | MenhirState24 | MenhirState27 | MenhirState30 | MenhirState43 | MenhirState57 | MenhirState77 | MenhirState169 | MenhirState80 | MenhirState89 | MenhirState91 | MenhirState93 | MenhirState95 | MenhirState97 | MenhirState105 | MenhirState107 | MenhirState157 | MenhirState153 | MenhirState113 | MenhirState115 | MenhirState118 | MenhirState124 | MenhirState126 | MenhirState149 | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | GETHAN | GTHAN | INSTANCEOF | LETHAN | LOGSHIFT | LSHIFT | LTHAN | NEQUAL | OR | QM | RBRAC | RCURL | RPAR | RSHIFT | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (add : (string))) = _menhir_stack in
            let _v : (string) =                         ( add ) in
            _menhir_goto_shiftExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | GETHAN | GTHAN | INSTANCEOF | LETHAN | LOGSHIFT | LSHIFT | LTHAN | NEQUAL | OR | QM | RBRAC | RCURL | RPAR | RSHIFT | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (sh : (string))), _, (add : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                       ( sh^">>>"^add ) in
            _menhir_goto_shiftExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | GETHAN | GTHAN | INSTANCEOF | LETHAN | LOGSHIFT | LSHIFT | LTHAN | NEQUAL | OR | QM | RBRAC | RCURL | RPAR | RSHIFT | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (sh : (string))), _, (add : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                     ( sh^"<<"^add ) in
            _menhir_goto_shiftExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | GETHAN | GTHAN | INSTANCEOF | LETHAN | LOGSHIFT | LSHIFT | LTHAN | NEQUAL | OR | QM | RBRAC | RCURL | RPAR | RSHIFT | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (sh : (string))), _, (add : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                     ( sh^">>"^add ) in
            _menhir_goto_shiftExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run143 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143

and _menhir_run145 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145

and _menhir_goto_exclusiveOrExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | XOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOR | COL | COMM | OR | QM | RBRAC | RCURL | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (ior : (string))), _, (eor : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                            ( ior^" | "^eor ) in
            _menhir_goto_inclusiveOrExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState2 | MenhirState447 | MenhirState5 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState214 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState246 | MenhirState248 | MenhirState420 | MenhirState412 | MenhirState413 | MenhirState417 | MenhirState414 | MenhirState271 | MenhirState273 | MenhirState395 | MenhirState398 | MenhirState274 | MenhirState383 | MenhirState276 | MenhirState278 | MenhirState300 | MenhirState302 | MenhirState318 | MenhirState320 | MenhirState377 | MenhirState362 | MenhirState364 | MenhirState371 | MenhirState368 | MenhirState365 | MenhirState326 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState338 | MenhirState341 | MenhirState313 | MenhirState304 | MenhirState296 | MenhirState262 | MenhirState239 | MenhirState218 | MenhirState225 | MenhirState210 | MenhirState198 | MenhirState24 | MenhirState27 | MenhirState30 | MenhirState43 | MenhirState57 | MenhirState169 | MenhirState77 | MenhirState89 | MenhirState105 | MenhirState157 | MenhirState153 | MenhirState113 | MenhirState118 | MenhirState115 | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | XOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOR | COL | COMM | OR | QM | RBRAC | RCURL | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (eor : (string))) = _menhir_stack in
            let _v : (string) =                            ( eor ) in
            _menhir_goto_inclusiveOrExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run124 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124

and _menhir_goto_multiplicativeExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState447 | MenhirState2 | MenhirState207 | MenhirState433 | MenhirState209 | MenhirState216 | MenhirState428 | MenhirState243 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState413 | MenhirState414 | MenhirState412 | MenhirState273 | MenhirState398 | MenhirState395 | MenhirState274 | MenhirState383 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState371 | MenhirState368 | MenhirState364 | MenhirState365 | MenhirState362 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState341 | MenhirState338 | MenhirState326 | MenhirState318 | MenhirState313 | MenhirState304 | MenhirState300 | MenhirState296 | MenhirState276 | MenhirState271 | MenhirState262 | MenhirState246 | MenhirState239 | MenhirState225 | MenhirState218 | MenhirState214 | MenhirState210 | MenhirState5 | MenhirState198 | MenhirState24 | MenhirState27 | MenhirState30 | MenhirState43 | MenhirState57 | MenhirState169 | MenhirState77 | MenhirState80 | MenhirState95 | MenhirState97 | MenhirState105 | MenhirState157 | MenhirState153 | MenhirState126 | MenhirState149 | MenhirState138 | MenhirState124 | MenhirState118 | MenhirState115 | MenhirState113 | MenhirState107 | MenhirState99 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState82 | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | GETHAN | GTHAN | INSTANCEOF | LETHAN | LOGSHIFT | LSHIFT | LTHAN | MINUS | NEQUAL | OR | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (mul : (string))) = _menhir_stack in
            let _v : (string) =                               ( mul ) in
            _menhir_goto_additiveExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | GETHAN | GTHAN | INSTANCEOF | LETHAN | LOGSHIFT | LSHIFT | LTHAN | MINUS | NEQUAL | OR | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (add : (string))), _, (mul : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                             ( add^"+"^mul ) in
            _menhir_goto_additiveExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | GETHAN | GTHAN | INSTANCEOF | LETHAN | LOGSHIFT | LSHIFT | LTHAN | MINUS | NEQUAL | OR | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (add : (string))), _, (mul : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                              ( add^"-"^mul ) in
            _menhir_goto_additiveExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce36 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (un : (string))) = _menhir_stack in
    let _v : (string) =                     ( un ) in
    _menhir_goto_castExpression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_assignmentOperator : _menhir_env -> 'ttv_tail -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_goto_andExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 | MenhirState447 | MenhirState5 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState214 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState246 | MenhirState248 | MenhirState420 | MenhirState412 | MenhirState413 | MenhirState417 | MenhirState414 | MenhirState271 | MenhirState273 | MenhirState395 | MenhirState398 | MenhirState274 | MenhirState383 | MenhirState276 | MenhirState278 | MenhirState300 | MenhirState302 | MenhirState318 | MenhirState320 | MenhirState377 | MenhirState362 | MenhirState364 | MenhirState371 | MenhirState368 | MenhirState365 | MenhirState326 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState338 | MenhirState341 | MenhirState313 | MenhirState304 | MenhirState296 | MenhirState262 | MenhirState239 | MenhirState218 | MenhirState225 | MenhirState210 | MenhirState198 | MenhirState24 | MenhirState27 | MenhirState30 | MenhirState43 | MenhirState57 | MenhirState77 | MenhirState169 | MenhirState89 | MenhirState91 | MenhirState105 | MenhirState107 | MenhirState157 | MenhirState153 | MenhirState113 | MenhirState115 | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOR | COL | COMM | OR | QM | RBRAC | RCURL | RPAR | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (a : (string))) = _menhir_stack in
            let _v : (string) =                  ( a ) in
            _menhir_goto_exclusiveOrExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOR | COL | COMM | OR | QM | RBRAC | RCURL | RPAR | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (eor : (string))), _, (a : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                  ( eor^" ^ "^a ) in
            _menhir_goto_exclusiveOrExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run95 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_run126 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126

and _menhir_goto_localVariableDeclStmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState324 | MenhirState252 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (lvds : (string)) = _v in
        let _v : (string) =                             ( lvds ) in
        _menhir_goto_forInit _menhir_env _menhir_stack _menhir_s _v
    | MenhirState447 | MenhirState2 | MenhirState433 | MenhirState209 | MenhirState428 | MenhirState243 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (lvd : (string)) = _v in
        let _v : (string) =                            ( lvd ) in
        _menhir_goto_localVariableDeclOrStmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run259 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run256 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState259

and _menhir_goto_castExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState2 | MenhirState447 | MenhirState5 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState214 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState246 | MenhirState248 | MenhirState420 | MenhirState412 | MenhirState413 | MenhirState417 | MenhirState414 | MenhirState271 | MenhirState273 | MenhirState395 | MenhirState398 | MenhirState274 | MenhirState383 | MenhirState276 | MenhirState278 | MenhirState300 | MenhirState302 | MenhirState318 | MenhirState320 | MenhirState377 | MenhirState362 | MenhirState364 | MenhirState371 | MenhirState368 | MenhirState365 | MenhirState326 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState338 | MenhirState341 | MenhirState313 | MenhirState304 | MenhirState296 | MenhirState262 | MenhirState239 | MenhirState218 | MenhirState225 | MenhirState210 | MenhirState198 | MenhirState24 | MenhirState27 | MenhirState30 | MenhirState43 | MenhirState57 | MenhirState59 | MenhirState169 | MenhirState77 | MenhirState80 | MenhirState82 | MenhirState89 | MenhirState91 | MenhirState93 | MenhirState95 | MenhirState97 | MenhirState105 | MenhirState157 | MenhirState107 | MenhirState153 | MenhirState113 | MenhirState115 | MenhirState124 | MenhirState126 | MenhirState149 | MenhirState147 | MenhirState141 | MenhirState138 | MenhirState118 | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (cast : (string)) = _v in
        let _v : (string) =                      ( cast ) in
        _menhir_goto_multiplicativeExpression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ct : (string)) = _v in
        let (_menhir_stack, _menhir_s, (aop : (string))) = _menhir_stack in
        let _v : (string) =                                                  ( aop^ct ) in
        _menhir_goto_unaryExpression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (cast : (string)) = _v in
        let (_menhir_stack, _menhir_s, (mul : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string) =                                                         ( mul^"%"^cast ) in
        _menhir_goto_multiplicativeExpression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (cast : (string)) = _v in
        let (_menhir_stack, _menhir_s, (mul : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string) =                                                         ( mul^"/"^cast ) in
        _menhir_goto_multiplicativeExpression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (cast : (string)) = _v in
        let (_menhir_stack, _menhir_s, (mul : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string) =                                                         ( mul^"*"^cast ) in
        _menhir_goto_multiplicativeExpression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ce : (string)) = _v in
        let ((_menhir_stack, _menhir_s), _, (pte : (string))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (string) =                                                            ( " ("^pte^") "^ce ) in
        _menhir_goto_castExpression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ce : (string)) = _v in
        let ((_menhir_stack, _menhir_s), _, (cte : (string))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (string) =                                                        ( " ("^cte^") "^ce ) in
        _menhir_goto_castExpression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_unaryExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (un : (string))) = _menhir_stack in
        let _1 = () in
        let _v : (string) =                                 ( "--"^un ) in
        _menhir_goto_unaryExpression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState447 | MenhirState2 | MenhirState207 | MenhirState433 | MenhirState209 | MenhirState216 | MenhirState428 | MenhirState243 | MenhirState420 | MenhirState248 | MenhirState417 | MenhirState413 | MenhirState414 | MenhirState412 | MenhirState273 | MenhirState398 | MenhirState395 | MenhirState383 | MenhirState274 | MenhirState278 | MenhirState302 | MenhirState377 | MenhirState320 | MenhirState371 | MenhirState368 | MenhirState364 | MenhirState365 | MenhirState362 | MenhirState328 | MenhirState346 | MenhirState329 | MenhirState341 | MenhirState338 | MenhirState326 | MenhirState318 | MenhirState313 | MenhirState304 | MenhirState300 | MenhirState296 | MenhirState276 | MenhirState271 | MenhirState262 | MenhirState246 | MenhirState239 | MenhirState218 | MenhirState214 | MenhirState210 | MenhirState5 | MenhirState198 | MenhirState24 | MenhirState27 | MenhirState30 | MenhirState169 | MenhirState105 | MenhirState157 | MenhirState113 | MenhirState107 | MenhirState89 | MenhirState77 | MenhirState57 | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ANDEQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (string) =             ( "&=" ) in
            _menhir_goto_assignmentOperator _menhir_env _menhir_stack _v
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (string) =         ( "=" ) in
            _menhir_goto_assignmentOperator _menhir_env _menhir_stack _v
        | DIVEQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (string) =             ( "/=" ) in
            _menhir_goto_assignmentOperator _menhir_env _menhir_stack _v
        | LOGSHIFTEQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (string) =                  ( ">>>=" ) in
            _menhir_goto_assignmentOperator _menhir_env _menhir_stack _v
        | LSHIFTEQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (string) =                ( "<<=" ) in
            _menhir_goto_assignmentOperator _menhir_env _menhir_stack _v
        | MINUSEQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (string) =               ( "-=" ) in
            _menhir_goto_assignmentOperator _menhir_env _menhir_stack _v
        | MODEQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (string) =             ( "%=" ) in
            _menhir_goto_assignmentOperator _menhir_env _menhir_stack _v
        | MULEQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (string) =             ( "*=" ) in
            _menhir_goto_assignmentOperator _menhir_env _menhir_stack _v
        | OREQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (string) =            ( "|=" ) in
            _menhir_goto_assignmentOperator _menhir_env _menhir_stack _v
        | PEQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (string) =           ( "+=" ) in
            _menhir_goto_assignmentOperator _menhir_env _menhir_stack _v
        | RSHIFTEQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (string) =                ( ">>=" ) in
            _menhir_goto_assignmentOperator _menhir_env _menhir_stack _v
        | XOREQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (string) =             ( "^=" ) in
            _menhir_goto_assignmentOperator _menhir_env _menhir_stack _v
        | AND | BAND | BOR | COL | COMM | DIV | EQUAL | GETHAN | GTHAN | INSTANCEOF | LETHAN | LOGSHIFT | LSHIFT | LTHAN | MINUS | MOD | MUL | NEQUAL | OR | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | SEMI | XOR ->
            _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState225 | MenhirState194 | MenhirState186 | MenhirState153 | MenhirState149 | MenhirState147 | MenhirState145 | MenhirState143 | MenhirState141 | MenhirState138 | MenhirState126 | MenhirState124 | MenhirState121 | MenhirState118 | MenhirState115 | MenhirState99 | MenhirState97 | MenhirState95 | MenhirState93 | MenhirState91 | MenhirState82 | MenhirState80 | MenhirState74 | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (loguop : (string))), _, (un : (string))) = _menhir_stack in
        let _v : (string) =                                                   ( loguop^un ) in
        _menhir_goto_logicalUnaryExpression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (un : (string))) = _menhir_stack in
        let _1 = () in
        let _v : (string) =                               ( "++"^un ) in
        _menhir_goto_unaryExpression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce34 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (string) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let ((_menhir_stack, _menhir_s), _, (lvds : (string))) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (string) =                                             ( "{\n"^lvds^"\n}\n" ) in
    _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce184 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (ss : (string)) ->
    let _v : (string) =                   ( ss ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_equalityExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 | MenhirState447 | MenhirState5 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState214 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState246 | MenhirState248 | MenhirState420 | MenhirState412 | MenhirState413 | MenhirState417 | MenhirState414 | MenhirState271 | MenhirState273 | MenhirState395 | MenhirState398 | MenhirState274 | MenhirState383 | MenhirState276 | MenhirState278 | MenhirState300 | MenhirState302 | MenhirState318 | MenhirState320 | MenhirState377 | MenhirState362 | MenhirState364 | MenhirState371 | MenhirState368 | MenhirState365 | MenhirState326 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState338 | MenhirState341 | MenhirState313 | MenhirState304 | MenhirState296 | MenhirState262 | MenhirState239 | MenhirState218 | MenhirState225 | MenhirState210 | MenhirState198 | MenhirState24 | MenhirState27 | MenhirState30 | MenhirState43 | MenhirState57 | MenhirState169 | MenhirState77 | MenhirState89 | MenhirState91 | MenhirState105 | MenhirState157 | MenhirState153 | MenhirState113 | MenhirState118 | MenhirState115 | MenhirState107 | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | OR | QM | RBRAC | RCURL | RPAR | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (eq : (string))) = _menhir_stack in
            let _v : (string) =                        ( eq ) in
            _menhir_goto_andExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | OR | QM | RBRAC | RCURL | RPAR | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (string))), _, (eq : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                               ( a^" & "^eq ) in
            _menhir_goto_andExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run97 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run128 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128

and _menhir_run138 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138

and _menhir_run149 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149

and _menhir_goto_newAllocationExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (all : (string)) = _v in
    let _v : (string) =                                ( all ) in
    _menhir_goto_notJustName _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce185 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (is : (string)) ->
    let _v : (string) =                ( is ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce182 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (ass : (string)) ->
    let _v : (string) =                   ( ass ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_variableDeclarators : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState255 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMM ->
            _menhir_run259 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (ts : (string))), _, (vd : (string))) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (string) =                                                       ( "final "^ts^" "^vd^";" ) in
            _menhir_goto_localVariableDeclStmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState422 | MenhirState265 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMM ->
            _menhir_run259 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (ts : (string))), _, (vd : (string))) = _menhir_stack in
            let _3 = () in
            let _v : (string) =                                               ( ts^vd^";" ) in
            _menhir_goto_localVariableDeclStmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run157 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | LCURL ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | COMM | RCURL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (ai : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string) =                              ( ai^" , " ) in
        _menhir_goto_arrayInitializers _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157

and _menhir_reduce172 : _menhir_env -> ((('ttv_tail * _menhir_state)) * _menhir_state * (string)) -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _ (sb : (string)) ->
    let ((_menhir_stack, _menhir_s), _, (e : (string))) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : (string) =                                                 ( "switch ("^e^") "^sb ) in
    _menhir_goto_selectStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_switchLabels : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run225 _menhir_env (Obj.magic _menhir_stack) MenhirState229
    | DEFAULT ->
        _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState229
    | LCURL ->
        _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState229
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState229

and _menhir_goto_logicalUnaryExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState447 | MenhirState2 | MenhirState207 | MenhirState433 | MenhirState209 | MenhirState216 | MenhirState428 | MenhirState243 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState413 | MenhirState414 | MenhirState412 | MenhirState273 | MenhirState398 | MenhirState395 | MenhirState274 | MenhirState383 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState371 | MenhirState368 | MenhirState364 | MenhirState365 | MenhirState362 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState341 | MenhirState338 | MenhirState326 | MenhirState318 | MenhirState313 | MenhirState304 | MenhirState300 | MenhirState296 | MenhirState276 | MenhirState271 | MenhirState262 | MenhirState246 | MenhirState239 | MenhirState225 | MenhirState218 | MenhirState214 | MenhirState210 | MenhirState5 | MenhirState198 | MenhirState24 | MenhirState194 | MenhirState27 | MenhirState186 | MenhirState29 | MenhirState30 | MenhirState33 | MenhirState43 | MenhirState57 | MenhirState59 | MenhirState74 | MenhirState169 | MenhirState77 | MenhirState80 | MenhirState82 | MenhirState95 | MenhirState97 | MenhirState105 | MenhirState157 | MenhirState153 | MenhirState126 | MenhirState149 | MenhirState147 | MenhirState141 | MenhirState145 | MenhirState143 | MenhirState138 | MenhirState124 | MenhirState121 | MenhirState118 | MenhirState115 | MenhirState113 | MenhirState107 | MenhirState99 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (logu : (string)) = _v in
        let _v : (string) =                                ( logu ) in
        _menhir_goto_unaryExpression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (lue : (string)) = _v in
        let ((_menhir_stack, _menhir_s), _, (e : (string))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (string) =                                                      ( " ("^e^") "^lue ) in
        _menhir_goto_castExpression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_realPostfixExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (id : (string)) = _v in
            let (_menhir_stack, _menhir_s, (rpe : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                ( rpe^"."^id ) in
            _menhir_goto_fieldAccess _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | AND | ANDEQUAL | ASSIGN | BAND | BOR | COL | COMM | DECREMENT | DIV | DIVEQUAL | EQUAL | GETHAN | GTHAN | INCREMENT | INSTANCEOF | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (rpost : (string))) = _menhir_stack in
        let _v : (string) =                                ( rpost ) in
        _menhir_goto_postfixExpression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_localVariableDeclAndStmts : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState243 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if _menhir_env._menhir_error then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState428
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | ASSERT ->
              _menhir_run395 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | BNOT ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | BOOLEAN ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | BOOLEANLIT _v ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState428 _v
          | BREAK ->
              _menhir_run391 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | BYTE ->
              _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | CHAR ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | CHARLIT _v ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState428 _v
          | CONTINUE ->
              _menhir_run387 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | DECREMENT ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | DO ->
              _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | DOUBLE ->
              _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | DOUBLELIT _v ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState428 _v
          | FINAL ->
              _menhir_run254 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | FLOAT ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | FLOATLIT _v ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState428 _v
          | FOR ->
              _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | IDENTIFIER _v ->
              _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState428 _v
          | IF ->
              _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | INCREMENT ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | INT ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | INTLIT _v ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState428 _v
          | LCURL ->
              _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | LONG ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | LPAR ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | MINUS ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | NEW ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | NOT ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | NULLLIT _v ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState428 _v
          | PLUS ->
              _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | RCURL ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState428 in
              let _menhir_env = _menhir_discard _menhir_env in
              _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) _menhir_s
          | RETURN ->
              _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | SEMI ->
              _menhir_run238 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | SHORT ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | STRLIT _v ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState428 _v
          | SUPER ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | SWITCH ->
              _menhir_run217 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | SYNCHRONIZED ->
              _menhir_run213 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | THIS ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | THROW ->
              _menhir_run210 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | TRY ->
              _menhir_run208 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | VOID ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | WHILE ->
              _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState428
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState428)
    | MenhirState209 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if _menhir_env._menhir_error then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState433
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | ASSERT ->
              _menhir_run395 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | BNOT ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | BOOLEAN ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | BOOLEANLIT _v ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState433 _v
          | BREAK ->
              _menhir_run391 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | BYTE ->
              _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | CHAR ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | CHARLIT _v ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState433 _v
          | CONTINUE ->
              _menhir_run387 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | DECREMENT ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | DO ->
              _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | DOUBLE ->
              _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | DOUBLELIT _v ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState433 _v
          | FINAL ->
              _menhir_run254 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | FLOAT ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | FLOATLIT _v ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState433 _v
          | FOR ->
              _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | IDENTIFIER _v ->
              _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState433 _v
          | IF ->
              _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | INCREMENT ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | INT ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | INTLIT _v ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState433 _v
          | LCURL ->
              _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | LONG ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | LPAR ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | MINUS ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | NEW ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | NOT ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | NULLLIT _v ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState433 _v
          | PLUS ->
              _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | RCURL ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState433 in
              let _menhir_env = _menhir_discard _menhir_env in
              _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) _menhir_s
          | RETURN ->
              _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | SEMI ->
              _menhir_run238 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | SHORT ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | STRLIT _v ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState433 _v
          | SUPER ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | SWITCH ->
              _menhir_run217 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | SYNCHRONIZED ->
              _menhir_run213 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | THIS ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | THROW ->
              _menhir_run210 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | TRY ->
              _menhir_run208 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | VOID ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | WHILE ->
              _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState433
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState433)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if _menhir_env._menhir_error then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState447
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | ASSERT ->
              _menhir_run395 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | BNOT ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | BOOLEAN ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | BOOLEANLIT _v ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState447 _v
          | BREAK ->
              _menhir_run391 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | BYTE ->
              _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | CHAR ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | CHARLIT _v ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState447 _v
          | CONTINUE ->
              _menhir_run387 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | DECREMENT ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | DO ->
              _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | DOUBLE ->
              _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | DOUBLELIT _v ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState447 _v
          | FINAL ->
              _menhir_run254 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | FLOAT ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | FLOATLIT _v ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState447 _v
          | FOR ->
              _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | IDENTIFIER _v ->
              _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState447 _v
          | IF ->
              _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | INCREMENT ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | INT ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | INTLIT _v ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState447 _v
          | LCURL ->
              _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | LONG ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | LPAR ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | MINUS ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | NEW ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | NOT ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | NULLLIT _v ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState447 _v
          | PLUS ->
              _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | RCURL ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState447 in
              _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) _menhir_s
          | RETURN ->
              _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | SEMI ->
              _menhir_run238 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | SHORT ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | STRLIT _v ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState447 _v
          | SUPER ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | SWITCH ->
              _menhir_run217 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | SYNCHRONIZED ->
              _menhir_run213 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | THIS ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | THROW ->
              _menhir_run210 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | TRY ->
              _menhir_run208 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | VOID ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | WHILE ->
              _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState447
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState447)
    | _ ->
        _menhir_fail ()

and _menhir_goto_selectStmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState274 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState368 | MenhirState365 | MenhirState328 | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce184 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState447 | MenhirState2 | MenhirState207 | MenhirState433 | MenhirState209 | MenhirState216 | MenhirState428 | MenhirState243 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState414 | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce184 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run107 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | LCURL ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | RCURL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState107 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (string) =                ( " { "^" } " ) in
        _menhir_goto_variableInitializer _menhir_env _menhir_stack _menhir_s _v
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_goto_catchHeader : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState280 | MenhirState290 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LCURL ->
            _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState292
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState292)
    | MenhirState435 | MenhirState439 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LCURL ->
            _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState441
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState441)
    | _ ->
        _menhir_fail ()

and _menhir_reduce68 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
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
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
        | LCURL ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState262)
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

and _menhir_run256 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_relationalExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState447 | MenhirState2 | MenhirState207 | MenhirState433 | MenhirState209 | MenhirState216 | MenhirState428 | MenhirState243 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState413 | MenhirState414 | MenhirState412 | MenhirState273 | MenhirState398 | MenhirState395 | MenhirState274 | MenhirState383 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState371 | MenhirState368 | MenhirState364 | MenhirState365 | MenhirState362 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState341 | MenhirState338 | MenhirState326 | MenhirState318 | MenhirState313 | MenhirState304 | MenhirState300 | MenhirState296 | MenhirState276 | MenhirState271 | MenhirState262 | MenhirState246 | MenhirState239 | MenhirState225 | MenhirState218 | MenhirState214 | MenhirState210 | MenhirState5 | MenhirState198 | MenhirState24 | MenhirState27 | MenhirState30 | MenhirState43 | MenhirState57 | MenhirState169 | MenhirState105 | MenhirState157 | MenhirState153 | MenhirState124 | MenhirState118 | MenhirState115 | MenhirState113 | MenhirState107 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GETHAN ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack)
        | GTHAN ->
            _menhir_run138 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack)
        | LETHAN ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LTHAN ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | NEQUAL | OR | QM | RBRAC | RCURL | RPAR | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (rel : (string))) = _menhir_stack in
            let _v : (string) =                           ( rel ) in
            _menhir_goto_equalityExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GETHAN ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack)
        | GTHAN ->
            _menhir_run138 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack)
        | LETHAN ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LTHAN ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | NEQUAL | OR | QM | RBRAC | RCURL | RPAR | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (eq : (string))), _, (rel : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                          ( eq^"!="^rel ) in
            _menhir_goto_equalityExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GETHAN ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack)
        | GTHAN ->
            _menhir_run138 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack)
        | LETHAN ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LTHAN ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | NEQUAL | OR | QM | RBRAC | RCURL | RPAR | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (eq : (string))), _, (rel : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                                         ( eq^"=="^rel ) in
            _menhir_goto_equalityExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_plainNewAllocationExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState447 | MenhirState2 | MenhirState207 | MenhirState433 | MenhirState209 | MenhirState216 | MenhirState428 | MenhirState243 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState413 | MenhirState414 | MenhirState412 | MenhirState273 | MenhirState398 | MenhirState395 | MenhirState274 | MenhirState383 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState371 | MenhirState368 | MenhirState364 | MenhirState365 | MenhirState362 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState341 | MenhirState338 | MenhirState326 | MenhirState318 | MenhirState313 | MenhirState304 | MenhirState300 | MenhirState296 | MenhirState276 | MenhirState271 | MenhirState262 | MenhirState246 | MenhirState239 | MenhirState225 | MenhirState218 | MenhirState214 | MenhirState210 | MenhirState5 | MenhirState198 | MenhirState24 | MenhirState194 | MenhirState191 | MenhirState27 | MenhirState186 | MenhirState29 | MenhirState30 | MenhirState33 | MenhirState43 | MenhirState57 | MenhirState169 | MenhirState77 | MenhirState80 | MenhirState95 | MenhirState97 | MenhirState105 | MenhirState157 | MenhirState153 | MenhirState126 | MenhirState149 | MenhirState147 | MenhirState145 | MenhirState143 | MenhirState141 | MenhirState138 | MenhirState124 | MenhirState121 | MenhirState118 | MenhirState115 | MenhirState113 | MenhirState107 | MenhirState99 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState83 | MenhirState82 | MenhirState74 | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (pall : (string)) = _v in
        let _v : (string) =                                    ( pall ) in
        _menhir_goto_newAllocationExpression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (pall : (string)) = _v in
        let ((_menhir_stack, _menhir_s, (qn : (string))), _) = _menhir_stack in
        let _2 = () in
        let _v : (string) =                                                           ( qn^"."^pall ) in
        _menhir_goto_newAllocationExpression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_iterStmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState274 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState368 | MenhirState365 | MenhirState328 | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce185 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState2 | MenhirState447 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState414 | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce185 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_assertStmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState274 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState368 | MenhirState365 | MenhirState328 | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce182 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState2 | MenhirState447 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState414 | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce182 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run169 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169

and _menhir_goto_variableDeclarator : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState259 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (vd : (string)) = _v in
        let (_menhir_stack, _menhir_s, (vds : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string) =                                                       ( vds^" , "^vd ) in
        _menhir_goto_variableDeclarators _menhir_env _menhir_stack _menhir_s _v
    | MenhirState422 | MenhirState265 | MenhirState255 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (vd : (string)) = _v in
        let _v : (string) =                         ( vd ) in
        _menhir_goto_variableDeclarators _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_arrayInitializers : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMM ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack)
        | RCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (arri : (string))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (string) =                                       ( " { "^arri^" } " ) in
            _menhir_goto_variableInitializer _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMM ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack)
        | RCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (arrall : (string))), _, (arri : (string))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (string) =                                                                            ( arrall^"{"^arri^"}" ) in
            _menhir_goto_plainNewAllocationExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_catches : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState280 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if _menhir_env._menhir_error then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState290
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | CATCH ->
              _menhir_run283 _menhir_env (Obj.magic _menhir_stack) MenhirState290
          | FINALLY ->
              _menhir_run281 _menhir_env (Obj.magic _menhir_stack) MenhirState290
          | ELSE | WHILE ->
              _menhir_reduce99 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState290)
    | MenhirState435 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if _menhir_env._menhir_error then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState439
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | CATCH ->
              _menhir_run283 _menhir_env (Obj.magic _menhir_stack) MenhirState439
          | FINALLY ->
              _menhir_run436 _menhir_env (Obj.magic _menhir_stack) MenhirState439
          | ASSERT | BNOT | BOOLEAN | BOOLEANLIT _ | BREAK | BYTE | CHAR | CHARLIT _ | CONTINUE | DECREMENT | DO | DOUBLE | DOUBLELIT _ | ELSE | FINAL | FLOAT | FLOATLIT _ | FOR | IDENTIFIER _ | IF | INCREMENT | INT | INTLIT _ | LCURL | LONG | LPAR | MINUS | NEW | NOT | NULLLIT _ | PLUS | RCURL | RETURN | SEMI | SHORT | STRLIT _ | SUPER | SWITCH | SYNCHRONIZED | THIS | THROW | TRY | VOID | WHILE ->
              _menhir_reduce99 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState439)
    | _ ->
        _menhir_fail ()

and _menhir_goto_switchBlock : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState220 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce172 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState306 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce172 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_switchLabel : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (sl : (string)) = _v in
        let (_menhir_stack, _menhir_s, (sls : (string))) = _menhir_stack in
        let _v : (string) =                                    ( sls^"\n"^sl ) in
        _menhir_goto_switchLabels _menhir_env _menhir_stack _menhir_s _v
    | MenhirState309 | MenhirState307 | MenhirState233 | MenhirState221 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (sl : (string)) = _v in
        let _v : (string) =                 ( sl ) in
        _menhir_goto_switchLabels _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_postfixExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DECREMENT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (post : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string) =                                     ( post^"--" ) in
        _menhir_goto_realPostfixExpression _menhir_env _menhir_stack _menhir_s _v
    | INCREMENT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (post : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string) =                                   ( post^"++" ) in
        _menhir_goto_realPostfixExpression _menhir_env _menhir_stack _menhir_s _v
    | AND | ANDEQUAL | ASSIGN | BAND | BOR | COL | COMM | DIV | DIVEQUAL | EQUAL | GETHAN | GTHAN | INSTANCEOF | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (post : (string))) = _menhir_stack in
        let _v : (string) =                         ( post ) in
        _menhir_goto_logicalUnaryExpression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_localVariableDeclOrStmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState2 | MenhirState209 | MenhirState243 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (lvd : (string)) = _v in
        let _v : (string) =                              ( lvd ) in
        _menhir_goto_localVariableDeclAndStmts _menhir_env _menhir_stack _menhir_s _v
    | MenhirState447 | MenhirState433 | MenhirState428 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (lvd : (string)) = _v in
        let (_menhir_stack, _menhir_s, (lvds : (string))) = _menhir_stack in
        let _v : (string) =                                                               ( lvds^lvd ) in
        _menhir_goto_localVariableDeclAndStmts _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_error419 : _menhir_env -> (((('ttv_tail * _menhir_state)) * _menhir_state * (string))) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    _menhir_reduce170 _menhir_env (Obj.magic _menhir_stack)

and _menhir_error381 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce103 : _menhir_env -> (((('ttv_tail * _menhir_state)) * _menhir_state * (string))) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _, (e : (string))), _, (s : (string))) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : (string) =                                           ( "while("^e^")"^s ) in
    _menhir_goto_iterStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce97 : _menhir_env -> (((('ttv_tail * _menhir_state)) * _menhir_state * (string))) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _, (e : (string))), _, (s : (string))) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : (string) =                                                  ( "synchronized ("^e^") "^s ) in
    _menhir_goto_guardingStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce171 : _menhir_env -> (((((('ttv_tail * _menhir_state)) * _menhir_state * (string))) * _menhir_state * (string))) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((((_menhir_stack, _menhir_s), _, (e : (string))), _, (s1 : (string))), _, (s2 : (string))) = _menhir_stack in
    let _6 = () in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : (string) =                                                             ( "if("^e^") "^s1^"\nelse "^s2 ) in
    _menhir_goto_selectStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce170 : _menhir_env -> (((('ttv_tail * _menhir_state)) * _menhir_state * (string))) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _, (e : (string))), _, (s : (string))) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : (string) =                                                            ( "if("^e^") "^s ) in
    _menhir_goto_selectStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_error376 : _menhir_env -> (((('ttv_tail * _menhir_state)) * _menhir_state * (string))) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce105 : _menhir_env -> (((((('ttv_tail * _menhir_state)) * _menhir_state * (string)) * _menhir_state * (string)) * _menhir_state * (string))) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((((_menhir_stack, _menhir_s), _, (fi : (string))), _, (fe : (string))), _, (fin : (string))), _, (s : (string))) = _menhir_stack in
    let _6 = () in
    let _2 = () in
    let _1 = () in
    let _v : (string) =                                                                ( "for("^fi^fe^fin^")"^s ) in
    _menhir_goto_iterStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce106 : _menhir_env -> ((((('ttv_tail * _menhir_state)) * _menhir_state * (string)) * _menhir_state * (string)) * _menhir_state) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((((_menhir_stack, _menhir_s), _, (fi : (string))), _, (fe : (string))), _), _, (s : (string))) = _menhir_stack in
    let _5 = () in
    let _2 = () in
    let _1 = () in
    let _v : (string) =                                                    ( "for("^fi^fe^")"^s ) in
    _menhir_goto_iterStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce107 : _menhir_env -> (((((('ttv_tail * _menhir_state)) * _menhir_state * (string))) * _menhir_state * (string))) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((((_menhir_stack, _menhir_s), _, (fvo : (string))), _, (e : (string))), _, (s : (string))) = _menhir_stack in
    let _6 = () in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : (string) =                                                             ( "for("^fvo^":"^e^")"^s ) in
    _menhir_goto_iterStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_error344 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_arrayAllocationExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LCURL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | LCURL ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | RCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState105 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (arrall : (string))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (string) =                                                     ( arrall^"{"^"}" ) in
            _menhir_goto_plainNewAllocationExpression _menhir_env _menhir_stack _menhir_s _v
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
    | AND | ANDEQUAL | ASSIGN | BAND | BOR | COL | COMM | DECREMENT | DIV | DIVEQUAL | DOT | EQUAL | GETHAN | GTHAN | INCREMENT | INSTANCEOF | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (arrall : (string))) = _menhir_stack in
        let _v : (string) =                                   ( arrall ) in
        _menhir_goto_plainNewAllocationExpression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run133 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (ds : (string))) = _menhir_stack in
    let _2 = () in
    let _v : (string) =                ( ds^" [ ] " ) in
    _menhir_goto_dims _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_typeSpecifier : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (rel : (string))), _, (ts : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string) =                                                         ( rel^" instanceof "^ts ) in
        _menhir_goto_relationalExpression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState254 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run256 _menhir_env (Obj.magic _menhir_stack) MenhirState255 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState255)
    | MenhirState324 | MenhirState252 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState265 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (ts : (string))), _, (id : (string))) = _menhir_stack in
                let _v : (string) =                                 ( ts^" "^id ) in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                (match _menhir_s with
                | MenhirState252 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | COL ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | BNOT ->
                            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | BOOLEAN ->
                            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | BOOLEANLIT _v ->
                            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _v
                        | BYTE ->
                            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | CHAR ->
                            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | CHARLIT _v ->
                            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _v
                        | DECREMENT ->
                            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | DOUBLE ->
                            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | DOUBLELIT _v ->
                            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _v
                        | FLOAT ->
                            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | FLOATLIT _v ->
                            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _v
                        | IDENTIFIER _v ->
                            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _v
                        | INCREMENT ->
                            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | INT ->
                            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | INTLIT _v ->
                            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _v
                        | LONG ->
                            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | LPAR ->
                            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | MINUS ->
                            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | NEW ->
                            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | NOT ->
                            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | NULLLIT _v ->
                            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _v
                        | PLUS ->
                            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | SHORT ->
                            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | STRLIT _v ->
                            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _v
                        | SUPER ->
                            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | THIS ->
                            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | VOID ->
                            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState271
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState271)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | MenhirState324 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | COL ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | BNOT ->
                            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | BOOLEAN ->
                            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | BOOLEANLIT _v ->
                            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState326 _v
                        | BYTE ->
                            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | CHAR ->
                            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | CHARLIT _v ->
                            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState326 _v
                        | DECREMENT ->
                            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | DOUBLE ->
                            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | DOUBLELIT _v ->
                            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState326 _v
                        | FLOAT ->
                            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | FLOATLIT _v ->
                            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState326 _v
                        | IDENTIFIER _v ->
                            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState326 _v
                        | INCREMENT ->
                            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | INT ->
                            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | INTLIT _v ->
                            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState326 _v
                        | LONG ->
                            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | LPAR ->
                            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | MINUS ->
                            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | NEW ->
                            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | NOT ->
                            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | NULLLIT _v ->
                            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState326 _v
                        | PLUS ->
                            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | SHORT ->
                            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | STRLIT _v ->
                            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState326 _v
                        | SUPER ->
                            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | THIS ->
                            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | VOID ->
                            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState326
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState326)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    _menhir_fail ())
            | ASSIGN | COMM | SEMI ->
                _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState265)
    | MenhirState284 ->
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
                let _v : (string) =                                                 ( "catch ( "^ts^id^" ) ") in
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
            let _v : (string) =                                     ( "catch ( "^ts^" ) " ) in
            _menhir_goto_catchHeader _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState447 | MenhirState2 | MenhirState433 | MenhirState209 | MenhirState428 | MenhirState243 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run256 _menhir_env (Obj.magic _menhir_stack) MenhirState422 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState422)
    | _ ->
        _menhir_fail ()

and _menhir_goto_classAllocationExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LCURL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (call : (string))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (string) =                                                   ( call^"{"^"}" ) in
            _menhir_goto_plainNewAllocationExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | AND | ANDEQUAL | ASSIGN | BAND | BOR | COL | COMM | DECREMENT | DIV | DIVEQUAL | DOT | EQUAL | GETHAN | GTHAN | INCREMENT | INSTANCEOF | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (call : (string))) = _menhir_stack in
        let _v : (string) =                                       ( call ) in
        _menhir_goto_plainNewAllocationExpression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run198 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198

and _menhir_reduce187 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (gs : (string)) ->
    let _v : (string) =                    ( gs ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_forExpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState362 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if _menhir_env._menhir_error then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState364
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | BNOT ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | BOOLEAN ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | BOOLEANLIT _v ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState364 _v
          | BYTE ->
              _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | CHAR ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | CHARLIT _v ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState364 _v
          | DECREMENT ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | DOUBLE ->
              _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | DOUBLELIT _v ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState364 _v
          | FLOAT ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | FLOATLIT _v ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState364 _v
          | IDENTIFIER _v ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState364 _v
          | INCREMENT ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | INT ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | INTLIT _v ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState364 _v
          | LONG ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | LPAR ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | MINUS ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | NEW ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | NOT ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | NULLLIT _v ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState364 _v
          | PLUS ->
              _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | RPAR ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState364 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | ASSERT ->
                  _menhir_run338 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | BNOT ->
                  _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | BOOLEAN ->
                  _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | BOOLEANLIT _v ->
                  _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState365 _v
              | BREAK ->
                  _menhir_run334 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | BYTE ->
                  _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | CHAR ->
                  _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | CHARLIT _v ->
                  _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState365 _v
              | CONTINUE ->
                  _menhir_run330 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | DECREMENT ->
                  _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | DO ->
                  _menhir_run329 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | DOUBLE ->
                  _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | DOUBLELIT _v ->
                  _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState365 _v
              | FLOAT ->
                  _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | FLOATLIT _v ->
                  _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState365 _v
              | FOR ->
                  _menhir_run323 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | IDENTIFIER _v ->
                  _menhir_run321 _menhir_env (Obj.magic _menhir_stack) MenhirState365 _v
              | IF ->
                  _menhir_run317 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | INCREMENT ->
                  _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | INT ->
                  _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | INTLIT _v ->
                  _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState365 _v
              | LCURL ->
                  _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | LONG ->
                  _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | LPAR ->
                  _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | MINUS ->
                  _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | NEW ->
                  _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | NOT ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | NULLLIT _v ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState365 _v
              | PLUS ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | RETURN ->
                  _menhir_run313 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | SEMI ->
                  _menhir_run312 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | SHORT ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | STRLIT _v ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState365 _v
              | SUPER ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | SWITCH ->
                  _menhir_run303 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | SYNCHRONIZED ->
                  _menhir_run299 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | THIS ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | THROW ->
                  _menhir_run296 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | TRY ->
                  _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | VOID ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | WHILE ->
                  _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState365
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState365)
          | SHORT ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | STRLIT _v ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState364 _v
          | SUPER ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | THIS ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | VOID ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState364
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState364)
    | MenhirState412 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if _menhir_env._menhir_error then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState413
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | BNOT ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | BOOLEAN ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | BOOLEANLIT _v ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState413 _v
          | BYTE ->
              _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | CHAR ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | CHARLIT _v ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState413 _v
          | DECREMENT ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | DOUBLE ->
              _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | DOUBLELIT _v ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState413 _v
          | FLOAT ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | FLOATLIT _v ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState413 _v
          | IDENTIFIER _v ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState413 _v
          | INCREMENT ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | INT ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | INTLIT _v ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState413 _v
          | LONG ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | LPAR ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | MINUS ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | NEW ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | NOT ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | NULLLIT _v ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState413 _v
          | PLUS ->
              _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | RPAR ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState413 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | ASSERT ->
                  _menhir_run395 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | BNOT ->
                  _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | BOOLEAN ->
                  _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | BOOLEANLIT _v ->
                  _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState414 _v
              | BREAK ->
                  _menhir_run391 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | BYTE ->
                  _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | CHAR ->
                  _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | CHARLIT _v ->
                  _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState414 _v
              | CONTINUE ->
                  _menhir_run387 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | DECREMENT ->
                  _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | DO ->
                  _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | DOUBLE ->
                  _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | DOUBLELIT _v ->
                  _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState414 _v
              | FLOAT ->
                  _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | FLOATLIT _v ->
                  _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState414 _v
              | FOR ->
                  _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | IDENTIFIER _v ->
                  _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState414 _v
              | IF ->
                  _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | INCREMENT ->
                  _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | INT ->
                  _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | INTLIT _v ->
                  _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState414 _v
              | LCURL ->
                  _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | LONG ->
                  _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | LPAR ->
                  _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | MINUS ->
                  _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | NEW ->
                  _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | NOT ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | NULLLIT _v ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState414 _v
              | PLUS ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | RETURN ->
                  _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | SEMI ->
                  _menhir_run238 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | SHORT ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | STRLIT _v ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState414 _v
              | SUPER ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | SWITCH ->
                  _menhir_run217 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | SYNCHRONIZED ->
                  _menhir_run213 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | THIS ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | THROW ->
                  _menhir_run210 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | TRY ->
                  _menhir_run208 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | VOID ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | WHILE ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState414
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState414)
          | SHORT ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | STRLIT _v ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState413 _v
          | SUPER ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | THIS ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | VOID ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState413
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState413)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expressionStmts : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMM ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _v
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState371
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState371)
    | RPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (es : (string))) = _menhir_stack in
        let _v : (string) =                     ( es ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState364 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ASSERT ->
                    _menhir_run338 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | BNOT ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | BOOLEAN ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | BOOLEANLIT _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState368 _v
                | BREAK ->
                    _menhir_run334 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | BYTE ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | CHAR ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | CHARLIT _v ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState368 _v
                | CONTINUE ->
                    _menhir_run330 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | DECREMENT ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | DO ->
                    _menhir_run329 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | DOUBLE ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | DOUBLELIT _v ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState368 _v
                | FLOAT ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | FLOATLIT _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState368 _v
                | FOR ->
                    _menhir_run323 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | IDENTIFIER _v ->
                    _menhir_run321 _menhir_env (Obj.magic _menhir_stack) MenhirState368 _v
                | IF ->
                    _menhir_run317 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | INCREMENT ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | INT ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | INTLIT _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState368 _v
                | LCURL ->
                    _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | LONG ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | LPAR ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | MINUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | NEW ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | NOT ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | NULLLIT _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState368 _v
                | PLUS ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | RETURN ->
                    _menhir_run313 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | SEMI ->
                    _menhir_run312 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | SHORT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | STRLIT _v ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState368 _v
                | SUPER ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | SWITCH ->
                    _menhir_run303 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | SYNCHRONIZED ->
                    _menhir_run299 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | THIS ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | THROW ->
                    _menhir_run296 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | TRY ->
                    _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | VOID ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | WHILE ->
                    _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState368
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState368)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState413 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ASSERT ->
                    _menhir_run395 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | BNOT ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | BOOLEAN ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | BOOLEANLIT _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState417 _v
                | BREAK ->
                    _menhir_run391 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | BYTE ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | CHAR ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | CHARLIT _v ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState417 _v
                | CONTINUE ->
                    _menhir_run387 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | DECREMENT ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | DO ->
                    _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | DOUBLE ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | DOUBLELIT _v ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState417 _v
                | FLOAT ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | FLOATLIT _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState417 _v
                | FOR ->
                    _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | IDENTIFIER _v ->
                    _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState417 _v
                | IF ->
                    _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | INCREMENT ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | INT ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | INTLIT _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState417 _v
                | LCURL ->
                    _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | LONG ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | LPAR ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | MINUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | NEW ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | NOT ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | NULLLIT _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState417 _v
                | PLUS ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | RETURN ->
                    _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | SEMI ->
                    _menhir_run238 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | SHORT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | STRLIT _v ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState417 _v
                | SUPER ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | SWITCH ->
                    _menhir_run217 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | SYNCHRONIZED ->
                    _menhir_run213 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | THIS ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | THROW ->
                    _menhir_run210 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | TRY ->
                    _menhir_run208 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | VOID ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | WHILE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState417
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState417)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce183 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (exs : (string))) = _menhir_stack in
    let _2 = () in
    let _v : (string) =                            ( exs^";\n") in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce104 : _menhir_env -> ((((('ttv_tail * _menhir_state) * _menhir_state * (string)))) * _menhir_state * (string)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _, (s : (string))), _, (e : (string))) = _menhir_stack in
    let _7 = () in
    let _6 = () in
    let _4 = () in
    let _3 = () in
    let _1 = () in
    let _v : (string) =                                                     ( "do "^s^" while ("^e^"); ") in
    _menhir_goto_iterStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce19 : _menhir_env -> ((('ttv_tail * _menhir_state) * _menhir_state * (string))) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _, (e1 : (string))), _, (e2 : (string))) = _menhir_stack in
    let _5 = () in
    let _3 = () in
    let _1 = () in
    let _v : (string) =                                                ( "assert "^e1^" : "^e2 ) in
    _menhir_goto_assertStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce18 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, (e : (string))) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (string) =                           ( "assert "^e ) in
    _menhir_goto_assertStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce112 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, (e : (string))) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (string) =                             ( "return "^e^"; "  ) in
    _menhir_goto_jumpStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce189 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _2 = () in
    let _1 = () in
    let _v : (string) =              ( "{ }" ) in
    _menhir_goto_switchBlock _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce114 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, (e : (string))) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (string) =                            ( "throw "^e^"; " ) in
    _menhir_goto_jumpStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_dimExprs : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DIM ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState202
    | LBRAC ->
        _menhir_run198 _menhir_env (Obj.magic _menhir_stack) MenhirState202
    | AND | ANDEQUAL | ASSIGN | BAND | BOR | COL | COMM | DECREMENT | DIV | DIVEQUAL | DOT | EQUAL | GETHAN | GTHAN | INCREMENT | INSTANCEOF | LCURL | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (tn : (string))), _, (de : (string))) = _menhir_stack in
        let _1 = () in
        let _v : (string) =                                ( "new "^tn^de ) in
        _menhir_goto_arrayAllocationExpression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState202

and _menhir_reduce50 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * (string)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, (ex : (string))) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (string) =                          ( "("^ex^")" ) in
    _menhir_goto_complexPrimary _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_argumentList : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMM ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (ma : (string))), _, (al : (string))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (string) =                                              ( ma^"( "^al^" ) ") in
            _menhir_goto_methodCall _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMM ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (tn : (string))), _), _, (args : (string))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (string) =                                              ( "new"^tn^"("^args^")" ) in
            _menhir_goto_classAllocationExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_arrayAccess : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (aa : (string)) = _v in
    let _v : (string) =                   ( aa ) in
    _menhir_goto_complexPrimaryNoParenthesis _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_variableInitializer : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState105 | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (vi : (string)) = _v in
        let _v : (string) =                         ( vi ) in
        _menhir_goto_arrayInitializers _menhir_env _menhir_stack _menhir_s _v
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (vi : (string)) = _v in
        let (_menhir_stack, _menhir_s, (ai : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string) =                                                      ( ai^" , "^vi ) in
        _menhir_goto_arrayInitializers _menhir_env _menhir_stack _menhir_s _v
    | MenhirState262 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (vi : (string)) = _v in
        let (_menhir_stack, _menhir_s, (dn : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string) =                                                    ( dn^" = "^vi ) in
        _menhir_goto_variableDeclarator _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce43 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (c : (string)) ->
    let _v : (string) =          ( c ) in
    _menhir_goto_catches _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce44 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _ (c : (string)) ->
    let (_menhir_stack, _menhir_s, (cs : (string))) = _menhir_stack in
    let _v : (string) =                       ( cs^c ) in
    _menhir_goto_catches _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce100 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * (string)) * _menhir_state * (string) -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _ (f : (string)) ->
    let (((_menhir_stack, _menhir_s), _, (b : (string))), _, (c : (string))) = _menhir_stack in
    let _1 = () in
    let _v : (string) =                                    ( "try "^b^c^f ) in
    _menhir_goto_guardingStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce98 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (string) -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _ (f : (string)) ->
    let ((_menhir_stack, _menhir_s), _, (b : (string))) = _menhir_stack in
    let _1 = () in
    let _v : (string) =                          ( "try "^b^f ) in
    _menhir_goto_guardingStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce190 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (string) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let ((_menhir_stack, _menhir_s), _, (sbsgs : (string))) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (string) =                                            ( "{ "^sbsgs^"}" ) in
    _menhir_goto_switchBlock _menhir_env _menhir_stack _menhir_s _v

and _menhir_run223 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        _menhir_goto_switchLabel _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run225 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState225
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState225

and _menhir_goto_primaryExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (pri : (string)) = _v in
    let _v : (string) =                        ( pri ) in
    _menhir_goto_postfixExpression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce199 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (qn : (string))) = _menhir_stack in
    let _v : (string) =                     ( qn ) in
    _menhir_goto_typeName _menhir_env _menhir_stack _menhir_s _v

and _menhir_run136 : _menhir_env -> ('ttv_tail * _menhir_state * (string)) * _menhir_state -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (string)) = _v in
    let ((_menhir_stack, _menhir_s, (qn : (string))), _) = _menhir_stack in
    let _2 = () in
    let _v : (string) =                                       ( qn^"."^id ) in
    _menhir_goto_qualifiedName _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce128 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (qn : (string))) = _menhir_stack in
    let _v : (string) =                     ( qn ) in
    _menhir_goto_methodAccess _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce147 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (qn : (string))) = _menhir_stack in
    let _v : (string) =                   ( qn ) in
    _menhir_goto_primaryExpression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run176 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState176 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (qn : (string))), _) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _v : (string) =                                  ( qn^".class " ) in
        _menhir_goto_fieldAccess _menhir_env _menhir_stack _menhir_s _v
    | IDENTIFIER _v ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | THIS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState176 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (qn : (string))), _) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _v : (string) =                                 ( qn^".this " ) in
        _menhir_goto_fieldAccess _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if _menhir_env._menhir_error then
          _menhir_error344 _menhir_env (Obj.magic _menhir_stack)
        else
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
                  let _tok = _menhir_env._menhir_token in
                  (match _tok with
                  | BNOT ->
                      _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | BOOLEAN ->
                      _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | BOOLEANLIT _v ->
                      _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _v
                  | BYTE ->
                      _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | CHAR ->
                      _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | CHARLIT _v ->
                      _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _v
                  | DECREMENT ->
                      _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | DOUBLE ->
                      _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | DOUBLELIT _v ->
                      _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _v
                  | FLOAT ->
                      _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | FLOATLIT _v ->
                      _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _v
                  | IDENTIFIER _v ->
                      _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _v
                  | INCREMENT ->
                      _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | INT ->
                      _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | INTLIT _v ->
                      _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _v
                  | LONG ->
                      _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | LPAR ->
                      _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | MINUS ->
                      _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | NEW ->
                      _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | NOT ->
                      _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | NULLLIT _v ->
                      _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _v
                  | PLUS ->
                      _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | SHORT ->
                      _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | STRLIT _v ->
                      _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _v
                  | SUPER ->
                      _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | THIS ->
                      _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | VOID ->
                      _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState346
                  | _ ->
                      assert (not _menhir_env._menhir_error);
                      _menhir_env._menhir_error <- true;
                      _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState346)
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  let _menhir_stack = Obj.magic _menhir_stack in
                  let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_error344 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState328 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce107 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState365 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce106 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState368 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce105 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState320 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if _menhir_env._menhir_error then
          _menhir_error376 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | ELSE ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | ASSERT ->
                  _menhir_run338 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | BNOT ->
                  _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | BOOLEAN ->
                  _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | BOOLEANLIT _v ->
                  _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState377 _v
              | BREAK ->
                  _menhir_run334 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | BYTE ->
                  _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | CHAR ->
                  _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | CHARLIT _v ->
                  _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState377 _v
              | CONTINUE ->
                  _menhir_run330 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | DECREMENT ->
                  _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | DO ->
                  _menhir_run329 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | DOUBLE ->
                  _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | DOUBLELIT _v ->
                  _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState377 _v
              | FLOAT ->
                  _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | FLOATLIT _v ->
                  _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState377 _v
              | FOR ->
                  _menhir_run323 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | IDENTIFIER _v ->
                  _menhir_run321 _menhir_env (Obj.magic _menhir_stack) MenhirState377 _v
              | IF ->
                  _menhir_run317 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | INCREMENT ->
                  _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | INT ->
                  _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | INTLIT _v ->
                  _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState377 _v
              | LCURL ->
                  _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | LONG ->
                  _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | LPAR ->
                  _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | MINUS ->
                  _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | NEW ->
                  _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | NOT ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | NULLLIT _v ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState377 _v
              | PLUS ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | RETURN ->
                  _menhir_run313 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | SEMI ->
                  _menhir_run312 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | SHORT ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | STRLIT _v ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState377 _v
              | SUPER ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | SWITCH ->
                  _menhir_run303 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | SYNCHRONIZED ->
                  _menhir_run299 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | THIS ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | THROW ->
                  _menhir_run296 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | TRY ->
                  _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | VOID ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | WHILE ->
                  _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState377
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState377)
          | WHILE ->
              _menhir_reduce170 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_error376 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState377 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce171 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState302 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState278 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce103 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState274 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if _menhir_env._menhir_error then
          _menhir_error381 _menhir_env (Obj.magic _menhir_stack)
        else
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
                  let _tok = _menhir_env._menhir_token in
                  (match _tok with
                  | BNOT ->
                      _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | BOOLEAN ->
                      _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | BOOLEANLIT _v ->
                      _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState383 _v
                  | BYTE ->
                      _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | CHAR ->
                      _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | CHARLIT _v ->
                      _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState383 _v
                  | DECREMENT ->
                      _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | DOUBLE ->
                      _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | DOUBLELIT _v ->
                      _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState383 _v
                  | FLOAT ->
                      _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | FLOATLIT _v ->
                      _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState383 _v
                  | IDENTIFIER _v ->
                      _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState383 _v
                  | INCREMENT ->
                      _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | INT ->
                      _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | INTLIT _v ->
                      _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState383 _v
                  | LONG ->
                      _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | LPAR ->
                      _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | MINUS ->
                      _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | NEW ->
                      _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | NOT ->
                      _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | NULLLIT _v ->
                      _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState383 _v
                  | PLUS ->
                      _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | SHORT ->
                      _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | STRLIT _v ->
                      _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState383 _v
                  | SUPER ->
                      _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | THIS ->
                      _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | VOID ->
                      _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState383
                  | _ ->
                      assert (not _menhir_env._menhir_error);
                      _menhir_env._menhir_error <- true;
                      _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState383)
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  let _menhir_stack = Obj.magic _menhir_stack in
                  let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_error381 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce107 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState414 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce106 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState417 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce105 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState248 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if _menhir_env._menhir_error then
          _menhir_error419 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | ELSE ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | ASSERT ->
                  _menhir_run395 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | BNOT ->
                  _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | BOOLEAN ->
                  _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | BOOLEANLIT _v ->
                  _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState420 _v
              | BREAK ->
                  _menhir_run391 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | BYTE ->
                  _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | CHAR ->
                  _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | CHARLIT _v ->
                  _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState420 _v
              | CONTINUE ->
                  _menhir_run387 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | DECREMENT ->
                  _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | DO ->
                  _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | DOUBLE ->
                  _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | DOUBLELIT _v ->
                  _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState420 _v
              | FLOAT ->
                  _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | FLOATLIT _v ->
                  _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState420 _v
              | FOR ->
                  _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | IDENTIFIER _v ->
                  _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState420 _v
              | IF ->
                  _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | INCREMENT ->
                  _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | INT ->
                  _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | INTLIT _v ->
                  _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState420 _v
              | LCURL ->
                  _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | LONG ->
                  _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | LPAR ->
                  _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | MINUS ->
                  _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | NEW ->
                  _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | NOT ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | NULLLIT _v ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState420 _v
              | PLUS ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | RETURN ->
                  _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | SEMI ->
                  _menhir_run238 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | SHORT ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | STRLIT _v ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState420 _v
              | SUPER ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | SWITCH ->
                  _menhir_run217 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | SYNCHRONIZED ->
                  _menhir_run213 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | THIS ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | THROW ->
                  _menhir_run210 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | TRY ->
                  _menhir_run208 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | VOID ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | WHILE ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState420
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState420)
          | ASSERT | BNOT | BOOLEAN | BOOLEANLIT _ | BREAK | BYTE | CHAR | CHARLIT _ | CONTINUE | DECREMENT | DO | DOUBLE | DOUBLELIT _ | FINAL | FLOAT | FLOATLIT _ | FOR | IDENTIFIER _ | IF | INCREMENT | INT | INTLIT _ | LCURL | LONG | LPAR | MINUS | NEW | NOT | NULLLIT _ | PLUS | RCURL | RETURN | SEMI | SHORT | STRLIT _ | SUPER | SWITCH | SYNCHRONIZED | THIS | THROW | TRY | VOID | WHILE ->
              _menhir_reduce170 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_error419 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState420 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce171 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState447 | MenhirState2 | MenhirState433 | MenhirState209 | MenhirState428 | MenhirState243 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (stmt : (string))) = _menhir_stack in
        let _v : (string) =                   ( stmt ) in
        _menhir_goto_localVariableDeclOrStmt _menhir_env _menhir_stack _menhir_s _v
    | MenhirState216 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce103 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        _menhir_fail ()

and _menhir_run363 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =         ( ";" ) in
    _menhir_goto_forExpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce186 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (js : (string)) ->
    let _v : (string) =                ( js ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_methodCall : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (mc : (string)) = _v in
    let _v : (string) =                  ( mc ) in
    _menhir_goto_complexPrimaryNoParenthesis _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_dims : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIM ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | COL | COMM | EQUAL | GETHAN | GTHAN | IDENTIFIER _ | INSTANCEOF | LETHAN | LTHAN | NEQUAL | OR | QM | RBRAC | RCURL | RPAR | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (tn : (string))), _, (ds : (string))) = _menhir_stack in
            let _v : (string) =                        ( tn^ds ) in
            _menhir_goto_typeSpecifier _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIM ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (qn : (string))), _, (d : (string))) = _menhir_stack in
            let _v : (string) =                          ( qn^d ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BNOT ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | BOOLEAN ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | BOOLEANLIT _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
                | BYTE ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | CHAR ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | CHARLIT _v ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
                | DECREMENT ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | DOUBLE ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | DOUBLELIT _v ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
                | FLOAT ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | FLOATLIT _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
                | IDENTIFIER _v ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
                | INCREMENT ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | INT ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | INTLIT _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
                | LONG ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | LPAR ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | MINUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | NEW ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | NOT ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | NULLLIT _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
                | PLUS ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | SHORT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | STRLIT _v ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
                | SUPER ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | THIS ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | VOID ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState194)
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
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIM ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (pt : (string))), _, (d : (string))) = _menhir_stack in
            let _v : (string) =                               ( pt^d ) in
            _menhir_goto_primitiveTypeExpression _menhir_env _menhir_stack _menhir_s _v
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
        | DIM ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack)
        | AND | ANDEQUAL | ASSIGN | BAND | BOR | COL | COMM | DECREMENT | DIV | DIVEQUAL | DOT | EQUAL | GETHAN | GTHAN | INCREMENT | INSTANCEOF | LCURL | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (tn : (string))), _, (d : (string))) = _menhir_stack in
            let _1 = () in
            let _v : (string) =                              ( "new "^tn^d ) in
            _menhir_goto_arrayAllocationExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState202 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIM ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack)
        | AND | ANDEQUAL | ASSIGN | BAND | BOR | COL | COMM | DECREMENT | DIV | DIVEQUAL | DOT | EQUAL | GETHAN | GTHAN | INCREMENT | INSTANCEOF | LCURL | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (tn : (string))), _, (de : (string))), _, (d : (string))) = _menhir_stack in
            let _1 = () in
            let _v : (string) =                                     ( "new "^tn^de^d ) in
            _menhir_goto_arrayAllocationExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typeName : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIM ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | LBRAC ->
            _menhir_run198 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | LPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState23 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | BOOLEAN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | BOOLEANLIT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | BYTE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | CHAR ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | CHARLIT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | DECREMENT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | DOUBLE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | DOUBLELIT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | FLOAT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | FLOATLIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | IDENTIFIER _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | INCREMENT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | INT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | INTLIT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | LONG ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | LPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | NULLLIT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState24 in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (tn : (string))), _) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _1 = () in
                let _v : (string) =                              ( "new"^tn^"("^")" ) in
                _menhir_goto_classAllocationExpression _menhir_env _menhir_stack _menhir_s _v
            | SHORT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | STRLIT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | SUPER ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | THIS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
    | MenhirState447 | MenhirState2 | MenhirState433 | MenhirState209 | MenhirState428 | MenhirState243 | MenhirState324 | MenhirState284 | MenhirState252 | MenhirState254 | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIM ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | AND | BAND | BOR | COL | COMM | EQUAL | GETHAN | GTHAN | IDENTIFIER _ | INSTANCEOF | LETHAN | LTHAN | NEQUAL | OR | QM | RBRAC | RCURL | RPAR | SEMI | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (tn : (string))) = _menhir_stack in
            let _v : (string) =              ( tn ) in
            _menhir_goto_typeSpecifier _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
    | _ ->
        _menhir_fail ()

and _menhir_goto_fieldAccess : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (fa : (string)) = _v in
    let _v : (string) =                   ( fa ) in
    _menhir_goto_complexPrimaryNoParenthesis _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_guardingStmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState274 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState368 | MenhirState365 | MenhirState328 | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce187 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState2 | MenhirState447 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState414 | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce187 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState262 | MenhirState105 | MenhirState157 | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (ex : (string))) = _menhir_stack in
        let _v : (string) =                ( ex ) in
        _menhir_goto_variableInitializer _menhir_env _menhir_stack _menhir_s _v
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | BOOLEAN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | BOOLEANLIT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | BYTE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | CHAR ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | CHARLIT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | DECREMENT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | DOUBLE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | DOUBLELIT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | FLOAT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | FLOATLIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | IDENTIFIER _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | INCREMENT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | INT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | INTLIT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | LONG ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | LPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | NULLLIT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | SHORT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | STRLIT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | SUPER ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | THIS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRAC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (cp : (string))), _, (e : (string))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (string) =                                               ( cp^" [ "^e^" ] ") in
            _menhir_goto_arrayAccess _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState24 | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (ex : (string))) = _menhir_stack in
        let _v : (string) =                ( ex ) in
        _menhir_goto_argumentList _menhir_env _menhir_stack _menhir_s _v
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (args : (string))), _, (ex : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string) =                                         ( args^" , "^ex ) in
        _menhir_goto_argumentList _menhir_env _menhir_stack _menhir_s _v
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRAC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (qn : (string))), _), _, (e : (string))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (string) =                                              ( qn^" [ "^e^" ] " ) in
            _menhir_goto_arrayAccess _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack)
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
            | BNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | BOOLEAN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | BOOLEANLIT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
            | BYTE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | CHAR ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | CHARLIT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
            | DOUBLE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | DOUBLELIT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
            | FLOAT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | FLOATLIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
            | IDENTIFIER _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
            | INT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | INTLIT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
            | LONG ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | NULLLIT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
            | SHORT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | STRLIT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
            | SUPER ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | THIS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | AND | ANDEQUAL | ASSIGN | BAND | BOR | COL | COMM | DECREMENT | DIV | DIVEQUAL | DOT | EQUAL | GETHAN | GTHAN | INCREMENT | INSTANCEOF | LBRAC | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
                _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRAC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (ex : (string))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (string) =                            ( "["^ex^"]" ) in
            (match _menhir_s with
            | MenhirState202 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (d : (string)) = _v in
                let (_menhir_stack, _menhir_s, (ds : (string))) = _menhir_stack in
                let _v : (string) =                          ( ds^d ) in
                _menhir_goto_dimExprs _menhir_env _menhir_stack _menhir_s _v
            | MenhirState23 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (de : (string)) = _v in
                let _v : (string) =             ( de ) in
                _menhir_goto_dimExprs _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSERT ->
                _menhir_run395 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | BNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | BOOLEAN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | BOOLEANLIT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
            | BREAK ->
                _menhir_run391 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | BYTE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | CHAR ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | CHARLIT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
            | CONTINUE ->
                _menhir_run387 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | DECREMENT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | DO ->
                _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | DOUBLE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | DOUBLELIT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
            | FLOAT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | FLOATLIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
            | FOR ->
                _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | IDENTIFIER _v ->
                _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
            | IF ->
                _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | INCREMENT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | INT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | INTLIT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
            | LCURL ->
                _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | LONG ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | LPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | NULLLIT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | RETURN ->
                _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | SEMI ->
                _menhir_run238 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | SHORT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | STRLIT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
            | SUPER ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | SWITCH ->
                _menhir_run217 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | SYNCHRONIZED ->
                _menhir_run213 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | THIS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | THROW ->
                _menhir_run210 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | TRY ->
                _menhir_run208 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | WHILE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState207)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState210 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce114 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState214 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSERT ->
                _menhir_run395 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | BNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | BOOLEAN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | BOOLEANLIT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
            | BREAK ->
                _menhir_run391 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | BYTE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | CHAR ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | CHARLIT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
            | CONTINUE ->
                _menhir_run387 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | DECREMENT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | DO ->
                _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | DOUBLE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | DOUBLELIT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
            | FLOAT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | FLOATLIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
            | FOR ->
                _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | IDENTIFIER _v ->
                _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
            | IF ->
                _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | INCREMENT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | INT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | INTLIT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
            | LCURL ->
                _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | LONG ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | LPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | NULLLIT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | RETURN ->
                _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | SEMI ->
                _menhir_run238 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | SHORT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | STRLIT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
            | SUPER ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | SWITCH ->
                _menhir_run217 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | SYNCHRONIZED ->
                _menhir_run213 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | THIS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | THROW ->
                _menhir_run210 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | TRY ->
                _menhir_run208 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | WHILE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState216
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState216)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState218 ->
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
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState220 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CASE ->
                    _menhir_run225 _menhir_env (Obj.magic _menhir_stack) MenhirState221
                | DEFAULT ->
                    _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState221
                | RCURL ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState221 in
                    let _menhir_env = _menhir_discard _menhir_env in
                    _menhir_reduce189 _menhir_env (Obj.magic _menhir_stack) _menhir_s
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState221)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState220)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState239 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce112 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState246 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSERT ->
                _menhir_run395 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | BNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | BOOLEAN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | BOOLEANLIT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
            | BREAK ->
                _menhir_run391 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | BYTE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | CHAR ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | CHARLIT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
            | CONTINUE ->
                _menhir_run387 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | DECREMENT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | DO ->
                _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | DOUBLE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | DOUBLELIT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
            | FLOAT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | FLOATLIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
            | FOR ->
                _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | IDENTIFIER _v ->
                _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
            | IF ->
                _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | INCREMENT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | INT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | INTLIT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
            | LCURL ->
                _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | LONG ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | LPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | NULLLIT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | RETURN ->
                _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | SEMI ->
                _menhir_run238 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | SHORT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | STRLIT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
            | SUPER ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | SWITCH ->
                _menhir_run217 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | SYNCHRONIZED ->
                _menhir_run213 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | THIS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | THROW ->
                _menhir_run210 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | TRY ->
                _menhir_run208 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | WHILE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState248
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState248)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState271 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSERT ->
                _menhir_run395 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | BNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | BOOLEAN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | BOOLEANLIT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _v
            | BREAK ->
                _menhir_run391 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | BYTE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | CHAR ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | CHARLIT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _v
            | CONTINUE ->
                _menhir_run387 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | DECREMENT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | DO ->
                _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | DOUBLE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | DOUBLELIT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _v
            | FLOAT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | FLOATLIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _v
            | FOR ->
                _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | IDENTIFIER _v ->
                _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _v
            | IF ->
                _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | INCREMENT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | INT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | INTLIT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _v
            | LCURL ->
                _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | LONG ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | LPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | NULLLIT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _v
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | RETURN ->
                _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | SEMI ->
                _menhir_run238 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | SHORT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | STRLIT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _v
            | SUPER ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | SWITCH ->
                _menhir_run217 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | SYNCHRONIZED ->
                _menhir_run213 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | THIS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | THROW ->
                _menhir_run210 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | TRY ->
                _menhir_run208 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | WHILE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState273
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState273)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState276 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSERT ->
                _menhir_run338 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | BNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | BOOLEAN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | BOOLEANLIT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _v
            | BREAK ->
                _menhir_run334 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | BYTE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | CHAR ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | CHARLIT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _v
            | CONTINUE ->
                _menhir_run330 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | DECREMENT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | DO ->
                _menhir_run329 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | DOUBLE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | DOUBLELIT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _v
            | FLOAT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | FLOATLIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _v
            | FOR ->
                _menhir_run323 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | IDENTIFIER _v ->
                _menhir_run321 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _v
            | IF ->
                _menhir_run317 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | INCREMENT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | INT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | INTLIT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _v
            | LCURL ->
                _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | LONG ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | LPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | NULLLIT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _v
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | RETURN ->
                _menhir_run313 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | SEMI ->
                _menhir_run312 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | SHORT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | STRLIT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _v
            | SUPER ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | SWITCH ->
                _menhir_run303 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | SYNCHRONIZED ->
                _menhir_run299 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | THIS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | THROW ->
                _menhir_run296 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | TRY ->
                _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | WHILE ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState278
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState278)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState296 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce114 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState300 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSERT ->
                _menhir_run338 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | BNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | BOOLEAN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | BOOLEANLIT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
            | BREAK ->
                _menhir_run334 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | BYTE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | CHAR ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | CHARLIT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
            | CONTINUE ->
                _menhir_run330 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | DECREMENT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | DO ->
                _menhir_run329 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | DOUBLE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | DOUBLELIT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
            | FLOAT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | FLOATLIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
            | FOR ->
                _menhir_run323 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | IDENTIFIER _v ->
                _menhir_run321 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
            | IF ->
                _menhir_run317 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | INCREMENT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | INT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | INTLIT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
            | LCURL ->
                _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | LONG ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | LPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | NULLLIT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | RETURN ->
                _menhir_run313 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | SEMI ->
                _menhir_run312 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | SHORT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | STRLIT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
            | SUPER ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | SWITCH ->
                _menhir_run303 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | SYNCHRONIZED ->
                _menhir_run299 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | THIS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | THROW ->
                _menhir_run296 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | TRY ->
                _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | WHILE ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState302)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState304 ->
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
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState306 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CASE ->
                    _menhir_run225 _menhir_env (Obj.magic _menhir_stack) MenhirState307
                | DEFAULT ->
                    _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState307
                | RCURL ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState307 in
                    let _menhir_env = _menhir_discard _menhir_env in
                    _menhir_reduce189 _menhir_env (Obj.magic _menhir_stack) _menhir_s
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState307)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState306)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState313 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce112 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState318 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSERT ->
                _menhir_run338 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | BNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | BOOLEAN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | BOOLEANLIT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _v
            | BREAK ->
                _menhir_run334 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | BYTE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | CHAR ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | CHARLIT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _v
            | CONTINUE ->
                _menhir_run330 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | DECREMENT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | DO ->
                _menhir_run329 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | DOUBLE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | DOUBLELIT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _v
            | FLOAT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | FLOATLIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _v
            | FOR ->
                _menhir_run323 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | IDENTIFIER _v ->
                _menhir_run321 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _v
            | IF ->
                _menhir_run317 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | INCREMENT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | INT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | INTLIT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _v
            | LCURL ->
                _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | LONG ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | LPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | NULLLIT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _v
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | RETURN ->
                _menhir_run313 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | SEMI ->
                _menhir_run312 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | SHORT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | STRLIT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _v
            | SUPER ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | SWITCH ->
                _menhir_run303 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | SYNCHRONIZED ->
                _menhir_run299 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | THIS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | THROW ->
                _menhir_run296 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | TRY ->
                _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | WHILE ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState320
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState320)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState326 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSERT ->
                _menhir_run338 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | BNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | BOOLEAN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | BOOLEANLIT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState328 _v
            | BREAK ->
                _menhir_run334 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | BYTE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | CHAR ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | CHARLIT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState328 _v
            | CONTINUE ->
                _menhir_run330 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | DECREMENT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | DO ->
                _menhir_run329 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | DOUBLE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | DOUBLELIT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState328 _v
            | FLOAT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | FLOATLIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState328 _v
            | FOR ->
                _menhir_run323 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | IDENTIFIER _v ->
                _menhir_run321 _menhir_env (Obj.magic _menhir_stack) MenhirState328 _v
            | IF ->
                _menhir_run317 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | INCREMENT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | INT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | INTLIT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState328 _v
            | LCURL ->
                _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | LONG ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | LPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | NULLLIT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState328 _v
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | RETURN ->
                _menhir_run313 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | SEMI ->
                _menhir_run312 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | SHORT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | STRLIT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState328 _v
            | SUPER ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | SWITCH ->
                _menhir_run303 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | SYNCHRONIZED ->
                _menhir_run299 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | THIS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | THROW ->
                _menhir_run296 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | TRY ->
                _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | WHILE ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState328)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState338 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | BOOLEAN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | BOOLEANLIT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
            | BYTE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | CHAR ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | CHARLIT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
            | DECREMENT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | DOUBLE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | DOUBLELIT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
            | FLOAT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | FLOATLIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
            | IDENTIFIER _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
            | INCREMENT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | INT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | INTLIT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
            | LONG ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | LPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | NULLLIT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | SHORT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | STRLIT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
            | SUPER ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | THIS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState341
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState341)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState341 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState346 ->
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
                _menhir_reduce104 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState2 | MenhirState447 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState248 | MenhirState420 | MenhirState413 | MenhirState417 | MenhirState414 | MenhirState273 | MenhirState274 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState364 | MenhirState371 | MenhirState368 | MenhirState365 | MenhirState328 | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (e : (string))) = _menhir_stack in
        let _v : (string) =               ( e ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState274 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState368 | MenhirState365 | MenhirState328 | MenhirState329 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                _menhir_reduce183 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState371 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (ess : (string))), _, (es : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                               ( ess^" , "^es) in
            _menhir_goto_expressionStmts _menhir_env _menhir_stack _menhir_s _v
        | MenhirState413 | MenhirState364 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (es : (string))) = _menhir_stack in
            let _v : (string) =                      ( es ) in
            _menhir_goto_expressionStmts _menhir_env _menhir_stack _menhir_s _v
        | MenhirState2 | MenhirState447 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState414 | MenhirState273 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                _menhir_reduce183 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | MenhirState412 | MenhirState362 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                    ( e^";" ) in
            _menhir_goto_forExpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState383 ->
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
                _menhir_reduce104 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState395 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | BOOLEAN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | BOOLEANLIT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState398 _v
            | BYTE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | CHAR ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | CHARLIT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState398 _v
            | DECREMENT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | DOUBLE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | DOUBLELIT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState398 _v
            | FLOAT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | FLOATLIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState398 _v
            | IDENTIFIER _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState398 _v
            | INCREMENT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | INT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | INTLIT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState398 _v
            | LONG ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | LPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | NULLLIT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState398 _v
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | SHORT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | STRLIT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState398 _v
            | SUPER ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | THIS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState398
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState398)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState398 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce180 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (es : (string)) ->
    let _v : (string) =               ( es ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run436 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LCURL ->
        _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState436
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState436

and _menhir_reduce188 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (b : (string))) = _menhir_stack in
    let _v : (string) =            ( b ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce40 : _menhir_env -> ('ttv_tail * _menhir_state * (string)) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (ch : (string))), _, (b : (string))) = _menhir_stack in
    let _v : (string) =                         ( ch^b ) in
    match _menhir_s with
    | MenhirState290 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState280 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState439 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState435 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce90 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, (b : (string))) = _menhir_stack in
    let _1 = () in
    let _v : (string) =                  ( "finally "^b ) in
    match _menhir_s with
    | MenhirState280 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState290 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce100 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState435 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState439 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce100 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run281 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LCURL ->
        _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState281
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState281

and _menhir_run283 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState284
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState284
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState284
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState284
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState284
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState284 _v
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState284
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState284
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState284
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState284
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState284)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_switchBlockStmtGroups : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState221 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CASE ->
            _menhir_run225 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | DEFAULT ->
            _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | RCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState233 in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce190 _menhir_env (Obj.magic _menhir_stack) _menhir_s
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState233)
    | MenhirState307 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CASE ->
            _menhir_run225 _menhir_env (Obj.magic _menhir_stack) MenhirState309
        | DEFAULT ->
            _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState309
        | RCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState309 in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce190 _menhir_env (Obj.magic _menhir_stack) _menhir_s
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState309)
    | _ ->
        _menhir_fail ()

and _menhir_goto_notJustName : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (id : (string)) = _v in
            let (_menhir_stack, _menhir_s, (njn : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (string) =                                      ( njn^"."^id ) in
            _menhir_goto_fieldAccess _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | AND | ANDEQUAL | ASSIGN | BAND | BOR | COL | COMM | DECREMENT | DIV | DIVEQUAL | EQUAL | GETHAN | GTHAN | INCREMENT | INSTANCEOF | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (njs : (string))) = _menhir_stack in
        let _v : (string) =                    (njs ) in
        _menhir_goto_primaryExpression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_qualifiedName : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState207 | MenhirState216 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState413 | MenhirState414 | MenhirState412 | MenhirState273 | MenhirState398 | MenhirState395 | MenhirState274 | MenhirState383 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState371 | MenhirState368 | MenhirState364 | MenhirState365 | MenhirState362 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState341 | MenhirState338 | MenhirState326 | MenhirState318 | MenhirState313 | MenhirState304 | MenhirState300 | MenhirState296 | MenhirState276 | MenhirState271 | MenhirState262 | MenhirState246 | MenhirState239 | MenhirState225 | MenhirState218 | MenhirState214 | MenhirState210 | MenhirState5 | MenhirState198 | MenhirState24 | MenhirState194 | MenhirState191 | MenhirState186 | MenhirState29 | MenhirState30 | MenhirState43 | MenhirState57 | MenhirState169 | MenhirState77 | MenhirState80 | MenhirState95 | MenhirState97 | MenhirState105 | MenhirState157 | MenhirState153 | MenhirState126 | MenhirState149 | MenhirState147 | MenhirState145 | MenhirState143 | MenhirState141 | MenhirState138 | MenhirState124 | MenhirState121 | MenhirState118 | MenhirState115 | MenhirState113 | MenhirState107 | MenhirState99 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState83 | MenhirState82 | MenhirState74 | MenhirState59 | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | LBRAC ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | AND | ANDEQUAL | ASSIGN | BAND | BOR | COL | COMM | DECREMENT | DIV | DIVEQUAL | EQUAL | GETHAN | GTHAN | INCREMENT | INSTANCEOF | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
            _menhir_reduce147 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_reduce128 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | MenhirState324 | MenhirState284 | MenhirState252 | MenhirState254 | MenhirState14 | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState134 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENTIFIER _v ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
        | AND | BAND | BOR | COL | COMM | DIM | EQUAL | GETHAN | GTHAN | IDENTIFIER _ | INSTANCEOF | LBRAC | LETHAN | LPAR | LTHAN | NEQUAL | OR | QM | RBRAC | RCURL | RPAR | SEMI | XOR ->
            _menhir_reduce199 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIM ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | DOT ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | LBRAC ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | AND | ANDEQUAL | ASSIGN | BAND | BOR | DECREMENT | DIV | DIVEQUAL | EQUAL | GETHAN | GTHAN | INCREMENT | INSTANCEOF | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RPAR | RSHIFT | RSHIFTEQUAL | XOR | XOREQUAL ->
            _menhir_reduce147 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_reduce128 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183)
    | MenhirState447 | MenhirState2 | MenhirState433 | MenhirState209 | MenhirState428 | MenhirState243 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState424
        | LBRAC ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState424
        | DIM | IDENTIFIER _ ->
            _menhir_reduce199 _menhir_env (Obj.magic _menhir_stack)
        | AND | ANDEQUAL | ASSIGN | BAND | BOR | DECREMENT | DIV | DIVEQUAL | EQUAL | GETHAN | GTHAN | INCREMENT | INSTANCEOF | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
            _menhir_reduce147 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_reduce128 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState424)
    | _ ->
        _menhir_fail ()

and _menhir_reduce181 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (ls : (string)) ->
    let _v : (string) =                 ( ls ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_forInit : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState324 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if _menhir_env._menhir_error then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState362
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | BNOT ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | BOOLEAN ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | BOOLEANLIT _v ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
          | BYTE ->
              _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | CHAR ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | CHARLIT _v ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
          | DECREMENT ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | DOUBLE ->
              _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | DOUBLELIT _v ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
          | FLOAT ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | FLOATLIT _v ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
          | IDENTIFIER _v ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
          | INCREMENT ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | INT ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | INTLIT _v ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
          | LONG ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | LPAR ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | MINUS ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | NEW ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | NOT ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | NULLLIT _v ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
          | PLUS ->
              _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | SEMI ->
              _menhir_run363 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | SHORT ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | STRLIT _v ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
          | SUPER ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | THIS ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | VOID ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState362
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState362)
    | MenhirState252 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if _menhir_env._menhir_error then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState412
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | BNOT ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | BOOLEAN ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | BOOLEANLIT _v ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
          | BYTE ->
              _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | CHAR ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | CHARLIT _v ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
          | DECREMENT ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | DOUBLE ->
              _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | DOUBLELIT _v ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
          | FLOAT ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | FLOATLIT _v ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
          | IDENTIFIER _v ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
          | INCREMENT ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | INT ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | INTLIT _v ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
          | LONG ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | LPAR ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | MINUS ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | NEW ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | NOT ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | NULLLIT _v ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
          | PLUS ->
              _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | SEMI ->
              _menhir_run363 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | SHORT ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | STRLIT _v ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
          | SUPER ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | THIS ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | VOID ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState412
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState412)
    | _ ->
        _menhir_fail ()

and _menhir_goto_jumpStmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState274 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState368 | MenhirState365 | MenhirState328 | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce186 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState2 | MenhirState447 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState414 | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce186 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_complexPrimary : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRAC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | AND | ANDEQUAL | ASSIGN | BAND | BOR | COL | COMM | DECREMENT | DIV | DIVEQUAL | DOT | EQUAL | GETHAN | GTHAN | INCREMENT | INSTANCEOF | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (cpri : (string))) = _menhir_stack in
        let _v : (string) =                        ( cpri ) in
        _menhir_goto_notJustName _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_methodAccess : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState77 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (ma : (string))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (string) =                              ( ma^"(  ) ") in
            _menhir_goto_methodCall _menhir_env _menhir_stack _menhir_s _v
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_primitiveTypeExpression : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =      ( " [ ] " ) in
    _menhir_goto_dims _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce198 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (pri : (string))) = _menhir_stack in
    let _v : (string) =                    ( pri ) in
    _menhir_goto_typeName _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (pt : (string))), _) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _v : (string) =                                  ( pt^".class " ) in
        _menhir_goto_fieldAccess _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_compilationUnit : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (string)) = _v in
    Obj.magic _1

and _menhir_reduce99 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * (string)) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _, (b : (string))), _, (c : (string))) = _menhir_stack in
    let _1 = () in
    let _v : (string) =                          ( "try "^b^c ) in
    _menhir_goto_guardingStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =          ( " an error has occured\n" ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _1 = () in
    let _v : (string) =       ( ";\n" ) in
    match _menhir_s with
    | MenhirState274 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState368 | MenhirState365 | MenhirState328 | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce180 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState2 | MenhirState447 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState414 | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce180 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce113 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _2 = () in
    let _1 = () in
    let _v : (string) =                ( "return;") in
    _menhir_goto_jumpStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_block : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (sls : (string))), _, (bss : (string))) = _menhir_stack in
        let _v : (string) =                             ( sls^"\n"^bss ) in
        (match _menhir_s with
        | MenhirState309 | MenhirState233 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (sbsg : (string)) = _v in
            let (_menhir_stack, _menhir_s, (sbsgs : (string))) = _menhir_stack in
            let _v : (string) =                                                          ( sbsgs^"\n"^sbsg ) in
            _menhir_goto_switchBlockStmtGroups _menhir_env _menhir_stack _menhir_s _v
        | MenhirState307 | MenhirState221 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (sbsg : (string)) = _v in
            let _v : (string) =                            ( sbsg ) in
            _menhir_goto_switchBlockStmtGroups _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            _menhir_fail ())
    | MenhirState279 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if _menhir_env._menhir_error then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState280
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | CATCH ->
              _menhir_run283 _menhir_env (Obj.magic _menhir_stack) MenhirState280
          | FINALLY ->
              _menhir_run281 _menhir_env (Obj.magic _menhir_stack) MenhirState280
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState280)
    | MenhirState281 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState292 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState274 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState368 | MenhirState365 | MenhirState328 | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce188 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState2 | MenhirState447 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState414 | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce188 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if _menhir_env._menhir_error then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState435
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | CATCH ->
              _menhir_run283 _menhir_env (Obj.magic _menhir_stack) MenhirState435
          | FINALLY ->
              _menhir_run436 _menhir_env (Obj.magic _menhir_stack) MenhirState435
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState435)
    | MenhirState436 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState441 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (s : (string))) = _menhir_stack in
        let _v : (string) =          ( s ) in
        _menhir_goto_compilationUnit _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_specialName : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AND | ANDEQUAL | ASSIGN | BAND | BOR | COL | COMM | DECREMENT | DIV | DIVEQUAL | DOT | EQUAL | GETHAN | GTHAN | INCREMENT | INSTANCEOF | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (spn : (string))) = _menhir_stack in
        let _v : (string) =                  ( spn ) in
        _menhir_goto_notJustName _menhir_env _menhir_stack _menhir_s _v
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (sn : (string))) = _menhir_stack in
        let _v : (string) =                   ( sn ) in
        _menhir_goto_methodAccess _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_arithmeticUnaryOperator : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121

and _menhir_reduce160 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
    let _v : (string) =                ( id ) in
    _menhir_goto_qualifiedName _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce115 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
    let _2 = () in
    let _v : (string) =                    ( id^" : " ) in
    match _menhir_s with
    | MenhirState274 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState368 | MenhirState365 | MenhirState328 | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce181 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState2 | MenhirState447 | MenhirState207 | MenhirState209 | MenhirState433 | MenhirState216 | MenhirState243 | MenhirState428 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState414 | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce181 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run253 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =         ( ";" ) in
    _menhir_goto_forInit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run275 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _v
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState276)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run279 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LCURL ->
        _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState279
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState279

and _menhir_run296 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState296

and _menhir_run299 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState300 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState300 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState300 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState300 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState300 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState300 _v
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState300 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState300 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState300)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run303 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState304 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState304 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState304 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState304 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState304 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState304 _v
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState304 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState304 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState304
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState304)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run312 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run313 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState313 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState313 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState313 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState313 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState313 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState313 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState313 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState313 in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce113 _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState313 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState313

and _menhir_run209 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSERT ->
        _menhir_run395 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _v
    | BREAK ->
        _menhir_run391 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _v
    | CONTINUE ->
        _menhir_run387 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | DO ->
        _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _v
    | FINAL ->
        _menhir_run254 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _v
    | FOR ->
        _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | IDENTIFIER _v ->
        _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _v
    | IF ->
        _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _v
    | LCURL ->
        _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | RCURL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState209 in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | RETURN ->
        _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | SEMI ->
        _menhir_run238 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | SWITCH ->
        _menhir_run217 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | SYNCHRONIZED ->
        _menhir_run213 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | THROW ->
        _menhir_run210 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | TRY ->
        _menhir_run208 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | WHILE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState209

and _menhir_run317 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState318 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState318 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState318 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState318 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState318 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState318 _v
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState318 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState318 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState318
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState318)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run321 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce115 _menhir_env (Obj.magic _menhir_stack)
    | AND | ANDEQUAL | ASSIGN | BAND | BOR | DECREMENT | DIV | DIVEQUAL | DOT | EQUAL | GETHAN | GTHAN | INCREMENT | INSTANCEOF | LBRAC | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LPAR | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
        _menhir_reduce160 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run323 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | FINAL ->
            _menhir_run254 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState324 _v
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | SEMI ->
            _menhir_run253 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState324)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run329 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSERT ->
        _menhir_run338 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
    | BREAK ->
        _menhir_run334 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
    | CONTINUE ->
        _menhir_run330 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | DO ->
        _menhir_run329 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
    | FOR ->
        _menhir_run323 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | IDENTIFIER _v ->
        _menhir_run321 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
    | IF ->
        _menhir_run317 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
    | LCURL ->
        _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | RETURN ->
        _menhir_run313 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | SEMI ->
        _menhir_run312 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | SWITCH ->
        _menhir_run303 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | SYNCHRONIZED ->
        _menhir_run299 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | THROW ->
        _menhir_run296 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | TRY ->
        _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | WHILE ->
        _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState329

and _menhir_run330 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_reduce110 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce111 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run334 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_reduce108 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce109 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run338 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState338 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState338 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState338 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState338 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState338 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState338 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState338 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState338 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState338

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_reduce111 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _2 = () in
    let _1 = () in
    let _v : (string) =                  ( "continue;") in
    _menhir_goto_jumpStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce110 : _menhir_env -> ('ttv_tail * _menhir_state) * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), (id : (string))) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (string) =                                    ( "continue "^id^"; ") in
    _menhir_goto_jumpStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce109 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _2 = () in
    let _1 = () in
    let _v : (string) =               ( "break;" ) in
    _menhir_goto_jumpStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce108 : _menhir_env -> ('ttv_tail * _menhir_state) * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), (id : (string))) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (string) =                           ( "break "^id^"; " ) in
    _menhir_goto_jumpStmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_complexPrimaryNoParenthesis : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (cpnp : (string))) = _menhir_stack in
        let _v : (string) =                                     ( cpnp ) in
        _menhir_goto_methodAccess _menhir_env _menhir_stack _menhir_s _v
    | AND | ANDEQUAL | ASSIGN | BAND | BOR | COL | COMM | DECREMENT | DIV | DIVEQUAL | DOT | EQUAL | GETHAN | GTHAN | INCREMENT | INSTANCEOF | LBRAC | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RBRAC | RCURL | RPAR | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (cprin : (string))) = _menhir_stack in
        let _v : (string) =                                      ( cprin ) in
        _menhir_goto_complexPrimary _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_primitiveType : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState207 | MenhirState216 | MenhirState248 | MenhirState420 | MenhirState417 | MenhirState413 | MenhirState414 | MenhirState412 | MenhirState273 | MenhirState398 | MenhirState395 | MenhirState274 | MenhirState383 | MenhirState278 | MenhirState302 | MenhirState320 | MenhirState377 | MenhirState371 | MenhirState368 | MenhirState364 | MenhirState365 | MenhirState362 | MenhirState328 | MenhirState329 | MenhirState346 | MenhirState341 | MenhirState338 | MenhirState326 | MenhirState318 | MenhirState313 | MenhirState304 | MenhirState300 | MenhirState296 | MenhirState276 | MenhirState271 | MenhirState262 | MenhirState246 | MenhirState239 | MenhirState225 | MenhirState218 | MenhirState214 | MenhirState210 | MenhirState5 | MenhirState198 | MenhirState24 | MenhirState194 | MenhirState191 | MenhirState186 | MenhirState29 | MenhirState30 | MenhirState33 | MenhirState43 | MenhirState57 | MenhirState169 | MenhirState77 | MenhirState80 | MenhirState95 | MenhirState97 | MenhirState105 | MenhirState157 | MenhirState153 | MenhirState126 | MenhirState149 | MenhirState147 | MenhirState145 | MenhirState143 | MenhirState141 | MenhirState138 | MenhirState124 | MenhirState121 | MenhirState118 | MenhirState115 | MenhirState113 | MenhirState107 | MenhirState99 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState83 | MenhirState82 | MenhirState74 | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState324 | MenhirState284 | MenhirState252 | MenhirState254 | MenhirState14 | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce198 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIM ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | DOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (pt : (string))) = _menhir_stack in
            let _v : (string) =                   ( pt ) in
            _menhir_goto_primitiveTypeExpression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188)
    | MenhirState447 | MenhirState2 | MenhirState433 | MenhirState209 | MenhirState428 | MenhirState243 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState425
        | DIM | IDENTIFIER _ ->
            _menhir_reduce198 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState425)
    | _ ->
        _menhir_fail ()

and _menhir_goto_logicalUnaryOperator : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce160 _menhir_env (Obj.magic _menhir_stack)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState447 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState447
    | MenhirState441 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState439 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce99 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState436 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState435 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState433 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState433
    | MenhirState428 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState428
    | MenhirState425 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState424 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState422 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState420 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState420
    | MenhirState417 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState417
    | MenhirState414 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState414
    | MenhirState413 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState413
    | MenhirState412 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState412
    | MenhirState398 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState398
    | MenhirState395 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | MenhirState383 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState383
    | MenhirState377 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState377
    | MenhirState371 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState371
    | MenhirState368 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState368
    | MenhirState365 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState365
    | MenhirState364 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState364
    | MenhirState362 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState362
    | MenhirState346 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState346
    | MenhirState341 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState341
    | MenhirState338 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState338
    | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState329
    | MenhirState328 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState328
    | MenhirState326 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState326
    | MenhirState324 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState320 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState320
    | MenhirState318 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState318
    | MenhirState313 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState313
    | MenhirState309 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState307 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState306 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState304 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState304
    | MenhirState302 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState302
    | MenhirState300 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState300
    | MenhirState296 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState296
    | MenhirState292 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState290 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState284 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState281 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState280 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState279 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState278 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState278
    | MenhirState276 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState276
    | MenhirState274 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState273
    | MenhirState271 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState271
    | MenhirState265 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState262 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState262
    | MenhirState259 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState255 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState254 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState252 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState248 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState248
    | MenhirState246 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState246
    | MenhirState243 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | MenhirState239 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | MenhirState233 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState225 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState221 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState220 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState218 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState218
    | MenhirState216 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState216
    | MenhirState214 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState214
    | MenhirState210 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | MenhirState209 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState209
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | MenhirState202 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (string) =          ( "ERROR -> statement" ) in
        _menhir_goto_compilationUnit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =         ( "void" ) in
    _menhir_goto_primitiveType _menhir_env _menhir_stack _menhir_s _v

and _menhir_run208 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LCURL ->
        _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState208
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState208

and _menhir_run210 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState210
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState210

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =       ( "this" ) in
    _menhir_goto_specialName _menhir_env _menhir_stack _menhir_s _v

and _menhir_run213 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _v
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState214)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run217 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState218)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =          ( "super" ) in
    _menhir_goto_specialName _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (stlit : (string)) = _v in
    let _v : (string) =               ( stlit ) in
    _menhir_goto_complexPrimaryNoParenthesis _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =          ( "short" ) in
    _menhir_goto_primitiveType _menhir_env _menhir_stack _menhir_s _v

and _menhir_run238 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run239 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState239 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState239 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState239 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState239 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState239 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState239 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState239 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState239 in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce113 _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState239 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState239
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState239

and _menhir_reduce35 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _2 = () in
    let _1 = () in
    let _v : (string) =                ( "{\n \n}\n" ) in
    _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =       ( "+" ) in
    _menhir_goto_arithmeticUnaryOperator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (string)) = _v in
    let _v : (string) =            ( "null" ) in
    _menhir_goto_specialName _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =        ( "!" ) in
    _menhir_goto_logicalUnaryOperator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =          ( "-" ) in
    _menhir_goto_arithmeticUnaryOperator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =         ( "long" ) in
    _menhir_goto_primitiveType _menhir_env _menhir_stack _menhir_s _v

and _menhir_run243 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSERT ->
        _menhir_run395 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState243 _v
    | BREAK ->
        _menhir_run391 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState243 _v
    | CONTINUE ->
        _menhir_run387 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | DO ->
        _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState243 _v
    | FINAL ->
        _menhir_run254 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState243 _v
    | FOR ->
        _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | IDENTIFIER _v ->
        _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState243 _v
    | IF ->
        _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState243 _v
    | LCURL ->
        _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState243 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | RCURL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState243 in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | RETURN ->
        _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | SEMI ->
        _menhir_run238 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState243 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | SWITCH ->
        _menhir_run217 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | SYNCHRONIZED ->
        _menhir_run213 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | THROW ->
        _menhir_run210 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | TRY ->
        _menhir_run208 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | WHILE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState243
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState243

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (ilit : (int)) = _v in
    let _v : (string) =                ( string_of_int ilit ) in
    _menhir_goto_complexPrimaryNoParenthesis _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =        ( "int" ) in
    _menhir_goto_primitiveType _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run245 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState246)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run249 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce115 _menhir_env (Obj.magic _menhir_stack)
    | AND | ANDEQUAL | ASSIGN | BAND | BOR | DECREMENT | DIM | DIV | DIVEQUAL | DOT | EQUAL | GETHAN | GTHAN | IDENTIFIER _ | INCREMENT | INSTANCEOF | LBRAC | LETHAN | LOGSHIFT | LOGSHIFTEQUAL | LPAR | LSHIFT | LSHIFTEQUAL | LTHAN | MINUS | MINUSEQUAL | MOD | MODEQUAL | MUL | MULEQUAL | NEQUAL | OR | OREQUAL | PEQUAL | PLUS | QM | RSHIFT | RSHIFTEQUAL | SEMI | XOR | XOREQUAL ->
        _menhir_reduce160 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run251 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | FINAL ->
            _menhir_run254 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | IDENTIFIER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | SEMI ->
            _menhir_run253 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState252)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (flit : (float)) = _v in
    let _v : (string) =                  ( string_of_float flit ) in
    _menhir_goto_complexPrimaryNoParenthesis _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =          ( "float" ) in
    _menhir_goto_primitiveType _menhir_env _menhir_stack _menhir_s _v

and _menhir_run254 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState254
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState254
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState254
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState254
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState254
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState254
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState254
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState254
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState254
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState254

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (dlit : (float)) = _v in
    let _v : (string) =                   ( string_of_float dlit ) in
    _menhir_goto_complexPrimaryNoParenthesis _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =           ( "double" ) in
    _menhir_goto_primitiveType _menhir_env _menhir_stack _menhir_s _v

and _menhir_run274 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSERT ->
        _menhir_run338 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState274 _v
    | BREAK ->
        _menhir_run334 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState274 _v
    | CONTINUE ->
        _menhir_run330 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | DO ->
        _menhir_run329 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState274 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState274 _v
    | FOR ->
        _menhir_run323 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | IDENTIFIER _v ->
        _menhir_run321 _menhir_env (Obj.magic _menhir_stack) MenhirState274 _v
    | IF ->
        _menhir_run317 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState274 _v
    | LCURL ->
        _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState274 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | RETURN ->
        _menhir_run313 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | SEMI ->
        _menhir_run312 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState274 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | SWITCH ->
        _menhir_run303 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | SYNCHRONIZED ->
        _menhir_run299 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | THROW ->
        _menhir_run296 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | TRY ->
        _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | WHILE ->
        _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState274

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run387 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_reduce110 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce111 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> (char) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (clit : (char)) = _v in
    let _v : (string) =                 ( "'"^(String.make 1 clit)^"'" ) in
    _menhir_goto_complexPrimaryNoParenthesis _menhir_env _menhir_stack _menhir_s _v

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =         ( "char" ) in
    _menhir_goto_primitiveType _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =         ( "byte" ) in
    _menhir_goto_primitiveType _menhir_env _menhir_stack _menhir_s _v

and _menhir_run391 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_reduce108 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce109 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (blit : (bool)) = _v in
    let _v : (string) =                    ( string_of_bool blit ) in
    _menhir_goto_complexPrimaryNoParenthesis _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =          ( "boolean" ) in
    _menhir_goto_primitiveType _menhir_env _menhir_stack _menhir_s _v

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (string) =       ( "~" ) in
    _menhir_goto_logicalUnaryOperator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run395 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | BOOLEAN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | BOOLEANLIT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
    | BYTE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | CHAR ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | CHARLIT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
    | DECREMENT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | DOUBLE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | DOUBLELIT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
    | FLOAT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | FLOATLIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
    | IDENTIFIER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
    | INCREMENT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | INT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | INTLIT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
    | LONG ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | NULLLIT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | SHORT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | STRLIT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
    | SUPER ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | THIS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState395
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState395

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
        | ASSERT ->
            _menhir_run395 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | BNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | BOOLEAN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | BOOLEANLIT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | BREAK ->
            _menhir_run391 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | BYTE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | CHAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | CHARLIT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | CONTINUE ->
            _menhir_run387 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DECREMENT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DO ->
            _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DOUBLE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DOUBLELIT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | FINAL ->
            _menhir_run254 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | FLOAT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | FLOATLIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | FOR ->
            _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | IDENTIFIER _v ->
            _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | IF ->
            _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | INCREMENT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | INT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | INTLIT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | LCURL ->
            _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LONG ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | NULLLIT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | RCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState2 in
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) _menhir_s
        | RETURN ->
            _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SEMI ->
            _menhir_run238 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SHORT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | STRLIT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | SUPER ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SWITCH ->
            _menhir_run217 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SYNCHRONIZED ->
            _menhir_run213 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | THIS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | THROW ->
            _menhir_run210 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | TRY ->
            _menhir_run208 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | WHILE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
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
  

