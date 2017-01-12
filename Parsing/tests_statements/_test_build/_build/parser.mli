
(* The type of tokens. *)

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

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val compilationUnit: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (string)
