%token LANG RANG LPAR RPAR LBRAC RBRAC LCURL RCURL (* <> () [] {} *) 
%token SEMI COL DOT COMM SQ DQ QM (* ; : . ,  '' "" ? *)
%token PLUS MINUS DIV MUL MOD  (* + - / * % *)
%token AND OR NOT (* && || ! *)
%token LSHIFT RSHIFT LOGSHIFT(* *)
%token BAND BOR XOR BNOT (* & | ^ ~ *) 
%token ANOT (* @ *)
%token EQUAL (* == *)
%token ASSIGN (* = *)
%token PEQUAL MEQUALEQUAL MULEQUAL DIVEQUAL MODEQUAL ANDEQUAL OREQUAL XOREQUAL RSHIFTEQUAL LSHIFTEQUAL LOGSHIFTEQUAL (* += -= *= /= %= &= |= ^= >>= <<= >>>= *)
(* keywords *)
%token ABSTRACT
%token ASSERT
%token BOOLEAN
%token BREAK
%token BYTE
%token CASE
%token CATCH
%token CHAR
%token CLASS
%token CONST
%token CONTINUE
%token DEFAULT
%token DO
%token DOUBLE 
%token ELSE
%token ENUM 
%token EXTENDS
%token FINAL
%token FINALLY
%token FLOAT
%token FOR
%token IF
%token GOTO
%token IMPLEMENTS
%token IMPORT
%token INSTANCEOF
%token INT
%token INTERFACE
%token LONG
%token NATIVE
%token NEW
%token PACKAGE
%token PRIVATE
%token PROTECTED
%token PUBLIC 
%token RETURN
%token SHORT
%token STATIC
%token STRICTFP
%token SUPER
%token SWITCH
%token SYNCHRONIZED
%token THIS
%token THROW
%token THROWS
%token TRANSIENT
%token TRY
%token VOID
%token VOLATILE
%token WHILE
(* *)
%token <string> IDENTIFIER
%token <string> STRLIT
%token <int> INTLIT
%token <float> FLOATLIT
%token EOF

%token ELIPSIS

%%

%%

