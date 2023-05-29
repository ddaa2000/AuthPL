# 9 "lexer.mll"
 
open Support.Error

let reservedWords = [
  (* Keywords *)
  ("import", fun i -> Parser.IMPORT i);
  ("Bot", fun i -> Parser.TBOT i);
  ("lambda", fun i -> Parser.LAMBDA i);
  ("Top", fun i -> Parser.TTOP i);
  ("as", fun i -> Parser.AS i);
  ("String", fun i -> Parser.USTRING i);
  ("unit", fun i -> Parser.UNIT i);
  ("ref", fun i -> Parser.REF i);
  ("Unit", fun i -> Parser.UUNIT i);
  ("Ref", fun i -> Parser.RREF i);
  ("case", fun i -> Parser.CASE i);
  ("of", fun i -> Parser.OF i);
  ("let", fun i -> Parser.LET i);
  ("in", fun i -> Parser.IN i);
  ("fix", fun i -> Parser.FIX i);
  ("letrec", fun i -> Parser.LETREC i);
  ("if", fun i -> Parser.IF i);
  ("then", fun i -> Parser.THEN i);
  ("else", fun i -> Parser.ELSE i);
  ("true", fun i -> Parser.TRUE i);
  ("false", fun i -> Parser.FALSE i);
  ("Bool", fun i -> Parser.BOOL i);
  ("timesfloat", fun i -> Parser.TIMESFLOAT i);
  ("Source", fun i -> Parser.SSOURCE i);
  ("Sink", fun i -> Parser.SSINK i);
  ("Float", fun i -> Parser.UFLOAT i);
  ("succ", fun i -> Parser.SUCC i);
  ("pred", fun i -> Parser.PRED i);
  ("iszero", fun i -> Parser.ISZERO i);
  ("inert", fun i -> Parser.INERT i);
  ("type", fun i -> Parser.TYPE i);
  ("Nat", fun i -> Parser.NAT i);
  ("up", fun i -> Parser.UP i);
  ("down", fun i -> Parser.DOWN i);
  ("require", fun i -> Parser.REQUIRE i);
  ("auas", fun i -> Parser.AUAS i);
  
  (* Symbols *)
  ("_", fun i -> Parser.USCORE i);
  ("'", fun i -> Parser.APOSTROPHE i);
  ("\"", fun i -> Parser.DQUOTE i);
  ("!", fun i -> Parser.BANG i);
  ("#", fun i -> Parser.HASH i);
  ("$", fun i -> Parser.TRIANGLE i);
  ("*", fun i -> Parser.STAR i);
  ("|", fun i -> Parser.VBAR i);
  (".", fun i -> Parser.DOT i);
  (";", fun i -> Parser.SEMI i);
  (",", fun i -> Parser.COMMA i);
  ("/", fun i -> Parser.SLASH i);
  (":", fun i -> Parser.COLON i);
  ("::", fun i -> Parser.COLONCOLON i);
  ("=", fun i -> Parser.EQ i);
  ("==", fun i -> Parser.EQEQ i);
  ("[", fun i -> Parser.LSQUARE i); 
  ("<", fun i -> Parser.LT i);
  ("{", fun i -> Parser.LCURLY i); 
  ("(", fun i -> Parser.LPAREN i); 
  ("<-", fun i -> Parser.LEFTARROW i); 
  ("{|", fun i -> Parser.LCURLYBAR i); 
  ("[|", fun i -> Parser.LSQUAREBAR i); 
  ("}", fun i -> Parser.RCURLY i);
  (")", fun i -> Parser.RPAREN i);
  ("]", fun i -> Parser.RSQUARE i);
  (">", fun i -> Parser.GT i);
  ("|}", fun i -> Parser.BARRCURLY i);
  ("|>", fun i -> Parser.BARGT i);
  ("|]", fun i -> Parser.BARRSQUARE i);

  (* Special compound symbols: *)
  (":=", fun i -> Parser.COLONEQ i);
  ("->", fun i -> Parser.ARROW i);
  ("=>", fun i -> Parser.DARROW i);
  ("==>", fun i -> Parser.DDARROW i);
]

(* Support functions *)

type buildfun = info -> Parser.token
let (symbolTable : (string,buildfun) Hashtbl.t) = Hashtbl.create 1024
let _ =
  List.iter (fun (str,f) -> Hashtbl.add symbolTable str f) reservedWords

let createID i str =
  try (Hashtbl.find symbolTable str) i
  with _ ->
    if (String.get str 0) >= 'A' && (String.get str 0) <= 'Z' then
       Parser.UCID {i=i;v=str}
    else 
       Parser.LCID {i=i;v=str}

let lineno   = ref 1
and depth    = ref 0
and start    = ref 0

and filename = ref ""
and startLex = ref dummyinfo

let create inFile stream =
  if not (Filename.is_implicit inFile) then filename := inFile
  else filename := Filename.concat (Sys.getcwd()) inFile;
  lineno := 1; start := 0; Lexing.from_channel stream

let newline lexbuf = incr lineno; start := (Lexing.lexeme_start lexbuf)

let info lexbuf =
  createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)

let text = Lexing.lexeme

let stringBuffer = ref (Bytes.create 2048)
let stringEnd = ref 0

let resetStr () = stringEnd := 0

let addStr ch =
  let x = !stringEnd in
  let buffer = !stringBuffer
in
  if x = Bytes.length buffer then
    begin
      let newBuffer = Bytes.create (x*2) in
      Bytes.blit buffer 0 newBuffer 0 x;
      Bytes.set newBuffer x ch;
      stringBuffer := newBuffer;
      stringEnd := x+1
    end
  else
    begin
      Bytes.set buffer x ch;
      stringEnd := x+1
    end

let getStr () = Bytes.sub_string (!stringBuffer) 0 (!stringEnd)

let extractLineno yytext offset =
  int_of_string (String.sub yytext offset (String.length yytext - offset))

# 146 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\241\255\242\255\243\255\244\255\091\000\006\000\101\000\
    \007\000\071\000\115\000\090\000\185\000\218\000\039\001\108\000\
    \099\000\095\000\254\255\152\000\253\255\252\255\049\001\038\000\
    \059\001\035\000\046\000\118\000\069\001\079\001\089\001\099\001\
    \246\255\092\000\158\000\251\255\252\255\253\255\109\000\115\000\
    \255\255\254\255\135\000\255\255\161\000\162\000\132\000\003\000\
    \168\000\251\255\252\255\253\255\254\255\255\255\125\001\249\255\
    \135\001\251\255\252\255\253\255\254\255\255\255\145\001\250\255\
    ";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\255\255\010\000\011\000\010\000\
    \011\000\011\000\010\000\011\000\010\000\008\000\006\000\011\000\
    \011\000\011\000\255\255\000\000\255\255\255\255\255\255\255\255\
    \004\000\255\255\255\255\255\255\255\255\005\000\255\255\007\000\
    \255\255\009\000\255\255\255\255\255\255\255\255\003\000\003\000\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \006\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\036\000\000\000\000\000\000\000\255\255\255\255\
    \000\000\000\000\255\255\000\000\045\000\045\000\255\255\047\000\
    \049\000\000\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    ";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\019\000\018\000\000\000\019\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \019\000\004\000\003\000\015\000\005\000\005\000\005\000\004\000\
    \004\000\004\000\017\000\005\000\004\000\010\000\004\000\016\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\012\000\004\000\011\000\009\000\004\000\004\000\
    \005\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\006\000\005\000\004\000\004\000\013\000\
    \005\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\008\000\007\000\004\000\005\000\005\000\
    \005\000\005\000\032\000\032\000\033\000\032\000\005\000\032\000\
    \005\000\005\000\005\000\005\000\022\000\021\000\020\000\025\000\
    \005\000\026\000\005\000\027\000\032\000\005\000\028\000\005\000\
    \005\000\005\000\032\000\005\000\041\000\040\000\005\000\005\000\
    \005\000\019\000\018\000\032\000\019\000\005\000\047\000\042\000\
    \035\000\043\000\000\000\255\255\255\255\005\000\000\000\000\000\
    \000\000\032\000\051\000\005\000\000\000\000\000\000\000\005\000\
    \019\000\000\000\000\000\005\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\032\000\255\255\255\255\005\000\000\000\000\000\
    \038\000\000\000\053\000\000\000\000\000\039\000\000\000\005\000\
    \000\000\000\000\000\000\005\000\000\000\000\000\032\000\005\000\
    \000\000\005\000\000\000\000\000\000\000\005\000\005\000\005\000\
    \000\000\005\000\032\000\005\000\005\000\000\000\005\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
    \000\000\005\000\000\000\005\000\000\000\000\000\032\000\000\000\
    \000\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\013\000\000\000\255\255\052\000\000\000\000\000\000\000\
    \000\000\000\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\000\000\005\000\000\000\000\000\
    \000\000\005\000\000\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\005\000\000\000\005\000\
    \000\000\013\000\000\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\030\000\000\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\023\000\037\000\058\000\
    \000\000\255\255\255\255\000\000\057\000\000\000\000\000\000\000\
    \050\000\000\000\000\000\000\000\000\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\059\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\061\000\000\000\000\000\000\000\000\000\
    \000\000\060\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\000\000\047\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
    \005\000\005\000\006\000\008\000\009\000\009\000\005\000\011\000\
    \005\000\007\000\007\000\007\000\015\000\016\000\017\000\023\000\
    \007\000\025\000\007\000\026\000\011\000\005\000\027\000\010\000\
    \010\000\010\000\033\000\005\000\038\000\039\000\010\000\007\000\
    \010\000\019\000\019\000\007\000\019\000\007\000\046\000\042\000\
    \034\000\042\000\255\255\044\000\045\000\010\000\255\255\255\255\
    \255\255\010\000\048\000\010\000\255\255\255\255\255\255\005\000\
    \019\000\255\255\255\255\005\000\255\255\255\255\255\255\255\255\
    \255\255\007\000\007\000\044\000\045\000\007\000\255\255\255\255\
    \034\000\255\255\048\000\255\255\255\255\034\000\255\255\010\000\
    \255\255\255\255\255\255\010\000\255\255\255\255\011\000\005\000\
    \255\255\005\000\255\255\255\255\255\255\012\000\012\000\012\000\
    \255\255\007\000\007\000\007\000\012\000\255\255\012\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\010\000\
    \255\255\010\000\255\255\012\000\255\255\255\255\012\000\255\255\
    \255\255\012\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\013\000\255\255\047\000\048\000\255\255\255\255\255\255\
    \255\255\255\255\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\255\255\012\000\255\255\255\255\
    \255\255\012\000\255\255\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\012\000\255\255\012\000\
    \255\255\013\000\255\255\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\014\000\255\255\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\022\000\034\000\054\000\
    \255\255\044\000\045\000\255\255\054\000\255\255\255\255\255\255\
    \048\000\255\255\255\255\255\255\255\255\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\054\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\054\000\255\255\255\255\255\255\255\255\
    \255\255\054\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\054\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec main lexbuf =
   __ocaml_lex_main_rec lexbuf 0
and __ocaml_lex_main_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 157 "lexer.mll"
                           ( main lexbuf )
# 367 "lexer.ml"

  | 1 ->
# 159 "lexer.mll"
                           ( newline lexbuf; main lexbuf )
# 372 "lexer.ml"

  | 2 ->
# 161 "lexer.mll"
       ( error (info lexbuf) "Unmatched end of comment" )
# 377 "lexer.ml"

  | 3 ->
# 163 "lexer.mll"
       ( depth := 1; startLex := info lexbuf; comment lexbuf; main lexbuf )
# 382 "lexer.ml"

  | 4 ->
# 166 "lexer.mll"
    ( lineno := extractLineno (text lexbuf) 2 - 1; getFile lexbuf )
# 387 "lexer.ml"

  | 5 ->
# 169 "lexer.mll"
    ( lineno := extractLineno (text lexbuf) 7 - 1; getFile lexbuf )
# 392 "lexer.ml"

  | 6 ->
# 172 "lexer.mll"
    ( Parser.INTV{i=info lexbuf; v=int_of_string (text lexbuf)} )
# 397 "lexer.ml"

  | 7 ->
# 175 "lexer.mll"
    ( Parser.FLOATV{i=info lexbuf; v=float_of_string (text lexbuf)} )
# 402 "lexer.ml"

  | 8 ->
# 179 "lexer.mll"
    ( createID (info lexbuf) (text lexbuf) )
# 407 "lexer.ml"

  | 9 ->
# 183 "lexer.mll"
    ( createID (info lexbuf) (text lexbuf) )
# 412 "lexer.ml"

  | 10 ->
# 186 "lexer.mll"
    ( createID (info lexbuf) (text lexbuf) )
# 417 "lexer.ml"

  | 11 ->
# 190 "lexer.mll"
    ( createID (info lexbuf) (text lexbuf) )
# 422 "lexer.ml"

  | 12 ->
# 192 "lexer.mll"
       ( resetStr(); startLex := info lexbuf; string lexbuf )
# 427 "lexer.ml"

  | 13 ->
# 194 "lexer.mll"
      ( Parser.EOF(info lexbuf) )
# 432 "lexer.ml"

  | 14 ->
# 196 "lexer.mll"
     ( error (info lexbuf) "Illegal character" )
# 437 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_main_rec lexbuf __ocaml_lex_state

and comment lexbuf =
   __ocaml_lex_comment_rec lexbuf 34
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 200 "lexer.mll"
    ( depth := succ !depth; comment lexbuf )
# 449 "lexer.ml"

  | 1 ->
# 202 "lexer.mll"
    ( depth := pred !depth; if !depth > 0 then comment lexbuf )
# 454 "lexer.ml"

  | 2 ->
# 204 "lexer.mll"
    ( error (!startLex) "Comment not terminated" )
# 459 "lexer.ml"

  | 3 ->
# 206 "lexer.mll"
    ( comment lexbuf )
# 464 "lexer.ml"

  | 4 ->
# 208 "lexer.mll"
    ( newline lexbuf; comment lexbuf )
# 469 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

and getFile lexbuf =
   __ocaml_lex_getFile_rec lexbuf 42
and __ocaml_lex_getFile_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 211 "lexer.mll"
            ( getName lexbuf )
# 481 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_getFile_rec lexbuf __ocaml_lex_state

and getName lexbuf =
   __ocaml_lex_getName_rec lexbuf 44
and __ocaml_lex_getName_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 214 "lexer.mll"
                ( filename := (text lexbuf); finishName lexbuf )
# 493 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_getName_rec lexbuf __ocaml_lex_state

and finishName lexbuf =
   __ocaml_lex_finishName_rec lexbuf 46
and __ocaml_lex_finishName_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 217 "lexer.mll"
                ( main lexbuf )
# 505 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finishName_rec lexbuf __ocaml_lex_state

and string lexbuf =
   __ocaml_lex_string_rec lexbuf 48
and __ocaml_lex_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 220 "lexer.mll"
       ( Parser.STRINGV {i = !startLex; v=getStr()} )
# 517 "lexer.ml"

  | 1 ->
# 221 "lexer.mll"
       ( addStr(escaped lexbuf); string lexbuf )
# 522 "lexer.ml"

  | 2 ->
# 222 "lexer.mll"
       ( addStr '\n'; newline lexbuf; string lexbuf )
# 527 "lexer.ml"

  | 3 ->
# 223 "lexer.mll"
       ( error (!startLex) "String not terminated" )
# 532 "lexer.ml"

  | 4 ->
# 224 "lexer.mll"
       ( addStr (Lexing.lexeme_char lexbuf 0); string lexbuf )
# 537 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_string_rec lexbuf __ocaml_lex_state

and escaped lexbuf =
   __ocaml_lex_escaped_rec lexbuf 54
and __ocaml_lex_escaped_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 227 "lexer.mll"
       ( '\n' )
# 549 "lexer.ml"

  | 1 ->
# 228 "lexer.mll"
       ( '\t' )
# 554 "lexer.ml"

  | 2 ->
# 229 "lexer.mll"
        ( '\\' )
# 559 "lexer.ml"

  | 3 ->
# 230 "lexer.mll"
         ( '\034'  )
# 564 "lexer.ml"

  | 4 ->
# 231 "lexer.mll"
        ( '\'' )
# 569 "lexer.ml"

  | 5 ->
# 233 "lexer.mll"
    (
      let x = int_of_string(text lexbuf) in
      if x > 255 then
	error (info lexbuf) "Illegal character constant"
      else
	Char.chr x
    )
# 580 "lexer.ml"

  | 6 ->
# 241 "lexer.mll"
    ( error (info lexbuf) "Illegal character constant" )
# 585 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_escaped_rec lexbuf __ocaml_lex_state

;;

