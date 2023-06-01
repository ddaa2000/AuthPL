(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open Support.Pervasive
open Support.Error
open Syntax
open Core

let searchpath = ref [""]

let argDefs = [
  "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path"]

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
     (fun s ->
       match !inFile with
         Some(_) -> err "You must specify exactly one input file"
       | None -> inFile := Some(s))
     "";
  match !inFile with
      None -> err "You must specify an input file"
    | Some(s) -> s

let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error -> 
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let alreadyImported = ref ([] : string list)

let checkbinding fi ctx b = match b with
    NameBind -> NameBind
  | VarBind(tyT,a) -> VarBind(tyT,a)
  | TmAbbBind(t,None,a,p) -> TmAbbBind(t, Some(typeof ctx t),a,p) (*should add authentication checking here*)
  | TmAbbBind(t,Some(tyT),a,p) ->   (*should add authentication checking here*)
     let tyT' = typeof ctx t in
     if subtype ctx tyT' tyT then TmAbbBind(t,Some(tyT),a,p)
     else error fi "Type of binding does not match declared type"
  | TyVarBind -> TyVarBind
  | TyAbbBind(tyT) -> TyAbbBind(tyT)

let prbindingty ctx b = match b with
    NameBind -> ()
  | VarBind(tyT,a) -> pr ": "; printty ctx tyT 
  | TmAbbBind(t, tyT_opt,p,a) -> pr ": ";     (*should add authentication checking here*)
     (match tyT_opt with
         None -> printty ctx (typeof ctx t)
       | Some(tyT) -> printty ctx tyT)
  | TyVarBind -> ()
  | TyAbbBind(tyT) -> pr ":: *"

let rec process_file f (ctx,store,pTab) =
  if List.mem f (!alreadyImported) then
    (ctx,store,pTab)
  else (
    alreadyImported := f :: !alreadyImported;
    let cmds,_ = parseFile f ctx in
    let g (ctx,store,pTab) c =  
      open_hvbox 0;
      let results = process_command (ctx,store,pTab) c in
      print_flush();
      results
    in
      List.fold_left g (ctx,store,pTab) cmds)

and process_command (ctx,store,pTab) cmd = match cmd with
    Import(f) -> 
      process_file f (ctx,store,pTab)
  | Eval(fi,p,t) -> 
      (* pr "evaluation"; *)
      let tyT = typeof ctx t in
      let auth = authOf ctx pTab p t in
      let t',store  = eval ctx store pTab p t in
      printtm_ATerm true ctx t'; 
      print_break 1 2;
      pr ": ";
      printty ctx tyT;
      force_newline();
      (ctx,store,pTab)
  | Bind(fi,p,x,bind) -> 
      let bind = checkbinding fi ctx bind in (*check some validity*)
      let bind',store' = evalbinding ctx store pTab p bind in (*evaluate the terms*)
      pr x; pr " "; prbindingty ctx bind'; force_newline();
      addbinding ctx x bind', (shiftstore 1 store'), pTab (*add the binding to context ??????shiftstore??????*)
  | PersetRelDecl(h,l) ->
      let pTab' = addPerset pTab h l in
      prPersetTab pTab'; force_newline();
      (ctx,store,pTab')
  | PersetDecl(_) ->
      (ctx,store,pTab)
  
let main () = 
  let inFile = parseArgs() in (*parse the input file?*)
  let _ = process_file inFile (emptycontext, emptystore, emptyPersetTable) in
  ()

let () = set_max_boxes 1000
let () = set_margin 67
let res = 
  Printexc.catch (fun () -> 
    try main();0 
    with Exit x -> x) 
  ()
let () = print_flush()
let () = exit res
