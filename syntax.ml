open Format
open Support.Error
open Support.Pervasive
open Set

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type ty =
    TyBot
  | TyTop
  | TyId of string
  | TyVar of int * int
  | TyArr of ty * ty
  | TyRecord of (string * ty) list
  | TyVariant of (string * ty) list
  | TyRef of ty
  | TyString
  | TyUnit
  | TyBool
  | TySource of ty
  | TySink of ty
  | TyFloat
  | TyNat

type perset = 
  | Perset of string

(* type auAtom = 
    AuAtomUp of string
  | AuAtomDown of string *)

(* type auComp =
    AuComp of auth * auAtom *)

type auth =
    AuAtomUp of string
  | AuAtomDown of string
  | AuComp of auth * auth
  | AuArr of auth * auth

type term =
    TmVar of info * int * int
  | TmAbs of info * string * ty * auth * term
  | TmApp of info * term * term
  | TmAscribe of info * term * ty
  | TmString of info * string
  | TmUnit of info
  | TmLoc of info * int
  | TmRef of info * term
  | TmDeref of info * term 
  | TmAssign of info * term * term
  | TmCase of info * term * (string * (string * term)) list
  | TmTag of info * string * term * ty
  | TmLet of info * string * term * term
  | TmFix of info * term
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmFloat of info * float
  | TmTimesfloat of info * term * term
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmInert of info * ty
  | TmAuas of info * term * auth
  | TmRequire of info * term * auth

type binding =
    NameBind 
  | VarBind of ty * auth
  | TmAbbBind of term * (ty option) * (auth option) * perset
  | TyVarBind
  | TyAbbBind of ty

type context = (string * binding) list

type command =
    Import of string
  | Eval of info * perset * term
  | Bind of info * perset * string * binding
  | PersetRelDecl of perset * perset
  | PersetDecl of perset

(* ---------------------------------------------------------------------- *)
(* Context management *)

let emptycontext = []

let ctxlength ctx = List.length ctx

let addbinding ctx x bind = (x,bind)::ctx (*context is a list of (x, bind) where x is a name*)

let addname ctx x = addbinding ctx x NameBind

let rec isnamebound ctx x =
  match ctx with
      [] -> false
    | (y,_)::rest ->
        if y=x then true
        else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else ((x,NameBind)::ctx), x

let index2name fi ctx x =
  try
    let (xn,_) = List.nth ctx x in
    xn
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg x (List.length ctx))

let rec name2index fi ctx x =
  match ctx with
      [] -> error fi ("Identifier " ^ x ^ " is unbound")
    | (y,_)::rest ->
        if y=x then 0
        else 1 + (name2index fi rest x)

(* ---------------------------------------------------------------------- *)
(* Shifting *)

let tymap onvar c tyT = 
  let rec walk c tyT = match tyT with
    TyBot -> TyBot
  | TyArr(tyT1,tyT2) -> TyArr(walk c tyT1,walk c tyT2)
  | TyTop -> TyTop
  | TyString -> TyString
  | TyVariant(fieldtys) -> TyVariant(List.map (fun (li,tyTi) -> (li, walk c tyTi)) fieldtys)
  | TyId(b) as tyT -> tyT
  | TyRecord(fieldtys) -> TyRecord(List.map (fun (li,tyTi) -> (li, walk c tyTi)) fieldtys)
  | TyBool -> TyBool
  | TyFloat -> TyFloat
  | TyUnit -> TyUnit
  | TyRef(tyT1) -> TyRef(walk c tyT1)
  | TySource(tyT1) -> TySource(walk c tyT1)
  | TySink(tyT1) -> TySink(walk c tyT1)
  | TyVar(x,n) -> onvar c x n
  | TyNat -> TyNat
  in walk c tyT

let tmmap onvar ontype c t = 
  let rec walk c t = match t with
    TmVar(fi,x,n) -> onvar fi c x n
  | TmAbs(fi,x,tyT1,auth,t2) -> TmAbs(fi,x,ontype c tyT1,auth,walk (c+1) t2)
  | TmApp(fi,t1,t2) -> TmApp(fi,walk c t1,walk c t2)
  | TmAscribe(fi,t1,tyT1) -> TmAscribe(fi,walk c t1,ontype c tyT1)
  | TmAuas(fi,t1,auth) -> TmAuas(fi,walk c t1, auth)    (*may need auth shift in future*)
  | TmRequire(fi,t1,auth) -> TmRequire(fi,walk c t1, auth) (*may need auth shift in future*)
  | TmString _ as t -> t
  | TmUnit(fi) as t -> t
  | TmLoc(fi,l) as t -> t
  | TmRef(fi,t1) -> TmRef(fi,walk c t1)
  | TmDeref(fi,t1) -> TmDeref(fi,walk c t1)
  | TmAssign(fi,t1,t2) -> TmAssign(fi,walk c t1,walk c t2)
  | TmTag(fi,l,t1,tyT) -> TmTag(fi, l, walk c t1, ontype c tyT)
  | TmCase(fi,t,cases) ->
      TmCase(fi, walk c t,
             List.map (fun (li,(xi,ti)) -> (li, (xi,walk (c+1) ti)))
               cases)
  | TmLet(fi,x,t1,t2) -> TmLet(fi,x,walk c t1,walk (c+1) t2)
  | TmFix(fi,t1) -> TmFix(fi,walk c t1)
  | TmFloat _ as t -> t
  | TmTimesfloat(fi,t1,t2) -> TmTimesfloat(fi, walk c t1, walk c t2)
  | TmTrue(fi) as t -> t
  | TmFalse(fi) as t -> t
  | TmIf(fi,t1,t2,t3) -> TmIf(fi,walk c t1,walk c t2,walk c t3)
  | TmProj(fi,t1,l) -> TmProj(fi,walk c t1,l)
  | TmRecord(fi,fields) -> TmRecord(fi,List.map (fun (li,ti) ->
                                               (li,walk c ti))
                                    fields)
  | TmZero(fi)      -> TmZero(fi)
  | TmSucc(fi,t1)   -> TmSucc(fi, walk c t1)
  | TmPred(fi,t1)   -> TmPred(fi, walk c t1)
  | TmIsZero(fi,t1) -> TmIsZero(fi, walk c t1)
  | TmInert(fi,tyT) -> TmInert(fi,ontype c tyT)
  in walk c t

let typeShiftAbove d c tyT =
  tymap
    (fun c x n -> if x>=c then TyVar(x+d,n+d) else TyVar(x,n+d))
    c tyT

let termShiftAbove d c t =
  tmmap
    (fun fi c x n -> if x>=c then TmVar(fi,x+d,n+d) 
                     else TmVar(fi,x,n+d))
    (typeShiftAbove d)
    c t

let termShift d t = termShiftAbove d 0 t

let typeShift d tyT = typeShiftAbove d 0 tyT

let bindingshift d bind =
  match bind with
    NameBind -> NameBind
  | VarBind(tyT,auth) -> VarBind((typeShift d tyT), auth)
  | TmAbbBind(t,tyT_opt,auth,p) ->
     let tyT_opt' = match tyT_opt with
                      None->None
                    | Some(tyT) -> Some(typeShift d tyT) in
     TmAbbBind(termShift d t, tyT_opt',auth,p)
  | TyVarBind -> TyVarBind
  | TyAbbBind(tyT) -> TyAbbBind(typeShift d tyT)

(* ---------------------------------------------------------------------- *)
(* Substitution *)

let termSubst j s t =
  tmmap
    (fun fi j x n -> if x=j then termShift j s else TmVar(fi,x,n))
    (fun j tyT -> tyT)
    j t

let termSubstTop s t = 
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let typeSubst tyS j tyT =
  tymap
    (fun j x n -> if x=j then (typeShift j tyS) else (TyVar(x,n)))
    j tyT

let typeSubstTop tyS tyT = 
  typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

let rec tytermSubst tyS j t =
  tmmap (fun fi c x n -> TmVar(fi,x,n))
        (fun j tyT -> typeSubst tyS j tyT) j t

let tytermSubstTop tyS t = 
  termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)

(* ---------------------------------------------------------------------- *)
(* Perset management (continued) *)
module OrderedPerset = struct 
  type t = perset
  (* use Pervasives compare *)
  let compare = compare
end

module Ints = Set.Make(Int)

module PSet = Set.Make(OrderedPerset)

type edge = {high : perset; low : perset}

type persetTable = PersetTable of PSet.t * edge list

let emptyPersetTable = PersetTable(PSet.empty, [])


let prPerset p =
  match p with Perset(s) -> pr s

let prPersets elist =
  List.iter (fun e -> match e with {high=h; low=l} -> prPerset h;pr "->"; prPerset l; pr "; ") elist
  
        
let prPersetTab persetTable =
  match persetTable with
  | PersetTable(pset, elist) -> prPersets elist



let rec persetChildren persetTable h = 
  match persetTable with
    | PersetTable(pset, elist) -> 
      let filterFrom edge = (match edge with
        | {high = h'; low = l'} when h = h' -> true
        | _ -> false)
        in h::List.concat (List.map (fun e -> (match e with {high = h'; low = l'} -> (persetChildren persetTable l'))) (List.filter filterFrom elist))


let hasPath persetTable h l =
  match persetTable with
    | PersetTable(pset, elist) -> 
        try let result = List.find (fun ele -> ele = l) (let children = persetChildren persetTable h in children) in true
        with Not_found -> false

let checkAddingEdge persetTable high low = 
  if hasPath persetTable low high then false else true


let addPerset persetTable high low =
  match persetTable with
    | PersetTable(pset, elist) ->
        if checkAddingEdge persetTable high low then PersetTable((PSet.add low (PSet.add high pset)), {high = high; low = low}::elist)
        (* else raise (Exit(1)) *)
        else ((pr "wrong perset relationship"); raise (Exit(1)))


let makeAuthUp p =
  match p with
    | Perset(s) -> AuAtomUp(s)

let makeAuthDown p =
  match p with
    | Perset(s) -> AuAtomDown(s)


(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)

let rec getbinding fi ctx i =
  try
    let (_,bind) = List.nth ctx i in
    bindingshift (i+1) bind 
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg i (List.length ctx))
 let getTypeFromContext fi ctx i =
   match getbinding fi ctx i with
         VarBind(tyT,_) -> tyT
     | TmAbbBind(_,Some(tyT),_,p) -> tyT     (* should add authentication checking here!*)
     | TmAbbBind(_,None,_,p) -> error fi ("No type recorded for variable "  (* should add authentication checking here!*)
                                        ^ (index2name fi ctx i))
     | _ -> error fi 
       ("getTypeFromContext: Wrong kind of binding for variable " 
         ^ (index2name fi ctx i)) 
(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tmInfo t = match t with
    TmVar(fi,_,_) -> fi
  | TmAbs(fi,_,_,_,_) -> fi
  | TmApp(fi, _, _) -> fi
  | TmAscribe(fi,_,_) -> fi
  | TmString(fi,_) -> fi
  | TmUnit(fi) -> fi
  | TmLoc(fi,_) -> fi
  | TmRef(fi,_) -> fi
  | TmDeref(fi,_) -> fi
  | TmAssign(fi,_,_) -> fi
  | TmTag(fi,_,_,_) -> fi
  | TmCase(fi,_,_) -> fi
  | TmLet(fi,_,_,_) -> fi
  | TmFix(fi,_) -> fi
  | TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmFloat(fi,_) -> fi
  | TmTimesfloat(fi,_,_) -> fi
  | TmProj(fi,_,_) -> fi
  | TmRecord(fi,_) -> fi
  | TmZero(fi) -> fi
  | TmSucc(fi,_) -> fi
  | TmPred(fi,_) -> fi
  | TmIsZero(fi,_) -> fi
  | TmInert(fi,_) -> fi 
  | TmAuas(fi,_,_) -> fi
  | TmRequire(fi,_,_) -> fi
(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let small t = 
  match t with
    TmVar(_,_,_) -> true
  | _ -> false

let rec printty_Type outer ctx tyT = match tyT with
    TyRef(tyT) -> pr "Ref "; printty_AType false ctx tyT
  | TySource(tyT) -> pr "Source "; printty_AType false ctx tyT
  | TySink(tyT) -> pr "Sink "; printty_AType false ctx tyT
  | tyT -> printty_ArrowType outer ctx tyT

and printty_ArrowType outer ctx  tyT = match tyT with 
    TyArr(tyT1,tyT2) ->
      obox0(); 
      printty_AType false ctx tyT1;
      if outer then pr " ";
      pr "->";
      if outer then print_space() else break();
      printty_ArrowType outer ctx tyT2;
      cbox()
  | tyT -> printty_AType outer ctx tyT

and printty_AType outer ctx tyT = match tyT with
    TyBot -> pr "Bot"
  | TyTop -> pr "Top"
  | TyString -> pr "String"
  | TyUnit -> pr "Unit"
  | TyVariant(fields) ->
        let pf i (li,tyTi) =
          if (li <> ((string_of_int i))) then (pr li; pr ":"); 
          printty_Type false ctx tyTi 
        in let rec p i l = match l with
            [] -> ()
          | [f] -> pf i f
          | f::rest ->
              pf i f; pr","; if outer then print_space() else break(); 
              p (i+1) rest
        in pr "<"; open_hovbox 0; p 1 fields; pr ">"; cbox()
  | TyBool -> pr "Bool"
  | TyId(b) -> pr b
  | TyRecord(fields) ->
        let pf i (li,tyTi) =
          if (li <> ((string_of_int i))) then (pr li; pr ":"); 
          printty_Type false ctx tyTi 
        in let rec p i l = match l with 
            [] -> ()
          | [f] -> pf i f
          | f::rest ->
              pf i f; pr","; if outer then print_space() else break(); 
              p (i+1) rest
        in pr "{"; open_hovbox 0; p 1 fields; pr "}"; cbox()
  | TyFloat -> pr "Float"
  | TyVar(x,n) ->
      if ctxlength ctx = n then
        pr (index2name dummyinfo ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | TyNat -> pr "Nat"
  | tyT -> pr "("; printty_Type outer ctx tyT; pr ")"

let printty ctx tyT = printty_Type true ctx tyT 

let rec printtm_Term outer ctx t = match t with
    TmAbs(fi,x,tyT1,_,t2) ->    (*print authority*)
      (let (ctx',x') = (pickfreshname ctx x) in
         obox(); pr "lambda ";
         pr x'; pr ":"; printty_Type false ctx tyT1; pr ".";
         if (small t2) && not outer then break() else print_space();
         printtm_Term outer ctx' t2;
         cbox())
  | TmAssign(fi, t1, t2) ->
       obox();
       printtm_AppTerm false ctx t1;
       pr " := ";
       printtm_AppTerm false ctx t2;
       cbox()
  | TmCase(_, t, cases) ->
      obox();
      pr "case "; printtm_Term false ctx t; pr " of";
      print_space();
      let pc (li,(xi,ti)) = let (ctx',xi') = (pickfreshname ctx xi) in
                              pr "<"; pr li; pr "="; pr xi'; pr ">==>"; 
                              printtm_Term false ctx' ti 
      in let rec p l = match l with 
            [] -> ()
          | [c] -> pc c
          | c::rest -> pc c; print_space(); pr "| "; p rest
      in p cases;
      cbox()
  | TmLet(fi, x, t1, t2) ->
       obox0();
       pr "let "; pr x; pr " = "; 
       printtm_Term false ctx t1;
       print_space(); pr "in"; print_space();
       printtm_Term false (addname ctx x) t2;
       cbox()
  | TmFix(fi, t1) ->
       obox();
       pr "fix "; 
       printtm_Term false ctx t1;
       cbox()
  | TmIf(fi, t1, t2, t3) ->
       obox0();
       pr "if ";
       printtm_Term false ctx t1;
       print_space();
       pr "then ";
       printtm_Term false ctx t2;
       print_space();
       pr "else ";
       printtm_Term false ctx t3;
       cbox()
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t = match t with
    TmApp(fi, t1, t2) ->
      obox0();
      printtm_AppTerm false ctx t1;
      print_space();
      printtm_ATerm false ctx t2;
      cbox()
  | TmRef(fi, t1) ->
       obox();
       pr "ref ";
       printtm_ATerm false ctx t1;
       cbox()
  | TmDeref(fi, t1) ->
       obox();
       pr "!";
       printtm_ATerm false ctx t1;
       cbox()
  | TmTimesfloat(_,t1,t2) ->
       pr "timesfloat "; printtm_ATerm false ctx t2; 
       pr " "; printtm_ATerm false ctx t2
  | TmPred(_,t1) ->
       pr "pred "; printtm_ATerm false ctx t1
  | TmIsZero(_,t1) ->
       pr "iszero "; printtm_ATerm false ctx t1
  | t -> printtm_PathTerm outer ctx t

and printtm_AscribeTerm outer ctx t = match t with
    TmAscribe(_,t1,tyT1) ->
      obox0();
      printtm_AppTerm false ctx t1;
      print_space(); pr "as ";
      printty_Type false ctx tyT1;
      cbox()
  | t -> printtm_ATerm outer ctx t

and printtm_PathTerm outer ctx t = match t with
    TmProj(_, t1, l) ->
      printtm_ATerm false ctx t1; pr "."; pr l
  | t -> printtm_AscribeTerm outer ctx t

and printtm_ATerm outer ctx t = match t with
    TmVar(fi,x,n) ->
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | TmString(_,s) -> pr ("\"" ^ s ^ "\"")
  | TmUnit(_) -> pr "unit"
  | TmLoc(fi, l) ->
       pr "<loc #"; print_int l; pr">"
  | TmTag(fi, l, t, tyT) ->
      obox();
      pr "<"; pr l; pr "="; printtm_Term false ctx t; pr ">";
      print_space();
      pr "as "; printty_Type outer ctx tyT;
      cbox();
  | TmTrue(_) -> pr "true"
  | TmFalse(_) -> pr "false"
  | TmFloat(_,s) -> pr (string_of_float s)
  | TmRecord(fi, fields) ->
       let pf i (li,ti) =
         if (li <> ((string_of_int i))) then (pr li; pr "="); 
         printtm_Term false ctx ti 
       in let rec p i l = match l with
           [] -> ()
         | [f] -> pf i f
         | f::rest ->
             pf i f; pr","; if outer then print_space() else break(); 
             p (i+1) rest
       in pr "{"; open_hovbox 0; p 1 fields; pr "}"; cbox()
  | TmZero(fi) ->
       pr "0"
  | TmSucc(_,t1) ->
     let rec f n t = match t with
         TmZero(_) -> pr (string_of_int n)
       | TmSucc(_,s) -> f (n+1) s
       | _ -> (pr "(succ "; printtm_ATerm false ctx t1; pr ")")
     in f 1 t1
  | TmInert(_,tyT) -> pr "inert["; printty_Type false ctx tyT; pr "]"
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

let printtm ctx t = printtm_Term true ctx t 

let prbinding ctx b = match b with
    NameBind -> ()
  | VarBind(tyT,_) -> pr ": "; printty ctx tyT (*todo print authority*)
  | TmAbbBind(t,tyT,_,p) -> pr "= "; printtm ctx t    (* should add authentication printing here!*)
  | TyVarBind -> ()
  | TyAbbBind(tyT) -> pr "= "; printty ctx tyT 


