(** Abstract syntax *)

(** The type of variable names. *)
type name = string

(** Haskella primitive types. *)
type htype =
    TInt (** integer [int] *)
  | TBool (** booleans [bool] *)
  | TTimes of htype * htype  (** Product [s * t] *)
  | TArrow of htype * htype  (** Function type *)
  | TList of htype (** Lists [t list] *)

(** Haskella expressions *)
(** Product types are used as custom data-structures. *)
type expr =
    Var of name 
  | Int of int
  | Bool of bool
  | Times of expr * expr
  | Divide of expr * expr
  | Mod of expr * expr
  | Plus of expr * expr
  | Minus of expr * expr
  | Equal of expr * expr
  | Less of expr * expr
  | If of expr * expr * expr
  | Fun of name * htype * expr
  | Apply of expr * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Rec of name * htype * expr
  | Nil of htype
  | Cons of expr * expr
  | Match of expr * htype * expr * name * name * expr

(** Toplevel commands *)
type toplevel_cmd =
    Expr of expr
  | Def of name * expr
  | Use of string
  | Quit

(** Conversion from a type to a string *)
let string_of_type ty =
  let rec to_str n ty =
    let (m, str) =
      match ty with
	  TInt -> (4, "int")
	| TBool -> (4, "bool")
	| TList ty -> (3, to_str 3 ty ^ " list")
	| TTimes (ty1, ty2) -> (2, (to_str 2 ty1) ^ " * " ^ (to_str 2 ty2))
	| TArrow (ty1, ty2) -> (1, (to_str 1 ty1) ^ " -> " ^ (to_str 0 ty2))
    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) ty

(** Conversion from an expression to a string *)
let string_of_expr e =
  let rec to_str n e =
    let (m, str) =
      match e with
	  Int n ->          (10, string_of_int n)
	| Bool b ->         (10, string_of_bool b)
	| Var x ->          (10, x)
	| Pair (e1, e2) ->  (10, "(" ^ (to_str 0 e1) ^ ", " ^ (to_str 0 e2) ^ ")")
	| Nil ty ->         (10, "[" ^ (string_of_type ty) ^ "]")
	| Fst e ->           (9, "fst " ^ (to_str 9 e))
	| Snd e ->           (9, "snd " ^ (to_str 9 e))
	| Apply (e1, e2) ->  (10, "<app>")
	    (* (9, (to_str 8 e1) ^ " " ^ (to_str 9 e2)) *)
	| Times (e1, e2) ->  (8, (to_str 7 e1) ^ " * " ^ (to_str 8 e2))
	| Divide (e1, e2) -> (8, (to_str 7 e1) ^ " / " ^ (to_str 8 e2))
	| Mod (e1, e2) ->    (8, (to_str 7 e1) ^ " % " ^ (to_str 8 e2))
	| Plus (e1, e2) ->   (7, (to_str 6 e1) ^ " + " ^ (to_str 7 e2))
	| Minus (e1, e2) ->  (7, (to_str 6 e1) ^ " - " ^ (to_str 7 e2))
	| Cons (e1, e2) ->   (6, (to_str 6 e1) ^ " :: " ^ (to_str 5 e2))
	| Equal (e1, e2) ->  (5, (to_str 5 e1) ^ " = " ^ (to_str 5 e2))
	| Less (e1, e2) ->   (5, (to_str 5 e1) ^ " < " ^ (to_str 5 e2))
	| If (e1, e2, e3) -> (4, "if " ^ (to_str 4 e1) ^ " then " ^
				(to_str 4 e2) ^ " else " ^ (to_str 4 e3))
	| Match (e1, ty, e2, x, y, e3) ->
	    (3, "match " ^ (to_str 3 e1) ^ " with " ^
	       "[" ^ (string_of_type ty) ^ "] -> " ^ (to_str 3 e2) ^ " | " ^
	       x ^ "::" ^ y ^ " -> " ^ (to_str 3 e3))
	| Fun (x, ty, e) -> (10, "<fun>")
	    (* (2, "fun " ^ x ^ " : " ^ (string_of_type ty) ^ " -> " ^ (to_str 0 e)) *)
	| Rec (x, ty, e) -> (10, "<rec>")
	    (* (1, "rec " ^ x ^ " : " ^ (string_of_type ty) ^ " is " ^ (to_str 0 e)) *)
	       
    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) e

(** [subst [(x1,e1);...;(xn;en)] e] replaces in [e] free occurrences
    of variables [x1], ..., [xn] with expressions [e1], ..., [en]. *)
let rec subst s = function
  |  (Var x) as e -> (try List.assoc x s with Not_found -> e)
  | (Int _ | Bool _ | Nil _) as e -> e
  | Times (e1, e2) -> Times (subst s e1, subst s e2)
  | Divide (e1, e2) -> Divide (subst s e1, subst s e2)
  | Mod (e1, e2) -> Mod (subst s e1, subst s e2)
  | Plus (e1, e2) -> Plus (subst s e1, subst s e2)
  | Minus (e1, e2) -> Minus (subst s e1, subst s e2)
  | Equal (e1, e2) -> Equal (subst s e1, subst s e2)
  | Cons (e1, e2) -> Cons  (subst s e1, subst s e2)
  | Less (e1, e2) -> Less (subst s e1, subst s e2)
  | If (e1, e2, e3) -> If (subst s e1, subst s e2, subst s e3)
  | Fun (x, ty, e) -> let s' = List.remove_assoc x s in Fun (x, ty, subst s' e)
  | Rec (x, ty, e) -> let s' = List.remove_assoc x s in Rec (x, ty, subst s' e)
  | Match (e1, ty, e2, x, y, e3) ->
      let s' = List.remove_assoc y (List.remove_assoc x s) in
	Match (subst s e1, ty, subst s e2, x, y, subst s' e3)
  | Apply (e1, e2) -> Apply (subst s e1, subst s e2)
  | Pair (e1, e2) -> Pair (subst s e1, subst s e2)
  | Fst e -> Fst (subst s e)
  | Snd e -> Snd (subst s e)

