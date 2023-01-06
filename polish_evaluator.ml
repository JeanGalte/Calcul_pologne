open Set
open String
open List
open Char
open Float
open Printf

exception BadExp of string 
exception UnkwnownOp of string


type symbole = float 
(* Un opérateur peut être unaire ou bien binaire sur une calculatrice classique. Dans les 2 cas il prend un ou plusieurs arguments mais ne renvoie qu'un seul symbole *)
type unaire = (float -> float)
type binaire= (float -> float -> float)
type operateur =  Unaire of unaire| Binaire of binaire | Swap | Dup
type mix = Operateur of operateur | Symbole of symbole

module SS1 = Set.Make(Char)
module SS2 = Set.Make(String) 
module SS3 = Set.Make(String)
module SS4 = Set.Make(String)


let chiffres = SS1.of_list ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9'] 
let constantes = SS2.of_list ["pi";"e"]
let binop = SS3.of_list ["+";"-";"/";"*"] 
let unop = SS4.of_list ["NEG";"SIN";"COS";"TAN";"COSH";"SINH";"TANH";"ARCSIN";"ARCOS";"ARCTAN";"EXP";"LN";"LOG10";"SQRT"] 

(* Dictionnaires eco+ *)
 let assoc_bin (s : string) : binaire=
	match s with 
	| "*" ->  ( *. )
	| "-" -> ( -. )
	| "+" -> ( +. )
	| "/" -> ( /.)
	| "POW" -> Float.pow


let assoc_un (s : string) : unaire=
	match s with
	| "NEG" -> fun (x : symbole) : symbole -> ( -. ) 0. x 
	| "SIN" -> Float.sin
	| "COS" -> Float.cos
	| "TAN" -> Float.tan
	| "COSH" -> Float.cosh
	| "SINH" -> Float.sinh
	| "TANH" -> Float.tanh
	| "ARCSIN" -> Float.asin
	| "ARCOS" -> Float.acos
	| "ARCTAN" -> Float.atan
	| "EXP" -> Float.exp
	| "LN" -> Float.log
	| "LOG10" -> Float.log10
	| "SQRT" -> Float.sqrt


let assoc_const (c : string) :  float =
	match c with 
	| "pi" -> Float.pi
	| "e" -> 2.71828182846

let parse (s : string) = 
    let rec p (s2 : string list) (acc : mix list) : mix list  =
        match s2 with
        | [] -> List.rev acc
        | x::xs when SS1.mem x.[0] chiffres -> (Symbole (float_of_string x)) :: acc |> p xs
        | x::xs when SS2.mem x constantes -> (Symbole (assoc_const x)) :: acc |> p xs 
        | x::xs when SS3.mem x binop -> (Operateur (Binaire (assoc_bin x))) :: acc |> p xs
        | x::xs when SS4.mem x unop -> ((Operateur (Unaire (assoc_un x))) :: acc) |> p xs 
        | "DUP"::xs -> (Operateur Dup) :: acc |> p xs 
        | "SWAP"::xs -> (Operateur Swap) :: acc |> p xs
        | x::xs -> UnkwnownOp ("Élément inconu : " ^ x) |> raise
    in
  p (String.split_on_char ' ' s) []


let ev (l : mix list) : symbole = 
	let rec ev2 (ls : mix list) (acc : symbole list)  : symbole =
		begin match ls with
			| [] -> begin match acc with 
				| n :: [] -> n
				| _ -> raise  (BadExp "Cette expression n'est pas correctement écrite") 
				end
			| x :: xs -> begin match x with 
				| Symbole s -> ev2 xs (s :: acc)
				| Operateur o -> begin match o with 
					| Unaire u -> begin match acc with 
						| [] -> raise (BadExp "Cette expression n'est pas correctement écrite")
						| h :: hs -> ev2 xs ((u h) :: hs)  
						end
					| Binaire b -> begin match acc with 
						| e1 :: e2 :: el -> ev2 xs ((b e2 e1) :: el) 
						| _ -> raise (BadExp "Cette expression n'est pas correctement écrite") 
						end
					| Swap -> begin match acc with 
						| e1 :: e2 ::el -> ev2 xs (e2 :: e1 :: el)
						| _ -> raise (BadExp "Cette expression n'est pas correctement écrite")
						end
					| Dup -> begin match acc with
						| e1 :: el -> ev2 xs (e1 :: e1 :: el)
						| _ -> raise (BadExp "Cette expression n'est pas correctement écrite")
						end
					end
				end
			end 
	in ev2 l []
