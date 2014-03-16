(**************************************************************************)
(* Adapted from:                                                          *)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               *)
(*  and Didier Rémy.                                                      *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; version 2 of the License.               *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  General Public License for more details.                              *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *)
(*  02110-1301 USA                                                        *)
(*                                                                        *)
(**************************************************************************)

open Lexing
open Operators

type position_interval =
    {
      start_p : Lexing.position;
      end_p   : Lexing.position
    }

(* FIXME: maybe use a polymorphic variant instead *)
type position =
  (* FIXME: better names *)
  | Location of Lexing.position
  | Interval of position_interval
  | Undefined

let undefined_position = Undefined

let start_of_position = function
  | Location p -> p
  | Interval p -> p.start_p
  | Undefined -> Lexing.dummy_pos

let end_of_position = function
  | Location p -> p
  | Interval p -> p.end_p
  | Undefined -> Lexing.dummy_pos


let line p =
  p.pos_lnum

let column p =
  p.pos_cnum - p.pos_bol

let characters p1 p2 =
  if line p1 = line p2 then
    (column p1, column p2)
  else
    (column p1, column p1 + 1)


let merge =
  (* x1 <= x2 *)
  (* Merge a list of positions *)
  let comp x1 x2 =
    (x1.pos_lnum < x2.pos_lnum || x1.pos_bol <= x2.pos_bol)
  in
  let rec f min max = function
    | [] ->
      if min = max (* TODO: check correctness *)
      then Location min
      else Interval { start_p = min; end_p = max }

    | h :: t ->
      f
        (if comp h min then h else min)
        (if comp max h then h else max)
        t
  in

  function
  | h :: t -> f h h t
  | [] -> assert false


let join' = function
  | Undefined, x -> x
  | x, Undefined -> x
  | x1, x2 ->
    let get_pos = function
      | Interval { start_p = s; end_p = e } -> [s; e]
      | Location x -> [x]
      | _ -> assert false
    in
    merge $ get_pos x1 @ get_pos x2



let join = curry join'

let lex_join x1 x2 =
  Interval
    {
      start_p = x1;
      end_p   = x2
    }

let pos_from_single_lex lex_pos =
  Location lex_pos

let string_of_characters (c1, c2) =
  (string_of_int c1)^"-"^(string_of_int c2)

let string_of_lex_pos p =
  let c = p.pos_cnum - p.pos_bol in
  (string_of_int p.pos_lnum)^":"^(string_of_int c)

let string_of_pos = function
  | Interval p ->
    (if p.start_p.pos_fname <> "" then "File \""^p.start_p.pos_fname^"\", "
     else "")
    ^"line "^(string_of_int p.start_p.pos_lnum)
    ^", characters "^ string_of_characters (characters p.start_p p.end_p)

  | _ -> ""

(* TODO: other cases *)


let pos_or_undef = function
  | None -> undefined_position
  | Some x -> x

let cpos lexbuf =
  Interval
    {
      start_p = Lexing.lexeme_start_p lexbuf;
      end_p   = Lexing.lexeme_end_p   lexbuf;
    }

let string_of_cpos lexbuf =
  string_of_pos $ cpos lexbuf

let joinf f t1 t2 =
  join (f t1) (f t2)

let ljoinf f =
  List.fold_left (fun p t -> join p (f t)) undefined_position
