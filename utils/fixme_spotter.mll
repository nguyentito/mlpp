{
type flag = Fixme | Todo

let cur_file = ref ""
let spotted = ref 0
let hashtags = ref 0
let do_print = ref true
let spot_limit = 20
let uppercase_only = true

let spot_tbl = Hashtbl.create 197 (* FIXME : use a hashset instead *)
let spot_list =
  [
    ("FIXME", 0);
    ("TODO", 0);
    ("CHECK", 0)
  ]


let id : 'a. 'a -> 'a = fun x -> x
let ($) f x = f x

let newline lexbuf =
  Lexing.new_line lexbuf

let extractPos lexbuf =
  let start_p = Lexing.lexeme_start_p lexbuf
  and stop_p = Lexing.lexeme_end_p lexbuf
  in
  (start_p.Lexing.pos_lnum,
   start_p.Lexing.pos_cnum - start_p.Lexing.pos_bol,
   stop_p.Lexing.pos_cnum - stop_p.Lexing.pos_bol)


let print_warning name lexbuf ishtag =
  let (line, start, stop) = extractPos lexbuf in
  if !do_print || ishtag then
    print_string
      (
	"File \"" ^ !cur_file ^
	  "\", line " ^ (string_of_int line) ^
	  ", characters " ^ (string_of_int start) ^
	  "-" ^ (string_of_int stop) ^
	  "\n" ^
	  "Warning: remaining \t" ^ (name) ^
	  "\n"
      );

  if ishtag then incr hashtags
  else incr spotted
  ;

  if !do_print && !spotted >= spot_limit
  then begin
    print_string "Too many flags remaining !\n";
    do_print := false
  end


let print_end () =
  if !spotted > 0 then
    print_string (!cur_file ^ ": " ^ (string_of_int !spotted)
                  ^ " remaining flags found.\n");

  if !hashtags > 0 then
    print_string (!cur_file ^ ": " ^ (string_of_int !hashtags)
                  ^ " remaining #tags found.\n")



let () = List.iter (fun x -> Hashtbl.add spot_tbl (String.uppercase (fst x)) (snd x))
  (* TODO: compact  ↑ *)
  spot_list;;


}
let alpha = ['a' - 'z' 'A' - 'Z']
let newline = ['\n']

rule spot = parse
  | newline {newline lexbuf; spot lexbuf}
  | _ {spot lexbuf}
  | alpha+ as word
      {
	    if (Hashtbl.mem spot_tbl
              (
                (if uppercase_only then id else String.uppercase) word
              ))
	    then print_warning word lexbuf false;
	    spot lexbuf
      }


  | ('#' alpha+) as hashtag
      {
	    print_warning hashtag lexbuf true;
	    spot lexbuf
      }

  | eof {print_end ()}


{
  for i = 1 to Array.length Sys.argv - 1 do
    cur_file := Sys.argv.(i);
    let c = open_in !cur_file in
    spot (Lexing.from_channel c);
    spotted := 0;
    hashtags := 0;
    do_print := true;
    close_in c;
  done
}
