(*** The entry point of the program ***)

open Operators

(** Command line options parsing. *)

let message = "Usage: joujou [options] input_file"
let print_version () =
  print_endline "TODO: version message.";
  exit 0

let flags_table = Hashtbl.create 12
(* TODO: descriptions for the flags *)
let flags = ["parsing-only"    , "Parse only";
             "inference-only"  , "";
             "elaboration-only", "";
             "implicitly-typed", "";
             "explicitly-typed", ""]
let set_flag fl = Hashtbl.add flags_table fl ()
let flags_option_list =
  List.map (fun (fl, desc) ->
             ("--" ^ fl, Arg.Unit (fun () -> set_flag fl), desc))
           flags
let flag_enabled = Hashtbl.mem flags_table
                               

let option_list = Arg.align $ flags_option_list @ [
  "--version", Arg.Unit print_version,
               "Display the version number."
]


let warning = output_string stderr =< ((^) "Warning: ") =< flip (^) "\n"

(* Get filename + handle flags *)
let filename =
  let filename = ref None in
  Arg.parse option_list (fun s -> filename := Some s) message;
  begin
    if flag_enabled "implicitly-typed" && flag_enabled "explicitly-typed"
    then Errors.fatal []
      "The flags '--implicitly-typed' and '--explicitly-typed' are incompatible."
  end;
  match !filename with
    | None -> Errors.fatal [] "No input file."
    | Some filename -> begin
      if Filename.check_suffix filename ".mle" then begin
        if flag_enabled "implicitly-typed"
        then warning "compiling a .mle file with '--implicitly-typed'."
        else set_flag "explicitly-typed"
      end
      else if Filename.check_suffix filename ".mlt" then begin
        if flag_enabled "explicitly-typed"
        then warning "compiling a .mlt file with '--explicitly-typed'."
        else set_flag "implicitly-typed"
      end
      else if not (flag_enabled "implicitly-typed"
                   || flag_enabled "explicitly-typed")
      then Errors.fatal [] "This is neither a .mlt file, nor a .mle file: you must specify whether it is implicitly or explicitly typed."
      (* Why should we only accept .mlt and .mle files *)
      else warning "this is neither a .mlt file, nor a .mle file."
    end;
    filename

let _ = if flag_enabled "explicitly-typed" && flag_enabled "inference-only"
  then warning "'--inference-only' makes no sense for an explicitly-typed file. This flag will be ignored."


type filename = Filename of string

type ('a, 'b) pass = 'a -> filename -> 'b

(* TODO: find a way to rewrite this with combinators *)
let ( $> ) : ('a, 'b) pass -> ('b, 'c) pass -> ('a, 'c) pass = fun p1 p2 ->
  fun x filename ->
    let y = p1 x filename in
    p2 y filename

let save_as ext ?(check = ignore) ?(skip = false) f =
  fun x ((Filename origin) as ofilename) ->
    let (y, printed_y) = f x ofilename in
    if not skip then begin
      let filename = Filename.chop_extension origin ^ ext in
      let cout = open_out filename in
      PPrint.ToChannel.pretty 0.8 100 cout printed_y;
      close_out cout;
      check filename
    end;
    y

(* Don't save .mls/.mlse unless we really want to try testing;
   since it's basically the identity function, nobody cares about that! *)

let parse : (unit, IAST.program) pass
= save_as ~skip:(not =< flag_enabled $ "parse-only") ".mls"
  (fun () (Filename f) ->
    let iast = ASTio.IAST.parse_program f in
    (iast, ASTio.IAST.pprint_program iast))

let parse_explicitly_typed : (unit, XAST.program) pass
= save_as ~skip:(not =< flag_enabled $ "parse-only") ".mlse"
  (fun () (Filename f) ->
    let xast = ASTio.XAST.parse_program f in
    (xast, ASTio.XAST.pprint_program xast))

let is_explicitly_typed_syntax f =
  try
    ignore (ASTio.XAST.parse_program f)
  with _ ->
    Errors.fatal [] (f ^ ": Syntax error in generated code.")

let elaborate_type_annotations : (IAST.program, XAST.program) pass
= save_as ".mle" ~check:is_explicitly_typed_syntax (fun iast _ ->
  let xast = InferTypes.program iast in
  (xast, ASTio.XAST.pprint_program xast)
)

let elaborate_dictionaries : (XAST.program, XAST.program) pass
= save_as ".mlr" ~check:is_explicitly_typed_syntax (fun xast _ ->
  let rast = ElaborateDictionaries.program xast in
  (rast, ASTio.XAST.pprint_program rast)
)

let compile : (XAST.program, unit) pass
= save_as ".ml" (fun xast _ ->
  ((), ASTio.XAST.pprint_program_in_ocaml xast)
)


let unless fl pass = if flag_enabled fl then fun _ _ -> exit 0 else pass

let () =
  (* "multi-way if" simulation with match + when *)
  let pipeline = match () with
    | () when flag_enabled "explicitly-typed" ->
      parse_explicitly_typed
        $> unless "parse-only"       elaborate_dictionaries
        $> unless "elaboration-only" compile
    | () when flag_enabled "implicitly-typed" ->
      parse 
        $> unless "parse-only"       elaborate_type_annotations
        $> unless "inference-only"   elaborate_dictionaries
        $> unless "elaboration-only" compile
    | () -> assert false
  in 
  pipeline () (Filename filename)
