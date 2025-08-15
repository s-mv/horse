type token_type =
  | Identifier of string
  | Number of int
  | Semicolon (* ; *)
  | Colon (* : *)
  | Equals (* = *)
  | DoubleEquals (* == *)
  | NotEquals (* != *)
  | Not (* ! *)
  | Loop
  | If
  | Then
  | Else
  | End
  | EOF

val to_string : token_type -> string

class position : ?row:int -> ?col:int -> ?index:int -> unit -> object
  method row : int
  method col : int
  method index : int
  method increment : int -> string -> unit
end

class token : token_type -> position -> object
  method typeof : token_type
  method pos : position
end

class lexer : string -> object
  method private peek : ?step:int -> unit -> char option
  method private advance : ?step:int -> unit -> bool
  method consume : (token, string) result
end

