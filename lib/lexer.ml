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

let to_string = function
  | Identifier i -> "Identifier(" ^ i ^ ")"
  | Number n -> "Number(" ^ string_of_int n ^ ")"
  | Semicolon -> ";"
  | Colon -> ":"
  | Equals -> "="
  | DoubleEquals -> "=="
  | NotEquals -> "!="
  | Not -> "!"
  | Loop -> "Loop"
  | If -> "If"
  | Then -> "Then"
  | Else -> "Else"
  | End -> "End"
  | EOF -> "EOF"

class position ?(row = 1) ?(col = 1) ?(index = 0) () =
  object
    val mutable row = row
    val mutable col = col
    val mutable index = index
    method row = row
    method col = col
    method index = index

    method increment (step : int) (src : string) =
      for _ = 1 to step do
        if index < String.length src then (
          let ch = src.[index] in
          index <- index + 1;
          if ch = '\n' then (
            row <- row + 1;
            col <- 1)
          else col <- col + 1)
      done
  end

class token (typeof : token_type) (pos : position) =
  object
    val typeof : token_type = typeof
    val pos : position = pos
    method typeof = typeof
    method pos = pos
  end

class lexer source =
  object (self)
    val mutable pos = new position ()
    val src = source
    val len = String.length source

    method private peek ?(step = 0) () : char option =
      let idx = pos#index + step in
      if idx >= len then None else Some (String.unsafe_get src idx)

    method private advance ?(step = 1) () : bool =
      if pos#index >= len then false
      else
        let actual_step = min step (len - pos#index) in
        pos#increment actual_step src;
        true

    method private skip_whitespace =
      while
        match self#peek () with
        | Some (' ' | '\n' | '\t' | '\r') -> self#advance ()
        | _ -> false
      do
        ()
      done

    method private read_number : (token, string) result =
      let start = pos#index in
      while
        match self#peek () with
        | Some ('0' .. '9' | '.') -> self#advance ()
        | _ -> false
      do
        ()
      done;
      let num_str = String.sub src start (pos#index - start) in
      match float_of_string_opt num_str with
      | Some f -> Ok (new token (Number (int_of_float f)) pos)
      | None -> Error ("invalid number: " ^ num_str)

    method private read_alpha : (token, string) result =
      let start = pos#index in
      while
        match self#peek () with
        | Some 'a' .. 'z' | Some 'A' .. 'Z' | Some '0' .. '9' | Some '_' ->
            ignore (self#advance ());
            true
        | _ -> false
      do
        ()
      done;
      let ident = String.sub src start (pos#index - start) in
      let token_type =
        match String.lowercase_ascii ident with
        | "loop" -> Loop
        | "if" -> If
        | "then" -> Then
        | "else" -> Else
        | "end" -> End
        | _ -> Identifier ident
      in
      Ok (new token token_type pos)

    method consume : (token, string) result =
      self#skip_whitespace;
      match self#peek () with
      | None -> Ok (new token EOF pos)
      | Some ch -> (
          let start =
            new position ~row:pos#row ~col:pos#col ~index:pos#index ()
          in
          match ch with
          | ';' ->
              ignore (self#advance ());
              Ok (new token Semicolon start)
          | ':' ->
              ignore (self#advance ());
              Ok (new token Colon start)
          | '=' -> (
              match self#peek ~step:1 () with
              | Some '=' ->
                  ignore (self#advance ~step:2 ());
                  Ok (new token DoubleEquals start)
              | _ ->
                  ignore (self#advance ());
                  Ok (new token Equals start))
          | '!' -> (
              match self#peek ~step:1 () with
              | Some '=' ->
                  ignore (self#advance ~step:2 ());
                  Ok (new token NotEquals start)
              | _ ->
                  ignore (self#advance ());
                  Ok (new token Not start))
          | '0' .. '9' -> self#read_number
          | 'a' .. 'z' | 'A' .. 'Z' | '_' -> self#read_alpha
          | _ -> Ok (new token EOF start))
  end

