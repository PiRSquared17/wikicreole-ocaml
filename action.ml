open Misc
  
open XHTML.M

let display source = 
  try  
    let lexbuf = Lexing.from_string source in
      Wikiparser.type_block Wikilexer.token lexbuf
  with _ -> [div ~a:[a_class ["error"]] [pcdata "SYNTAX ERROR"]]  
    
