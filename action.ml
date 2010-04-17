(* This is directly inspired from Ocsimore *)

open Misc

let (>>=) = Lwt.bind


let nothing _ _ = () and nothing1 _ = () ;; 

  (* This builder does essentially nothing, except on extensions
     where it calls plugin_action *)
let preparse_builder = {
  Wikicreole.chars = nothing1;
  strong_elem = nothing;
  em_elem = nothing;
  a_elem = (fun _ _ _ -> ());
  make_href = (fun _ a fragment -> match fragment with 
                 | None -> a
                 | Some f -> a ^"#"^f);
  br_elem = nothing1;
  img_elem = (fun _ _ _ -> ());
  tt_elem = nothing;
    monospace_elem = nothing;
    underlined_elem = nothing;
    linethrough_elem = nothing;
    subscripted_elem = nothing;
    superscripted_elem = nothing;
    nbsp = ();
    endash = ();
    emdash = ();
    p_elem = nothing;
    pre_elem = nothing;
    h1_elem = nothing;
    h2_elem = nothing;
    h3_elem = nothing;
    h4_elem = nothing;
    h5_elem = nothing;
    h6_elem = nothing;
    ul_elem = nothing;
    ol_elem = nothing;
    dl_elem = nothing;
    hr_elem = nothing1;
    table_elem = nothing;
    inline = nothing1;
    plugin = (fun _ -> ()) ;
    plugin_action = (fun  _-> ()); 
    link_action = (fun _ _ _ _ _ _ -> ()); 
    error = nothing1;
  } ;; 
  
let translate connection domain page = 
  Db.load connection domain page >>> Wikicreole_lexer.from_string () default_builder 
      
