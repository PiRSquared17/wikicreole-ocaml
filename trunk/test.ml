open Lwt 

open Wiki_types 
open Wiki_widgets_interface

let stub_unit_right_aux ~sp _ = Lwt.return true 
let stub_wiki_right_aux ~sp _ =  Lwt.return true
let stub_wikibox_right_aux ~sp _ = Lwt.return true
let stub_wikipage_right_aux ~sp _ =  Lwt.return true

class wiki_rights_o : wiki_rights  =
object
  method can_create_wiki = stub_unit_right_aux 

  method can_admin_wiki = stub_wiki_right_aux
  method can_set_wiki_permissions = stub_wiki_right_aux
  method can_create_wikiboxes = stub_wiki_right_aux
  method can_create_subwikiboxes= stub_wiki_right_aux
  method can_create_wikicss   = stub_wiki_right_aux
  method can_create_wikipages  = stub_wiki_right_aux
  method can_delete_wikiboxes = stub_wiki_right_aux
  method can_view_static_files = stub_wiki_right_aux
  method can_edit_metadata = stub_wiki_right_aux

  method can_admin_wikibox = stub_wikibox_right_aux
  method can_set_wikibox_specific_permissions = stub_wikibox_right_aux
  method can_write_wikibox = stub_wikibox_right_aux
  method can_read_wikibox = stub_wikibox_right_aux
  method can_view_src = stub_wikibox_right_aux
  method can_view_history = stub_wikibox_right_aux
  method can_view_oldversions = stub_wikibox_right_aux
  method can_view_oldversions_src = stub_wikibox_right_aux

  method can_create_wikipagecss = stub_wikipage_right_aux
  method can_admin_wikipage = stub_wikipage_right_aux

end


let display sp =

  let bi = 
    { 
      bi_sp = sp ;
      bi_subbox = (fun _ -> Lwt.return None); 
      bi_ancestors = Ancestors.no_ancestors; 
      bi_box = Int32.zero; 
      bi_wiki = Int32.zero; 
      bi_page = Int32.zero, None; 
      bi_rights = new wiki_rights_o ;
      bi_menu_style = `Linear ; 
    } in 

    Wiki_syntax.std_of_wiki bi "== test ==" 

