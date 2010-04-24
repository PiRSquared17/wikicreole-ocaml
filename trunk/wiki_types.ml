(* Ocsimore
 * Copyright (C) 2005
 * Laboratoire PPS - Universit� Paris Diderot - CNRS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Incza., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(**
   @author Vincent Balat
   @author Boris Yakobowski
*)

open Opaque 
type wiki_arg = [ `Wiki ]
type wiki = [`Wiki] int32_t
let wiki_of_sql (i : int32) = (int32_t i : wiki)
let sql_of_wiki (i : wiki) = t_int32 i
let string_of_wiki i = Int32.to_string (sql_of_wiki i)
let wiki_of_string s = (Opaque.int32_t (Int32.of_string s) : wiki)


type wikibox_arg = [ `Wikibox ]
type wikibox = wikibox_arg Opaque.int32_t

let wikibox_of_sql (i : int32) = (int32_t i : wikibox)
let sql_of_wikibox (i : wikibox) = t_int32 i
let string_of_wikibox i = Int32.to_string (sql_of_wikibox i)

type wikipage = wiki * string

type wikipage_arg = [ `Wikipage ]
type wikipage_uid = wikipage_arg Opaque.int32_t

type 'a content_type = string
type wiki_model = string
let string_of_wiki_model = Ocsigen_lib.id
let wiki_model_of_string = Ocsigen_lib.id
let string_of_content_type = Ocsigen_lib.id
let content_type_of_string = Ocsigen_lib.id

type wiki_info = {
  wiki_id : wiki;
  wiki_title : string;
  wiki_descr : string;
  wiki_pages : string option;
  wiki_boxrights : bool;
  wiki_container : wikibox option;
  wiki_staticdir : string option;
  wiki_model : wiki_model;
  wiki_siteid: string option;
}

type wikibox_info = {
  wikibox_wiki : wiki;
  wikibox_comment: string option;
  wikibox_special_rights: bool;
  wikibox_id : wikibox;
}

type wikipage_info = {
  wikipage_wiki: wiki;
  wikipage_wikibox: wikibox;
  wikipage_page: string;
  wikipage_title: string option;
  wikipage_uid : wikipage_uid;
(*  wikipage_css_special_rights; *)
}

type media_type = string list


type 'a rights_aux = sp:Eliom_sessions.server_params -> 'a -> bool Lwt.t

class type wiki_rights =
object
  method can_create_wiki : unit rights_aux

  method can_admin_wiki :          wiki rights_aux
  method can_set_wiki_permissions :wiki rights_aux
  method can_create_wikiboxes :    wiki rights_aux
  method can_create_subwikiboxes : wiki rights_aux
  method can_create_wikicss :      wiki rights_aux
  method can_create_wikipages :    wiki rights_aux
  method can_delete_wikiboxes :    wiki rights_aux
  method can_view_static_files :   wiki rights_aux
  method can_edit_metadata :       wiki rights_aux

  method can_admin_wikibox : wikibox rights_aux
  method can_set_wikibox_specific_permissions : wikibox rights_aux
  method can_write_wikibox : wikibox rights_aux
  method can_read_wikibox : wikibox rights_aux
  method can_view_src : wikibox rights_aux
  method can_view_history : wikibox rights_aux
  method can_view_oldversions : wikibox rights_aux
  method can_view_oldversions_src : wikibox rights_aux

  method can_create_wikipagecss : wikipage rights_aux
  method can_admin_wikipage : wikipage rights_aux

end


type 'a wikibox_content =
    'a content_type * string option * int32


let wikibox_data_of_raw = Ocsigen_lib.id

let raw_of_wikibox_data = Ocsigen_lib.id 
