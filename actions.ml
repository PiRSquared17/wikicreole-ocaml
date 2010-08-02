(*
 * Simple wiki interface
 *
 * hacked from Ocsimore
 * removed utf8 support I'm afraid .. 
 *)

open XHTML.M

open Lwt 
open Lexer
  
let (>>>) f g = g f 

(* Helpers *)

let eat attribs label f acc = 
  try 
    let c = List.assoc label attribs in
      f c :: acc 
  with Not_found -> acc ;;


let length s = 
  if s.[0] = '%' then 
   `Percent (int_of_string (String.sub s 1 (String.length s - 1)))
 else 
   `Pixels (int_of_string (String.sub s 2 (String.length s - 2)))

let parse_common_attribs attribs = 
  let eat = eat attribs in 
    [] 
    >>> eat "class" (fun c -> a_class [c]) 
    >>> eat "id" (fun c -> a_id c) ;;


let parse_table_attribs attribs = 
  let eat = eat attribs in 
    [] 
    >>> eat "border" (fun c -> a_class [c]) 
    >>> eat "cellpadding" (fun c -> c >>> length >>> a_cellpadding) 
    >>> eat "cellspacing" (fun c -> c >>> length >>> a_cellspacing) 
    >>> eat "summary" (fun c -> a_summary c) 
    >>> eat "width" (fun c -> c >>> length >>> a_width) ;;

let parse_valign_attrib attribs =
  eat attribs "valign" (function 
			    "top" -> a_valign `Top
			  | "middle" -> a_valign `Middle
			  | "bottom" -> a_valign `Bottom
			  | "baseline" -> a_valign `Baseline) ;;

let parse_align_attrib attribs = 
  eat attribs "align" (function 
			   "left" -> a_align `Left
			 | "center" -> a_align `Center 
			 | "right" -> a_align `Right 
			 | "justify" -> a_align `Justify 
			 | "char" -> a_align `Char) ;;
    
let parse_scope_attrib attribs = 
  eat attribs "scope" (function 
			   "row" -> a_scope `Row 
			 | "col" -> a_scope `Col 
			 | "rowgroup" -> a_scope `Rowgroup
			 | "colgroup" -> a_scope `Colgroup) ;;
    
let parse_table_row_attribs attribs =
  let eat = eat attribs in
  [] 
    >>> eat "char" (fun c -> a_char c.[0])
    >>> eat "charoff" (fun c -> c >>> length >>> a_charoff)
    >>> parse_valign_attrib attribs 
    >>> parse_align_attrib attribs ;;

let parse_table_cell_attribs attribs =
  let eat = eat attribs in
    parse_common_attribs attribs 
    >>> eat "char" (fun c -> a_char c.[0])
    >>> eat "charoff" (fun c -> c >>> length >>> a_charoff)
    >>> eat "abbr" (fun c -> a_abbr c)
    >>> eat "axis" (fun c -> a_axis c)
    >>> eat "colspan" (fun c -> c >>> int_of_string >>> a_colspan)
    >>> eat "headers" (fun c -> a_headers [c]) 
    >>> eat "rowspan" (fun c -> c >>> int_of_string >>> a_rowspan)
    >>> parse_valign_attrib attribs 
    >>> parse_align_attrib attribs 
    >>> parse_scope_attrib attribs ;;

let make_string = (fun s -> pcdata s >>> return)

let element a = 
  Lwt_util.map_serial (fun x -> x) a >>= fun c -> return c

let strong_elem = (fun attribs a -> 
                     let atts = parse_common_attribs attribs in
		       element a >>= (fun r -> strong ~a:atts r >>> return))

let em_elem = (fun attribs a -> 
                 let atts = parse_common_attribs attribs in
		   element a >>= (fun r -> em ~a:atts r >>> return))

let a_elem = (fun attribs addr c -> 
		let atts = parse_common_attribs attribs in
		  element c >>= fun r -> a ~a:((a_href (uri_of_string addr)) :: atts) [ span r ] >>> return)

let br_elem = (fun attribs -> 
                 let atts = parse_common_attribs attribs in
		   br ~a:atts () >>> return)

let img_elem =
  (fun attribs addr alt -> 
     let atts = parse_common_attribs attribs in
       img ~a:atts ~alt:alt ~src:(uri_of_string addr) () >>> return)


let tt_elem = (fun attribs a ->
                 let atts = parse_common_attribs attribs in
		  element a >>= (fun r -> tt ~a:atts r >>> return))


let monospace_elem = (fun attribs a -> 
                        let atts = parse_common_attribs attribs in
			  element a >>= (fun r -> tt ~a:atts r >>> return))

let underlined_elem = (fun attribs a -> 
                         let atts = parse_common_attribs attribs in
			   element a >>= (fun r -> span ~a:((a_class [ "underlined"]) :: atts) r >>> return))

let linethrough_elem = (fun attribs a -> 
                          let atts = parse_common_attribs attribs in
			    element a >>= (fun r -> span ~a:((a_class [ "linethrough" ]) :: atts) r >>> return))

let subscripted_elem = (fun attribs a -> 
                          let atts = parse_common_attribs attribs in
			    element a >>= (fun r -> sub ~a:atts r >>> return))

let superscripted_elem = (fun attribs a -> 
                            let atts = parse_common_attribs attribs in
			     element a >>= (fun r -> sup ~a:atts r >>> return))



let p_elem = (fun attribs a -> 
                let atts = parse_common_attribs attribs in
                  element a >>= (fun r -> p ~a:atts r >>> return))

let pre_elem = (fun attribs a ->
                  let atts = parse_common_attribs attribs in
		    pre ~a:atts [String.concat "" a >>> pcdata] >>> return)


let h1_elem = (fun attribs a ->
                 let atts = parse_common_attribs attribs in
		   element a >>= fun r -> h1 ~a:atts r >>> return)

let h2_elem = (fun attribs a ->
                 let atts = parse_common_attribs attribs in
		   element a >>= fun r -> h2 ~a:atts r >>> return)
let h3_elem = (fun attribs a ->
                 let atts = parse_common_attribs attribs in
		   element a >>= fun r -> h3 ~a:atts r >>> return)
let h4_elem = (fun attribs a ->
                 let atts = parse_common_attribs attribs in
		   element a >>= fun r -> h4 ~a:atts r >>> return)
let h5_elem = (fun attribs a ->
                 let atts = parse_common_attribs attribs in
		   element a >>= fun r -> h5 ~a:atts r >>> return)
let h6_elem = (fun attribs a ->
                 let atts = parse_common_attribs attribs in
		   element a >>= fun r -> h6 ~a:atts r >>> return)

let list_builder = function 
    [] -> failwith "error in list_builder" 
  | a::l -> 
      let f (
	n, 
	children_option,
	(attribs: Lexer.attribs)) = 

	let atts = parse_common_attribs attribs in
	  element n >>= fun node -> 
	    match children_option with 
		None -> li ~a:atts node >>> return 
	      | Some c -> c >>= (fun r -> li ~a:atts node >>> return) in
	f a >>= 
	  fun r -> 
	    ((Lwt_util.map_serial f l) >>= 
	       fun r' -> (r, r') >>> return)

let descr_builder = function 
    [] -> failwith "error in descr_builder" 
  | a::l -> 
      let f (
	istitle, 
	d, 
	(attribs: Lexer.attribs)) = 
	let atts = parse_common_attribs attribs in
	  element d >>= fun d ->
	    if istitle 
	    then dt ~a:atts d >>> return 
	    else dd ~a:atts d >>> return in
	f a >>= 
	  fun r -> 
	    ((Lwt_util.map_serial f l) >>= 
	       fun r' -> (r, r') >>> return)



      
let ul_elem = (fun attribs a ->
                 let atts = parse_common_attribs attribs in
		 list_builder a >>= 
		   fun (t,q) -> ul ~a:atts t q >>> return)

let ol_elem = (fun attribs a ->
                 let atts = parse_common_attribs attribs in
		   list_builder a >>=
		     fun (t,q) -> ol ~a:atts t q >>> return)

let dl_elem = (fun attribs a ->
                 let atts = parse_common_attribs attribs in
		   descr_builder a >>= 
		     fun (t,q) -> dl ~a:atts t q >>> return)

let hr_elem = (fun attribs -> 
		 let atts = parse_common_attribs attribs in
		   hr ~a:atts () >>> return)

		  
let table_elem = (fun attribs l -> 
		    let atts = parse_table_attribs attribs in 
		      match l with 
			  [] -> failwith "error in table_elem"
			| row::rows -> 
			    let f (h, attribs, c) = 
			      let atts = parse_table_cell_attribs attribs in 
				element c >>= fun c  -> 
				  (if h 
				   then th ~a:atts c
				   else td ~a:atts c) >>> return in
			    let f2 (row, attribs) = match row with 
				[] -> failwith "error in table_elem" 
			      | a::l -> 
				  let atts = parse_table_row_attribs attribs in 
				    f a >>= fun r -> 
				      Lwt_util.map_serial f l >>=  fun l ->
					tr ~a:atts r l >>> return in 
			      f2 row >>= fun row -> 
				Lwt_util.map_serial f2 rows >>= fun rows -> 
				  table ~a:atts row rows >>> return )
					  

let inline e =  
  e >>= fun r -> span [ r ] >>> return 
   
let error s = 
  b [ pcdata s ] >>> return 

type actions = 
    (([ `Dl
      | `H1
      | `H2
      | `H3
      | `H4
      | `H5
      | `H6
      | `Hr
      
      | `Ol
      | `P
      | `Pre
      | `Table
      | `Ul ]
     ) XHTML.M.elt Lwt.t,
     [ `A
     | `Abbr
     | `Acronym
     | `B
     | `Bdo
     | `Big
     | `Br
     | `Button
     | `Cite
     | `Code
     | `Del
     | `Dfn
     | `Em
     | `I
     | `Img
     | `Input
     | `Ins
     | `Kbd
     | `Label
     | `Map
     | `Noscript
     | `Object
     | `PCDATA
     | `Q
     | `Samp
     | `Script
     | `Select
     | `Small
     | `Span
     | `Strong
     | `Sub
     | `Sup
     | `Textarea
     | `Tt
     | `Var
     | `Span ] XHTML.M.elt Lwt.t,

     [ `A
     | `Abbr
     | `Acronym
     | `B
     | `Bdo
     | `Big
     | `Br
     | `Button
     | `Cite
     | `Code
     | `Del
     | `Dfn
     | `Em
     | `I
     | `Img
     | `Input
     | `Ins
     | `Kbd
     | `Label
     | `Map
     | `Noscript
     | `Object
     | `PCDATA
     | `Q
     | `Samp
     | `Script
     | `Select
     | `Small
     | `Span
     | `Strong
     | `Sub
     | `Sup
     | `Textarea
     | `Tt
     | `Var
     ]
       XHTML.M.elt Lwt.t, 
     unit)  Lexer.builder
      
let actions : actions = 
  
  let plugin_action = (fun _ _ _ _ _ _ -> ()) and link_action = (fun _ _ _ _ _ -> ()) in
    
    {
      Lexer.chars = make_string;
      strong_elem = strong_elem; 
      em_elem = em_elem ;
      a_elem = a_elem ;
      make_href = (fun () a fragment -> match fragment with 
                     | None -> a
                     | Some f -> a ^"#"^f);
      br_elem = br_elem; 
      img_elem = img_elem; 
      tt_elem = tt_elem;
      monospace_elem = monospace_elem; 
      underlined_elem = underlined_elem;
      linethrough_elem = linethrough_elem;
      subscripted_elem = subscripted_elem;
      superscripted_elem = superscripted_elem;
      nbsp = "_" >>> pcdata >>> return;
      endash = "–" >>> pcdata >>> return;
      emdash = "—" >>> pcdata >>> return; 
      p_elem = p_elem;
      pre_elem = pre_elem;
      h1_elem = h1_elem;
      h2_elem = h2_elem;
      h3_elem = h3_elem;
      h4_elem = h4_elem;
      h5_elem = h5_elem;
      h6_elem = h6_elem;

      ul_elem = ul_elem;

      ol_elem = ol_elem;
      dl_elem = dl_elem; 
      hr_elem = hr_elem;
      table_elem = table_elem;
      inline = inline;

      plugin =
	(fun name ->
           let wiki_content = false in
             (wiki_content, (fun _ _ _ -> Lexer.A_content (span [] >>> return))));

      plugin_action = plugin_action;
      link_action = link_action;
      error = error ;
    } 

let display  source =
  Lexer.from_string () actions source >>= element 
    
