/*
 *
 *  This file is part of OWiki.
 *  
 *  OWiki is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  OWiki is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with OWiki.  If not, see <http://www.gnu.org/licenses/>.
 *  
 *
 *  wikiparser.mly ~wlf  23/07/2009 10:56
 *
 */


%{
  open XHTML.M
  open Eliom_predefmod.Xhtml
  open Eliom_predefmod
  open Eliom_services
  open Eliom_parameters
  open Eliom_sessions
    
  open Lwt 
 
  type cont_body = Xhtmltypes.body_content XHTML.M.elt list

  let spaces = Str.regexp_string " " 
%}


%token PRE
%token OPEN
%token CLOSE

%token INF

%token H1
%token H2
%token H3

%token<string> CONTENT
%token<string> CONTENTNOS

%token EGRAVE
%token EAIGU
%token UCIR
%token ECIR
%token AGRA
%token UGRA
%token CCED
%token OCIR

%token EOL
%token EOF

%token STAR1
%token STAR2
%token STAR3

%token SHARP1
%token SHARP2
%token SHARP3

%token BR

%token OCENTER
%token ECENTER 

%token BOLD
%token ITALIC 

%token TBRACKETL
%token TBRACKETR

%token OBRACKETL
%token OBRACKETR

%token QUOTE
%token WIKILINK
%token HTTPLINK
%token MAILTO

%token STYLE

%token IMAGE
%token OMAP
%token CMAP
%token RECT
%token TABLE

%token OBLOC 
%token CBLOC

%token OBOX
%token CBOX

%token SUP

%token OROW
%token CROW

%token OCAMLSCRIPT 

%token OSCRIPT
%token CSCRIPT 

%token SCRIPTURL 

%start type_block             /* the entry point */

%type < [> `Li] XHTML.M.elt list>list

%type < [> `A | `B | `Br | `Div | `H2 | `H3 | `I | `Ol | `PCDATA | `Ul] XHTML.M.elt list > box_content

%type < [> `A | `B | `Br | `I | `PCDATA] XHTML.M.elt list > text

%type < [> `H1] XHTML.M.elt > h1
%type < [> `H2] XHTML.M.elt > h2
%type < [> `H3] XHTML.M.elt > h3

%type < [> `Div] XHTML.M.elt >image
%type <Xhtmltypes.body_content XHTML.M.elt list> type_block
%%


content: 
| CONTENT          {  $1 }
| EGRAVE           {  "è" }
| EAIGU            {  "é" } 
| UCIR             {  "û" }
| ECIR             {  "ê" }
| AGRA             {  "à" }
| UGRA             {  "û" }
| CONTENT content  { $1 ^ $2 }
| EGRAVE content   { "è" ^ $2 } 
| EAIGU content    { "é" ^ $2 }
| UCIR content     { "û" ^ $2 }
| ECIR content     { "ê" ^ $2 }
| AGRA content     { "à" ^ $2 }
| UGRA content     { "û" ^ $2 }
| CCED content     { "ç" ^ $2 }
| OCIR content     { "ô" ^ $2 }

h1: 
  H1 content H1    { h1 [ pcdata $2]}

h2: 
  H2 content H2    { h2 [ pcdata $2]}

h3: 
  H3 content H3    { h3 [ pcdata $2]}

bloc: 
  OBLOC CONTENT SUP bloc_content { match Str.split spaces $2 with 
					  id::_ -> div ~a:[a_id id] $4 } 

box: 
  OBOX CONTENT SUP box_content   { match Str.split spaces $2 with 
				       classs::id::_ -> div ~a:[a_id id; a_class ["box" ^ classs]] $4 
				     | classs::_ -> div ~a:[a_class ["box" ^ classs]] $4 
				 } 

map:
  OMAP CONTENT EOL map_content  { match $4 with t::q ->   map ~id:("map"^ $2) t q }

map_content: 
  RECT CONTENT OBRACKETR map_content  { match Str.split spaces $2 with 
						img::a::b::c::d::_ -> 
						  area ~alt:"imgmap" ~a:[ a_shape `Rect; 
									  a_href (uri_of_string img); 
									  a_coords ([
										      int_of_string a; 
										      int_of_string b; 
										      int_of_string c; 
										      int_of_string d
										    ])] () :: $4 }

| EOL map_content                      { $2 }
| CMAP                       { [] }

image: 
  IMAGE CONTENT OBRACKETR
     {  match Str.split spaces $2 with 
	    image::id::width::style::legend::_ ->
	      div ~a:[a_class ["illustration"]] 
		[ 
		  img  ~a:[ a_id id; 
			    a_usemap ("#"^id) ; 
			    a_class ["thumb"^style];  
			    a_width  
			      (if width.[String.length width - 1] = '%' then 
				 `Percent 
				   (
				     int_of_string 
				       (
					 String.sub width 0 (String.length width - 1) ))
			       else
				 `Pixels (int_of_string width))
			  ] ~alt:legend ~src:(uri_of_string ("images/"^image)) () 
		] 
     }

wikilink:
    WIKILINK content OBRACKETR
       {  match Str.split spaces $2 with
	      | page::[] ->  
		XHTML.M.a ~a:[a_href (uri_of_string page)]
		  [pcdata page]
		  
	      | page::label ->
		XHTML.M.a ~a:[a_href (uri_of_string page)]
		  [pcdata (String.concat " " label)]
	    
       }

line: 
   content line   { $1 ^ $2 }
  | H1 line       { "=" ^ $2 }
  | SHARP1 line   { "#" ^ $2 }
  | /* Empty */   { "" }
 
httplink:
   HTTPLINK line OBRACKETR
     {  match Str.split spaces $2 with 
	  | uri::[] -> 
	      XHTML.M.a ~a:[a_href (uri_of_string ("http:" ^ uri))]
		[pcdata ("http:" ^ uri)]
	  | uri::label -> 
		  XHTML.M.a ~a:[a_href (uri_of_string ("http:" ^ uri))]
		    [pcdata (String.concat " " label)]
	  
      }


mailto: 
    MAILTO CONTENT OBRACKETR 
{
  match Str.split spaces $2 with 
     mail :: _ ->
       XHTML.M.a ~a:[a_href (uri_of_string ("mailto:" ^ mail))] [pcdata mail]
      
}

style:
   STYLE content OBRACKETR 
        { match Str.split spaces $2 with 
	      position :: content -> 
		span ~a:[a_class [position]] [pcdata (String.concat " " content)]
	    | _ -> span  [pcdata "little failure .."]
	      
	}
list_numbered_grandchild:
  | SHARP3 content EOL list_numbered_grandchild              { li [pcdata $2] :: $4 }
  | /* Empty */                                    { [] }
      
list_numbered_child: 
    SHARP2 content EOL list_numbered_child                    { li [pcdata $2] :: $4 }
  | SHARP2 content EOL list_numbered_grandchild list_numbered_child   { match $4 with 
									   [] -> li [pcdata $2 ] :: $5 
									 | t::q -> li (pcdata $2 :: [ol t q]) :: $5 }
  | /* Empty */                                    { [] }

list_numbered: 
  | SHARP1 textline EOL list_numbered             { li  $2 :: $4 }
  | SHARP1 textline EOL list_numbered_child list_numbered  { match $4 with 
					     [] -> li $2  :: $5 
					   | t::q -> li ($2 @ [ol t q]) :: $5 }
  | EOL                                { [] }
  | /* Empty */                        { [] }


list_grandchild:
  | STAR3 content EOL list_grandchild              { li [pcdata $2] :: $4 }
  | /* Empty */                                    { [] }
list_child: 
    STAR2 content EOL list_child                    { li [pcdata $2] :: $4 }
  | STAR2 content EOL list_grandchild list_child   { match $4 with 
							 [] -> li [pcdata $2 ] :: $5 
						       | t::q -> li (pcdata $2 :: [ul t q]) :: $5 }
  | /* Empty */                                    { [] }

list: 
  | STAR1 textline EOL list             { li $2 :: $4 }
  | STAR1 textline EOL list_child list  { match $4 with 
					     [] -> li $2 :: $5 
					   | t::q -> li ( $2 @ [ul t q]) :: $5 }
  | EOL                                { [] }
  | /* Empty */                        { [] }

textline: 
| content textline                          { pcdata $1 :: $2 }
| wikilink textline                         { $1 :: $2 }
| httplink textline                         { $1 :: $2 }
| mailto textline                          { $1 :: $2 }
| style textline                               { $1 :: $2 }
| H1 content textline                     {  pcdata ("=" ^ $2)  :: $3 }
| BOLD content OBRACKETR textline                { b [ pcdata $2 ]  :: $4 }
| ITALIC content OBRACKETR textline            { i [ pcdata $2 ]  :: $4 }
| /* Empty */                               { [] }

text: 
| content text                          { pcdata $1 :: $2 }
| wikilink text                         { $1 :: $2 }
| httplink text                         { $1 :: $2 }
| style text                            { $1 :: $2 }
| mailto textline                         { $1 :: $2 }
| EOL text                              { br () :: $2 }
| BR text                               { br () :: $2 }
| H1 content textline                     {  pcdata ("=" ^ $2)  :: $3 }
| BOLD content OBRACKETR text                { b [ pcdata $2 ]  :: $4 }
| ITALIC content OBRACKETR text            { i [ pcdata $2 ]  :: $4 }
| EOL                                   { [] }
| /* Empty */                           { [] }

bloc_content: 
  text bloc_content                     { $1 @ $2 }
| h1 bloc_content                         { $1 :: $2 }
| h2 bloc_content                         { $1 :: $2 }
| h3 bloc_content                         { $1 :: $2 } 
| list bloc_content                     { match $1 with t::q -> ul t q :: $2 }
| list_numbered bloc_content            { match $1 with t::q -> ol t q :: $2 } 
| image bloc_content                    { $1 :: $2 } 
| EOL bloc_content                      { $2 }
| CBLOC { [] }

box_content: 
  text box_content                     { $1 @ $2 } 
| h1 box_content                         { $1 :: $2 }
| h2 box_content                         { $1 :: $2 }
| h3 box_content                         { $1 :: $2 }
| list box_content                     { match $1 with t::q -> ul t q :: $2 }
| list_numbered box_content            { match $1 with t::q -> ol t q :: $2 } 
| image box_content                    { $1 :: $2 } 
| EOL box_content                      { $2 }
| CBOX { [] }


table_contents: 
| textline EOL table_contents                           { td $1 :: $3}
| h1 EOL table_contents                         { td [$1] :: $3 }
| h2 EOL table_contents                         { td [$1] :: $3 }
| h3 EOL table_contents                         { td [$1] :: $3 }
| bloc EOL table_contents                                  { td [$1] :: $3 }
| image EOL table_contents                             { td [$1] :: $3 }
| list EOL table_contents                     { match $1 with t::q -> td [ul t q] :: $3 }
| list_numbered EOL  table_contents            { match $1 with t::q -> td [ol t q] :: $3 } 

| CROW                                                 { [] }

table_rows: 
| OROW EOL table_contents EOL table_rows             { match $3 with t::q -> tr t q :: $5 }     
| /* Empty */                                    { [] }


table: 
  TABLE CONTENT EOL table_rows          { match $4 with t::q -> 
					    match Str.split spaces $2 with 
						id::classs::width::_ -> 
						  table ~a:[a_id id; 
							    a_class ["table"^classs]; 
							    a_width 
							      (if width.[String.length width - 1] = '%' then 
								 `Percent 
								   (
								     int_of_string 
								       (
									 String.sub width 0 (String.length width - 1) ))
							       else
								 `Pixels (int_of_string width))
							   ] t q }

ocamlscript: 
   OCAMLSCRIPT CONTENT SUP              { 
     match Str.split spaces $2 with
	 sc::_ -> 
	   [
	     script "text/javascript" (pcdata ("window.onload = function () {exec_caml ('"^ sc ^".exe.uue') ;}")) ;
	     js_script ~uri:(uri_of_string "vm.js") () ;
	   ] }

script_content:
  EOL script_content    { "\r\n" ^ $2 }
| CONTENT script_content { $1 ^ $2 }
| OBRACKETR script_content      { "]" ^ $2 }
| OBRACKETL script_content      { "[" ^ $2 }
| QUOTE script_content          { "'" ^ $2 }
| INF script_content            { " < " ^ $2}
| SUP script_content            { " > " ^ $2}
| H1 script_content          { "=" ^ $2 }
| H2 script_content          { "==" ^ $2 }
| /* Empty */                   { "" }
 
script: 
   OSCRIPT EOL script_content CSCRIPT    { script "text/javascript" (cdata_script $3 )  }

scripturl:
   SCRIPTURL line SUP                    {  match Str.split spaces $2 with 
					      | uri::[] -> 
						  js_script ~uri:(uri_of_string uri) ()  
					 }

type_block:
  h1 type_block                         { $1 :: $2 }
| h2 type_block                         { $1 :: $2 }
| h3 type_block                         { $1 :: $2 }
| bloc type_block                       { $1 :: $2 }
| box type_block                       { $1 :: $2 }
| image type_block                      { $1 :: $2 } 
| map type_block                      {p  [$1] :: $2 } 

| text type_block                       { p $1 :: $2 }
| list type_block                       { match $1 with t::q -> ul t q :: $2 }
| list_numbered type_block              { match $1 with t::q -> ol t q :: $2 }
| table type_block                      { $1 :: $2 }
| ocamlscript type_block                { $1 @ $2 }
| script type_block                      { $1 :: $2 }
| scripturl type_block                      { $1 :: $2 }
| EOF                                   { [] }
| EOL type_block                        { $2 }

| /* Empty */                           { [] }

  

