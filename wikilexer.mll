(*
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
 *  wikilexer.mll ~wlf  23/07/2009 10:56
 *
 *)
 
  {
    open Wikiparser        (* The type token is defined in parser.mli *)
    exception Eof
  }
    
rule token = 
     parse
	 [' ' '\t']     { token lexbuf }     (* skip blanks *)
       | "\r\n"         { EOL }
       | ['\n' ]        { EOL }
       | "==="        { H3 }
       | "=="         { H2 }
       | "="          { H1 } 
       | ['*']        { STAR1 }
       | "**"         { STAR2 }
       | "***"        { STAR3 }
       | "#"          { SHARP1 }
       | "##"          { SHARP2 }
       | "###"          { SHARP3 }
	   
       | "<center>"     { OCENTER }
       | "</center>"    { ECENTER }

       | "[''':"          { BOLD }
       | "['':"           { ITALIC }

       | "]"            { OBRACKETR }
       | "["            { OBRACKETL }

       | "'"            { QUOTE }

       | "[wiki:"       { WIKILINK }
       | "[http:"       { HTTPLINK }
       | "[mailto:"     { MAILTO }

       | "[style:"      { STYLE }

       | "[image:"      { IMAGE }
       | "[table:"      { TABLE }
       | "[map:"        { OMAP }
       | "map]"         { CMAP } 
       | "[rect"        { RECT }
      
       | "<script>"     { OSCRIPT }
       | "</script>"    { CSCRIPT }
	  
       | "<script ="    { SCRIPTURL }
       | "<bloc"        { OBLOC }
       | ">"            { SUP }
 
       | "</bloc>"      { CBLOC }

       | "<box"        { OBOX }

       | "</box>"      { CBOX }
       | "< "         { INF }
	   
       | "<br />"      { BR } 
       | eof           { EOF }

       | "[!"          { OROW }
       | "!]"          { CROW }
       
       |  "<ocamlscript " { OCAMLSCRIPT }
       | [';' 'a' - 'z' ' ' '0' - '9' '%' 'A' - 'Z' '@' '!' '\"'  '?' '.' '&' '_' '/' ':' '-' '\'' ',' '(' ')' '+' '}' '{' ]+ as content { CONTENT (content) } 
       | "è"           { EGRAVE }
       | "é"           { EAIGU }
       | "ê"           { ECIR }
       | "û"           { UCIR }
       | "à"           { AGRA }
       | "ù"           { UGRA }
       | "ç"           { CCED }
       | "ô"           { OCIR }
