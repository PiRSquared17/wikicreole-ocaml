(*
 * Db connector 
 * 
 *)

open Misc

open Aws_shared.Types

let load connection domain page = 
  Aws_simpledb.Command.GetAttributes.exec connection domain page  >>> Aws_shared.Result.get_attribute "contents"

let save connection domain page = 
  [ { name = "contents"; value = page } ] >>> Aws_simpledb.Command.PutAttributes.exec ~replace:true connection domain page
