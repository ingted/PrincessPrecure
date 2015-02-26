module PrincessPrecure.Twinkle

open System
open System.Collections
open Microsoft.FSharp.Reflection

// limitation: This function print "null" if Type.Name equals "Object" and object is () or None.
let rec internal printImpl (t: Type, o: obj) =
  match o with
  | null ->
    if t.Name = "Unit" then "()"
    elif t.FullName.StartsWith("Microsoft.FSharp.Core.FSharpOption`1") then "None"
    else "null"
  | _ ->
    let t = o.GetType()
    if FSharpType.IsRecord t then
      FSharpValue.GetRecordFields(o)
      |> Array.zip (FSharpType.GetRecordFields(t))
      |> Array.map (fun (p, o) -> (p.GetType(), o) |> printImpl |> sprintf "%s = %s" p.Name)
      |> String.concat "; "
      |> sprintf "{ %s }"
    elif t.IsArray then
      let tmp = ResizeArray()
      let t = t.GetElementType()
      for x in o :?> Array do tmp.Add(printImpl (t, x))
      tmp |> String.concat "; " |> sprintf "[|%s|]"
    elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ list> then
      string o
    elif FSharpType.IsUnion t then
      let u, fs = FSharpValue.GetUnionFields(o, t)
      if Array.isEmpty fs then u.Name
      else
        fs
        |> Array.zip (u.GetFields() |> Array.map (fun p -> p.GetType()))
        |> Array.map printImpl
        |> String.concat ", "
        |> sprintf "%s(%s)" u.Name
    elif FSharpType.IsTuple t then
      FSharpValue.GetTupleFields o
      |> Array.zip (FSharpType.GetTupleElements(t))
      |> Array.map printImpl
      |> String.concat ", "
      |> sprintf "(%s)"
    else
      match o with
      | :? bool as o -> sprintf "%b" o
      | :? string as o -> sprintf "\"%s\"" o
      | :? IEnumerable as o ->
        let tmp = ResizeArray()
        let t = t.GetElementType()
        for x in o do tmp.Add(printImpl (t, x))
        tmp |> String.concat "; " |> sprintf "seq [%s]"
      | _ -> string o

let hamming (t: 'T) = printImpl (typeof<'T>, t)
