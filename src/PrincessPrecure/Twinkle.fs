module PrincessPrecure.Twinkle

open System
open System.Collections
open System.Reflection
open Microsoft.FSharp.Reflection
open Attoparsec
open Attoparsec.String

let private (|StructuredFormatDisplay|_|) (o: obj) =

  let t = o.GetType()

  let str =
    takeWhile1 (not << Helper.inClass "{}")
    |> map BmpString.toString

  let callProperty =
    pstring "{" >>. str .>> pstring "}"
    |> map (fun x -> t.GetMethod("get_" + x).Invoke(o, [||]) :?> string)

  let display = many1 (callProperty <|> str) |> map (String.concat "")

  let parse s =
    if s = "" then Some ""
    else parse display s |> ParseResult.feed "" |> ParseResult.option
  
  t.GetCustomAttributes()
  |> Seq.tryFind (fun attr -> attr.GetType() = typeof<StructuredFormatDisplayAttribute>)
  |> Option.bind (fun x -> (x :?> StructuredFormatDisplayAttribute).Value |> parse)

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
      | :? char as o -> sprintf "'%c'" o
      | :? float32 as o ->
        if Single.IsNaN(o) then string o
        else o.ToString()
      | :? float as o ->
        if Double.IsNaN(o) then string o
        else o.ToString()
      | :? string as o -> sprintf "\"%s\"" o
      | :? IEnumerable as o ->
        let tmp = ResizeArray()
        let t = t.GetElementType()
        for x in o do tmp.Add(printImpl (t, x))
        tmp |> String.concat "; " |> sprintf "seq [%s]"
      | StructuredFormatDisplay s -> s
      | _ -> string o

let hamming (t: 'T) = printImpl (typeof<'T>, t)
