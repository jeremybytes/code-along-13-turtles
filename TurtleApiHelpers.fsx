//#load "Common.fsx"

open System
open Common

open Result

type ErrorMessage =
    | InvalidDistance of string
    | InvalidAngle of string
    | InvalidColor of string
    | InvalidCommand of string

let validateDistance distanceStr =
    try
        Ok (float distanceStr)
    with
    | ex ->
        Error (InvalidDistance distanceStr)

let validateAngle angleStr =
    try
        Ok ((float angleStr) * 1.0<Degrees>)
    with
    | ex ->
        Error (InvalidAngle angleStr)

// convert the color parameter to PenColor, or throw an exception
let validateColor colorStr =
    match colorStr with
    | "Black" -> Ok Black
    | "Blue" -> Ok Blue
    | "Red" -> Ok Red
    | _ ->
        Error (InvalidColor colorStr)
