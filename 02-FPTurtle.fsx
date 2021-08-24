#load "Common.fsx"
open Common

#load "FPTurtleLib.fsx"
open FPTurtleLib

module FPTurtleClient = 

    let log message =
        printfn "%s" message

    let move = Turtle.move log
    let turn = Turtle.turn log
    let penDown = Turtle.penDown log
    let penUp = Turtle.penUp log
    let setColor = Turtle.setColor log

    let drawTriangle() =
        Turtle.initialTurtleState
        |> move 100.0
        |> turn 120.0<Degrees>
        |> move 100.0
        |> turn 120.0<Degrees>
        |> move 100.0
        |> turn 120.0<Degrees>

    let drawPolygon n =
        let angle = 180.0 - (360.0/float n)
        let angleDegrees = angle * 1.0<Degrees>

        let oneSide state sideNumber =
            state
            |> move 100.0
            |> turn angleDegrees

        // repeat for all sides
        [1..n]
        |> List.fold oneSide Turtle.initialTurtleState

FPTurtleClient.drawTriangle() |> ignore
FPTurtleClient.drawPolygon 5 |> ignore