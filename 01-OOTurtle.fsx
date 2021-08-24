#load "Common.fsx"

open System
open Common

#load "OOTurtleLib.fsx"

module OOTurtleClient =

    open OOTurtleLib

    let log message = 
        printfn "%s" message

    let drawTriangle() = 
        let turtle = Turtle(log)
        turtle.Move 100.0
        turtle.Turn 120.0<Degrees>
        turtle.Move 100.0
        turtle.Turn 120.0<Degrees>
        turtle.Move 100.0
        turtle.Turn 120.0<Degrees>

    let drawPolygon n =
        let turtle = Turtle(log)
        let angle = 180.0 - (360.0/float n)
        let angleDegrees = angle * 1.0<Degrees>

        // define a function that draws one side
        let drawOneSide() =
            turtle.Move 100.0
            turtle.Turn angleDegrees

        // repeate for all sides
        for i in [1..n] do
            drawOneSide()

//OOTurtleClient.drawTriangle()
OOTurtleClient.drawPolygon 8