#load "Common.fsx"
#load "FPTurtleLib.fsx"

open Common
open FPTurtleLib

type TurtleStateComputation<'a> =
    TurtleStateComputation of (Turtle.TurtleState -> 'a * Turtle.TurtleState)

module TurtleStateComputation =

    let runT turtle state =
        // pattern match against the turtle to extract the innner function
        let (TurtleStateComputation innerFn) = turtle
        // run the inner function with the passed state
        innerFn state

    let returnT x =
        let innerFn state =
            (x,state)
        TurtleStateComputation innerFn

    let bindT f xT =
        let innerFn state =
            let x,state2 = runT xT state
            runT (f x) state2
        TurtleStateComputation innerFn

    let mapT f =
        bindT (f >> returnT)

    let toComputation f =
        let innerFn state =
            let (result,newState) = f state
            (result,newState)
        TurtleStateComputation innerFn
    
    let toUnitComputation f =
        let f2 state =
            (),f state  
        toComputation f2

    // define a computation expression builder
    type TurtleBuilder() =
        member this.Return(x) = returnT x
        member this.Bind(x,f) = bindT f x

    // create an instance of the computation expression builder
    let turtle = TurtleBuilder()
    
module TurtleComputationClient =

    open TurtleStateComputation
    open Result

    // Function to log a message
    let log message =
        printfn "%s" message

    let initialTurtleState =
        Turtle.initialTurtleState

    let move dist =
        toUnitComputation (Turtle.move log dist)

    let turn angle =
        toUnitComputation (Turtle.turn log angle)

    let penDown =
        toUnitComputation (Turtle.penDown log)

    let penUp =
        toUnitComputation (Turtle.penUp log)

    let setColor color =
        toUnitComputation (Turtle.setColor log color)

    let drawTriangle() =
        let t = turtle {
            do! move 100.0
            do! turn 120.0<Degrees>
            do! move 100.0
            do! turn 120.0<Degrees>
            do! move 100.0
            do! turn 120.0<Degrees>
            }

        runT t initialTurtleState

    let drawPolygon n =
        let angle = 180.0 - (360.0/float n)
        let angleDegrees = angle * 1.0<Degrees>

        // define a function that draws one side
        let oneSide = turtle {
            do! move 100.0
            do! turn angleDegrees
            }

        // change two turtle operations in sequence
        let chain f g = turtle {
            do! f
            do! g
            }

        // create a list of operations, one for each side
        let sides = List.replicate n oneSide

        // chain all the sides into one operations
        let all = sides |> List.reduce chain

        runT all initialTurtleState

TurtleComputationClient.drawTriangle()
TurtleComputationClient.drawPolygon 5