//#load "Common.fsx"

open System
open Common

module Turtle =
    type TurtleState = {
        position : Position
        angle : float<Degrees>
        color : PenColor
        penState : PenState
    }

    let initialTurtleState = {
        position = initialPosition
        angle = 0.0<Degrees>
        color = initialColor
        penState = initialPenState
    }

    let move log distance state =
        log (sprintf "Move %0.1f" distance)
        // calculate new position
        
        let newPosition = calcNewPosition distance state.angle state.position
        // draw line if needed
        if state.penState = Down then
            dummyDrawLine log state.position newPosition state.color
        // update the state
        {state with position = newPosition}

    let turn log angle state =
        log (sprintf "Turn %0.1f" angle)
        // calculate new angle
        let newAngle = (state.angle + angle) % 360.0<Degrees>
        // update the state
        {state with angle = newAngle}

    let penUp log state =
        log "Pen up"
        {state with penState = Up}

    let penDown log state =
        log "Pen down"
        {state with penState = Down}

    let setColor log color state =
        log (sprintf "SetColor %A" color)
        {state with color = color}