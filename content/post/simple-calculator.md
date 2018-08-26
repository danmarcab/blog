---
title: "Simple Calculator"
date: 2018-08-26T19:20:27+01:00
draft: false
---

{{< highlight elm >}}
type Model
    = Reading { input : String, pendingOperation : Maybe PendingOperation }
    | Executed { result : Float, pendingOperation : Maybe PendingOperation }
    | Error


type alias PendingOperation =
    ( Float, Op )


init : Model
init =
    Reading { input = "0", pendingOperation = Nothing }

{{</ highlight >}}

{{< elm-element "simple-calculator">}}
