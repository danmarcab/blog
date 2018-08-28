module Main exposing (main)

import Browser
import Element exposing (Color, Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input


type Model
    = Reading { input : String, pendingOperation : Maybe PendingOperation }
    | Executed { result : Float, pendingOperation : Maybe PendingOperation }
    | Error


type alias PendingOperation =
    ( Float, Op )


init : Model
init =
    Reading { input = "0", pendingOperation = Nothing }


type Msg
    = Number Int
    | Operation Op
    | Equals
    | Dot


type Op
    = Division
    | Multiplication
    | Substraction
    | Addition


update : Msg -> Model -> Model
update msg model =
    case model of
        Reading reading ->
            case msg of
                Number num ->
                    Reading { reading | input = addToInput (String.fromInt num) reading.input }

                Dot ->
                    Reading { reading | input = addToInput "." reading.input }

                Operation op ->
                    String.toFloat reading.input
                        |> Maybe.map
                            (\num ->
                                case reading.pendingOperation of
                                    Nothing ->
                                        Executed { result = num, pendingOperation = Just ( num, op ) }

                                    Just ( pendingNum, operation ) ->
                                        let
                                            result =
                                                applyOperation operation pendingNum num
                                        in
                                        Executed { result = result, pendingOperation = Just ( result, op ) }
                            )
                        |> Maybe.withDefault Error

                Equals ->
                    case reading.pendingOperation of
                        Nothing ->
                            Reading reading

                        Just ( pendingNum, operation ) ->
                            String.toFloat reading.input
                                |> Maybe.map
                                    (\num ->
                                        let
                                            result =
                                                applyOperation operation pendingNum num
                                        in
                                        Executed { result = result, pendingOperation = Nothing }
                                    )
                                |> Maybe.withDefault Error

        Executed executed ->
            case msg of
                Number num ->
                    Reading { input = addToInput (String.fromInt num) "", pendingOperation = executed.pendingOperation }

                Dot ->
                    Reading { input = addToInput "." "", pendingOperation = executed.pendingOperation }

                Operation op ->
                    case executed.pendingOperation of
                        Nothing ->
                            Executed { executed | pendingOperation = Just ( executed.result, op ) }

                        Just ( result, oldOp ) ->
                            Executed { executed | pendingOperation = Just ( result, op ) }

                Equals ->
                    Executed executed

        Error ->
            Error


addToInput : String -> String -> String
addToInput toAdd input =
    input ++ toAdd


applyOperation : Op -> Float -> Float -> Float
applyOperation op num1 num2 =
    case op of
        Division ->
            num1 / num2

        Multiplication ->
            num1 * num2

        Substraction ->
            num1 - num2

        Addition ->
            num1 + num2


view : Model -> Element Msg
view model =
    Element.column
        [ Element.padding 20
        , Element.spacing 20
        , Background.color lightBlue
        , Font.size 30
        , Font.alignRight
        , Element.height (Element.px 500)
        ]
        [ --        Element.text <| Debug.toString model
          screenView model
        , keyPadView model
        ]


screenView : Model -> Element Msg
screenView model =
    Element.el
        [ Background.color white
        , Element.padding 20
        , Element.width Element.fill
        ]
    <|
        Element.text <|
            case model of
                Reading { input } ->
                    input

                Executed { result } ->
                    String.fromFloat result

                Error ->
                    "Error"


keyPadView : Model -> Element Msg
keyPadView model =
    let
        buttonRow msgs =
            Element.row [ Element.spacing 10 ] (List.map buttonView msgs)
    in
    Element.column [ Element.spacing 10 ]
        [ buttonRow
            [ Number 7
            , Number 8
            , Number 9
            , Operation Division
            ]
        , buttonRow
            [ Number 4
            , Number 5
            , Number 6
            , Operation Multiplication
            ]
        , buttonRow
            [ Number 1
            , Number 2
            , Number 3
            , Operation Substraction
            ]
        , buttonRow
            [ Dot
            , Number 0
            , Equals
            , Operation Addition
            ]
        ]


buttonView : Msg -> Element Msg
buttonView msg =
    let
        label =
            case msg of
                Number num ->
                    String.fromInt num

                Operation Division ->
                    "÷"

                Operation Multiplication ->
                    "×"

                Operation Addition ->
                    "+"

                Operation Substraction ->
                    "-"

                Equals ->
                    "="

                Dot ->
                    "."
    in
    Input.button
        [ Background.color blue
        , Font.color white
        , Font.size 30
        , Element.width (Element.px 80)
        , Element.height (Element.px 80)
        ]
        { label =
            Element.el [ Element.centerX ]
                (Element.text label)
        , onPress = Just msg
        }


lightBlue : Color
lightBlue =
    Element.rgb 0.455 0.725 1


blue : Color
blue =
    Element.rgb 0.035 0.518 0.89


black : Color
black =
    Element.rgb 0 0 0


white : Color
white =
    Element.rgb 1 1 1


main : Program () Model Msg
main =
    Browser.sandbox
        { update = update
        , init = init
        , view = Element.layout [] << view
        }
