module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Flags =
    { paypalFormAction : String
    , paypalButtonId : String
    , formsparkAction : String
    }


type alias Model =
    { flags : Flags
    , flavor : Maybe Flavor
    , customLocation : Location
    , selectedLocation : SelectedLocation
    , surprise : Bool
    , name : String
    , email : String
    , space : String
    , instructions : String
    , samples : List String
    }


type Flavor
    = Local
    | Original


type alias Location =
    { city : String, state : String }


type SelectedLocation
    = NoSelection
    | Prefilled Location
    | Custom


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags
      , flavor = Nothing
      , customLocation = Location "" ""
      , selectedLocation = NoSelection
      , surprise = False
      , name = ""
      , email = ""
      , space = ""
      , instructions = ""
      , samples = []
      }
    , Cmd.none
    )


type Msg
    = ToggleSample String
    | SelectFlavor Flavor
    | SelectLocation SelectedLocation
    | SetCustomCity String
    | SetCustomState String
    | SetSurprise Bool
    | SetName String
    | SetEmail String
    | SetSpace String
    | SetInstructions String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ customLocation } as model) =
    case msg of
        ToggleSample sampleName ->
            s { model | samples = toggle sampleName model.samples }

        SelectFlavor flavor ->
            s { model | flavor = Just flavor }

        SelectLocation selection ->
            s { model | selectedLocation = selection }

        SetCustomCity value ->
            s { model | customLocation = { customLocation | city = value } }

        SetCustomState value ->
            s { model | customLocation = { customLocation | state = value } }

        SetSurprise value ->
            s { model | surprise = value }

        SetName value ->
            s { model | name = value }

        SetEmail value ->
            s { model | email = value }

        SetSpace value ->
            s { model | space = value }

        SetInstructions value ->
            s { model | instructions = value }


toggle : a -> List a -> List a
toggle needle haystack =
    if List.member needle haystack then
        List.filter ((/=) needle) haystack

    else
        needle :: haystack


s : a -> ( a, Cmd msg )
s model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [ class "columns" ]
            [ viewFlavor model.flavor
                { icon = "palette"
                , title = [ text "Original" ]
                , description = "Join the global community"
                , selection = Original
                }
            , viewFlavor model.flavor
                { icon = "city"
                , title =
                    [ text "Local"
                    , space
                    , div [ class "tag is-link" ] [ text "New!" ]
                    ]
                , description = "Discover artists in your city"
                , selection = Local
                }
            ]
        , viewFlavorDetails model
        ]


viewFlavor :
    Maybe Flavor
    -> { icon : String, title : List (Html Msg), description : String, selection : Flavor }
    -> Html Msg
viewFlavor selected samples =
    div [ class "column" ]
        [ div
            [ class "notification is-large is-animated-float"
            , animatedTransition
            , style "cursor" "pointer"
            , onClick <| SelectFlavor samples.selection
            , classList
                [ ( "is-link", selected == Just samples.selection )
                , ( "is-light", selected /= Just samples.selection )
                ]
            ]
            [ h2
                [ class "title" ]
                [ span
                    [ class "icon is-large" ]
                    [ i [ class <| "fas fa-" ++ samples.icon ] [] ]
                , span [] samples.title
                ]
            , h3 [ class "subtitle" ] [ text samples.description ]
            ]
        ]


viewFlavorDetails : Model -> Html Msg
viewFlavorDetails model =
    case model.flavor of
        Nothing ->
            headingMessage
                { before = span [ class "icon" ] [ i [ class "fas fa-hand-point-up" ] [] ]
                , message = "Pick with a flavor to get started"
                , after = empty
                }

        Just Local ->
            viewLocal model

        Just Original ->
            viewOriginal model


viewLocal : Model -> Html Msg
viewLocal model =
    Html.form [ action model.flags.formsparkAction, method "POST" ]
        [ input
            [ type_ "hidden"
            , name "_redirect"
            , value "https://applesfordinner.com/thanks"
            ]
            []
        , infoMessage
            [ h3 [ class "title" ] [ text "There is a waitlist" ]
            , p []
                [ text """
                    We're just getting started with our Local flavor,
                    which means we're still looking for great artists across the country.
                    When you choose Local, we let you know when we find artists in your city.
                  """
                , strong [] [ text """
                    Want to start right away? Try our Original flavor!
                  """ ]
                ]
            ]
        , div [ class "field" ] <|
            List.concat
                [ [ label [ class "label" ] [ text "Your city" ] ]
                , List.map (viewPrefilledLocation model.selectedLocation) prefilledLocations
                , [ viewCustomLocation model.selectedLocation model.customLocation ]
                ]
        , input [ type_ "hidden", name "city", cityAttribute model ] []
        , fieldControl { label = "Full name", icon = "fas fa-id-card" }
            [ type_ "text"
            , placeholder "Sam Moore"
            , value model.name
            , onInput SetName
            , name "name"
            ]
        , fieldControl { label = "Email address", icon = "fas fa-at" }
            [ type_ "email"
            , placeholder "sam@example.com"
            , value model.email
            , onInput SetEmail
            , name "email"
            ]
        , viewSpaceInput { name = "space", value = model.space }
        , viewInstructionsInput { name = "instructions", value = model.space }
        , viewSubmitButton
            { icon = "fas fa-envelope"
            , text = "Get in touch"
            , title = "Get in touch"
            }
            [ disabled <| model.selectedLocation == NoSelection ]
        ]


viewPrefilledLocation : SelectedLocation -> Location -> Html Msg
viewPrefilledLocation selected location =
    let
        this =
            Prefilled location
    in
    div [ class "control" ]
        [ button
            [ class "button is-white", onClick <| SelectLocation this ]
            [ checkbox <| this == selected
            , span [] [ text <| locationString location ]
            ]
        ]


viewCustomLocation : SelectedLocation -> Location -> Html Msg
viewCustomLocation selection { city, state } =
    div
        [ class "control"
        , style "display" "flex"
        , style "align-items" "center"
        , style "margin-left" "0.625em"
        , style "cursor" "pointer"
        , onClick <| SelectLocation Custom
        ]
        [ checkbox <| selection == Custom
        , div
            [ class "field has-addons" ]
            [ div
                [ class "control", style "margin-left" "0.35em" ]
                [ textInput [ placeholder "City", value city, onInput SetCustomCity ] ]
            , div
                [ class "control" ]
                [ textInput [ placeholder "State", value state, onInput SetCustomState ] ]
            ]
        ]


viewOriginal : Model -> Html Msg
viewOriginal model =
    div [] <|
        if model.surprise then
            [ headingMessage
                { before = empty
                , message = "Start with a surprise or"
                , after =
                    button
                        [ class "button is-dark is-uppercase"
                        , onClick <| SetSurprise False
                        ]
                        [ text "Pick from our samples" ]
                }
            , infoMessage
                [ text """
                OK! We'll send you some of our favorites then get your feedback.
                """
                ]
            , viewQuiz model [] []
            ]

        else
            [ headingMessage
                { before = empty
                , message = "Pick from our samples or"
                , after =
                    button
                        [ class "button is-dark is-uppercase"
                        , onClick <| SetSurprise True
                        ]
                        [ text "Start with a surprise" ]
                }
            , viewQuiz model
                [ disabled <| List.isEmpty model.samples ]
                (List.map (viewSample model.samples) allSamples)
            ]


viewQuiz : Model -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
viewQuiz model submitAttrs children =
    Html.form [ action model.flags.paypalFormAction ] <|
        input [ type_ "hidden", name "hosted_button_id", value model.flags.paypalButtonId ] []
            :: input [ type_ "hidden", name "cmd", value "_s-xclick" ] []
            :: input [ type_ "hidden", name "charset", value "utf-8" ] []
            :: input [ type_ "hidden", name "on0", value "Space" ] []
            :: input [ type_ "hidden", name "on1", value "Selection" ] []
            :: input [ type_ "hidden", name "on2", value "Instructions" ] []
            :: viewHiddenSelection model.surprise model.samples
            :: children
            ++ [ viewSpaceInput { name = "os0", value = model.space }
               , viewInstructionsInput { name = "os2", value = model.instructions }
               , viewSubmitButton
                    { icon = "fab fa-paypal"
                    , text = "Checkout"
                    , title = "Checkout with PayPal"
                    }
                    submitAttrs
               ]


viewSample : List String -> String -> Html Msg
viewSample samples sampleName =
    let
        checked =
            List.member sampleName samples
    in
    div
        [ for sampleName
        , style "cursor" "pointer"
        , onClick <| ToggleSample sampleName
        ]
        [ div
            [ class "card"
            , style "margin-bottom" "2em"
            , animatedTransition
            , classList
                [ ( "is-animated-shrink", checked )
                , ( "has-background-light", checked )
                ]
            ]
            [ div
                [ class "card-content" ]
                [ div
                    [ style "width" "100%"
                    , style "white-space" "nowrap"
                    , style "overflow-x" "scroll"
                    , style "overflow-y" "hidden"
                    , style "cursor" "pointer"
                    ]
                  <|
                    List.intersperse space <|
                        List.map (viewSampleImage sampleName) (List.range 1 4)
                ]
            , div
                [ class "card-footer" ]
                [ div
                    [ class "card-footer-item" ]
                    [ p [ class "heading" ] [ checkbox checked, space, text "I like these!" ]
                    ]
                ]
            ]
        ]


viewSampleImage : String -> Int -> Html Msg
viewSampleImage sampleName index =
    img
        [ style "display" "inline-block"
        , style "height" "100%"
        , style "max-height" "15em"
        , alt <| sampleName ++ "picture #" ++ String.fromInt index
        , src <|
            "/images/profile-option/"
                ++ sampleName
                ++ String.fromInt index
                ++ ".jpg"
        ]
        []


viewSpaceInput : { name : String, value : String } -> Html Msg
viewSpaceInput options =
    fieldControl
        { label = "What kind of space are you styling?"
        , icon = "fas fa-door-open"
        }
        [ type_ "text"
        , name options.name
        , placeholder "Living room"
        , value options.value
        , onInput SetSpace
        ]


viewInstructionsInput : { name : String, value : String } -> Html Msg
viewInstructionsInput options =
    fieldControl
        { label = "Anything else we should know?"
        , icon = "fas fa-question"
        }
        [ name options.name
        , type_ "text"
        , placeholder "I prefer bright colors and faded backgrounds"
        , value options.value
        , onInput SetInstructions
        , required False
        ]


viewHiddenSelection : Bool -> List String -> Html msg
viewHiddenSelection surprise samples =
    let
        attr =
            if surprise then
                value "Surprise"

            else
                value <| "Samples " ++ String.join ", " samples
    in
    input [ type_ "hidden", name "os1", attr ] []


viewSubmitButton : { icon : String, text : String, title : String } -> List (Attribute msg) -> Html msg
viewSubmitButton options attrs =
    div [ class "field" ]
        [ p [ class "content" ]
            [ text <| "By clicking " ++ options.text ++ ", you agree to our "
            , a [ href "/terms" ] [ text "terms of service" ]
            , text "."
            ]
        , div
            [ class "control" ]
            [ button
                (class "button is-link is-large"
                    :: type_ "submit"
                    :: title options.title
                    :: attrs
                )
                [ span [ class "icon is-large" ] [ i [ class options.icon ] [] ]
                , span [] [ text options.text ]
                ]
            ]
        ]


fieldControl : { label : String, icon : String } -> List (Attribute msg) -> Html msg
fieldControl options attrs =
    div [ class "field" ]
        [ label [ class "label" ] [ text options.label ]
        , div
            [ class "control has-icons-left" ]
            [ input (class "input" :: required True :: attrs) []
            , span [ class "icon is-small is-left" ] [ i [ class options.icon ] [] ]
            ]
        ]


headingMessage : { before : Html msg, message : String, after : Html msg } -> Html msg
headingMessage { before, message, after } =
    div
        [ class "is-flex-desktop"
        , style "align-items" "baseline"
        , style "margin" "2em 0 2em"
        ]
        [ before
        , h1 [ class "heading is-size-6", style "margin" "0 0.35em" ] [ text message ]
        , after
        ]


infoMessage : List (Html msg) -> Html msg
infoMessage children =
    div [ class "message is-info" ] [ div [ class "message-body" ] children ]


textInput : List (Attribute msg) -> Html msg
textInput attrs =
    input (class "input" :: type_ "text" :: attrs) []


checkbox : Bool -> Html msg
checkbox isChecked =
    let
        attrs =
            if isChecked then
                [ class "fas fa-check-square"
                , attribute "aria-checked" "true"
                , attribute "role" "checkbox"
                ]

            else
                [ class "far fa-square"
                , attribute "aria-checked" "false"
                , attribute "role" "checkbox"
                ]
    in
    span [ class "icon is-small is-left" ] [ i attrs [] ]


space : Html msg
space =
    text " "


empty : Html msg
empty =
    text ""


animatedTransition : Attribute msg
animatedTransition =
    style "transition" "all 75ms ease-in-out"


cityAttribute : Model -> Attribute msg
cityAttribute { customLocation, selectedLocation } =
    case selectedLocation of
        NoSelection ->
            value ""

        Prefilled location ->
            value <| locationString location

        Custom ->
            value <| locationString customLocation


locationString : Location -> String
locationString { city, state } =
    city ++ ", " ++ state


prefilledLocations : List Location
prefilledLocations =
    [ { city = "New York", state = "NY" }
    , { city = "Los Angeles", state = "CA" }
    , { city = "Chicago", state = "IL" }
    , { city = "Dallas", state = "TX" }
    , { city = "San Francisco", state = "CA" }
    ]


allSamples : List String
allSamples =
    [ "animals"
    , "architecture"
    , "city"
    , "food"
    , "outdoors"
    , "technology"
    , "words"
    ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
