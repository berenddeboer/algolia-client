module SimpleExample exposing (..)

import Algolia exposing (..)
import Algolia.Api as Api exposing (..)
import Html as Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Decode exposing (field)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode exposing (encode, string)

main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { searchString : String
    , algoliaResponseString : String
    , algoliaResponse : AlgoliaResponse
    }


type Msg
    = NoOp
    | InputString String
    | AlgoliaMsg Algolia.Msg


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


init : ( Model, Cmd msg )
init =
    ( { searchString = ""
      , algoliaResponseString = ""
      , algoliaResponse = initAlgoliaResponse
      }
    , Cmd.none
    )


minCharsToStartSearch : Int
minCharsToStartSearch =
    3

{-- View
--}
view : Model -> Html Msg
view model =
    div []
        [ section [ class "hero is-info" ]
            [ div [ class "hero-body" ]
                [ h1 [ class "title" ]
                    [ text "Algolia instasearch with Elm" ]
                ]
            ]
        , section [ class "section" ]
            [ div [ class "columns" ]
                [ div [ class "container column is-half" ]
                    [ label [ class "label" ]
                        [ text "Search for actors e.g 'robert', 'robn' (intentional typo), 'julia', 'ben'"]
                    , input [ class "input", onInput InputString ] []
                    , displayResults model.algoliaResponse
                    ]
                , div [ class "container column is-half" ]
                    [ label [ class "label" ] [ text "Raw Json response" ]
                    , textarea [ class "textarea", disabled True, readonly True, rows 30 ]
                        [ prettifyJsonResponse model.algoliaResponseString ]
                    ]
                ]
            ]
        ]

{-- Update
--}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputString str ->
            if String.length str >= minCharsToStartSearch then
                let
                    subCmd =
                        Algolia.fetch (config str)
                in
                ( model, Cmd.map AlgoliaMsg subCmd )
            else
                ( { model | algoliaResponse = initAlgoliaResponse }, Cmd.none )

        AlgoliaMsg subMsg ->
            let
                ( algoliaResponse, algoliaResponseString ) =
                    case Algolia.getSearchResults subMsg of
                        AllGood str ->
                            case Json.Decode.decodeString decodeAlgoliaResponse str of
                                Ok a ->
                                    ( a, str )

                                Err err ->
                                    ( initAlgoliaResponse, err )

                        FatalError str ->
                            ( initAlgoliaResponse, str )
            in
            ( { model
                | algoliaResponse = algoliaResponse
                , algoliaResponseString = algoliaResponseString
              }
            , Cmd.none
            )


{-- Config and query options for Algolia
--}
config : String -> Algolia.Config
config str =
    { apiKey = "d3cff3e276e384e86347c50d1ff7b8f3"
    , appId = "VTU1B49X2Y"
    , api = SearchAnIndex (searchOptions str)
    , extraHeaders = []
    }


searchOptions : String -> Api.SearchRecord
searchOptions str =
    { indexName = "getstarted_actors"
    , params =
        Just
            [ TypoTolerance TypoToleranceMin
            , HitsPerPage 10
            , Query str
            ]
    }


{-- View helpers for the search results
--}

displayResults : AlgoliaResponse -> Html Msg
displayResults a =
    ul [ class "section" ]
        (List.map (displayActor a.query) a.hits)


displayActor : String -> Actor -> Html Msg
displayActor query actor =
    li [ class "container is-fluid" ]
        [ div [ class "box columns" ]
            [ div [ class "column" ]
                [ img [ src ("https://image.tmdb.org/t/p/w154" ++ actor.image_path) ]
                    []
                ]
            , div [ class "column" ]
                (highlightedName actor.highlightResult)
            ]
        , br [] []
        ]


highlightedName : Actor_highlightResult -> List (Html Msg)
highlightedName result =
    let
        nameHtml =
            case result.name of
                Just struct ->
                    formatHighlight struct.value

                Nothing ->
                    []

        altNameHtml =
            case result.alternative_name of
                Just struct ->
                    formatHighlight struct.value

                Nothing ->
                    []
    in
    [ p [ class "subtitle" ] nameHtml
    , p [] altNameHtml
    ]


formatHighlight : String -> List (Html Msg)
formatHighlight str =
    let
        pre =
            String.split "<em>" str |> List.head |> Maybe.withDefault ""

        post =
            String.split "</em>" str |> List.reverse |> List.head |> Maybe.withDefault ""

        highlight =
            String.split "<em>" str
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ""
                |> String.split "</em>"
                |> List.head
                |> Maybe.withDefault ""
    in
    [ span [] [ Html.text pre ]
    , span [ style [("background-color", "#3273DC"), ("color", "white")] ] [ Html.text highlight ]
    , span [] [ Html.text post ]
    ]

prettifyJsonResponse : String -> Html Msg
prettifyJsonResponse str =
    str
        |> Encode.string
        |> Encode.encode 4
        |> Html.text

{-- Now start the chunk of decoder code.
--}

type alias AlgoliaResponse =
    { hits : List Actor
    , nbHits : Int
    , page : Int
    , nbPages : Int
    , hitsPerPage : Int
    , processingTimeMS : Int
    , exhaustiveNbHits : Bool
    , query : String
    , params : String
    }

initAlgoliaResponse : AlgoliaResponse
initAlgoliaResponse =
    { hits = []
    , nbHits = 0
    , page = 0
    , nbPages = 0
    , hitsPerPage = 0
    , processingTimeMS = 0
    , exhaustiveNbHits = False
    , query = ""
    , params = ""
    }



type alias Actor =
    { name : String
    , rating : Int
    , image_path : String
    , alternative_name : Maybe String
    , objectID : String
    , highlightResult : Actor_highlightResult
    }


type alias HighlightResult =
    { value : String
    , matchLevel : String
    , fullyHighlighted : Maybe Bool
    , matchedWords : Maybe (List String)
    }


type alias Actor_highlightResult =
    { name : Maybe HighlightResult
    , alternative_name : Maybe HighlightResult
    }



decodeAlgoliaResponse : Json.Decode.Decoder AlgoliaResponse
decodeAlgoliaResponse =
    Json.Decode.Pipeline.decode AlgoliaResponse
        |> Json.Decode.Pipeline.required "hits" (Json.Decode.list decodeActor)
        |> Json.Decode.Pipeline.required "nbHits" Json.Decode.int
        |> Json.Decode.Pipeline.required "page" Json.Decode.int
        |> Json.Decode.Pipeline.required "nbPages" Json.Decode.int
        |> Json.Decode.Pipeline.required "hitsPerPage" Json.Decode.int
        |> Json.Decode.Pipeline.required "processingTimeMS" Json.Decode.int
        |> Json.Decode.Pipeline.required "exhaustiveNbHits" Json.Decode.bool
        |> Json.Decode.Pipeline.required "query" Json.Decode.string
        |> Json.Decode.Pipeline.required "params" Json.Decode.string


decodeActor : Json.Decode.Decoder Actor
decodeActor =
    Json.Decode.Pipeline.decode Actor
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "rating" Json.Decode.int
        |> Json.Decode.Pipeline.required "image_path" Json.Decode.string
        |> Json.Decode.Pipeline.required "alternative_name" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "objectID" Json.Decode.string
        |> Json.Decode.Pipeline.required "_highlightResult" decodeActor_highlightResult


decode_highlightResult : Json.Decode.Decoder HighlightResult
decode_highlightResult =
    Json.Decode.Pipeline.decode HighlightResult
        |> Json.Decode.Pipeline.required "value" Json.Decode.string
        |> Json.Decode.Pipeline.required "matchLevel" Json.Decode.string
        |> Json.Decode.Pipeline.optional "fullyHighlighted" (Json.Decode.nullable Json.Decode.bool) Nothing
        |> Json.Decode.Pipeline.optional "matchedWords" (Json.Decode.nullable (Json.Decode.list Json.Decode.string)) Nothing


decodeActor_highlightResult : Json.Decode.Decoder Actor_highlightResult
decodeActor_highlightResult =
    Json.Decode.Pipeline.decode Actor_highlightResult
        |> Json.Decode.Pipeline.required "name" (Json.Decode.nullable decode_highlightResult)
        |> Json.Decode.Pipeline.optional "alternative_name" (Json.Decode.nullable decode_highlightResult) (Just defaultHighlightResult)


defaultHighlightResult : HighlightResult
defaultHighlightResult =
    { value = ""
    , matchLevel = "none"
    , fullyHighlighted = Nothing
    , matchedWords = Nothing
    }
