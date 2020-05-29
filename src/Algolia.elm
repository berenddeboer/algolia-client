module Algolia exposing
    ( Config
    , fetch
    , Response
    )


{-| Helper functions for Algolia clients.


# Helpers

@docs Config, fetch, Msg

-}

import Algolia.Api as Api exposing (Method, formRqst)
import Http exposing (request, expectJson, Header, Error(..))
import RemoteData exposing (WebData)
import Json.Decode exposing (succeed, list, bool, int, string)
import Json.Decode.Pipeline exposing (required)



{-| Config to pass to `fetch`.
-}
type alias Config =
    { apiKey : String
    , appId : String
    , api : Method
    , extraHeaders : List ( String, String )
    }


{-| Fetch data from Algolia.

Pass it the decoder for your search specific results. For example our index has actors:

    type alias Actor =
        { name : String
        , rating : Int
        , image_path : String
        , alternative_name : Maybe String
        , objectID : String
        , highlightResult : Actor_highlightResult
        }

Write the decoder:

    import Json.Decode exposing (succeed, list, bool, int, string)
    import Json.Decode.Pipeline exposing (required)

    decodeActor : Json.Decode.Decoder Actor
    decodeActor =
        succeed Actor
            |> required "name" string
            |> required "rating" int
            |> required "image_path" string
            |> required "alternative_name" (Json.Decode.nullable string)
            |> required "objectID" string
            |> required "_highlightResult" decodeActor_highlightResult

Then define a response with your specific result as parameter:

    import RemoteData exposing (RemoteData(..), WebData)

    type alias Response = Algolia.Response Actor

    type Msg
        = AlgoliaResponse (WebData Response)

    type alias Model = WebData Response

And in your update function call `fetch` when some data has been typed in:

    import Algolia

    Algolia.fetch (config "search text") AlgoliaResponse decodeActor

The config function returns the authentication data and what index to search:

    config : String -> Algolia.Config
    config str =
        { apiKey = "6be0576ff61c053d5f9a3225e2a90f76"
        , appId = "latency"
        , api = SearchAnIndex (searchOptions str)
        , extraHeaders = []
        }

In your update function you can then handle the AlgoliaResponse msg as
usual for RemoteData:

        AlgoliaResults response ->
            ( { model | algoliaResponse = response }, Cmd.none )


-}
fetch : Config -> (WebData (Response hit) -> msg) -> Json.Decode.Decoder hit -> Cmd msg
fetch config msg decodeHit =
    let
        rqstRec =
            formRqst config

        rqst =
            { method = rqstRec.httpMethod
            , headers = formatHeaders config
            , url = rqstRec.url
            , body = rqstRec.body
            , expect = expectJson (RemoteData.fromResult >> msg) (decodeResponse decodeHit)
            , timeout = Nothing
            , tracker = Nothing
            }
    in
    Http.request rqst


formatHeaders : Config -> List Header
formatHeaders config =
    let
        extraHeaders =
            List.map (\( k, v ) -> Http.header k v) config.extraHeaders

        headers =
            [ Http.header "X-Algolia-Application-Id" config.appId
            , Http.header "X-Algolia-API-Key" config.apiKey
            ]
    in
    headers ++ extraHeaders


type alias Response hit =
    { hits : List hit
    , nbHits : Int
    , page : Int
    , nbPages : Int
    , hitsPerPage : Int
    , processingTimeMS : Int
    , exhaustiveNbHits : Bool
    , query : String
    , params : String
    }


decodeResponse : Json.Decode.Decoder hit -> Json.Decode.Decoder (Response hit)
decodeResponse decodeHit =
    succeed Response
        |> required "hits" (list decodeHit)
        |> required "nbHits" int
        |> required "page" int
        |> required "nbPages" int
        |> required "hitsPerPage" int
        |> required "processingTimeMS" int
        |> required "exhaustiveNbHits" bool
        |> required "query" string
        |> required "params" string
