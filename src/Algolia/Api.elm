--Version 1
module Algolia.Api
    exposing
        ( AroundRadiusValue(..)
        , ExactOnSingleWordQueryValue(..)
        , FacetsValue(..)
        , IgnorePluralsValue(..)
        , IndexOperation
        , Method(..)
        , QueryTypeValue(..)
        , RemoveStopWordsValue(..)
        , RemoveWordsIfNoResultsValue(..)
        , SearchParam(..)
        , SearchRecord
        , Strategy(..)
        , TypoToleranceValue(..)
        , formRqst
        )

{-|

Helper functions for Algolia clients.

# Search parameters

@docs   AroundRadiusValue, ExactOnSingleWordQueryValue, FacetsValue , IgnorePluralsValue
        , IndexOperation, QueryTypeValue, RemoveStopWordsValue , RemoveWordsIfNoResultsValue
        , Strategy , TypoToleranceValue

# Api methods and configuration helpers

@docs  Method, SearchParam, SearchRecord, formRqst

-}

import Http exposing (Header, emptyBody, header, jsonBody)
import Json.Encode as Encode exposing (Value, bool, encode, float, int, list, object, string)


{-| SearchParam
-}
type SearchParam
    = Query String --The text to search for in the index
    | AttributesToRetrieve (List String) -- List of object attributes you want to retrieve.
    | RestrictSearchableAttributes (List String) -- List of attributes to be considered for textual search.
    | Filters String -- Filter the query with numeric, facet and/or tag filters.
    | Facets FacetsValue -- Facets to retrieve.
    | MaxValuesPerFacet Int -- Maximum number of facet values returned for each facet.
    | FacetFilters (List String) -- Filter hits by facet value.
    | FacetingAfterDistinct Bool -- Force faceting to be applied after de-duplication.
    | OptionalFilters (List String) -- Create filters for ranking purposes, to rank higher records that contain the filter(s)
    | SortFacetValuesBy String -- Controls how facet values are sorted.
    | SumOrFiltersScores Bool -- Determines how to calculate the total score for filtering
    | AttributesToHighlight (List String) -- List of attributes to highlight.
    | AttributesToSnippet (List String) -- List of attributes to snippet| with an optional maximum number of words to snippet.
    | HighlightPreTag String -- String inserted before highlighted parts in highlight and snippet results.
    | HighlightPostTag String -- String inserted after highlighted parts in highlight and snippet results.
    | SnippetEllipsisText String -- String used as an ellipsis indicator when a snippet is truncated.
    | RestrictHighlightAndSnippetArrays Bool -- Restrict arrays in highlight and snippet results to items that matched the query.
    | Page Int -- Number of the page to retrieve.
    | HitsPerPage Int -- Maximum number of hits per page.
    | Offset Int -- Offset of the first hit to return (zero-based).
    | Length Int -- Maximum number of hits to return. (1000 is the maximum)
    | MinWordSizefor1Typo Int -- Minimum number of characters a word in the query string must contain to accept matches with one typo.
    | MinWordSizefor2Typos Int -- Minimum number of characters a word in the query string must contain to accept matches with two typos.
    | TypoTolerance TypoToleranceValue -- Controls whether typo tolerance is enabled and how it is applied:
    | AllowTyposOnNumericTokens Bool -- Whether to allow typos on numbers ("numeric tokens") in the query string.
    | IgnorePlurals IgnorePluralsValue -- Consider singular and plurals forms a match without typo.
    | DisableTypoToleranceOnAttributes (List String) -- List of attributes on which you want to disable typo tolerance
    | AroundLatLng String -- Search for entries around a given location.
    | AroundLatLngViaIP Bool -- Search for entries around a given location automatically computed from the requester's IP address.
    | AroundRadius AroundRadiusValue -- Maximum radius for geo search (in meters).
    | AroundPrecision Int -- Precision of geo search (in meters).
    | MinimumAroundRadius Int -- Minimum radius (in meters) used for a geo search when <%= parameter_link('aroundRadius') -%> is not set.
    | InsideBoundingBox (List Float) -- Search inside a rectangular area (in geo coordinates).
    | InsidePolygon (List Float) -- Search inside a polygon (in geo coordinates).
    | QueryType QueryTypeValue -- Controls if and how query words are interpreted as prefixes.
    | RemoveWordsIfNoResults RemoveWordsIfNoResultsValue -- Selects a strategy to remove words from the query when it doesn't match any hits.
    | AdvancedSyntax Bool -- Enables the advanced query syntax.
    | OptionalWords (List String) -- List of words that should be considered as optional when found in the query.
    | RemoveStopWords RemoveStopWordsValue -- Remove stop words from the query **before** executing it.
    | DisableExactOnAttributes (List String) -- List of attributes on which you want to disable computation of the `exact` ranking criterion
    | ExactOnSingleWordQuery ExactOnSingleWordQueryValue -- Controls how the `exact` ranking criterion is computed when the query contains only one word.
    | AlternativesAsExact (List String) -- List of alternatives that should be considered an exact match by the `exact` ranking criterion.
    | EnableRules Bool -- Whether rules should be globally enabled.
    | RuleContexts (List String) -- Enables contextual rules.
    | MinProximity Int -- Precision of the `proximity` ranking criterion.
    | ResponseFields (List String) -- Choose which fields the response will contain. Applies to search and browse queries.
    | MaxFacetHits Int -- Maximum number of facet hits to return during a search for facet values.
    | PercentileComputation Bool -- Whether to include the query in processing time percentile computation.
    | Distinct Int -- Controls de-duplication of results.
    | GetRankingInfo Bool -- Enables detailed ranking information.
    | NumericFilters (List String) -- Filter hits based on values of numeric attributes.
    | TagFilters (List String) -- Filter hits by tags.
    | Analytics Bool -- Whether the current query will be taken into account in the Analytics.
    | AnalyticsTags (List String) -- List of tags to apply to the query in the analytics.
    | Synonyms Bool -- Whether to take into account synonyms defined for the targeted index.
    | ReplaceSynonymsInHighlight Bool -- Whether to replace words matched via synonym expansion by the matched synonym in highlight and snippet results.


{-| -}
type FacetsValue
    = FacetsStrings (List String)
    | FacetsAll


{-| -}
type ExactOnSingleWordQueryValue
    = ExactOnSingleWordQueryAttribute
    | ExactOnSingleWordQueryNone
    | ExactOnSingleWordQuertWord


{-| -}
type TypoToleranceValue
    = TypoToleranceTrue
    | TypoToleranceFalse
    | TypoToleranceMin
    | TypoToleranceStrict


{-| -}
type IgnorePluralsValue
    = IgnorePluralsTrue
    | IgnorePluralsFalse
    | IgnorePluralsList (List String)


{-| -}
type AroundRadiusValue
    = AroundRadiusAll
    | AroundRadiusInt Int


{-| -}
type QueryTypeValue
    = PrefixLast
    | PrefixAll
    | PrefixNone


{-| -}
type RemoveWordsIfNoResultsValue
    = RemoveNoneIfNoResults
    | LastWords
    | FirstWords
    | AllOptional


{-| -}
type RemoveStopWordsValue
    = RemoveStopWordsTrue
    | RemoveStopWordsFalse
    | RemoveStopWordsList (List String)


getSearchString : SearchParam -> String
getSearchString a =
    case a of
        Query str ->
            "query=" ++ str

        AttributesToRetrieve ls ->
            "attributesToRetrieve=" ++ encodeList ls

        RestrictSearchableAttributes ls ->
            "restrictSearchableAttributes=" ++ encodeList ls

        Filters str ->
            "filters=" ++ str

        Facets f ->
            case f of
                FacetsStrings ls ->
                    "facets=" ++ encodeList ls

                FacetsAll ->
                    "facets=*"

        MaxValuesPerFacet n ->
            "maxValuesPerFacet=" ++ toString n

        FacetFilters ls ->
            "facetFilters=" ++ encodeList ls

        FacetingAfterDistinct bool_ ->
            "facetingAfterDistinct=" ++ (Encode.bool bool_ |> Encode.encode 0)

        OptionalFilters ls ->
            "optionalFilters=" ++ encodeList ls

        SortFacetValuesBy str ->
            "sortFacetValuesBy=" ++ str

        SumOrFiltersScores bool_ ->
            "sumOrFiltersScores=" ++ (Encode.bool bool_ |> Encode.encode 0)

        AttributesToHighlight ls ->
            "attributesToHighlight=" ++ encodeList ls

        AttributesToSnippet ls ->
            "attributesToSnippet=" ++ encodeList ls

        HighlightPreTag str ->
            "highlightPreTag=" ++ str

        HighlightPostTag str ->
            "highlightPostTag=" ++ str

        SnippetEllipsisText str ->
            "snippetEllipsisText=" ++ str

        RestrictHighlightAndSnippetArrays bool_ ->
            "restrictHighlightAndSnippetArrays=" ++ (Encode.bool bool_ |> Encode.encode 0)

        Page n ->
            "page=" ++ toString n

        HitsPerPage n ->
            "hitsPerPage=" ++ toString n

        Offset n ->
            "offset=" ++ toString n

        Length n ->
            "length=" ++ toString n

        MinWordSizefor1Typo n ->
            "minWordSizefor1Typo=" ++ toString n

        MinWordSizefor2Typos n ->
            "minWordSizefor2Typos=" ++ toString n

        AroundPrecision n ->
            "aroundPrecision=" ++ toString n

        MinimumAroundRadius n ->
            "minimumAroundRadius=" ++ toString n

        MinProximity n ->
            "minProximity=" ++ toString n

        MaxFacetHits n ->
            "maxFacetHits=" ++ toString n

        Distinct n ->
            "distinct=" ++ toString n

        DisableTypoToleranceOnAttributes ls ->
            "disableTypoToleranceOnAttributes=" ++ encodeList ls

        OptionalWords ls ->
            "optionalWords=" ++ encodeList ls

        DisableExactOnAttributes ls ->
            "disableExactOnAttributes=" ++ encodeList ls

        AlternativesAsExact ls ->
            "alternativesAsExact=" ++ encodeList ls

        RuleContexts ls ->
            "ruleContexts=" ++ encodeList ls

        ResponseFields ls ->
            "responseFields=" ++ encodeList ls

        NumericFilters ls ->
            "numericFilters=" ++ encodeList ls

        TagFilters ls ->
            "tagFilters=" ++ encodeList ls

        AnalyticsTags ls ->
            "analyticsTags=" ++ encodeList ls

        AroundLatLng str ->
            "aroundLatLng=" ++ str

        AllowTyposOnNumericTokens bool_ ->
            "allowTyposOnNumericTokens=" ++ (Encode.bool bool_ |> Encode.encode 0)

        AroundLatLngViaIP bool_ ->
            "aroundLatLngViaIP=" ++ (Encode.bool bool_ |> Encode.encode 0)

        AdvancedSyntax bool_ ->
            "advancedSyntax =" ++ (Encode.bool bool_ |> Encode.encode 0)

        EnableRules bool_ ->
            "enableRules=" ++ (Encode.bool bool_ |> Encode.encode 0)

        PercentileComputation bool_ ->
            "percentileComputation=" ++ (Encode.bool bool_ |> Encode.encode 0)

        GetRankingInfo bool_ ->
            "getRankingInfo=" ++ (Encode.bool bool_ |> Encode.encode 0)

        Analytics bool_ ->
            "analytics=" ++ (Encode.bool bool_ |> Encode.encode 0)

        Synonyms bool_ ->
            "synonyms=" ++ (Encode.bool bool_ |> Encode.encode 0)

        ReplaceSynonymsInHighlight bool_ ->
            "replaceSynonymsInHighlight=" ++ (Encode.bool bool_ |> Encode.encode 0)

        InsideBoundingBox float_ls ->
            "insideBoundingBox=" ++ encodeFloatList float_ls

        InsidePolygon float_ls ->
            "insidePolygon=" ++ encodeFloatList float_ls

        TypoTolerance val ->
            case val of
                TypoToleranceTrue ->
                    "typoTolerance=true"

                TypoToleranceFalse ->
                    "typoTolerance=false"

                TypoToleranceMin ->
                    "typoTolerance=min"

                TypoToleranceStrict ->
                    "typoTolerance=strict"

        IgnorePlurals val ->
            case val of
                IgnorePluralsTrue ->
                    "ignorePlural=true"

                IgnorePluralsFalse ->
                    "ignorePlural=false"

                IgnorePluralsList ls ->
                    "ignorePlural=" ++ encodeList ls

        AroundRadius val ->
            case val of
                AroundRadiusAll ->
                    "aroundRadius=all"

                AroundRadiusInt n ->
                    "aroundRadius=" ++ toString n

        QueryType val ->
            case val of
                PrefixLast ->
                    "queryType=prefixLast"

                PrefixAll ->
                    "queryType=prefixAll"

                PrefixNone ->
                    "queryType=prefixNone"

        RemoveWordsIfNoResults val ->
            case val of
                RemoveNoneIfNoResults ->
                    "removeWordsIfNoResults=none"

                LastWords ->
                    "removeWordsIfNoResults=lastWords"

                FirstWords ->
                    "removeWordsIfNoResults=firstWords"

                AllOptional ->
                    "removeWordsIfNoResults=allOptional"

        RemoveStopWords val ->
            case val of
                RemoveStopWordsTrue ->
                    "removeStopWords=true"

                RemoveStopWordsFalse ->
                    "removeStopWords=false"

                RemoveStopWordsList ls ->
                    "removeStopWordsList=" ++ encodeList ls

        ExactOnSingleWordQuery val ->
            case val of
                ExactOnSingleWordQueryAttribute ->
                    "exactOnSingleWordQuery=attribute"

                ExactOnSingleWordQueryNone ->
                    "exactOnSingleWordQuery=none"

                ExactOnSingleWordQuertWord ->
                    "exactOnSingleWordQuert=word"


encodeList : List String -> String
encodeList ls =
    List.map Encode.string ls
        |> Encode.list
        |> Encode.encode 0


encodeFloatList : List Float -> String
encodeFloatList ls =
    List.map Encode.float ls
        |> Encode.list
        |> Encode.encode 0



-- Method related stuff


{-| -}
type alias SearchRecord =
    { indexName : String, params : Maybe (List SearchParam) }


{-| -}
type Strategy
    = NoStrategy
    | StopIfEnoughMatches


{-| Method

-}
type Method
    = -- Version1
      --Indices api
      SearchAnIndex SearchRecord
    | SearchAnIndexAlternative SearchRecord
    | SearchMultipleIndexes { requests : List SearchRecord, strategy : Strategy }
    | SearchForFacetvalues
        { searchRecord : SearchRecord
        , facetName : String
        , facetQuery : Maybe String
        , maxFacetHits : Maybe Int
        }
    | AddAnObjectWithoutID { indexName : String, value : Value }
    | AddUpdateAnObjectByID { indexName : String, value : Value, objectId : String }
    | DeleteAnObject { indexName : String, objectId : String }
    | DeleteByQuery SearchRecord
    | PartiallyUpdateAnObject { indexName : String, objectId : String, createIfNotExists : Maybe Bool, value : Value }
    | RetrieveMultipleObjects { indexName : String, objectId : String, attributesToRetreive : Maybe (List String) }
    | RetrieveAnObject { indexName : String, objectId : String, attributesToRetreive : Maybe (List String) }
    | GetIndexSettings { indexName : String }
    | ChangeIndexSettings { searchRecord : SearchRecord, forwardToReplicas : Maybe Bool }
    | ClearIndex { indexName : String }
    | DeleteIndex { indexName : String }
    | CopyIndex { indexName : String, operation : IndexOperation, destination : String }
    | ListIndexes { page : Maybe Int }



{--Cannot think of a use case to include these api Methods on the client side

    | AddAnIndexSpecificAPIkey
    | UpdateAnIndexSpecificAPIKey
    | ListIndexSpecificAPIKeys
    | ListIndexSpecificAPIKeysAll
    | RetrieveAnIndexSpecificAPIKey
    | DeleteAnIndexSpecificAPIKey
-
    | BatchWriteOperations
    | BatchWriteOperationsMultiple
    | BrowseAllIndexContent
    | BrowseAllIndexContentAlternative
    | GetTaskStatus

    -- Synonyms api
    | UpdateSynonym
    | BatchSynonyms
    | GetSynonym
    | DeleteAllSynonyms
    | DeleteSynonymSet
    | SearchSynonyms

      --Keys api
    | AddAPIKey
    | UpdateAPIKey
    | ListAPIKeys
    | GetAPIKey
    | DeleteAPIKey

      -- Logs api
    | GetLogs
--}


{-| -}
type IndexOperation
    = Move
    | Copy



formatUrl: {a | appId : String} -> String
formatUrl config =
    "https://"
        ++ config.appId
        ++ "-dsn.algolia.net/"
        ++ "1/"

formatParams : List SearchParam -> String
formatParams params =
    params
        |> List.map (\a -> getSearchString a)
        |> List.intersperse "&"
        |> List.foldl (++) ""



{-| -}
formRqst : { a | api : Method, appId : String } -> {url : String, body : Http.Body, httpMethod : String} 
formRqst config =
    let
        urlDsn =
            formatUrl config
    in
    case config.api of
        SearchAnIndex a ->
            { url = urlDsn ++ "indexes/" ++ a.indexName ++ "/query"
            , body = getParams a.params |> jsonBody
            , httpMethod = "POST"
            }

        SearchAnIndexAlternative a ->
            let
                params =
                    getParams a.params |> Encode.encode 0
            in
            { url = urlDsn ++ "indexes/" ++ a.indexName ++ "?query=" ++ params
            , body = emptyBody
            , httpMethod = "GET"
            }

        SearchMultipleIndexes a ->
            let
                strategy =
                    case a.strategy of
                        NoStrategy ->
                            "none"

                        StopIfEnoughMatches ->
                            "stopIfEnoughMatches"

                requests =
                    a.requests
                        |> List.map
                            (\a ->
                                Encode.object
                                    [ ( "indexName", Encode.string a.indexName )
                                    , ( "params", getParams a.params )
                                    ]
                            )
                        |> Encode.list

                body =
                    Encode.object [ ( "requests", requests ), ( "strategy", Encode.string strategy ) ]
                        |> jsonBody
            in
            { url = urlDsn ++ "indexes/*/queries"
            , body = body
            , httpMethod = "POST"
            }

        SearchForFacetvalues a ->
            let
                url =
                    urlDsn
                        ++ a.searchRecord.indexName
                        ++ "/facets/"
                        ++ a.facetName
                        ++ "/query"

                maxHits =
                    case a.maxFacetHits of
                        Just v ->
                            [ ( "maxFacetHits", Encode.int v ) ]

                        Nothing ->
                            []

                query =
                    case a.facetQuery of
                        Just v ->
                            [ ( "facetQuery", Encode.string v ) ]

                        Nothing ->
                            []

                body =
                    Encode.object ([ ( "params", getParams a.searchRecord.params ) ] ++ maxHits ++ query)
                        |> jsonBody

                httpMethod =
                    "POST"
            in
            { url = url
            , body = body
            , httpMethod = httpMethod
            }

        AddAnObjectWithoutID a ->
            { url = urlDsn ++ "indexes/" ++ a.indexName
            , body = jsonBody a.value
            , httpMethod = "POST"
            }

        AddUpdateAnObjectByID a ->
            { url = urlDsn ++ "indexes/" ++ a.indexName ++ "/" ++ "a.objectId"
            , body = jsonBody a.value
            , httpMethod = "PUT"
            }

        DeleteAnObject a ->
            { url = urlDsn ++ "indexes/" ++ a.indexName ++ "/" ++ "a.objectId"
            , body = emptyBody
            , httpMethod = "DELETE"
            }

        DeleteByQuery a ->
            { url = urlDsn ++ "indexes/" ++ a.indexName ++ "/deleteByQuery"
            , body = getParams a.params |> jsonBody
            , httpMethod = "POST"
            }

        PartiallyUpdateAnObject a ->
            let
                createIfNotExists =
                    case a.createIfNotExists of
                        Just val ->
                            Encode.bool val |> Encode.encode 0 |> (++) "?createIfNotExists="

                        Nothing ->
                            ""
            in
            { url = urlDsn ++ "indexes/" ++ a.indexName ++ "/" ++ a.objectId ++ "/partial"
            , body = jsonBody a.value
            , httpMethod = "POST"
            }

        RetrieveMultipleObjects a ->
            let
                attributes =
                    case a.attributesToRetreive of
                        Just attr ->
                            attr
                                |> List.map Encode.string
                                |> Encode.list
                                |> (\a -> [ ( "attributesToRetrieve", a ) ])

                        Nothing ->
                            []

                requests =
                    Encode.object
                        ([ ( "indexName", Encode.string a.indexName )
                         , ( "objectId", Encode.string a.objectId )
                         ]
                            ++ attributes
                        )

                body =
                    Encode.object [ ( "requests", requests ) ]
                        |> jsonBody
            in
            { url = urlDsn ++ "indexes/*/objects"
            , body = body
            , httpMethod = "POST"
            }

        RetrieveAnObject a ->
            let
                attributes =
                    case a.attributesToRetreive of
                        Just attr ->
                            attr
                                |> List.map Encode.string
                                |> Encode.list
                                |> Encode.encode 0
                                |> (++) "?attributes"

                        Nothing ->
                            ""

                url =
                    urlDsn ++ "indexes/" ++ a.indexName ++ "/" ++ "objectId" ++ attributes
            in
            { url = url
            , body = emptyBody
            , httpMethod = "GET"
            }

        GetIndexSettings a ->
            { url = urlDsn ++ "indexes/" ++ a.indexName ++ "/settings"
            , body = emptyBody
            , httpMethod = "GET"
            }

        ChangeIndexSettings a ->
            let
                forwardToReplicas =
                    case a.forwardToReplicas of
                        Just val ->
                            val
                                |> Encode.bool
                                |> Encode.encode 0
                                |> (++) "?forwardToReplicas="

                        Nothing ->
                            ""
            in
            { url = urlDsn ++ "indexes/" ++ a.searchRecord.indexName ++ "/settings" ++ forwardToReplicas
            , body = getParams a.searchRecord.params |> jsonBody
            , httpMethod = "POST"
            }

        ClearIndex a ->
            { url = urlDsn ++ "indexes/" ++ a.indexName ++ "/clear"
            , body = emptyBody
            , httpMethod = "GET"
            }

        DeleteIndex a ->
            { url = urlDsn ++ "indexes/" ++ a.indexName
            , body = emptyBody
            , httpMethod = "DELETE"
            }

        CopyIndex a ->
            let
                operation =
                    case a.operation of
                        Move ->
                            Encode.string "move"

                        Copy ->
                            Encode.string "copy"

                body =
                    Encode.object
                        [ ( "operation", operation )
                        , ( "destination", Encode.string a.destination )
                        ]
                        |> jsonBody
            in
            { url = urlDsn ++ "indexes/" ++ a.indexName ++ "/operation"
            , body = body
            , httpMethod = "POST"
            }

        ListIndexes a ->
            let
                attributes =
                    case a.page of
                        Just pg ->
                            Encode.object [ ( "page", Encode.int pg ) ]
                                |> Encode.encode 0
                                |> (++) "?page="

                        Nothing ->
                            ""
            in
            { url = urlDsn ++ "indexes" ++ attributes
            , body = emptyBody
            , httpMethod = "GET"
            }


getParams : Maybe (List SearchParam) -> Encode.Value
getParams a =
    case a of
        Just params ->
            Encode.object [ ( "params", Encode.string (formatParams params) ) ]

        Nothing ->
            Encode.null
