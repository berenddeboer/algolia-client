## Instasearch for ELM powered by Algolia

[Algolia](https://www.algolia.com/) is a Saas that provides a powerful hosted search engine Api.

This package provides helper functions that make it easier to use Algolia search on client side with Elm.

[Link to the demo - Actor search](https://kaashyapan.github.io/algolia-client/examples/simpleExample/SimpleExample.html)

*This repo still has a few rough edges and is yet to be complete.*

#### Included Api (Indices api)

```
     SearchAnIndex
     SearchAnIndexAlternative
     SearchMultipleIndexes
     SearchForFacetvalues
     AddAnObjectWithoutID 
     AddUpdateAnObjectByID
     DeleteAnObject
     DeleteByQuery 
     PartiallyUpdateAnObject
     RetrieveMultipleObjects
     RetrieveAnObject 
     GetIndexSettings 
     ChangeIndexSettings
     ClearIndex 
     DeleteIndex
     CopyIndex 
     ListIndexes
```
     
#### Excluded Api 
     
Cannot think of a use case to include these api Methods on the client side. 
Can add them if someone requests
     
     
```
     AddAnIndexSpecificAPIkey
     UpdateAnIndexSpecificAPIKey
     ListIndexSpecificAPIKeys
     ListIndexSpecificAPIKeysAll
     RetrieveAnIndexSpecificAPIKey
     DeleteAnIndexSpecificAPIKey
     BatchWriteOperations
     BatchWriteOperationsMultiple
     BrowseAllIndexContent
     BrowseAllIndexContentAlternative
     GetTaskStatus
     
     -- Synonyms api
     UpdateSynonym
     BatchSynonyms
     GetSynonym
     DeleteAllSynonyms
     DeleteSynonymSet
     SearchSynonyms
     
     --Keys api
     AddAPIKey
     UpdateAPIKey
     ListAPIKeys
     GetAPIKey
     DeleteAPIKey
     
     -- Logs api
     GetLogs
```
     
