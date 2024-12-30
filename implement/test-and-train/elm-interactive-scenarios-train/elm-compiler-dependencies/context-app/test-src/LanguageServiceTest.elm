module LanguageServiceTest exposing (..)

import LanguageService
import LanguageServiceInterface


getHoverForSingleModule moduleText row column filePath =
    let
        ( _, stateAfterAddModule ) =
            LanguageService.handleRequestInCurrentWorkspace
                (LanguageServiceInterface.AddWorkspaceFileRequest
                    filePath
                    -- { asBase64 = Base64.fromString moduleText, asText = Just moduleText }
                    { asBase64 = "", asText = Just moduleText }
                )
                (LanguageService.initLanguageServiceState [])

        ( hoverResponse, _ ) =
            LanguageService.handleRequestInCurrentWorkspace
                (LanguageServiceInterface.ProvideHoverRequest
                    { fileLocation = LanguageServiceInterface.WorkspaceFileLocation filePath
                    , positionLineNumber = row
                    , positionColumn = column
                    }
                )
                stateAfterAddModule
    in
    hoverResponse
