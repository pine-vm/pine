module ElmSyntax.Abstract.Import exposing (Import)

import ElmSyntax.Abstract.Exposing exposing (Exposing)
import ElmSyntax.Abstract.Module exposing (ModuleName)


type alias Import =
    { moduleName : ModuleName
    , moduleAlias : Maybe ModuleName
    , exposingList : Maybe Exposing
    }
