Add this to your application state module:

```haskell
import Control.Category ((.))
import Data.Lens.Common
import Prelude hiding ((.))
import Snap.Snaplet
import Snap.Snaplet.Muttonchops

-- The state
data App {
    -- ...
    _muttonchops :: Snaplet Muttonchops,
    -- ...
}

instance HasMuttonchops App where
    getMuttonchops = getL (snapletValue . muttonchops)
```

And this to your app initialiser:
```haskell
app :: SnapletInit App App
app = makeSnaplet "name" "description" Nothing $ do
    -- ...
    mc <- nestSnaplet "muttonchops" muttonchops $ muttonchopsInit "templates"
    -- ...
    return $ App mc -- and other stuff
```

Use at your own risk.
