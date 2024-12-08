-- Emulates some features found only in MegaParsec 9.6.*,
-- unavailable with our stack lts
module Megaparsec96 (initialPosState, initialState) where

import Text.Megaparsec (State (..), PosState(..), initialPos, defaultTabWidth)

initialState :: FilePath -> s -> State s e
initialState name s = State
    { stateInput = s
    , stateOffset = 0
    , statePosState = initialPosState name s
    , stateParseErrors = []
    }

initialPosState :: FilePath -> s -> PosState s
initialPosState name s = PosState
    { pstateInput = s
    , pstateOffset = 0
    , pstateSourcePos = initialPos name
    , pstateTabWidth = defaultTabWidth
    , pstateLinePrefix = ""
    }
