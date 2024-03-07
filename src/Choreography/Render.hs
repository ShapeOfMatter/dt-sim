module Choreography.Render where

import Choreography.AbstractSyntaxTree (Program)
import Choreography.Functors (removeContext)
import Utils (Pretty(pretty))

render :: Program ((,) a) -> String
render = pretty . removeContext
