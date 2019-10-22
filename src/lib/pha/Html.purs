module Pha.Html where

import Prelude
import Pha (h, Action, VDom, Prop(..))

key :: forall a. String -> Prop a
key = Key

attr :: forall a. String -> String -> Prop a
attr = Attr

class' :: forall a. String -> Boolean -> Prop a
class' = Class

style :: forall a. String -> String -> Prop a
style = Style

click :: forall a. Action a -> Prop a
click = Event "click"

-- elements

br :: forall a. VDom a
br = h "br" [] []

button :: forall a. Array (Prop a) -> Array (VDom a) -> VDom a
button = h "button"

div' :: forall a. Array (Prop a) -> Array (VDom a) -> VDom a
div' = h "div"

span :: forall a. Array (Prop a) -> Array (VDom a) -> VDom a
span = h "span"

h2 :: forall a. Array (Prop a) -> Array (VDom a) -> VDom a
h2 = h "h2"

-- attributes

width :: forall a. String -> Prop a
width = attr "width"
height :: forall a. String -> Prop a
height = attr "height"
href :: forall a. String -> Prop a
href = attr "href"


    -- svg
x :: forall a. String -> Prop a
x = attr "x"
y :: forall a. String -> Prop a
y = attr "y"
stroke :: forall a. String -> Prop a
stroke = attr "stroke"
fill :: forall a. String -> Prop a
fill = attr "fill"
viewBox :: forall a. String -> Prop a
viewBox = attr "viewBox"
transform :: forall a. String -> Prop a
transform = attr "transform"

g :: forall a. Array (Prop a) -> Array (VDom a) -> VDom a
g = h "g"

svg :: forall a. Array (Prop a) -> Array (VDom a) -> VDom a
svg = h "svg"

rect :: forall a. Array (Prop a) -> Array (VDom a) -> VDom a
rect = h "rect"

use :: forall a. Array (Prop a) -> Array (VDom a) -> VDom a
use = h "use"