module Pha.Html where

import Prelude
import Pha (h, text, Action, VDom, Prop(..))

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
stroke :: forall a. String -> Prop a
stroke = attr "stroke"
fill :: forall a. String -> Prop a
fill = attr "fill"
viewBox :: forall a. String -> Prop a
viewBox = attr "viewBox"
transform :: forall a. String -> Prop a
transform = attr "transform"
strokeWidth :: forall a. String -> Prop a
strokeWidth = attr "stroke-width"
strokeDasharray :: forall a. String -> Prop a
strokeDasharray = attr "stroke-dasharray"


g :: forall a. Array (Prop a) -> Array (VDom a) -> VDom a
g = h "g"

svg :: forall a. Array (Prop a) -> Array (VDom a) -> VDom a
svg = h "svg"

rect :: forall a. Number -> Number -> Number -> Number -> Array (Prop a) -> VDom a
rect x y w h' props = h "rect" ([attr "x" $ show x, attr "y" $ show y, attr "width" $ show w, attr "height" $ show h'] <> props) []

path :: forall a. String -> Array (Prop a) -> VDom a
path d props = h "path" ([attr "d" d] <> props) []

line :: forall a. Number -> Number -> Number -> Number -> Array (Prop a) -> VDom a
line x1 y1 x2 y2 props = h "line" ([attr "x1" $ show x1, attr "y1" $ show y1, attr "x2" $ show x2, attr "y2" $ show y2] <> props) []

circle :: forall a. Number -> Number -> Number -> Array (Prop a) -> VDom a
circle cx cy r props = h "circle" ([attr "cx" $ show cx, attr "cy" $ show cy, attr "r" $ show r] <> props) []

use :: forall a. Number -> Number -> Number -> Number -> String -> Array (Prop a) -> VDom a
use x y w h' href' props =
    h "use" ([attr "x" $ show x, attr "y" $ show y, attr "width" $ show w, attr "height" $ show h', attr "href" href'] <> props) []

text' :: forall a. Number -> Number -> String -> Array (Prop a) -> VDom a
text' x y t props = h "text" ([attr "x" $ show x, attr "y" $ show y] <> props) [text t]

translate :: Number -> Number -> String
translate x y = "translate(" <> show x <> "px," <> show y <> "px)"


svguse :: forall a. String -> Array (Prop a) -> VDom a
svguse symbol props = svg [width "100%", height "100%"] [h "use" [attr "href" symbol] []]

