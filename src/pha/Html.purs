module Pha.Html where

import Prelude
import Pha (VDom, Prop(..), h, text)
import Pha.Action (Action)

key :: ∀a effs. String -> Prop a effs
key = Key

attr :: ∀a effs. String -> String -> Prop a effs
attr = Attr

class' :: ∀a effs. String -> Boolean -> Prop a effs
class' = Class

style :: ∀a effs. String -> String -> Prop a effs
style = Style

click :: ∀a effs. Action a effs -> Prop a effs
click = Event "click"

contextmenu :: ∀a effs. Action a effs -> Prop a effs
contextmenu = Event "contextmenu"

pointermove :: ∀a effs. Action a effs -> Prop a effs
pointermove = Event "pointermove"

pointerup :: ∀a effs. Action a effs -> Prop a effs
pointerup = Event "pointerup"

pointerdown :: ∀a effs. Action a effs -> Prop a effs
pointerdown = Event "pointerdown"

pointerenter :: ∀a effs. Action a effs -> Prop a effs
pointerenter = Event "pointerenter"

pointerleave :: ∀a effs. Action a effs -> Prop a effs
pointerleave = Event "pointerleave"

keydown :: ∀a effs. Action a effs -> Prop a effs
keydown = Event "keydown"

-- elements

br :: ∀a effs. VDom a effs
br = h "br" [] []

button :: ∀a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
button = h "button"

div' :: ∀a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
div' = h "div"

span :: ∀a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
span = h "span"

h2 :: ∀a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
h2 = h "h2"

p :: ∀a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
p = h "p"

a :: ∀a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
a = h "a"

-- attributes

disabled :: ∀a effs. Boolean -> Prop a effs
disabled b = attr "disabled" (if b then "true" else "")
width :: ∀a effs. String -> Prop a effs
width = attr "width"
height :: ∀a effs. String -> Prop a effs
height = attr "height"
href :: ∀a effs. String -> Prop a effs
href = attr "href"

    -- svg
x :: ∀a effs. String -> Prop a effs
x = attr "x"
y :: ∀a effs. String -> Prop a effs
y = attr "y" 
stroke :: ∀a effs. String -> Prop a effs
stroke = attr "stroke"
fill :: ∀a effs. String -> Prop a effs
fill = attr "fill"
viewBox :: ∀a effs. Int -> Int -> Int -> Int -> Prop a effs
viewBox a b c d = attr "viewBox" $ show a <> " " <> show b <> " " <> show c <> " " <> show d
transform :: ∀a effs. String -> Prop a effs
transform = attr "transform"
strokeWidth :: ∀a effs. String -> Prop a effs
strokeWidth = attr "stroke-width"
strokeDasharray :: ∀a effs. String -> Prop a effs
strokeDasharray = attr "stroke-dasharray"


g :: ∀a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
g = h "g"

svg :: ∀a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
svg = h "svg"

rect :: ∀a effs. Number -> Number -> Number -> Number -> Array (Prop a effs) -> VDom a effs
rect x' y' w h' props = h "rect" ([attr "x" $ show x', attr "y" $ show y', attr "width" $ show w, attr "height" $ show h'] <> props) []

path :: ∀a effs. String -> Array (Prop a effs) -> VDom a effs
path d props = h "path" ([attr "d" d] <> props) []

line :: ∀a effs. Number -> Number -> Number -> Number -> Array (Prop a effs) -> VDom a effs
line x1 y1 x2 y2 props = h "line" ([attr "x1" $ show x1, attr "y1" $ show y1, attr "x2" $ show x2, attr "y2" $ show y2] <> props) []

circle :: ∀a effs. Number -> Number -> Number -> Array (Prop a effs) -> VDom a effs
circle cx cy r props = h "circle" ([attr "cx" $ show cx, attr "cy" $ show cy, attr "r" $ show r] <> props) []

use :: ∀a effs. Number -> Number -> Number -> Number -> String -> Array (Prop a effs) -> VDom a effs
use x y w h' href' props =
    h "use" ([attr "x" $ show x, attr "y" $ show y, attr "width" $ show w, attr "height" $ show h', attr "href" href'] <> props) []

text' :: ∀a effs. Number -> Number -> String -> Array (Prop a effs) -> VDom a effs
text' x y t props = h "text" ([attr "x" $ show x, attr "y" $ show y] <> props) [text t]

translate :: Number -> Number -> String
translate x y = "translate(" <> show x <> "px," <> show y <> "px)"

svguse :: ∀a effs. String -> Array (Prop a effs) -> VDom a effs
svguse symbol props = svg ([width "100%", height "100%"]  <> props) [h "use" [attr "href" symbol] []]

rgbColor :: Int -> Int -> Int -> String
rgbColor r g' b = "rgb(" <> show r <> "," <> show g' <> "," <> show b <> ")"

