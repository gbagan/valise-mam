module Game.Sansmot.View where

import MamPrelude
import Data.Map as Map
import Pha.Html (Html)
import Pha.Html as H
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import Pha.Html.Events as E
import Pha.Html.Util (pc, translate)
import Game.Sansmot.Model (Model, Msg(..), Page(..), pythaAnimation, carollAnimation)

line' ∷ ∀ a. Int → Int → Int → Int → Array (H.Prop a) → Html a
line' x1 y1 x2 y2 props = S.line ([ SA.x1 x1, SA.x2 x2, SA.y1 y1, SA.y2 y2 ] <> props)

-- besoin d'un transform par défault pour empécher un bug sous safari
defaultStyle ∷ ∀ a. Array (H.Prop a)
defaultStyle = [ H.style "transform" "translate(0px, 0px)" ]

compStyle ∷ ∀ a. Number → Number → { rotation ∷ Int, translation ∷ Tuple Int Int, duration ∷ Int } → Array (H.Prop a)
compStyle width height { translation: x ∧ y, duration } =
  [ H.style "transform" $
      translate (pc $ toNumber x / width) (pc $ toNumber y / height)
  , H.style "transition" $ "transform linear " <> show duration <> "ms"
  ]

pythaStyles ∷ ∀ a. Map String (Array (Array (H.Prop a)))
pythaStyles = Map.fromFoldable
  [ "a" ∧ [ [ SA.opacity 0.0 ], defaultStyle, compStyle 700.0 300.0 { translation: 400 ∧ (-100), rotation: 0, duration: 600 } ]
  , "b" ∧ [ [ SA.opacity 0.0 ], defaultStyle, compStyle 700.0 300.0 { translation: 600 ∧ 0, rotation: 0, duration: 600 } ]
  , "c" ∧ [ [ SA.opacity 0.0 ], defaultStyle, compStyle 700.0 300.0 { translation: 400 ∧ 0, rotation: 0, duration: 600 } ]
  , "d" ∧ [ [ SA.opacity 0.0 ], defaultStyle, compStyle 700.0 300.0 { translation: 300 ∧ 200, rotation: 0, duration: 600 } ]
  , "e" ∧ [ [ SA.opacity 0.0 ], [] ]
  ]

carollStyles ∷ ∀ a. Map String (Array (Array (H.Prop a)))
carollStyles = Map.fromFoldable
  [ "a" ∧ [ defaultStyle, compStyle 1370.0 270.0 { translation: 300 ∧ 150, rotation: 0, duration: 600 } ]
  , "b" ∧ [ defaultStyle, compStyle 1370.0 270.0 { translation: 550 ∧ 50, rotation: 0, duration: 600 } ]
  , "c" ∧ [ defaultStyle, compStyle 1370.0 270.0 { translation: 700 ∧ 0, rotation: 0, duration: 600 } ]
  , "d" ∧ [ defaultStyle, compStyle 1370.0 270.0 { translation: 950 ∧ (-100), rotation: 0, duration: 600 } ]
  , "e" ∧ [ [ SA.opacity 0.0 ], [] ]
  ]

animPytha ∷ Model → Html Msg
animPytha { anim } =
  let
    f key =
      let
        phase = anim ^. at key ?: 0
      in
        pythaStyles ^? (ix key ∘ ix phase) ?: []
  in
    S.svg [ H.class_ "sansmot-svg", SA.viewBox 0.0 0.0 700.0 300.0 ]
      [ S.path $ [ SA.d "M 0 300 h 300 v -300 h -300 Z L 100 100 M 0 100 h 300 l -200 -100 v 300", SA.fill "transparent", SA.stroke "#000" ]
      , S.path $
          [ SA.d "M 400 300 h 300 v -300 h -300 Z M 400 200 l 200 100 l 100 -200 l -200 -100 l -100 200"
          , SA.fill "transparent"
          , SA.stroke "#000"
          ]
      , S.path $ [ SA.d "M 0 300 v -200 h 100 Z", SA.fill "blue", SA.stroke "#000" ] <> f "a"
      , S.path $ [ SA.d "M 0 300 h 100 v -200 Z", SA.fill "yellow", SA.stroke "#000" ] <> f "b"
      , S.path $ [ SA.d "M 100 0 h 200 v 100 Z", SA.fill "#00FF00", SA.stroke "#000" ] <> f "c"
      , S.path $ [ SA.d "M 100 0 v 100 h 200 Z", SA.fill "red", SA.stroke "#000" ] <> f "d"
      , S.path $ [ SA.d "M 0 300 v -200 h 100 Z", SA.fill "blue", SA.stroke "#000" ] <> f "e"
      , S.path $ [ SA.d "M 0 300 h 100 v -200 Z", SA.fill "yellow", SA.stroke "#000" ] <> f "e"
      , S.path $ [ SA.d "M 100 0 h 200 v 100 Z", SA.fill "#00FF00", SA.stroke "#000" ] <> f "e"
      , S.path $ [ SA.d "M 100 0 v 100 h 200 0 Z", SA.fill "red", SA.stroke "#000" ] <> f "e"
      , S.text ([ SA.x 5.0, SA.y 55.0, H.style "font-size" "20" ] <> f "e") [ H.text "a" ] 
      , S.text ([ SA.x 46.0, SA.y 12.0, H.style "font-size" "20" ] <> f "e")  [ H.text "a" ]
      , S.text ([ SA.x 105.0, SA.y 210.0, H.style "font-size" "20" ] <> f "e") [ H.text "b" ]
      , S.text ([ SA.x 198.0, SA.y 120.0, H.style "font-size" "20" ] <> f "e") [ H.text "b" ]
      , S.text ([ SA.x 450.0, SA.y 98.0, H.style "font-size" "20" ] <> f "e") [ H.text "c" ]
      , S.text ([ SA.x 595.0, SA.y 80.0, H.style "font-size" "20" ] <> f "e") [ H.text "c" ]
      ]

animCaroll ∷ Model → Html Msg
animCaroll { anim } =
  let
    f key =
      let
        phase = anim ^. at key ?: 0
      in
        carollStyles ^? (ix key ∘ ix phase) ?: []
  in
    S.svg [ H.class_ "sansmot-svg", SA.viewBox (-10.0) (-10.0) 1370.0 270.0, H.style "width" "90vw", H.style "height" "19vw"] $ concat
      [ [ S.path $ [ SA.d "M 400 100 h 250 v -100 Z", SA.fill "orange" ] <> f "a"
        , S.path $ [ SA.d "M 400 200 h 150 v -50 h 100 v -50 h -250 Z", SA.fill "red" ] <> f "b"
        , S.path $ [ SA.d "M 400 250 h 250 v -100 h -100 v 50 h -150 Z", SA.fill "blue" ] <> f "c"
        , S.path $ [ SA.d "M 0 250 h 400 v -150 Z", SA.fill "green" ] <> f "d"
        , S.path $ [ SA.d "M 400 100 h 250 v -100 Z", SA.fill "orange" ] <> f "e"
        , S.path $ [ SA.d "M 400 200 h 150 v -50 h 100 v -50 h -250 Z", SA.fill "red" ] <> f "e"
        , S.path $ [ SA.d "M 400 250 h 250 v -100 h -100 v 50 h -150 Z", SA.fill "blue" ] <> f "e"
        , S.path $ [ SA.d "M 0 250 h 400 v -150 Z", SA.fill "green" ] <> f "e"
        ]
      , repeat 28 \i →
          line' (50 * i) (-10) (50 * i) 260 [ H.class_ "sansmot-grid" ]
      , repeat 6 \i →
          line' (-10) (50 * i) 1360 (50 * i) [ H.class_ "sansmot-grid" ]
      ]

view ∷ Model → Html Msg
view model =
  H.div [ H.class_ "sansmot-main" ]
    [ H.div [ H.class_ "sansmot-menu" ]
        [ H.div [ H.class_ "sansmot-pagelink", E.onClick \_ → SetPage PythaPage ] [ H.text "1" ]
        , H.div [ H.class_ "sansmot-pagelink", E.onClick \_ → SetPage CarollPage ] [ H.text "2" ]
        ]
    , main model.page
    ]
  where

  main PythaPage =
    H.div []
      [ H.h1 [ H.class_ "sansmot-title" ] [ H.text "Preuve sans mot" ]
      , H.h2 [ H.class_ "sansmot-h2" ] [ H.text "Que raconte le théorème de Pythagore ?" ]
      , H.p [ H.class_ "sansmot-center" ]
          [ S.svg [ H.class_ "sansmot-svg", SA.viewBox 0.0 (-100.0) 200.0 250.0 ]
              [ S.path [ SA.d "M 50 50 h 100 v 100 h -100 Z", SA.fill "yellow", SA.stroke "black" ]
              , S.path [ SA.d "M 0 0 h 50 v 50 h -50 Z", SA.fill "yellow", SA.stroke "black" ]
              , S.path [ SA.d "M 50 0 l 100 50 l 50 -100 l -100 -50 Z", SA.fill "#00ff00", SA.stroke "black" ]
              , S.text [ SA.x 90.0, SA.y 105.0, SA.fontSize 35 ] [H.text "a²"] 
              , S.text [ SA.x 18.0, SA.y 35.0, SA.fontSize 35 ]  [H.text "b²"]
              , S.text [ SA.x 110.0, SA.y (-10.0), SA.fontSize 35 ] [H.text "c²"]
              ]
          ]
      , H.h2 [ H.class_ "sansmot-h2" ] [ H.text "Preuve sans mot due à un auteur chinois inconnu qui vivait vers 200 avant J.-C." ]
      , H.p [ H.class_ "sansmot-center" ] [ animPytha model ]
      , H.p [ H.class_ "sansmot-center sansmot-link", E.onClick \_ → Animate pythaAnimation ] [ H.text "Lancer l'animation" ]
      ]

  main CarollPage =
    H.div []
      [ H.h1 [ H.class_ "sansmot-title" ] [ H.text "Preuve sans mot" ]
      , H.h2 [ H.class_ "sansmot-h2" ] [ H.text "Où est passé le carré manquant ?" ]
      , H.p [ H.class_ "sansmot-center" ] [ animCaroll model ]
      , H.p [ H.class_ "sansmot-center sansmot-link", E.onClick \_ → Animate carollAnimation ] [ H.text "Lancer l'animation" ]
      ]
