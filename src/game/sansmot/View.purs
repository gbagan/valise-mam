module Game.Sansmot.View where

import MyPrelude
import Data.Map (Map, fromFoldable) as M
import Lib.Util (map2)
import Game.Effs (EFFS)
import Pha (VDom, Prop, text, maybeN)
import Pha.Action ((🔍))
import Pha.Html (div', p, h1, h2, svg, path, text',
                class', attr, style, stroke, fill, viewBox, pc, opacity, width, height, translate, click)
import Game.Sansmot.Model (State, pythaAnimation, animateA)

compStyle :: ∀a effs. { rotation :: Int, translation :: Tuple Int Int, duration :: Int} -> Array (Prop a effs)
compStyle { rotation, translation: x ~ y, duration} = [
    style "transform" $ 
        "rotate(" <> show rotation <> "deg) " <> translate (pc $ toNumber x / 700.0) (pc $ toNumber y / 300.0),
    style "transition" $ "all linear " <> show duration <> "ms"
]

pythaStyles :: ∀a effs. M.Map String (Array (Array (Prop a effs)))
pythaStyles = M.fromFoldable [
    "a" ~ [[opacity "0"], [opacity "1"], compStyle { translation: 400 ~ (-100), rotation: 0, duration: 600 }],
    "b" ~ [[opacity "0"], [opacity "1"], compStyle { translation: 600 ~ 0,      rotation: 0, duration: 600 }],
    "c" ~ [[opacity "0"], [opacity "1"], compStyle { translation: 400 ~ 0,      rotation: 0, duration: 600 }],
    "d" ~ [[opacity "0"], [opacity "1"], compStyle { translation: 300 ~ 200,    rotation: 0, duration: 600 }],
    "e" ~ [[opacity "0"], [opacity "1"]]
]

-- const bbbb = styles => state => styles |> omap((style, name) => style[state.anim[name] || 0]);
animPytha :: ∀a effs. State -> M.Map String (Array (Array (Prop a effs))) -> VDom a effs
animPytha {anim} styles =
    let f key = fromMaybe [] $ do
                let phase = anim ^. at key # fromMaybe 0
                aaa <- pythaStyles ^. at key
                aaa !! phase
                
    in
    svg [class' "sansmot-svg" true, viewBox 0 0 700 300, style "width" "84vmin", style "height" "36vmin"] [
        path "M 0 300 h 300 v -300 h -300 Z L 100 100 M 0 100 h 300 l -200 -100 v 300" [fill "transparent", stroke "#000"],
        path "M 400 300 h 300 v -300 h -300 Z M 400 200 l 200 100 l 100 -200 l -200 -100 l -100 200" [fill "transparent", stroke "#000"],
        path "M 0 300 v -200 h 100 Z" $ [fill "blue", stroke "#000"] <> f "a",
        path "M 0 300 h 100 v -200 Z" $ [fill "yellow", stroke "#000"] <> f "b",
        path "M 100 0 h 200 v 100 Z" $ [fill "#00FF00", stroke "#000"] <> f "c",
        path "M 100 0 v 100 h 200 Z" $ [fill "red", stroke "#000"] <> f "d",
        path "M 0 300 v -200 h 100 Z" $ [fill "blue", stroke "#000"] <> f "e",
        path "M 0 300 h 100 v -200 Z" $ [fill "yellow", stroke "#000"] <> f "e",
        path "M 100 0 h 200 v 100 Z" $ [fill "#00FF00", stroke "#000"] <> f "e",
        path "M 100 0 v 100 h 200 0 Z" $ [fill "red", stroke "#000"] <> f "e",
        text' 5 55 "a" $ [style "font-size" 20] <> f "e",
        text' 46 12 "a" $ [style "font-size" 20] <> f "e",
        text' 105 210 "b" $  [style "font-size" 20] <> f "e",
        text' 198 120 "b" $ [style "font-size" 20] <> f "e",
        text' 460 98 "c" $ [style "font-size" 20] <> f "e",
        text' 595 80 "c" $ [style "font-size" 20] <> f "e"
    ]

{-    
const LewisCaroll1 = () =>
    svg({ class: 'sansmot-svg', viewBox: '-10 -10 670 270', width: 670, height: 270 },
        path({ d: 'M 0 250 h 250 v -100 Z', fill: 'orange' }),
        path({ d: 'M 250 250 h 150 v -50 h 100 v -50 h -250 Z', fill: 'red' }),
        path({ d: 'M 400 250 h 250 v -100 h -100 v 50 h -150 Z', fill: 'blue' }),
        path({ d: 'M 250 150 h 400 v -150 Z', fill: 'green' }),
        repeat(14, i =>
            line({ x1: 50 * i, y1: -10, x2: 50 * i, y2: 260, class: 'sansmot-grid' })
        ),
        repeat(6, i =>
            line({ x1: -10, y1: 50 * i, x2: 660, y2: 50 * i, class: 'sansmot-grid' })
        )
    );

const LewisCaroll2 = () =>
    svg({ class: 'sansmot-svg', viewBox: '-10 -10 670 270', width: 670, height: 270 },
        path({ d: 'M 400 100 h 250 v -100 Z', fill: 'orange' }),
        path({ d: 'M 400 200 h 150 v -50 h 100 v -50 h -250 Z', fill: 'red' }),
        path({ d: 'M 400 250 h 250 v -100 h -100 v 50 h -150 Z', fill: 'blue' }),
        path({ d: 'M 0 250 h 400 v -150 Z', fill: 'green' }),
        repeat(14, i =>
            line({ x1: 50 * i, y1: -10, x2: 50 * i, y2: 260, class: 'sansmot-grid' })
        ),
        repeat(6, i =>
            line({ x1: -10, y1: 50 * i, x2: 660, y2: 50 * i, class: 'sansmot-grid' })
        )
    );
-}

view :: ∀a. Lens' a State -> State -> VDom a EFFS
view lens state = main where
    main = div' [] [
            --state.dialog === 'pytha' &&
            --Dialog({ onOk: [actions.setDialog, null] },
            --    AnimPytha({styles: bbbb(pythaStyles)(state)})
            --),
            --state.dialog === 'caroll' &&
            --Dialog({ onOk: [actions.setDialog, null], title: 'Cherchez l\'erreur' },
            --    LewisCaroll1(),
            --    LewisCaroll2(),
            --),

            -- h1({ class: 'sansmot-title' }, 'Preuve sans mot'),
            -- p(`Comme mise en bouche de cette série de divertissements mathématiques, nous vous proposons la
            --    preuve chinoise du théorème de Pythagore.`),

        h1 [class' "sansmot-title" true] "Preuve sans mot",

        h2 [class' "sansmot-h2" true] "Que raconte le théorème de Pythagore ?",
            {-
            p({ class: 'sansmot-center' },
                svg({
                    class: 'sansmot-svg',
                    viewBox: '0 0 700 100',
                    width: '490',
                    height: '70'
                },
                    mpath('M 350 0 V 100 H 500 Z', 'transparent'),
                    text({ x: 1, y: 50, 'font-size': 20 }, 'Etant donné un triangle rectangle'),
                    text({ x: 360, y: 50, 'font-size': 20 }, 'a'),
                    text({ x: 410, y: 90, 'font-size': 20 }, 'b'),
                    text({ x: 430, y: 40, 'font-size': 20 }, 'c'),
                    text({ x: 510, y: 50, 'font-size': 20 }, 'On a: a² + b² = c²'),
                )
            ),

            h2({ class: 'sansmot-h2' },
                'En voici un énoncé presque sans mot'
            ),
            -}

        p [class' "sansmot-center" true] [
            svg [class' "sansmot-svg" true, viewBox 0 (-100) 200 250, style "width" "20vmin", height "25vmin"] [
                path "M 50 50 h 100 v 100 h -100 Z" [fill "yellow", stroke "black"],
                path "M 0 0 h 50 v 50 h -50 Z" [fill "yellow", stroke "black"],
                path "M 50 0 l 100 50 l 50 -100 l -100 -50 Z" [fill "#00ff00", stroke "black"],
                text' 90 105 "a²" [attr "font-size" 35],
                text' 18 35 "b²" [attr "font-size" 35],
                text' 110 (-10) "c²" [attr "font-size" 35]
            ]
        ],

        h2 [class' "sansmot-h2" true] "Preuve sans mot due à un auteur chinois inconnu qui vivait vers 200 avant J.-C.",

        p [class' "sansmot-center" true] [
            animPytha state pythaStyles
        ],
        {-
        p [class' "sansmot-center" true] [
            svg [class' "sansmot-svg" true, viewBox 0 0 700 300, width 350, height 150] [
                path "M 0 300 l 300 0 l 0 -300 l -300 0 Z L 100 100 M 0 100 l 300 0 l -200 -100 l 0 300" [fill "transparent", stroke "black"],
                path "M 400 300 l 300 0 l 0 -300 l -300 0 Z M 400 200 l 200 100 l 100 -200 l -200 -100 l -100 200" [fill "transparent", stroke "black"]
            ]
        ],
        -}

        p [class' "sansmot-center" true, click $ lens 🔍 animateA pythaAnimation] [text "Explications"]
    ]
        {-
        a({
                    class: 'sansmot-link',
                    onclick: combine([actions.setDialog, 'pytha'],
                        actions.animate)
                },
                    'EXPLICATIONS'
                )
            ),
            p('Avant de vous convaincre avec d\'autres preuves sans mot, nous vous mettons cependant ',
                a({
                    class: 'sansmot-link',
                    onclick: [actions.setDialog, 'caroll']
                },
                    'en garde'
                )
            )
        )
