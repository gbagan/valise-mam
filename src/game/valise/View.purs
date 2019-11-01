module Game.Valise.View where
import Prelude
import Data.Maybe (Maybe(..))
import Data.Lens (Lens')
import Game.Valise.Model (ValiseState, showHelpA)
import Pha (VDom, Prop, h, text, whenN, maybeN)
import Pha.Action ((🎲))
import Pha.Html (div', a, svg, g, class', svguse, href, width, height, viewBox, fill, attr, transform, style, x, y,
    pointerenter, pointerleave)

pos :: forall a. Int -> Int -> Int -> Int -> Array (Prop a)
pos x' y' w h = [
    width (show w),
    height (show h),
    x (show x'),
    y (show y')
]



valise :: forall a.  Lens' a ValiseState -> ValiseState -> VDom a
valise lens state = svg [viewBox "0 0 825 690"] [
    h "use" [href "#valise", class' "valise-close" true, width "100%", height "100%"] [], 
    
    -- onpointerup: actions.finishDrag,
    -- onpointermove: [actions.moveObject, relativePointerPosition],

    g [class' "valise-open" true] [
        h "use" [href "#openvalise"] [],

        object { symbol: "switch", link: Nothing, help: "", drag: false } 
            300 460 42 60 [] [], {-

            Object({
                symbol: 'bulboff',
                class: {'valise-bulb': true, on: state.isSwitchOn},
                help: 'Trouve un moyen d\'allumer l\'ampoule',
            }, -}

        object { symbol: "bulbon", link: Just "noirblanc", help: "Jeu: tour noir, tout blanc", drag: false } 
            477 280 48 48 [] [],
        {-
            Object({
                drag: 'flowerpot',
                symbol: 'flowerpot',
                class: 'valise-flowerpot',
                help: 'Quelque chose se cache derrière ce pot'
            }),
        -}
        object { symbol: "frog", link: Just "frog", help: "Jeu: la grenouille", drag: false}
            549 320 35 34 [fill "#bcd35f"] [],

        object { symbol: "hanoibot", link: Just "solitaire", help: "Jeu: solitaire", drag: false}
            500 430 75 51 [] [],       

          --      symbol: 'hanoitop',
          --      drag: 'hanoitop',
          --      class: 'valise-hanoi-top',
          --      help: 'Quelque chose se cache sous cette tour'
          --  }),

        object {symbol: "knight", link: Just "queens", help: "Jeu: les 8 reines", drag: false}
            461 380 24 48 [transform "rotate(40)"] [],
    
        object { symbol: "pen", link: Nothing, help: "Jeu: dessin (non implémenté)", drag: false}
            610 400 60 60 [] [],
        
        object {symbol: "stack", link: Just "jetons", help: "Jeu: jetons", drag: false}
            350 500 50 50 [] [],

        object {symbol: "wheel", link: Just "roue", help: "Jeu: roue des couleurs", drag: false}
            400 205 50 50 [transform "scale(1,0.8)"] [],

        object {symbol: "card", link: Just "nim", help: "Jeu: Poker Nim", drag: false}
            450 130 40 50 [transform "rotate(30)"] [],

        object {symbol: "tile", link: Just "tiling", help: "Jeu: carrelage", drag: false}
            280 400 120 60 [] [],

        object {symbol: "tricolor", link: Just "baseball", help: "Jeu: baseball multicolore", drag: false}
            350 330 90 60 [] [],

        object {symbol: "race", link: Just "paths", help: "Jeu: chemins", drag: false}
            450 445 64 64 [transform "rotate(40)"] [],
        -- ;

        {-
            repeat(7, i => i !== 3 &&
                Object({
                    symbol: 'paw',
                    class: `valise-pawimage valise-paw-${i}`
                })
            ),
            Object({
                symbol: 'paw',
                class: 'valise-pawimage valise-paw-3 valise-paw',
                style: {opacity: state.pawPassings*0.1},
                help: state.pawPassings === 4 ? 'Jeu: La bête' : 'Trouve un moyen de dévoiler l\'empreinte cachée',
                onpointerleave: actions.incPawPassings,
                link: state.pawPassings === 4 && 'labete'
            }),



            Object({
                symbol: 'quiet',
                class: 'valise-quiet',
                link: 'sansmot',
                help: 'Jeu: Preuve sans mot'
            }),
        -}
        object {symbol: "chocolate", link: Nothing, help: "Jeu: chocolat (non implémenté)", drag: false}
            200 200 60 60 [transform "rotate(40)"] []
            --  ;
    ]
] where
        object {drag, link, help, symbol} x' y' w' h' props children = 
            g [transform $ "translate(" <> show x' <> " " <> show y' <> ")"] [
                g props [
                    svg [
                        -- payloadFn = relativePointerPosition -- >>= (set('name', drag));
                        -- position = if drag then state.position[drag];
            
                        -- 'touch-action': 'none',
                        class' "valise-object ui-touch-action-none" true,
                        class' "draggable" drag,
                        width $ show w',
                        height $ show h',
                        pointerenter (lens 🎲 showHelpA help),
                        pointerleave (lens 🎲 showHelpA "")
                        -- style: {
                        --    ...(position && { left: position.left + 'px', top: position.top + 'px' })
                        --},
                        -- pointerdown: drag && combine([actions.beginDrag, payloadCreator], preventDefault),
                    ] [
                        h "use" [
                            href $ " #" <> symbol, class' "valise-symbol" true
                        ] [],
                        maybeN $ link <#> \l -> a [ href $ "#" <> l] [
                            h "rect" ([class' "valise-object-link" true, fill "transparent"] <> children) []
                        ]
                    ]
                ]
            ]

view :: forall a. Lens' a ValiseState -> ValiseState -> VDom a 
view lens state = div' [
    class' "ui-flex-center valise-main-container" true,
    class' "open" state.isOpen
] [
    div' [] [
        div' [class' "valise-logo" true] [svguse "#logo" []],
        div' [class' "valise-container" true] [
            valise lens state,
            div' [
                class' "valise-help" true,
                class' "visible" (state.helpVisible && state.help /= "")
            ] [text state.help] 
        ]
    ]
]