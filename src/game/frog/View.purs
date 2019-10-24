module Game.Frog.View where

import Prelude
import Math (cos, sin, pi, sqrt)
import Data.Array (concat, mapWithIndex, reverse)
import Data.Tuple (Tuple(..))
import Data.String (joinWith)
import Data.Int (toNumber)
import Lib.Core (tabulate, pairwise, floatRange)
import Pha (VDom)
import Pha.Html (use, class', href, x, y, width, height)

type Cartesian = { x :: Number, y :: Number}
type Polar = { radius :: Number, theta :: Number }

lineIntersection :: Number -> Number -> Number -> Number -> { x :: Number, y :: Number }
lineIntersection  m1 b1 m2 b2 = { x, y: m1 * x + b1 } where x = (b2 - b1) / (m1 - m2)

polarToCartesian :: Polar -> Cartesian
polarToCartesian {radius, theta} = { x: radius * cos theta, y: radius * sin theta }

spiral :: Cartesian -> Number -> Number -> Number -> Number -> Number -> String
spiral center startRadius radiusStep startTheta endTheta thetaStep =
    floatRange startTheta endTheta thetaStep <#> (\theta ->
        let b = radiusStep / (2.0 * pi)
            r = startRadius + b * theta
            point = { x: center.x + r * cos theta, y: center.y + r * sin theta }
            slope = (b * sin theta + r * cos theta) / (b * cos theta - r * sin theta)
            intercept = -(slope * r * cos theta - r * sin theta)
        in
            { point, slope, intercept }
    )
    # pairwise
    # mapWithIndex (\i (Tuple a b) ->
        let { x, y } = lineIntersection a.slope a.intercept b.slope b.intercept
            p = ["Q", show $ x + center.x, show $ y + center.y, show $ b.point.x, show b.point.y]
        in
            if i == 0 then ["M", show a.point.x, show a.point.y] <> p else p
    )
    # concat
    # joinWith " "

spiralPointsPolar :: Int -> Array Polar
spiralPointsPolar n = reverse $ tabulate (n + 1) \i ->
    let theta = sqrt(if i == n then 21.0 else toNumber i * 20.0 / toNumber n) * 1.36 * pi
        radius = 61.0 * theta / (2.0 * pi)
    in
        { theta, radius }


spiralPoints :: Int -> Array Cartesian
spiralPoints n = spiralPointsPolar n <#> polarToCartesian

spiralPath :: String
spiralPath = spiral { x: 0.0, y: 0.0 } 0.0 61.0 0.0 (37.0 / 6.0 * pi) (pi / 6.0)

-- shift = (_, e) => e.shiftKey;
lily :: forall a. Number -> Number -> Int -> Boolean -> Boolean -> VDom a
lily px py i reachable hidden =
    use ([
        href "#lily",
        class' "frog-lily" true,
        class' "reachable" reachable,
        class' "hidden" hidden  
    ] <> (if i == 0 then
            [ width "80", height "80", x $ show $ px - 30.0, y $ show $ py - 45.0]
        else
            [ width "48", height "48", x $ show $ px - 24.0, y $ show $ py - 24.0]
    ))
{-
export default state => template(state, actions, C => {
    const pointsPolar = spiralPointsPolar(state.rows);

    const Config = () => iconfactory(state, actions)(I =>
        Card({ title: 'La grenouille' },
            I.Group({
                title: 'Déplacements autorisés',
                list: [1, 2, 3, 4, 5],
                multi: true,
                select: state.moves,
                onclick: actions.selectMove
            }),
            I.For2Players(),
            I.Group({ title: 'Options' },
                I.Help(), I.Undo(), I.Redo(), I.Reset(), I.Rules()
            )
        )
    );

    const Grid = () =>
        div({ class: 'ui-board frog-board' },
            svg({ viewBox: '-190 -200 400 400', height: '100%', width: '100%' },
                path({ id: 'spiral', d: spiralPath, fill: 'none', stroke: 'black', 'stroke-width': 3 }),
                line({ x1: 153, y1: 9, x2: 207, y2: 20, stroke: 'black', 'stroke-dasharray': 5, 'stroke-width': 6 }),
                line({ x1: 153, y1: 7, x2: 153, y2: 39, stroke: 'black', 'stroke-width': 3 }),
                line({ x1: 207, y1: 18, x2: 207, y2: 50, stroke: 'black', 'stroke-width': 3 }),

                spiralPoints(state.rows).map(({ x, y }, i) =>
                    g({
                        key: 'lily' + i,
                        onclick: actions.when(shift, [actions.mark, i], [actions.play, i])
                    },
                        lily({ i, x, y, reachable: false }),
                        lily({ i, x, y, reachable: true, hidden: !state.reachable[i] || state.hideReachable }),
                        text({ x, y, class: 'frog-index' }, state.help && state.rows - i)
                    )
                ),

                state.marked.map((b, i) => b && i !== state.position &&
                    use({
                        key: 'reach' + i,
                        href: '#frog',
                        class: 'frog-frog marked',
                        x: spiralPoints[i].x - 16,
                        y: spiralPoints[i].y - 20,
                        width: 32,
                        height: 32
                    })
                ),

                g({
                    key: 'frog',
                    class: 'frog-frog-container',
                    style: {
                        transform: `translate(${pointsPolar[state.position].radius}px, 0)`
                            + ` rotate(${pointsPolar[state.position].theta * 180 / Math.PI}deg)`,
                        'transform-origin': `${-pointsPolar[state.position].radius}px 0`,
                    }
                },
                    g({
                        class: 'frog-frog-container',
                        style: {
                            transform: `rotate(-${pointsPolar[state.position].theta * 180 / Math.PI}deg)`,
                        }
                    },
                        use({
                            href: '#frog',
                            width: 40,
                            height: 40,
                            x: -20,
                            y: -20,
                            class: {
                                'frog-frog': true,
                                goal: state.position === 0
                            },
                        })
                    ),
                )
            ),
            span(state.position === 0 ? 'Partie finie' : state.turn === 0
                ? 'Tour du premier joueur' : state.mode === 'duel' ? 'Tour du second joueur' : 'Tour de l\'IA'
            )
        );

    const Board = () => C.IncDecGrid(Grid());

    const HelpDialog = () =>
        C.HelpDialog(
            'Jeu de la grenouille', br,
            'Règles pas encore définies'
        );

    return {
        Board, HelpDialog, Config,
        winTitle: winTitleFor2Players(state)
    };
});
-}