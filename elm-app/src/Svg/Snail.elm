module Svg.Snail exposing (snail)

import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)


snail : Svg msg
snail =
    svg [ width "100%", height "100%", viewBox "0 0 600 429", version "1.1" ] [ g [ transform "matrix(1,0,0,1,-200,-285.714)" ] [ g [ transform "matrix(2.14286,0,0,2.14286,-442.857,-1000)" ] [ Svg.Styled.path [ d "M340.021,780C315.725,761.747 300,732.695 300,700C300,644.808 344.808,600 400,600C455.192,600 500,644.808 500,700L480,700C480,655.847 444.153,620 400,620C355.847,620 320,655.847 320,700C320,744.153 355.847,780 400,780L480.002,780C524.184,780 560,744.184 560,700.002L560,660L580,660L580,700.002C580,755.229 535.229,800 480.002,800L300,800L300,780L340.021,780ZM399.993,760C366.88,759.995 340,733.112 340,700C340,666.885 366.885,640 400,640C433.115,640 460,666.885 460,700L440,700C440,677.923 422.077,660 400,660C377.923,660 360,677.923 360,700C360,722.074 377.92,739.996 399.993,740L400,740L480,740C502.091,740 520,722.091 520,700L520,660L540,660L540,700C540,733.137 513.137,760 480,760L400,760L399.993,760L399.993,760ZM540,620L520,620L520,640L540,640L540,620ZM580,620L560,620L560,640L580,640L580,620Z", Svg.Styled.Attributes.style "fill:url(#_Linear1);" ] [] ] ], defs [] [ linearGradient [ id "_Linear1", x1 "0", y1 "0", x2 "1", y2 "0", gradientUnits "userSpaceOnUse", gradientTransform "matrix(280,0.00200101,-0.00200101,280,300,700)" ] [ stop [ offset "0", stopColor "rgb(153,61,142)", stopOpacity "1" ] [], stop [ offset "1", stopColor "rgb(151,95,255)", stopOpacity "1" ] [] ] ] ]