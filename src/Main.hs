{-# LANGUAGE TypeApplications #-}

module Main where

import Codec.Picture.Types
import Control.Lens hiding ((<.>))
import Graphics.SvgTree
import Linear hiding (rotate)
import Reanimate
import System.FilePath

main :: IO ()
main = reanimate partyFerris

partyFerris :: Animation
partyFerris =
  animate $ \t ->
    dancingFerris t

dancingFerris :: Double -> SVG
dancingFerris t =
  mkGroup
    [ mkRainbowFilter t
    , mkDarkerRainbowFilter t
    , lowerBody
        & filterRef ?~ Ref "rainbow-darker"
    , upperBody
        & filterRef ?~ Ref "rainbow"
    , eyes
    , leftHand & filterRef ?~ Ref "rainbow"
        & rotateAround (- abs (t - 1 / 2) * 360) leftHandCenter
    , rightHand & rotateAround (- abs (t - 1 / 2) * 360 + 60) rightHandCenter
        & filterRef ?~ Ref "rainbow"
    ]
    & translate (0.5 * sin (2 * pi * (t -0.5))) 0.0
    & rotate (abs $ 5 * sin (4 * pi * t))

mkRainbowFilter :: Double -> SVG
mkRainbowFilter = mkRainbowFilterWith "rainbow" sinebow

mkDarkerRainbowFilter :: Double -> SVG
mkDarkerRainbowFilter = mkRainbowFilterWith "rainbow-darker" $
  \t ->
    let PixelRGB8 r g b = sinebow t
     in PixelRGB8 (r `div` 2) (g `div` 2) (b `div` 2)

mkRainbowFilterWith :: String -> (Double -> PixelRGB8) -> Double -> SVG
mkRainbowFilterWith name colMap t =
  let PixelRGBF r g b = promotePixel $ colMap t
   in FilterTree $
        mkFilter
          name
          [ FEColorMatrix $
              defaultSvg
                & colorMatrixType .~ Matrix
                & colorMatrixValues
                  .~ unwords
                    ( map
                        (unwords . map show)
                        [ [0, 0, 0, 0, r]
                        , [0, 0, 0, 0, g]
                        , [0, 0, 0, 0, b]
                        , [0, 0, 0, 1, 0]
                        ]
                    )
                & colorMatrixIn ?~ SourceGraphic
                & filterResult ?~ "matrix"
          , FEBlend $
              defaultSvg
                & blendIn ?~ SourceGraphic
                & blendIn ?~ SourceRef "matrix"
                & blendMode .~ Overlay
          ]

mkFilter :: String -> [FilterElement] -> Filter
mkFilter ident fe = defaultSvg & filterChildren .~ fe & attrId ?~ ident

upperBody :: SVG
upperBody = mkImage screenWidth screenHeight ("images" </> "ferris-upper-body" <.> "svg")

lowerBody :: SVG
lowerBody = mkImage screenWidth screenHeight ("images" </> "ferris-lower-body" <.> "svg")

leftHand :: SVG
leftHand = mkImage screenWidth screenHeight ("images" </> "ferris-left-hand" <.> "svg")

rightHand :: SVG
rightHand = mkImage screenWidth screenHeight ("images" </> "ferris-right-hand" <.> "svg")

eyes :: SVG
eyes = mkImage screenWidth screenHeight ("images" </> "ferris-eyes" <.> "svg")

rightHandCenter :: V2 Double
rightHandCenter = toCanvasCoord rightHandCenterInOriginalCoord

leftHandCenter :: V2 Double
leftHandCenter = toCanvasCoord leftHandCenterInOriginalCoord

toCanvasCoord :: V2 Double -> V2 Double
toCanvasCoord pt = (screenWidth / origWidth) *^ ((pt & _y %~ negate) ^-^ origCenter)

rightHandCenterInOriginalCoord :: V2 Double
rightHandCenterInOriginalCoord =
  V2 981.4999 491.7315

-- >>> screenWidth
leftHandCenterInOriginalCoord :: V2 Double
leftHandCenterInOriginalCoord =
  V2 390.007 491.7641

origWidth, origHeight :: Double
(origWidth, origHeight) = (1366, 768.375)

origCenter :: V2 Double
origCenter = V2 683 (-384.1875)
