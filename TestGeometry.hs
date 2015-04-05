module TestGeometry where
import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Control.Lens

import Data.Maybe

import Linear
import Geometry
import Checkpoint


main :: IO ()
main = hspec $ do
    testRotations
    testHeading
    testCenterPoints
    testEndPoints

testEndPoints = 
    describe "Turn End Points" $ do
        let cp = Checkpoint (V2 5 0) (V2 0 1)

        it "computes 90-degree left turn" $ do
            let geo = Geometry cp (Just $ (-pi/2))
            (geo ^. position1) `shouldAlmostBe` (V2 4 1)
        
        it "computes 180-degree left turn" $ do
            let geo = Geometry cp (Just $ (-pi))
            (geo ^. position1) `shouldAlmostBe` V2 3 0

        it "computes 90-degree right turn" $ do
            let geo = Geometry cp (Just $ (pi/2))
            (geo ^. position1) `shouldAlmostBe` (V2 6 1)
        
        it "computes 180-degree right turn" $ do
            let geo = Geometry cp (Just $ pi)
            (geo ^. position1) `shouldAlmostBe` V2 7 0

        it "returns to original position after 360 degrees rotation" $ do
            let geo = Geometry cp (Just $ 2 * pi)
            (geo ^. position1) `shouldAlmostBe` (geo ^. position0)


testCenterPoints =
    describe "Turn Center Points" $ do
        let cp = Checkpoint (V2 5 0) (V2 0 1)

        it "computes right-turn center point to the right" $ do
            let geo = Geometry cp (Just $ (pi/10))
            (fromJust $ centre geo) `shouldAlmostBe` V2 1 0
        
        it "computes right-turn center point to the right (considering orientation)" $ do
            let geo = Geometry (cp & heading . _y .~ -1) (Just $ pi/10)
            (fromJust $ centre geo) `shouldAlmostBe` V2 (-1) 0

        it "computes left-turn center point to the left" $ do
            let geo = Geometry cp (Just $ (-pi/20))
            (fromJust $ centre geo) `shouldAlmostBe` V2 (-1) 0

        it "computes left-turn center point to the left (considering orientation)" $ do
            let geo = Geometry (cp & heading .~ V2 1 0) (Just $ (-pi/5))
            (fromJust $ centre geo) `shouldAlmostBe` V2 0 1


testHeading =
    describe "Heading Calculations" $ do
        let cp = Checkpoint (V2 5 0) (V2 0 1)

        it "corrects course 45 degrees clockwise" $ do
            let geo = Geometry cp (Just (pi/4))
            (geo ^. heading1) `shouldAlmostBe`  V2 (1 / sqrt 2) (1 / sqrt 2)

        it "corrects course 90 degrees counter-clockwise" $ do
            let geo = Geometry cp (Just (-pi/2))
            (geo ^. heading1) `shouldAlmostBe`  V2 (-1) 0

        it "corrects course 90 degrees clockwise" $ do
            let geo = Geometry cp (Just (pi/2))
            (geo ^. heading1) `shouldAlmostBe`  V2 1 0

        it "keeps course steady" $ do
            let geo = Geometry cp Nothing
            geo ^. heading1 `shouldBe` geo ^. heading0

            
            
testRotations = 
    describe "Rotations" $ do
        it "rotates 90 degrees counter-clockwise" $ do
            Geometry.rotate (pi / 2) (unit _x) `shouldAlmostBe` unit _y

        it "rotates 45 degrees counter-clockwise" $ do
            Geometry.rotate (pi / 4) (unit _x) `shouldAlmostBe` V2 (1 / sqrt 2) (1 / sqrt 2)

        it "rotates 45 degrees clockwise" $ do
            Geometry.rotate (-pi / 4) (unit _x) `shouldAlmostBe` V2 (1 / sqrt 2) (-1 / sqrt 2)

        it "rotates 180 degrees" $ do
            Geometry.rotate pi (unit _x) `shouldAlmostBe` Geometry.rotate (-pi) (unit _x)

shouldAlmostBe a b = 
    if nearZero (a - b) 
    then return () 
    else expectationFailure $ unwords ["Not equal", show a, "and", show b]
