-- test/Spec.hs
import Test.Hspec
import Test.QuickCheck
import Planar



main :: IO ()
main = hspec $ do
  describe "Planar.moveRight" $ do
    it "If v = 2 and l = Link (1, [2,3,4]), should output 3" 
      $ do
          let v = 2
          let l = Link (1, [2,3,4])
          moveRight v l `shouldBe` (3::Vertex)
    it "If v = 4 and l = Link (1, [2,3,4]), should output 2" 
      $ do
          let v = 4
          let l = Link (1, [2,3,4])
          moveRight v l `shouldBe` (2::Vertex)
  describe "Planar.moveLeft" $ do
    it "If v = 2 and l = Link (1, [2,3,4]), should output 4" 
      $ do
          let v = 2
          let l = Link (1, [2,3,4])
          moveLeft v l `shouldBe` (4::Vertex)
    it "If v = 4 and l = Link (1, [2,3,4]), should output 3" 
      $ do
          let v = 4
          let l = Link (1, [2,3,4])
          moveLeft v l `shouldBe` (3::Vertex)


  -- describe "Lib.answer1" $ do
    -- let rules = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]
    -- it "turns rules into sequence of steps to be taken"
      -- $ do
      -- answer1 rules `shouldBe` ("CABDFE"::[Char])
  -- describe "Lib.actionTime" $ do
    -- it "outputs the amount of time Action takes"
      -- $ do map actionTime ['A'..'Z'] `shouldBe` ([61..86] :: [Int])
