import Quipper
import Quipper.Libraries.Arith

main :: IO ()
main = print_generic Preview labelled_add (qdint_shape 3) (qdint_shape 3)


labelled_add :: QDInt -> QDInt -> Circ (QDInt, QDInt,QDInt)
labelled_add x y = do
  label (x,y) ("x","y")
  xy <- q_add x y
  label xy ("x","y","x+y")
  return xy