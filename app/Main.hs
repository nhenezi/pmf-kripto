module Main where

import Lib

text :: String
text = unlines [
  "TCFVGIHHMEEOILXIEKCEVMZIWKOKVYHKEAKAXMJL",
  "CHCYMTKDKAXMJLMXIVTKVMBIJCXSAIVIBIFLTKZJ",
  "IXHIOKVLIDCVMTKHCOSTKAILKDXSYKCOKLXMTKVI",
  "EZKSWMHMELSSCNRCXDS"
  ]


main :: IO ()
main = do
  print $ Lib.alphaDecode 3 10 text
  return ()
