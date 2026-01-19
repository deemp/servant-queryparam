{-# LANGUAGE OverloadedStrings #-}

import Data.Text.IO qualified as T
import Lima.Converter (Format (..), convertTo, def)

main :: IO ()
main = do
  prefix_ <- T.readFile "../servant-queryparam-core/Prefix.md"
  main_ <- T.readFile "../example/Main.hs"
  let full = prefix_ <> "\n" <> (Hs `convertTo` Md) def main_
  T.writeFile "../servant-queryparam-core/README.md" full
