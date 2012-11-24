module Rendering.Colour.Names where

import Control.Monad.Identity(runIdentity)
import Data.Colour (Colour)
import Data.Map (Map, fromList)
import qualified Data.Colour.Names as C (readColourName)

import Rendering.Colour (colourFromHex)

--------------------------------------------------------------------------------
--Colour names.

--SVG 1.1 colour names, as supported by the Data.Colour module.
--In alphabetical order.
colourNames :: [String]
colourNames = ["aliceblue","antiquewhite","aqua","aquamarine","azure","beige"
        ,"bisque","black","blanchedalmond","blue","blueviolet","brown"
        ,"burlywood","cadetblue","chartreuse","chocolate","coral"
        ,"cornflowerblue","cornsilk","crimson","cyan","darkblue","darkcyan"
        ,"darkgoldenrod","darkgray","darkgreen","darkgrey","darkkhaki"
        ,"darkmagenta","darkolivegreen","darkorange","darkorchid","darkred"
        ,"darksalmon","darkseagreen","darkslateblue","darkslategray"
        ,"darkslategrey","darkturquoise","darkviolet","deeppink","deepskyblue"
        ,"dimgray","dimgrey","dodgerblue","firebrick","floralwhite"
        ,"forestgreen","fuchsia","gainsboro","ghostwhite","gold","goldenrod"
        ,"gray","grey","green","greenyellow","honeydew","hotpink","indianred"
        ,"indigo","ivory","khaki","lavender","lavenderblush","lawngreen"
        ,"lemonchiffon","lightblue","lightcoral","lightcyan"
        ,"lightgoldenrodyellow","lightgray","lightgreen","lightgrey"
        ,"lightpink","lightsalmon","lightseagreen","lightskyblue"
        ,"lightslategray","lightslategrey","lightsteelblue","lightyellow"
        ,"lime","limegreen","linen","magenta","maroon","mediumaquamarine"
        ,"mediumblue","mediumorchid","mediumpurple","mediumseagreen"
        ,"mediumslateblue","mediumspringgreen","mediumturquoise"
        ,"mediumvioletred","midnightblue","mintcream","mistyrose","moccasin"
        ,"navajowhite","navy","oldlace","olive","olivedrab","orange"
        ,"orangered","orchid","palegoldenrod","palegreen","paleturquoise"
        ,"palevioletred","papayawhip","peachpuff","peru","pink","plum"
        ,"powderblue","purple","red","rosybrown","royalblue","saddlebrown"
        ,"salmon","sandybrown","seagreen","seashell","sienna","silver"
        ,"skyblue","slateblue","slategray","slategrey","snow","springgreen"
        ,"steelblue","tan","teal","thistle","tomato","turquoise","violet"
        ,"wheat","white","whitesmoke","yellow","yellowgreen"]

readColourName :: String -> Colour Double
readColourName = runIdentity . C.readColourName

--------------------------------------------------------------------------------
--Gradient names.

gradientDict :: Map String [(Colour Double, Double)]
gradientDict = fmap (flip zip [0,1/8..1] . map colourFromHex) $ fromList gradientDict'
             --all gradients below are defined at 9 equally spaced points

gradientNames :: [String]
gradientNames = map fst gradientDict' 

gradientDict' :: [(String,[String])]
gradientDict' =
 [(,) "alpine"          ["475b7b", "4e6d77", "567760", "65835a", "7e8f6d", "9ca089", "bbbaa7", "dddbc8", "fffee9"]
 ,(,) "aquamarine"      ["aebcd9", "c0cbde", "becdda", "afc6d1", "9abbc6", "87afbe", "7da9bc", "83abc3", "a1bbd8"]
 ,(,) "army"            ["749781", "759771", "7e976d", "8b976d", "94926c", "988d6f", "a0957a", "b3b191", "c2c1a7"]
 ,(,) "atlantic"        ["22292a", "384647", "5a7577", "739ba1", "80b0bc", "8ab7cb", "93b6d0", "97a9c8", "7a7f9f"]
 ,(,) "aurora"          ["424242", "3a3660", "563d60", "765c5b", "898467", "92a088", "9a9cb9", "b077e5", "dc46f3"]
 ,(,) "avocado"         ["000000", "00380a", "007113", "259017", "4aaf1c", "7cc120", "aed425", "d7e730", "fffb3b"]
 ,(,) "beach"           ["da8042", "df9c48", "e5b44c", "e2c25b", "d6c478", "c5bfa1", "babdd0", "c6ccf6", "ffffff"]
 ,(,) "bluegreenyellow" ["1f0266", "18367b", "176387", "1e898b", "2da687", "4abb7c", "73cc71", "a8da64", "e9e559"]
 ,(,) "brass"           ["25270c", "5d5524", "a79746", "f1da6a", "dcc762", "e0cb64", "bcab53", "7c7134", "2d2a0e"]
 ,(,) "browncyan"       ["593316", "8f6b4a", "ba9d83", "d2c3b2", "d5e1db", "c2eff3", "a3ecf8", "81daef", "57a6c5"]
 ,(,) "candy"           ["673458", "94385b", "bc4468", "d46486", "d488a9", "c3a3c6", "b2badb", "acd4e8", "a8dee1"]
 ,(,) "cherry"          ["373737", "852a2a", "b32c2c", "cd3b3c", "db5456", "e37678", "eb9ea0", "f4cbcd", "ffffff"]
 ,(,) "cmyk"            ["4daee6", "7ba8df", "ac74b9", "d1739c", "e8a184", "f0cf7c", "ded589", "a29f8b", "221f20"]
 ,(,) "coffee"          ["685447", "897057", "a4845d", "b58d54", "c49b58", "d3ad66", "e2c98e", "efe4c2", "f9ffff"]
 ,(,) "cold"            ["000000", "000060", "0000bf", "0020ff", "0080ff", "00dfff", "40ffff", "9fffff", "ffffff"]
 ,(,) "darkrainbow"     ["3d5793", "415d83", "477354", "628940", "9fac43", "d3c14f", "d89f50", "bf4f40", "ba3d3b"]
 ,(,) "darkterrain"     ["001177", "3e567d", "5e7478", "717c69", "777051", "785f3b", "846340", "b39c86", "ffffff"]
 ,(,) "deepsea"         ["2b004d", "3c0873", "4e0f99", "4532b7", "3c54d4", "427ee6", "48a9f8", "86cafb", "c5ecff"]
 ,(,) "fall"            ["426565", "546550", "665c43", "7a533b", "945438", "b76d38", "d9903a", "f0b23d", "f5ca43"]
 ,(,) "fruitpunch"      ["ff7f00", "fd9701", "eba708", "d7a91e", "c99d44", "c78574", "d06a9f", "e257b0", "f45c8a"]
 ,(,) "fuchsia"         ["1a1a1a", "422c3f", "6a3f63", "8e5484", "af74a4", "cc98c3", "e3c1dd", "f0dced", "f7eef6"]
 ,(,) "gray"            ["1a1a1a", "2d3134", "40474c", "525a60", "6c7478", "8b9192", "b1b3b0", "d0d1ca", "eaebe1"]
 ,(,) "grayyellow"      ["2e374b", "464e65", "666e84", "8b91a4", "b0b4c0", "d1d0ce", "e8dec5", "f2d99a", "eebc3f"]
 ,(,) "greenbrown"      ["000000", "324241", "677c61", "959e68", "b1a661", "bf9f5b", "c39b65", "d0b394", "ffffff"]
 ,(,) "greenpink"       ["003e06", "02a711", "34ec3c", "9bf99b", "ece6eb", "fe99fd", "f147f0", "a724a7", "3e0d3e"]
 ,(,) "island"          ["c35d35", "889e98", "8dc2c7", "acdade", "cce9e2", "deeed3", "dae7b1", "c3d881", "a9c84f"]
 ,(,) "lake"            ["4b0f87", "653cab", "7f69d0", "9691e8", "a9afe9", "bccee9", "cedce4", "dfe1dc", "f0e7d5"]
 ,(,) "lighttemp"       ["2a48ef", "6199f3", "96d1f7", "c5f3fb", "eefee8", "fcf6b7", "f3d985", "e6a555", "d85e31"]
 ,(,) "lightterrain"    ["8cc5d8", "86a9a3", "90a087", "a4a882", "bab98c", "cdcba0", "dbdab8", "e3e2d0", "e6e6e6"]
 ,(,) "mint"            ["77f9a3", "93f6b5", "adf2c5", "c3ecd1", "d3e1d7", "e0d5da", "e7c6d8", "e9b3d2", "e99ec9"]
 ,(,) "neon"            ["b8ec4c", "bebc4a", "c48d48", "cc6344", "d75143", "db5358", "d74d7b", "d241a0", "cd35c4"]
 ,(,) "pastel"          ["c278f0", "d49be2", "e5afad", "f0c598", "f4e097", "f2f2a0", "dcedc1", "a9d5ea", "6eb5ec"]
 ,(,) "pearl"           ["e7d6c6", "ede8dd", "d3d8cd", "b4beb7", "9ea9ad", "9aa0b6", "aaa7ce", "cabaeb", "f3cffb"]
 ,(,) "pigeon"          ["322d38", "45464f", "555d62", "61726c", "7e8984", "a1a1a1", "cbbbc8", "e9dbe6", "ffffff"]
 ,(,) "plum"            ["000000", "3f0f0f", "631d29", "752d47", "7d4165", "845b7e", "927d8b", "b1aa88", "e9e36e"]
 ,(,) "rainbow"         ["781c86", "4039c4", "447ccd", "5ca7a5", "83ba70", "b4bd4c", "dcab3c", "e67431", "db2122"]
 ,(,) "redblue"         ["732838", "a14e52", "c48674", "d9ba98", "dbd8bb", "bbd4d0", "86b9cf", "4d8fbb", "244f8c"]
 ,(,) "redgreen"        ["ff0000", "ff4444", "ff8888", "ffc3c3", "ffffff", "c1e0c6", "84c28e", "42a151", "008015"]
 ,(,) "rose"            ["274e17", "4d5b2d", "776c43", "a78459", "c08960", "ce825e", "c4654a", "bb4534", "b2231c"]
 ,(,) "rust"            ["000131", "321829", "633021", "95471a", "c65e12", "d56510", "e36b0e", "f1720b", "ff7809"]
 ,(,) "sandy"           ["a85135", "c3703f", "db9e48", "eabe4d", "eec64e", "e3bf4a", "b7a23d", "7b7b32", "4a5b31"]
 ,(,) "sienna"          ["772c12", "a14415", "cb5c19", "da7935", "e99752", "eab076", "ecca9a", "ead5b4", "e9e0cf"]
 ,(,) "solar"           ["780004", "a51003", "d21f01", "e44005", "f76008", "fb820e", "ffa514", "ffbb1a", "ffd120"]
 ,(,) "southwest"       ["654f34", "9e4b26", "af6b32", "bf965c", "d1b96d", "d5cb64", "c0c44c", "94b06f", "5998da"]
 ,(,) "starrynight"     ["162534", "2e4953", "426872", "688e90", "93b1a2", "becfa6", "dddc98", "f0da7e", "f4cf5e"]
 ,(,) "sunset"          ["000000", "471a61", "943263", "d54e37", "fa730d", "fe9f1c", "ffc84f", "ffe89e", "ffffff"]
 ,(,) "temp"            ["2e4eee", "6181f1", "a4b8f6", "e0e7fc", "fbfce8", "fdfc93", "f4dc48", "e18d39", "d0222a"]
 ,(,) "thermometer"     ["2a1fca", "4f66e8", "87aff2", "bbdceb", "dce3d1", "e7ccaa", "da977d", "ba5150", "88162b"]
 ,(,) "valentine"       ["841c33", "94253e", "a3314b", "b1445e", "c25f77", "d37c93", "e59db2", "efbbca", "f5d6df"]
 ,(,) "warm"            ["000000", "600000", "bf0000", "ff2000", "ff8000", "ffdf00", "ffff40", "ffff9f", "ffffff"]
 ,(,) "watermelon"      ["1a1a1a", "435633", "65884e", "85af6f", "a6cd93", "c7dbb3", "e3d4c2", "f0adad", "e15c5c"]
 ]
