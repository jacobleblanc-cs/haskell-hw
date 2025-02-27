-- Write a function radius :: Double -> Double -> Double which takes two fp numbers, x and y, and returns the distance from the point (x,y) to the origin (Pythagorean theorem)
radius :: Double -> Double -> Double
radius x y = sqrt((x*x)+(y*y))

-- Write a function sumEvens :: Integer -> Integer which adds up all the even numbers from 1 to its input argument (Inclusive)
sumEvens :: Integer -> Integer
sumEvens x = sum[0,2..x]

-- Using ranges and/or list comprehension, create a list of all numbers 1<=n<=200 divisible by 17. Save this as a Haskell term multiplesOfSeventeen :: [Integer]
multiplesOfSeventeen :: [Integer]
multiplesOfSeventeen = [17,34..200]

-- Write a function multiplyEnds :: [Integer] -> Integer which multiplies the first and last element of the given list. If it is empty, return 1
multiplyEnds :: [Integer] -> Integer
multiplyEnds n = if null n
                   then 1
                   else (head n) * (last n)

-- Write a function dropLastTwo :: [Integer] -> [Integer] which returns all but the last two elements (assuming they exist)
dropLastTwo :: [Integer] -> [Integer]
dropLastTwo n = take (length n - 2) n

-- Write a function findEmpty :: [String] -> Bool that takes a list of strings, and determines whether it contains an empty string
-- Not sure if this should strictly contain an empty string, or if an empty list should also count? I went for the former.
findEmpty :: [String] -> Bool
findEmpty n = [] `elem` n

-- Write a function checkPalindrome :: String -> Bool that returns true if the string is the same when traversed in the opposite direction
checkPalindrome :: String -> Bool
checkPalindrome s = if s == reverse s
                      then True
                      else False

-- Write a function checkSize :: [Integer] -> Bool which takes a list of integers and returns True if the number of elements in the list is at least 3, and the first element in the list as at least 10
checkSize :: [Integer] -> Bool
checkSize n = if length n >= 3 && head n >= 10
                then True
                else False

-- Write a function checkAnySize :: Integer -> [Integer] -> Bool which works similar to the prev fn, but now takes additional param as 1st arg, and checks whether both the length of the list and the first element of the list are at least as big as this param
checkAnySize :: Integer -> [Integer] -> Bool
checkAnySize x n = if length n >= (fromIntegral x) && head n >= (fromIntegral x)
                     then True
                     else False

-- Begin Test File


tests =
  [ ((radius 1.5504042752057867 0.1910247067527331) == 1.5621279893678202)
  , ((radius (-4.102495835438859) (-4.4813723734754864)) == 6.075620991268505)
  , ((radius (-0.2874195656543531) 0.9592326672207974) == 1.0013677229591844)
  , ((radius 0.7979032854449627 (-1.1645625290045303)) == 1.4116853533579263)
  , ((radius 0.9537911833924358 (-1.628015570573809)) == 1.8868365905790327)
  , ((radius 2.198720449142465 3.2553169612023134) == 3.928289720905105)
  , ((radius (-0.3498279781305803) 1.6976275704284305) == 1.7332971419123917)
  , ((radius (-0.7125234572840466) 0.4852822210580831) == 0.8620838191585996)
  , ((radius 0.93671455094102 0.2383358011934226) == 0.9665599329969907)
  , ((radius 0.0 0.0) == 0.0)
  , ((sumEvens 17) == 72)
  , ((sumEvens 7) == 12)
  , ((sumEvens 12) == 42)
  , ((sumEvens 5) == 6)
  , ((sumEvens 10) == 30)
  , ((sumEvens 4) == 6)
  , ((sumEvens 9) == 20)
  , ((sumEvens 8) == 20)
  , ((sumEvens 2) == 2)
  , ((sumEvens 3) == 2)
  , ((multiplyEnds [1,2,3]) == 3)
  , ((multiplyEnds []) == 1)
  , ((multiplyEnds [1..10]) == 10)
  , ((multiplyEnds [-5..5]) == -25)
  , ((multiplyEnds [-10,-8..25]) == -240)
  , ((multiplyEnds [2,5..17]) == 34)
  , ((multiplyEnds [100..1000]) == 100000)
  , ((multiplyEnds [10,100,1000]) == 10000)
  , ((multiplyEnds [5,25..200]) == 925)
  , ((multiplyEnds [0,1,2]) == 0)
  , ((dropLastTwo [1,2,3]) == [1])
  , ((dropLastTwo [1..10]) == [1,2,3,4,5,6,7,8])
  , ((dropLastTwo [-5..5]) == [-5,-4,-3,-2,-1,0,1,2,3])
  , ((dropLastTwo [-10,-8..25]) == [-10,-8,-6,-4,-2,0,2,4,6,8,10,12,14,16,18,20])
  , ((dropLastTwo [100..200]) == [100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198])
  , ((dropLastTwo [10,100,1000]) == [10])
  , ((dropLastTwo [5,25..200]) == [5,25,45,65,85,105,125,145])
  , ((dropLastTwo [2,5..17]) == [2,5,8,11])
  , ((dropLastTwo [1,1,1,1,1]) == [1,1,1])
  , ((dropLastTwo [0,1,-1,2,-2]) == [0,1,-1])
  , ((findEmpty ["6\SI'\DC3\1027161!=x","\vs+\160448\ETB\1061280z\"Q\fy","2+\DC1",";\SOH","\ACK\EMo","\SOg\ENQ9Q-@\"6\166073\1039424+","0","`","U-{Hh\1034503\134220N\119597!5","p\CANP","\1010796R}Fg\157119\146458\SO\ENQ3","\151648\141286\EOT\40913","\f\143701\998821"]) == False)
  , ((findEmpty ["\DEL\126988","`\1012677\EM%\161304&\SYN|K","\1098753\170156\998760\1585vf\ETX\ENQ\RSX\ENQ9","P\DC2\1054987\DC2\DC3","\ETBL<\NULDy=-=\134315\68302B",")N\1041585J\1111037\148487","\1096260\STXEw'y\1000028KE[","","\120064\1027547g\FS6\38548","j\150259\1039269,\1035079Qv\SUBa\1064748\1066820v#","XD\1047693\71179\151066\1057128vE$\1030494","\GSc\EOT"]) == True)
  , ((findEmpty ["938\168488\54608\RS"]) == False)
  , ((findEmpty ["\v'0\RS@\ETX\50814","\23081","i\ETX.\CAN,\ESCfpU","","Ev\STX.|M\1034455","","Ey\1014596<we\1040114z\SI","\1113656\27538\11398%\r\"w","\37626\121064\&2\1077959\1022422rq"]) == True)
  , ((findEmpty ["","\SI\DLE0\1066638$%P","[\SUB","\ETXH\1042205","","\f\9679\&6\STX","wp","\174780,\\\SYN"]) == True)
  , ((findEmpty ["\n'\ENQ\57904~","",",\ETB&\DLE\DC4+/","\13092-","p;2\EM"]) == True)
  , ((findEmpty ["\ETX_\1017422","","","\179999","F|F"]) == True)
  , ((findEmpty ["\ESCE","0w\22159","\ACK`jv"]) == False)
  , ((findEmpty [""]) == True)
  , ((findEmpty []) == False)
  , ((checkPalindrome "hello") == False)
  , ((checkPalindrome "hi") == False)
  , ((checkPalindrome "heh") == True)
  , ((checkPalindrome "hahah") == True)
  , ((checkPalindrome "haha") == False)
  , ((checkPalindrome "racecar") == True)
  , ((checkPalindrome " o . o ") == True)
  , ((checkPalindrome "aha") == True)
  , ((checkPalindrome "12345654321") == True)
  , ((checkPalindrome "a long string") == False)
  , ((checkSize [-5..5]) == False)
  , ((checkSize [-10,-8..25]) == False)
  , ((checkSize [100..1000]) == True)
  , ((checkSize [10,100,1000]) == True)
  , ((checkSize [5,25..200]) == False)
  , ((checkSize [12,15..170]) == True)
  , ((checkSize [1,1,1,1,1]) == False)
  , ((checkSize [11,11,11,11,11]) == True)
  , ((checkSize [0,1,-1,2,-2]) == False)
  , ((checkSize [235,235]) == False)
  , ((checkAnySize 5 [-10,-8..25]) == False)
  , ((checkAnySize 20 [100..1000]) == True)
  , ((checkAnySize 200 [100..1000]) == False)
  , ((checkAnySize 3 [5,25..200]) == True)
  , ((checkAnySize 7 [5,25..200]) == False)
  , ((checkAnySize 30 [5,25..200]) == False)
  , ((checkAnySize 10 [12,15..170]) == True)
  , ((checkAnySize 20 [12,15..170]) == False)
  , ((checkAnySize 1 [1,1,1,1,1]) == True)
  , ((checkAnySize 11 [11,11,11,11,11]) == False)
  ]

testCount =
  putStrLn $ "Correct answers: " ++ show (length (filter id tests)) ++ '/' : show (length tests)

wrongAnswers = map fst $ filter (not . snd) (zip [1..] tests)
