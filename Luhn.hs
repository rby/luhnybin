module Main where

import Data.Char (ord)

isDigit c = c >= '0' && c <= '9'
accept c = isDigit c || c == ' ' || c == '-'

readDigit c = ord c - ord '0'



mask [] buff diglen masked = error "niy" -- reverse $  buff ++ masked
mask (x:xs) buff diglen masked | accept x =
    let buff' = x:buff
        (consumed, remains, diglen'') = process buff' diglen'
        diglen' = diglen + (if isDigit x then 1 else 0)
    in if diglen' == 16
        then mask xs remains diglen'' (consumed ++ masked)
        else mask xs buff' diglen' masked
                              | otherwise =
    if diglen >= 14 then mask xs remains diglen' $ x:consumed ++ masked
                    else mask xs [] 0            $ x:buff ++ masked
        where (consumed, remains, diglen') = process buff diglen

process buff diglen = error "niy"
process' escaped buff cnt sum | cnt >= 14 && sum `mod` 10 == 0 =
    (escaped, replaceDigits buff, cnt)
process' escaped (x:xs) cnt sum | cnt > 14 =
    process' (x:escaped) xs (cnt - 1) (sum - evalDigit x)
                              | otherwise = (escaped, x:xs, cnt)
evalDigit c | not $ isDigit c  = 0
            | otherwise        = if x >= 10 then s else x
                                    where x = 2 * readDigit c
                                          s = x `mod` 10 + x `div` 10
replaceDigits chars = rd chars ""
    where rd [] acc = reverse acc -- FIXME ?
          rd (x:xs) acc | isDigit x = rd xs $ 'X':acc
                        | otherwise = rd xs $ x:acc

sumAll = foldl1 (+) . map evalDigit
