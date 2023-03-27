-- Do not alter the following line
module Assignment2 (transaction_to_string, trade_report_list, stock_test, get_trades, trade_report, update_money, profit, profit_report, complex_profit_report) where

import Data.List
type Transaction = (Char, Int, Int, String, Int) 

test_log :: [Transaction]
test_log = [('B', 100, 1104,  "VTI",  1),
            ('B', 200,   36, "ONEQ",  3),
            ('B',  50, 1223,  "VTI",  5),
            ('S', 150, 1240,  "VTI",  9),
            ('B', 100,  229, "IWRD", 10),
            ('S', 200,   32, "ONEQ", 11), 
            ('S', 100,  210, "IWRD", 12)
            ]


-- Part A

frst (a,_,_,_,_) = a
scnd (_,a,_,_,_) = a 
thrd (_,_,a,_,_) = a
frth (_,_,_,a,_) = a
ffth (_,_,_,_,a) = a

transaction_to_string :: Transaction -> String
transaction_to_string x = 
    if frst(x) == 'B' then
        "Bought " ++ show (scnd(x)) ++ " units of " ++ frth(x) ++ " for " ++ show (thrd(x)) ++ " pounds each on day " ++ show (ffth(x))
    else 
        "Sold " ++ show (scnd(x)) ++ " units of " ++ frth(x) ++ " for " ++ show (thrd(x)) ++ " pounds each on day " ++ show (ffth(x))


trade_report_list :: [Transaction] -> [String]
trade_report_list x = map transaction_to_string x 


stock_test :: String -> Transaction -> Bool
stock_test x y = 
    if x == frth(y) then True else False 


get_trades :: String -> [Transaction] -> [Transaction]
get_trades x yss = filter (\ys -> frth(ys) == x) yss


trade_report :: String -> [Transaction] -> String
trade_report x y = unlines (trade_report_list(get_trades x y))



-- Part B


update_money :: Transaction -> Int -> Int
update_money x y = 
    if frst(x) == 'B' then 
        y - (scnd(x) * thrd(x))
    else
        y + (scnd(x) * thrd(x))


profit :: [Transaction] -> String -> Int
profit [] _ = 0
profit (x:xs) y = 
    if frst(x) == 'B' && frth(x) == y then 
        - (foldr (*) 1 [scnd(x),thrd(x)]) + profit xs y
    else if frst(x) == 'S' && frth(x) == y then 
        foldr (*) 1 [scnd(x),thrd(x)] + profit xs y
    else 
        profit xs y


profit_report :: [String] -> [Transaction] -> String
profit_report [] _ = []
profit_report (x:xs) y =  x ++ ": " ++ show (profit y x ) ++ "\n" ++ (profit_report xs y)




-- Part C


test_str_log = "BUY 100 VTI 1\nBUY 200 ONEQ 3\nBUY 50 VTI 5\nSELL 150 VTI 9\nBUY 100 IWRD 10\nSELL 200 ONEQ 11\nSELL 100 IWRD 12\n"



type Prices = [(String, [Int])]

test_prices :: Prices
test_prices = [
                ("VTI", [1689, 1785, 1772, 1765, 1739, 1725, 1615, 1683, 1655, 1725, 1703, 1726, 1725, 1742, 1707, 1688, 1697, 1688, 1675]),
                ("ONEQ", [201, 203, 199, 199, 193, 189, 189, 183, 185, 190, 186, 182, 186, 182, 182, 186, 183, 179, 178]),
                ("IWRD", [207, 211, 213, 221, 221, 222, 221, 218, 226, 234, 229, 229, 228, 222, 218, 223, 222, 218, 214])
              ]

complex_profit_report :: String -> Prices -> String
complex_profit_report "" _ = ""
complex_profit_report strings y =
    unlines(reverse(check_report(convert_to_list(build_report strings y))))


convert_to_list :: String -> [String]
convert_to_list report = reverse(lines report)

check_report :: [String] -> [String]
check_report [] = []
check_report (h:t) = 
    if isInfixOf ((words h) !! 0) (unlines(t)) then 
        check_report t
    else
        h : check_report t 
        
build_report :: String -> Prices -> String
build_report "" _ = ""            
build_report strings y = 
    let a = takeWhile (/= '\n') strings
        b = tail (lines strings)
    in 
        if length b /= 1  then
            (words a) !! 2 ++ ": " ++ show (complex_profit strings y ((words a) !! 2)) ++ "\n" ++ (complex_profit_report (unlines b) y)
        else 
            (words a) !! 2 ++ ": " ++ show (complex_profit strings y ((words a) !! 2)) ++ "\n" ++ (complex_profit_report "" y)

complex_profit "" _ _ = 0
complex_profit strings y currStock =
    if length(lines strings) /= 1 then
        let t = takeWhile (/= '\n') strings 
            u = tail (lines strings)
        in  
            if (words t) !! 0 == "BUY" && (words t) !! 2 == currStock then
                - (find_prices ((words t) !! 2) ((words t) !! 3) y) * read ((words t) !! 1) + (complex_profit (unlines u) y currStock)
            else if words t !! 0 == "SELL" && words t !! 2 == currStock then
                find_prices ((words t) !! 2) ((words t) !! 3) y * read ((words t) !! 1) + (complex_profit (unlines u) y currStock)
            else 
                complex_profit (unlines u) y currStock
    else
        let t = takeWhile (/= '\n') strings 
            u = ""
        in
            if (words t) !! 0 == "BUY" && (words t) !! 2 == currStock then
                - (find_prices ((words t) !! 2) ((words t) !! 3) y) * read ((words t) !! 1) + (complex_profit u y currStock)
            else if words t !! 0 == "SELL" && words t !! 2 == currStock then
                find_prices ((words t) !! 2) ((words t) !! 3) y * read ((words t) !! 1) + (complex_profit u y currStock)
            else 
                complex_profit u y currStock

find_prices :: String -> String -> Prices -> Int
find_prices name day [] = 0
find_prices name day (x:xs) = 
    if fst x == name then
        snd x !! (read day - 1)
    else 
        find_prices name day xs
        

