-- Do not alter the following line
module Assignment1 (char_to_int, repeat_char, decode, int_to_char, length_char, drop_char, encode, complex_encode, complex_decode) where


-- Part A

char_to_int :: Char -> Integer
char_to_int '0' = 0
char_to_int '1' = 1
char_to_int '2' = 2
char_to_int '3' = 3
char_to_int '4' = 4
char_to_int '5' = 5
char_to_int '6' = 6
char_to_int '7' = 7
char_to_int '8' = 8
char_to_int '9' = 9


repeat_char :: Char -> Integer -> String
repeat_char s 0 = [] 
repeat_char s 1 = [s] 
repeat_char s r = s : (repeat_char s (r-1))


decode :: String -> String
decode [] = []
decode (x:y:xs) = (repeat_char x (char_to_int y)) ++ decode xs



-- Part B

int_to_char :: Integer -> Char
int_to_char 0 = '0'
int_to_char 1 = '1'
int_to_char 2 = '2'
int_to_char 3 = '3'
int_to_char 4 = '4'
int_to_char 5 = '5'
int_to_char 6 = '6'
int_to_char 7 = '7'
int_to_char 8 = '8'
int_to_char 9 = '9'


length_char :: Char -> String -> Integer
length_char n [] = 0
length_char n (x:xs) = if n == x then 1 + length_char n xs else 0


drop_char :: Char -> String -> String
drop_char n [] = []
drop_char n (x:xs) = if x == n then drop_char n xs else x:xs


encode :: String -> String
encode [] = []
encode (x:xs) = x : int_to_char (1 + length_char x xs) : encode (drop_char x xs)


-- Part C

splitNum 0 = []
splitNum x = splitNum (x `div` 10) ++ [x `mod` 10]

largeIntToChar [] = []
largeIntToChar (x:xs) = int_to_char x : largeIntToChar xs

complex_encode :: String -> String
complex_encode [] = []
complex_encode (x:xs) = 
    if length_char x xs >= 1 && length_char x xs <= 10 then  x : int_to_char (1 + length_char x xs) : complex_encode (drop_char x xs) 
    else if length_char x xs == 0  then x : complex_encode (drop_char x xs)
    else  x : largeIntToChar (splitNum(1 + length_char x xs)) ++ complex_encode (drop_char x xs)


complex_decode :: String -> String
complex_decode = error "Not implemented"


