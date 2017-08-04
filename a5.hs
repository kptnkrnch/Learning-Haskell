-- Question 1 -------------------------------------------------------------------------
snoc :: a -> [a] -> [a]
snoc val []  = [val]
snoc val lst = (head lst) : snoc val (tail lst)

-- Question 2 -------------------------------------------------------------------------
myappend :: [a] -> [a] -> [a]
myappend [] []     = []
myappend lst1 []   = lst1
myappend [] lst2   = lst2
myappend lst1 lst2 = (head lst1) : myappend (tail lst1) lst2

-- Question 3 -------------------------------------------------------------------------
myreverse :: [a] -> [a]
myreverse []  = []
myreverse lst = last lst : myreverse (init lst)

-- Question 4 -------------------------------------------------------------------------
count_emirps :: Int -> Int
count_emirps n | n < 13    = 0
			   | otherwise = if ((is_prime n) 
			   					&& (is_prime (digitsToInt (myreverse (intToDigits n)))) 
			   					&& (n /= (digitsToInt (myreverse (intToDigits n)))))
			   				 then (1 + (count_emirps (n - 1)))
			   				 else (count_emirps (n - 1))
-- helpers
smallest_divisor :: Int -> Int
smallest_divisor n | n < 0     = error "n must be >= 0"
				   | n == 0    = 0
				   | n == 1    = 1
				   | otherwise = head (dropWhile (\x -> n `mod` x /= 0) [2..n])

is_prime :: Int -> Bool
is_prime n | n <= 1    = False
		   | otherwise = (smallest_divisor n == n)


intToDigits :: Int -> [Int]
intToDigits num | num == 0  = []
				| otherwise = snoc (num `mod` 10) (intToDigits (num `div` 10))

digitsToInt :: [Int] -> Int
digitsToInt digits | digits == [] = 0
				   | otherwise    = 10 * digitsToInt(init digits) + last digits

-- Question 5 -------------------------------------------------------------------------
biggest_sum :: [[Int]] -> [Int]
biggest_sum intList | intList == [] = []
					| otherwise     = if (sumGreaterThanRest (head intList) (tail intList))
									  then (head intList)
									  else (biggest_sum (tail intList))

sumGreaterThanRest :: [Int] -> [[Int]] -> Bool
sumGreaterThanRest lst rest | lst == []                       = error "lst cannot be an empty list"
						 	| rest == []                      = True
						 	| (sum lst) <= (sum (head rest))  = False
						 	| otherwise                       = sumGreaterThanRest lst (tail rest)

-- Question 6 -------------------------------------------------------------------------
greatest :: (a -> Int) -> [a] -> a
greatest f lst = if (greaterThanRest f (head lst) (tail lst))
			   	 then (head lst)
			   	 else (greatest f (tail lst))

greaterThanRest :: (a -> Int) -> a -> [a] -> Bool
greaterThanRest f val lst | mylength(lst) == 0          = True
						  | (f val) <= (f (head lst))   = False
						  | otherwise                   = greaterThanRest f val (tail lst) 

-- helpers
mylength :: [a] -> Int
mylength []  = 0
mylength lst = 1 + mylength (tail lst)

-- Question 7 -------------------------------------------------------------------------
is_bit :: Int -> Bool
is_bit 0 = True
is_bit 1 = True
is_bit _ = False

-- Question 8 -------------------------------------------------------------------------
flip_bit :: Int -> Int
flip_bit 0 = 1
flip_bit 1 = 0
flip_bit _ = error "x was not a bit; x must be either a 0 or a 1"

-- Question 9 -------------------------------------------------------------------------
is_bit_seq1 :: [Int] -> Bool
is_bit_seq1 lst | lst == []         = True
				| is_bit (head lst) = is_bit_seq1 (tail lst)
				| otherwise         = False

is_bit_seq2 :: [Int] -> Bool
is_bit_seq2 []  = True
is_bit_seq2 lst = if (is_bit (head lst))
				  then is_bit_seq2 (tail lst)
				  else False

is_bit_seq3 :: [Int] -> Bool
is_bit_seq3 []  = True
is_bit_seq3 lst = all is_bit lst

-- Question 10 ------------------------------------------------------------------------
invert_bits1 :: [Int] -> [Int]
invert_bits1 x | x == []   = []
			   | otherwise = flip_bit (head x) : invert_bits1 (tail x)

invert_bits2 :: [Int] -> [Int]
invert_bits2 x = map flip_bit x

invert_bits3 :: [Int] -> [Int]
invert_bits3 x = [flip_bit n | n <- x]

-- Question 11 ------------------------------------------------------------------------
bit_count :: [Int] -> (Int, Int)
bit_count x | x == []   = (0, 0)
			| otherwise = (count_zeros x, count_ones x)

count_ones :: [Int] -> Int
count_ones lst | lst == []     = 0
			   | head lst == 1 = 1 + count_ones (tail lst)
			   | otherwise     = count_ones (tail lst)

count_zeros :: [Int] -> Int
count_zeros lst | lst == []     = 0
			    | head lst == 0 = 1 + count_zeros (tail lst)
			    | otherwise     = count_zeros (tail lst)

-- Question 12 ------------------------------------------------------------------------
all_basic_bit_seqs :: Int -> [[Int]]
all_basic_bit_seqs n | n < 1     = []
					 | otherwise = (generate_bit_seqs (init_bit_seq n) [])

-- helpers
generate_bit_seqs :: [Int] -> [[Int]] -> [[Int]]
generate_bit_seqs curseq sequences | is_end_bit_seq curseq = (myreverse curseq) : sequences
								   | otherwise             = (myreverse curseq) : (generate_bit_seqs (next_bit_seq curseq) sequences)

-- returns the next bit sequence (bit sequence + 1) (Note: returned sequence is in reverse)
next_bit_seq :: [Int] -> [Int]
next_bit_seq seq | seq == [] = []
				 | otherwise = next_bit_seq_generator seq False

-- creates the initial bit sequence (all zeros)
init_bit_seq :: Int -> [Int]
init_bit_seq bitsremaining | bitsremaining == 0 = []
						   | otherwise          = 0 : (init_bit_seq (bitsremaining - 1))

-- calcuates the next sequence of bits based on the previous sequence
next_bit_seq_generator :: [Int] -> Bool -> [Int]
next_bit_seq_generator seq found | seq == []                           = []
								 | ((head seq) == 0 && found == False) = 1 : (next_bit_seq_generator (tail seq) True)
								 | found == True                       = (head seq) : (next_bit_seq_generator (tail seq) found)
								 | otherwise                           = 0 : (next_bit_seq_generator (tail seq) found)

-- checks if the sequence is all 1's, which denotes the end of the bit sequence generation
is_end_bit_seq :: [Int] -> Bool
is_end_bit_seq seq | seq == []       = True
				   | (head seq) == 1 = (is_end_bit_seq (tail seq))
				   | otherwise       = False

-- Question 13 ------------------------------------------------------------------------
data Bit = Zero | One
	deriving (Show, Eq)

flipBit :: Bit -> Bit
flipBit Zero = One
flipBit One  = Zero

-- Question 14 ------------------------------------------------------------------------
invert :: [Bit] -> [Bit]
invert bits = map flipBit bits

-- Question 15 ------------------------------------------------------------------------
all_bit_seqs :: Int -> [[Bit]]
all_bit_seqs n | n < 1     = []
			   | otherwise = map (padList n) (map listToBit (map listToBinaryInt (map generator [0..z])))
			     where z = (2^n) - 1

-- converts a list of even and odd integers into a list of 1's (for odd integers) and 0's (for even integers)
listToBinaryInt :: [Int] -> [Int]
listToBinaryInt [] = []
listToBinaryInt lst = map (\x -> x `mod` 2) lst

-- converts a list of integers consisting of 1's and 0's into the Bit type
listToBit :: [Int] -> [Bit]
listToBit [] = []
listToBit lst = map intToBit lst

-- converts an integer (either 1 or 0) into its corresponding Bit value
intToBit :: Int -> Bit
intToBit 1 = One
intToBit 0 = Zero
intToBit _ = error "input must be either 0 or 1"

-- adds Zero's to the front of the list until the list is the desired size
padList :: Int -> [Bit] -> [Bit]
padList size lst | size <= 0 = lst
				 | otherwise = temp ++ lst
							   where temp = take (size - (length lst)) (repeat Zero)

-- from a starting positive number num, the generator creates a list of even and odd integers by repeatedly
-- performing integer division on the last (tail) number of the list until the last number is a 1
-- by taking the modulo 2 of the integers in the list, you will get the binary representation of the 
-- initial number num
generator :: Int -> [Int]
generator num = reverse (takeWhile (>= 1) lst) 
				where lst = num : [x `div` 2 | x <- lst]

-- Question 16 ------------------------------------------------------------------------
bitSum1 :: [Bit] -> Int
bitSum1 [] = 0
bitSum1 bits = foldr (bitAdd) 0 bits

-- helpers
bitAdd :: Bit -> Int -> Int
bitAdd One x   = x + 1
bitAdd Zero x  = x

-- Question 17 ------------------------------------------------------------------------
bitSum2 :: [Maybe Bit] -> Int
bitSum2 [] = 0
bitSum2 maybebits = foldr (maybeBitAdd) 0 maybebits

-- helpers
maybeBitAdd :: Maybe Bit -> Int -> Int
maybeBitAdd Nothing x     = x
maybeBitAdd (Just One) x  = x + 1
maybeBitAdd (Just Zero) x = x

-- Question 18 ------------------------------------------------------------------------
data List a = Empty | Cons a (List a)
    deriving Show

toList :: [a] -> List a
toList []  = Empty
toList lst = Cons (head lst) (toList (tail lst))

-- Question 19 ------------------------------------------------------------------------
toHaskellList :: List a -> [a]
toHaskellList Empty = []
toHaskellList (Cons x rest) = x : (toHaskellList rest)

-- Question 20 ------------------------------------------------------------------------
append :: List a -> List a -> List a
append Empty Empty = Empty
append Empty (Cons y yrest) = (Cons y yrest)
append (Cons x xrest) Empty = (Cons x xrest)
append (Cons x xrest) (Cons y yrest) = (Cons x (append xrest (Cons y yrest)))

-- Question 21 ------------------------------------------------------------------------
removeAll :: (a -> Bool) -> List a -> List a
removeAll f Empty                          = Empty
removeAll f (Cons x xrest) | f (x) == True = removeAll f xrest
						   | otherwise     = (Cons x (removeAll f xrest))

-- Question 22 ------------------------------------------------------------------------
sort :: Ord a => List a -> List a
sort Empty = Empty
sort (Cons x xrest) | listLength (Cons x xrest) > 2 = merge (sort a) (sort b)
					| otherwise                     = merge (Cons x Empty) xrest
					where (a, b) = split (Cons x xrest)

-- Merges two sorted lists into one larger sorted list
merge :: Ord a => List a -> List a -> List a
merge Empty Empty = Empty
merge (Cons x xrest) Empty = (Cons x xrest)
merge Empty (Cons y yrest) = (Cons y yrest)
merge (Cons x xrest) (Cons y yrest) | x < y     = (Cons x (merge xrest (Cons y yrest)))
									| otherwise = (Cons y (merge (Cons x xrest) yrest))

-- helpers
listLength :: List a -> Int
listLength Empty          = 0
listLength (Cons x xrest) = 1 + listLength xrest

-- Splits the list in half
split :: List a -> (List a, List a)
split Empty = (Empty, Empty)
split lst = ((splitHalfOne lst ((listLength lst) `div` 2)), (splitHalfTwo lst ((listLength lst) `div` 2)))

-- Returns the first part of the list, minus the last "splitLength" number of elements
splitHalfOne :: List a -> Int -> List a
splitHalfOne Empty _ = Empty
splitHalfOne (Cons x xrest) splitLength | listLength (xrest) == splitLength = (Cons x Empty)
								 		| otherwise                         = (Cons x (splitHalfOne xrest splitLength))
-- Returns the last "splitLength" number of elements of the list
splitHalfTwo :: List a -> Int -> List a
splitHalfTwo Empty _ = Empty
splitHalfTwo (Cons x xrest) splitLength | listLength (xrest) == splitLength = xrest
								 		| otherwise                         = splitHalfTwo xrest splitLength