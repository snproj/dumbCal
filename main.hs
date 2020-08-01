import Data.List
import Data.Char
import Data.Monoid
import System.IO

------------------------- DATA DEFINITIONS -------------------------
data Snum = Snum Int deriving (Eq, Ord)
instance Show Snum where
    show (Snum a) = show a
snum :: Int -> Snum
snum n
    | n > 0 || n < 10 = Snum n
    | otherwise = error "SUNRISE ERROR! Invalid Snum: snum function error"
snumList :: [Int] -> [Snum]
snumList [] = []
snumList (n:ns) = go_snumList [] (n:ns)
    where
        go_snumList acc [] = acc
        go_snumList acc (n:ns) = go_snumList (acc ++ [snum n]) ns
fromSnum :: Snum -> Int
fromSnum (Snum x) = x


data Subj = Subj Char deriving (Eq, Ord)
instance Enum Subj where
    toEnum i = Subj (toEnum i::Char)
    fromEnum (Subj a) = fromEnum a
instance Show Subj where
    show (Subj a) = show a
subj :: Char -> Subj
subj c
    | elem c ['A'..'J'] = Subj c
    | c == 'X' = Subj c
    | otherwise = error "SUNRISE ERROR! Invalid Subj: subj function error"
subjList :: [Char] -> [Subj]
subjList [] = []
-- subjList (c:cs) = subj c : subjList cs
subjList (c:cs) = go_subjList [] (c:cs)
    where
        go_subjList acc [] = acc
        go_subjList acc (c:cs) = go_subjList (acc ++ [subj c]) cs
fromSubj :: Subj -> Char
fromSubj (Subj x) = x


data InputStudent = InputStudent { getName :: [Char]
                                 , getSubjList :: [Subj]
                                 } deriving (Show, Eq)
instance Ord InputStudent where
    a `compare` b = getSubjList a `compare` getSubjList b


data Student = Student { getInputStudent :: InputStudent
                       , getSnum :: Snum
                       } deriving (Show, Eq)
instance Ord Student where
    a `compare` b = getInputStudent a `compare` getInputStudent b
getIntSubjList = getSubjList . getInputStudent


data PosInt = PosInt Int
instance Show PosInt where
    show (PosInt a) = show a
posInt n
    | n > 0 = PosInt n
    | otherwise = error "SUNRISE ERROR! Invalid PosInt: posInt function error"
instance Num PosInt where
    (+) (PosInt a) (PosInt b) = PosInt (a+b)
    (-) (PosInt a) (PosInt b) = PosInt (a-b)
    (*) (PosInt a) (PosInt b) = PosInt (a*b)
    negate (PosInt a) = PosInt (negate a)
    abs (PosInt a) = PosInt (abs a)
    signum (PosInt a) = case (signum a) of
        -1 -> PosInt (-1)
        0 -> PosInt 0
        1 -> PosInt 1
    fromInteger a = PosInt (fromInteger a)
    -- {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
instance Eq PosInt where
    (==) (PosInt a) (PosInt b) = (==) a b
    -- {-# MINIMAL (==) | (/=) #-}
fromPosInt :: PosInt -> Int
fromPosInt (PosInt x) = x


data Tamount = Tamount PosInt deriving (Show, Eq)
tamount :: Int -> Tamount
tamount n = Tamount (posInt n)
tamountList :: [Int] -> [Tamount]
tamountList [] = []
tamountList (n:ns) = go_tamountList [] (n:ns)
    where
        go_tamountList acc [] = acc
        go_tamountList acc (n:ns) = go_tamountList (acc ++ [tamount n]) ns
fromTamount :: Tamount -> Int
fromTamount (Tamount x) = fromPosInt x

data SubjMax = SubjMax PosInt deriving (Show, Eq)
subjMax :: Int -> SubjMax
subjMax n = SubjMax (posInt n)
subjMaxList :: [Int] -> [SubjMax]
subjMaxList [] = []
subjMaxList (n:ns) = go_subjMaxList [] (n:ns)
    where
        go_subjMaxList acc [] = acc
        go_subjMaxList acc (n:ns) = go_subjMaxList (acc ++ [subjMax n]) ns
fromSubjMax :: SubjMax -> Int
fromSubjMax (SubjMax x) = fromPosInt x

data Plist = Plist { getPlistSubj :: Subj
                   , getPlistTeacherTotal :: Tamount
                   , getPlistAcc :: [Snum]
                   }
instance Show Plist where
    show a = "PLIST: " ++ show (getPlistSubj a) ++ " | " ++ show (getPlistTeacherTotal a) ++ " | " ++ show (getPlistAcc a) ++ "\n"


data Timeslot = Timeslot { getTimeslotMax :: SubjMax
                         , getTimeslotAcc :: [Subj]
                         }
instance Show Timeslot where
    show a = "TIMESLOT: " ++ show (getTimeslotMax a) ++ " | " ++ show (getTimeslotAcc a) ++ "\n"
--------------------------------------------------------------------


------------------------- SORTING FUNCTIONS -------------------------
processInputStudents :: [InputStudent] -> [Student]
processInputStudents xs = giveSnumbers sortedList
    where
        sortedList = sort xs
        giveSnumbers (y:ys) = go_giveSnumbers 1 [] (y:ys)
            where
                go_giveSnumbers _ acc [] = acc
                -- go_giveSnumbers n (y:ys) = Student {getInputStudent = y, getSnum = snum n} : go_giveSnumbers (n+1) ys
                go_giveSnumbers n acc (y:ys) = go_giveSnumbers (n+1) (acc ++ [Student {getInputStudent = y, getSnum = snum n}]) ys
---------------------------------------------------------------------


------------------------- RAW DATA -------------------------
main_maxPerStudent = 7
subjectsList = subjList "ABCDEFGHIJ"
teacherAmountsList = tamountList [2,1,3,3,2,3,3,3,3,2]
main_stList = zip subjectsList teacherAmountsList

inputStudent1 = InputStudent {getName = "Pierre", getSubjList = subjList "ABCDEFG"}
inputStudent2 = InputStudent {getName = "Mark", getSubjList = subjList "ACDEGHI"}
inputStudent3 = InputStudent {getName = "Greta", getSubjList = subjList "ACDFGHI"}
inputStudent4 = InputStudent {getName = "Belle", getSubjList = subjList "ACEGHIJ"}
inputStudent5 = InputStudent {getName = "Poe", getSubjList = subjList "ADEFHIJ"}
inputStudent6 = InputStudent {getName = "Liam", getSubjList = subjList "BCDFGHI"}
inputStudent7 = InputStudent {getName = "Jessica", getSubjList = subjList "BCFGHIJ"}
inputStudent8 = InputStudent {getName = "Gary", getSubjList = subjList "BDFGHIJ"}
inputStudent9 = InputStudent {getName = "Charles", getSubjList = subjList "CDEFGHI"}
inputStudent10 = InputStudent {getName = "Hannah", getSubjList = subjList "CEFGHIJ"}

inputStudentList =  [
    inputStudent5,
    inputStudent2,
    inputStudent4,
    inputStudent3,
    inputStudent1,
    inputStudent9,
    inputStudent8,
    inputStudent10,
    inputStudent6,
    inputStudent7   ]

numOfStudents = length inputStudentList
------------------------------------------------------------


------------------------- MAKE LONG STRING -------------------------
-- maxLength = length (min (map getIntSubjList list))

makeLongString :: [Student] -> [(Snum, Subj)]
-- go_makeLongString :: [Subj] -> Int -> [Student] -> [[Subj]]
makeLongString list = go_makeLongString [] 0 list
    where
        go_makeLongString acc i list = if i == minimum (map length (map getIntSubjList list))
            then acc
            else go_makeLongString (acc ++ result (i) list) (i+1) list
                where
                    result :: Int -> [Student] -> [(Snum, Subj)]
                    result i list = zip (map getSnum list) (map (!!i) (map getIntSubjList list))


{-
makeLongString :: [Student] -> [Subj]
-- go_makeLongString :: [Subj] -> Int -> [Student] -> [[Subj]]
makeLongString list = go_makeLongString (subjList []) 0 list
    where
        go_makeLongString acc i list = if i == minimum (map length (map getIntSubjList list))
            then acc
            else go_makeLongString (acc ++ result (i) list) (i+1) list
                where
                    result :: Int -> [Student] -> [Subj]
                    result i list = map (!!i) (map getIntSubjList list)
-}
{-
makeLongString :: [Student] -> [Subj]
-- makeLongString [] = []
makeLongString (x:xs) = go_makeLongString (subjList "") (x:xs)
    where
        go_makeLongString acc [] = acc
        go_makeLongString acc (x:xs) = go_makeLongString (acc ++ getSubjList (getInputStudent x)) xs
-}
--------------------------------------------------------------------


------------------------- PLIST FUNCTIONS -------------------------
initPlist :: Subj -> Tamount -> [(Snum, Subj)] -> Plist
initPlist subject tamt longString = Plist { getPlistSubj = subject
                                          , getPlistTeacherTotal = tamt
                                          , getPlistAcc = [ fst longStringElem | longStringElem <- longString, snd longStringElem == subject ]
                                          }

initAllPlists :: [(Subj, Tamount)] -> [(Snum, Subj)] -> [Plist]
initAllPlists stList longString = [ initPlist (fst stListElem) (snd stListElem) longString | stListElem <- stList ]

transformPlist :: Timeslot -> Plist -> (Timeslot, Plist)
transformPlist timeslot plist = go_transformPlist (length (getPlistAcc plist)) (fromTamount (getPlistTeacherTotal plist)) timeslot plist -- CHECK TIMEOUT OR TIMEOUT - 1 IF THINGS GO WRONG

go_transformPlist timeout countdown timeslot plist = if isFull timeslot || countdown == 0 || timeout == 0
                            then (timeslot, plist)
                            else if currYes timeslot plist
                                then go_transformPlist (timeout-1) (countdown-1) (addToTimeslot timeslot (head (getPlistAcc plist)) (getPlistSubj plist)) (truncPlist plist)
                                else go_transformPlist (timeout-1) countdown timeslot (shiftPlist plist)
truncPlist plist = Plist { getPlistSubj = getPlistSubj plist
                                 , getPlistTeacherTotal = getPlistTeacherTotal plist
                                 , getPlistAcc = tail (getPlistAcc plist)
                                 }
shiftPlist plist = Plist { getPlistSubj = getPlistSubj plist
                                 , getPlistTeacherTotal = getPlistTeacherTotal plist
                                 , getPlistAcc = (tail (getPlistAcc plist)) ++ [head (getPlistAcc plist)]
                                 }

transformAllPlists :: Timeslot -> [Plist] -> (Timeslot, [Plist])
transformAllPlists timeslot plists = go_transformAllPlists [] timeslot plists

go_transformAllPlists acc_plist timeslot [] = (timeslot, acc_plist)
go_transformAllPlists acc_plist timeslot (p:ps) = go_transformAllPlists (acc_plist ++ [(snd (transformPlist timeslot p))]) (fst (transformPlist timeslot p)) ps

makeAllTimeslots :: [Timeslot] -> [Plist] -> [Timeslot]
makeAllTimeslots timeslots plists = go_makeAllTimeslots [] timeslots plists

go_makeAllTimeslots acc_timeslot (t:ts) plists =  if (length(getPlistAcc(last plists)) == 0)
    then acc_timeslot
    else go_makeAllTimeslots (acc_timeslot ++ [fst (transformAllPlists t plists)]) ts (snd (transformAllPlists t plists))



{- WITH INDEX BUT POSSIBLY USELESS
transformPlist :: Timeslot -> Plist -> (Timeslot, Plist)
transformPlist timeslot plist = go_transformPlist (fromTamount (getPlistTeacherTotal plist)) 0 timeslot plist

go_transformPlist countdown index timeslot plist = if (isFull timeslot || countdown == 0)
            then (timeslot, plist)
            else if currYes timeslot plist
                then go_transformPlist (countdown-1) (index+1) (addToTimeslot timeslot (head (getPlistAcc plist)) (getPlistSubj plist)) (truncPlist plist)
                else go_transformPlist countdown (index+1) timeslot (shiftPlist plist)
truncPlist plist = Plist { getPlistSubj = getPlistSubj plist
                                 , getPlistTeacherTotal = getPlistTeacherTotal plist
                                 , getPlistAcc = tail (getPlistAcc plist)
                                 }
shiftPlist plist = Plist { getPlistSubj = getPlistSubj plist
                                 , getPlistTeacherTotal = getPlistTeacherTotal plist
                                 , getPlistAcc = (tail (getPlistAcc plist)) ++ [head (getPlistAcc plist)]
                                 }
-}






-------------------------------------------------------------------
{-
then go_transformPlist (countdown-1) (index+1) (addToTimeslot timeslot index ((getPlistAcc plist) !! index)) (truncPlist plist)
                        where
                            truncPlist plist = Plist { getPlistSubj = getPlistSubj plist
                                                    , getPlistTeacherTotal = getPlistTeacherTotal plist
                                                    , getPlistAcc = tail (getPlistAcc plist)
                                                    }
-}


------------------------- TIMESLOT FUNCTIONS -------------------------
initTimeslot = Timeslot { getTimeslotMax = subjMax numOfStudents
                        , getTimeslotAcc = subjList "XXXXXXXXXX"
                        }

medTimeslot = Timeslot { getTimeslotMax = subjMax numOfStudents
                        , getTimeslotAcc = subjList "AXCXEXGXIX"
                        }

finTimeslot = Timeslot { getTimeslotMax = subjMax numOfStudents
                        , getTimeslotAcc = subjList "ABCDEFGHIJ"
                        }

initEmptyTimeslotList :: Int -> Timeslot -> [Timeslot]
initEmptyTimeslotList n timeslot = go_initEmptyTimeslotList [] timeslot n

go_initEmptyTimeslotList acc_initTimeslot timeslot 0 = acc_initTimeslot
go_initEmptyTimeslotList acc_initTimeslot timeslot n = go_initEmptyTimeslotList (acc_initTimeslot ++ [timeslot]) timeslot (n-1)

emptyTimeslotList = initEmptyTimeslotList 10 initTimeslot

{-
compInt_SubjMax :: Int -> SubjMax -> Bool
compInt_SubjMax int sm = (==) int (sm)
-}

isFull :: Timeslot -> Bool
isFull timeslot = length anyXs == 0
    where
        anyXs = [ x | x <- getTimeslotAcc timeslot, x == Subj 'X' ]


currYes :: Timeslot -> Plist -> Bool
currYes timeslot plist = ((getTimeslotAcc timeslot) !! (fromSnum (head (getPlistAcc plist)) - 1)) == subj 'X' -- CURRENT ISSUE 20200709_0704

takeR :: Int -> [a] -> [a]
takeR n = reverse . take n . reverse 

addToTimeslot :: Timeslot -> Snum -> Subj -> Timeslot
addToTimeslot timeslot snumber subject = Timeslot { getTimeslotMax = subjMax numOfStudents
                                                  , getTimeslotAcc = (take ((fromSnum snumber) - 1) (getTimeslotAcc timeslot)) ++ [indexCheck ((getTimeslotAcc timeslot) !! ((fromSnum snumber) - 1)) timeslot subject (fromSnum (snumber)-1)] ++ takeR (numOfStudents - (fromSnum snumber)) (getTimeslotAcc timeslot)
                                                  }
indexCheck orig timeslot subject index = if (getTimeslotAcc timeslot) !! index == Subj 'X'
                                                            then subject
                                                            else orig
-- getTimeslotAcc = [ indexCheck x timeslot subject (fromSnum (snumber)-1) | x <- getTimeslotAcc timeslot ]
----------------------------------------------------------------------

-- TEMP EXECUTION SECTION FOR DEBUGGING
list = processInputStudents inputStudentList
procString = makeLongString list
procPlists = initAllPlists main_stList procString

timeslot = initTimeslot
plist = head procPlists