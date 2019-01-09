import Data.Time.Clock
import Data.Time.Calendar
import System.IO
import Control.Monad (when)
import System.Environment

type Date = (Integer, Int, Int) -- (YYYY, MM, DD)
type Subject = String -- SubjectName
type StudyTime = (Int, Int) -- (Start, End)
data Study = Study Date Subject StudyTime deriving (Eq, Show, Read)

-- 1 subject studytime
-- recordInputOfToday subject studytime
-- 2 date subject studytime
-- recordInputOfDate date subject studytime
-- 3
-- printRecord getList
-- 4 date
-- printMoreDataOfDate date
-- 5 subject
-- printMoreDataOfSubject subject
-- 6 int
-- 6 deleteLineOfFile int
-- 7
-- flushFile

main :: IO ()
main = do
    args <- getArgs
    run args

run :: [String] -> IO ()
run [] = putStrLn "ERROR: Invalid Argument"
run (x:xs) =
    case x of
        "1" -> recordInputOfToday (xs!!0) (read $ xs!!1, read $ xs!!2)
        "2" -> recordInputOfDate (read $ xs!!0,read $ xs!!1,read $ xs!!2) (xs!!3) (read $ xs!!4, read $ xs!!5)
        "3" -> printRecord getList
        "4" -> printMoreDataOfDate (read $ xs!!0,read $ xs!!1,read $ xs!!2)
        "5" -> printMoreDataOfSubject (xs!!0)
        "6" -> deleteLineOfFile (read $ xs!!0)
        "7" -> flushFile
        _ -> putStrLn "ERROR: Invalid Argument"
        

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

deleteByIndex :: Int -> [a] -> [a]
deleteByIndex 0 (x:xs) = xs
deleteByIndex n (x:xs) = x:(deleteByIndex (n-1) xs)

dataFilePath :: String
dataFilePath = "data.txt"

date :: IO Date
date = getCurrentTime >>= return . toGregorian . utctDay

-- get subject and studytime as inputs and records it at txt file.
recordInputOfToday :: Subject -> StudyTime -> IO ()
recordInputOfToday subject studytime = do
    todayDate <- date
    isValidTime <- isValidStudyTime todayDate studytime
    if isValidTime 
        then do
            studyInfo <- return (Study todayDate subject studytime)
            appendFile dataFilePath $ (show studyInfo) ++ "\n"
            sortFile
        else putStrLn "Actaully, There is an overlap between data and input studytime.\nyou have to choose another time."

recordInputOfDate :: Date -> Subject -> StudyTime -> IO ()
recordInputOfDate date subject studytime = do
    isValidTime <- isValidStudyTime date studytime
    if isValidTime
        then (appendFile dataFilePath $ (show $ Study date subject studytime) ++ "\n") >> sortFile
        else putStrLn "Actaully, There is an overlap between data and input studytime.\nyou have to choose another time."
        

printRecord :: IO [Study] -> IO () -- print study data.
printRecord list = list >>= \x ->
     if x == [] 
        then putStrLn "There is no study record on that day!"
        else mapM_ (putStrLn . showInfo) x

-- check if given studytime is valid for data of given date.
isValidStudyTime :: Date -> StudyTime -> IO Bool
isValidStudyTime date studytime = do
    studyList <- getListByDate date
    let isOK (s2,e2) (s1,e1) = if s2 < e2 && e1 < s2 then True else False
    let studyTimeList = map getStudyTime studyList
    return $ and $ map (isOK studytime) studyTimeList
    
-- get all data to list from txt file.
getList :: IO [Study] 
getList = readFile dataFilePath >>= return . (map read) . lines

-- get filtered data by date.
getListByDate :: Date -> IO [Study]
getListByDate date = getList >>= return . (filter (\x -> date == getDate x))

-- get filtered data by subject.
getListBySubject :: Subject -> IO [Study] 
getListBySubject subject = getList >>= return . (filter (\x -> subject == getSubject x))

getDate :: Study -> Date
getDate (Study date _ _) = date

getSubject :: Study -> Subject
getSubject (Study _ subject _) = subject

getStudyTime :: Study -> StudyTime
getStudyTime (Study _ _ studytime) = studytime

showDate :: Date -> String
showDate (y, m, d) = show y ++ "/" ++ show m ++ "/" ++ show d

showTime :: StudyTime -> String
showTime (s, e) = "from " ++ show s ++ " to " ++ show e

showInfo :: Study -> String -- transform study data to a readable string.
showInfo (Study date subject studytime) =
    showDate date ++ ", " ++ "Studied <" ++
    subject ++ "> " ++ showTime studytime

-- show more detailed data of given date.
printMoreDataOfDate :: Date -> IO ()
printMoreDataOfDate date = do
    dataList <- getListByDate date
    let totalStudyTime = foldl (+) 0 $ map ((-) <$> snd <*> fst) $ map getStudyTime dataList
    putStrLn $ "Subjects you studied on " ++ showDate date ++ " :"
    printRecord (return dataList)
    when (totalStudyTime /= 0) $
        putStrLn $ "You studied " ++ show totalStudyTime ++ " hours on " ++ showDate date ++ "."

-- show more detailed data of given subject.
printMoreDataOfSubject :: Subject -> IO ()
printMoreDataOfSubject subject = do
    dataList <- getListBySubject subject
    let totalStudyTime = foldl (+) 0 $ map ((-) <$> snd <*> fst) $ map getStudyTime dataList
    putStrLn $ "The date you have studied <" ++ subject ++ "> :"
    printRecord (return dataList)
    when (totalStudyTime /= 0) $
        putStrLn $ "You have studied <" ++ subject ++ "> " ++ show totalStudyTime ++ " hours."

-- sort data by date.
sortByDate :: [Study] -> [Study]
sortByDate [] = []
sortByDate (study:xs) =
    filter (\x -> not (isFuture study x)) (sortByDate xs) ++ [study] ++ filter (\x -> isFuture study x) (sortByDate xs)
    where isFuture past future =
            let (y1, m1, d1) = getDate past
                (y2, m2, d2) = getDate future in
                    if y1 < y2 then True else
                    if y1 > y2 then False else
                    if m1 < m2 then True else
                    if m1 > m2 then False else
                    if d1 <= d2 then True else False

-- sort data file.
sortFile :: IO ()
sortFile = do
    sortedList <- getList >>= return . sortByDate
    when (length sortedList > 0) $ do
        flushFile
        mapM_ (appendFile dataFilePath) $ map (\x -> show x ++ "\n") sortedList

flushFile :: IO ()
flushFile = writeFile dataFilePath ""

-- delete line of file by it's line number.
deleteLineOfFile :: Int -> IO ()
deleteLineOfFile n = do
    studyList <- getList
    if (length studyList >= n) 
        then do
            let deletedList = deleteByIndex (n-1) studyList
            flushFile
            mapM_ (appendFile dataFilePath) $ map (\x -> show x ++ "\n") deletedList
        else putStrLn $ "ERROR: There in no line " ++ show (length studyList) ++ "."
