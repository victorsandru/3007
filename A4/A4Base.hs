module A4Base
where

-- import           Data.List.NonEmpty (NonEmpty (..))
-- import           Data.Text          (Text)
-- import           Data.Validation                     -- package: validation
-- import           Hedgehog                            -- package: hedgehog
-- import qualified Hedgehog.Gen       as Gen
-- import qualified Hedgehog.Range     as Range

-- Imports for generating sample data -- can be ignored
-- import System.Random  
-- import Data.List
-- import Control.Applicative

-- Names of people (profs and students)
type Name = String

-- Course names
type Course = String

-- A rating by a student of a course they attend.
data Rating = Rating 
    { ratingStudent :: Name    -- the student giving the rating
    , ratingProf :: Name       -- the prof teaching the course
    , ratingCourse :: Course   -- the course identifier (a string) 
    , ratingScore :: Int       -- the student's score for the course, on
                               -- a 0-50 scale.
    }
    deriving (Show, Eq)

-- A more compact presentation of a rating. Run `map ppRating ratings` to see
-- the test data this way.
ppRating :: Rating -> String
ppRating (Rating student prof course score) =
    student ++ " gives " ++ show score ++ " to " ++ course ++ "[" ++ prof ++ "]"

-- Run `ppRatings ratings" in ghci to see a pretty-printed presentation of the
-- test data.
ppRatings :: [Rating] -> IO ()
ppRatings rs = do
    mapM (putStrLn . ppRating) rs
    return ()

-- IMPORTANT NOTE: the constructor Rating is not made available in the
-- assignment file, so you will not be able to construct a new value using it.
-- However, you can make a new value from an existing one use Haskell's "record"
-- syntax for data types that are defined using "{/}". E.g. if r is a value of type
-- Rating, then
--    r { ratingProf = "Howe" }
-- is a new record that is the same as r except that the "ratingProf" field is
-- "Howe".










-------------------------------------------------------------------------------
-- You can ignore the stuff below. It's just for generating sample data.
-- All you need from it is the value named "ratings" which is a large sample
-- list of ratings.
-- ----------------------------------------------------------------------------

-- rando :: Int -> IO Int
-- rando n = do
--     seed <- newStdGen
--     return $ abs . flip rem n . fst . random $ seed 

-- randos :: Int -> Int -> IO [Int]
-- randos n m =
--     mapM id (replicate m (rando n))

-- genRatings :: IO [Rating]
-- genRatings = do
--     let (courses, profs) = unzip courseProfs
--     let nc = length courses
--     let ns = length students
--     scores <- randos 50 (nc * ns)
--     return $
--         zipWith4 Rating 
--             (concatMap (replicate nc) students)
--             (concat (replicate ns profs))
--             (concat (replicate ns courses))
--             scores

students :: [Name]
students =
    [ "Jane Barnett"
    , "John Bigboote"
    , "Jane Camp"
    , "John Careful Walker"
    , "Jane Chief Crier"
    , "John Cooper"
    , "Jane Coyote"
    , "John Edwards"
    , "Jane Fat Eating"
    , "John Fish"
    , "Jane Fledgling"
    , "John Gomez"
    , "Jane Grim"
    , "John Guardian"
    , "Jane Icicle Boy"
    , "John Jones"
    , "Jane Joseph"
    , "John Kim Chi"
    , "Jane Lee"
    , "John Littlejohn"
    , "Jane Many Jars"
    , "John Milton"
    , "Jane Mud Head"
    , "John Nephew"
    , "Jane Nolan"
    , "John O'Connor"
    , "Jane Omar"
    , "John Parrot"
    , "Jane Rajeesh"
    , "John Ready to Fly"
    , "Jane Repeat Dance"
    , "John Roberts"
    , "Jane Scott"
    , "John Shaw"
    , "Jane Smallberries"
    , "John Starbird"
    , "Jane Take Cover"
    , "John Thorny Stick"
    , "Jane Turk"
    , "John Whorfin"
    ]

courseProfs :: [(Course,Name)] 
courseProfs =
    [ ("cs100", "Jane Wood")
    , ("cs101", "Jane Wood")
    , ("cs102", "John Write")
    , ("cs103", "John Write")
    , ("cs104", "Jane Ya Ya")
    , ("cs105", "Jane Ya Ya")
    , ("cs106", "John Two Horns")
    , ("cs107", "John Two Horns")
    , ("cs108", "Jane Web")
    , ("cs109", "Jane Web")
    ]

ratings = 
    [ Rating 
        { ratingStudent = "Jane Barnett"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 15
        }
    , Rating 
        { ratingStudent = "Jane Barnett"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 12
        }
    , Rating 
        { ratingStudent = "Jane Barnett"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 21
        }
    , Rating 
        { ratingStudent = "Jane Barnett"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 36
        }
    , Rating 
        { ratingStudent = "Jane Barnett"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 42
        }
    , Rating 
        { ratingStudent = "Jane Barnett"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 15
        }
    , Rating 
        { ratingStudent = "Jane Barnett"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 30
        }
    , Rating 
        { ratingStudent = "Jane Barnett"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 38
        }
    , Rating 
        { ratingStudent = "Jane Barnett"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 38
        }
    , Rating 
        { ratingStudent = "Jane Barnett"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 29
        }
    , Rating 
        { ratingStudent = "John Bigboote"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 38
        }
    , Rating 
        { ratingStudent = "John Bigboote"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 8
        }
    , Rating 
        { ratingStudent = "John Bigboote"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 36
        }
    , Rating 
        { ratingStudent = "John Bigboote"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 39
        }
    , Rating 
        { ratingStudent = "John Bigboote"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 45
        }
    , Rating 
        { ratingStudent = "John Bigboote"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 16
        }
    , Rating 
        { ratingStudent = "John Bigboote"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 2
        }
    , Rating 
        { ratingStudent = "John Bigboote"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 35
        }
    , Rating 
        { ratingStudent = "John Bigboote"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 31
        }
    , Rating 
        { ratingStudent = "John Bigboote"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 42
        }
    , Rating 
        { ratingStudent = "Jane Camp"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 5
        }
    , Rating 
        { ratingStudent = "Jane Camp"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 38
        }
    , Rating 
        { ratingStudent = "Jane Camp"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 10
        }
    , Rating 
        { ratingStudent = "Jane Camp"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 48
        }
    , Rating 
        { ratingStudent = "Jane Camp"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 31
        }
    , Rating 
        { ratingStudent = "Jane Camp"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 9
        }
    , Rating 
        { ratingStudent = "Jane Camp"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 42
        }
    , Rating 
        { ratingStudent = "Jane Camp"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 32
        }
    , Rating 
        { ratingStudent = "Jane Camp"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 29
        }
    , Rating 
        { ratingStudent = "Jane Camp"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 13
        }
    , Rating 
        { ratingStudent = "John Careful Walker"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 20
        }
    , Rating 
        { ratingStudent = "John Careful Walker"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 18
        }
    , Rating 
        { ratingStudent = "John Careful Walker"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 8
        }
    , Rating 
        { ratingStudent = "John Careful Walker"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 21
        }
    , Rating 
        { ratingStudent = "John Careful Walker"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 11
        }
    , Rating 
        { ratingStudent = "John Careful Walker"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 3
        }
    , Rating 
        { ratingStudent = "John Careful Walker"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 25
        }
    , Rating 
        { ratingStudent = "John Careful Walker"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 34
        }
    , Rating 
        { ratingStudent = "John Careful Walker"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 10
        }
    , Rating 
        { ratingStudent = "John Careful Walker"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 1
        }
    , Rating 
        { ratingStudent = "Jane Chief Crier"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 14
        }
    , Rating 
        { ratingStudent = "Jane Chief Crier"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 28
        }
    , Rating 
        { ratingStudent = "Jane Chief Crier"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 42
        }
    , Rating 
        { ratingStudent = "Jane Chief Crier"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 11
        }
    , Rating 
        { ratingStudent = "Jane Chief Crier"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 18
        }
    , Rating 
        { ratingStudent = "Jane Chief Crier"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 5
        }
    , Rating 
        { ratingStudent = "Jane Chief Crier"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 33
        }
    , Rating 
        { ratingStudent = "Jane Chief Crier"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 8
        }
    , Rating 
        { ratingStudent = "Jane Chief Crier"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 15
        }
    , Rating 
        { ratingStudent = "Jane Chief Crier"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 32
        }
    , Rating 
        { ratingStudent = "John Cooper"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 26
        }
    , Rating 
        { ratingStudent = "John Cooper"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 41
        }
    , Rating 
        { ratingStudent = "John Cooper"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 0
        }
    , Rating 
        { ratingStudent = "John Cooper"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 44
        }
    , Rating 
        { ratingStudent = "John Cooper"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 32
        }
    , Rating 
        { ratingStudent = "John Cooper"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 38
        }
    , Rating 
        { ratingStudent = "John Cooper"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 14
        }
    , Rating 
        { ratingStudent = "John Cooper"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 4
        }
    , Rating 
        { ratingStudent = "John Cooper"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 45
        }
    , Rating 
        { ratingStudent = "John Cooper"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 36
        }
    , Rating 
        { ratingStudent = "Jane Coyote"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 14
        }
    , Rating 
        { ratingStudent = "Jane Coyote"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 28
        }
    , Rating 
        { ratingStudent = "Jane Coyote"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 43
        }
    , Rating 
        { ratingStudent = "Jane Coyote"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 9
        }
    , Rating 
        { ratingStudent = "Jane Coyote"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 28
        }
    , Rating 
        { ratingStudent = "Jane Coyote"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 2
        }
    , Rating 
        { ratingStudent = "Jane Coyote"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 30
        }
    , Rating 
        { ratingStudent = "Jane Coyote"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 46
        }
    , Rating 
        { ratingStudent = "Jane Coyote"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 36
        }
    , Rating 
        { ratingStudent = "Jane Coyote"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 35
        }
    , Rating 
        { ratingStudent = "John Edwards"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 16
        }
    , Rating 
        { ratingStudent = "John Edwards"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 17
        }
    , Rating 
        { ratingStudent = "John Edwards"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 49
        }
    , Rating 
        { ratingStudent = "John Edwards"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 23
        }
    , Rating 
        { ratingStudent = "John Edwards"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 6
        }
    , Rating 
        { ratingStudent = "John Edwards"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 48
        }
    , Rating 
        { ratingStudent = "John Edwards"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 34
        }
    , Rating 
        { ratingStudent = "John Edwards"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 13
        }
    , Rating 
        { ratingStudent = "John Edwards"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 3
        }
    , Rating 
        { ratingStudent = "John Edwards"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 12
        }
    , Rating 
        { ratingStudent = "Jane Fat Eating"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 29
        }
    , Rating 
        { ratingStudent = "Jane Fat Eating"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 38
        }
    , Rating 
        { ratingStudent = "Jane Fat Eating"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 44
        }
    , Rating 
        { ratingStudent = "Jane Fat Eating"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 21
        }
    , Rating 
        { ratingStudent = "Jane Fat Eating"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 18
        }
    , Rating 
        { ratingStudent = "Jane Fat Eating"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 23
        }
    , Rating 
        { ratingStudent = "Jane Fat Eating"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 16
        }
    , Rating 
        { ratingStudent = "Jane Fat Eating"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 5
        }
    , Rating 
        { ratingStudent = "Jane Fat Eating"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 3
        }
    , Rating 
        { ratingStudent = "Jane Fat Eating"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 32
        }
    , Rating 
        { ratingStudent = "John Fish"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 11
        }
    , Rating 
        { ratingStudent = "John Fish"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 25
        }
    , Rating 
        { ratingStudent = "John Fish"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 13
        }
    , Rating 
        { ratingStudent = "John Fish"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 21
        }
    , Rating 
        { ratingStudent = "John Fish"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 47
        }
    , Rating 
        { ratingStudent = "John Fish"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 33
        }
    , Rating 
        { ratingStudent = "John Fish"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 32
        }
    , Rating 
        { ratingStudent = "John Fish"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 15
        }
    , Rating 
        { ratingStudent = "John Fish"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 30
        }
    , Rating 
        { ratingStudent = "John Fish"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 10
        }
    , Rating 
        { ratingStudent = "Jane Fledgling"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 44
        }
    , Rating 
        { ratingStudent = "Jane Fledgling"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 42
        }
    , Rating 
        { ratingStudent = "Jane Fledgling"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 46
        }
    , Rating 
        { ratingStudent = "Jane Fledgling"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 31
        }
    , Rating 
        { ratingStudent = "Jane Fledgling"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 24
        }
    , Rating 
        { ratingStudent = "Jane Fledgling"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 40
        }
    , Rating 
        { ratingStudent = "Jane Fledgling"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 27
        }
    , Rating 
        { ratingStudent = "Jane Fledgling"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 9
        }
    , Rating 
        { ratingStudent = "Jane Fledgling"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 17
        }
    , Rating 
        { ratingStudent = "Jane Fledgling"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 40
        }
    , Rating 
        { ratingStudent = "John Gomez"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 37
        }
    , Rating 
        { ratingStudent = "John Gomez"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 14
        }
    , Rating 
        { ratingStudent = "John Gomez"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 32
        }
    , Rating 
        { ratingStudent = "John Gomez"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 9
        }
    , Rating 
        { ratingStudent = "John Gomez"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 40
        }
    , Rating 
        { ratingStudent = "John Gomez"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 44
        }
    , Rating 
        { ratingStudent = "John Gomez"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 30
        }
    , Rating 
        { ratingStudent = "John Gomez"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 39
        }
    , Rating 
        { ratingStudent = "John Gomez"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 40
        }
    , Rating 
        { ratingStudent = "John Gomez"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 30
        }
    , Rating 
        { ratingStudent = "Jane Grim"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 6
        }
    , Rating 
        { ratingStudent = "Jane Grim"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 31
        }
    , Rating 
        { ratingStudent = "Jane Grim"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 26
        }
    , Rating 
        { ratingStudent = "Jane Grim"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 37
        }
    , Rating 
        { ratingStudent = "Jane Grim"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 44
        }
    , Rating 
        { ratingStudent = "Jane Grim"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 4
        }
    , Rating 
        { ratingStudent = "Jane Grim"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 24
        }
    , Rating 
        { ratingStudent = "Jane Grim"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 39
        }
    , Rating 
        { ratingStudent = "Jane Grim"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 4
        }
    , Rating 
        { ratingStudent = "Jane Grim"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 19
        }
    , Rating 
        { ratingStudent = "John Guardian"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 42
        }
    , Rating 
        { ratingStudent = "John Guardian"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 4
        }
    , Rating 
        { ratingStudent = "John Guardian"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 18
        }
    , Rating 
        { ratingStudent = "John Guardian"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 24
        }
    , Rating 
        { ratingStudent = "John Guardian"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 31
        }
    , Rating 
        { ratingStudent = "John Guardian"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 18
        }
    , Rating 
        { ratingStudent = "John Guardian"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 21
        }
    , Rating 
        { ratingStudent = "John Guardian"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 28
        }
    , Rating 
        { ratingStudent = "John Guardian"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 35
        }
    , Rating 
        { ratingStudent = "John Guardian"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 20
        }
    , Rating 
        { ratingStudent = "Jane Icicle Boy"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 6
        }
    , Rating 
        { ratingStudent = "Jane Icicle Boy"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 21
        }
    , Rating 
        { ratingStudent = "Jane Icicle Boy"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 7
        }
    , Rating 
        { ratingStudent = "Jane Icicle Boy"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 47
        }
    , Rating 
        { ratingStudent = "Jane Icicle Boy"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 19
        }
    , Rating 
        { ratingStudent = "Jane Icicle Boy"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 22
        }
    , Rating 
        { ratingStudent = "Jane Icicle Boy"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 12
        }
    , Rating 
        { ratingStudent = "Jane Icicle Boy"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 44
        }
    , Rating 
        { ratingStudent = "Jane Icicle Boy"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 13
        }
    , Rating 
        { ratingStudent = "Jane Icicle Boy"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 38
        }
    , Rating 
        { ratingStudent = "John Jones"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 45
        }
    , Rating 
        { ratingStudent = "John Jones"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 11
        }
    , Rating 
        { ratingStudent = "John Jones"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 31
        }
    , Rating 
        { ratingStudent = "John Jones"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 24
        }
    , Rating 
        { ratingStudent = "John Jones"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 38
        }
    , Rating 
        { ratingStudent = "John Jones"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 4
        }
    , Rating 
        { ratingStudent = "John Jones"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 23
        }
    , Rating 
        { ratingStudent = "John Jones"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 29
        }
    , Rating 
        { ratingStudent = "John Jones"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 47
        }
    , Rating 
        { ratingStudent = "John Jones"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 25
        }
    , Rating 
        { ratingStudent = "Jane Joseph"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 36
        }
    , Rating 
        { ratingStudent = "Jane Joseph"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 33
        }
    , Rating 
        { ratingStudent = "Jane Joseph"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 5
        }
    , Rating 
        { ratingStudent = "Jane Joseph"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 46
        }
    , Rating 
        { ratingStudent = "Jane Joseph"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 48
        }
    , Rating 
        { ratingStudent = "Jane Joseph"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 8
        }
    , Rating 
        { ratingStudent = "Jane Joseph"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 12
        }
    , Rating 
        { ratingStudent = "Jane Joseph"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 38
        }
    , Rating 
        { ratingStudent = "Jane Joseph"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 39
        }
    , Rating 
        { ratingStudent = "Jane Joseph"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 16
        }
    , Rating 
        { ratingStudent = "John Kim Chi"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 0
        }
    , Rating 
        { ratingStudent = "John Kim Chi"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 23
        }
    , Rating 
        { ratingStudent = "John Kim Chi"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 40
        }
    , Rating 
        { ratingStudent = "John Kim Chi"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 26
        }
    , Rating 
        { ratingStudent = "John Kim Chi"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 28
        }
    , Rating 
        { ratingStudent = "John Kim Chi"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 30
        }
    , Rating 
        { ratingStudent = "John Kim Chi"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 8
        }
    , Rating 
        { ratingStudent = "John Kim Chi"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 39
        }
    , Rating 
        { ratingStudent = "John Kim Chi"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 28
        }
    , Rating 
        { ratingStudent = "John Kim Chi"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 0
        }
    , Rating 
        { ratingStudent = "Jane Lee"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 19
        }
    , Rating 
        { ratingStudent = "Jane Lee"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 20
        }
    , Rating 
        { ratingStudent = "Jane Lee"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 9
        }
    , Rating 
        { ratingStudent = "Jane Lee"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 42
        }
    , Rating 
        { ratingStudent = "Jane Lee"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 11
        }
    , Rating 
        { ratingStudent = "Jane Lee"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 45
        }
    , Rating 
        { ratingStudent = "Jane Lee"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 42
        }
    , Rating 
        { ratingStudent = "Jane Lee"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 39
        }
    , Rating 
        { ratingStudent = "Jane Lee"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 37
        }
    , Rating 
        { ratingStudent = "Jane Lee"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 37
        }
    , Rating 
        { ratingStudent = "John Littlejohn"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 2
        }
    , Rating 
        { ratingStudent = "John Littlejohn"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 48
        }
    , Rating 
        { ratingStudent = "John Littlejohn"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 36
        }
    , Rating 
        { ratingStudent = "John Littlejohn"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 22
        }
    , Rating 
        { ratingStudent = "John Littlejohn"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 32
        }
    , Rating 
        { ratingStudent = "John Littlejohn"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 10
        }
    , Rating 
        { ratingStudent = "John Littlejohn"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 44
        }
    , Rating 
        { ratingStudent = "John Littlejohn"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 47
        }
    , Rating 
        { ratingStudent = "John Littlejohn"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 4
        }
    , Rating 
        { ratingStudent = "John Littlejohn"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 10
        }
    , Rating 
        { ratingStudent = "Jane Many Jars"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 9
        }
    , Rating 
        { ratingStudent = "Jane Many Jars"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 21
        }
    , Rating 
        { ratingStudent = "Jane Many Jars"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 48
        }
    , Rating 
        { ratingStudent = "Jane Many Jars"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 16
        }
    , Rating 
        { ratingStudent = "Jane Many Jars"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 49
        }
    , Rating 
        { ratingStudent = "Jane Many Jars"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 18
        }
    , Rating 
        { ratingStudent = "Jane Many Jars"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 38
        }
    , Rating 
        { ratingStudent = "Jane Many Jars"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 46
        }
    , Rating 
        { ratingStudent = "Jane Many Jars"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 16
        }
    , Rating 
        { ratingStudent = "Jane Many Jars"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 4
        }
    , Rating 
        { ratingStudent = "John Milton"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 39
        }
    , Rating 
        { ratingStudent = "John Milton"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 16
        }
    , Rating 
        { ratingStudent = "John Milton"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 20
        }
    , Rating 
        { ratingStudent = "John Milton"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 21
        }
    , Rating 
        { ratingStudent = "John Milton"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 37
        }
    , Rating 
        { ratingStudent = "John Milton"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 35
        }
    , Rating 
        { ratingStudent = "John Milton"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 28
        }
    , Rating 
        { ratingStudent = "John Milton"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 49
        }
    , Rating 
        { ratingStudent = "John Milton"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 4
        }
    , Rating 
        { ratingStudent = "John Milton"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 3
        }
    , Rating 
        { ratingStudent = "Jane Mud Head"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 18
        }
    , Rating 
        { ratingStudent = "Jane Mud Head"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 6
        }
    , Rating 
        { ratingStudent = "Jane Mud Head"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 32
        }
    , Rating 
        { ratingStudent = "Jane Mud Head"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 1
        }
    , Rating 
        { ratingStudent = "Jane Mud Head"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 40
        }
    , Rating 
        { ratingStudent = "Jane Mud Head"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 17
        }
    , Rating 
        { ratingStudent = "Jane Mud Head"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 27
        }
    , Rating 
        { ratingStudent = "Jane Mud Head"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 33
        }
    , Rating 
        { ratingStudent = "Jane Mud Head"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 30
        }
    , Rating 
        { ratingStudent = "Jane Mud Head"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 43
        }
    , Rating 
        { ratingStudent = "John Nephew"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 3
        }
    , Rating 
        { ratingStudent = "John Nephew"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 16
        }
    , Rating 
        { ratingStudent = "John Nephew"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 41
        }
    , Rating 
        { ratingStudent = "John Nephew"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 0
        }
    , Rating 
        { ratingStudent = "John Nephew"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 9
        }
    , Rating 
        { ratingStudent = "John Nephew"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 48
        }
    , Rating 
        { ratingStudent = "John Nephew"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 15
        }
    , Rating 
        { ratingStudent = "John Nephew"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 14
        }
    , Rating 
        { ratingStudent = "John Nephew"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 27
        }
    , Rating 
        { ratingStudent = "John Nephew"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 41
        }
    , Rating 
        { ratingStudent = "Jane Nolan"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 29
        }
    , Rating 
        { ratingStudent = "Jane Nolan"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 29
        }
    , Rating 
        { ratingStudent = "Jane Nolan"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 13
        }
    , Rating 
        { ratingStudent = "Jane Nolan"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 14
        }
    , Rating 
        { ratingStudent = "Jane Nolan"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 20
        }
    , Rating 
        { ratingStudent = "Jane Nolan"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 19
        }
    , Rating 
        { ratingStudent = "Jane Nolan"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 47
        }
    , Rating 
        { ratingStudent = "Jane Nolan"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 24
        }
    , Rating 
        { ratingStudent = "Jane Nolan"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 31
        }
    , Rating 
        { ratingStudent = "Jane Nolan"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 19
        }
    , Rating 
        { ratingStudent = "John O'Connor"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 32
        }
    , Rating 
        { ratingStudent = "John O'Connor"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 28
        }
    , Rating 
        { ratingStudent = "John O'Connor"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 49
        }
    , Rating 
        { ratingStudent = "John O'Connor"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 1
        }
    , Rating 
        { ratingStudent = "John O'Connor"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 29
        }
    , Rating 
        { ratingStudent = "John O'Connor"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 35
        }
    , Rating 
        { ratingStudent = "John O'Connor"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 33
        }
    , Rating 
        { ratingStudent = "John O'Connor"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 18
        }
    , Rating 
        { ratingStudent = "John O'Connor"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 40
        }
    , Rating 
        { ratingStudent = "John O'Connor"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 33
        }
    , Rating 
        { ratingStudent = "Jane Omar"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 22
        }
    , Rating 
        { ratingStudent = "Jane Omar"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 13
        }
    , Rating 
        { ratingStudent = "Jane Omar"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 3
        }
    , Rating 
        { ratingStudent = "Jane Omar"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 8
        }
    , Rating 
        { ratingStudent = "Jane Omar"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 7
        }
    , Rating 
        { ratingStudent = "Jane Omar"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 16
        }
    , Rating 
        { ratingStudent = "Jane Omar"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 47
        }
    , Rating 
        { ratingStudent = "Jane Omar"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 2
        }
    , Rating 
        { ratingStudent = "Jane Omar"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 48
        }
    , Rating 
        { ratingStudent = "Jane Omar"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 24
        }
    , Rating 
        { ratingStudent = "John Parrot"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 35
        }
    , Rating 
        { ratingStudent = "John Parrot"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 14
        }
    , Rating 
        { ratingStudent = "John Parrot"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 18
        }
    , Rating 
        { ratingStudent = "John Parrot"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 49
        }
    , Rating 
        { ratingStudent = "John Parrot"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 34
        }
    , Rating 
        { ratingStudent = "John Parrot"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 7
        }
    , Rating 
        { ratingStudent = "John Parrot"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 17
        }
    , Rating 
        { ratingStudent = "John Parrot"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 18
        }
    , Rating 
        { ratingStudent = "John Parrot"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 23
        }
    , Rating 
        { ratingStudent = "John Parrot"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 32
        }
    , Rating 
        { ratingStudent = "Jane Rajeesh"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 33
        }
    , Rating 
        { ratingStudent = "Jane Rajeesh"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 46
        }
    , Rating 
        { ratingStudent = "Jane Rajeesh"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 5
        }
    , Rating 
        { ratingStudent = "Jane Rajeesh"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 28
        }
    , Rating 
        { ratingStudent = "Jane Rajeesh"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 30
        }
    , Rating 
        { ratingStudent = "Jane Rajeesh"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 0
        }
    , Rating 
        { ratingStudent = "Jane Rajeesh"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 28
        }
    , Rating 
        { ratingStudent = "Jane Rajeesh"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 37
        }
    , Rating 
        { ratingStudent = "Jane Rajeesh"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 5
        }
    , Rating 
        { ratingStudent = "Jane Rajeesh"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 2
        }
    , Rating 
        { ratingStudent = "John Ready to Fly"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 29
        }
    , Rating 
        { ratingStudent = "John Ready to Fly"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 48
        }
    , Rating 
        { ratingStudent = "John Ready to Fly"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 25
        }
    , Rating 
        { ratingStudent = "John Ready to Fly"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 45
        }
    , Rating 
        { ratingStudent = "John Ready to Fly"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 47
        }
    , Rating 
        { ratingStudent = "John Ready to Fly"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 48
        }
    , Rating 
        { ratingStudent = "John Ready to Fly"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 34
        }
    , Rating 
        { ratingStudent = "John Ready to Fly"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 25
        }
    , Rating 
        { ratingStudent = "John Ready to Fly"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 12
        }
    , Rating 
        { ratingStudent = "John Ready to Fly"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 39
        }
    , Rating 
        { ratingStudent = "Jane Repeat Dance"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 2
        }
    , Rating 
        { ratingStudent = "Jane Repeat Dance"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 18
        }
    , Rating 
        { ratingStudent = "Jane Repeat Dance"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 9
        }
    , Rating 
        { ratingStudent = "Jane Repeat Dance"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 33
        }
    , Rating 
        { ratingStudent = "Jane Repeat Dance"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 44
        }
    , Rating 
        { ratingStudent = "Jane Repeat Dance"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 43
        }
    , Rating 
        { ratingStudent = "Jane Repeat Dance"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 25
        }
    , Rating 
        { ratingStudent = "Jane Repeat Dance"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 44
        }
    , Rating 
        { ratingStudent = "Jane Repeat Dance"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 21
        }
    , Rating 
        { ratingStudent = "Jane Repeat Dance"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 15
        }
    , Rating 
        { ratingStudent = "John Roberts"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 25
        }
    , Rating 
        { ratingStudent = "John Roberts"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 21
        }
    , Rating 
        { ratingStudent = "John Roberts"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 28
        }
    , Rating 
        { ratingStudent = "John Roberts"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 25
        }
    , Rating 
        { ratingStudent = "John Roberts"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 26
        }
    , Rating 
        { ratingStudent = "John Roberts"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 5
        }
    , Rating 
        { ratingStudent = "John Roberts"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 30
        }
    , Rating 
        { ratingStudent = "John Roberts"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 35
        }
    , Rating 
        { ratingStudent = "John Roberts"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 35
        }
    , Rating 
        { ratingStudent = "John Roberts"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 45
        }
    , Rating 
        { ratingStudent = "Jane Scott"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 19
        }
    , Rating 
        { ratingStudent = "Jane Scott"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 44
        }
    , Rating 
        { ratingStudent = "Jane Scott"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 3
        }
    , Rating 
        { ratingStudent = "Jane Scott"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 3
        }
    , Rating 
        { ratingStudent = "Jane Scott"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 41
        }
    , Rating 
        { ratingStudent = "Jane Scott"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 10
        }
    , Rating 
        { ratingStudent = "Jane Scott"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 44
        }
    , Rating 
        { ratingStudent = "Jane Scott"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 34
        }
    , Rating 
        { ratingStudent = "Jane Scott"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 1
        }
    , Rating 
        { ratingStudent = "Jane Scott"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 32
        }
    , Rating 
        { ratingStudent = "John Shaw"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 9
        }
    , Rating 
        { ratingStudent = "John Shaw"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 9
        }
    , Rating 
        { ratingStudent = "John Shaw"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 15
        }
    , Rating 
        { ratingStudent = "John Shaw"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 45
        }
    , Rating 
        { ratingStudent = "John Shaw"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 49
        }
    , Rating 
        { ratingStudent = "John Shaw"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 12
        }
    , Rating 
        { ratingStudent = "John Shaw"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 45
        }
    , Rating 
        { ratingStudent = "John Shaw"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 32
        }
    , Rating 
        { ratingStudent = "John Shaw"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 49
        }
    , Rating 
        { ratingStudent = "John Shaw"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 17
        }
    , Rating 
        { ratingStudent = "Jane Smallberries"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 27
        }
    , Rating 
        { ratingStudent = "Jane Smallberries"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 1
        }
    , Rating 
        { ratingStudent = "Jane Smallberries"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 37
        }
    , Rating 
        { ratingStudent = "Jane Smallberries"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 10
        }
    , Rating 
        { ratingStudent = "Jane Smallberries"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 38
        }
    , Rating 
        { ratingStudent = "Jane Smallberries"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 0
        }
    , Rating 
        { ratingStudent = "Jane Smallberries"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 9
        }
    , Rating 
        { ratingStudent = "Jane Smallberries"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 27
        }
    , Rating 
        { ratingStudent = "Jane Smallberries"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 3
        }
    , Rating 
        { ratingStudent = "Jane Smallberries"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 48
        }
    , Rating 
        { ratingStudent = "John Starbird"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 36
        }
    , Rating 
        { ratingStudent = "John Starbird"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 7
        }
    , Rating 
        { ratingStudent = "John Starbird"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 42
        }
    , Rating 
        { ratingStudent = "John Starbird"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 36
        }
    , Rating 
        { ratingStudent = "John Starbird"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 1
        }
    , Rating 
        { ratingStudent = "John Starbird"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 8
        }
    , Rating 
        { ratingStudent = "John Starbird"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 19
        }
    , Rating 
        { ratingStudent = "John Starbird"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 8
        }
    , Rating 
        { ratingStudent = "John Starbird"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 11
        }
    , Rating 
        { ratingStudent = "John Starbird"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 4
        }
    , Rating 
        { ratingStudent = "Jane Take Cover"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 7
        }
    , Rating 
        { ratingStudent = "Jane Take Cover"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 46
        }
    , Rating 
        { ratingStudent = "Jane Take Cover"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 37
        }
    , Rating 
        { ratingStudent = "Jane Take Cover"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 8
        }
    , Rating 
        { ratingStudent = "Jane Take Cover"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 26
        }
    , Rating 
        { ratingStudent = "Jane Take Cover"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 39
        }
    , Rating 
        { ratingStudent = "Jane Take Cover"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 30
        }
    , Rating 
        { ratingStudent = "Jane Take Cover"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 13
        }
    , Rating 
        { ratingStudent = "Jane Take Cover"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 47
        }
    , Rating 
        { ratingStudent = "Jane Take Cover"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 38
        }
    , Rating 
        { ratingStudent = "John Thorny Stick"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 20
        }
    , Rating 
        { ratingStudent = "John Thorny Stick"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 38
        }
    , Rating 
        { ratingStudent = "John Thorny Stick"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 14
        }
    , Rating 
        { ratingStudent = "John Thorny Stick"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 32
        }
    , Rating 
        { ratingStudent = "John Thorny Stick"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 14
        }
    , Rating 
        { ratingStudent = "John Thorny Stick"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 21
        }
    , Rating 
        { ratingStudent = "John Thorny Stick"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 7
        }
    , Rating 
        { ratingStudent = "John Thorny Stick"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 19
        }
    , Rating 
        { ratingStudent = "John Thorny Stick"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 24
        }
    , Rating 
        { ratingStudent = "John Thorny Stick"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 36
        }
    , Rating 
        { ratingStudent = "Jane Turk"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 49
        }
    , Rating 
        { ratingStudent = "Jane Turk"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 30
        }
    , Rating 
        { ratingStudent = "Jane Turk"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 20
        }
    , Rating 
        { ratingStudent = "Jane Turk"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 36
        }
    , Rating 
        { ratingStudent = "Jane Turk"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 14
        }
    , Rating 
        { ratingStudent = "Jane Turk"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 3
        }
    , Rating 
        { ratingStudent = "Jane Turk"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 47
        }
    , Rating 
        { ratingStudent = "Jane Turk"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 16
        }
    , Rating 
        { ratingStudent = "Jane Turk"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 10
        }
    , Rating 
        { ratingStudent = "Jane Turk"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 43
        }
    , Rating 
        { ratingStudent = "John Whorfin"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs100"
        , ratingScore = 27
        }
    , Rating 
        { ratingStudent = "John Whorfin"
        , ratingProf = "Jane Wood"
        , ratingCourse = "cs101"
        , ratingScore = 24
        }
    , Rating 
        { ratingStudent = "John Whorfin"
        , ratingProf = "John Write"
        , ratingCourse = "cs102"
        , ratingScore = 38
        }
    , Rating 
        { ratingStudent = "John Whorfin"
        , ratingProf = "John Write"
        , ratingCourse = "cs103"
        , ratingScore = 4
        }
    , Rating 
        { ratingStudent = "John Whorfin"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs104"
        , ratingScore = 13
        }
    , Rating 
        { ratingStudent = "John Whorfin"
        , ratingProf = "Jane Ya Ya"
        , ratingCourse = "cs105"
        , ratingScore = 30
        }
    , Rating 
        { ratingStudent = "John Whorfin"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs106"
        , ratingScore = 42
        }
    , Rating 
        { ratingStudent = "John Whorfin"
        , ratingProf = "John Two Horns"
        , ratingCourse = "cs107"
        , ratingScore = 12
        }
    , Rating 
        { ratingStudent = "John Whorfin"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs108"
        , ratingScore = 2
        }
    , Rating 
        { ratingStudent = "John Whorfin"
        , ratingProf = "Jane Web"
        , ratingCourse = "cs109"
        , ratingScore = 18
        }
    ]
