module A4 where

import A4Base
    ( Name, Course
    , Rating, ratingStudent, ratingProf, ratingCourse, ratingScore, ppRating
    , ratings, ppRatings
    )
import Data.List (nub)
import Prelude 
    ( Int, (+), (-), div
    , fst, snd
    , map, filter, lookup, length, sum, (!!)
    , Maybe(..) 
    , Bool(..), (&&), (||), not, (==)
    , undefined, error, ($)
    )

-- DON'T TOUCH THE ABOVE!
--------------------------

{-
 
ASSIGNMENT 4 

DUE: Thursday Oct 12 23:59

INSTRUCTIONS: 

    1. Do not change the existing type declarations. 

    2. There is a second file to download, A4Base.hs. This has the data type
       definitions you will need and needs to be placed in the same
       directory/folder as the assignment file. Do not change anything in 
       A4Base.hs.  

    3. Only submit this file.

This assignment is to write some reporting functions for student evaluations of
courses. The raw data is just a list called "ratings" that's defined in
A4Base.hs. Each element of the list is an individual rating given by a student
to a particular course taught by a particular prof.

You will be writing a bunch of small functions to derive useful facts from this
"database" of ratings. 

The point of this assignment is to become proficient with the use of map and
filter for processing of collections of data (lists in this case).

The import statements at the top of this file restrict what you can use. Read
them carefully.

Here is a description of most of the imported functions.

    map    -- apply a function to every member of a list
    filter -- get list of elements satisfying a given predicate
    nub    -- remove duplicates from a list
    take   -- get a prefix of a list
    drop   -- remove a prefix of a list
    !!     -- l!!i is the i-th element of the list l (beginning at 0)
    sum    -- sum of a list of numbers
    lookup -- lookup a key in a map represented as an "association list"

One thing to note is that the constructor for the Rating type is *not* imported.
You will not need to build any values in this type, and the fields of a rating
can by accessed using the accessor functions defined by the data type, which
*are* imported. E.g.

    ratingStudent :: Rating -> Name

gives the name of the student giving the rating. 

The functions you write will be mostly based on map, filter, the Rating
accessors, and functions you defined earlier in this file.

-}

-- Bad-software-engineering alert! To save a tiny bit of ugly code, let's defeat
-- the purpose of Maybe and force out the value, raising an error if there isn't
-- one. 
force :: Maybe a -> a 
force Nothing = error "force: can't force Nothing; your program is broken"
force (Just v) = v

-- The average of a list of integers, truncated to an integer. Use this for
-- computations requiring an average.
average :: [Int] -> Int
average [] = error "average: no numbers to average" 
average ss = div (sum ss) (length ss)

-- List of all profs, no duplicates.
profs :: [Rating] -> [Name]
profs rs = 
    nub (map ratingProf rs)

-- List of all courses, no duplicates.
courses :: [Rating] -> [Course]
courses rs = 
    nub (map ratingCourse rs)

-- The ratings received by a particular prof.
profRatings :: [Rating] -> Name -> [Rating]
profRatings rs name = 
    filter  (\r -> name == ratingProf r) rs
--
-- The ratings given by a particular student. 
studentRatings :: [Rating] -> Name -> [Rating]
studentRatings rs student = 
    filter  (\r -> student == ratingStudent r) rs

-- All ratings of a particular course.
courseRatings :: [Rating] -> Course -> [Rating]
courseRatings rs course = 
    filter  (\r -> course == ratingCourse r) rs

-- The ratings for a particular prof in a particular course.
profCourseRatings :: [Rating] -> Name -> Course -> [Rating]
profCourseRatings rs prof course =
    filter (\r -> ratingCourse r == course && ratingProf r == prof) rs

-- List of all courses taught be a particular prof. No duplicates (use nub).
profCourses :: [Rating] -> Name -> [Course]
profCourses rs prof =
    nub (map ratingCourse (profRatings rs prof))

-- The average score received by a prof for a particular course.
profCourseAvg :: [Rating] -> Name -> Course -> Int
profCourseAvg rs prof course =
    average (map ratingScore (profCourseRatings rs prof course))

-- A mapping, represented as an association list, where the keys are the courses
-- taught by the given prof and the values are the average student score for that
-- course.
profCourseAvgs :: [Rating] -> Name -> [(Course,Int)]
profCourseAvgs rs prof =
    map (\course -> (course, profCourseAvg rs prof course)) (profCourses rs prof)

-- The average score for courses taught by the prof. Note: this is the average
-- of the course averages, not the average over all ratings received by the
-- prof.
profAvg :: [Rating] -> Name -> Int
profAvg rs prof = 
    average (map snd (profCourseAvgs rs prof))

-- A mapping giving the average score for each prof
allProfAvgs :: [Rating] -> [(Name, Int)]
allProfAvgs rs =
    map (\prof -> (prof, profAvg rs prof)) (profs rs)

-- The average score given by a student.
studentAvg :: [Rating] -> Name -> Int
studentAvg rs student =
    average (map ratingScore (studentRatings rs student)) 

-- The average of all student averages.
avgStudentAvg :: [Rating] -> Int    
avgStudentAvg rs =
    let students = nub (map ratingStudent rs) in
    average (map (\student -> studentAvg rs student) students)
