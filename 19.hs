isLeap year = if (year `mod` 4 /= 0) then False
              else if (year `mod` 100 /= 0) then True
              else if (year `mod` 400 == 0) then True
              else False

dayInYear year = if (isLeap year) then 366 else 365

dayInMonth 2 year = if (isLeap year) then 29 else 28
dayInMonth month year = if (elem month [1, 3, 5, 7, 8, 10, 12]) then 31
                        else 30

getDate 1 1 1900 = 1
getDate 1 1 year = (dayInYear (year - 1) + getDate 1 1 (year - 1)) `mod` 7
getDate 1 month year = (dayInMonth (month - 1) year+ getDate 1 (month - 1) year) `mod` 7
getDate day month year = (day - 1 + getDate 1 month year) `mod` 7

ans = length $ filter (==0) [getDate 1 month year | year <- [1901..2000], month <- [1..12]]
