! Polls for input from the user and returns as a string.
! Intended to be used to ask the user for the name of a text file
function getFileName() result(fname)
  ! Create but do not instantiate a new string variable, intended to be the name of the input text file
  character(len=32) :: fname

  ! Ask user for name of input text file
  write(*, "(A)", advance="no") " Enter the name of the file: "

  ! Read in a string from keyboard and save it in fname
  read (*, *) fname
end function getFileName

! Returns whether or not the given array contains the given value
function stringArrayContainsString(arr, val) result(flag)
  ! Create a new string array variable of size 50, declare it to be input (its value comes from the function arguments)
  character(len=32), dimension(50), intent(in) :: arr
  ! Create a new string variable of length 32, declare it to be input
  character(len=32), intent(in) :: val
  ! Create a new logical (boolean) variable, will be the result of the function
  logical :: flag
  ! Create a new integer variable, will be used as the index when looping through arr
  integer :: i

  ! Initialize flag with a default value of false
  flag = .FALSE.

  ! Loop through arr (Fortran is a 1 indexed language, so i starts at 1 instead of 0)
  do i = 1, size(arr)
    ! If current element in the array has the same value as val, set flag to true and break out of loop
    if (arr(i) == val) then
      flag = .TRUE.
      exit
    end if
  end do
end function stringArrayContainsString

! Reads and parses the content of a selected file
! Repeatedly polls the user for a valid file until one is found
! Once found, each unique line in the file is inserted into a string array
! A maximum of 50 words will be read from the file
! Returns three values: array of unique lines in the file, the number of unique words found, the number of words read in
recursive subroutine getWordListFromFile(words, uniqueWords, totalWords)
  ! Create a new string array variable of size 50, declare it to be output
  character(len=32), dimension(50), intent(out) :: words
  ! Create two integer variables, declare them to be output
  integer, intent(out) :: uniqueWords, totalWords
  ! Create a new integer variable, will be used to maintain the current index in the words array
  integer :: index
  ! Create three new string variables
  character(len=32) :: fname, getFileName, line
  ! Create three new logical (boolean) variables
  logical :: fileExists, wordIsRepeat, stringArrayContainsString

  ! Initialize some variables with values
  ! index is 1 because Fortran arrays are 1 indexed
  ! uniqueWords and totalWords start as 0 because we haven't read in any words yet
  ! fname is the name of the file we will be reading from
  index = 1
  uniqueWords = 0
  totalWords = 0
  fname = getFileName()

  ! Checks if the selected file exists or not, sets the resulting value in fileExists
  inquire(file=fname, exist=fileExists)

  ! If the file does not exist, report the error to the user and re-call this subroutine to try again
  if (.NOT. fileExists) then
    print *, "Unable to read file, try again..."
    call getWordListFromFile(words, uniqueWords, totalWords)
  else
    ! Determined the selected file does exist, open the file and set its reference number to 1 since we're only opening one file
    open(1, file = fname)

    ! Loop, used to loop through each line of the file
    do

      ! If we've read in 50 words already, break out of loop
      if (totalWords == 50) exit

      ! Read the next line of file number 1
      read(1, *, iostat=io) line
      ! If we didn't read in anything, break out of the loop
      if (io /= 0) exit

      ! We successfully read in a word, increment counter
      totalWords = totalWords + 1

      ! Check if the word is a repeat
      wordIsRepeat = stringArrayContainsString(words, line)

      ! If the word is not a repeat
      if (.NOT. wordIsRepeat) then
        Store in the current spot in the array, increment counters
        words(index) = line
        index = index + 1
        uniqueWords = uniqueWords + 1
      end if

    end do

  end if

end subroutine getWordListFromFile

! Declare the "program" (this is like the main method in other languages)
program stringarray
  ! Create a new string array of size 50
  character(len=32), dimension(50) :: words
  ! Create two new integer variables
  integer :: uniqueWords, totalWords

  ! Call the getWordListFromFile subroutine to get the words, uniqueWords, and totalWords in the file
  call getWordListFromFile(words, uniqueWords, totalWords)

  ! Print out the summary of the results (uniqueWords and totalWords entered)
  write(*, "(I2, A12, I2, A21)") uniqueWords, " out of the ", totalWords, " entered were unique."
  ! Print out a notice before printing all unique words
  print *, NEW_LINE('A')//"Those unique words are as follows:"

  ! Loop through the uniqueWords array an print each word on its own line
  do i = 1, uniqueWords
    print *, words(i)
  end do
end program stringarray
