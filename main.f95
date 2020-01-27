function getFileName() result(fname)
  character(len=32) :: fname

  write(*, "(A)", advance="no") " Enter the name of the file: "
  read (*, *) fname
end function getFileName

function stringArrayContainsString(arr, val) result(flag)
  character(len=32), dimension(50), intent(in) :: arr
  character(len=32), intent(in) :: val
  logical :: flag
  integer :: i

  flag = .FALSE.

  do i = 1, size(arr)
    if (arr(i) == val) then
      flag = .TRUE.
      exit
    end if
  end do
end function stringArrayContainsString

recursive subroutine getWordListFromFile(words, uniqueWords, totalWords)
  character(len=32), dimension(50), intent(out) :: words
  integer, intent(out) :: uniqueWords, totalWords
  integer :: index
  character(len=32) :: fname, getFileName, line
  logical :: fileExists, wordIsRepeat, stringArrayContainsString

  index = 1
  uniqueWords = 0
  totalWords = 0

  fname = getFileName()

  inquire(file=fname, exist=fileExists)
  if (.NOT. fileExists) then
    print *, "Unable to read file, try again..."
    call getWordListFromFile(words, uniqueWords, totalWords)
  else

    open(1, file = fname)

    do

      if (totalWords == 50) exit

      read(1, *, iostat=io) line
      if (io /= 0) exit

      totalWords = totalWords + 1

      wordIsRepeat = stringArrayContainsString(words, line)
      if (.NOT. wordIsRepeat) then
        words(index) = line
        index = index + 1
        uniqueWords = uniqueWords + 1
      end if

    end do

  end if

end subroutine getWordListFromFile

program stringarray
  character(len=32), dimension(50) :: words
  integer :: uniqueWords, totalWords

  call getWordListFromFile(words, uniqueWords, totalWords)

  write(*, "(I2, A12, I2, A21)") uniqueWords, " out of the ", totalWords, " entered were unique."
  print *, NEW_LINE('A')//"Those unique words are as follows:"

  do i = 1, uniqueWords
    print *, words(i)
  end do
end program stringarray
