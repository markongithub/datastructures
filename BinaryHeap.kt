class BinaryMinHeap {
  var myArray: IntArray = IntArray(33)
  var size: Int = 0
    
  fun insert(newValue: Int): Unit {
      var insertIndex = size + 1
      size++
      if (insertIndex == 1) {
          myArray[insertIndex] = newValue
      }
      while (insertIndex > 1) {
          myArray[insertIndex] = newValue
          println("Considering leaving it at location" + insertIndex)
          var parentIndex = insertIndex / 2
          if (myArray[parentIndex] > newValue) {
              myArray[insertIndex] = myArray[parentIndex]
              myArray[parentIndex] = newValue
          }
          insertIndex = parentIndex
      }
      println("The array is now " + myArray.joinToString())
  }
    
  fun findMin(): Int {
      return myArray[1]
  }
    
  fun swapIndexs(x: Int, y: Int): Unit {
      val temp = myArray[x]
      myArray[x] = myArray[y]
      myArray[y] = temp
  }

  fun deleteMin(): Int {
      val returnValue = myArray[1]
      var currentIndex = 1
      val homelessValue = myArray[size]
      myArray[1] = homelessValue
      size--
      while (true) {
          val leftIndex = 2 * insertIndex
          val rightIndex = 2 * insertIndex + 1
          if (leftIndex > size) break
          if (leftIndex == size) {
              if (homelessValue > myArray[leftIndex]) {
                  swapIndexs(insertIndex, leftIndex)
                  break
              }
          }
          var lesserChild = leftIndex
          var greaterChild = rightIndex
          if (myArray[rightIndex] < myArray[leftIndex]) {
              lesserChild = rightIndex
              greaterChild = leftIndex
          }
          if (myArray[lesserChild] < homelessValue) {
              swapIndexs(insertIndex, lesserChild)
              insertIndex = lesserChild 
          }
          else {
              break
          }
      }
  println("The array is now " + myArray.joinToString())    
  return returnValue
  }
}

fun main(args: Array<String>) {
    var myHeap = BinaryMinHeap()
    myHeap.insert(10)
    myHeap.insert(9)
    myHeap.insert(8)
    myHeap.insert(7)
    myHeap.insert(6)
		myHeap.insert(5)
    println("I just deleted " + myHeap.deleteMin())
}
