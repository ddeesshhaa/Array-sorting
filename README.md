# Array-sorting
> The project is about sorting Arrays in according to ascending and descending orders using three different algorithms two simple sorting Techniques and one advanced sorting > technique  The code can operate in 8086 processor which has 16-bit Architecture  and use model small and stack. firstly, the program asks the user to enter the number of elements, > then the program asks him to enter the values of numbers. then the program asks the user to choose the sorting method from three different algorithms (bubble sort , selection sort, quick sort )

# Project Advantages
+ Three types of sorting algorithms.
+ Choosing the method of sorting is based on the user desire.
+ The number of values is variable and determined by the user.
+ Handling for inputs such that the user can only enters Hexa Numbers.
+ The numbers can be sorted ascending or descending.


# Bubble sort 
> it works by repeatedly swapping the adjacent elements if they are in wrong order.
> the algorithm makes about N^(2)⁄2 comparisons.
> If the data is random, a swap is necessary about half the time, so there will be about N^(2)⁄4 swaps.
> Both swaps and comparisons are proportional to N^(2)

![image](https://user-images.githubusercontent.com/66144435/148587653-194b127a-e346-47c1-bcc9-529745e79e61.png)

# Selection sort
>sorts an array by repeatedly finding the maximum element (considering ascending order) from unsorted part and putting it at the ending.
>The selection sort improves on the bubble sort by reducing the number of swaps necessary from O(N^(2)) to O(N).
>the number of comparisons remains O(N^(2)).
>For large values of N, the comparison times will dominate, so we would have to say that the selection sort runs in O(N^(2)) time


# Quick sort
> Basically, the quicksort algorithm operates by partitioning an array into two subarrays and then calling itself recursively to quicksort each of these subarrays.
> In most situations, it’s the fastest, operating in O(N*log N) time.
> You can pick a data item to be the pivot at random.
> After the partition, if the pivot is inserted at the boundary between the left and right subarrays, it will be in its final sorted position.


# Comparing between the three algorithms
> Bubble and selection sorts are easy to implement 
> Bubble and selection sorts Time complexity is N^(2) which is a long time
> Swapping in selection sort is faster than swapping in bubble sort 
> Quick sort is harder than bubble and selection sorts to implement 
> Quick sort time complexity is Nlog(N) Which is better zoom mobile and selection sort
> Quick Sort takes less time than selection and bubble sort
> Bubble sort and selection sort are more stable than quick sort 
