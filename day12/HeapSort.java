package org.example;

public class HeapSort {

    public static void main(String[] args) {
        int[] arr = new int[10];
        for (int i = 0; i < arr.length; ++i) arr[i] = i + 1;
        shuffle(arr);
        printArr(arr);
        heapSort(arr);
        printArr(arr);
    }

    private static void heapSort(int[] a) {
        // First, we make sure the array represents a Max Heap.
        // We do this by looping through all the nodes that
        // have children and making sure they follow the Max Heap
        // property. We go back to front to ensure we fix deeper
        // subtrees before touching their parents.
        // Recall that for a node `i` to have children:
        // 2 * i + 1 < a.length <=> i < (a.length - 1) / 2
        // So we start at `(a.length - 1) / 2`.
        for (int fixIndex = (a.length - 1) / 2; fixIndex >= 0; --fixIndex) {
            pushHeapNode(a, fixIndex, a.length);
        }

        // now we'll have two arrays in one:
        // - the sorted array, which we'll be building
        // downwards (from the right).
        // - the heap, which will start on the left and continue
        // till the pushing index from the sorted array.
        // [ h0 h1 h2 h3 ... | s3 s2 s1 s0 ]
        // where '|' is the barrier that separates, and is exactly the
        // index at which we're adding the next sorted element to the sorted array.
        // The barrier moves to the left as we push values to the sorted part and remove
        // them from the heap.

        // NOTE: we don't process the 0th index because by then
        // we'll only have a heap with just one element:
        // [ h0 | sN ... s0 ]
        // which will coincide with the minimum value.
        for (int pushIndex = a.length - 1; pushIndex > 0; --pushIndex) {
            // 'pop' the root from the heap and insert it into the pushIndex.
            // then ensure that the heap property is preserved.
            swap(a, 0, pushIndex);
            pushHeapNode(a, 0, pushIndex);
        }
    }

    // push back one node till it's in the right place.
    // the array is really a slice a[0..len], where
    // limit <= a.length.
    static void pushHeapNode(int[] a, int node, int len) {
        while (true) {
            int child = 2 * node + 1;
            if (child >= len) break;
            // select the biggest child, so that the
            // max property always holds when swapping
            if (child + 1 < len && a[child + 1] > a[child]) {
                child = child + 1;
            }

            if (a[child] > a[node]) {
                swap(a, child, node);
                // since we've just swapped,
                // we have to make sure that it's still
                // greater than the rest of the subtree.
                // Hence, we (tail) recurse on the new index.
                node = child;
            } else break;
        }
    }

    // helper method to swap two elements at two indices.
    private static void swap(int[] arr, int i, int j) {
        int tmp = arr[i];
        arr[i] = arr[j];
        arr[j] = tmp;
    }

    // print a non-empty array.
    // assumes arr.length > 0.
    private static void printArr(int[] arr) {
        System.out.print('{');
        System.out.print(arr[0]);
        for (int i = 1; i < arr.length; ++i)
            System.out.print(", " + arr[i]);
        System.out.println('}');
    }

    // shuffles the elements of an array,
    // ensuring each element is sampled just once.
    private static void shuffle(int[] a) {
        for (int i = 0; i < a.length; ++i) {
            // select an element from the remaining array.
            int j = i + (int) Math.floor(Math.random() * (a.length - i));
            swap(a, i, j);
        }
    }

    public void getCountInFamily(String family) {
        if (isEmpty()) return 0;
        int count = 0;
        if (getInfo().getHogwartsFamily().equals(family))
            count += 1;
        count += getLeft().getCountInFamily();
        count += getRight().getCountInFamily();
        return cuont;
    }


}
