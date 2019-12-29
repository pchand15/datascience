package com.leetcode.problem.algorithm.hard.median;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

class Solution {
    public double findMedianSortedArrays(int[] nums1, int[] nums2) {
    		List<Integer> mergedList = new ArrayList<Integer>();
			List<Integer> num1List = new ArrayList<Integer>();
			List<Integer> num2List = new ArrayList<Integer>();
			float medianInd;
			double median=0.0;
			try{
				if(nums1.length <= 0 | nums2.length <= 0){
					System.out.println("array 1 "+ Arrays.toString(nums1));
					System.out.println("array 2 "+ Arrays.toString(nums2));
				}
			}catch(Exception e){
				System.out.println("Array is empty");
				e.printStackTrace();
			}
			
			if(nums1.length > 0){
				System.out.println("Nums1 "+Arrays.toString(nums1));
				for(int i=0;i<nums1.length;i++){
					num1List.add(nums1[i]);
				}
			}
			
			if(nums2.length > 0){
				for(int i=0;i<nums2.length;i++){
					num2List.add(nums2[i]);
				}
			}
			//create final list 
			//if(!num1List.isEmpty() && !num2List.isEmpty()){
				mergedList = new ArrayList<Integer>(num1List);
				mergedList.addAll(num2List);
			//}
			int sizeofnum1 = num1List.size();
			int sizeofnum2 = num2List.size();
			
			//sort the final concatenated array
			Collections.sort(mergedList);
			System.out.println("Concatenated List: {} "+ mergedList);
			if(mergedList.size() > 1){
				if(mergedList.size()%2 != 0){
					medianInd = mergedList.size()/2;
					System.out.println("The array is odd sized: Median Index value: "+ (int)medianInd);
					median = (double)mergedList.get((int)medianInd);
				}
				else{
					medianInd = mergedList.size()/2;
					int prevInd = (int)medianInd;
					System.out.println("Median Index: "+ medianInd);
					System.out.println("Prevind "+prevInd);
		            int nextInd = 0;
		            if(medianInd > 0.0){
		               nextInd = prevInd - 1; 
					    System.out.println("median Ind "+ medianInd);
					    System.out.println("Indices for median: "+ prevInd+" "+nextInd);
					    median = (double)(mergedList.get(prevInd)+mergedList.get(nextInd))/2;
		            }else if(medianInd == 0.0){
		                System.out.println("median Ind "+ medianInd);
					    System.out.println("Indices for median: "+ prevInd+" "+nextInd);
					    median = (double)mergedList.get(prevInd);
		            }else{
		                System.out.println("The list is empty. Median is null ");
		            }
				}
			}else if(mergedList.size()==1){
				median = (double)(mergedList.get(0));
			}
			
			System.out.println("Median value: "+ median);
			return median;
    }
}