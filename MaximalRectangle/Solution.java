/* https://oj.leetcode.com/problems/maximal-rectangle/
 * solution by Pierre Galland
 * At each iteration on j :
 * bottom[i][k] is the number of lines of the largest rectangle with
 * top-left (i,j), top-right(i,j+k), extending on the bottom,
 * therefore bottom[i][k]*(k+1) is the size of this rectangle,
 * we can compare it to the current max size found
 *
 * right[i] is the number of '1' on the right from case (i,j)
 */
	
public class Solution {
    public int maximalRectangle(char[][] matrix) {
        int n = matrix.length;
        if(n==0)
            return 0;
        int m = matrix[0].length;
        if(m==0)
            return 0;
        int[] right = new int[n];
        int[] rightjp1 = new int[n];
        int[][] bottom = new int[n][m];
        int aireMax=0;
        
        for(int j=m-1;j>=0;j--){
            for(int i=n-1;i>=0;i--){
                if(matrix[i][j]=='1'){
                
                    right[i]=1+rightjp1[i];
                    
                    for(int k=0;j+k<m;k++){
                        if(right[i]>=k+1){
                            bottom[i][k]=1+(i+1>=n ? 0 : bottom[i+1][k]);
                            aireMax = bottom[i][k]*(k+1)>aireMax ? bottom[i][k]*(k+1) : aireMax;
                        }
                    }
                }
            }
            for(int r=0;r<n;r++){
                rightjp1[r]=right[r];
                right[r]=0;
            }
            for(int r=0;r<n;r++){
                for(int s=0;s<m;s++){
                    bottom[r][s]=0;
                }
            }
        }
        return aireMax;
    }
}
