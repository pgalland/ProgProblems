/*
*https://oj.leetcode.com/problems/combinations/
*Simple recursive solution
*/
public class Solution {
    public List<List<Integer>> combine(int n, int k) {
        if(k==0){
            List<List<Integer>> l=new LinkedList<List<Integer>>();
            l.add(new LinkedList<Integer>());
            return l;
        }
        if(k>n)
            return new LinkedList<List<Integer>>();
        
        List<List<Integer>> aux1=combine(n-1,k);
        List<List<Integer>> aux2=combine(n-1,k-1);
        for(List<Integer> shortTuple : aux2){
            shortTuple.add(n);
            aux1.add(shortTuple);
        }
        return aux1;
    }
}
