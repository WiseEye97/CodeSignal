using System;
using System.Collections.Generic;

using System.Linq;
using System.Text;

namespace LeetCodeSpiral
{
    class Program
    {
        static ListNode addToList(ListNode tail,int v)
        {
            tail.next = new ListNode(v);
            return tail.next;
        }

        static void Main(string[] args)
        {
            var s = new Solution();

            var list = new ListNode(-10);
            var tail = list;
            var nums = new []{ -3, 0, 5, 9 };
            foreach(var num in nums)
            {
                tail = addToList(tail, num);
            }
            var result = s.sortedListToBST(list);
            Console.WriteLine("Hello World!");
        }
    }

    public class ListNode
    {
       public int val;
       public ListNode next;
       public ListNode(int x) { val = x; }
    }

    public class TreeNode
    {
       public int val;
       public TreeNode left;
       public TreeNode right;
       public TreeNode(int x) { val = x; }
  }

    class Solution
    {   
        public ListNode findMiddle(ListNode list)
        {
            ListNode slow = list, fast = list,prev = null;
         
            while (fast != null && fast.next != null)
            {
                prev = slow;
                slow = slow.next;
                fast = fast.next;
                if (fast == null)
                    break;
                fast = fast.next;
            }
            if(prev != null)
                prev.next = null;
            return slow;
        }

        public TreeNode ConstructBST(ListNode list)
        {
            if (list == null)
                return null;
            if (list.next == null)
                return new TreeNode(list.val);
            var middle = findMiddle(list);

            var root = new TreeNode(middle.val);

            root.left = ConstructBST(list);
            root.right = ConstructBST(middle.next);

            return root;
        }

        public TreeNode sortedListToBST(ListNode head)
        {
            return ConstructBST(head);

        }
    }
}
