package com.company;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.*;

//https://app.codesignal.com/interview-practice/task/s7E6FdhGKCdaCMNoL

class Range{
    public int start;
    public int end;

    public Range(int start,int end) {
        this.start = start;
        this.end = end;
    }


}

class CityNode{
    public String cityName;
    public Range range;
}

class TravelNode{
    public String cityName;
    public int stamp;

    public TravelNode(String cityName,int stamp) {
        this.stamp = stamp;
        this.cityName = cityName;
    }
}

public class FlightPlan {

    public static void main(String[] args) {
        String[][] data = { {"Orlando","Columbus","00:00","08:25"},
                            {"Orlando","Seattle","04:52","09:59"},
                            {"Orlando","Seattle","01:33","04:40"},
                            {"Orlando","Laredo","05:04","12:56"},
                            {"Laredo","Chicago","13:30","16:56"},
                            {"Laredo","Chicago","22:04","26:50"},
                            {"Columbus","Laredo","14:30","16:27"},
                            {"Columbus","Orlando","09:53","16:59"},
                            {"Orlando","Seattle","09:39","11:57"}};

        String r = flightPlan(data,"Orlando","Chicago");

        System.out.print("stop");
	    // write your code here
    }

    static int getT(String s){
        String[] parts = s.split(":");
        int stamp = Integer.parseInt(parts[0])*60 + Integer.parseInt(parts[1]);
        return stamp;
    }
    static String getS(int stamp){
        int hours = stamp/60;
        int minutes = stamp%60;
        String h = ((Integer)hours).toString();
        String m = ((Integer)minutes).toString();
        String res = "" + (h.length() == 1 ? "0"+h:h) + ":" + (m.length() == 1 ? "0"+m:m);
        return res;
    }

    public static Range getRange(String start , String end){
        return new Range(getT(start) , getT(end));
    }

    public static String flightPlan(String[][] times, String source, String dest) {

            Map<String, List<CityNode>> graph = new HashMap<String, List<CityNode>>();
            Map<String, Integer> best = new HashMap<String, Integer>();

            for(String[] flight : times){
                String key = flight[0];
                CityNode cityNode = new CityNode();
                cityNode.cityName = flight[1];
                cityNode.range = getRange(flight[2],flight[3]);

                if(graph.containsKey(key)){
                    graph.get(key).add(cityNode);
                }else {
                    LinkedList<CityNode> l = new LinkedList<CityNode>();
                    l.add(cityNode);
                    graph.put(key,l);
                }
            }

            Comparator<TravelNode> comparator = new Comparator<TravelNode>() {
                @Override
                public int compare(TravelNode o1, TravelNode o2) {
                    return o1.stamp - o2.stamp;
                }
            };

            Queue<TravelNode> q = new PriorityQueue<TravelNode>(1600,comparator);

            q.add(new TravelNode(source,0));

            while(!q.isEmpty()){
                TravelNode current = q.poll();
                if(current == null)
                    return "-1";
                if(current.cityName == dest){
                    return getS(current.stamp);
                }
                int stamp = current.stamp + (current.stamp == 0 ? 0 : 60);
                if(!graph.containsKey(current.cityName))
                    continue;
                for(CityNode node : graph.get(current.cityName)){
                    String key = node.cityName;
                    if(node.range.start >= stamp){
                        int end = node.range.end;

                        if(!best.containsKey(key) || best.get(key) > end){
                            q.add(new TravelNode(key,end));
                            if(best.containsKey(key)){
                                best.remove(key);
                            }
                            best.put(key,end);
                        }
                    }

                }
            }

        return  "-1";
    }

}
