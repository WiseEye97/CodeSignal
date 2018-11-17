package com.company;

import java.util.HashMap;
import java.util.Map;

public class Main {

    static class NodeV{
        public double distance = -1*Double.POSITIVE_INFINITY;
        public int parent = -1;

        public NodeV(double distance,int parent){
            this.distance = distance;
            this.parent = parent;
        }
        public NodeV(){}
    }

    public static void main(String[] args) {
        double[][] exchange = {{1.0,0.5,0.2},
                               {1.8,1.0,0.5},
                               {4.05,1.2,1.0}};
        boolean r = currencyArbitrage(exchange);

        System.out.print("All STOP");
    }


    public static boolean currencyArbitrage(double[][] exchange) {
        int vertices_cnt = exchange.length;
            Map<Integer, NodeV> map = new HashMap<Integer, NodeV>();

            for (int i = 0; i < vertices_cnt; i++){
                if(i == 0){
                    map.put(i,new NodeV(1.0,-1));
                }else{
                    map.put(i, new NodeV());
                }

            }

            for (int i = 0; i < vertices_cnt; i++) {
                for (int j = 0; j < vertices_cnt; j++) {
                    for (int k = 0; k < vertices_cnt; k++) {
                        if (k == j)
                            continue;

                        double weight = exchange[j][k];

                        double dv = map.get(k).distance, du = map.get(j).distance;

                        if ((dv == -1*Double.POSITIVE_INFINITY && du == -1*Double.POSITIVE_INFINITY) || du == -1*Double.POSITIVE_INFINITY)
                            continue;

                        double nw = (du < 0.000001 ? 0.0 : du * weight);
                        if (dv < nw) {
                            if (i == vertices_cnt - 1)
                                return true;
                            NodeV n = new NodeV();
                            n.distance = nw;
                            n.parent = j;
                            map.replace(k, n);
                        }
                    }
                }
            }

        return false;
    }
}
