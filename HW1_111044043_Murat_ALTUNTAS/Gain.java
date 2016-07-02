package VeriMad_hw1_111044043;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

/**
 *
 * @author Murat ALTUNTAS
 */
public class Gain {

    public static double info(ArrayList<Integer> yesNoNum) {
        double result= 0;
        for (int i = 0; i < yesNoNum.size(); i++) {
            if (yesNoNum.get(i) == 0) {
                return 0;
            }
        }
        int total=0;
        for (int i = 0; i < yesNoNum.size(); i++) {
            total += yesNoNum.get(i);
        }
        
        for (int i = 0; i < yesNoNum.size(); i++) {
            double trueData = (double) (yesNoNum.get(i) / (double) total);
            result += -(trueData * (Math.log(trueData) / Math.log(2)));
        }
        
        return result;
    }

    public static double entropy(int attNum, int labelsIndex,
                                ArrayList<ArrayList<String>> attribute,
                                ArrayList<ArrayList<String>> devidedData) {
        ArrayList<ArrayList<Integer>> attTable = new ArrayList<ArrayList<Integer>>();
        // colNum ı disaridan veriyoruz. attribute gore kolon seciyoruz. orn: age icin colnum = 0 veriyorum
        for (int h = 0; h < attribute.get(attNum).size(); h++) {
            ArrayList<Integer> intTable = new ArrayList<Integer>();
            int yesCount = 0, noCount = 0;
            for (int i = 0; i < devidedData.size(); i++) {
                if (devidedData.get(i).get(attNum).equals(attribute.get(attNum).get(h))) {
                    if (devidedData.get(i).get(labelsIndex).equals(attribute.get(labelsIndex).get(0))) {
                        yesCount++;
                    }
                    if ((attribute.get(labelsIndex).size() > 1 ) && devidedData.get(i).get(labelsIndex).equals(attribute.get(labelsIndex).get(1))) {
                        noCount++;
                    }
                }
            }
            intTable.add(yesCount);
            intTable.add(noCount);
            attTable.add(intTable);
        }

        double infoAtt = 0;
        for (int i = 0; i < attTable.size(); i++) {
            infoAtt += ((double) (attTable.get(i).get(0) + attTable.get(i).get(1)) / (double) devidedData.size()) * info(attTable.get(i));

        }
        return infoAtt;
    }

    public static double gain(double info, double entropy) {
        return info - entropy;
    }

    public static void readFile(ArrayList<ArrayList<String>> table){
        Scanner sc2 = null;

        try {
            sc2 = new Scanner(new File("data1.txt"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        while (sc2.hasNextLine()) {
            Scanner s2 = new Scanner(sc2.nextLine());
            ArrayList<String> datas = new ArrayList<String>();
            while (s2.hasNext()) {
                String s = s2.next();
                datas.add(s);
            }
            table.add(datas);
        }
    }
    
    public static void attributes(ArrayList<ArrayList<String>> table,
                                  ArrayList<ArrayList<String>> attribute){
        for (int i = 0; i < table.get(0).size(); i++) {
            ArrayList<String> att = new ArrayList<String>();
            att.add(table.get(0).get(i));
            attribute.add(att);
        }

        for (int i = 0; i < table.size(); i++) {
            for (int j = 0; j < table.get(i).size(); j++) {
                if (attribute.get(j).contains(table.get(i).get(j)) == false) {
                    attribute.get(j).add(table.get(i).get(j));
                }
            }
        }
    }
   
    public static void gains(int labelNum, int labelsIndex,
                             ArrayList<ArrayList<String>> table,
                             ArrayList<ArrayList<String>> attribute,
                             ArrayList<Double> gains,
                             ArrayList<Integer> yesNoNum){
        double gn;
        for (int i = 0; i < table.size(); i++) {
            
        }
        for (int i = 0; i < table.get(0).size() - 1; i++) {
            double inform = info(yesNoNum);
            double ent = entropy(i,labelsIndex, attribute, table);
            gn = gain(inform, ent);
            gains.add(gn);            
        }
    }
    
    public static int bestGain(ArrayList<Double> list,
                               ArrayList<String> title,
                               int contour){
        double max = Double.MIN_VALUE;
        int index = -1;
        for(int i=0; i<list.size(); i++){
            if(list.get(i) > max){
                max = list.get(i);
                index = i;
            }
        }

        for (int i = 0; i < contour; i++) {
            System.out.print("\t");
        }
        if(index != -1)
            System.out.println(title.get(index));
        
        return index;
    }
    
    public static void rooter(ArrayList<ArrayList<String>> devidedData,
                              ArrayList<ArrayList<String>> attribute,
                              ArrayList<ArrayList<ArrayList<String>>> dataPart,
                              ArrayList<Integer> yesNoNum,
                              int colNum, int contour, int labelsIndex){

        
        if (colNum == -1) {
            int yesNum = labelNumber(devidedData, attribute, yesNoNum, labelsIndex);
            if (yesNum == devidedData.size()) {
                for (int i = 0; i < contour; i++) {
                    System.out.print("\t");
                }
                System.out.println(devidedData.get(0).get(labelsIndex));
            }
        }else{
            for (int i = 0; i < attribute.get(colNum).size(); i++) {
                ArrayList<ArrayList<String>> root = new ArrayList<ArrayList<String>>();

                for (int j = 0; j < devidedData.size(); j++) {
                    if(attribute.get(colNum).get(i).equals(devidedData.get(j).get(colNum))){
                        root.add(devidedData.get(j));
                    }
                }
                dataPart.add(root);
            }
        }
        
    }
    
    public static int labelNumber(ArrayList<ArrayList<String>> table,
                                  ArrayList<ArrayList<String>> attribute,
                                  ArrayList<Integer> yesNoNum,
                                  int labelsIndex){
        int labelNum = 0;
        for (int j = 0; j < attribute.get(labelsIndex).size(); j++) {
            labelNum = 0; // son sütundaki yes sayısı
            for (int i = 0; i < table.size(); i++) {
                if (table.get(i).get(labelsIndex).equals(attribute.get(labelsIndex).get(j))) {
                    labelNum++;

                }
            }
            yesNoNum.add(labelNum);
        }
        
        
        return labelNum;
    }
    
    public static void tree(ArrayList<ArrayList<String>> table, int contour, int labelsIndex){        
        ArrayList<ArrayList<String>> attribute = new ArrayList<ArrayList<String>>();
        ArrayList<ArrayList<ArrayList<String>>> dataPart = new ArrayList<ArrayList<ArrayList<String>>>();
        ArrayList<Double> gains = new ArrayList<Double>();
        ArrayList<String> title = new ArrayList<String>();
        ArrayList<Integer> yesNoNum = new ArrayList<Integer>();
        int colNum = 0,   // sütun sayısı
            labelNum = 0; // son sütundaki yes sayısı
        //datanın başlıklarını bu kısımda array'e doldurmanız gerekmektedir.
        title.add("age");
        title.add("income");
        title.add("student");
        title.add("credit_rating");
        title.add("buys_computer");
        
        attributes(table, attribute);
        labelNum = labelNumber(table, attribute,yesNoNum, labelsIndex);
        gains(labelNum, labelsIndex, table, attribute, gains, yesNoNum);      
        int index = bestGain(gains,title, contour);
        rooter(table, attribute, dataPart, yesNoNum, index, contour, labelsIndex);
        
        contour++;
 
        for (int i = 0; i < dataPart.size(); i++) {
            for (int j = 0; j < contour; j++) {
                System.out.print("\t");
            }
            System.out.println(dataPart.get(i).get(0).get(index));
            tree(dataPart.get(i), contour, labelsIndex);
        }
    }
    
    public static void main(String[] args) throws IOException {

        ArrayList<ArrayList<String>> table = new ArrayList<ArrayList<String>>();
        int contour = 0;
        int labelsIndex = 4;
        readFile(table);
        tree(table, contour, labelsIndex);
    }
}
