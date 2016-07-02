/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package veriMad_hw2_111044043;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

/**
 *
 * @author Murat ALTUNTAS
 */
public class Bayes {
    
    private final static int labelIndex = 5;
    private final static String fileName = "data2.txt";
    private final static int continuousAtt = 4;
    
    public static void readFile(ArrayList<ArrayList<String>> table){
        Scanner sc2 = null;

        try {
            sc2 = new Scanner(new File(fileName));
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
    
    /* label sayilari */
    public static void labelNumber(String label, int curColNum, int labelsIndex,
                                   ArrayList<ArrayList<String>> table,
                                   ArrayList<ArrayList<String>> attribute,
                                   ArrayList<Integer> yesNoNum){
        int labelNum = 0;
        for (int j = 0; j < attribute.get(labelsIndex).size(); j++) {
            labelNum = 0; // son sütundaki yes sayısı
            for (int i = 0; i < table.size(); i++) {
                if(table.get(i).get(curColNum).equals(label)){
                    if (table.get(i).get(labelsIndex).equals(attribute.get(labelsIndex).get(j))) {
                        labelNum++;

                    }
                }
            }
            yesNoNum.add(labelNum);
        }
    }
    
    /* bütün labellerin genel olasılığı */
    public static void possibAllLabels(ArrayList<ArrayList<String>> table,
                                       ArrayList<ArrayList<String>> attribute,
                                       ArrayList<Integer> yesNoNum,
                                       ArrayList<Double> PCi,
                                       int labelsIndex){
        
        double total=0;
        for (int i = 0; i < attribute.get(labelsIndex).size(); i++) {
             int labelNum = 0;
             for (int j = 0; j < table.size(); j++) {
                if (table.get(j).get(labelsIndex).equals(attribute.get(labelsIndex).get(i))) {
                    ++labelNum;
                }
             }
             yesNoNum.add(labelNum);
        }
        
            for (int i = 0; i < yesNoNum.size(); i++) {
                total += yesNoNum.get(i); // total yes no sayıları
                //System.out.print(yesNoNum.get(i) + " ");
            }
            
            for (int i = 0; i < yesNoNum.size(); i++) {
                PCi.add(((double)yesNoNum.get(i)/total));
            }
    }
    
    /* mean */
    public static double mean(ArrayList <Double> marks) {
        Double sum = 0.0;
        if(!marks.isEmpty()) {
          for (Double mark : marks) {
              sum += mark;
          }
          return sum.doubleValue() / marks.size();
        }
        return sum;
     }
    
    /* standard deviation */
    public static double sd (ArrayList<Double> a){
        int sum = 0;
        double mean = mean(a);
 
        for (Double i : a)
            sum += Math.pow((i - mean), 2);
        return Math.sqrt( sum / ( a.size() - 1 ) ); // sample
    }
    
    public static double gaussian(double x, double mu, double sigma2){
        double result=0.0;
        double first, second;
        first = (1/(Math.sqrt(2 * Math.PI * sigma2)));
        second = ((Math.pow((x - mu),2))/(2 * sigma2));
        result = first * Math.pow(Math.E, (-1 * second));
        return result;
    }
    
    public static void continPred(ArrayList<ArrayList<String>> table,
                                  ArrayList<ArrayList<String>> attribute,
                                  ArrayList<Double> gauss,
                                  ArrayList<String> test){
        ArrayList <ArrayList <Double>> continList = new ArrayList <ArrayList <Double>> ();
        
        double mean, std, gaus;
        for (int j = 0; j < attribute.get(labelIndex).size(); j++) {
            ArrayList <Double> contList = new ArrayList <Double> ();
            for (int i = 0; i < table.size(); i++) {
                if (table.get(i).get(labelIndex).equals(attribute.get(labelIndex).get(j))) {
                    contList.add(Double.parseDouble(table.get(i).get(continuousAtt)));
                }
            }
            continList.add(contList);
        }
        
        for (int i = 0; i < continList.size(); i++) {
            mean = mean(continList.get(i));
            std = sd(continList.get(i));
            gaus = gaussian(Double.parseDouble(test.get(continuousAtt)), mean, Math.pow(std, 2.0));
            gauss.add(gaus);
            //System.out.println("mean: " + mean + "\nsd: " + std + "\ngaus: " + gaus);
        }
        
    }
    
    /* attribute arrayi */
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
       
    public static String allLabelsNum(ArrayList<ArrayList<String>> table,
                                    ArrayList<ArrayList<Integer>> LabelsNum,
                                    ArrayList<Integer> yesNoNumbers,
                                    ArrayList<Double> PCi,
                                    ArrayList<Double> PXCi,
                                    ArrayList<Double> PXCiPCi,
                                    ArrayList<String> test,
                                    ArrayList<Double> gauss,
                                    String resultLabel,
                                    int labelsIndex){
        ArrayList<ArrayList<String>> attribute = new ArrayList<ArrayList<String>>();
        attributes(table, attribute);
        continPred(table,attribute,gauss,test);
        
        possibAllLabels(table, attribute, yesNoNumbers, PCi, labelsIndex);
        //System.out.println("\n*****************************\n");
        for (int i = 0; i < test.size(); i++) {
            ArrayList<Integer> yesNoNum = new ArrayList<Integer>();
            labelNumber(test.get(i), i, labelsIndex, table, attribute, yesNoNum);
            LabelsNum.add(yesNoNum);            
        }
        
        for (int i = 0; i < LabelsNum.get(i).size(); i++) {
            double totalN=1;
            for (int j = 0; j < LabelsNum.size(); j++) {
                if(j != continuousAtt)
                    totalN *= (double)(LabelsNum.get(j).get(i) / (double)yesNoNumbers.get(i));
            }
            totalN *= gauss.get(i);
            PXCi.add(totalN);
        }
        
        for (int j = 0; j < PXCi.size(); j++) {
            PXCiPCi.add(PXCi.get(j)*PCi.get(j));
        }
        
        double max=Double.MIN_VALUE;
        int index=-1;
        
        for (int i = 0; i < PXCiPCi.size(); i++) {
            
            if (max < PXCiPCi.get(i)) {
                max = PXCiPCi.get(i);
                index = i;
            }
        }
        
        //System.out.println(index);
        //System.out.println(attribute.get(labelsIndex).get(index));
        resultLabel = attribute.get(labelsIndex).get(index);
        return resultLabel;
    }
    public static void main(String[] args) throws IOException {
        ArrayList<ArrayList<String>> table = new ArrayList<ArrayList<String>>();
        ArrayList<ArrayList<Integer>> LabelsNum = new ArrayList<ArrayList<Integer>>();
        ArrayList<Double> PCi = new ArrayList<Double>();
        ArrayList<Double> PXCi = new ArrayList<Double>();
        ArrayList<Double> PXCiPCi = new ArrayList<Double>();
        ArrayList<Double> gauss = new ArrayList<Double> ();
        ArrayList<Integer> yesNoNumbers = new ArrayList<Integer>();
        String resultLabel=null;
        //int labelsIndex = 4;
        
        readFile(table);
        
        for (int j = 0; j < table.size(); j++) {
            for (int i = 0; i < table.get(i).size(); i++) {
                System.out.print(table.get(j).get(i) + " ");
            }
            System.out.println("");
        }
        
        ArrayList<String> test = new ArrayList<String>();
        test.add("<=30");
        test.add("medium");
        test.add("yes");
        test.add("fair");
        test.add("80");
        
        //System.out.println("\n*****************************\n");
        
        resultLabel = allLabelsNum(table, LabelsNum, yesNoNumbers,
                                   PCi, PXCi, PXCiPCi, test, gauss,
                                   resultLabel, labelIndex);
        /*
        for (int j = 0; j < LabelsNum.size()-1; j++) {
            for (int i = 0; i < LabelsNum.get(i).size(); i++) {
                System.out.print(LabelsNum.get(j).get(i) + "  ");
            }
            System.out.println("");
        }
        
        System.out.println("\n*********** gaussian ************\n");
        for (int i = 0; i < gauss.size(); i++) {
            System.out.println(gauss.get(i) + " ");
        } 
        */
        System.out.println("\n************* PCi ************\n");
        for (int i = 0; i < PCi.size(); i++) {
            System.out.println(PCi.get(i) + " ");
        }
        System.out.println("\n************ PXCi ***********\n");
        for (int i = 0; i < PXCi.size(); i++) {
            System.out.println(PXCi.get(i) + " ");
        }
        System.out.println("\n*********** PXCi*PCi ************\n");
        for (int i = 0; i < PXCiPCi.size(); i++) {
            System.out.println(PXCiPCi.get(i) + " ");
        }
               
        System.out.println("\n*********** Result ************\n");
        System.out.println(resultLabel);
        
    }
}
