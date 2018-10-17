import java.util.*;
import java.io.*;

public class interpreter {

  static Map <String, String> myMap = new HashMap <String, String>();
  static Map <String, String> letMap = new HashMap <String, String>();
  static ArrayList<Map <String, String>> arrayOfMaps = new ArrayList<Map <String, String>>();
  static Map<String, ArrayList<String>> mapOfFunctions = new HashMap<String, ArrayList<String>>();
  static int letCount = -1;
  static int funCount = -1;
  static int letFun = 0;

  public static boolean isInteger(String s){

    for(int i = 0; i < s.length(); ++i){
      char c = s.charAt(i);
      if((s.charAt(0)== '-') || (Character.isDigit(c))) continue;
      else return false;
    }
    return true;
  }

  public static boolean isBool(String s){
    return (s.contains("true") || s.contains("false"));
  }

  public static boolean containSpecialChars(String s){
    String specials = "~!@#$%^&*()_+`={:}|[]''`<>?,./";
    for(int k = 0; k < s.length(); k++){
      char x = s.charAt(k);
      for(int l = 0; l < specials.length(); l++){
        char y = specials.charAt(l);
        if(x == y ) return true;
      }
    }
    return false;
  }

  public static boolean containsGlobally(String key, ArrayList<Map <String, String>> globalMap){
    for(int i = letCount; i > -1; i--){
      if(globalMap.get(i).containsKey(key)){
        return true;
      }
    }
    return myMap.containsKey(key);
  }

  public static String valueInMap(String key, ArrayList<Map <String, String>> globalMap){
    for(int i = letCount; i > -1; i--){
      if(globalMap.get(i).containsKey(key)){
        return globalMap.get(i).get(key);
      }
    }
    if(myMap.containsKey(key)) return myMap.get(key);
    return "";
  }

  public static void add(ArrayList<String> stac){

    if(stac.size() > 1){
      String topElement = stac.get(stac.size() - 2);
      String secondElement = stac.get(stac.size() -1);

      if(!isInteger(topElement) && !topElement.contains(":") && containsGlobally(topElement, arrayOfMaps)){
        topElement = valueInMap(topElement, arrayOfMaps);
      }
      if(!isInteger(secondElement) && !secondElement.contains(":") && containsGlobally(secondElement, arrayOfMaps)){
        secondElement = valueInMap(secondElement, arrayOfMaps);
      }

      if(isInteger(topElement) && isInteger(secondElement)){
        int sum = Integer.parseInt(topElement) + Integer.parseInt(secondElement);
        stac.remove(stac.size() - 2);
        stac.remove(stac.size() - 1);
        stac.add(Integer.toString(sum));
      }
      // else if(myMap.containsKey(topElement) || myMap.containsKey(secondElement)){
      //
      // }
      else{
        stac.add(":error:");
      }
    }
    else stac.add(":error:");
  }

  public static void sub(ArrayList<String> stac){

    if(stac.size() > 1){
      String topElement = stac.get(stac.size() -2);
      String secondElement = stac.get(stac.size() -1);

      if(!isInteger(topElement) && !topElement.contains(":") && containsGlobally(topElement, arrayOfMaps)){
        topElement = valueInMap(topElement, arrayOfMaps);
      }
      if(!isInteger(secondElement) && !secondElement.contains(":") && containsGlobally(secondElement, arrayOfMaps)){
        secondElement = valueInMap(secondElement, arrayOfMaps);
      }

      if(isInteger(topElement) && isInteger(secondElement)){
        int difference = Integer.parseInt(topElement) - Integer.parseInt(secondElement);
        stac.remove(stac.size() -2);
        stac.remove(stac.size() -1);
        stac.add(Integer.toString(difference));
      }
      else{
        stac.add(":error:");
      }
    }
    else stac.add(":error:");
  }

  public static void mul(ArrayList<String> stac){

    if(stac.size() > 1){
      String topElement = stac.get(stac.size() -2);
      String secondElement = stac.get(stac.size() -1);

      if(!isInteger(topElement) && !topElement.contains(":") && containsGlobally(topElement, arrayOfMaps)){
        topElement = valueInMap(topElement, arrayOfMaps);
      }
      if(!isInteger(secondElement) && !secondElement.contains(":") && containsGlobally(secondElement, arrayOfMaps)){
        secondElement = valueInMap(secondElement, arrayOfMaps);
      }

      if(isInteger(topElement) && isInteger(secondElement)){
        int product = Integer.parseInt(topElement) * Integer.parseInt(secondElement);
        stac.remove(stac.size() -2);
        stac.remove(stac.size() -1);
        stac.add(Integer.toString(product));
      }
      else{
        stac.add(":error:");
      }
    }
    else stac.add(":error:");
  }

  public static void div(ArrayList<String> stac){

    if(stac.size() > 1){
      String topElement = stac.get(stac.size() -2);
      String secondElement = stac.get(stac.size() -1);

      if(!isInteger(topElement) && !topElement.contains(":") && containsGlobally(topElement, arrayOfMaps)){
        topElement = valueInMap(topElement, arrayOfMaps);
      }
      if(!isInteger(secondElement) && !secondElement.contains(":") && containsGlobally(secondElement, arrayOfMaps)){
        secondElement = valueInMap(secondElement, arrayOfMaps);
      }

      if(isInteger(topElement) && isInteger(secondElement)){

        if(Integer.parseInt(secondElement) == 0){
          stac.add(":error:");
          return;
        }

        int quotient = Integer.parseInt(topElement) / Integer.parseInt(secondElement);
        stac.remove(stac.size() -2);
        stac.remove(stac.size() -1);
        stac.add(Integer.toString(quotient));
      }
      else{
        stac.add(":error:");
      }
    }
    else stac.add(":error:");
  }

  public static void rem(ArrayList<String> stac){

    if(stac.size() > 1){
      String topElement = stac.get(stac.size() -2);
      String secondElement = stac.get(stac.size() -1);

      if(!isInteger(topElement) && !topElement.contains(":") && containsGlobally(topElement, arrayOfMaps)){
        topElement = valueInMap(topElement, arrayOfMaps);
      }
      if(!isInteger(secondElement) && !secondElement.contains(":") && containsGlobally(secondElement, arrayOfMaps)){
        secondElement = valueInMap(secondElement, arrayOfMaps);
      }

      if(isInteger(topElement) && isInteger(secondElement)){
        if(Integer.parseInt(topElement) == 0){
          stac.add(":error:");
          return;
        }

        int remainder = Integer.parseInt(topElement) % Integer.parseInt(secondElement);
        stac.remove(stac.size() -2);
        stac.remove(stac.size() -1);
        stac.add(Integer.toString(remainder));
      }
      else{
        stac.add(":error:");
      }
    }
    else stac.add(":error:");
  }

  public static void neg(ArrayList<String> stac){

    if(stac.size() > 0){
      String topElement = stac.get(stac.size() -1);

      if(!isInteger(topElement) && !topElement.contains(":") && containsGlobally(topElement, arrayOfMaps)){
        topElement = valueInMap(topElement, arrayOfMaps);
      }

      if(isInteger(topElement)){
        int negation = 0 - Integer.parseInt(topElement);
        stac.remove(stac.size() -1);
        stac.add(Integer.toString(negation));
      }
      else{
        stac.add(":error:");
      }
    }
    else stac.add(":error:");
  }

  public static void swap(ArrayList<String> stac){

    if(stac.size() > 1){
      String topElement = stac.get(stac.size() -2);
      String secondElement = stac.get(stac.size() -1);

      stac.remove(stac.size() -2);
      stac.remove(stac.size() -1);

      stac.add(secondElement);
      stac.add(topElement);
    }
    else stac.add(":error:");
  }

  public static void and(ArrayList<String> stac){

    if(stac.size() > 1){
      String topElement = stac.get(stac.size() - 2);
      String secondElement = stac.get(stac.size() -1);

      if(!isInteger(topElement) && !topElement.contains(":") && containsGlobally(topElement, arrayOfMaps)){
        topElement = valueInMap(topElement, arrayOfMaps);
      }
      if(!isInteger(secondElement) && !secondElement.contains(":") && containsGlobally(secondElement, arrayOfMaps)){
        secondElement = valueInMap(secondElement, arrayOfMaps);
      }

      if(isBool(topElement) && isBool(secondElement)){
        stac.remove(stac.size() - 2);
        stac.remove(stac.size() - 1);
        if (topElement.equals(secondElement)) stac.add(topElement);
        else stac.add(":false:");
      }
      else{
        stac.add(":error:");
      }
    }
    else stac.add(":error:");
  }

  public static void or(ArrayList<String> stac){

    if(stac.size() > 1){
      String topElement = stac.get(stac.size() - 2);
      String secondElement = stac.get(stac.size() -1);

      if(!isInteger(topElement) && !topElement.contains(":") && containsGlobally(topElement, arrayOfMaps)){
        topElement = valueInMap(topElement, arrayOfMaps);
      }
      if(!isInteger(secondElement) && !secondElement.contains(":") && containsGlobally(secondElement, arrayOfMaps)){
        secondElement = valueInMap(secondElement, arrayOfMaps);
      }

      if(isBool(topElement) && isBool(secondElement)){
        stac.remove(stac.size() - 2);
        stac.remove(stac.size() - 1);
        if (topElement.equals(secondElement)) stac.add(topElement);
        else stac.add(":true:");
      }
      else{
        stac.add(":error:");
      }
    }
    else stac.add(":error:");
  }

  public static void not(ArrayList<String> stac){

    if(stac.size() > 0){
      String topElement = stac.get(stac.size() -1);

      if(!isInteger(topElement) && !topElement.contains(":") && containsGlobally(topElement, arrayOfMaps)){
        topElement = valueInMap(topElement, arrayOfMaps);
      }

      if(isBool(topElement)){
        stac.remove(stac.size() - 1);
        if (topElement.equals(":true:")) stac.add(":false:");
        else stac.add(":true:");
      }
      else{
        stac.add(":error:");
      }
    }
    else stac.add(":error:");
  }

  public static void equal(ArrayList<String> stac){

    if(stac.size() > 1){
      String topElement = stac.get(stac.size() - 2);
      String secondElement = stac.get(stac.size() -1);

      if(!isInteger(topElement) && !topElement.contains(":") && containsGlobally(topElement, arrayOfMaps)){
        topElement = valueInMap(topElement, arrayOfMaps);
      }
      if(!isInteger(secondElement) && !secondElement.contains(":") && containsGlobally(secondElement, arrayOfMaps)){
        secondElement = valueInMap(secondElement, arrayOfMaps);
      }

      if(isInteger(topElement) && isInteger(secondElement)){
        stac.remove(stac.size() - 2);
        stac.remove(stac.size() - 1);
        if(Integer.parseInt(topElement) == Integer.parseInt(secondElement)) stac.add(":true:");
        else stac.add(":false:");
      }
      else{
        stac.add(":error:");
      }
    }
    else stac.add(":error:");
  }

  public static void lessThan(ArrayList<String> stac){

    if(stac.size() > 1){
      String topElement = stac.get(stac.size() - 2);
      String secondElement = stac.get(stac.size() -1);

      if(!isInteger(topElement) && !topElement.contains(":") && containsGlobally(topElement, arrayOfMaps)){
        topElement = valueInMap(topElement, arrayOfMaps);
      }
      if(!isInteger(secondElement) && !secondElement.contains(":") && containsGlobally(secondElement, arrayOfMaps)){
        secondElement = valueInMap(secondElement, arrayOfMaps);
      }

      if(isInteger(topElement) && isInteger(secondElement)){
        stac.remove(stac.size() - 2);
        stac.remove(stac.size() - 1);
        if(Integer.parseInt(topElement) < Integer.parseInt(secondElement)) stac.add(":true:");
        else stac.add(":false:");
      }
      else{
        stac.add(":error:");
      }
    }
    else stac.add(":error:");
  }

  public static void iff(ArrayList<String> stac){

    if(stac.size() > 2){
      String topElement = stac.get(stac.size() - 3);
      String secondElement = stac.get(stac.size() - 2);
      String thirdElement = stac.get(stac.size() -1);

      if(!isInteger(topElement) && !topElement.contains(":") && containsGlobally(topElement, arrayOfMaps)){
        topElement = valueInMap(topElement, arrayOfMaps);
      }
      if(!isInteger(secondElement) && !secondElement.contains(":") && containsGlobally(secondElement, arrayOfMaps)){
        secondElement = valueInMap(secondElement, arrayOfMaps);
      }

      if(isBool(topElement)){
        stac.remove(stac.size() - 3);
        stac.remove(stac.size() - 2);
        stac.remove(stac.size() - 1);
        if(topElement.equals(":true:")) stac.add(thirdElement);
        else stac.add(secondElement);
      }
      else{
        stac.add(":error:");
      }
    }
    else stac.add(":error:");
  }

  public static void bind(ArrayList<String> stac){

    if(stac.size() > 1 && letCount == -1){

      String topElement = stac.get(stac.size() - 2);
      String secondElement = stac.get(stac.size() -1);
      // top must be a name (it's the key) second could be anything valid (value)
      if(!isInteger(topElement) && !topElement.contains(":") && !secondElement.equals(":error:") && myMap.containsKey(secondElement)){
        stac.remove(stac.size() - 2);
        stac.remove(stac.size() - 1);
        myMap.put(topElement, myMap.get(secondElement));
        stac.add(":unit:");
        return;
        // topElement = myMap.get(secondElement);
      }
      if(!isInteger(topElement) && !topElement.contains(":") && !secondElement.equals(":error:")){
        stac.remove(stac.size() - 2);
        stac.remove(stac.size() - 1);
        myMap.put(topElement, secondElement);
        stac.add(":unit:");
      }
      else{
        stac.add(":error:");
      }
    }
    else if(stac.size() > 1 && letCount > -1){

      String topElement = stac.get(stac.size() - 2);
      String secondElement = stac.get(stac.size() -1);

      letMap = arrayOfMaps.get(letCount);
      // top must be a name (it's the key) second could be anything valid (value)
      if(!isInteger(topElement) && !topElement.contains(":") && !secondElement.equals(":error:") && letMap.containsKey(secondElement)){
        stac.remove(stac.size() - 2);
        stac.remove(stac.size() - 1);
        letMap.put(topElement, letMap.get(secondElement));
        stac.add(":unit:");
        return;
        // topElement = myMap.get(secondElement);
      }
      if(!isInteger(topElement) && !topElement.contains(":") && !secondElement.equals(":error:")){
        stac.remove(stac.size() - 2);
        stac.remove(stac.size() - 1);
        letMap.put(topElement, secondElement);
        stac.add(":unit:");
      }
      else{
        stac.add(":error:");
      }
    }
    else stac.add(":error:");
  }

  public static String[] call(ArrayList<String> stac){
    if(stac.size() > 1){
      String topElement = stac.get(stac.size() - 2);
      String secondElement = stac.get(stac.size() -1);

      if(!isInteger(topElement) && !topElement.contains(":") && containsGlobally(topElement, arrayOfMaps)){
        topElement = valueInMap(topElement, arrayOfMaps);
      }
      if(!isInteger(secondElement) && !secondElement.contains(":") && containsGlobally(secondElement, arrayOfMaps)){
        secondElement = valueInMap(secondElement, arrayOfMaps);
      }

      if(!isInteger(secondElement) && !secondElement.contains(":") && !topElement.equals(":error:")){
        stac.remove(stac.size() - 2);
        stac.remove(stac.size() - 1);
        return new String[] {secondElement, topElement}; //secondElement ->> funName ; topElement ->> argument
      }
      // else if(myMap.containsKey(topElement) || myMap.containsKey(secondElement)){
      //
      // }
      else{
        stac.add(":error:");
        return new String[] {"invalid", "invalid"};
      }
    }
    else stac.add(":error:");
    return new String[] {"invalid", "invalid"};
  }

  public static void interpreter(String inFile, String outFile){

    String sentence = null;
    Vector<String> myVector = new Vector<String>();
    ArrayList<String> myStack = new ArrayList<String>();
    ArrayList<ArrayList<String>> arrayOfArrays = new ArrayList<ArrayList<String>>();
    ArrayList<String> letArray = new ArrayList<String>();
    ArrayList<String> funArray = new ArrayList<String>();
    int currentStackSize = 0;

    try{

      FileReader fr = new FileReader(inFile);
      BufferedReader br = new BufferedReader(fr);

      FileWriter fw = new FileWriter(outFile);
      BufferedWriter bw = new BufferedWriter(fw);

      while((sentence = br.readLine()) != null) myVector.add(sentence);

      String inputLine = "";
      String funName = "";

      for(int i = 0; i < myVector.size(); i++){

        inputLine = myVector.get(i);
        Scanner scanner = new Scanner(inputLine);
        String firstWord = scanner.next();

        if(firstWord.equals("fun")){
          funCount++;
          letFun--;
          funName = scanner.next();
          String arg = scanner.next();
          funArray = new ArrayList<String>();
          funArray.add(arg);
          continue;
        }
        if(firstWord.equals("funEnd") && letCount < 0){
          funCount--;
          mapOfFunctions.put(funName, funArray);
          myStack.add(":unit:");
        }
        else if(firstWord.equals("funEnd") && letCount > -1){
          funCount--;
          mapOfFunctions.put(funName, funArray);
          letArray.add(":unit:");
        }

        if(funCount > -1){
          if(firstWord.equals("push")){
            String secondWord = scanner.next();
            if(containsGlobally(secondWord, arrayOfMaps)){
              secondWord = valueInMap(secondWord, arrayOfMaps);
              inputLine = "push "+ secondWord;
            }
          }
          else if(firstWord.equals(":true:")) inputLine = ":true:";
          else if(firstWord.equals(":false")) inputLine = ":false:";
          funArray.add(inputLine);
          continue;
        }
        // if(firstWord.equals("call") && funCount > -1) funArray.add("call");
        if(firstWord.equals("call") && funCount == -1){
          String parameter[] = call(myStack);
          if(parameter[0] != "invalid" && parameter[1] != "invalid"){
            if(mapOfFunctions.containsKey(parameter[0])){
              ArrayList<String> temp = new ArrayList<String>();
              temp = mapOfFunctions.get(parameter[0]);

              for(int j = temp.size() - 1; j > -1; j--){
                String replaced;
                // if(temp.get(j).startsWith("push") && temp.get(j).substring(5).equals(temp.get(0))) replaced = "push "+ parameter[1];
                // else if(temp.get(j).startsWith("push") && temp.get(j).substring(5).length() > 1) replaced = temp.get(j).replaceAll(temp.get(0), parameter[1]);
                // else replaced = temp.get(j);
                replaced = temp.get(j).replaceAll(temp.get(0), parameter[1]);
                temp.remove(j); temp.add(j, replaced);
                if(j != 0) myVector.add(i + 1, temp.get(j));
              }
            }
            else{
              myStack.add(parameter[1]);
              myStack.add(parameter[0]);
              myStack.add(":error:");
          }
            if(funCount == -1) currentStackSize = myStack.size();
          }
        }

        if(firstWord.equals("return") && !mapOfFunctions.containsKey("add1")){
          if(myStack.size() > 1) while(myStack.size() != currentStackSize + 1) myStack.remove(currentStackSize);
        }

        if(letCount == -1 && funCount == -1){

          if(firstWord.equals("push") && inputLine.endsWith("\"")){
            myStack.add(inputLine.substring(6, inputLine.length() - 1));
            continue;
          }
          if(firstWord.equals("push") && !scanner.hasNext()) myStack.add(":error:");
          if(scanner.hasNext()){
            String secondWord = scanner.next();

            if(firstWord.equals("push")){
              if(secondWord.equals("-0")) myStack.add("0");
              else if(containSpecialChars(secondWord)) myStack.add(":error:");
              else myStack.add(secondWord);
            }
          }

          if(firstWord.equals("pop")){
            if(myStack.isEmpty()) myStack.add(":error:");
            else myStack.remove(myStack.size()-1);
          }

          if(firstWord.equals(":true:")) myStack.add(":true:");
          if(firstWord.equals(":false:")) myStack.add(":false:");
          if(firstWord.equals(":error:")) myStack.add(":error:");

          if(firstWord.equals("add")) add(myStack);
          if(firstWord.equals("sub")) sub(myStack);
          if(firstWord.equals("mul")) mul(myStack);
          if(firstWord.equals("div")) div(myStack);
          if(firstWord.equals("rem")) rem(myStack);
          if(firstWord.equals("neg")) neg(myStack);
          if(firstWord.equals("swap")) swap(myStack);
          if(firstWord.equals("and")) and(myStack);
          if(firstWord.equals("or")) or(myStack);
          if(firstWord.equals("not")) not(myStack);
          if(firstWord.equals("equal")) equal(myStack);
          if(firstWord.equals("lessThan")) lessThan(myStack);
          if(firstWord.equals("bind")) bind(myStack);
          if(firstWord.equals("if")) iff(myStack);
        }

        else if(letCount > -1){ //when the letCount is > -1

          if(firstWord.equals("push") && inputLine.endsWith("\"")){
            letArray.add(inputLine.substring(6, inputLine.length() - 1));
            continue;
          }
          if(firstWord.equals("push") && !scanner.hasNext()){ letArray.add("");}
          if(scanner.hasNext()){
            String secondWord = scanner.next();

            if(firstWord.equals("push")){
              if(secondWord.equals("-0")) letArray.add("0");
              else if(containSpecialChars(secondWord)) letArray.add(":error:");
              else letArray.add(secondWord);
            }
          }

          if(firstWord.equals("pop")){
            if(letArray.isEmpty()) letArray.add(":error:");
            else letArray.remove(letArray.size() -1);
          }

          if(firstWord.equals(":true:")) letArray.add(":true:");
          if(firstWord.equals(":false:")) letArray.add(":false:");
          if(firstWord.equals(":error:")) letArray.add(":error:");

          if(firstWord.equals("add")) add(letArray);
          if(firstWord.equals("sub")) sub(letArray);
          if(firstWord.equals("mul")) mul(letArray);
          if(firstWord.equals("div")) div(letArray);
          if(firstWord.equals("rem")) rem(letArray);
          if(firstWord.equals("neg")) neg(letArray);
          if(firstWord.equals("swap")) swap(letArray);
          if(firstWord.equals("and")) and(letArray);
          if(firstWord.equals("or")) or(letArray);
          if(firstWord.equals("not")) not(letArray);
          if(firstWord.equals("equal")) equal(letArray);
          if(firstWord.equals("lessThan")) lessThan(letArray);
          if(firstWord.equals("bind")) bind(letArray);
          if(firstWord.equals("if")) iff(letArray);


          if(firstWord.equals("end") && letCount > 0){

            String toStore = letArray.get(letArray.size() -1);
            arrayOfArrays.get(letCount - 1).add(toStore);
            arrayOfArrays.remove(arrayOfArrays.size() -1);
            letArray = arrayOfArrays.get(letCount - 1);
            letCount--;
          }

          else if(firstWord.equals("end") && letCount == 0){
            String toStore = letArray.get(letArray.size() -1);
            myStack.add(toStore);
            letCount--;
            if(letFun == 0){
              mapOfFunctions.clear();
            }
          }

        }

        if(firstWord.equals("let")){
          letCount++;
          arrayOfArrays.add(new ArrayList<String>());
          arrayOfMaps.add(new HashMap <String, String>());
          letArray = arrayOfArrays.get(letCount);
          letFun++;
        }

        if(firstWord.equals("quit")){
          for(int j = myStack.size() -1; j > -1; --j){
            bw.write(myStack.get(j));
            bw.newLine();
          }
        }

      }

      br.close();
      bw.close();
    }

    catch(FileNotFoundException ex){
    }
    catch(IOException e){
    }
    catch(EmptyStackException empStack){
    }
  }

  public static void main(String[] args) {

    interpreter("input.txt", "output.txt");

  }
}
