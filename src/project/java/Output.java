
public class Output
{
static double i=0.0;

public static Option main2(int argc, String argv)
{
try{Option  s = new Option();
s.valMap.put("strike" , "100.0");
 s.valMap.put("stock" , "150.0");
 s.valMap.put("interestRate" , "0.1");
 s.valMap.put("period" , "1.0");
 s.valMap.put("sigma" , "2.0");
 s.valMap.put("optionType" , "call");
i = Double.parseDouble(s.valMap.get("strike"));
return s;
}catch(Exception e){e.printStackTrace();return null;}}

public static void main(int argc2, String m)
{
try{Option  result = new Option();
result.valMap.put("b" , "1");
result = main2(0, "str");
double d=0.0;
d = (result).price();
System.out.println(ToString.toString(d));
}catch(Exception e){e.printStackTrace();return ;}}

public static void main(String[] args){ try{main(0,"");}catch(Exception e){return ;}}
}