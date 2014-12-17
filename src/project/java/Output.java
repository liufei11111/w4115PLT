
public class Output
{
static int i=0;

static boolean b=false;

public static Structure main2(int argc, String argv)
{
try{Structure s = new Structure();
s.valMap.put("a" , "1");
 s.valMap.put("b" , ToString.toString(argc));
i = Integer.parseInt(s.valMap.get("a"));
return s;
}catch(Exception e){return null;}}

public static void main(int argc2, String m)
{
try{Structure result = new Structure();
;
result = main2(0, "str");
System.out.println(ToString.toString(result));
}catch(Exception e){return ;}}

public static void main(String[] args){ try{main(0,"");}catch(Exception e){return ;}}
}