
public class Output
{
static int i=0;

static boolean b=false;

public static int main2(int argc, String argv)
{
try{Structure s = new Structure();
s.valMap.put("a" , "1");
 s.valMap.put("b" , ToString.toString(argc));
i = Integer.parseInt(s.valMap.get("a"));
return i;
}catch(Exception e){return -1;}}

public static void main(int argc2, String m)
{
try{int result=0;
result = main2(0, "str");
}catch(Exception e){return ;}}

public static void main(String[] args){ try{main(0,"");}catch(Exception e){return ;}}
}