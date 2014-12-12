/**
 *    Int -> "int"
    | Float -> "double"
    | String -> "String"
    | Matrix -> "Matrix"
    | Option -> "Option"
    | Structure -> "Structure"
    | Boolean -> "boolean"
    | Void -> "void"
 * **/
public class ToString {
    public static String toString(int a){
        return ""+a;
    }
    public static String toString(double a){
        return ""+a;
    }
    public static String toString(String a){
        return a;
    }
    public static String toString(Matrix a){
        return a.toString();
    }
    public static String toString(Option a){
        return a.toString();
    }
    public static String toString(Structure a){
        return a.toString();
    }
    public static String toString(boolean a){
        return ""+a;
    }

}
