import java.util.HashMap;


public class Structure {
    public HashMap<String,String> valMap;
//    HashMap<String,String> typeMap;
    public Structure (){
        valMap=new HashMap<String,String>();
//        typeMap=new HashMap<String,String>();
    }
    public String toString(){
        String result="{ ";
        int count=0;
        int size=this.valMap.keySet().size();
        for (String key:this.valMap.keySet()){
            result+=key+":"+this.valMap.get(key);
            if (count!=size-1)
                result+=", ";
           count++;
        }
        result+="}";
        return result;
        
    }
//    public HashMap<String,String> accessElement(String id){
//        if (valMap.containsKey(id)){
//            String val=valMap.get(id);
////            String type=typeMap.get(id);
//            HashMap<String,String> result=new HashMap<String,String>();
//            result.put(type,val);
//            return result;
//        }else return null;
//    }
    
}
