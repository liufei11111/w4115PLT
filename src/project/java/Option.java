import org.apache.commons.math3.distribution.NormalDistribution;

public class Option extends Structure {
    private static NormalDistribution d;
    public Option(){
        super();
        d = new NormalDistribution(0, 1);
    }
    public String toString(){
        return super.toString()+"\nPrice: "+this.price()+"\n";
    }
    public Option(double strike, double stock, double interestRate,
            double period, double sigma) {
        this.valMap.put("strike", "" + strike);
        this.valMap.put("stock", "" + stock);
        this.valMap.put("interestRate", "" + interestRate);
        this.valMap.put("period", "" + period);
        this.valMap.put("sigma", "" + sigma);
        this.valMap.put("optionType", "call");
        d = new NormalDistribution(0, 1);
    }
    public Option(double strike, double stock, double interestRate,
            double period, double sigma,String type) {
        this.valMap.put("strike", "" + strike);
        this.valMap.put("stock", "" + stock);
        this.valMap.put("interestRate", "" + interestRate);
        this.valMap.put("period", "" + period);
        this.valMap.put("sigma", "" + sigma);
        this.valMap.put("optionType", type);
        d = new NormalDistribution(0, 1);
    }
    public void setValue(String type, String value, String propertyName) {
        this.valMap.put(propertyName, value);
    }
public static Matrix priceM(Matrix strike, Matrix stock, Matrix interestRate, Matrix period, Matrix sigma){
    int row=strike.getNrows();
    int col=strike.getNcols();
    Matrix result=new Matrix(row,col);
    for (int i=0;i<row;++i){
        for (int j=0;j<col;++j){
            Option opt=new Option(strike.getValueAt(i, j)
                    ,stock.getValueAt(i, j),interestRate.getValueAt(i, j),period.getValueAt(i, j),sigma.getValueAt(i, j));
            double price=opt.price();
            result.setValueAt(i, j, price);
        }
    }
    return result;
}

    public double price() {

        double strike = Double.parseDouble(valMap.get("strike"));
        double stock = Double.parseDouble(valMap.get("stock"));
        double interestRate = Double.parseDouble(valMap.get("interestRate"));
        double period = Double.parseDouble(valMap.get("period"));
        double sigma = Double.parseDouble(valMap.get("sigma"));
        if (period == 0.0) {
            return stock - strike;
        } else if (strike == 0.0) {
            return 0.0;
        }
        double d1 = (1 / (Math.sqrt(period) * sigma))
                * (Math.log(stock) - Math.log(strike) + period
                        * (interestRate + sigma * sigma / 2));
        double d2 = (1 / (Math.sqrt(period) * sigma))
                * (Math.log(stock) - Math.log(strike) + period
                        * (interestRate - sigma * sigma / 2));
        String str = this.valMap.get("optionType");
        if (str == null || str.equals("call")) {
            double cumND1 = d.cumulativeProbability(d1);
            double cumND2 = d.cumulativeProbability(d2);

            return cumND1 * stock - cumND2 * strike
                    * Math.exp(-1 * interestRate * period);

        }else{
            double cumND1 = d.cumulativeProbability(-1*d1);
            double cumND2 = d.cumulativeProbability(-1*d2);

            return  cumND2 * strike
                    * Math.exp(-1 * interestRate * period)-cumND1 * stock;
        }
    }
    
}
