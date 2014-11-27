import org.apache.commons.math3.distribution.NormalDistribution;

public class Option extends Struct {
    private static NormalDistribution d;

    public Option(double strike, double stock, double interestRate,
            double period, double sigma) {
        this.valMap.put("strike", "" + strike);
        this.typeMap.put("strike", "double");

        this.valMap.put("stock", "" + stock);
        this.typeMap.put("stock", "double");

        this.valMap.put("interestRate", "" + interestRate);
        this.typeMap.put("interestRate", "double");

        this.valMap.put("period", "" + period);
        this.typeMap.put("period", "double");

        this.valMap.put("sigma", "" + sigma);
        this.typeMap.put("sigma", "double");

        this.valMap.put("optionType", "call");
        this.typeMap.put("optionType", "String");
        d = new NormalDistribution(0, 1);
    }

    public void setValue(String type, String value, String propertyName) {
        this.typeMap.put(propertyName, type);
        this.valMap.put(propertyName, value);
    }

    public double price() {

        double strike = Double.parseDouble(valMap.get("strike"));
        double stock = Double.parseDouble(valMap.get("stoke"));
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
