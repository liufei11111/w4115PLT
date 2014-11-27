
public class Matrix {
    double[][] matrix;
    public Matrix(int x,int y){
        matrix=new double[x][y];
    }
    public void setEntry(int i,int j,double value){
        matrix[i][j]=value;
    }
    public double getEntry(int i,int j){
        return matrix[i][j];
    }
}
