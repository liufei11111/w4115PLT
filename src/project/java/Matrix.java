public class Matrix {

    private int nrows;
    private int ncols;
    public double[][] data;

    public Matrix(double[][] dat) {
        this.data = dat;
        this.nrows = dat.length;
        this.ncols = dat[0].length;
    }

    public Matrix(int nrow, int ncol) {
        this.nrows = nrow;
        this.ncols = ncol;
        data = new double[nrow][ncol];
    }

    public int getNrows() {
        return nrows;
    }

    public void setNrows(int nrows) {
        this.nrows = nrows;
    }

    public int getNcols() {
        return ncols;
    }

    public void setNcols(int ncols) {
        this.ncols = ncols;
    }

    public double[][] getValues() {
        return data;
    }

    public void setValues(double[][] values) {
        this.data = values;
    }

    public void setValueAt(int row, int col, double value) {
        data[row][col] = value;
    }

    public double getValueAt(int row, int col) {
        return data[row][col];
    }

    public boolean isSquare() {
        return nrows == ncols;
    }

    public int size() {
        if (isSquare())
            return nrows;
        return -1;
    }

    public Matrix multiplyByConstant(double constant) {
        Matrix mat = new Matrix(nrows, ncols);
        for (int i = 0; i < nrows; i++) {
            for (int j = 0; j < ncols; j++) {
                mat.setValueAt(i, j, data[i][j] * constant);
            }
        }
        return mat;
    }

    public Matrix insertColumnWithValue1() {
        Matrix X_ = new Matrix(this.getNrows(), this.getNcols() + 1);
        for (int i = 0; i < X_.getNrows(); i++) {
            for (int j = 0; j < X_.getNcols(); j++) {
                if (j == 0)
                    X_.setValueAt(i, j, 1.0);
                else
                    X_.setValueAt(i, j, this.getValueAt(i, j - 1));

            }
        }
        return X_;
    }

    public void print() {
        for (int i = 0; i < nrows; ++i) {
            for (int j = 0; j < ncols; ++j) {
                System.out.print(data[i][j]);
                if (j != ncols - 1) {
                    System.out.print(", ");
                }
            }
            System.out.println();
        }
        System.out.println();
    }

    public Matrix addByConstant(int constant) {
        Matrix mat = new Matrix(nrows, ncols);
        for (int i = 0; i < nrows; i++) {
            for (int j = 0; j < ncols; j++) {
                mat.setValueAt(i, j, data[i][j] + constant);
            }
        }
        return mat;
    }

    public String toString() {
        String result = "{";
        for (int i = 0; i < nrows; i++) {
            result += "[";
            for (int j = 0; j < ncols; j++) {
                result += data[i][j] + ", ";
            }
            if (i == nrows - 1)
                result += "], ";
            else
                result += "]";
        }
        result+="}";
        return result;
    }

    public Matrix addByConstant(double constant) {
        Matrix mat = new Matrix(nrows, ncols);
        for (int i = 0; i < nrows; i++) {
            for (int j = 0; j < ncols; j++) {
                mat.setValueAt(i, j, data[i][j] + constant);
            }
        }
        return mat;
    }
}
