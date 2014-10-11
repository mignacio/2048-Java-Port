//Brian Moex, Ignacio Moya. FCEIA-2014

package nc.player;

import java.util.Vector;

public class Grilla {
	private int accion;
	private Vector<Integer> vector;

	public Vector<Integer> getVector() {
		return vector;
	}

	public void setVector(Vector<Integer> vector) {
		this.vector = vector;
	}

	public Grilla() {
		vector = new Vector<Integer>();
	}

	public void setAccion(int accion) {
		this.accion = accion;
	}

	public int getAccion() {
		return accion;
	}

	public void setVector(int[][] matriz) {
		for (int j = 0; j <= 3; j++) {
			for (int i = 0; i <= 3; i++) {
				vector.insertElementAt(matriz[j][i], (i+(4*j)));
			}
		}

	}
}
