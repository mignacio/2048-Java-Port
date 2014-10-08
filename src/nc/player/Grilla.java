//Brian Moex, Ignacio Moya. FCEIA-2014

package nc.player;

import java.util.Vector;

public class Grilla {
	private int accion;
	private Vector<Integer> vector;

	public Vector<Integer> getVectorE() {
		return vector;
	}

	public void setVectorE(Vector<Integer> vectorE) {
		this.vector = vectorE;
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

	public Vector<Integer> setVector(int[][] matriz) {
		for (int i = 0; i < 3; i++) {
			for (int j = 0; j < 3; j++) {
				vector.add(matriz[i][j]);
			}
		}
		return vector;

	}
}
