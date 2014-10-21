//Brian Moex, Ignacio Moya. FCEIA-2014

package nc.player;

import java.io.IOException;
import java.util.Random;
import jess.*;
import nc.Game;

public class JessPlayer extends AbstractPlayer {
	private final Game game;
	// private int[][] grid;

	private Rete motorJ;
	private Grilla grilla;
	private int accion;

	public JessPlayer(Random random, Game game) throws JessException {
		super(random);
		this.game = game;
		grilla = new Grilla();
		motorJ = new Rete();
		motorJ.reset();
		motorJ.batch("Jessica.clp");
	}

	@Override
	public int getAction() {

		grilla.setVector(game.getGrid());

		try {
			// resetea el motor Jess
			motorJ.reset();
			// cargar la matriz al .clp
			motorJ.add(grilla);

			// correr .clp
			motorJ.run();

		} catch (JessException e) {
			e.printStackTrace();
		}

		try {
			System.in.read();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		// obtener accion
		accion = grilla.getAccion();
		return accion;

	}

}
