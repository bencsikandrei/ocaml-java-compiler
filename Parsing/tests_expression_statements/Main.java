import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Calendar;

import dev4a.competition.CompetitionException;
import dev4a.competition.ExistingCompetitionException;
import dev4a.competitor.Competitor;
import dev4a.competitor.ExistingCompetitorException;
import dev4a.db.RootManager;
import dev4a.exceptions.AuthenticationException;
import dev4a.exceptions.BadParametersException;
import dev4a.graphicalview.CLIBettingSoft;
import dev4a.graphicalview.CLIClient;
import dev4a.subscriber.ExistingSubscriberException;
import dev4a.subscriber.SubscriberException;
import dev4a.system.BettingSystem;
import dev4a.utils.DatabaseConnection;

public class Main {
	/* the managers pass */
	private static String managerPass;
	/* just a defautl password */
	private static String tempPass = "1234";
	
	public static void main(String[] args) {
		/* open up the system */
		try {
			managerPass = RootManager.getPassword();
		} catch (SQLException sqlex) {
			System.out.println("Could not connect to DB!");
		}
		if( managerPass == null) {
			/* is it a new system , a fresh DB ? */
			System.out.println("New system! Welcome, default pass is 1234. Please change it!\nDatabase was truncated");
			managerPass = "1234";
			try {
				initializeDatabase();
				/* persist the first pass */
				RootManager.persist(managerPass);
			} catch (SQLException e) {
				System.out.println("Could not connect to DB!");
			}
		}
		/* properly initialized */
		final BettingSystem bettingSystem = new BettingSystem(managerPass);
		
		/* fire up the CLI */
		/* it being a prototype, we have only one main and a single program
		 * to launch the two CLIs we just swap out one of them with the other
		 * uncomment CLIBETTINGSOFT for a manager CLI (comment the other)
		 * uncomment CLIENTBETTINGSOFT for a client CLI (comment the other)
		 */
		//CLIBettingSoft cli = new CLIBettingSoft(bettingSystem);
		CLIClient cli = new CLIClient(bettingSystem);
	}
	/* TESTS */
	/*try {
		java.lang.System.out.println("Truncating ...");
		initializeDatabase();
	} catch (SQLException ex) {
		ex.printStackTrace();
	}
	addSubscribers(bettingSystem);
	
	addCompetitions(bettingSystem);

	addCompetitor(bettingSystem);*/

	private static void initializeDatabase() throws SQLException {
		/* open the connection */
		Connection conn = DatabaseConnection.getConnection();
		/* create the delete query */
		Statement sTruncate = conn.createStatement();
		/* clean up */
		sTruncate.executeUpdate("TRUNCATE subscriber CASCADE");
		sTruncate.executeUpdate("TRUNCATE bet CASCADE");
		sTruncate.executeUpdate("TRUNCATE competition CASCADE");
		sTruncate.executeUpdate("TRUNCATE competitor CASCADE");
		sTruncate.executeUpdate("TRUNCATE participant CASCADE");
		sTruncate.executeUpdate("TRUNCATE manager CASCADE");
		
		sTruncate.close();
		conn.close();

	}

	private static void addCompetitions(BettingSystem bettingSystem) {
		try {
			/* make some competitiors */
			java.util.ArrayList<Competitor> listOfCompetitors = new java.util.ArrayList<Competitor>();
			
			listOfCompetitors.add(bettingSystem.createCompetitor("Ronaldo","Cristiano", "1985-02-05", managerPass));
			listOfCompetitors.add(bettingSystem.createCompetitor("Iniesta","Andres", "1984-05-11", managerPass));

			bettingSystem.addCompetition("Real_Madrid_-_Barcelona_Primera_Division", Calendar.getInstance(), listOfCompetitors, managerPass);
			bettingSystem.addCompetition("Real_Madrid_-_Barcelona_Primera_Division_1", Calendar.getInstance(), listOfCompetitors, managerPass);
			
			//bettingSystem.printCompetitions();
			
		} catch (Exception ex) {
			ex.printStackTrace();
		} 

	}

	private static void addSubscribers(BettingSystem bettingSystem) {
		try {
			/* adding some subscribers */
			bettingSystem.subscribe("Andrei", "Bencsik", "afbencsi", "1992-08-12", managerPass);
			bettingSystem.subscribe("Ahmed", "Sami-Mohamed", "asamimoh", "1992-08-12", managerPass);
			bettingSystem.subscribe("Clara", "Galimberti", "cgalimbe", "1992-08-12", managerPass);
			bettingSystem.subscribe("Ignacio", "Evangelista", "ievangel", "1992-08-12", managerPass);
			bettingSystem.subscribe("Pfuong-Bich", "Luc", "pfluc", "1992-08-12", managerPass);
			bettingSystem.subscribe("Leonard", "Toshimori", "ltoshimor", "1992-08-12", managerPass);
			
			bettingSystem.subscribe("Florian", "Dumbovski", "fdumbov", "1992-08-12", managerPass);
			/* testing the to String method */
			//java.lang.System.out.println(bettingSystem.getSubscriberByUserName("afbencsi").toString());
			
			
			bettingSystem.creditSubscriber("asamimoh", 9999, managerPass);
			
			bettingSystem.creditSubscriber("afbencsi", 20, managerPass);
			bettingSystem.creditSubscriber("fdumbov", 100, managerPass);
			
			/* some debiting */
			bettingSystem.debitSubscriber("afbencsi", 10,managerPass);
			bettingSystem.debitSubscriber("afbencsi", 10,managerPass);
			bettingSystem.debitSubscriber("fdumbov", 10,managerPass);
			

		} catch (BadParametersException ex) {
			ex.printStackTrace();
		} catch (AuthenticationException auth) {
			auth.printStackTrace();
		} catch (ExistingSubscriberException subEx) {
			subEx.printStackTrace();
			java.lang.System.out.println("Username already exists!");
		} catch( SubscriberException ex1) {
			ex1.printStackTrace();
		} catch (Exception gen) {
			gen.printStackTrace();
		}

		try {
			/* some unsubscribtions */
			//bettingSystem.unsubscribe("afbencsi", managerPass)
			

		} catch(Exception ex) {
			ex.printStackTrace();
		}
		
		try {
			//bettingSystem.printSubscribers(managerPass);
		} catch (Exception ex) {

		}
	}

	private static void addCompetitor(BettingSystem bettingSystem){
		try{
			//bettingSystem.createCompetitor("Messi","Lionel", "1987-06-24", managerPass);
			/* adding Lionel Messi to the match, he does not want to miss it! and we don't want him to miss it either */
			//bettingSystem.addCompetitor("Real_Madrid_-_Barcelona_Primera_Division",bettingSystem.createCompetitor("Messi","Lionel", "1987-06-24", managerPass),managerPass);
			Competitor player = bettingSystem.createCompetitor("Messi","Lionel", "1987-06-24", managerPass);
			Competitor player1 = bettingSystem.createCompetitor("Rodriguez","James", "1991-07-12", managerPass);
			
			Competitor team = bettingSystem.createCompetitor("Real-Madrid", managerPass);
			Competitor team2 = bettingSystem.createCompetitor("Barcelona", managerPass);
			
			team.addMember(player1);
			team2.addMember(player);
			
			bettingSystem.addCompetitor("Real_Madrid_-_Barcelona_Primera_Division", team,managerPass);	
			bettingSystem.addCompetitor("Real_Madrid_-_Barcelona_Primera_Division", team2,managerPass);	
			/* deleting poor James :( */
			//bettingSystem.deleteCompetitor("Real_Madrid_-_Barcelona_Primera_Division", player,managerPass);
			//bettingSystem.printCompetitors("Real_Madrid_-_Barcelona_Primera_Division");
			
			bettingSystem.betOnWinner(100, "Real_Madrid_-_Barcelona_Primera_Division", team, "asamimoh", tempPass);
			
			
			
		} catch (AuthenticationException auth){
			auth.printStackTrace();
		} catch (ExistingCompetitionException compEx){
			compEx.printStackTrace();
		} catch (CompetitionException comp){
			comp.printStackTrace();
		} catch (ExistingCompetitorException comptrEx){
			comptrEx.printStackTrace();
		} catch (BadParametersException ex){
			ex.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}


}
