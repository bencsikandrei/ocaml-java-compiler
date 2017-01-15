/////////////////////////////////////////////////////////////////////////

import java.util.*;

import org.hibernate.*;
import org.hibernate.criterion.*;

import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.Query;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.List;

import java.io.Serializable;
import java.util.Date;
import java.util.Set;
import java.util.LinkedHashSet;

public class Main {
  
  
  public static void main(String[] args) {
    HibernateUtil.setup("create table EVENTS ( uid int, name VARCHAR, start_Date date, duration int);");
    
    // hibernate code start


        SimpleEventDao eventDao = new SimpleEventDao();
        Event event1 = new Event();
        Event event2 = new Event();
        event1.setName("Events 1");
        event2.setName("Events 2");

        eventDao.create(event1);
        eventDao.create(event2);
        List eventList = eventDao.findAll();
        
        System.out.println();
        System.out.println();
        System.out.println();
        System.out.println();
        System.out.println(eventList.size());

        

        HibernateUtil.checkData("select uid, name from events");        
    // hibernate code end
  }
  
}



/////////////////////////////////////////////////////////////////////////


public class SimpleEventDao {
    Log log = LogFactory.getLog(SimpleEventDao.class);
    private Session session;
    private Transaction tx;

    public SimpleEventDao() {
        HibernateFactory.buildIfNeeded();
    }

    /**
     * Insert a new Event into the database.
     * @param event
     */
    public void create(Event event) throws DataAccessLayerException {
        try {
            startOperation();
            session.save(event);
            tx.commit();
        } catch (HibernateException e) {
            handleException(e);
        } finally {
            HibernateFactory.close(session);
        }
    }


    /**
     * Delete a detached Event from the database.
     * @param event
     */
    public void delete(Event event) throws DataAccessLayerException {
        try {
            startOperation();
            session.delete(event);
            tx.commit();
        } catch (HibernateException e) {
            handleException(e);
        } finally {
            HibernateFactory.close(session);
        }
    }
    /**
     * Find an Event by its primary key.
     * @param id
     * @return
     */
    public Event find(Long id) throws DataAccessLayerException {
        Event event = null;
        try {
            startOperation();
            event = (Event) session.load(Event.class, id);
            tx.commit();
        } catch (HibernateException e) {
            handleException(e);
        } finally {
            HibernateFactory.close(session);
        }
        return event;
    }

    /**
     * Updates the state of a detached Event.
     *
     * @param event
     */
    public void update(Event event) throws DataAccessLayerException {
        try {
            startOperation();
            session.update(event);
            tx.commit();
        } catch (HibernateException e) {
            handleException(e);
        } finally {
            HibernateFactory.close(session);
        }
    }

    /**
     * Finds all Events in the database.
     * @return
     */
    public List findAll() throws DataAccessLayerException{
        List events = null;
        try {
            startOperation();
            Query query = session.createQuery("from Event");
            events =  query.list();
            tx.commit();
        } catch (HibernateException e) {
            handleException(e);
        } finally {
            HibernateFactory.close(session);
        }
        return events;
    }

    private void handleException(HibernateException e) throws DataAccessLayerException {
        HibernateFactory.rollback(tx);
        throw new DataAccessLayerException(e);
    }

    private void startOperation() throws HibernateException {
        session = HibernateFactory.openSession();
        tx = session.beginTransaction();
    }

}




/////////////////////////////////////////////////////////////////////////

/**
 * Represents Exceptions thrown by the Data Access Layer.
 */
public class DataAccessLayerException extends RuntimeException {
    public DataAccessLayerException() {
    }

    public DataAccessLayerException(String message) {
        super(message);
    }

    public DataAccessLayerException(Throwable cause) {
        super(cause);
    }

    public DataAccessLayerException(String message, Throwable cause) {
        super(message, cause);
    }
}



/////////////////////////////////////////////////////////////////////////

public class Event implements Serializable {
    private Long id;
    private int duration;
    private String name;
    private Date startDate;

    public Event() {

    }

    public Event(String name) {
        this.name = name;
    }

    /**
     * @hibernate.id generator-class="native" column="uid"
     * @return
     */
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    /**
     * @hibernate.property column="name"
     * @return
     */
    public String getName() { return name; }
    public void setName(String name) { this.name = name;   }

    /**
     * @hibernate.property column="start_date"
     * @return
     */
    public Date getStartDate() { return startDate; }
    public void setStartDate(Date startDate) { this.startDate = startDate; }

    /**
     * @hibernate.property column="duration"
     * @return
     */
    public int getDuration() { return duration; }
    public void setDuration(int duration) { this.duration = duration; }

}