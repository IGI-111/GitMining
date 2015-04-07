package fr.atewix.GitMining.ihm;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;

/**
 * Created by xawirses on 07/04/15.
 */

public class Menu extends JMenuBar implements ActionListener {

    private static final String FILE = "File";
    private static final String F_OPEN = "Open";
    private static final String F_EXIT = "Exit";
    private static final String VIEW = "View";
    private static final String V_SEARCH = "Search";
    private static final String V_APRIORI = "Apriori";
    private static final String V_ASSOTIATION = "Association";

    public Menu() {
        super();
        add(fileMenu());
        add(viewMenu());
    }

    public JMenuItem newItem(String text) {
        JMenuItem menuItem = new JMenuItem(text);
        menuItem.addActionListener(this);
        return menuItem;
    }

    public JMenu fileMenu(){
        JMenu File = new JMenu(FILE);
        File.add(newItem(F_OPEN));
        File.add(newItem(F_EXIT));
        return File;
    }

    public JMenu viewMenu(){
        JMenu View = new JMenu(VIEW);
        View.add(newItem(V_SEARCH));
        View.add(newItem(V_APRIORI));
        View.add(newItem(V_ASSOTIATION));
        return View;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        String commande = e.getActionCommand();
        if(commande.equals(F_EXIT))
            System.exit(0);
    }
}