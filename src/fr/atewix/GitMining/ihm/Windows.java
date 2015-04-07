package fr.atewix.GitMining.ihm;

import javax.swing.*;
import java.awt.*;
/**
 * Created by xawirses on 07/04/15.
 */

public class Windows extends JFrame {
    private JPanel mainWindows = new JPanel();

    public Windows(String nom, Dimension taille) {
        super(nom);
        setResizable(false);
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        setJMenuBar(new Menu());
        setContentPane(mainWindows);
        setPreferredSize(taille);
        pack();
        setLocationRelativeTo(null);
        setVisible(true);
    }
}