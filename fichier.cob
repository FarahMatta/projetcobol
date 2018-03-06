IDENTIFICATION DIVISION.
  PROGRAM-ID.fichier.
  
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.       
  SELECT fclient ASSIGN TO "clients.dat"
  ORGANIZATION INDEXED
  ACCESS MODE IS DYNAMIC
  RECORD KEY fcl_id
  ALTERNATE RECORD KEY fcl_fidele WITH DUPLICATES
  FILE STATUS IS fclient_stat.
  
  SELECT fachat ASSIGN TO "achats.dat"
  ORGANIZATION INDEXED
  ACCESS MODE IS DYNAMIC
  RECORD KEY fa_id
  ALTERNATE RECORD KEY fa_idcmd WITH DUPLICATES
  ALTERNATE RECORD KEY fa_idart
  FILE STATUS IS fachat_stat.
  
  SELECT fcmd ASSIGN TO "commandes.dat"
  ORGANIZATION INDEXED
  ACCESS MODE IS DYNAMIC
  RECORD KEY fco_id
  ALTERNATE RECORD KEY fco_idClient WITH DUPLICATES
  FILE STATUS IS fcmd_stat.
  
  SELECT fart ASSIGN TO "articles.dat"
  ORGANIZATION INDEXED
  ACCESS MODE IS DYNAMIC
  RECORD KEY far_id
  ALTERNATE RECORD KEY far_nom WITH DUPLICATES
  ALTERNATE RECORD KEY far_taille WITH DUPLICATES
  ALTERNATE RECORD KEY far_type WITH DUPLICATES
  FILE STATUS IS fart_stat.
  
DATA DIVISION.
FILE SECTION.
FD fclient.
  01 clientTamp.
    02 fcl_id PIC 9.
    02 fcl_nom PIC X(30).
    02 fcl_prenom PIC X(30).
    02 fcl_mail PIC X(30).
    02 fcl_adresse PIC X(30).
    02 fcl_fidele PIC 9.
    
FD fachat.
  01 achatTamp.
    02 fa_id PIC 9.
    02 fa_idcmd PIC 9.
    02 fa_idart PIC 9.
    02 fa_quantite PIC 9.
    
FD fcmd.
  01 cmdTamp.
    02 fco_id PIC 9.
    02 fco_idClient PIC 9.
    02 fco_nbArticles PIC 9.
    02 fco_prix PIC 9.
    
FD fart.
  01 artTamp.
    02 far_id PIC 9.
    02 far_nom PIC X(30).
    02 far_prix PIC 9.
    02 far_couleur PIC X(30).
    02 far_taille PIC X(30).
    02 far_type PIC X(30).
    02 far_stock PIC 9.
    
WORKING-STORAGE SECTION.
  77 fclient_stat PIC 9(2).
  77 fachat_stat PIC 9(2).
  77 fcmd_stat PIC 9(2).
  77 fart_stat PIC 9(2).
  77 Wfin PIC 9.

  
PROCEDURE DIVISION.

OPEN I-O fclient
IF fclient_stat =35 THEN
        OPEN OUTPUT fclient
END-IF
CLOSE fclient

OPEN I-O fachat
IF fachat_stat =35 THEN
        OPEN OUTPUT fachat
END-IF
CLOSE fachat

OPEN I-O fcmd
IF fcmd_stat =35 THEN
        OPEN OUTPUT fcmd
END-IF
CLOSE fcmd

OPEN I-O fart
IF fart_stat =35 THEN
        OPEN OUTPUT fart
END-IF
CLOSE fart

PERFORM WITH TEST AFTER UNTIL wf=0
DISPLAY 'Saisissez le numero de la fonction:'
DISPLAY '1:ajout_client, 2:supprimer_client, 3:modifier_infoCl,'
DISPLAY '4:Recherche_fidlite, 5:effectuer_achat, 6:Echange,'
DISPLAY '7:remboursement, 8:Ajout_commande, 9:Ajout_article,'
DISPLAY '10:Fin_stock, 11:Articles_populaires, 12:Supprimer_commande,'
DISPLAY '13: Gerer_stock,14:modifier_commande, 0:quitter'
        ACCEPT Wf
        EVALUATE Wf
        WHEN 1
                PERFORM AJOUT_CLIENT
        WHEN 2
                PERFORM SUPPRIMER_CLIENT
        WHEN 3
                PERFORM MODIFIER_INFOCL
        WHEN 4
                PERFORM RECHERCHE_FIDELITE
        WHEN 5
                PERFORM EFFECTUER_ACHAT
        WHEN 6
                PERFORM ECHANGE
        WHEN 7
                PERFORM REMBOURSEMENT
        WHEN 8
                PERFORM AJOUT_COMMANDE
        WHEN 9
                PERFORM AJOUT_ARTICLE
        WHEN 10
                PERFORM FIN_STOCK
        WHEN 11
                PERFORM ARTICLES_POPULAIRES
        WHEN 12
                PERFORM SUPPRIMER_COMMANDE
        WHEN 13
                PERFORM GERER_STOCK
        WHEN 14
                PERFORM MODIFIER_COMMANDE               
        END-EVALUATE

END-PERFORM
STOP RUN.




