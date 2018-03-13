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
  77 Wident PIC 9.
  77 Wf PIC 9(2).
  77 Wok PIC 9(2).
  77 Wpre PIC X(30).
  77 Wnom PIC X(30).
  77 Widart PIC 9.

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

PERFORM WITH TEST AFTER UNTIL Wf=0
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

        AJOUT_CLIENT.
        DISPLAY 'Veuillez saisir les informations de la competition'
        DISPLAY 'Nom du client'
        ACCEPT fcl_nom
        DISPLAY 'Prenom du client'
        ACCEPT fcl_prenom
        DISPLAY 'Mail du client'
        ACCEPT fcl_mail
        DISPLAY ' Adresse du client'
        ACCEPT fcl_adresse
        MOVE 0 TO Wfin
        PERFORM WITH TEST AFTER UNTIL fcl_fidele < 2
          DISPLAY' Saisir 1 si le client a choisi notre programme de fidelité, 0 sinon'
          ACCEPT fcl_fidele
        END-PERFORM

        OPEN I-O fclient
        WRITE clientTamp END-WRITE
        CLOSE fclient.


      SUPPRIMER_CLIENT.

      OPEN I-O fclient
      DISPLAY 'Veuillez saisir l`identifiant du client à supprimer'
      ACCEPT Wident
      MOVE Wident TO fcl_id
      READ fclient
      INVALID KEY
        DISPLAY 'Client inexistant'
      NOT INVALID KEY
        DELETE fclient RECORD
      END-READ
      CLOSE fclient.


      MODIFIER_INFOCL.

      OPEN I-O fclient
      PERFORM WITH TEST AFTER UNTIL Wok=0
        DISPLAY 'Saisissez un numero selon la modification souhaitée'
        DISPLAY '1:nom,2:prenom,3:Mail, 4:Adresse, 5:Fidelite, 0:quitter'
        ACCEPT Wok
        DISPLAY ' Veuillez saisir l`identifiant du client concerné'
        ACCEPT Wident
        MOVE Wident TO fcl_id
        READ fclient
        INVALID KEY
          DISPLAY 'Client inexistant'
        NOT INVALID KEY
          EVALUATE Wok
          WHEN 1
            DISPLAY ' Veuillez saisir le nouveau nom'
            ACCEPT fcl_nom
            REWRITE clientTamp END-REWRITE
          WHEN 2
            DISPLAY ' Veuillez saisir le nouveau prenom'
            ACCEPT fcl_prenom
            REWRITE clientTamp END-REWRITE
          WHEN 3
            DISPLAY ' Veuillez saisir le nouveau mail'
            ACCEPT fcl_mail
            REWRITE clientTamp END-REWRITE
          WHEN 4
            DISPLAY ' Veuillez saisir la nouvelle adresse'
            ACCEPT fcl_adresse
            REWRITE clientTamp END-REWRITE
          WHEN 5
            PERFORM WITH TEST AFTER UNTIL fcl_fidele < 2
              DISPLAY ' Veuillez saisir la nouvelle état du fidelite'
              DISPLAY '(1:Fidele, 0:NonFidele)'
              ACCEPT fcl_fidele
            END-PERFORM
            REWRITE clientTamp END-REWRITE
        END-EVALUATE
        END-READ
      END-PERFORM
      CLOSE fclient.


      RECHERCHE_FIDELITE.

      OPEN OUTPUT fclient
      MOVE 1 TO fcl_fidele
      MOVE 0 TO Wfin
      START fclient KEY = fcl_fidele
      INVALID KEY
        DISPLAY ' clients fidéles inexistants'
      NOT INVALID KEY
        PERFORM WITH TEST AFTER UNTIL Wfin=1
          READ fclient NEXT
          AT END
            MOVE 1 TO Wfin
          NOT AT END
            IF fcl_fidele = 1 THEN
              DISPLAY 'client',fcl_id,': ',fcl_nom,fcl_prenom,fcl_mail
            ELSE
              MOVE 1 TO Wfin
            END-IF
          END-READ
        END-PERFORM
      END-START
      CLOSE fclient.

      AJOUT_ARTICLE.

      *> id à définir encore. On récuppére l'id d'une certaine maniére puis on vérifie si elle existe ou pas.
      *>Si oui on met à jour les stock. Sinon on crée un nouveau article
      OPEN I-O fart

      MOVE Wident TO far_id
      READ fart
      INVALID KEY
        DISPLAY 'Veuillez saisir les informations de l article'
        DISPLAY 'nom de l article'
        ACCEPT far_nom
        DISPLAY 'le prix de l article'
        ACCEPT far_prix
        DISPLAY 'couleur de l article'
        ACCEPT far_couleur
        DISPLAY 'Taille de l article'
        PERFORM WITH TEST AFTER UNTIL far_type < 7
          DISPLAY ' Type de l article: 1:hautFemme, 2:basFemme,'
          DISPLAY ' 3:AccesoireFemme, 4:HautHomme, 5:basHomme,'
          DISPLAY ' 6:AccesoireHomme'
          ACCEPT far_type
        END-PERFORM
        DISPLAY 'La quantité en stock'
        ACCEPT far_stock
        REWRITE artTamp END-REWRITE
      NOT INVALID KEY
        PERFORM GERER_STOCK
      END-READ
      CLOSE fart.

      EFFECTUER_ACHAT.

      DISPLAY 'Veuillez saisir les informations de l achat'
      DISPLAY 'Veuillez saisir l id de la commande'
      ACCEPT fa_idcmd
      DISPLAY 'Veuillez saisir l id de l article'
      ACCEPT Widart
      OPEN fart INPUT
      READ

      DISPLAY 'Veuillez saisir la quantité acheté'
      ACCEPT fa_quantite
