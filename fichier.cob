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
  ALTERNATE RECORD KEY fa_idart WITH DUPLICATES
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
  ALTERNATE RECORD KEY far_stock WITH DUPLICATES
  FILE STATUS IS fart_stat.

  SELECT fdonnees ASSIGN TO "donnees.dat"
  ORGANIZATION SEQUENTIAL
  ACCESS IS SEQUENTIAL
  FILE STATUS IS fdo_stat.

DATA DIVISION.
FILE SECTION.
FD fclient.
  01 clientTamp.
    02 fcl_id PIC 9(2).
    02 fcl_nom PIC X(30).
    02 fcl_prenom PIC X(30).
    02 fcl_mail PIC X(30).
    02 fcl_adresse PIC X(30).
    02 fcl_fidele PIC 9.

FD fachat.
  01 achatTamp.
    02 fa_id PIC 9(2).
    02 fa_idcmd PIC 9(2).
    02 fa_idart PIC 9(2).
    02 fa_quantite PIC 9(2).

FD fcmd.
  01 cmdTamp.
    02 fco_id PIC 9(2).
    02 fco_idClient PIC 9(2).
    02 fco_nbArticles PIC 9(2).
    02 fco_prix PIC 9(2).

FD fart.
  01 artTamp.
    02 far_id PIC 9(2).
    02 far_nom PIC X(30).
    02 far_prix PIC 9(3).
    02 far_couleur PIC X(30).
    02 far_taille PIC X(30).
    02 far_type PIC 9.
    02 far_stock PIC 9(2).

FD fdonnees.
01 donneesTamp.
  02 fdo_achat PIC 9(2).
  02 fdo_commande PIC 9(2).
  02 fdo_client PIC 9(2).
  02 fdo_article PIC 9(2).


WORKING-STORAGE SECTION.
  77 fclient_stat PIC 9(2).
  77 fachat_stat PIC 9(2).
  77 fcmd_stat PIC 9(2).
  77 fart_stat PIC 9(2).
  77 fdo_stat PIC 9(2).
  77 Wfin PIC 9.
  77 Wident PIC 9.
  77 Wf PIC 9(2).
  77 WidClient PIC 9(2).
  77 WidCommande PIC 9(2).
  77 Wok PIC 9(2).
  77 Wpre PIC X(30).
  77 Wnom PIC X(30).
  77 Widart PIC 9(2).
  77 do_achat PIC 9(15).
  77 do_commande PIC 9(15).
  77 do_client PIC 9(15).
  77 do_article PIC 9(15).
  77 Wqte PIC 9(2).
  77 Wval PIC 9.


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

OPEN EXTEND  fdonnees
IF fdo_stat=35 THEN
    OPEN OUTPUT fdonnees
    MOVE 0 TO fdo_achat
    MOVE 0 TO fdo_commande
    MOVE 0 TO fdo_client
    MOVE 0 TO fdo_article
    WRITE donneesTamp END-WRITE
END-IF
CLOSE fdonnees

PERFORM WITH TEST AFTER UNTIL Wf=0
DISPLAY 'Saisissez le numero de la fonction souhaité:'
DISPLAY '1:ajout_client, 2:supprimer_client, 3:modifier_infoCl,'
DISPLAY '4:affichage_client, 5:Ajout_article, 6:supprimer_article'
DISPLAY '7:affichage_article, 8:ajout_commande, 9:supprimer_commande,'
DISPLAY '10:affichage_commande, 11:affichage_achat, 12:Gerer_stock'
DISPLAY '13:Fin_stock, 14:Recherche_fidelite, 15:Articles_populaires,0:quitter'
        ACCEPT Wf
        EVALUATE Wf
        WHEN 1
                PERFORM AJOUT_CLIENT
        WHEN 2
                PERFORM SUPPRIMER_CLIENT
        WHEN 3
                PERFORM MODIFIER_INFOCL
        WHEN 4
                PERFORM AFFICHAGE_CLIENT
        WHEN 5
                PERFORM AJOUT_ARTICLE
        WHEN 6
                PERFORM SUPPRIMER_ARTICLE
        WHEN 7
                PERFORM AFFICHAGE_ARTICLE
        WHEN 8
                PERFORM AJOUT_COMMANDE
        WHEN 9
                PERFORM SUPPRIMER_COMMANDE
        WHEN 10
                PERFORM AFFICHAGE_COMMANDE
        WHEN 11
                PERFORM AFFICHAGE_ACHAT
        WHEN 12
                PERFORM GERER_STOCK
        WHEN 13
                PERFORM FIN_STOCK
        WHEN 14
                PERFORM RECHERCHE_FIDELITE
        END-EVALUATE


END-PERFORM
STOP RUN.


        AJOUT_ID_CLIENT.
        OPEN I-O fdonnees
        READ fdonnees
        ADD 1 TO fdo_client
        REWRITE donneesTamp END-REWRITE
        MOVE fdo_client TO do_client
        CLOSE fdonnees.

        AJOUT_ID_ARTICLE.
        OPEN I-O fdonnees
        READ fdonnees
        ADD 1 TO fdo_article
        REWRITE donneesTamp END-REWRITE
        MOVE fdo_article TO do_article
        CLOSE fdonnees.


        AJOUT_ID_ACHAT.
        OPEN I-O fdonnees
        READ fdonnees
        ADD 1 TO fdo_achat
        REWRITE donneesTamp END-REWRITE
        MOVE fdo_achat TO do_achat
        CLOSE fdonnees.


        AJOUT_ID_COMMANDE.

        OPEN I-O fdonnees
        READ fdonnees
        ADD 1 TO fdo_commande
        REWRITE donneesTamp END-REWRITE
        MOVE fdo_commande TO do_commande
        CLOSE fdonnees.



        AJOUT_CLIENT.
        PERFORM AJOUT_ID_CLIENT
        MOVE fdo_client TO fcl_id
        DISPLAY 'ICII ',fcl_id
        DISPLAY 'Veuillez saisir les informations du client'
        DISPLAY 'Nom du client'
        ACCEPT fcl_nom
        DISPLAY 'Prenom du client'
        ACCEPT fcl_prenom
        DISPLAY 'Mail du client'
        ACCEPT fcl_mail
        DISPLAY 'Adresse du client'
        ACCEPT fcl_adresse
        MOVE 0 TO Wfin
        PERFORM WITH TEST AFTER UNTIL fcl_fidele > 0 AND fcl_fidele < 3
          DISPLAY 'Saisir 1 si le client a choisi notre programme de fidelité, 2 sinon'
          ACCEPT fcl_fidele
        END-PERFORM
        OPEN I-O fclient
        WRITE clientTamp END-WRITE
        CLOSE fclient.

        AFFICHAGE_CLIENT.
        OPEN INPUT fclient
        MOVE 0 TO Wfin
        PERFORM WITH TEST AFTER UNTIL Wfin=1
          READ fclient NEXT
          AT END
            MOVE 1 TO Wfin
            DISPLAY 'Fin de fichier'
          NOT AT END
            DISPLAY '--------------------'
            DISPLAY 'numero: ',fcl_id
            DISPLAY 'Nom: ',fcl_nom
            DISPLAY 'prenom: ',fcl_prenom
            DISPLAY 'mail:',fcl_mail
            DISPLAY 'adresse:',fcl_adresse
            DISPLAY 'fidelite:',fcl_fidele
            DISPLAY '--------------------'
        END-PERFORM
        CLOSE fclient.

        AFFICHAGE_ARTICLE.
        OPEN INPUT fart
        MOVE 0 TO Wfin
        PERFORM WITH TEST AFTER UNTIL Wfin=1
          READ fart NEXT
          AT END
            MOVE 1 TO Wfin
            DISPLAY 'Fin de fichier'
          NOT AT END
            DISPLAY '--------------------'
            DISPLAY 'numero: ',far_id
            DISPLAY 'Nom: ',far_nom
            DISPLAY 'prix: ',far_prix
            DISPLAY 'Taille:',far_taille
            DISPLAY 'type:',far_type
            DISPLAY 'couleur:',far_couleur
            DISPLAY 'quantite:',far_stock
            DISPLAY '--------------------'
        END-PERFORM
        CLOSE fart.

        AFFICHAGE_ACHAT.
        OPEN INPUT fachat
        MOVE 0 TO Wfin
        PERFORM WITH TEST AFTER UNTIL Wfin=1
          READ fachat NEXT
          AT END
            MOVE 1 TO Wfin
            DISPLAY 'Fin de fichier'
          NOT AT END
            DISPLAY '--------------------'
            DISPLAY 'numero: ',fa_id
            DISPLAY 'num commande: ',fa_idcmd
            DISPLAY 'num article: ',fa_idart
            DISPLAY 'quantite:',fa_quantite
            DISPLAY '--------------------'
        END-PERFORM
        CLOSE fachat.

      SUPPRIMER_CLIENT.

      OPEN I-O fclient
      DISPLAY 'Veuillez saisir l`identifiant du client �  supprimer'
      ACCEPT Wident
      MOVE Wident TO fcl_id
      READ fclient
      INVALID KEY
        DISPLAY 'Client inexistant'
      NOT INVALID KEY
        DELETE fclient RECORD
      END-READ
      CLOSE fclient.

      SUPPRIMER_ARTICLE.

      OPEN I-O fart
      DISPLAY 'Veuillez saisir l`identifiant de l article �  supprimer'
      ACCEPT Wident
      MOVE Wident TO far_id
      READ fart
      INVALID KEY
        DISPLAY 'Article inexistant'
      NOT INVALID KEY
        DELETE fart RECORD
      END-READ
      CLOSE fart.


      MODIFIER_INFOCL.
      OPEN I-O fclient
      PERFORM WITH TEST AFTER UNTIL Wok=0
        DISPLAY 'Saisissez un numero selon la modification souhaitée'
        DISPLAY '1:nom,2:prenom,3:Mail, 4:Adresse, 5:Fidelite, 0:quitter'
        ACCEPT Wok
        IF Wok NOT = 0 THEN
          DISPLAY 'Veuillez saisir l`identifiant du client concerné'
          ACCEPT Wident
          MOVE Wident TO fcl_id
          READ fclient
          INVALID KEY
            DISPLAY 'Client inexistant'
          NOT INVALID KEY
            EVALUATE Wok
            WHEN 1
              DISPLAY 'Veuillez saisir le nouveau nom'
              ACCEPT fcl_nom
              REWRITE clientTamp END-REWRITE
            WHEN 2
              DISPLAY 'Veuillez saisir le nouveau prenom'
              ACCEPT fcl_prenom
              REWRITE clientTamp END-REWRITE
            WHEN 3
              DISPLAY 'Veuillez saisir le nouveau mail'
              ACCEPT fcl_mail
              REWRITE clientTamp END-REWRITE
            WHEN 4
              DISPLAY 'Veuillez saisir la nouvelle adresse'
              ACCEPT fcl_adresse
              REWRITE clientTamp END-REWRITE
            WHEN 5
              PERFORM WITH TEST AFTER UNTIL fcl_fidele < 2
                DISPLAY 'Veuillez saisir la nouvelle état du fidelite'
                DISPLAY '(1:Fidele, 0:NonFidele)'
                ACCEPT fcl_fidele
              END-PERFORM
              REWRITE clientTamp END-REWRITE
          END-EVALUATE
          END-READ
        END-IF
      END-PERFORM
      CLOSE fclient.


      RECHERCHE_FIDELITE.
      OPEN INPUT fclient
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
            DISPLAY 'Fin de fichier'
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
      PERFORM AJOUT_ID_ARTICLE
      MOVE fdo_article TO far_id
      DISPLAY 'Veuillez saisir les informations de l article'
      DISPLAY 'nom de l article'
      ACCEPT far_nom
      DISPLAY 'le prix de l article'
      ACCEPT far_prix
      DISPLAY 'couleur de l article'
      ACCEPT far_couleur
      PERFORM WITH TEST AFTER UNTIL far_taille = "xs" OR far_taille = "s" OR far_taille = "m" OR far_taille = "l" OR far_taille = "xl"
        DISPLAY 'Taille de l article (xs,s,m,l,xl)'
        ACCEPT far_taille
      END-PERFORM
      PERFORM WITH TEST AFTER UNTIL far_type < 7
          DISPLAY ' Type de l article: 1:hautFemme, 2:basFemme,'
          DISPLAY ' 3:AccesoireFemme, 4:HautHomme, 5:basHomme,'
          DISPLAY ' 6:AccesoireHomme'
          ACCEPT far_type
      END-PERFORM
      DISPLAY 'La quantité en stock'
      ACCEPT far_stock
      OPEN I-O fart
      WRITE artTamp END-WRITE
      CLOSE fart.



      GERER_STOCK.
      DISPLAY 'Processus changement de stock en cours'
      OPEN I-O fart
      DISPLAY 'Veuillez saisir les informations de l article'
      DISPLAY 'l identifiant de l article'
      ACCEPT Widart
      MOVE Widart TO far_id
      READ fart
      INVALID KEY
        DISPLAY 'Article inexistant'
      NOT INVALID KEY
        PERFORM WITH TEST AFTER UNTIL Wok > 0 AND Wok < 3
          DISPLAY 'Saisissez 1 pour un retrait dans les stocks'
          DISPLAY 'et 2 pour un rajout'
          ACCEPT Wok
        END-PERFORM
        DISPLAY 'Veuillez saisir la quantité �  rajouter / retirer'
        ACCEPT fa_quantite
        EVALUATE Wok
          WHEN 1
            IF far_stock < fa_quantite THEN
              DISPLAY 'Retrait du stock impossible.La valeur du retrait depasse'
              DISPLAY 'la quantité en stock'
            ELSE
              compute far_stock = far_stock - fa_quantite
            END-IF
          WHEN 2
            compute far_stock = far_stock + fa_quantite
        END-EVALUATE
        REWRITE artTamp END-REWRITE
      END-READ
      CLOSE fart.


      EFFECTUER_ACHAT.

      DISPLAY 'Processus Achat en cours'
      OPEN I-O fart
      DISPLAY 'Veuillez saisir les informations de l achat'
      DISPLAY 'Veuillez saisir l id de l article'
      ACCEPT Widart
      DISPLAY 'Veuillez saisir la quantité acheté / retourné'
      ACCEPT Wqte
      MOVE Widart TO far_id
      READ fart
      INVALID KEY
        DISPLAY 'Article inexistant'
      NOT INVALID KEY
        IF far_stock < Wqte THEN
          DISPLAY 'La quantité en stock n est pas suffisante.'
          DISPLAY 'Vous ne pouvez pas effectuer l achat'
        ELSE
          PERFORM AJOUT_ID_ACHAT
          MOVE fdo_achat TO fa_id
          MOVE fdo_commande TO fa_idcmd
          MOVE Wqte TO fa_quantite
          MOVE far_id TO fa_idart
          CLOSE fart
          PERFORM GERER_STOCK
          OPEN I-O fachat
          WRITE achatTamp END-WRITE
          CLOSE fachat
        END-IF
      END-READ.



      ECHANGE.
      DISPLAY 'Processus echange en cours'
      PERFORM AJOUT_COMMANDE
      PERFORM SUPPRIMER_COMMANDE.

      REMBOURSEMENT.

      DISPLAY 'Processus Remboursement en cours'
      PERFORM GERER_STOCK
      OPEN I-O fachat
      EVALUATE Wval
        WHEN 1
          MOVE 0 TO Wfin
          MOVE Wident TO fa_idcmd
          START fachat KEY = fa_idcmd
          INVALID KEY
            DISPLAY 'Il n y a aucun achat lié à cette commande'
          NOT INVALID KEY
            PERFORM WITH TEST AFTER UNTIL Wfin = 1
              READ fachat NEXT
              AT END
                MOVE 1 TO Wfin
                DISPLAY 'Fin de fichier'
              NOT AT END
                IF Wident = fa_idcmd THEN
                  DELETE  fachat RECORD
                END-IF
                END-READ
              END-PERFORM
          END-START
        WHEN 2
          DISPLAY 'Veuillez saisir l identifiant de l achat remboursé'
          ACCEPT Wident
          DISPLAY 'Veuillez saisir la quantité rembourser'
          ACCEPT Wqte
          MOVE Wident TO fa_id
          READ fachat
          INVALID KEY
            DISPLAY 'Achat inexistant'
          NOT INVALID KEY
            IF fa_quantite = Wqte THEN
              DELETE  fachat RECORD
            ELSE
              compute fa_quantite= fa_quantite - Wqte
              REWRITE achatTamp END-REWRITE
          END-READ
        END-EVALUATE
      CLOSE fachat.

      FIN_STOCK.

      OPEN INPUT fart
      MOVE 0 TO Wfin
      MOVE 0 TO far_stock
      START fart KEY = far_stock
      INVALID KEY
        DISPLAY 'Le stock est rempli. Tous les articles sont présent'
      NOT INVALID KEY
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
          READ fart NEXT
          AT END
            MOVE 1 TO Wfin
            DISPLAY 'Fin de fichier'
          NOT AT END
            IF far_stock = 0 THEN
              DISPLAY 'L article ',far_id,': ',far_nom
            ELSE
              MOVE 1 TO Wfin
            END-IF
          END-READ
        END-PERFORM
      END-START
      CLOSE fart.

      AJOUT_COMMANDE.
      OPEN I-O fcmd
      MOVE 0 TO Wfin
      MOVE 0 TO fco_prix
      MOVE 0 TO fco_nbArticles
      DISPLAY 'Veuillez saisir les informations de la commande :'
      PERFORM WITH TEST AFTER UNTIL Wfin = 1
        DISPLAY 'id client : '
        ACCEPT WidClient
        OPEN INPUT fclient
        MOVE WidClient TO fcl_id
        READ fclient
        INVALID KEY
            DISPLAY 'Aucun client pour cet ID'
        NOT INVALID KEY
            MOVE 1 TO Wfin
            MOVE fcl_id TO fco_idClient
        END-READ
        CLOSE fclient
      END-PERFORM

      PERFORM AJOUT_ID_COMMANDE
      MOVE fdo_commande TO fco_id

      DISPLAY 'Ajout des achats de cette commande'
      MOVE 1 TO Wfin
      PERFORM WITH TEST AFTER UNTIL Wfin = 0
        PERFORM EFFECTUER_ACHAT
        DISPLAY 'Voulez vous ajouter un autre article ? oui:1 non:0'
        ACCEPT Wfin
      END-PERFORM
      OPEN INPUT fachat
      MOVE 0 TO Wfin
      MOVE fco_id TO fa_idcmd
      START fachat KEY = fa_idcmd
      INVALID KEY
        DISPLAY 'invalid key lecture sur zone fa_icmd (Ajout commande)'
      NOT INVALID KEY
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
          READ fachat NEXT
          AT END
            MOVE 1 TO Wfin
            DISPLAY 'Fin de fichier achat ajout article'
          NOT AT END
            COMPUTE fco_nbArticles = fco_nbArticles + fa_quantite

            OPEN INPUT fart
            MOVE fa_idart TO far_id
            READ fart
                INVALID KEY
                    DISPLAY 'Article inexistant'
                NOT INVALID KEY
                    COMPUTE fco_prix = fco_prix + (far_prix*fa_quantite)
            END-READ
            CLOSE fart
          END-READ
        END-PERFORM
      END-START
      CLOSE fachat
      WRITE cmdTamp
      CLOSE fcmd.

      AFFICHAGE_COMMANDE.
      OPEN INPUT fcmd
      MOVE 0 TO Wfin
      PERFORM WITH TEST AFTER UNTIL Wfin=1
        READ fcmd NEXT
        AT END
          MOVE 1 TO Wfin
          DISPLAY 'Fin de fichier'
        NOT AT END
          DISPLAY '--------------------'
          DISPLAY 'numero commande: ',fco_id
          DISPLAY 'numero client: ',fco_idClient
          DISPLAY 'nombre d article: ',fco_nbArticles
          DISPLAY 'prix total: ',fco_prix
          DISPLAY '--------------------'
      END-PERFORM
      CLOSE fcmd.


      SUPPRIMER_COMMANDE.
      OPEN I-O fcmd
      DISPLAY 'Veuillez saisir l`identifiant de la commande'
      ACCEPT Wident
      PERFORM WITH TEST AFTER UNTIL Wval = 1 OR Wval = 2
        DISPLAY 'Veuillez saisir 1 pour rembourser toute la commande ou 2'
        DISPLAY 'Pour rembourser une partie de la commande'
        ACCEPT Wval
      END-PERFORM
      EVALUATE Wval
        WHEN 1
          PERFORM REMBOURSEMENT
          MOVE Wident TO fco_id
          READ fcmd
          INVALID KEY
            DISPLAY 'Commande inexistante'
          NOT INVALID KEY
            DELETE fcmd RECORD
          END-READ
        WHEN 2
          PERFORM REMBOURSEMENT
      END-EVALUATE
      CLOSE fcmd.
