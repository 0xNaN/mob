## Kademlia - TODO
    1. Trasformare il tutto in un Application
        - introdurre un supervisor, possibile albero:
            --+ peer  
              |
              + network (possibile nome futuro: crawler)
              |
              + repository
              |
              + kbucket
           Per questo punto va capito quale processo può essere riavviato senza danneggiare l'intera
           application

         - il kbucket non dovrà più contenere {PID, HASH(ID)} come "contatti" degli atri peer
           nella rete; questo perché il PID può cambiare in seguito ad un riavvio da parte del
           supervisor. Ogni peer sarà identificato da {{Node, peer}, HASH(ID)} dove ogni
           Nodo (Erlang) avrà l'attuale PID del peer registrato sotto il nome "peer"

         - Occorre trovare un nome per quest'application (semplicemente: `dht` ?). 
           "kad" già esiste (Emule kad).
           Una volta trovato occorrerà rinominare i moduli come <nome>_<modulo>.erl
         
         - L'utilizzo di named process è essenziale. L'impressione - però - è quella che
           va un po' contro ad alcuni principi del Clean Code. per esempio:
           Poniamo di registrare il PID del peer con nome `peer`. Questo è essenziale affinché il supervisor
           faccia il suo lavoro; Se accettiamo questa scelta, questo ci *limita* ad avere un solo peer per
           Nodo (Erlang) [e questo, per lo scopo di Mob, va più che bene]. Ma dunque possiamo evitare di
           portarci il PID (ormai registrato) a destra e sinistra? es:

                   dht:get(PeerPid, Key) può diventare dht:get(Key)

           Ovvero sviluppare l'Application dal punto di vista di una sua sola istanza (1 `peer`, 1 `kbucket`, ...)
           Ovviamente bisogna capire quanto questo influenza il testing (magari nei moduli interni iniettare tutto, 
           mentre le interfacce del client più rilassate);
        
         - convertire ciò che resta in gen_*:
                - kbucket
                - network
                - repository
 
    2. licenza 
           
    3. Test + Implementazione di join di un peer con un ID già presente
        - comportamento richiesto: join negato e nessun pollution degli altri peer

    4. L'idea del modulo `dht` è quella di fornire l'interfaccia più alto livello per utilizzare la rete quindi:
            dht:start(K, Alpha),        <- k e alpha magari presi da una configurazione
            dht:get(Peer, my_k), 
            dht:put(Peer, {my_k, value})
            dht:join(Peer, BootstrapPeer)

        Dht implementa la logica delle sudette operazioni sulle spalle delle (ormai non più chiamate così)
        iterative_find_*, offerte dal modulo `network`. `network` in effetti fornisce find_node(), find_value()
        ed è per questo che un nome più idoneo può essere `crawler` (l'ho visto già utilizzato in un altra impl. di Kademlia)


    5. L'estazione del modulo `dht` ha permesso di ottenere un modulo `peer` il cui scopo è **solo** quello di esporre
       le RPC Kademlia! Dal momento che questo processo sarà quello più "attivo" (nel senso che a lui stesso arriveranno
       i messaggi degli altri peer nella rete) è necessario mantenerlo quanto più possibile libero, il che implica
       il minimo utilizzo di chiamate sincrone. Vanno rivisti con particolare attenzione i valori di ritorno delle funzioni
       che esporta;
       Non amo molto la funzione peer:refresh() che a sua volta chiama refresh() sul kbucket; in particolare perché
       il refresh() necessità di una ricerca globale nella rete e quindi si inietta un processo dht;
       Attualemente le rpc vengono gestite utilizzando un gen_event: credo che sia stato un esperimento in parte fallito
       questo perché non permette di gestire ogni rpc in un processo separato.


    6. il modulo network (meglio `crawler`).
       Va convertito in un gen_server.
       È **davvero** poco chiaro come è implementato attualmente, seppur non credo ci siano soluzioni molto eleganti.
       È probabile che sarà necessario splittarlo in più moduli;
       Scriverò come funziona attualmente per aiutare Joe a capire e rifattorizzare insieme
       
       !! è **assolutamente** necessario capire come vogliamo per la prima milestone implementare la ricerca, per es:
            - fermarsi al primo valore trovato      (assolutamente no)
            - ritornare una lista ordinata per frequenza
            - introdurre vector clock e ritornare una lista di valori
       
       !! finché non capiamo questo il test `should_find_a_value_stored_in_itself_test` rimarrà rosso perché appunto dipende
          da come definiamo la ricerca

    7. i bucket in kbucket sono implementate come liste; niente di più sbagliato: sono sets!!! anche in quel modulo si può tanto
       rifattorizzare! Questo permetterà anche di fermare un test "ballerino" che va in rosso perché testa su eguaglianza piuttosto
       che contenuto
    
    8. Piccola nota: esiste un utility in linux chiamata `tc` (traffic control) permette in maniera semplice di settare delay
       anche molto realistici sulle interfacce; ci potrà essere utile se vogliamo testare la dht in ambienti con ritardi/perdita
       di pacchetti
       
    9. Va totalmente rivisto il logging 
