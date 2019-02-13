#lang racket
(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ ()
    null))

;creeaza o tabela (lista de coloane) si o concateneaza la numele ei
(define create-table
  (λ (table columns-name)
    (append (list table) (map list columns-name))))

;primeste o tabela si intoarce primul element, adica numele ei 
(define get-name
  (λ (table)
    (car table))) 

;itereaza prin tabela si adauga primul element din fiecare lista, adica numele coloanelor
(define get-columns 
  (λ (table)
    (foldl (λ(first-elem acc) (append acc (list (car first-elem)))) '() (cdr table))))

;intoarce lista cu tabelele
(define get-tables 
  (λ (db)
    (if (null? db)
       '()
        (foldl (λ (each-table acc) (append acc (list each-table))) '() db)))) 

;ia fiecare tabela si ii verifica numele (primul element); daca e cea cautata, o returneaza cu totul,
;adica incluzand numele
;o tabela e o lista formata din 2 elemente: primul e chiar numele ei si al doilea e o lista de coloane
(define get-table 
  (λ (db table-name)
    (if(equal? (car (car db)) table-name)
       (car db)
       (get-table (cdr db) table-name))))

;adauga la db noua tabela
(define add-table
  (λ (db table)
    (append db (list table))))

;foldl itereaza prin db si verifica numele primit cu numele fiecarei tabele (primul element din ea)
;daca sunt egale, nu se adauga acea tabela la rezultat (acc), iar daca sunt diferite, se adauga,
;formand astfel baza de date, dar fara tabela de sters
(define remove-table
  (λ (db table-name)
    (foldl (λ (each-table acc)
             (if(equal? (car each-table) table-name)
                (append acc null)
                (append acc (list each-table)))) '() db)))

;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================

;db = lista de tabele, tabel = nume concatenat la lista de coloane; coloana = lista de elemente
(define db (list (list "Studenți" (list "Număr matricol" 123 124 125 126) (list "Nume" "Ionescu" "Popescu" "Popa" "Georgescu")
                                   (list "Prenume" "Gigel" "Maria" "Ionel" "Ioana") (list "Grupă" "321CA" "321CB" "321CC" "321CD")
                                   (list "Medie" 9.82 9.91 9.99 9.87))
                 (list "Cursuri" (list "Anul" "I" "II" "III" "IV" "I" "III") (list "Semestru" "I" "II" "I" "I" "II" "II")
                                  (list "Disciplină" "Programarea calculatoarelor" "Paradigme de programare"
                                       "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date"
                                        "Baze de date") (list "Număr credite" 5 6 5 6 5 5)
                                  (list "Număr teme" 2 3 3 3 3 0))))

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================

;intoarce o lista cu coloanele unei tabele, dar pentru o tabela ce contine doar lista de coloane, nu si numele
(define get-only-columns
  (λ (table)
    (foldl (λ (first-elem acc) (append acc (list (car first-elem)))) '() table)))

;foldl cu acc: ia fiecare table din db si ii verifica numele - daca e cel cautat,
;il appenduieste modificat, daca nu, il pune asa cum e
;face list de append ca sa formeze formatul initial de tabela: numele si apoi lista de coloane
;foldl cu acc2: ia una cate una coloanele din tabelul curent si adauga la ele ce e cazul,
;apoi le appenduieste la rezultat
;foldl cu acc1: trebuie aplicat (list) pe rezultat (acc1) ca sa faca fiecare coloana in formatul initial -
;acesta itereaza prin lista de record si daca primul element din pereche coincide cu numele coloanei,
;adauga al doilea element al perechii (valoarea efectiva a atributului) la acc1, care porneste initial
;chiar cu coloana, pentru ca trebuie sa se insereze la sfarsitul ei
(define insert
  (λ (db table-name record)
    (foldl (λ (searched-table acc) 
             (if (equal? (car searched-table) table-name)
                (append acc (list (append (list table-name) (foldl (λ (one-column acc2)
                                                                     (append acc2 (list (if (member (car one-column) (get-only-columns record))
                                                                                           (foldl (λ (elem acc1)
                                                                                                    (if (equal? (car elem) (car one-column))
                                                                                                       (append acc1 (list (cdr elem)))
                                                                                                       (append acc1 null)))
                                                                                                  one-column
                                                                                                  record)
                                                                                           (append one-column (list 'null))))))
                                                                   '()
                                                                   (cdr searched-table)))))
                                                                                       

                (append acc (list searched-table))))
           '()
           db)))

;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================

;foldl-ul cu acc itereaza prin lista primita cu numele coloanelor
;pentru fiecare nume de coloana, se appenduieste la acc rezultatul foldl-ului cu acc1, care e o lista
;foldl-ul cu acc1 itereaza prin tabela cu numele primit ca parametru, obtinuta cu functia get-table
;si verifica daca numele coloanei e acelasi cu fiecare prim element din lista "elem" ("elem" e o lista
;de forma '("Nume" ".." "..)) - cand ele coincid, se adauga restul listei "elem" la acc1, adica exact
;valorile efective de pe acea coloana 
(define simple-select-aux
  (λ (db table-name columns)
       (foldl (λ (column-name acc)
             (append acc (list (foldl (λ (elem acc1)
                                        (if(equal? column-name (car elem))
                                           (append acc1 (cdr elem))
                                           (append acc1 null))) 
                                       '()
                                       (cdr (get-table db table-name))))))
           '()
           columns)))

;daca rezultatul de la simple-select-aux e o lista cu liste nule, functia va returna
;lista vida; daca nu, returneaza rezultatul functiei ajutatoare
(define simple-select
  (λ (db table-name columns)
    (if(andmap null? (simple-select-aux db table-name columns))
       '()
       (simple-select-aux db table-name columns))))

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================

;tabelul poate fi privit ca o matrice, astfel ca aceasta functie ii va face transpusa
(define transpose-table
  (λ (table)
    (apply map list (cdr table))))

;funtia va intoarce lista cu numele de coloane din columns
(define names
  (λ (columns)
    (foldl (λ (elem acc)
             (if(pair? elem)
                 (append acc null)
                 (append acc (list elem))))
           '()
           columns)))

;functia va intoarce lista cu listele de operatii din columns
(define operations
  (λ (columns)
    (foldl (λ (elem acc)
             (if(pair? elem)
                (append acc (list elem))
                (append acc null)))
           '()
           columns)))

;map formeaza o lista de perechi de genul ("Nume" "Popescu")
;filter se aplica peste aceasta lista formata si verifica daca primul element este
;numele campului cautat si va reurna o lista formata din perechea cautata
;functia returneaza al doilea element al perechii, adica exact valoarea de la campul
;primit "field"
(define searched-value
  (λ (names record field)
    (car (cdr (car (filter (λ (pair-elem)
                             (if(equal? (car pair-elem) field)
                                #t
                                #f))
                           (map (λ (el1 el2)
                                  (append (list el1) (list el2)))
                                names
                                record))))))) 

;primeste names = lista cu numele coloanelor, record = o linie din tabelul transpus (o inregistrare),
;si lista de conditii: itereaza prin lista de conditii si returneaza true doar daca toate
;sunt adevarate, intrucat if-ul primeste primul elem din conditie (functia de comparare), rezultatul
;functiei searched-value (valoarea din inregistrare de la campul cautat) si valoarea cu care se
;compara (ultimul elem dintr-o conditie)
(define apply-cond
  (λ (names record conditions)
    (foldl (λ (one-cond acc)
             (if (eq? (searched-value names record (car (cdr one-cond))) 'null)
                (and acc #f)
                (if((car one-cond) (searched-value names record (car (cdr one-cond))) (car (reverse one-cond)))
                   (and acc #t)
                   (and acc #f))))
           #t
           conditions)))

;returneaza tabelul cu inregistrarile ramase dupa aplicarea conditions, folosind apply-cond
(define apply-conditions
  (λ (table conditions)
        (append (list (car table))
                (list (car (transpose-table table)))
                (foldl (λ (line acc)
                         (if (apply-cond (car (transpose-table table)) line conditions)
                             (append acc (list line))
                             (append acc null)))
                       '()
                       (cdr (transpose-table table))))))

;functia itereaza prin tabelul cu inregistrarile si returneaza coloana (lista) cu
;numele pe care il primeste ca argument
(define find-col
  (λ (col-name records-table)
    (foldl (λ (col-list acc)
             (if (eq? (car col-list) col-name)
                 (append acc col-list)
                 (append acc null)))
           '()
           records-table)))

;adauga in rezultat doar elementele ce nu au fost adaugate inca, din lista primita ca parametru
;astfel, aplicand length pe rezultat, se obtine numarul de valori unice din lista respectiva (pt count)
(define count-unique
  (λ (list-with-duplicates)
    (foldl (λ (elem acc)
             (if(member elem acc)
                (append acc null)
                (append acc (list elem))))
           '()
           list-with-duplicates)))

;primeste oper = operatia de efectuat si records-table = tabelul cu inregistrari
;verifica tipul operatiei si o aplica pe valorile returnate de (cdr find-col ..), returnand
;un numar pentru min, max, count, sum si avg si lista sortata pentru sort-asc si sort-desc
(define do-operation
  (λ (oper records-table)
    (cond
      ((eq? (car oper) 'min) (list (apply min (cdr (find-col (cdr oper) records-table)))))
      ((eq? (car oper) 'max) (list (apply max (cdr (find-col (cdr oper) records-table)))))
      ((eq? (car oper) 'count) (list (length (count-unique (cdr (find-col (cdr oper) records-table))))))
      ((eq? (car oper) 'sum) (list (apply + (cdr (find-col (cdr oper) records-table)))))
      ((eq? (car oper) 'avg) (list (/ (apply + (cdr (find-col (cdr oper) records-table)))
                                      (length (cdr (find-col (cdr oper) records-table))))))
      ((eq? (car oper) 'sort-asc) (list (sort (cdr (find-col (cdr oper) records-table)) <)))
      ((eq? (car oper) 'sort-desc) (list (sort (cdr (find-col (cdr oper) records-table)) >))))))

;itereaza prin columns si daca elementul e nume de coloana, doar appendeaza coloana la rezultat
;daca elementul e operatie, adauga la rezultat ce reurneaza do-operation pentru operatia respectiva
;efectuata pe tabel
(define select
  (λ (db table-name columns conditions)
    (foldl (λ (column-what acc)
             (if(pair? column-what)
                (append acc (do-operation column-what (transpose-table (apply-conditions (get-table db table-name) conditions)) ))
                (append acc (list (cdr (find-col column-what (transpose-table (apply-conditions (get-table db table-name) conditions))))))))
           '()
           columns)))

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================

;primeste o pereche de genul ("Medie" 9.80) si lista values, itereaza prin values
;si verifica numele campului, returnand lista cu valoarea propriu-zisa de la campul
;respectiv din values (adica valoarea cu care trebuie inlocuit la update)
(define search-for-value
  (λ (one-pair values)
    (foldl (λ (pairr acc)
             (if (eq? (car one-pair) (car pairr))
                 (append acc (list (cdr pairr)))
                 (append acc null)))
           '()
           values)))

;primeste o pereche de genul ("Medie" 9.80) si verifica folosind functia member daca campul
;(ex "Medie") se afla prin lista values (foldl-ul cu acc formeaza lista cu campurile continute
;de values): daca se afla, se returneaza o noua pereche formata din primul element din
;cea veche si noul element returnat de search-for-value (valoarea cu care trebuie modificat)
;astfel perechea e modificata; daca nu se afla, se returneaza perechea neschimbata
(define modify-pair
  (λ (one-pair values)
    (if(member (car one-pair) (foldl (λ (elem acc) (append acc (list (car elem)))) '() values))
       (append (list (car one-pair)) (search-for-value one-pair values)) 
       one-pair)))

;primeste names = lista cu numele coloanelor, record = inregistrare si values
;foldl-ul cu acc retuneaza o lista cu toate perechile de tip (numeCamp valoareCamp) deja
;modificate, iar foldl-ul cu acc1 ia fiecare al doilea element din aceste perechi si retunreaza
;lista obtinuta, adica o inregistrare dupa ce s-au aplicat modificarile corespunzatoare
(define modify-record
  (λ (names record values)
    (foldl (λ (elem acc1)
             (append acc1 (cdr elem)))
           '()
           (foldl (λ (one-pair acc)
                    (append acc (list (modify-pair one-pair values))))
                  '()
                  (map (λ (el1 el2)
                         (append (list el1) (list el2)))
                       names
                       record)))))

;formeaza tabelul cu nume, lista cu numele coloanelor si inregistrarile: daca nu toate conditiile
;din conditions sunt adevarate, inregistrarea se va adauga la rezultat nemodificata; altfel,
;se adauga rezultatul functiei modify-record, ce intoarce o inregistrare modificata
(define modify-or-not
  (λ (table conditions values)
    (append (list (car table))
            (list (car (transpose-table table)))
            (foldl (λ (line acc)
                     (if (apply-cond (car (transpose-table table)) line conditions)
                         (append acc (list (modify-record (car (transpose-table table)) line values)))
                         (append acc (list line))))
                   '()
                   (cdr (transpose-table table))))))

;primeste tabelul vechi si il formeaza pe cel nou cu nume si transpunand tabelul returnat
;de modify-or-not, pentru a se forma la loc listele de coloane de tipul (numeCamp valorileDinCamp)
(define modify-table
  (λ (searched-table conditions values)
    (append (list (get-name searched-table)) (transpose-table (modify-or-not searched-table conditions values)))))

;cauta tabelul in db si il modifica atunci cand il gaseste, aplicand modify-table pe acesta
;ideea utilizata se bazeaza pe folosirea functiei transpose-table, pentru a manipula mai
;usor valoarea de la un anumit camp din tabel (a o gasi si a o modifica)
(define update
  (λ (db table-name values conditions)
    (foldl (λ (searched-table acc) 
             (if (equal? (car searched-table) table-name)
                 (append acc (list (modify-table searched-table conditions values)))
                 (append acc (list searched-table))))
           '()
           db)))

;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================

;functia formeaza o tabela forata din nume, lista cu numele coloanelor si lista returnata de foldl
;rezultatul foldl-ului e o lista cu inregistrarile pentru care functia apply-cond returneaza false,
;deoarece pentru true inseamna ca linia respectiva (inregistrarea) trebuie stearsa, asa ca se adauga doar null
(define remains-or-not
  (λ (table conditions)
    (append (list (car table))
            (list (car (transpose-table table)))
            (foldl (λ (line acc)
                     (if (apply-cond (car (transpose-table table)) line conditions)
                         (append acc null) ;e de sters daca e #t
                         (append acc (list line))))
                   '()
                   (cdr (transpose-table table))))))

;returneaza tabelul ce va contine doar numele concatenat la listele cu numele coloanelor
(define delete-columns-info-from-table 
  (λ (table columns-list)
    (create-table (car table) columns-list)))

;daca lista de conditii e nula, se apeleaza delete-columns-info-from-table ce sterge
;valorile din coloane, ramanand insa structura tabelului: nume concatenat la liste
;ce contin doar numele coloanelor
;daca nu, se appenduieste la numele tabelului lista de coloane ramase
(define verify-null-conditions
  (λ (conditions searched-table)
    (if (null? conditions)
        (delete-columns-info-from-table searched-table (get-columns searched-table))
        (append (list (get-name searched-table)) (transpose-table (remains-or-not searched-table conditions))))))

;itereaza prin db si cauta tabelul de modificat - daca nu e cel cautat, doar se adauga in rezultat
;daca e cel cautat, se apeleaza functia verify-null-conditions si tabelul e intors modificat
(define delete
  (λ (db table-name conditions)
    (foldl (λ (searched-table acc) 
             (if (equal? (car searched-table) table-name)
                 (append acc (list (verify-null-conditions conditions searched-table)))
                 (append acc (list searched-table))))
           '()
           db)))   

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================

;returneaza numele coloanei comune continute de cele 2 tabele
(define get-common-col-name
  (λ (db tables)
    (list (car (foldl (λ (col-name acc)
                        (if (and (member col-name (get-only-columns (cdr (get-table db (car tables)))))
                                 (member col-name (get-only-columns (cdr (get-table db (car (cdr tables)))))))
                            (append acc (list col-name))
                            (append acc null)))
                      '()
                      (append (get-columns (get-table db (car tables))) (get-columns(get-table db (car (cdr tables))))))))))

;returneaza coloana din tabel corespunzatoare numelui primit
(define give-col
  (λ (col-name tb)
    (foldl (λ (one-col acc)
             (if (eq? col-name (car one-col))
                 (append acc one-col)
                 (append acc null)))
           '()
           tb)))

;muta la inceputul tabelei coloana comuna
(define put-first-col
  (λ (table common-col)
    (if (eq? (car (car table)) (car common-col))
        table
        (append (list common-col) 
                (foldl (λ (col acc)
                         (if (eq? (car col) (car common-col))
                             (append acc null)
                             (append acc (list col))))
                       '()
                       table)))))

;transpune o tabela ce e formata doar din liste de coloane
(define transpose-table-without-name
  (λ (table)
    (apply map list table)))

;formeaza primul tabel in formatul final, pastrand doar inregistrarile ce se
;regasesc si in al doilea (din coloana comuna)
(define final-tb1
  (λ (tb1 tb2)
    (transpose-table-without-name (append (list (car (transpose-table-without-name tb1)))
                                          (foldl (λ (record acc)
                                                   (if (member (car record) (cdr (car tb2)))
                                                       (append acc (list record))
                                                       (append acc null)))
                         
                                                 '()
                                                 (cdr (transpose-table-without-name tb1)))))))

(define get-record
  (λ (value records)
    (foldl (λ (rec acc)
             (if (eq? (car rec) value)
                 (append acc rec)
                 (append acc null)))
           '()
           records)))

;formeaza tabelul 2 in formaul final, primindu-l pe prmul deja in formatul final
(define final-tb2 
  (λ (tb1 tb2)
    (transpose-table-without-name (append (list (car (transpose-table-without-name tb2)))
                                          (foldl (λ (record acc)
                                                   (append acc (list (get-record (car record) (cdr (transpose-table-without-name tb2))))))
                                                 '()
                                                 (cdr (transpose-table-without-name tb1)))))))

;formeaza tabela finala adaugand cele 2 tabele in formatul final
(define final-table
  (λ (db tables columns conditions)
    (append (final-tb1 (put-first-col (transpose-table (apply-conditions (get-table db (car tables)) conditions))
                                      (give-col (car (get-common-col-name db tables))
                                                (transpose-table (apply-conditions (get-table db (car tables)) conditions)))) 
                       (put-first-col (transpose-table (apply-conditions (get-table db (car (cdr tables))) conditions))
                                      (give-col (car (get-common-col-name db tables))
                                                (cdr (get-table db (car (cdr tables))))))) 
            
            (cdr (final-tb2 (final-tb1 (put-first-col (transpose-table (apply-conditions (get-table db (car tables)) conditions))
                                                      (give-col (car (get-common-col-name db tables))
                                                                (transpose-table (apply-conditions (get-table db (car tables)) conditions)))) ;put-first de primu
                                       (put-first-col (transpose-table (apply-conditions (get-table db (car (cdr tables))) conditions))
                                                      (give-col (car (get-common-col-name db tables))
                                                                (cdr (get-table db (car (cdr tables))))))) 
                            (put-first-col (transpose-table (apply-conditions (get-table db (car (cdr tables))) conditions))
                                      (give-col (car (get-common-col-name db tables))
                                                (cdr (get-table db (car (cdr tables))))))))))) 

;tables = lista cu numele tabelelor; columns = lista doar cu numele coloanelor
(define natural-join-aux 
  (λ (db tables columns conditions)
    (transpose-table-without-name (cdr (transpose-table-without-name (foldl (λ (col-name acc)
                                                                              (append acc (list (give-col col-name (final-table db tables columns conditions)))))
                                                                            '()
                                                                            columns))))))

;decide care tabel are lista coloanei comune mai mare si acela va fi primul tabel, celalat al doilea
(define natural-join 
 (λ (db tables columns conditions)
    (if (> (length (car (cdr (get-table db (car tables))))) (length (car (cdr (get-table db (car (cdr tables)))))))
        (natural-join-aux db tables columns conditions)
        (natural-join-aux db (reverse tables) columns conditions))))

