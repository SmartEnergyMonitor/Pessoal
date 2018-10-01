;;-------------------------------------------------------------------------
;;                           Estrutura de Dados
;;-------------------------------------------------------------------------

(defstruct nSquare
  num        ;Índice máximo da linha e coluna da matriz do tabuleiro
  sMatrix)     ;Matrix que representa o tabuleiro

;;; O campo num (funções (getNum foo) e (getNumPlus1 foo)) é utilizado em vez de (length (nSquare-sMatrix foo)),
;;; uma vez que o campo sMatrix é um array bi-dimensional, logo, um array de arrays, e como tal, não é um tipo
;;; de sequência, requisito para a utilização de length

;;-------------------------------------------------------------------------



;;-------------------------------------------------------------------------
;;              Funções de Inicialização  - Específicas ao problema
;;-------------------------------------------------------------------------

;; Inicializa tabuleiro
(defun initSBoard (n)
   (make-nSquare :num (- n 1)
                 :sMatrix (make-array (list n n) :initial-element 0)))

;; Preenche o tabuleiro com valores sequenciais
(defun boardSequencer (sBoard)
  (loop for y from 0 to (nsquare-num sBoard) by 1 do
        (loop for x from 0 to (nsquare-num sBoard) by 1 do
              (setf (aref (nsquare-smatrix sBoard) x y) y)))
  sboard)



;;Baralha um tabuleiro
(defun 2dShuffler (sBoard)
  (loop for y from 0 to (nsquare-num sBoard) by 1 do
        (loop for x from 0 to (nsquare-num sBoard) by 1 do
              (swapnumber sBoard y (random (getnumplus1 sboard))(random (getnumplus1 sboard)))))
  sboard)


;;Cria um novo tabuleiro aleatório
(defun makeNewRandomBoard (n)
  (2dshuffler (boardsequencer (initsboard n))))

;Troca dois números de posição
(defun swapNumber (sboard line column1 column2)
  (rotatef (aref (nsquare-smatrix sboard) line column1)(aref (nsquare-smatrix sboard) line column2))sboard)

;Retorna o valor de num de uma estrutura
(defun getNum (sboard)
  (nsquare-num sboard))

;Retorna o valor de num de uma estrutura +1
(defun getNumPlus1 (sboard)
  (+ (nsquare-num sboard) 1))


; Cria uma nova estrutura idêntica à anterior
(defun makeNewNode (sboard)
  (let ((newNode (copy-nsquare sboard))(n (getnumplus1 sboard)))
    (setf (nsquare-smatrix newNode) (make-array (list n n)))
    (loop for y from 0 to (nsquare-num sBoard) by 1 do
        (loop for x from 0 to (nsquare-num sBoard) by 1 do
              (setf (aref (nsquare-smatrix newnode) y x) (aref (nsquare-smatrix sboard) y x))))
    newNode))


;Troca a posição de dois números aleatórios numa mesma linha aleatória
(defun swapNumberRandomLine (sBoard)
  (swapnumber sboard (random (getnumplus1 sBoard)) (random (getnumplus1 sBoard))(random (getnumplus1 sBoard))))





;;--------------------------------------------------------------------------
;;              Heurística  - Específicas ao problema
;;--------------------------------------------------------------------------

;Verifica se dois números são idênticos em duas posições numa única coluna
(defun threatAssessment (sboard column line1 line2)
  (= (aref (nsquare-smatrix sboard) line1 column) (aref (nsquare-smatrix sboard) line2 column)))

;Avalia o número de vezes que um número é repetido dentro de uma coluna para todo o tabuleiro
(defun evalBoard (sboard)
  (evalboardaux sboard 0))


(defun evalBoardAux (sboard n)
  (loop for column from 0 to (nsquare-num sboard) by 1 do
        (loop for line1 from 0 to (nsquare-num sboard) by 1 do
              (loop for line2 from (+ line1 1) to (nsquare-num sboard) by 1 do
                    (if (threatassessment sboard column line1 line2)(setf n (+ n 1))))))
  n)
  



;;--------------------------------------------------------------------------
;;         Funções do Algoritmo de Procura - Específicas ao problema
;;--------------------------------------------------------------------------

; Gera uma lista de nós sucessores
(defun generateNewNodes (sboard)
  (let ((list))
    (loop for x from 0 to 200 by 1 do
          (setf list (cons (swapnumberrandomline (makenewnode sboard)) list)))
    list))
          




;; Valida input e conta o tempo de execução
(defun solve (n)
  (if (AND (integerp n) (> n 0)) (time(solvefor n)) nil))



;;--------------------------------------------------------------------------
;;        Funções do Algoritmo de Procura  - Não específicas ao problema
;;--------------------------------------------------------------------------

;Percorre uma lista e retorna o primeiro elemento cujo valor da heurística é igual ou inferior ao valor indicado
(defun searchList (l threatLvl)
  (cond
   ((null l) nil)
   ((<= (evalboard (first l)) threatlvl)(first l))
   (T (searchList (rest l) threatLvl))))

       
;; Gera um novo tabuleiro aleatório e gera novos nós com base neste, até enontrar uma solução. Se esta não for encontrada antes
;; da iteração 4n, gera um novo tabuleiro e repete o processo. Se a qualquer momento não conseguir gerar um nó com valor de
;; heurística melhor ou igual ao anterior, reinicia o processo com um novo tabuleiro.
(defun solvefor (n)
  (let ((currentStateThreat) (currentState)(tempState))
    (format t "Start")(terpri)
    (loop while T do
    (setf currentState (makeNewRandomBoard n))
    (setf currentStateThreat (evalBoard currentState))
    (loop for x from 0 to (expt n 3) by 1 do
          (setf tempState (searchList (generatenewnodes currentState) currentStateThreat))
          (if tempstate (setf currentState tempState)(return))
          (setf currentStateThreat (evalboard currentState))
          (if (= 0 currentStateThreat)(return))
          (format t "Iteração:")(write x) (terpri))
    (if (= 0 currentStateThreat)(return))
    (format t "Reset")(terpri))
    currentState))



;;---------------------------------------------------------------------------
;;                   Funções de Data para Relatório
;;---------------------------------------------------------------------------

(defun populateDataTable (m)
  (loop for x from 2 to m by 1 do
        (loop for y from 0 to 1 by 1 do
              (let((realTime (get-internal-real-time)))
                (solve x)
                (with-open-file (str "C:/Users/Brollo/Desktop/lispData/data4.txt"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
                (format str "~,Dp~,F~%" x(/ (- (get-internal-real-time) realTime) internal-time-units-per-second)))))))
