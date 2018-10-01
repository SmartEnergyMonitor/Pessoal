;;-------------------------------------------------------------------------
;;                           Estrutura de Dados
;;-------------------------------------------------------------------------

(defstruct nQueen
  num        ;tabuleiro num x num
  qarray)     ;array de posições das rainhas


;;-------------------------------------------------------------------------



;;-------------------------------------------------------------------------
;;             Funções de Inicialização - Específicas ao problema
;;-------------------------------------------------------------------------

;; Inicializa tabuleiro
(defun initQBoard (n)
   (make-nQueen :num n
                :qarray (make-array n :initial-element 0)))

;; Gera posição aleatória
(defun randomPosition (board)
  (random (length(nqueen-qarray board))))


;; Novo tabuleiro sequencial baralhado
(defun newSeqRandomBoard (n)
	(swapRandom (newSeqBoard n)))  


;;swapRandom
(defun swapRandom (qboard)
  (loop for x from 0 to (getmaxpos qboard)by 1 do
  (swapQueen qboard))
  qboard)


;;troca posição de duas rainhas aleatórias
(defun swapQueen (qboard)
  (let ((temp) (num1 (randomposition qboard)) (num2 (randomposition qboard)))
    (setf temp (getqueenfromboard qboard num1))
    (setqueen qboard num1 (getqueenfromboard qboard num2))
    (setqueen qboard num2 temp)
  qboard))


; Sequencia um array
(defun seqArray (qboard)
 (loop for x from 0 to (getMaxPos qboard) do
        (setQueen qboard x x))
  qboard)

;; Novo Tabuleiro sequencial
(defun newSeqBoard (n)
  (seqArray (initqboard n)))


;; Copia um tabuleiro
(defun copyBoard (board)
  (copy-nqueen board))


;; Faz uma cópia de um nó
(defun makeNewNode (board)
  (let ((newNode (copyBoard board)))
    (setf (nqueen-qarray newNode)(make-array (length (getQArray board))))
    (loop for x from 0 to (getmaxpos board) by 1 do
          (setQueen newNode x (getqueenfromboard board x))) 
  newNode))
       


;;-------------------------------------------------------------------------



;;-------------------------------------------------------------------------
;;            Funções de Manipulação - Específicas ao problema
;;-------------------------------------------------------------------------

;; Posição máxima do tabuleiro
(defun getMaxPos (qboard)
  (- (length (nqueen-qarray qboard)) 1))

;; Set Rainha
(defun setQueen (board pos value)
  (setf (aref (nqueen-qarray board) pos) value))

;;Get Rainha
(defun getQueenFromBoard (board pos)
  (aref (nqueen-qarray board) pos))


;; Get Array
(defun getQArray (board)
  (nqueen-qarray board))


;;-------------------------------------------------------------------------



;;-------------------------------------------------------------------------
;;                Funções de Heurística - Específicas ao problema
;;-------------------------------------------------------------------------

;; Avalia o estado do tabuleiro
(defun EvalBoard (qboard)
  (EvalAux qboard 0))

(defun EvalAux (qboard n)
  (loop for x from 0 to (getMaxPos qboard) by 1 do
        (loop for y from (+ x 1) to (getMaxPos qboard) by 1 do
             (if (threatAssessment2 qboard x y) (setf n (+ n 1)))))
  n)


;; Compara duas rainhas e vê se se ameaçam
(defun threatAssessment (board  x y)
  (cond
   ((= (getQueenFromBoard board x) (getQueenFromBoard board y)) T)
   ((= (- (getQueenFromBoard board x) x) (- (getQueenFromBoard board y) y)) T)
   ((= (+ (getQueenFromBoard board x) x) (+ (getQueenFromBoard board y) y)) T)
   (T NIL)))


;; Compara duas rainhas e vê se se ameaçam - optimizado
(defun threatAssessment2 (board  x y)
   (OR (= (getQueenFromBoard board x) (getQueenFromBoard board y))
       (= (- (getQueenFromBoard board x) x) (- (getQueenFromBoard board y) y))
       (= (+ (getQueenFromBoard board x) x) (+ (getQueenFromBoard board y) y))))

;;---------------------------------------------------------------------------




;;---------------------------------------------------------------------------
;;          Funções do Algoritmo de Procura - Específicas ao problema
;;---------------------------------------------------------------------------

;; Gera novos nós
(defun generateNewNodes (board)
  (let ((list))
    (loop for x from 0 to 200 by 1 do
          (setf list(cons (swapQueen (makenewnode board)) list)))
    list))





;; Valida input e conta o tempo de execução
(defun solve (n)
  (if (AND (integerp n) (> n 3)) (time(solvefor n)) nil))


;;---------------------------------------------------------------------------




;;---------------------------------------------------------------------------
;;       Funções do Algoritmo de Procura - Não específicas ao problema
;;---------------------------------------------------------------------------


;; Percorre uma lista e retorna o primeiro nó com uma valor de heurística menor que o indicado
(defun searchList (l threatLvl)
  (cond
   ((null l) nil)
   ((<= (evalboard (first l)) threatlvl)(first l))
   (T (searchList (rest l) threatLvl))))

       
;; Gera um novo tabuleiro aleatório e gera novos nós com base neste, até enontrar uma solução. Se esta não for encontrada antes
;; da iteração n, gera um novo tabuleiro e repete o processo. Se a qualquer momento não conseguir gerar um nó com valor de
;; heurística melhor ou igual ao anterior, reinicia o processo com um novo tabuleiro.
(defun solvefor (n)
  (let ((currentStateThreat) (currentState)(tempState))
    (format t "Start")(terpri)
    (loop while T do
    (setf currentState (newseqrandomboard n))
    (setf currentStateThreat (evalboard currentState))
    (loop for x from 0 to (* n 4) by 1 do
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

(defun realTimeInSecondsInTerminal (function)
  (let ((userTime (get-internal-real-time)))
    (funcall function)
    (format t "Run Time: ~,6F seconds~%" (/ (- (get-internal-real-time) usertime) internal-time-units-per-second))))


(defun populateDataTable (m)
  (loop for x from 10 to m by 2 do
        (loop for y from 0 to 1 by 1 do
              (let((realTime (get-internal-real-time)))
                (solve x)
                (with-open-file (str "C:/Users/Brollo/Desktop/lispData/data.txt"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
                (format str "~,Dp~,F~%" x(/ (- (get-internal-real-time) realTime) internal-time-units-per-second)))))))
