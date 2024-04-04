;; Este arquivo contém um modelo para a solução dos exercícios da
;; lista a ser resolvida no laboratório 1 de INF05008.
;; Este exercício não será entregue, é um exercício de fixação.

;; Em cada questão, há um início da solução. Deixamos essas linhas
;; sempre comentadas para não causar erros de compilação, pois
;; não estão completas (em muitos pontos há '...' que deve ser
;; completado por você).

;;===============================================================
;; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;;===============================================================

;;Definindo as cartas especiais como constantes
(define PULA_VEZ 10)
(define COMPRA2 11)
(define INVERTE 12)
(define CURINGA 13)
(define CURINGA_COMPRA4 14)


;; Questão: Por que não é necessário definir contratos, objetivos
;; ou exemplos aqui?

;;===============================================================
;; 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
;;===============================================================

(define CIRCULO_BRANCO (circle 40 "solid" "white"))

(define QUADRADOS_COLORIDOS
    ;;cola as imagens para representar uma carta de quadrados coloridos
    ;;cola os retangulos de cima
    (beside (rectangle 50 75 "solid" "red")(rectangle 50 75 "solid" "green"))
    ;;cola os retangulos de baixo
    (beside (rectangle 50 75 "solid" "red")(rectangle 50 75 "solid" "green")))

(define CONTORNO_PRETO (rectangle 110 160 "outline" "black"))
;;===============================================================
;; 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 
;;===============================================================
;; ==> TRADUZ-COR
;; traduz-cor : String -> String
;; Objetivo: Dada uma cor, que pode ser  amarelo, verde, vermelho, azul ou 4cores,
;; retorna a respectiva cor em ingles, ou seja, "blue", "green", "yellow", "red" ou "4colors".
;; Exemplos:
;; (traduz-cor "azul") = "blue"
;; (traduz-cor "verde") = "green"

(define (traduz-cor uma-cor) ;; Dada uma cor uma-cor
  (cond
    ;; se uma-cor for "azul, devolver "blue"
    [(string=? uma-cor "azul") "blue"]
    ;; se ... for "verde", devolver "green"
    [(string=? "verde" uma-cor) "green"]
    ;; se ... for "vermelho, devolver "red"
    [(string=? "vermelho" uma-cor) "red"]
    ;; se ... for "amarelo, devolver "yellow"
    [(string=? "amarelo" uma-cor) "yellow"]
    ;; senão, devolver "4colors"
    [else "4colors"]))

;; Testes:
(check-expect (traduz-cor "vermelho") "red")
(check-expect (traduz-cor "verde") "green")
(check-expect (traduz-cor "4cores") "4colors")

;;===============================================================
;; 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
;;===============================================================

;; ========================
;; ESCOLHE-FUNDO - versão 1
;; ========================
;; escolhe-fundo: String -> Imagem
;; Dada uma cor, que pode ser amarelo, verde, vermelho, azul ou 4cores
;; gera a imagem de fundo para uma carta de UNO desta cor.
;; Exemplos:
;; (escolhe-fundo "vermelho")  desenha o fundo de uma carta vermelha
;; (escolhe-fundo "4cores")     desenha o fundo de uma carta curinga

(define (escolhe-fundo uma-cor) ;; dada uma cor uma-cor
    (overlay ;; sobrepor
        CIRCULO_BRANCO ;; circulo branco
        (cond
           ;; se a cor uma-cor for 4cores, desenhar os quadrados coloridos no fundo
           [(string=? uma-cor "4cores") QUADRADOS_COLORIDOS]
           ;; senão, desenhar um retângulo da cor desejada
           [else (rectangle 50 75 "solid" uma-cor)])  ;; retangulo da cor da carta
        CONTORNO_PRETO));; contorno preto

;; ========================
;; ESCOLHE-FUNDO - versão 2
;; ========================
;; escolhe-fundo-v2: String -> Imagem
;; Dada uma cor, que pode ser amarelo, verde, vermelho, azul ou 4cores
;; gera a imagem de fundo para uma carta de UNO desta cor.
;; Exemplos:
;; (escolhe-fundo "vermelho")  desenha o fundo de uma carta vermelha
;; (escolhe-fundo "4cores")     desenha o fundo de uma carta curinga

;(define (escolhe-fundo-v2 ...) ;; Dada uma cor...
;   (cond
;        ;; se a cor COR for 4cores, desenhar os quadrados coloridos no fundo
;       ...
;        ;; senão, desenhar um retângulo da cor desejada
;       ...

;; Testes:       

    (check-expect (escolhe-fundo "vermelho")  .)
   (check-expect (escolhe-fundo "vermelho") (overlay  CIRCULO_BRANCO (rectangle 100 150 "solid" "red") CONTORNO_PRETO))

   (check-expect (escolhe-fundo "4cores")  .)
   (check-expect (escolhe-fundo "4cores") (overlay  CIRCULO_BRANCO QUADRADOS_COLORIDOS CONTORNO_PRETO))

;; Qual a diferença entre as versões desta função?

;;===============================================================
;; 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
;;===============================================================
;; ========================
;; DESENHA-CARTAS
;; ========================
;; desenha-carta : Número String -> Imagem
;; Objetivo: Dados um número e uma cor, representando uma carta de UNO,
;; gera uma imagem para esta carta,
;; Exemplos:
;; (desenha-carta 4 "vermelha")  desenha a carta número 4 vermelha
;; (desenha-carta COMPRA4 "4cores")    desenha a carta curinga compra 4
(define (desenha-carta numero uma-cor)
  (overlay    ;; sobrepor
          (escolhe-simbolo numero) ;; o desenho do símbolo da carta e
          fundo))                 ;; o desenho do fundo da carta

;; escolhe-simbolo: Número -> Imagem
;; Dado um número, que pode ser de 0 a 9 ou as constantes referentes às
;; cartas especiais de UNO, devolve uma imagem que representa este número na carta.
;; Exemplos:
;;    (escolhe-simbolo 8) devolve a imagem do número oito
;;    (escolhe-simbolo COMPRA2) devolve a imagem +2

(define (escolhe-simbolo numero)
  (cond
      ;; se a carta for numérica, (0<=numero<=9)     desenha a carta numérica
      [(and (>= numero 0) (<= numero 9)) (text (number->string numero) 30 "black")]
      ;; se a carta for um COMPRA2, desenha +2
      [(= numero COMPRA2) (text "+2" 30 "black")]
      ;; se a carta for um INVERTE, desenha «
      [(= numero INVERTE) (text "«" 30 "black")]
      ;; se a carta for um PULA_VEZ, desenha Ø
      [(= numero PULA_VEZ) (text "Ø" 30 "black")]
      ;; se a carta for um CURINGA, não desenha nada (devolve uma imagem vazia)
      [(= numero CURINGA) empty-image]
      ;; se a carta for um CURINGA_COMPRA4, devolve +4
      [(= numero CURINGA_COMPRA4) (text "+4" 30 "black")])) 


;;===============================================================
;; 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 
;;===============================================================

;; jogada-válida? : Numero String Numero String -> Booleano
;; Objetivo: A função analiza duas cartas (representadas por 4 argumentos: um
;; número e uma string, representando a carta da mão e um número e uma string,
;; representando a carta da mesa, nesta ordem) e verifica se é possível
;; jogar uma sobre a outra, de acordo com as regras do UNO
;; Exemplos:
;;    (jogada-válida? 2 "vermelho" 4 "vermelho")=  #t 
;;    (jogada-válida? 2 "vermelho" CURINGA "4cores") = #t 
;;    (jogada-válida? CURINGA "4cores" 2 "vermelho") = #t
;;    (jogada-válida? 2 "vermelho" 2 "verde") = #t
;;    (jogada-válida? 2 "vermelho" 3 "verde") = #f

(define (jogada-válida?  num-hand cor-hand num-table cor-table)
   (cond
     ;; caso 1: se as duas cartas sao da mesma cor, devolver true
     [(string=? cor-hand cor-table) true]
     ;; caso 2: se as duas cartas sao do mesmo numero, devolver true
     
     )

;;; Testes:
;   (check-expect (jogada-válida? 3 "azul" 3 "verde")  #t )
;   (check-expect (jogada-válida? 8 "verde" CURINGA "4cores")  #t )
;   (check-expect (jogada-válida? CURINGA_COMPRA4 "4cores" PULA_VEZ "amarelo")  #t )
;   (check-expect (jogada-válida? INVERTE "azul" INVERTE "verde")  #t )
;   (check-expect (jogada-válida? 8 "verde" PULA_VEZ "amarelo")  #f )
;   (check-expect (jogada-válida? PULA_VEZ "amarelo" 5 "vermelho")  #f )
;   (check-expect (jogada-válida? 8 "verde" 5 "vermelho")  #f )

;;===============================================================
;; 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
;;===============================================================
;; mostra-jogada: Numero String Numero String -> Imagem
;; Objetivo: A função analiza duas cartas (representadas por 4 argumentos: um
;; número e uma string, representando a carta da mesa e um número e uma string,
;; representando a carta da mão, nesta ordem) e verifica se é possível
;; jogar uma sobre a outra, de acordo com as regras do UNO, desenhando uma
;; imagem mostrando as cartas e se é possível fazer a jogada ou não.


