;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lista1-2024-1-modelo) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Nome:

;; ========================================================================================
;;                              DEFINIÇÕES DE DADOS - não modifique
;; ========================================================================================

;; Definição de constantes:

;; Constantes para os valores das cartas especiais do UNO:
(define PULA_VEZ -1)
(define COMPRA2 -2)
(define INVERTE -3)
(define CURINGA -5)
(define CURINGA_COMPRA4 -4)

;; Constante representanto a posição da mão livre:
(define LIVRE "Livre")

;; Constantes de imagens:
(define CIRCULO_BRANCO (circle 40 "solid""white"))

(define QUADRADOS_COLORIDOS
         (above                                  
            (beside (rectangle 50 75 "solid" "red")
                    (rectangle 50 75 "solid" "green"))
            (beside (rectangle 50 75 "solid" "yellow")
                    (rectangle 50 75 "solid" "blue"))))

(define CONTORNO_PRETO (rectangle 110 160 "outline" "black"))


;; Tipos:

;; -----------------
;; TIPO Carta:
;; -----------------
(define-struct carta (cor valor))  
;; Um elemento do conjunto Carta é
;;   (make-carta c v)     onde
;;   c : String, é a cor da carta, que pode ser "azul", "verde", "amarelo" ou "vermelho" 
;;   v : Número, é o valor da carta, que pode ser qualquer inteiro entre 0 e 9,
;;               ou um número negativo -1 (PulaVez), -2 (Compra2), -3 (Inverte)

;; -------------
;; TIPO CartaUNO
;; -------------
;; Uma CartaUNO é
;; 1. Uma Carta, ou
;; 2. Um Número

;; ---------------------
;; TIPO CartaUNOouString
;; ---------------------
;; Uma  CartaUNOouString é
;; 1. Uma CartaUNO, ou
;; 2. Uma String

;; -----------------
;; TIPO Mão
;; -----------------
(define-struct mão (carta1 carta2 carta3))
;; Um elemento do conjunto Mão é
;;   (make-mão p1 p2 p3  )     onde
;;    p1, p2, p3: CartaUNOouString, que é carta ocupando cada posição.


;; ==============================================================
;; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;; ==============================================================

;; pontuação: .... -> ....
;; Obj: Dada uma carta UNO, devolve a pontuação de acordo com as regras do UNO:
;; cartas de 0 a 9 têm o seu valor nominal (0 a 9), cartas compra 2,
;; inverte e pula vez valem 20 e os curingas valem 50 pontos.
;; Exemplos:
;       (pontuação ...) = ...
;       (pontuação ...) = ...

(define (pontuação ...)
  ...)

;; Testes:
;     (check-expect (pontuação ...) ...) 
;     (check-expect (pontuação ...) ...)

;; ==============================================================
;; 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
;; ==============================================================

;; pontuação-mão: ... -> ...
;; Obj: ...
;; Exemplos/testes:
;       (chekc-expect (pontuação-mão ...) = ...
;       (check-expect (pontuação-mão ...) = ...

(define (pontuação-mão ...)
 ...)

;; ==============================================================
;; 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
;; ==============================================================

;; -----------------
;; TIPO Jogador:
;; -----------------
(define-struct jogador (...))
;; Um elemento do conjunto Jogador é
;;   (...)     onde
;;    ...: ..., é o nome do jogador
;;    ...: ..., é o número de pontos do jogador
;;    ...: ..., é a mão atual do jogador.

;; Constantes do tipo jogador:


;; ==============================================================
;; 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
;; ==============================================================

(define MÃO-VAZIA (make-mão LIVRE LIVRE LIVRE))

;; atualiza-jogador: ... -> ...
;; Obj: ...
;; Exemplos:
;; ...

(define (...)
  ...)

;; Testes: 
;; ...

;; ==============================================================
;; 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 
;; ==============================================================

;; -----------------
;; TIPO Mesa:
;; -----------------
(define-struct ...)
;; Um elemento do conjunto Mesa é
;;   (make-mesa ...)     onde
;;    ...: ...,
;;    ...

;; Constantes do tipo mesa:


;; ==============================================================
;; 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 
;; ==============================================================
;; -----------------
;; TIPO Fim:
;; -----------------
;; ...

;; -----------------
;; TIPO Resultado:
;; -----------------
;; ...

;; fim-de-jogo: ... -> ...
;; Obj: ...
;; Exemplos:
;; ...

(define (...)
  ...)

;; Testes: 
;; ...


;; ==============================================================
;; 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
;;                        QUESTÃO BÔNUS
;; ==============================================================

;; mostra-mesa : ... -> ...
;; Obj: ...
;; Exemplos:
;; ...

(define (...)
  ...)



;; ========================================================================================
;; FUNÇÕES DOS LABS 1 E 2 - As funções de desenho podem ser modificadas para a questão 7
;; ========================================================================================

;; ========================
;; JOGADA-VÁLIDA?
;; ========================
;; jogada-válida? : CartaUNO CartaUNO -> Booleano
;; Objetivo: A função analiza duas cartas e verifica se é possível
;; jogar uma sobre a outra, de acordo com as regras do UNO
;; Exemplos/teste:
    (check-expect (jogada-válida?  AZUL3 VERDE3)   #t)
    (check-expect (jogada-válida?  AZUL3 AMARELO1) #f)
    (check-expect (jogada-válida?  VERMELHO5 VERMELHO-COMPRA2) #t)
    (check-expect (jogada-válida?  VERMELHO-COMPRA2 CURINGA)   #t)
(define (jogada-válida? @carta-mesa @carta-mao)
     (cond
          ;; se a carta da mão for curinga, a jogada é válida
          [(number? @carta-mao)  #t]
          ;; se a carta da mesa for curinga, a jogada é válida
          [(number? @carta-mesa) #t] 
          ;; se as duas cartas forem da mesma cor ou do mesmo tipo/valor, a jogada é válida
          [(or (string=? (carta-cor @carta-mesa)(carta-cor @carta-mao))
               (= (carta-valor @carta-mesa) (carta-valor @carta-mao))) #t]
          ;; senão, a jogada é inválida
          [else #f] ))

;;====================
;; TRADUZ-COR
;;====================
;; traduz-cor : String -> String
;; Objetivo: a função recebe uma cor de carta UNO, e retorna a respectiva cor
;; em ingles, ou seja, "blue", "green", "yellow", "red" ou "black".
;; Exemplos:
     ;; (traduz-cor "azul") = "blue"
     ;; (traduz-cor "verde") = "green"
(define (traduz-cor uma-cor) ;; Dada uma cor uma-cor
  (cond
    ;; se uma-cor for "azul, devolver "blue"
    [(string=? "azul" uma-cor) "blue"]
    ;; se C for "verdel, devolver "green"
    [(string=? "verde" uma-cor) "green"]
    ;; se C for "vermelho, devolver "red"
    [(string=? "vermelho" uma-cor) "red"]
    ;; se C for "amarelol, devolver "yellow"
    [(string=? "amarelo" uma-cor) "yellow"]
    ;; senão, devolver "black"
    [else "black"]))

;; ========================
;; ESCOLHE-FUNDO 
;; ========================
;; escolhe-fundo: String -> Imagem
;; Dada uma cor, que pode ser amarelo, verde, vermelho, azul ou preto
;; gera a imagem de fundo para uma carta de UNO desta cor.
;; Exemplos:
     ;; (escolhe-fundo "vermelho")  desenha o fundo de uma carta vermelha
     ;; (escolhe-fundo "preto")     desenha o fundo de uma carta curinga
(define (escolhe-fundo *cor);; Dada uma cor *cor
    (overlay 
        CIRCULO_BRANCO ;; circulo branco
        (cond
           ;; se a cor COR for preto, desenhar os quadrados coloridos no fundo
           [(string=? *cor "4cores") QUADRADOS_COLORIDOS]
           ;; senão, desenhar um retângulo da cor desejada
           [else (rectangle 100 150 "solid" (traduz-cor *cor))])  ;; retangulo da cor da carta
        CONTORNO_PRETO));; contorno preto

;; ========================
;; DESENHA-CARTAS
;; ========================
;; desenha-carta : Número String -> Imagem
;; Objetivo: Dados um número e uma cor, representando uma carta de UNO,
;; gera uma imagem para esta carta,
;; Exemplos:
      ;; (desenha-carta 4 "vermelha")  desenha a carta número 4 vermelha
      ;; (desenha-carta COMPRA4 "preta")    desenha a carta curinga compra 4
(define (desenha-carta *num *cor)
  (overlay ;; sobrepor
          (escolhe-simbolo *num)  ;; o desenho do símbolo da carta
          (escolhe-fundo *cor)))  ;; com o fundo da carta

;; ========================
;; ESCOLHE-SIMBOLO
;; ========================
;; escolhe-simbolo: Número -> Imagem
;; Dado um número, que pode ser de 0 a 9 ou as constantes referentes às
;; cartas especiais de UNO, devolve uma imagem que representa este número na carta.
;; Exemplos/testes:
      (check-expect (escolhe-simbolo 8) (text (number->string 8) 70 "black"))
      (check-expect (escolhe-simbolo COMPRA2) (text "+2" 60 "black"))
(define (escolhe-simbolo *num)
  (cond
      ;; se a carta for numérica,     desenha a carta numérica
      [(and (>= *num  0) (<= *num 9))  (text (number->string *num) 70 "black")]
      ;; se a carta for um COMPRA2, desenha +2
      [(= *num COMPRA2)   (text "+2" 60 "black")]
      ;; se a carta for um INVERTE, desenha «
      [(= *num INVERTE)   (text "«" 60 "black")]    
      ;; se a carta for um PULA_VEZ, desenha Ø
      [(= *num PULA_VEZ)   (text "Ø" 60 "black")] 
      ;; se a carta for um CURINGA, não desenha nada (devolve uma imagem vazia)
      [(= *num CURINGA)   empty-image] 
      ;; se a carta for um CURINGA_COMPRA4, devolve +4
      [(= *num CURINGA_COMPRA4)  (text "+4" 60 "black")]))

;; ========================
;; DESENHA-MÃO
;; ========================
;; desenha-mão: Mão -> Imagem
;; Dada uma mão, gera uma imagem com as cartas da mão, lado a lado
;; Exemplo:
    ;; (desenha-mão (make-mão AZUL3 VERMELHO5 CURINGA AZUL-INVERTE)  desenha as cartas AZUL3 VERMELHO5 CURINGA AZUL-INVERTE, lado a lado
(define (desenha-mão @mao) ;; Dada uma mão @mao
  (beside ;; coloca lado a lado
        (desenha-carta-mão (mão-carta1 @mao))    ;; a imagem da carta da posição 1 da @mao
        (desenha-carta-mão (mão-carta2 @mao))    ;; a imagem da carta da posição 2 da @mao
        (desenha-carta-mão (mão-carta3 @mao))))  ;; a imagem da carta da posição 3 da @mao


;; ========================
;; DESENHA-CARTA-MÃO
;; ========================
;; desenha-carta-mão: Carta -> Imagem
;; Dada uma carta da mão, geraa imagem da carta, se for uma carta,
;; ou uma imagem de um retângulo escrito LIVRE, se não for carta.
;; Exemplos:
     ;; (desenha-carta-mão VERMELHO5) desenha ca carta vermelho 5
     ;; (desenha-carta-mão LIVRE) desenha um retângulo escrito LIVRE
(define (desenha-carta-mão @carta)
  (cond
      ;; se a posição for livre, desenha um retângulo escrito livre
      [(string? @carta) (overlay (text @carta 40 "black") CONTORNO_PRETO)]
      ;; se a carta for um curinga, desenha o curinga correspondente
      [(number? @carta) (desenha-carta @carta "4cores")]
      ;; senão, desenha a carta colorida correspondente
      [else (desenha-carta (carta-valor @carta) (carta-cor @carta))]))
