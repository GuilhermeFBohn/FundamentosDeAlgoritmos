
;; ===============================================================
;;                   DEFINIÇÕES DE DADOS
;; ===============================================================

;; Definição de constantes:

;; Constantes para as cartas especiais:
(define PULA_VEZ -1)
(define COMPRA2 -2)
(define INVERTE -3)
(define CURINGA -5)
(define CURINGA_COMPRA4 -4)

;; Constantes do tipo Imagem:
(define CIRCULO_BRANCO (circle 40 "solid""white"))
(define QUADRADOS_COLORIDOS
         (above                                  
            (beside (rectangle 50 75 "solid" "red")
                    (rectangle 50 75 "solid" "green"))
            (beside (rectangle 50 75 "solid" "yellow")
                    (rectangle 50 75 "solid" "blue"))))
(define CONTORNO_PRETO (rectangle 110 160 "outline" "black"))

;; ==============================================================
;; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;; ==============================================================
;; -----------------
;; TIPO Carta:
;; -----------------
(define-struct carta (cor valor))
;; Um elemento do conjunto Carta é
;;   (make-carta a1 a2)     onde
;;   a1 : String, é a cor da carta, que pode ser "azul", "verde", "amarelo" ou "vermelho" 
;;   a2 : Número, é o valor da carta

;; Constantes do tipo Carta:
 (define AZUL3 (make-carta "azul" 3))
 (define VERMELHO5 (make-carta "vermelho" 5))
 (define VERMELHO-COMPRA2 (make-carta "vermelho" COMPRA2))
 (define VERDE3 (make-carta "verde" 3))

;; ==============================================================
;; 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
;; ==============================================================

;; -----------------
;; TIPO CartaUNO: <==== Tipo Misto
;; -----------------
;; Um elemento do conjunto CartaUNO pode ser
;; 1. Uma Carta, ou
;; 2. Um Número
 

;; ==============================================================
;; 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
;; ==============================================================

;; jogada-válida? : Carta Carta  -> BooleanoOUString
;; Objetivo: A função analiza duas cartas e verifica se é possível
;; jogar uma sobre a outra, de acordo com as regras do UNO
;; Exemplos:
    (check-expect (jogada-válida?  AZUL3 VERDE3)   #t)
    (check-expect (jogada-válida?  AZUL3 VERMELHO5) #f)
    (check-expect (jogada-válida?  VERMELHO5 VERMELHO-COMPRA2) #t)
    (check-expect (jogada-válida?  VERMELHO-COMPRA2 CURINGA)   #t)

(define (jogada-válida? @carta-mesa @carta-mao)
     (cond
          ;; se a carta da mão for curinga, a jogada é válida
          [ (number? @carta-mao) #t ]
          [ (not (carta? @carta-mao)) #t ]
          ;; se a carta da mesa for curinga, a jogada é válida
          [ (number? @carta-mesa) #t ]
          ;; se as duas cartas forem da mesma cor ou do mesmo tipo/valor, a jogada é válida
          [ (or (string=? (carta-cor @carta-mesa) (carta-cor @carta-mao)) (= (carta-valor @carta-mesa) (carta-valor @carta-mao)))]
          
          ;; senão, a jogada é inválida
          [else #f ))


;; ==============================================================
;; 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
;; ==============================================================

;; Uma  CartaUNOouString é
;; 1. ..., ou
;; 2. ...

;; próxima-carta: ... -> ...
;; obj: ...
;; Exemplos/testes:
    ;; (check-expect (próxima-carta AZUL3)  (make-carta "azul" 4))
    ;; (check-expect (próxima-carta (make-carta "verde" 9))  (make-carta "verde" 0) )
    ;; (check-expect (próxima-carta VERMELHO-COMPRA2)  "Não é possível definir a próxima carta." )
    ;; (check-expect (próxima-carta CURINGA)  "Não é possível definir a próxima carta.")

;;(define (próxima-carta ...)

          ;; se a carta for curinga, devolver....

          ;; se a carta for especial de alguma cor, devolver ....
 
          ;; senão, devolver uma nova carta com
          [else (make-carta
               ;; a mesma cor da carta uma-carta
               (carta-cor uma-carta)
               ;;um novo numero que pode ser o numero da uma-carta+1
               (cond
                 [(= 9 (carta-valor uma-carta)) 0]
                 [else (+1 

;; ==============================================================
;; 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 
;; ==============================================================

;; Constante LIVRE:
(define LIVRE "Livre")

;; -----------------
;; TIPO Mão:
;; -----------------
(define-struct mão (carta1 carta2 carta3)
;; Um elemento do conjunto Mão é
;;   (make-mão c1 c2 c3)     onde
;;  c1 = CartaUNOouString, representa a carta da primeira posição da mão
;;  c2 = CartaUNOouString, representa a carta da segunda posição da mão
;;  c3 = CartaUNOouString, representa a carta da terceira posição da mão

  
;; Constantes do tipo Mão
 (define M1 (make-mão
             (make-carta "vermelho" 4)
           CURINGA
           "Livre"))
 (define M2 (make-mão VERMELHO5 AZUL3 VERMELHO-COMPRA2)
;; (define ... ...)
;; (define ... ...)
  
;; ==============================================================
;; 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 
;; ==============================================================

;; desenha-mão: ... -> ...
;; Dada uma mão, gera uma imagem com as cartas da mão, lado a lado
;; Exemplos:
    ;; ...
(define (desenha-mão uma-mao) ;; Dada uma mão 
  ;; coloca lado a lado
  (beside
         ;; a imagem da carta da posição 1 da mao
         (desenha-carta-mao (mão-carta1 uma-mao))
         ;; a imagem da carta da posição 2 da mao
         (desenha-carta-mao (mão-carta2 uma-mao))
         ;; a imagem da carta da posição 3 da mao
         (desenha-carta-mao (mão-carta3 uma-mao))))

   ;; desenha-carta-mao: CartaUNOouString -> Imagem
   (define (desenha-carta-mao @c)

       ;; se @c for uma string, ...
       [(string? @c) (rectangle 80 160 "outline" "black")]
       ;; se @c for um numero, desenha a carta curinga correspondente
       [(number? @c) (desena-carta @c "4cores")]
       ;; se @c for uma carta, desenha a carta com a cor e o numero de @c
       [(carta? @c) (desenha-carta (carta-valor @c) (carta-cor @c))]









;; =======================================================================
;;  FUNÇÕES DEFINIDAS NO LABORATÓRIO 1:
;; =======================================================================

;; ==> DESENHA-CARTAS
;; desenha-carta : Número String -> Imagem
;; Objetivo: Dados um número e uma cor, representando uma carta de UNO,
;; gera uma imagem para esta carta,
;; Exemplos:
;; (desenha-carta 4 "vermelha")  desenha a carta número 4 vermelha
;; (desenha-carta COMPRA4 "4cores")    desenha a carta curinga compra 4
(define (desenha-carta *num *cor)
  (overlay ;; sobrepor
          (escolhe-simbolo *num)  ;; o desenho do símbolo da carta
          (escolhe-fundo *cor)))  ;; com o fundo da carta

;; ==> ESCOLHE-SIMBOLO:
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

;; ==> ESCOLHE-FUNDO:
;; escolhe-fundo: String -> Imagem
;; Dada uma cor, que pode ser amarelo, verde, vermelho, azul ou colorido
;; gera a imagem de fundo para uma carta de UNO desta cor.
;; Exemplos:
;;    (escolhe-fundo "vermelho")  desenha o fundo de uma carta vermelha
;;    (escolhe-fundo "4cores")     desenha o fundo de uma carta curinga

(define (escolhe-fundo *cor);; Dada uma cor *cor
    (overlay 
        CIRCULO_BRANCO ;; circulo branco
        (cond
           ;; se a cor COR for preto, desenhar os quadrados coloridos no fundo
           [(string=? *cor "4cores") QUADRADOS_COLORIDOS]
           ;; senão, desenhar um retângulo da cor desejada
           [else (rectangle 100 150 "solid" (traduz-cor *cor))])  ;; retangulo da cor da carta
        CONTORNO_PRETO));; contorno preto

;; ==> TRADUZ-COR
;; traduz-cor : String -> String
;; Objetivo: a função recebe uma cor de carta UNO, e retorna a respectiva cor
;; em ingles, ou seja, "blue", "green", "yellow", "red" ou "4cores".
;; Exemplos/testes:
     (check-expect (traduz-cor "azul") "blue")
     (check-expect (traduz-cor "verde") "green")

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
    ;; senão, devolver "4cores"
    [else "4cores"]))


