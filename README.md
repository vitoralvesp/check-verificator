# ♟️Está em Xeque?
Apresentado no **5º semestre de Ciência da Computação** na **Universidade Presbiteriana Mackenzie (UPM)** na disciplina de *Paradigmas de Linguagens de Programação*, "Está em Xeque?" é um projeto elaborado para o estudo de **paradigmas funcionais de programação** com *Haskell*. O objetivo é escrever um programa que irá receber uma lista de listas no formato da **Notação de Forsyth** e retornar verdadeiro se o rei branco está em xeque, ou falso caso contrário.

### Sumário
**Parte I:**  
[O que é Xeque](o-que-é-xeque?)  
[O que é Notação de Forsyth](o-que-é-notação-de-forsyth)  

**Parte II:**  
[Detalhes de Implementação](detalhes-de-implementação)  
[Documentação](documentação)  

**Parte III:**  
[Referências](referências)

---

<div id="user-content-toc">
  <ul style="list-style: none;">
    <summary>
      <h2>Parte I</h2>
    </summary>
  </ul>
</div>

### O que é Xeque?
Em partidas de xadrez, o xeque atua como um anúncio de perigo para algum dos reis no tabuleiro. Como um rei nunca pode ser capturado, o termo é utilizado para determinar que o rei está em ameaça e que o jogador que está em xeque deve mover o rei de modo que ele saia dessa posição.  
<details>
  <summary><code><b>Exemplo</b></code></summary>

  No tabuleiro a seguir, o jogador das peças brancas moveu o seu bispo para a casa **b5** para atacar o rei negro, colocando-o em posição de xeque.
  ![Exemplo de Xeque](https://images.chesscomfiles.com/uploads/v1/images_users/tiny_mce/pdrpnht/phpHXP5E1.png)
  
</details>

### O que é Notação Forsyth
Notação de Forsyth é um método utilizado para o registro da posição de uma peça em uma partida de xadrez, criado pelo jornalista escocês *David Forsyth*, tendo se tornado muito popular no século XIX.  
A regra para o registro de uma **posição Forsyth** é começar a registrar a posição das peças da linha das pretas (linha 8) até a linha das brancas (linha 1), correndo da esquerda para a direita, como em uma leitura. As peças são anotadas pelas letras, como ‘R’ para o Rei, ‘D’ para a Dama, ‘B’ para o Bispo, ‘C’ para Cavalo, ‘T’ para Torre e ‘P’ para Peão. As peças brancas são anotadas com letras maiúsculas e as peças pretas com letras minúsculas. As casas vazias são anotadas apenas com um número, indicando quantas casas vazias estão “na corrida”. Por exemplo, uma linha sem peças é anotada como um número 8. Se temos três casas vazias, uma Torre branca e quatro casas vazias, a linha é anotada como 3T4. Diferentes linhas são anotadas com um traço ou barra entre elas. Assim, a posição inicial do jogo é anotada como: `tcbdrbct-pppppppp-8-8-8-8-PPPPPPPP-TCBDRBCT`.

<br>

---

<div id="user-content-toc">
  <ul style="list-style: none;">
    <summary>
      <h2>Parte II</h2>
    </summary>
  </ul>
</div>

### Detalhes de Implementação
> [!NOTE]
> Os detalhes de implementação serão adicionados conforme a progressão do projeto.

### Documentação
> [!NOTE]
> Os detalhes da documentação serão adicionados conforme a progressão do projeto na seção **Wiki**.

<br>

---

<div id="user-content-toc">
  <ul style="list-style: none;">
    <summary>
      <h2>Parte III</h2>
    </summary>
  </ul>
</div>

### Referências
[Termos de Xadrez: Xeque](https://www.chess.com/pt-BR/terms/xeque-xadrez)  
[Notação de Forsyth](https://pt.wikipedia.org/wiki/Nota%C3%A7%C3%A3o_Forsyth)


