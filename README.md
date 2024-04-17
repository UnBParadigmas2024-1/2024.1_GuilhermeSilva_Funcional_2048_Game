<!-- # RepositorioTemplate
Esse repositório é para ser utilizado pelos grupos como um template inicial, da home page do Projeto.
As seções do Template NÃO DEVEM SER OMITIDAS, sendo TODAS RELEVANTES.

**!! *Atenção: Renomeie o seu repositório para (Ano.Semestre)_(Grupo)_(Paradigma)_(NomeDoProjeto)*. !!** 

Paradigmas:
 - Funcional
 - Logico
 - SMA -->

<!-- **!! *Não coloque os nomes dos alunos no título do repositório*. !!**

**!! *Exemplo de título correto: 2023.2_G1_Logico_ProjetoRoteirosAereos*. !!**
 
 (Apague esses comentários) -->

# 2024.1_GuilhermeSilva_Funcional_2048_Game

**Disciplina**: FGA0210 - PARADIGMAS DE PROGRAMAÇÃO - T01 <br>
**Nro do Grupo (de acordo com a Planilha de Divisão dos Grupos)**: Individual<br>
**Paradigma**: Funcional<br>

## Alunos
|Matrícula | Aluno |
| -- | -- |
| 18/0018019  |  Guilherme Daniel Fernandes da Silva |

## Sobre 
O jogo 2048 é um quebra-cabeça digital que foi desenvolvido em 2014 por Gabriele Cirulli, um desenvolvedor de software italiano. Desde então, tornou-se extremamente popular e foi adaptado para várias plataformas, incluindo navegadores da web, dispositivos móveis e computadores.

### Objetivo do Jogo:
O objetivo do jogo 2048 é combinar blocos numerados para criar um bloco com o valor 2048 em uma grade 4x4. O jogador move os blocos deslizando-os em uma direção específica (para cima, para baixo, para a esquerda ou para a direita) e todos os blocos no tabuleiro se movem na mesma direção até atingirem uma borda ou se combinarem com outro bloco.

### Regras do Jogo:
1. O jogo começa com dois blocos aleatórios (geralmente com o valor 2 ou 4) em uma grade 4x4.
2. O jogador pode deslizar os blocos na direção desejada usando as teclas de seta ou gestos de toque.
3. Quando dois blocos com o mesmo valor colidem durante um movimento, eles se combinam em um único bloco cujo valor é a soma dos valores originais dos dois blocos.
4. Após cada movimento, um novo bloco (com valor 2 ou 4) é adicionado aleatoriamente a uma posição vazia na grade.
5. O jogo continua até que não seja possível fazer mais movimentos (ou seja, não há mais espaços vazios e nenhum movimento válido disponível) ou até que um bloco com o valor 2048 seja criado.

### Estratégia:
Para alcançar o bloco de valor 2048, os jogadores geralmente seguem algumas estratégias básicas:
- Concentre-se em manter os blocos maiores em uma das bordas do tabuleiro para maximizar o espaço disponível.
- Priorize combinar blocos menores para liberar espaço e criar oportunidades para combinações maiores.
- Evite bloquear o movimento de blocos-chave que podem ser combinados no futuro.

### Objetivo:
Implementar o jogo de quebra-cabeça 2048 utilizando Haskell e consequentemente o paradígma funcional.

## Screenshots


## Instalação 
**Linguagens**: Haskell<br>
**Tecnologias**: Gloss, Reactive-Banana<br>

## Pré-requisitos
- Stack
- Homebrew (macOS) ou apt (Linux)

### Para instalar

#### macOS
```
brew install stack
```

#### Linux
```
sudo apt install stack
```

## Uso 
 
### Usando Stack
```
stack setup
stack build
stack exec 2048-game
```

## Vídeo
Adicione 1 ou mais vídeos com a execução do projeto.
Procure: 
(i) Introduzir o projeto;
(ii) Mostrar passo a passo o código, explicando-o, e deixando claro o que é de terceiros, e o que é contribuição real da equipe;
(iii) Apresentar particularidades do Paradigma, da Linguagem, e das Tecnologias, e
(iV) Apresentar lições aprendidas, contribuições, pendências, e ideias para trabalhos futuros.
OBS: TODOS DEVEM PARTICIPAR, CONFERINDO PONTOS DE VISTA.
TEMPO: +/- 15min

## Participações
Apresente, brevemente, como cada membro do grupo contribuiu para o projeto.
|Nome do Membro | Contribuição | Significância da Contribuição para o Projeto (Excelente/Boa/Regular/Ruim/Nula) |
| -- | -- | -- |
| Guilherme Daniel Fernandes da Silva  |  - Implementação de todo o projeto  | Excelente |

## Outros 
Quaisquer outras informações sobre o projeto podem ser descritas aqui. Não esqueça, entretanto, de informar sobre:
(i) Lições Aprendidas;
- **Dificuldade no uso de variáveis imutáveis:** Apesar da vantagem de tornarem o código mais previsível, as variáveis imutáveis muitas vezes tornam o processo de desenvolvimento desafiador pela necessidade de pensar de forma mais funcional na concepção da solução.

- **Abstrair o conceito de funções superiores pode ser desafiador:**  O uso de funções de ordem superior, ou seja, funções que aceitam outras funções como argumentos ou as retornam como resultados, torna às vezes o código ilegível.

- **Recursão:** É necessário utilizar recursão a todo momento. Isso torna muitas vezes difícil, principalmente quando nos deparamos com algo que estamos acostumados a pensar iterativamente.

(ii) Percepções;
- Uma dificuldade que tive durante a realização do trabalho foi em relação à curva de aprendizado da linguagem. Me senti muito atrás dos meus colegas, de modo que não consegui prosseguir no trabalho de forma coletiva. Com isso, tive que reduzir o escopo do que iria desenvolver em paralelo para conseguir apresentar algo.

- Um desafio que encontrei foi entender o código de outras pessoas. Devido ao excesso de abreviações e variáveis com nome pouco significativo, os códigos presentes na Internet apresentam baixa legibilidade, dificultando o processo de aprendizado na linguagem.

- A sintaxe da linguagem é distoante da maioria das outras linguagens modernas, o que dificulta o aprendizado. Mesmo utilizando um paradigma funcional, o Python, por exemplo, tem uma sintaxe muito mais amigável para o programador.

(iii) Contribuições e Fragilidades, e
**Contribuições**
- Uma contribuição que pude fazer foi na implementação de um jogo utilizando o paradígma funcional.
**Fragilidades**
- A lógica do jogo não está funcionando como deveria, de modo que os blocos não crescem além de 256.
- O placar do jogo não está preciso.

(iV) Trabalhos Futuros.
- Implementar um contador de movimentos, para limitar os movimentos possíveis para o usuário.
- Implementar um ranking de pontuações
- Implementar um menu para ser possível reiniciar o jogo
- Adicionar uma condição de derrota (ainda não funciona)

## Fontes
Referencie, adequadamente, as referências utilizadas.
Indique ainda sobre fontes de leitura complementares.

- Utilizei como base para a interface a seguinte referência: [2048 Haskell](https://github.com/8Gitbrix/2048Haskell)
- Também utilizei a seguinte referência como base para a lógica do jogo: [2048](https://github.com/gregorulm/h2048)
