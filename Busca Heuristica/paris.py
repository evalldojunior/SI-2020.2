import heapq

parisMetro = [
    [0,10,0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,8.5,0,0,0,0,0,10,3.5,0,0,0,0],
    [0,0,0,6.3,0,0,0,0,9.4,0,0,0,18.7,0],
    [0,0,0,0,13,0,0,15.3,0,0,0,0,12.8,0],
    [0,0,0,0,0,3,2.4,30,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,9.6,0,0,6.4,0,0],
    [0,0,0,0,0,0,0,0,0,0,12.2,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,5.1],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0]
]
paris = [
    [0,10,18.5,24.8,36.4,38.8,35.8,25.4,17.6,9.1,16.7,27.3,27.6,29.8],
    [0,0,8.5,14.8,26.6,29.1,26.1,17.3,10,3.5,15.5,20.9,19.1,21.8],
    [0,0,0,6.3,18.2,20.6,17.6,13.6,9.4,10.3,19.5,19.1,12.1,16.6],
    [0,0,0,0,12,14.4,11.5,12.4,12.6,16.7,23.6,18.6,10.6,15.4],
    [0,0,0,0,0,3,2.4,19.4,23.3,28.2,34.2,24.8,14.5,17.9],
    [0,0,0,0,0,0,3.3,22.3,25.7,30.3,36.7,27.6,15.2,18.2],
    [0,0,0,0,0,0,0,20,23,27.3,34.2,25.7,12.4,15.6],
    [0,0,0,0,0,0,0,0,8.2,20.3,16.1,6.4,22.7,27.6],
    [0,0,0,0,0,0,0,0,0,13.5,11.2,10.9,21.2,26.6],
    [0,0,0,0,0,0,0,0,0,0,17.6,24.2,18.7,21.2],
    [0,0,0,0,0,0,0,0,0,0,0,14.2,31.5,35.5],
    [0,0,0,0,0,0,0,0,0,0,0,0,28.8,33.6],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,5.1],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0]
]

class PriorityQueue:
    def __init__(self):
        self.estacoes = []

    def push(self, city, cost):
        heapq.heappush(self.estacoes, (cost, city))

    def pop(self):
        return heapq.heappop(self.estacoes)
    
    def border(self):
        return sorted(self.estacoes)

    def isEmpty(self):
        if (self.estacoes == []):
            return True
        else:
            return False


def getG(comeco, fim): 
    a = comeco.split('E')
    a = int(a[1]) - 1
    b = fim.split('E')
    b = int(b[1]) - 1

    if a > b:
        a, b = b, a
    resposta = parisMetro[a][b] 

    return resposta


def getH(comeco, fim):
    a = comeco.split('E')
    a = int(a[1]) - 1
    b = fim.split('E')
    b = int(b[1]) - 1

    if a > b:
        a, b = b, a
    resposta = paris[a][b]

    return resposta


def estacoesPossiveis(estacaoAtual,estacaoAnterior): 
    a = estacaoAtual.split('E')
    a = int(a[1]) - 1
    b = estacaoAnterior.split('E')
    b = int(b[1]) - 1
    estacoesPossiveis = []

    for i in range(len(parisMetro[a])):
        if parisMetro[a][i] != 0 and i > a: 
            estacao = "E" + str(i + 1)
            estacoesPossiveis.append(estacao)
        if parisMetro[i][a] != 0 and i <= a:
            estacao = "E" + str(i + 1)
            estacoesPossiveis.append(estacao)

    return estacoesPossiveis


def magicaAEstrelinhaLinda(comeco, fim):
    caminhos = {}
    distancias = {}
    pq = PriorityQueue()
    pq.push(comeco, 0)
    distancias[comeco] = 0
    caminhos[comeco] = None
    estacoesExpandidas = []
    estacaoAnterior = comeco

    while (pq.isEmpty() == False):
        estacaoAtual = pq.pop()[1]
        estacoesExpandidas.append(estacaoAtual)
        if (estacaoAtual == fim):
            break

        estacoes = estacoesPossiveis(estacaoAtual, estacaoAnterior)
        for i in estacoes:
            valorG = distancias[estacaoAtual] + getG(estacaoAtual,i)
            if (i not in distancias or valorG < distancias[i]):
                distancias[i] = valorG
                valorH = valorG + getH(i,fim)
                pq.push(i,valorH)
                caminhos[i] = estacaoAtual

        printFronteiras(pq, estacoesExpandidas)
        
    # printa a ultima fronteira
    printFronteiras(pq, estacoesExpandidas)
    # print resultado final
    printResultado(comeco, fim, caminhos, distancias)


def printFronteiras(pq, estacoesExpandidas):
    print(str(len(estacoesExpandidas)) + "ª Iteração \t" + "Fronteira Ordenada: " + str(pq.border()))
    print("\t\tEstações Expandidas: " + str(estacoesExpandidas) + "\n")


def printResultado(comeco, fim, caminhos, distancias):
    caminhoFinal = []
    i = fim
    adicionalMudarEstacao = 0

    sequenciaCores = []
    red = [3, 9, 11, 13]
    blue = [1, 2, 3, 4, 5, 6]
    yellow = [2, 5, 7, 8, 9, 10]
    green = [4, 8, 12, 13, 14]

    while (caminhos.get(i) != None):
        caminhoFinal.append(i)
        i = caminhos[i]

    caminhoFinal.append(comeco)
    # ordenando o percuso final na ordem certa
    caminhoFinal.reverse()

    # saber se trocou de linha
    for j in range(len(caminhoFinal)):
        if j == len(caminhoFinal)-1:
            break

        estacaoAtual = caminhoFinal[j].split('E')
        estacaoAtual = int(estacaoAtual[1])
        estacaoSeguinte = caminhoFinal[j+1].split('E')
        estacaoSeguinte = int(estacaoSeguinte[1])
        if (estacaoAtual in red) and (estacaoSeguinte in red):
            sequenciaCores.append("red")
        elif (estacaoAtual in blue) and (estacaoSeguinte in blue):
            sequenciaCores.append("blue")
        elif (estacaoAtual in yellow) and (estacaoSeguinte in yellow):
            sequenciaCores.append("yellow")
        elif (estacaoAtual in green) and (estacaoSeguinte in green):
            sequenciaCores.append("green")
            
    for j in range(len(sequenciaCores)):
        if j == len(sequenciaCores)-1:
            break

        corAtual = sequenciaCores[j]
        corSeguinte = sequenciaCores[j+1]
        if corAtual != corSeguinte:
            adicionalMudarEstacao = adicionalMudarEstacao + 4

    # prints
    print("Fim das iterações.")
    print("\n˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜\n")
    print("Resultados:\n")
    print("Estações percorridas: \t\t\t\t" + str(caminhoFinal))
    print("Tempo total: \t\t\t\t\t" + str(((distancias[fim]/30)*60)+adicionalMudarEstacao) + " minutos")
    print("Tempo sem as mudanças de linha: \t\t" + str(((distancias[fim]/30)*60)) + " minutos")
    print("Numero de estações percorridas: \t\t" + str(len(caminhoFinal)) + " estações")
    print("Distância total percorrida: \t\t\t" + str(distancias[fim]) + " Km")
    print("Sequencia das cores das linhas percorridas: \t" + str(sequenciaCores) + "\n")
    print("\n˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜\n\n")


def printCabecalho(comeco, fim):
    print("\n˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜\n")
    print("Problema do metrô de Paris - Busca Heusrística A*\n")
    print("Aluno: Evaldo Júnior - egsj@cin.ufpe.br\n")
    print("˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜\n")
    print("Menor caminho da estação " + comeco + " para a estação " + fim + ".\n\n")
    print("Fronteiras de busca:\n")


def main():
    # inputs 
    comeco = "E6"
    fim = "E13"

    # print cabecalho
    printCabecalho(comeco, fim)
    
    # magica
    magicaAEstrelinhaLinda(comeco, fim) 


if __name__ == "__main__":
    main()