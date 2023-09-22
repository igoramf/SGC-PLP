## Configuração do ambiente para HASKELL


#### Para começar, é preciso ter o GHC e Cabal instalado na sua máquina 

- Instalações mínimas : apenas GHC (o compilador) e ferramentas de compilação (principalmente Cabal e Stack) são instalados globalmente em seu sistema, usando o gerenciador de pacotes do sistema.

     - [Ubuntu](https://www.haskell.org/downloads/linux/) 
    
- Se você utiliza outro sistema (Linux/Unix, Windows ou MacOS), [clique neste link](https://www.haskell.org/downloads/), apesar de não garatirmos um bom funcionamento.

## Execução

- É necessário usar apenas os arquivos funcionais dentro da Branch "Corrigidos"
- Vá em modulos e baixe os arquivos Cliente,Filme,Funcionario,Principal,Util e Venda (Todos .hs)
- Logo em seguida baixe os arquivos CSV dentro da pasta de nome "arquivos"
- Para uma fácil execução, basta só digitar esse comando no terminal na pasta do projeto.

     ```hs
     runhaskell Principal.hs
     ```
