main:-
    menu().

menu:-
    write("\nTESTE DE DATILOGRAFIA: \n \n"), write("Menu:\n 1. Registrar\n 2. Login\n 3. Ajuda\n 4. Sair\n"), leitura(Opcao), opcaoEscolhidaMenu(Opcao).

opcaoEscolhidaMenu(1):- write("Digite o nome de usuário:"),read_line_to_string(user_input,Username),
    string_concat('Usuarios/',Username,N1),string_concat(N1,'.txt',N2),
    \+ exists_file(N2) -> registraUser(Username); write("\nEsse nome de usuário já está em uso!\n"),
    menu().
opcaoEscolhidaMenu(2):- write("Digite o seu nome de usuário para restaurar seu progresso \n"), 
    read_line_to_string(user_input,Username), direcionaLogin(Username).
opcaoEscolhidaMenu(3):- open('guia.txt',read,Str), read_lines(Str,Guia), close(Str), write(Guia), nl, 
    write("\n Pressione enter para retornar ao menu \n"), read_line_to_string(user_input,Descarte), menu().
opcaoEscolhidaMenu(4):- halt(0).
opcaoEscolhidaMenu(_):- menu().

direcionaLogin(Username):- string_concat('Usuarios/',Username,Complemento),
    string_concat(Complemento,'.txt',Arquivo), exists_file(Arquivo) -> open(Arquivo,read,Str),
    read_line_to_string(Str,Nivel),close(Str),
    write("\nRegistro do usuário encontrado!\n"), ajusteNivel(Username,Nivel)
    ; write("\nUsuário inexistente! \n"), menu(). 

ajusteNivel(Username,"facil"):- exibeTexto(1,'facil',Username).
ajusteNivel(Username,"medio"):- write("Seu nível de dificuldade atual é Média. \n"),
    write("Deseja continuar do nível onde parou ou começar o desafio novamente?\n 1. Continuar \n 2. Reiniciar\n"),
    leitura(Opcao), Opcao =:= 1 -> exibeTexto(1,'medio',Username); exibeTexto(1,'facil',Username).
ajusteNivel(Username,"dificil"):- write("Seu nível de dificuldade atual é Difícil. \n"),
    write("Deseja continuar do nível onde parou ou começar o desafio novamente?\n 1. Continuar \n 2. Reiniciar\n"),
    leitura(Opcao), Opcao =:= 1 -> exibeTexto(1,'dificil',Username); exibeTexto(1,'facil',Username).

leitura(Opcao):-
	read_line_to_codes(user_input, Z),
	string_to_atom(Z, A),
	atom_number(A, Opcao).

registraUser(Username):- string_concat('Usuarios/',Username,Complemento),
    string_concat(Complemento,'.txt',Arquivo),
    open(Arquivo,append,X),write(X,'facil'),close(X), write("\nRegistrado com sucesso!\n"),
    exibeTexto(1,'facil',Username).

exibeTexto(4,'facil',Username):- string_concat('Usuarios/',Username,Complemento) ,string_concat(Complemento,'.txt',Arquivo)
    ,open(Arquivo,write,Stream),write(Stream,'medio')
    ,nl(Stream),close(Stream), exibeTexto(1,'medio',Username).
exibeTexto(Nivel,'facil',Username):- 
    write("Dificuldade: Fácil \n"),
    write("Nível: "), write(Nivel), write("\n"),
    controlaNivelFacil(Nivel,Texto),
    split_string(Texto,ListaTexto), 
    write("Pressione enter quando estiver preparado ou 0 para retornar ao menu. \n"),
    read_line_to_string(user_input,Descarte), Descarte \= "0" ->
    write("Comece a digitar! \n\n"),
    write(Texto),write("\n"), get_time(T),
    read_line_to_string(user_input,Resposta),get_time(G),
    split_string(Resposta,ListaResposta),
    calcula_tempo(T,G,Tempo), verificaTempo(Tempo,ResultadoTempo),
    verificaTexto(ListaTexto,ListaResposta,ResultadoTexto),
    verificacaoFinalFacil(ResultadoTempo,ResultadoTexto,Nivel,Username,Tempo,ListaResposta); menu().
exibeTexto(4,'medio',Username):- string_concat('Usuarios/',Username,Complemento) ,string_concat(Complemento,'.txt',Arquivo)
    ,open(Arquivo,write,Stream),write(Stream,'dificil')
    ,nl(Stream),close(Stream), exibeTexto(1,'dificil',Username). 
exibeTexto(Nivel,'medio',Username):- write("Dificuldade: Média \n"),
    write("Nível: "), write(Nivel), write("\n"),
    controlaNivelMedio(Nivel,Texto), split_string(Texto,ListaTexto),
    write("Pressione enter quando estiver preparado ou 0 para retornar ao menu. \n"),
    read_line_to_string(user_input,Descarte), Descarte \= "0" ->
    write("Comece a digitar! \n\n"),
    write(Texto),write("\n"), get_time(T),
    read_line_to_string(user_input,Resposta),get_time(G),
    split_string(Resposta,ListaResposta),
    calcula_tempo(T,G,Tempo), verificaTempo(Tempo,ResultadoTempo),
    verificaTexto(ListaTexto,ListaResposta,ResultadoTexto),
    verificacaoFinalMedio(ResultadoTempo,ResultadoTexto,Nivel,Username,Tempo,ListaResposta); menu().
exibeTexto(4,'dificil',Username):- write(" \n Parabéns! Você concluiu o Teste de Datilografia! \n") ,testeNovamente(Username).
exibeTexto(Nivel,'dificil',Username):- write("Dificuldade: Difícil \n"),
    write("Nivel: "), write(Nivel), write("\n"),
    controlaNivelDificil(Nivel,Texto), split_string(Texto,ListaTexto),
    write("Pressione enter quando estiver preparado ou 0 para retornar ao menu. \n"),
    read_line_to_string(user_input,Descarte), Descarte \= "0" ->
    write("Comece a digitar! \n\n"),
    write(Texto),write("\n"), get_time(T),
    read_line_to_string(user_input,Resposta),get_time(G),
    split_string(Resposta,ListaResposta),
    calcula_tempo(T,G,Tempo), verificaTempo(Tempo,ResultadoTempo),
    verificaTexto(ListaTexto,ListaResposta,ResultadoTexto),
    verificacaoFinalDificil(ResultadoTempo,ResultadoTexto,Nivel,Username,Tempo,ListaResposta); menu().

verificaTexto(A,B,-1):- length(A,LenA),length(B,LenB), LenA > LenB,!.
verificaTexto(A,B,-2):- length(A,LenA),length(B,LenB), LenB > LenA,!.
verificaTexto(A, B, N) :-
    length(A, LenA),
    length(B, LenB),
    (LenA \= LenB -> N = -1;
    (
        findall(X, (nth1(I, A, X), \+ member(X, B)), L),
        length(L, N)
    )).

verificacaoFinalFacil("Reprovado",_,Nivel,Username,_,_):- 
    write("\nO tempo máximo para digitação foi superado. Tente novamente! \n"), 
    exibeTexto(Nivel,'facil',Username).
verificacaoFinalFacil("Aprovado",-1,Nivel,Username,_,_):- 
    write("\nO texto fornecido é maior que o texto digitado! Tente Novamente \n"), exibeTexto(Nivel,'facil',Username).
verificacaoFinalFacil("Aprovado",-2,Nivel,Username,_,_):- 
    write("\nO texto digitado é maior que o texto fornecido!. Tente Novamente \n"), exibeTexto(Nivel,'facil',Username).
verificacaoFinalFacil("Aprovado",0,Nivel,Username,Tempo,ListaResposta):- 
    write("\nParabéns, você passou para o proximo nível!\n"), 
    write("Tempo gasto para digitar o texto proposto: "),
    write(Tempo),write(" segundos. \n"),
    write("Taxa de palavras por minuto (PPM): "), ppm(ListaResposta,Tempo,PPM), write(PPM),write("\n"),
    Lvl is Nivel+1, exibeTexto(Lvl,'facil',Username).
verificacaoFinalFacil("Aprovado",N,Nivel,Username,_,_):- 
    write("\nErro ao Digitar. Você digitou "), write(N), 
    write(" palavras erradas.  Tente novamente!\n") ,exibeTexto(Nivel,'facil',Username).

controlaNivelDificil(1,Texto):- random(1,4,N), atom_string(N,Txt), 
    string_concat('Textos/Dificil/dificil1',Txt,T1),string_concat(T1,'.txt',T2),
    open(T2,read,Str),read_lines(Str,Texto),close(Str),nl.
controlaNivelDificil(2,Texto):- random(1,4,N), atom_string(N,Txt), 
    string_concat('Textos/Dificil/dificil2',Txt,T1),string_concat(T1,'.txt',T2),
    open(T2,read,Str),read_lines(Str,Texto),close(Str),nl.
controlaNivelDificil(3,Texto):- random(1,4,N), atom_string(N,Txt), 
    string_concat('Textos/Dificil/dificil3',Txt,T1),string_concat(T1,'.txt',T2),
    open(T2,read,Str),read_lines(Str,Texto),close(Str),nl.

verificacaoFinalDificil("Reprovado",_,Nivel,Username,_,_):- 
    write("\nO tempo máximo para digitação foi superado. Tente novamente! \n"), 
    exibeTexto(Nivel,'dificil',Username).
verificacaoFinalDificil("Aprovado",-1,Nivel,Username,_,_):- 
    write("\nO texto fornecido é maior que o texto digitado!. Tente Novamente \n"), exibeTexto(Nivel,'dificil',Username).
verificacaoFinalDificil("Aprovado",-2,Nivel,Username,_,_):- 
    write("\nO texto digitado é maior que o texto fornecido!. Tente Novamente \n"), exibeTexto(Nivel,'dificil',Username).
verificacaoFinalDificil("Aprovado",0,Nivel,Username,Tempo,ListaResposta):- 
    write("\nParabéns, você passou para o proximo nível!\n"), 
    write("Tempo gasto para digitar o texto proposto: "),
    write(Tempo),write(" segundos. \n"),
    write("Taxa de palavras por minuto (PPM): "), ppm(ListaResposta,Tempo,PPM), write(PPM),write("\n"),
    Lvl is Nivel+1, exibeTexto(Lvl,'dificil',Username).
verificacaoFinalDificil("Aprovado",N,Nivel,Username,_,_):- 
    write("\nErro ao Digitar. Você digitou "), write(N), 
    write(" palavras erradas.  Tente novamente!\n") ,exibeTexto(Nivel,'dificil',Username).

controlaNivelMedio(1,Texto):- random(1,4,N), atom_string(N,Txt), 
    string_concat('Textos/Medio/medio1',Txt,T1),string_concat(T1,'.txt',T2),
    open(T2,read,Str),read_lines(Str,Texto),close(Str),nl.
controlaNivelMedio(2,Texto):- random(1,4,N), atom_string(N,Txt), 
    string_concat('Textos/Medio/medio2',Txt,T1),string_concat(T1,'.txt',T2),
    open(T2,read,Str),read_lines(Str,Texto),close(Str),nl.
controlaNivelMedio(3,Texto):- random(1,4,N), atom_string(N,Txt), 
    string_concat('Textos/Medio/medio3',Txt,T1),string_concat(T1,'.txt',T2),
    open(T2,read,Str),read_lines(Str,Texto),close(Str),nl.

verificacaoFinalMedio("Reprovado",_,Nivel,Username,_,_):- 
    write("\nO tempo máximo para digitação foi superado. Tente novamente! \n"), 
    exibeTexto(Nivel,'medio',Username).
verificacaoFinalMedio("Aprovado",-1,Nivel,Username,_,_):- 
    write("\nO texto fornecido é maior que o texto digitado! Tente Novamente \n"), exibeTexto(Nivel,'medio',Username).
verificacaoFinalMedio("Aprovado",-2,Nivel,Username,_,_):- 
    write("\nO texto digitado é maior que o texto fornecido! Tente Novamente \n"), exibeTexto(Nivel,'medio',Username).
verificacaoFinalMedio("Aprovado",0,Nivel,Username,Tempo,ListaResposta):- 
    write("\nParabéns, você passou para o proximo nível!\n"), 
    write("Tempo gasto para digitar o texto proposto: "),
    write(Tempo),write(" segundos. \n"),
    write("Taxa de palavras por minuto (PPM): "), ppm(ListaResposta,Tempo,PPM), write(PPM),write("\n"),
    Lvl is Nivel+1, exibeTexto(Lvl,'medio',Username).
verificacaoFinalMedio("Aprovado",N,Nivel,Username,_,_):- 
    write("\nErro ao Digitar. Você digitou "), write(N), 
    write(" Palavras erradas.  Tente novamente!\n"), exibeTexto(Nivel,'medio',Username).

controlaNivelFacil(1,Texto):- random(1,4,N), atom_string(N,Txt), 
    string_concat('Textos/Facil/facil1',Txt,T1),string_concat(T1,'.txt',T2),
    open(T2,read,Str),read_lines(Str,Texto),close(Str),nl.
controlaNivelFacil(2,Texto):- random(1,4,N), atom_string(N,Txt), 
    string_concat('Textos/Facil/facil2',Txt,T1),string_concat(T1,'.txt',T2),
    open(T2,read,Str),read_lines(Str,Texto),close(Str),nl.
controlaNivelFacil(3,Texto):- random(1,4,N), atom_string(N,Txt), 
    string_concat('Textos/Facil/facil3',Txt,T1),string_concat(T1,'.txt',T2),
    open(T2,read,Str),read_lines(Str,Texto),close(Str),nl.

verificaTempo(Tempo,"Reprovado"):- Tempo > 60.
verificaTempo(Tempo,"Aprovado"):- Tempo =< 60.

testeNovamente(Username):- write("Quer realizar o teste novamente no nível dificil?\n\n[S]im\n[N]ao\n"),
 read_line_to_string(user_input, R), opcaoTesteNovamente(R,Username).

opcaoTesteNovamente("S",Username):- exibeTexto(1,'dificil',Username).
opcaoTesteNovamente("N",_):- menu().
opcaoTesteNovamente(_,Username):- testeNovamente(Username).

calcula_tempo(T,G,Tempo):- 
    stamp_date_time(T,date(_,_,_,_,M1,S1,_,_,_),'UTC'),
    stamp_date_time(G,date(_,_,_,_,M2,S2,_,_,_),'UTC'),
    tempo(M1,M2,S1,S2,Tempo).

read_lines(Stream,Content):-
    read_string(Stream,End,Content).

tempo(M1,M1,S1,S2,Tempo):- T is S2-S1, Tempo is round(T).
tempo(M1,M2,S1,S2,Tempo):- Diferenca is M2-M1, Total is 60*Diferenca, S3 is S2+Total, T is S3-S1, Tempo is round(T).

split_string(String,Lista):-
    string_codes(String,Codes),
    atom_codes(Atom,Codes),
    atomic_list_concat(Lista,' ',Atom).

ppm(Lista,Tempo,PPM):- length(Lista,Len),A is Len / (Tempo/60), PPM is round(A).