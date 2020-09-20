% ------------------------------------------------------------------------%
% Declaração da classe abntex2 como uma extensão da classe memoir         %
% ------------------------------------------------------------------------%
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ABNT/abntex2_report}[12/12/2017 v1.0 abnTeX_report]
\usepackage[utf8]{inputenc}
\usepackage{multirow}
\RequirePackage{keyval}
\newcommand{\removelinebreaks}[1]{\begingroup\def\\{ }#1\endgroup}
% ------------------------------------------------------------------------%
% Repassa todos os parametros a classe memoir, 							  %
% mas captura as options article e twocolumn								  %
% ------------------------------------------------------------------------%
\providecommand{\ABNTEXisarticle}{false}
\DeclareOption{article}{
	\renewcommand{\ABNTEXisarticle}{true}
	\PassOptionsToClass{article}{memoir}%}
}
\providecommand{\ABNTEXistwocolumn}{false}
\DeclareOption{twocolumn}{
	\renewcommand{\ABNTEXistwocolumn}{true}
	\PassOptionsToClass{twocolumn}{memoir}%}
}
\DeclareOption*{%
  \PassOptionsToClass{\CurrentOption}{memoir}%
}
\ProcessOptions\relax
\LoadClass{memoir}
% ------------------------------------------------------------------------%
% Pacotes e customizações gerais do memoir                                %
% Controla as referencias internas do PDF                                 %
% ------------------------------------------------------------------------%
\RequirePackage{hyperref}
\pdfstringdefDisableCommands{\def\\{, }\def\and{; }}
% ------------------------------------------------------------------------%
% Controla os bookmarks do PDF											  %
% ------------------------------------------------------------------------%
\RequirePackage{bookmark}
% ------------------------------------------------------------------------%
% Nomes constantes do abnTeX2 em portugues (definicoes padroes)           %
% ------------------------------------------------------------------------%
\newcommand{\folhaderostoname}{Title page}
\newcommand{\epigraphname}{Epigraph}
\newcommand{\dedicatorianame}{Dedication}
\newcommand{\errataname}{Errata sheet}
\newcommand{\agradecimentosname}{Acknowledgements}
\newcommand{\anexoname}{ANNEX}
\newcommand{\anexosname}{Annex}
\newcommand{\apendicename}{APPENDIX}
\newcommand{\apendicesname}{Appendix}
\newcommand{\orientadorname}{Supervisor:}
\newcommand{\coorientadorname}{Co-supervisor:}
\newcommand{\folhadeaprovacaoname}{Approval}
\newcommand{\resumoname}{Summary}
\newcommand{\listadesiglasname}{List of abbreviations and acronyms}
\newcommand{\listadesimbolosname}{List of symbols}
\newcommand{\listadetabelasname}{Figure list}
\newcommand{\listadefigurasname}{Symbol list}
%\newcommand{\DOC}{sections/DOC.tex}
\renewcommand{\bibname}{REFERENCES }
\renewcommand{\pageautorefname}{page}
\renewcommand{\sectionautorefname}{section}
\renewcommand{\subsectionautorefname}{subsection}
\renewcommand{\subsubsectionautorefname}{subsubsection}
\renewcommand{\paragraphautorefname}{paragraph}
\renewcommand{\contentsname}{}
\renewcommand{\listfigurename}{}
\renewcommand{\listtablename}{}
\newcommand{\execsummarytitlename}[1][EXECUTIVE SUMMARY]{#1}
\newcommand{\resumoatitlename}[1][RESUMO]{#1}
\newcommand{\resumobtitlename}[1][ABSTRACT]{#1}
\newcommand{\listadesimbolsabrevtitlename}[1][LIST OF SYMBOLS AND ABBREVIATIONS]{#1}
\newcommand{\glosariotitlename}[1][GLOSSARY]{#1}
%\newcommand{\mainheadertitlename}{\MakeUppercase{BIR~TECHNICAL PRODUCTION}}
\newcommand{\techprodsimple}{BIR~TECH. PROD. }
\newcommand{\mainsubsubtitle}{\imprimirtiporelatorio~Report, of Project \imprimirnomeprojeto}
\newcommand{\authorstitle}{Authors}
\newcommand{\externalauthorstitle}{External Authors}
\newcommand{\titlename}{Title}
\newcommand{\documentversion}{Version}
\newcommand{\documentclassification}{Classification}
\newcommand{\advisoryinformation}{
	\begin{description}
		\item \textbf{Confidential Information} - Strategic information for the BIR and Senai Cimatec. Its handling is restricted to users previously authorized by the area Manager.
		\item \textbf{Restricted Information} - Information whose knowledge, handling and access control should be limited to a restricted group of employees who need to use it to perform their professional activities.
		\item \textbf{Internal Use Information} - Information intended for internal use by researchers and partners.
		\item \textbf{Public Information} - Information that can be distributed to outside audiences, which is usually done through the appropriate channels.
	\end{description}
}
\newcommand{\internationalcip}{International Cataloging Data in Publication (CIP)}
% ------------------------------------------------------------------------%
% Hifenização e caracteres especiais português-brasil por padrão          %
% ------------------------------------------------------------------------%
\RequirePackage[brazil]{babel}
\addto\captionsbrazil{% portugues-brasil
%  \renewcommand{\DOC}{sections/DOC.tex}
  \renewcommand{\listadetabelasname}{Lista de tabelas}
  \renewcommand{\listadefigurasname}{Lista de figuras}
  \renewcommand{\internationalcip}{Dados Internacionais de Catalogação na Publicação (CIP)}
  \renewcommand{\documentversion}{Versão}
  \renewcommand{\documentclassification}{Classificação}
  \renewcommand{\titlename}{Título}
  \renewcommand{\authorstitle}{Autores}
  \renewcommand{\externalauthorstitle}{Autores parceiros}
  \renewcommand{\mainsubsubtitle}{Relatório~\imprimirtiporelatorio~para o Experimento:~\imprimirnomeprojeto}
  %\renewcommand{\mainheadertitlename}{\MakeUppercase{PRODUÇÃO TÉCNICA BIR}}
  \renewcommand{\techprodsimple}{PROD. TEC. BIR }
  \renewcommand{\bibname}{REFERÊNCIAS }
  \renewcommand{\contentsname}{}
  \renewcommand{\indexname}{\'Indice}
  \renewcommand{\listfigurename}{}
  \renewcommand{\listtablename}{}
% ------------------------------------------------------------------------%
% ajusta nomes usados com a macro \autoref								  %
% ------------------------------------------------------------------------%
  \renewcommand{\pageautorefname}{p\'agina}
  \renewcommand{\sectionautorefname}{se{\c c}\~ao}
  \renewcommand{\subsectionautorefname}{subse{\c c}\~ao}
  \renewcommand{\subsubsectionautorefname}{subse{\c c}\~ao}
  \renewcommand{\paragraphautorefname}{par\'agrafo}
  \renewcommand{\folhaderostoname}{Folha de rosto}
  \renewcommand{\epigraphname}{Ep\'igrafe}
  \renewcommand{\dedicatorianame}{Dedicat\'oria}
  \renewcommand{\errataname}{Errata}
  \renewcommand{\agradecimentosname}{Agradecimentos}
  \renewcommand{\anexoname}{ANEXO}
  \renewcommand{\anexosname}{Anexos}
  \renewcommand{\apendicename}{AP\^ENDICE}
  \renewcommand{\apendicesname}{Ap\^endices}
  \renewcommand{\orientadorname}{Orientador:}
  \renewcommand{\coorientadorname}{Coorientador:}
  \renewcommand{\folhadeaprovacaoname}{Folha de aprova\c{c}\~ao}
  \renewcommand{\resumoname}{Resumo}
  \renewcommand{\listadesiglasname}{Lista de abreviaturas e siglas}
  \renewcommand{\listadesimbolosname}{Lista de s\'imbolos}
  \renewcommand{\execsummarytitlename}[1][SUMÁRIO EXECUTIVO]{#1}
  \renewcommand{\listadesimbolsabrevtitlename}[1][LISTRA DE SÍMBOLOS E ABREVIATURAS]{#1}
  \renewcommand{\glosariotitlename}[1][SUMÁRIO]{#1}
% ------------------------------------------------------------------------
% Configurações do pacote backref
% Usado sem a opção hyperpageref de backref
  \renewcommand{\backrefpagesname}{Citado na(s) página(s):~}
% ------------------------------------------------------------------------
% Texto padrão antes do número das páginas
  \renewcommand{\backref}{}
% ------------------------------------------------------------------------
% Define os textos da citação
  \renewcommand*{\backrefalt}[4]{
  	\ifcase #1 %
  	  Nenhuma citação no texto.%
  	\or
  	  Citado na página #2.%
  	\else
  	  Citado #1 vezes nas páginas #2.%
  	\fi%
  }%
  %
  \renewcommand{\advisoryinformation}{
  	\begin{description}
  		\item \textbf{Informações Confidenciais} - Informações estratégicas para o BIR e Senai Cimatec. Seu manuseio é restrito a usuários previamente autorizados pelo Gestor da área.
  		\item \textbf{Informações Restritas} - Informação cujo conhecimento, manuseio e controle de acesso devem estar limitados a um grupo restrito de pesquisadores que necessitam utilizá-la para exercer suas atividades profissionais.
  		\item \textbf{Informações de Uso Interno} - São informações destinadas à utilização interna por pesquisadores e parceiros.
  		\item \textbf{Informações Públicas} - Informações que podem ser distribuídas ao público externo, o que, usualmente, é feito através dos canais apropriados.
  	\end{description}
  }
}
%
\addto\captionsenglish{% ingles
% ------------------------------------------------------------------------%
% adjusts names from abnTeX2												  %
% ------------------------------------------------------------------------%
  \renewcommand{\bibname}{REFERENCES }
  \renewcommand{\folhaderostoname}{Title page}
  \renewcommand{\epigraphname}{Epigraph}
  \renewcommand{\dedicatorianame}{Dedication}
  \renewcommand{\errataname}{Errata sheet}
  \renewcommand{\agradecimentosname}{Acknowledgements}
  \renewcommand{\anexoname}{ANNEX}
  \renewcommand{\anexosname}{Annex}
  \renewcommand{\apendicename}{APPENDIX}
  \renewcommand{\apendicesname}{Appendix}
  \renewcommand{\orientadorname}{Supervisor:}
  \renewcommand{\coorientadorname}{Co-supervisor:}
  \renewcommand{\folhadeaprovacaoname}{Approval}
  \renewcommand{\resumoname}{Abstract}
  \renewcommand{\listadesiglasname}{List of abbreviations and acronyms}
  \renewcommand{\listadesimbolosname}{List of symbols}
  \renewcommand{\pageautorefname}{page}
  \renewcommand{\sectionautorefname}{section}
  \renewcommand{\subsectionautorefname}{subsection}
  \renewcommand{\subsubsectionautorefname}{subsubsection}
  \renewcommand{\paragraphautorefname}{paragraph}
  \renewcommand{\contentsname}{}
  \renewcommand{\listfigurename}{}
  \renewcommand{\listtablename}{}
  %
  \renewcommand{\backrefpagesname}{Cited in page(s):~}
  \renewcommand{\backref}{}
  \renewcommand*{\backrefalt}[4]{
  	\ifcase #1
  	No citation in the text.
  	\or
  	Cited on page #2.
  	\else
  	Cited #1 times on pages #2.
  	\fi
  }
}
% ------------------------------------------------------------------------%
% Customiza listas														  %
% ------------------------------------------------------------------------%
\RequirePackage{enumitem}
% ------------------------------------------------------------------------%
% Pacote de if/them/else													  %
% ------------------------------------------------------------------------%
\RequirePackage{ifthen}
\newcommand{\ifnotempty}[2]{\ifthenelse{\not\equal{#1}{}}{#2}{}}
\newcommand{\ifelsenotempty}[3]{\ifthenelse{\not\equal{#1}{}}{#2}{#3}}
\newcommand{\checkfor}[3]{\ifthenelse{\isundefined{#1}}{#2}{#3}}
% ------------------------------------------------------------------------%
% Pacote que permite realizar operações matemáticas no código TeX		  %
% ------------------------------------------------------------------------%
\RequirePackage{calc}
% ------------------------------------------------------------------------%
% Margens - NBR 14724/2011 - 5.1 Formato									  %
% ------------------------------------------------------------------------%
\setlrmarginsandblock{3cm}{2cm}{*}
\setulmarginsandblock{3cm}{2cm}{*}
\checkandfixthelayout
% ------------------------------------------------------------------------%
% Espacamento entre linhas - NBR 14724/2011 - 5.2 Espacamento			  %
% ------------------------------------------------------------------------%
\OnehalfSpacing
% ------------------------------------------------------------------------%
% Comandos públicos                                                       %
% ``Tamanho menor e uniforme'' ditada pela NBR 14724/2011 - 5.1           %
% ------------------------------------------------------------------------%
\newcommand{\ABNTEXfontereduzida}{\footnotesize}
% ------------------------------------------------------------------------%
% abnTeX2 name															  %
% ------------------------------------------------------------------------%
\newcommand{\abnTeX}{abn\TeX2}
% ------------------------------------------------------------------------%
% Fontes padrões de part, chapter, section, subsection e subsubsection    %
% ------------------------------------------------------------------------%
\newcommand{\ABNTEXchapterfont}{\bfseries}
\newcommand{\ABNTEXchapterfontsize}{\Large}
\newcommand{\ABNTEXsectionfont}{}
\newcommand{\ABNTEXsectionfontsize}{\large}
\newcommand{\ABNTEXsubsectionfont}{\bfseries}
\newcommand{\ABNTEXsubsectionfontsize}{\large}
\newcommand{\ABNTEXsubsubsectionfont}{\bfseries}
\newcommand{\ABNTEXsubsubsectionfontsize}{\normalsize}
\newcommand{\ABNTEXpartfont}{}
\newcommand{\ABNTEXpartfontsize}{\ABNTEXchapterfontsize}
% ------------------------------------------------------------------------%
% Comandos e variáveis privados                                           %
% Comando para registro do bookmark interno                               %
% ------------------------------------------------------------------------%
\newcounter{abntex@bookmarkcounter}
\setcounter{abntex@bookmarkcounter}{0}
\newcommand{\PRIVATEbookmarkthis}[1]{%
  \addtocounter{abntex@bookmarkcounter}{1}%
  \phantomsection\pdfbookmark[0]{#1}{chapterb\arabic{abntex@bookmarkcounter}}
}
% ------------------------------------------------------------------------%
% Comando que cria capítulos nao numerados ja com bookmark anotado        %
% ------------------------------------------------------------------------%
\newcommand{\pretextualchapter}[1]{%
  \addtocounter{abntex@bookmarkcounter}{1}%
  \PRIVATEbookmarkthis{#1}%
  \chapter*[#1]{#1}%
  }
\provideboolean{abntex@apendiceousecao}
\setboolean{abntex@apendiceousecao}{false}
% ------------------------------------------------------------------------%
% cleardoublepage se este documento não for article
% ------------------------------------------------------------------------%
\newcommand{\PRIVATEclearpageifneeded}{%
  \ifthenelse{\equal{\ABNTEXisarticle}{true}}{%
    %
  }{ % else
    \cleardoublepage%
  }
}
% ------------------------------------------------------------------------%
% Configurações de layout                                                 %
% Notas de rodapé - NBR 14724/2011 - 5.2.1                                %
% ------------------------------------------------------------------------%
\makeatletter
\renewcommand\@makefntext[1]{\hangindent=1.2em\leftskip=0.4em\hskip-0.4em\@makefnmark\hskip1.2em\relax#1}
%\renewcommand\@makefntext[1]{\leftskip=0.7cm\hskip-0.15cm\@makefnmark#1}
\makeatother
\renewcommand{\foottextfont}{\ABNTEXfontereduzida}
% ------------------------------------------------------------------------%
% Virgula em footnote consecutiva										  %
% ------------------------------------------------------------------------%
\let\oldFootnote\footnote
\newcommand\nextToken\relax
\renewcommand\footnote[1]{%
    \oldFootnote{#1}\futurelet\nextToken\isFootnote}
\newcommand\isFootnote{%
    \ifx\footnote\nextToken\textsuperscript{,}\fi}
% ------------------------------------------------------------------------%
% Numeração continua das figuras											  %
% ------------------------------------------------------------------------%
\ifthenelse{\equal{\ABNTEXisarticle}{true}}{%
  \counterwithout{section}{chapter}
  \counterwithout{figure}{section}
  \counterwithout{table}{section}
}{%else
  \counterwithout{figure}{chapter}
  \counterwithout{table}{chapter}
}
% ------------------------------------------------------------------------%
% Estilos de caption													  %
% ------------------------------------------------------------------------%
\captiondelim{\space--\space}
\captionnamefont{\ABNTEXfontereduzida}
\captiontitlefont{\ABNTEXfontereduzida}
% ------------------------------------------------------------------------%
% Configurações do TOC													  %
% nivel de profundidade													  %
% ------------------------------------------------------------------------%
\setsecnumdepth{subsubsection}
\settocdepth{subsubsection}   %\maxtocdepth{subsubsection}
% ------------------------------------------------------------------------%
% lista de ilustrações													  %
% ------------------------------------------------------------------------%
\renewcommand{\cftfigurename}{\figurename\space}
 \renewcommand*{\cftfigureaftersnum}{: }
%\renewcommand*{\cftfigureaftersnum}{~~--}
% ------------------------------------------------------------------------%
% lista de tabelas  													  %
% ------------------------------------------------------------------------%
\renewcommand{\cfttablename}{\tablename\space}
\renewcommand*{\cfttableaftersnum}{: }
% \renewcommand*{\cfttableaftersnum}{~~--}
% ------------------------------------------------------------------------%
% muda o tamanho do chapter no sumário									  %
% ------------------------------------------------------------------------%
\renewcommand{\tocheadstart}{\large}
% ------------------------------------------------------------------------%
% leaders para os capitulos												  %
% ------------------------------------------------------------------------%
%\renewcommand*{\cftchapterdotsep}{\cftdotsep}
% ------------------------------------------------------------------------%
% Configuração dos títulos dos capítulos									  %
% Novo estido do memoir 													  %
% ABNT NBR 14724-2011 - 5.2.2-5.2.4; ABNT NBR 6024-2003 - 3.1-3.8		  %
% ------------------------------------------------------------------------%
\provideboolean{abntex@innonumchapter}
\setboolean{abntex@innonumchapter}{true}
\makechapterstyle{abnt}{%
  \newlength{\chapternamenumlength}
% ------------------------------------------------------------------------%
% tamanhos de fontes de chapter e part									  %
% ------------------------------------------------------------------------%
   \ifthenelse{\equal{\ABNTEXisarticle}{true}}{%
     \setlength\beforechapskip{\baselineskip}
     \renewcommand{\chaptitlefont}{\ABNTEXsectionfont\bfseries\ABNTEXsectionfontsize}
   }{%else
     \setlength{\beforechapskip}{0pt}
     \renewcommand{\chaptitlefont}{\ABNTEXchapterfont\bfseries\ABNTEXchapterfontsize}
   }
  \renewcommand{\chapnumfont}{\chaptitlefont}
  \renewcommand{\parttitlefont}{\ABNTEXpartfont\bfseries\ABNTEXpartfontsize}
  \renewcommand{\partnumfont}{\ABNTEXpartfont\bfseries\ABNTEXpartfontsize}
  \renewcommand{\partnamefont}{\ABNTEXpartfont\bfseries\ABNTEXpartfontsize}
% ------------------------------------------------------------------------%
% tamanhos de fontes de section, subsection e subsubsection				  %
% ------------------------------------------------------------------------%
  \setsecheadstyle{\ABNTEXchapterfont\ABNTEXsectionfontsize}
  \setsubsecheadstyle{\ABNTEXsubsectionfont\ABNTEXsubsectionfontsize}
  \setsubsubsecheadstyle{\ABNTEXsubsubsectionfont\ABNTEXsubsubsectionfontsize}
% ------------------------------------------------------------------------%
% impressão do numero do capitulo										  %
% ------------------------------------------------------------------------%
  \renewcommand{\chapternamenum}{}
% ------------------------------------------------------------------------%
% impressão do nome do capitulo											  %
% ------------------------------------------------------------------------%
  \renewcommand{\printchaptername}{%
   \chaptitlefont
   \ifthenelse{\boolean{abntex@apendiceousecao}}{\appendixname}{}%
  }
% ------------------------------------------------------------------------%
% impressão do titulo do capitulo										  %
% ------------------------------------------------------------------------%
  \def\printchaptertitle##1{%
    \chaptitlefont%
    \ifthenelse{\boolean{abntex@innonumchapter}}{\centering##1}{% else
	    \settowidth{\chapternamenumlength}{\printchaptername\printchapternum\afterchapternum}%
	    \parbox[t]{\columnwidth-\chapternamenumlength}{##1}%
	}
  }
% ------------------------------------------------------------------------%
% impressão do número do capítulo                                         %
% ------------------------------------------------------------------------%
  \renewcommand{\printchapternum}{%
     \setboolean{abntex@innonumchapter}{false}
     \chapnumfont%
     \space\thechapter\space%
     \ifthenelse{\boolean{abntex@apendiceousecao}}{%
       --\space%
     }{} % else
  }
  \renewcommand{\afterchapternum}{}
% ------------------------------------------------------------------------%
% impressão do capitulo não numerado										  %
% ------------------------------------------------------------------------%
  \renewcommand\printchapternonum{%
     \setboolean{abntex@innonumchapter}{true}%
    }
}
\chapterstyle{abnt}
% ------------------------------------------------------------------------%
% Declarações de cabeçalhos 								       			  %
% Cabeçalho padrão														  %
% ------------------------------------------------------------------------%
\makepagestyle{abntheadings}
\makeevenfoot{abntheadings}{}{\thepage}{\ABNTEXfontereduzida\textit\leftmark}
% ------------------------------------------------------------------------%
% Cabecalho do inicio do capitulo									      %
% ------------------------------------------------------------------------%
\makepagestyle{abntchapfirst}
\makeoddfoot{abntchapfirst}{}{\thepage}{}
% ------------------------------------------------------------------------%
% Configura layout para elementos pre-textuais							  %
% ------------------------------------------------------------------------%
\newcommand{\pretextual}{%
  \aliaspagestyle{chapter}{empty}% customizing chapter pagestyle
  \pagestyle{empty}
  \aliaspagestyle{cleared}{empty}
  \aliaspagestyle{part}{empty}
}
\renewcommand{\frontmatter}{\pretextual}
% ------------------------------------------------------------------------%
% por default, inicializa o documento com elementos \pretextuais			  %
% ------------------------------------------------------------------------%
\AtBeginDocument{\pretextual}
% ------------------------------------------------------------------------%
% Configura layout para elementos textuais								  %
% ------------------------------------------------------------------------%
\newcommand{\textual}{%
  \ifthenelse{\equal{\ABNTEXisarticle}{true}}{%
    \pagestyle{plain}%
  }{%else
    \pagestyle{abntheadings}%
    \aliaspagestyle{chapter}{abntchapfirst}% customizing chapter pagestyle
  }
  \nouppercaseheads%
  \bookmarksetup{startatroot}%
}
\renewcommand{\mainmatter}{\textual}
% ------------------------------------------------------------------------%
% Configura layout para elementos pós-textuais							  %
% ------------------------------------------------------------------------%
\newcommand{\postextual}{%
 \bookmarksetup{startatroot}%
}
\renewcommand{\backmatter}{\postextual}
% ------------------------------------------------------------------------%
% Comandos de dados para CAPA e FOLHA DE ROSTO 							  %
% Comandos de dados - autor (use \and para multiplos autores)			  %
% ------------------------------------------------------------------------%
\newcommand{\autores}[1]{\author{#1}}
\newcommand{\imprimirautores}{\theauthor}
%\newcommand*{\portugueseuniversity}[1]{\gdef\@portugueseuniversity{#1}}
%\providecommand{\imprimirautoresexternos}{}
%\newcommand{\autoresexternos}[1]{\renewcommand{\imprimirautoresexternos}{#1}}
%%\newcommand{\autoresexternos}{}
% ------------------------------------------------------------------------%
% Comandos de dados - titulo do documento								  %
% ------------------------------------------------------------------------%
\newcommand{\titulo}[1]{\title{#1}}
\newcommand{\imprimirtitulo}{\thetitle}
% ------------------------------------------------------------------------%
% Comandos de dados - data												  %
% ------------------------------------------------------------------------%
\newcommand{\data}[1]{\date{#1}}
\newcommand{\imprimirdata}{\thedate}
% ------------------------------------------------------------------------%
% Comandos de dados - instituição										  %
% ------------------------------------------------------------------------%
\providecommand{\imprimirinstituicao}{}
\newcommand{\instituicao}[1]{\renewcommand{\imprimirinstituicao}{#1}}
% ------------------------------------------------------------------------%
% Comandos de dados - local												  %
% ------------------------------------------------------------------------%
\providecommand{\imprimirlocal}{}
\newcommand{\local}[1]{\renewcommand{\imprimirlocal}{#1}}
% ------------------------------------------------------------------------%
% Comandos de dados - prodtecnica										  %
% ------------------------------------------------------------------------%
\providecommand{\imprimirprodtecnica}{}
\newcommand{\prodtecnica}[1]{\renewcommand{\imprimirprodtecnica}{#1}}
% ------------------------------------------------------------------------%
% Comandos de dados - preambulo											  %
% ------------------------------------------------------------------------%
\providecommand{\imprimirpreambulo}{}
\newcommand{\preambulo}[1]{\renewcommand{\imprimirpreambulo}{#1}}
% ------------------------------------------------------------------------%
% Comandos de dados - orientador											  %
% ------------------------------------------------------------------------%
\providecommand{\imprimirorientadorRotulo}{}
\providecommand{\imprimirorientador}{}
\newcommand{\orientador}[2][\orientadorname]%
  {\renewcommand{\imprimirorientadorRotulo}{#1}%
   \renewcommand{\imprimirorientador}{#2}}
% ------------------------------------------------------------------------%
% Comandos de dados - coorientador										  %
% ------------------------------------------------------------------------%
\providecommand{\imprimircoorientadorRotulo}{}
\providecommand{\imprimircoorientador}{}
\newcommand{\coorientador}[2][\coorientadorname]%
  {\renewcommand{\imprimircoorientadorRotulo}{#1}%
   \renewcommand{\imprimircoorientador}{#2}}
% ------------------------------------------------------------------------%
% Comandos de dados - tipo de trabalho 									  %
% ------------------------------------------------------------------------%
\providecommand{\imprimirtipotrabalho}{}
\newcommand{\tipotrabalho}[1]{\renewcommand{\imprimirtipotrabalho}{#1}}
% ------------------------------------------------------------------------%
% Comandos de dados - tipo de trabalho 									  %
% ------------------------------------------------------------------------%
\providecommand{\imprimirtiporelatorio}{}
\newcommand{\tiporelatorio}[1]{\renewcommand{\imprimirtiporelatorio}{#1}}
% ------------------------------------------------------------------------%
% Comandos de dados - nome do projeto 									  %
% ------------------------------------------------------------------------%
\providecommand{\imprimirnomeprojeto}{}
\newcommand{\nomeprojeto}[1]{\renewcommand{\imprimirnomeprojeto}{#1}}
% ------------------------------------------------------------------------%
% Comandos de dados - outros subtítulos									  %
% ------------------------------------------------------------------------%
\providecommand{\imprimiroutrossubtitulos}{}
\newcommand{\outrossubtitulos}[1]{\renewcommand{\imprimiroutrossubtitulos}{#1}}
% ------------------------------------------------------------------------%
% Comandos de dados - classificação										  %
% ------------------------------------------------------------------------%
\providecommand{\imprimirclassificacao}{}
\newcommand{\classificacao}[1]{\renewcommand{\imprimirclassificacao}{#1}}
% ------------------------------------------------------------------------%
% Comandos de dados - revisão											  %
% ------------------------------------------------------------------------%
\providecommand{\imprimirrevisao}{}
\newcommand{\revisao}[1]{\renewcommand{\imprimirrevisao}{#1}}
\providecommand{\imprimirtabelacutter}{}
\newcommand{\tabelacutter}[1]{\renewcommand{\imprimirtabelacutter}{#1}}
\providecommand{\imprimirclassificacaoassunto}{}
\newcommand{\classificacaoassunto}[1]{\renewcommand{\imprimirclassificacaoassunto}{#1}}
\providecommand{\imprimirpalavraschave}{}
\newcommand{\palavraschave}[1]{\renewcommand{\imprimirpalavraschave}{#1}}
\providecommand{\imprimirparceirologo}{}
\newcommand{\parceirologo}[1]{\renewcommand{\imprimirparceirologo}{#1}}
% ------------------------------------------------------------------------%
% Comandos e ambientes de editoração PRE-TEXTUAIS						  %
% Ambiente para definicao da Capa										  %
% ------------------------------------------------------------------------%
\newenvironment{capa}{\begin{titlingpage}}{\end{titlingpage}\cleardoublepage}
\newenvironment{catalografica}{\begin{titlingpage}}{\end{titlingpage}\cleardoublepage}
% ------------------------------------------------------------------------%
% Ficha catalografica (Dados internacionais de catalogação-na-publicacao) %
% ------------------------------------------------------------------------%
\newenvironment{fichacatalografica}{%
    \thispagestyle{empty}
    \begin{SingleSpacing}
      \ABNTEXfontereduzida
}{%
    \end{SingleSpacing}%
}%
% ------------------------------------------------------------------------%
% Errata																  %
% ------------------------------------------------------------------------%
\newenvironment{errata}[1][\errataname]{%
   \pretextualchapter{#1}
  }{\PRIVATEclearpageifneeded}
% ------------------------------------------------------------------------%
% Dedicatoria															  %
% ------------------------------------------------------------------------%
\newenvironment{dedicatoria}[1][]{%
   \ifthenelse{\equal{#1}{}}{%
    \PRIVATEbookmarkthis{\dedicatorianame}
    }{\pretextualchapter{#1}}
  }{\PRIVATEclearpageifneeded}
% ------------------------------------------------------------------------%
% Agradecimentos														  %
% ------------------------------------------------------------------------%
\newenvironment{agradecimentos}[1][\agradecimentosname]{%
   \pretextualchapter{#1}
  }{\PRIVATEclearpageifneeded}
% ------------------------------------------------------------------------%
% Epigrafe																  %
% ------------------------------------------------------------------------%
\newenvironment{epigrafe}[1][]{%
   \ifthenelse{\equal{#1}{}}{%
    \PRIVATEbookmarkthis{\epigraphname}
    }{\pretextualchapter{#1}}
  }{\PRIVATEclearpageifneeded}
% ------------------------------------------------------------------------%
% Resumo e abstracts em diferentes idiomas - NBR 14724/2011				  %
% ------------------------------------------------------------------------%
\newenvironment{resumo}[1][\resumoname]{%
   \PRIVATEbookmarkthis{#1}
   \renewcommand{\abstractnamefont}{\chaptitlefont}
   \renewcommand{\abstractname}{#1}
   \begin{abstract}
   \SingleSpacing
  }{\end{abstract}\PRIVATEclearpageifneeded}
\newenvironment{resumoumacoluna}[1][\resumoname]{%
   \PRIVATEbookmarkthis{#1}
   \renewcommand{\abstractnamefont}{\chaptitlefont}
   \renewcommand{\abstractname}{#1}
   \begin{onecolabstract}
   \SingleSpacing
}{\end{onecolabstract}\PRIVATEclearpageifneeded}
% ------------------------------------------------------------------------%
% Lista de abreviaturas e siglas											  %
% ------------------------------------------------------------------------%
\newenvironment{siglas}{%
  \pretextualchapter{\listadesiglasname}
  \begin{symbols}
}{%
  \end{symbols}
  \cleardoublepage
}
% ------------------------------------------------------------------------%
% Lista de sembolos														  %
% ------------------------------------------------------------------------%
\newenvironment{simbolos}{%
  \pretextualchapter{\listadesimbolosname}
  \begin{symbols}
}{%
  \end{symbols}
  \cleardoublepage
}
% ------------------------------------------------------------------------%
% Comandos e ambientes de editoracao APENDICES e ANEXOS					  %
% ------------------------------------------------------------------------%
\makeatletter
\newcommand\@switch[1]{ \@writefile{toc}{\renewcommand*{\cftappendixname}{#1 \space}} }
\newcommand{\switchchapname}[1]{ \protected@write \@auxout {}{\string\@switch{#1} }}
\makeatother
% ---
\newcommand{\PRIVATEapendiceconfig}[2]{%
 \setboolean{abntex@apendiceousecao}{true}%
 \renewcommand{\appendixname}{#1}
 \renewcommand{\appendixpagename}{#2}
 \renewcommand{\appendixtocname}{#2}
 \switchchapname{#1}% Corrected from \switchapname -> \switchchapname
 \renewcommand*{\cftappendixname}{#1 \space}
}
% ------------------------------------------------------------------------%
% Apendices - comando  													  %
% ------------------------------------------------------------------------%
\newcommand{\apendices}{%
 \PRIVATEapendiceconfig{\apendicename}{\apendicesname}
 \appendix
}
% ------------------------------------------------------------------------%
% Apendices - ambiente                                                    %
% ------------------------------------------------------------------------%
\newenvironment{apendicesenv}{%
 \PRIVATEapendiceconfig{\apendicename}{\apendicesname}
 \begin{appendix}
}{
 \end{appendix}
 \setboolean{abntex@apendiceousecao}{false}
 \bookmarksetup{startatroot}
}
% ------------------------------------------------------------------------%
% Anexos - comando														  %
% ------------------------------------------------------------------------%
\newcommand{\anexos}{%
 \cftinserthook{toc}{AAA}

 \PRIVATEapendiceconfig{\anexoname}{\anexosname}
 \appendix
 \renewcommand\theHchapter{anexochapback.\arabic{chapter}}
}
% ------------------------------------------------------------------------%
% Anexos - ambiente														  %
% ------------------------------------------------------------------------%
\newenvironment{anexosenv}{%
 \cftinserthook{toc}{AAA}

 \PRIVATEapendiceconfig{\anexoname}{\anexosname}
 \begin{appendix}
 \renewcommand\theHchapter{anexochapback.\arabic{chapter}}
}{
 \end{appendix}
 \setboolean{abntex@apendiceousecao}{false}
 \bookmarksetup{startatroot}
}
% ------------------------------------------------------------------------%
% Imprime uma pagina de divisao no estilo parte							  %
% ------------------------------------------------------------------------%
\newcommand{\partpage}{%
  \cleardoublepage
  \phantomsection
  \addcontentsline{toc}{part}{\appendixpagename}
  \appendixpage*
}
% ------------------------------------------------------------------------%
% atalhos para \partpage ou \appendixpage* 								  %
% ------------------------------------------------------------------------%
\makeatletter
\newcommand{\partapendices}{\@ifstar{\appendixpage*}{\partpage}}
\newcommand{\partanexos}{\@ifstar{\appendixpage*}{\partpage}}
\makeatother
% ------------------------------------------------------------------------%
% Comandos e ambientes de editoracao CITACAO, ALINEA, INCISO, ASSINATURA  %
% Citacao direta com mais de 3 linhas - ABNT NBR 10520/2002 - 5.3         %
% ------------------------------------------------------------------------%
\newlength{\ABNTEXcitacaorecuo}% recuo de 4 cm da margem esquerda
\ifthenelse{\equal{\ABNTEXistwocolumn}{true}}{%
	\setlength{\ABNTEXcitacaorecuo}{1.8cm}
}{% else
\setlength{\ABNTEXcitacaorecuo}{4cm}
}
\newenvironment*{citacao}{%
	\list{}%
	\footnotesize%
	\addtolength{\leftskip}{\ABNTEXcitacaorecuo}%
	\item[]%
	\begin{SingleSpace}%
	}{%
\end{SingleSpace}%
\endlist}%
% ------------------------------------------------------------------------%
% Ambiente para alineas e e subalineas (incisos)							  %
% ABNT NBR 6024/2012 - 4.2 e 4.3											  %
% ------------------------------------------------------------------------%
\newlist{alineas}{enumerate}{2}
\setlist[alineas,1]{label={\alph*)},topsep=0pt,itemsep=0pt,leftmargin=\parindent+\labelwidth-\labelsep}%
\setlist[alineas,2]{label={--},topsep=0pt,itemsep=0pt,leftmargin=*}
\newlist{subalineas}{enumerate}{1}
\setlist[subalineas,1]{label={--},topsep=0pt,itemsep=0pt,leftmargin=*}%
\newlist{incisos}{enumerate}{1}
\setlist[incisos,1]{label={--},topsep=0pt,itemsep=0pt,leftmargin=*}%
% ------------------------------------------------------------------------%
% Comando for signature : width of the line and text under the line       %
% ------------------------------------------------------------------------%
\newlength{\ABNTEXsignwidth}
\setlength{\ABNTEXsignwidth}{13.4cm}
% ------------------------------------------------------------------------%
% thickness of the line													  %
% ------------------------------------------------------------------------%
\newlength{\ABNTEXsignthickness}
\setlength{\ABNTEXsignthickness}{1pt}
% ------------------------------------------------------------------------%
% ammount of space left between previous text and th signature line       %
% ------------------------------------------------------------------------%
\newlength{\ABNTEXsignskip}
\setlength{\ABNTEXsignskip}{1cm}
% ---
\makeatletter
\newcommand{\assinatura}{\@ifstar{\ABNTEXsign}{\ABNTEXcsign}}
\makeatother
% ---
\newcommand{\ABNTEXsign}[1]{%
	\parbox[t]{\ABNTEXsignwidth}{\SingleSpacing\vspace*{\ABNTEXsignskip}\centering%
		\rule{\ABNTEXsignwidth}{\ABNTEXsignthickness}\\%
		\nopagebreak #1\par}%
}
\newcommand{\ABNTEXcsign}[1]%
{\begingroup\par\centering\ABNTEXsign{#1}\par\endgroup}
% ------------------------------------------------------------------------%
% IMPRESSOES PADROES DE CAPA E FOLHA DE ROSTO							  %
% ------------------------------------------------------------------------%
\newcommand{\imprimircapa}{
	\begin{capa}

		\begin{center}
			\begin{figure}[htp]
				\centering
				\ifelsenotempty{\imprimirparceirologo}{
					\raisebox{-0.5\height}{\includegraphics[height=1.8cm]{\imprimirparceirologo}}
					\hspace{\fill}
					\raisebox{-0.5\height}{\includegraphics[height=1.8cm]{logos/LogoFiebSenai.png}}
				}{
					\hspace{\fill}
					\raisebox{-0.5\height}{\includegraphics[height=1.8cm]{logos/LogoFiebSenai.png}}
				}
			\end{figure}
		\end{center}

		\vspace*{-1cm}

		%\begin{flushright}
			%\textbf{\footnotesize \techprodsimple - \imprimirprodtecnica}
		%\end{flushright}

		\centering
		\vspace*{1cm}
		%\MakeUppercase{\large\textbf{\mainheadertitlename}}\\
		\vspace*{1cm}
		\MakeUppercase{\large\textbf{\imprimirtitulo}}\\
		\mainsubsubtitle\\
		\imprimiroutrossubtitulos\\
		\vspace*{\fill}
		\begin{flushright}
			\textbf{\authorstitle:}\\
			\imprimirautores
			\\~\\
			\checkfor{\autoresexternos}{}{
				\textbf{\externalauthorstitle:}\\
				\autoresexternos
			}
		\end{flushright}

		\vspace*{\fill}
		{
			\ABNTEXchapterfont\Large\textbf\imprimirlocal\\
			\vspace*{1cm}
			\imprimirdata
		}
		\par
	\end{capa}
}
% ------------------------------------------------------------------------
% Impressão da Capa catalografica
\newcommand{\imprimircatalografica}{
	\begin{catalografica}
		\centering
		\begin{table}[]
			\centering
			\begin{tabular}{|l|c|}
				\hline
				\multicolumn{2}{|l|}{\titlename: \imprimirtitulo} \\ \hline
				\textbf{\small{\techprodsimple - \imprimirprodtecnica}}          & \documentversion          \\ \cline{1-1}
				\textbf{\documentclassification:} \imprimirclassificacao          & \imprimirrevisao          \\ \hline
			\end{tabular}
		\end{table}
		\advisoryinformation
		\vspace*{\fill}
		\textbf{\internationalcip}
		\begin{table}[H]
			\centering
			\begin{tabular}{|llr|}
				\hline
				&                                   &                                                  \\
				\imprimirtabelacutter & \multicolumn{2}{l|}{
					\begin{tabular}[c]{@{}l@{}}
						\imprimirautores
						\\~
						\checkfor{\autoresexternos}{}{
							\\\autoresexternos
						}
					\end{tabular}
				}
				\\
				&                                   &                                                  \\
				& \multicolumn{2}{l|}{
					\begin{tabular}[c]{@{}l@{}}
						\imprimirtitulo \\
					\end{tabular}
				}
				\\
				& \multicolumn{2}{l|}{
					\begin{tabular}[c]{@{}l@{}}
						\imprimirlocal \\
						\imprimirdata
					\end{tabular}
				}                                                     \\
				&                                   &                                                  \\
				&                                   &                                                  \\
				%&                                   &                                                  \\
				& Keywords:                          &                                                  \\
				& \multicolumn{2}{l|}{\imprimirpalavraschave}            \\
				&                                   & \imprimirclassificacaoassunto                         \\ \hline
			\end{tabular}
		\end{table}
	\end{catalografica}
	\clearpage
}
