\documentclass[a4paper,12pt]{article}
\usepackage[utf8]{inputenc}
\usepackage{graphicx}

\title{HQMpd -- Real-time mp3 decoder in Haskell}
\author{Anders Karlsson \\ \small{\texttt{andekar@@student.chalmers.se}} \and 
        Tobias Olausson \\ \small{\texttt{olaussot@@student.chalmers.se}}}

\begin{document}
\maketitle % Make a "real" title page
\includegraphics[width=\textwidth]{yeah.jpg}
\newpage

\tableofcontents
\newpage

\listoffigures
\newpage

\begin{abstract}
In this implementation we show that Haskell can indeed capture the needs
for both bit-level and high-performance operations, by implementing an mp3
decoder. To show off our decoder, we wrote a small mp3 player that uses
our implementation.
\end{abstract}

\section{Background}
    \subsection{Existing systems}
    \subsection{The mp3 format}
        \subsubsection{Huffman codes}
    \subsection{Sound support}
\section{Motivation}
    \subsection{Lazy vs Strict}
    \subsection{Readability}
\section{Implementation}
    \subsection{Huffman library}
    \subsection{Lazy BitGet}
    \subsection{The decoder}
\section{Results}
    \subsection{Testing}
    \subsection{Performance}
    \subsection{Correctness}
    \subsection{Beauty}
\section{Future work}
\section{Conclusion}
\section{References}

\end{document}
