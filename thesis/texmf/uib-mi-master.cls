\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{uib-mi-master}[2009/01/07 Master-oppg. for Matematisk institutt ved UiB] %fixme: date

% Languages:
\newif\ifuibmi@nynorsk        \uibmi@nynorsktrue        % Norwegian Nynorsk *is* the default.
\newif\ifuibmi@bokmal         \uibmi@bokmalfalse        % Norwegian Bokmål is not the default.
\newif\ifuibmi@UKenglish      \uibmi@UKenglishfalse     % British English is not the default.
\newif\ifuibmi@USenglish      \uibmi@USenglishfalse     % US English is not the default.

\newif\ifuibmi@norwlang       \uibmi@norwlangtrue       % Norwegian typography is the default.

% Fonts:
\newif\ifuibmi@mathpazo       \uibmi@mathpazotrue       % Mathpazo *is* the default font.
\newif\ifuibmi@osfmathpazo    \uibmi@osfmathpazofalse   % Mathpazo *is* the default font.
\newif\ifuibmi@tmmath         \uibmi@tmmathfalse        % TM-Math (Times) is not the default font.
\newif\ifuibmi@hvmath         \uibmi@hvmathfalse        % HV-Math (Helvetica Math) is not the default font.
\newif\ifuibmi@charter        \uibmi@charterfalse       % Bitstream Charter is not the default font.
\newif\ifuibmi@utopia         \uibmi@utopiafalse        % Adobe Utopia (Fourier) is not the default font.
\newif\ifuibmi@fourier        \uibmi@fourierfalse       % Adobe Utopia (Fourier) is not the default font.
\newif\ifuibmi@kpfonts        \uibmi@kpfontsfalse       % Kp-fonts is not the default font.

\newif\ifuibmi@gyre           \uibmi@gyrefalse          % TeX Gyre fonts (including Latin Modern) is not used by default.
\newif\ifuibmi@lmodern        \uibmi@lmodernfalse       % Latin Modern is not the default font.
\newif\ifuibmi@adventor       \uibmi@adventorfalse      % TeX Gyre Adventor is not the default font.
\newif\ifuibmi@bonum          \uibmi@bonumfalse         % TeX Gyre Bonum is not the default font.
\newif\ifuibmi@chorus         \uibmi@chorusfalse        % TeX Gyre Chorus is not the default font.
\newif\ifuibmi@cursor         \uibmi@cursorfalse        % TeX Gyre Cursor is not the default font.
\newif\ifuibmi@heros          \uibmi@herosfalse         % TeX Gyre Heros is not the default font.
\newif\ifuibmi@pagella        \uibmi@pagellafalse       % TeX Gyre Pagella is not the default font.
\newif\ifuibmi@schola         \uibmi@scholafalse        % TeX Gyre Schola is not the default font.
\newif\ifuibmi@termes         \uibmi@termesfalse        % TeX Gyre Termes is not the default font.

\newif\ifuibmi@nonhvmath      \uibmi@nonhvmathfalse     % Use commercial HV-Math for headings (if appropriate).
                                                        % If true, use Helvetica instead (does not support maths).

\newif\ifuibmi@uibnatural     \uibmi@uibnaturaltrue  % Use the natural colour model as default.
\newif\ifuibmi@uibcmyk        \uibmi@uibcmykfalse       % Do not use the RGB colour model as default.
\newif\ifuibmi@uibrgb         \uibmi@uibrgbfalse        % Do not use CMYK colour model as default.

\newif\ifuibmi@showexpl       \uibmi@showexplfalse      % Allow typesetting of LaTeX examples.
                                                        % fixme: Needed because something is wrong with showexpl.
\newif\ifuibmi@korrektur      \uibmi@korrekturfalse     % Double-spacing and other proof-reading stuff.

\newif\ifuibmi@alpha          \uibmi@alphafalse         % Use alpha-transparency (on the title page).
\newif\ifuibmi@papp           \uibmi@pappfalse          % Remove UiB column, and use white text on tile page.
                                                        % (Useful for cardboard title page.)

\newif\ifuibmi@utf           \uibmi@utffalse            % Do not use utf-8 as document character encoding.


\newif\ifuibmi@phd      \uibmi@phdfalse                 % PhD thesis is not the default.
\DeclareOption{phd}{\uibmi@phdtrue}

\newif\ifuibmi@article      \uibmi@articlefalse         % Article (scrartcl) class is not the default.
\DeclareOption{article}{\uibmi@articletrue}\ProcessOptions\relax


% More language settings:
\DeclareOption{nynorsk}{\uibmi@nynorsktrue\uibmi@bokmalfalse\uibmi@norwlangtrue}
\DeclareOption{norwegian}{\uibmi@bokmaltrue\uibmi@nynorskfalse\uibmi@norwlangtrue}
\DeclareOption{norsk}{\uibmi@bokmaltrue\uibmi@nynorskfalse\uibmi@norwlangtrue}
\DeclareOption{bokmal}{\uibmi@bokmaltrue\uibmi@nynorskfalse\uibmi@norwlangtrue}
\DeclareOption{english}{\uibmi@USenglishtrue\uibmi@nynorskfalse\uibmi@norwlangfalse}
\DeclareOption{USenglish}{\uibmi@USenglishtrue\uibmi@nynorskfalse\uibmi@norwlangfalse}
\DeclareOption{american}{\uibmi@USenglishtrue\uibmi@nynorskfalse\uibmi@norwlangfalse}
\DeclareOption{UKenglish}{\uibmi@UKenglishtrue\uibmi@nynorskfalse\uibmi@norwlangfalse}
\DeclareOption{british}{\uibmi@UKenglishtrue\uibmi@nynorskfalse\uibmi@norwlangfalse}

% Font settings
\DeclareOption{nonhvmath}{\uibmi@nonhvmathtrue}
\DeclareOption{mathpazo}{\uibmi@mathpazotrue\uibmi@lmodernfalse}
\DeclareOption{osfmathpazo}{\uibmi@osfmathpazotrue\uibmi@mathpazofalse}
\DeclareOption{tmmath}{\uibmi@tmmathtrue\uibmi@mathpazofalse}
\DeclareOption{hvmath}{\uibmi@hvmathtrue\uibmi@mathpazofalse\uibmi@nonhvmathfalse}
\DeclareOption{charter}{\uibmi@chartertrue\uibmi@mathpazofalse}
\DeclareOption{utopia}{\uibmi@utopiatrue\uibmi@fouriertrue\uibmi@mathpazofalse}
\DeclareOption{fourier}{\uibmi@utopiatrue\uibmi@fouriertrue\uibmi@mathpazofalse}
\DeclareOption{kpfonts}{\uibmi@kpfontstrue\uibmi@lmodernfalse\uibmi@mathpazofalse}
\DeclareOption{lmodern}{\uibmi@lmoderntrue\uibmi@mathpazofalse\uibmi@gyretrue}
\DeclareOption{adventor}{\uibmi@adventortrue\uibmi@mathpazofalse\uibmi@gyretrue}
\DeclareOption{bonum}{\uibmi@bonumtrue\uibmi@mathpazofalse\uibmi@gyretrue}
\DeclareOption{chorus}{\uibmi@chorustrue\uibmi@mathpazofalse\uibmi@gyretrue}
\DeclareOption{cursor}{\uibmi@cursortrue\uibmi@mathpazofalse\uibmi@gyretrue}
\DeclareOption{heros}{\uibmi@herostrue\uibmi@mathpazofalse\uibmi@gyretrue}
\DeclareOption{pagella}{\uibmi@pagellatrue\uibmi@mathpazofalse\uibmi@gyretrue}
\DeclareOption{schola}{\uibmi@scholatrue\uibmi@mathpazofalse\uibmi@gyretrue}
\DeclareOption{termes}{\uibmi@termestrue\uibmi@mathpazofalse\uibmi@gyretrue}

% Other stuff
\DeclareOption{uibnatural}{\uibmi@uibcmykfalse\uibmi@uibrgbfalse\uibmi@uibnaturaltrue}
\DeclareOption{uibcmyk}{\uibmi@uibcmyktrue\uibmi@uibrgbfalse\uibmi@uibnaturalfalse}
\DeclareOption{uibrgb}{\uibmi@uibcmykfalse\uibmi@uibrgbtrue\uibmi@uibnaturalfalse}
\DeclareOption{showexpl}{\uibmi@showexpltrue}
\DeclareOption{korrektur}{\uibmi@korrekturtrue}

\DeclareOption{alpha}{\uibmi@alphatrue}
\DeclareOption{papp}{\uibmi@papptrue} % For utskrift av forsida på tjukk papp.

\DeclareOption{utf}{\uibmi@utftrue} % For utskrift av forsida på tjukk papp.

% Final or draft
\newif\iffinal \finalfalse
\newif\ifdraft \draftfalse
\DeclareOption{final}{\finaltrue\draftfalse}
\DeclareOption{draft}{\finalfalse\drafttrue}

\ifuibmi@article
  \DeclareOption*{\PassOptionsToClass{\CurrentOption}{scrartcl}}
  \PassOptionsToClass{fontsize=11pt,BCOR12mm,DIV9,titlepage,numbers=noenddot,bibliography=totoc,version=last}{scrartcl}
  \PassOptionsToPackage{twoside}{typearea}
  \ProcessOptions\relax
  \LoadClass{scrartcl}
\else
  \DeclareOption*{\PassOptionsToClass{\CurrentOption}{scrreprt}}
  \PassOptionsToClass{fontsize=11pt,BCOR12mm,DIV9,titlepage,numbers=noenddot,bibliography=totoc,version=last}{scrreprt}
  \PassOptionsToPackage{twoside}{typearea}
  \ProcessOptions\relax
  \LoadClass{scrreprt}
\fi
\ifuibmi@phd
  \setlength{\paperwidth}{170mm}
  \setlength{\paperheight}{240mm}
\fi


% Conditional commands (depending on whether pdftex/PDF or tex/DVI is used).
\RequirePackage{ifpdf}

% Create smaller PDF files.
\ifpdf
  \iffinal
    \pdfcompresslevel=9
  \else
    \pdfcompresslevel=1 % Slightly faster?
  \fi
  \ifnum\pdftexversion>139
    \pdfminorversion=5
    \iffinal
      \pdfobjcompresslevel=3
    \fi
  \fi
\fi

% Make PDF files searchable, even if they use ligatures.
% Must come before inputenc/fontenc.
\ifpdf
\usepackage{cmap}
\fi

% Font and input encodings:
\ifuibmi@utf
  \RequirePackage[utf8]{inputenc}
\else
  \RequirePackage[latin1]{inputenc}
\fi

\RequirePackage[T1]{fontenc}

% Comment code.
\RequirePackage{comment}

% For greater leading (line-spacing). Defines the 'spacing' environment.
% Must be loaded before hyperref.
\RequirePackage{setspace}

% Newcommands with two optional arguments
\RequirePackage{twoopt}

% Prevent TeX eating spaces after newcommand substitutions.
\RequirePackage{xspace}

% Support the Text Companion fonts which provide many text symbols
% (such as baht, bullet, copyright, musicalnote, onequarter, section
% and yen) in the TS1 encoding.
\RequirePackage{textcomp}

% Fonts for body text and headings:
\ifuibmi@lmodern
  \RequirePackage{lmodern}
  \def\codefontsize{\@setfontsize\codefontsize{10.5}{13.5}}
\else\ifuibmi@adventor
  \RequirePackage{tgadventor}
  \def\codefontsize{\@setfontsize\codefontsize{10.5}{13.5}}
\else\ifuibmi@bonum
  \RequirePackage{tgbonum}
  \def\codefontsize{\@setfontsize\codefontsize{10.5}{13.5}}
\else\ifuibmi@chorus
  \RequirePackage{tgchorus}
  \def\codefontsize{\@setfontsize\codefontsize{10.5}{13.5}}
\else\ifuibmi@cursor
  \RequirePackage{tgcursor}
  \def\codefontsize{\@setfontsize\codefontsize{10.5}{13.5}}
\else\ifuibmi@heros
  \RequirePackage{tgheros}
  \def\codefontsize{\@setfontsize\codefontsize{10.5}{13.5}}
\else\ifuibmi@pagella
  \RequirePackage{tgpagella}
  \def\codefontsize{\@setfontsize\codefontsize{10.5}{13.5}}
\else\ifuibmi@schola
  \RequirePackage{tgschola}
  \def\codefontsize{\@setfontsize\codefontsize{10.5}{13.5}}
\else\ifuibmi@termes
  \RequirePackage{tgtermes}
  \def\codefontsize{\@setfontsize\codefontsize{10.5}{13.5}}
\else\def\codefontsize{\normalsize}
  \ifuibmi@mathpazo
  \RequirePackage[sc]{mathpazo}
  \ifuibmi@nonhvmath
    \RequirePackage[scaled=.95]{helvet}
  \else
    \RequirePackage[sans,scaled=.95]{hvmaths}
  \fi
  \RequirePackage[scaled=0.84]{beramono}
  \linespread{1.15} % Palatino has two small leading by default
                    % (and this also helps a lot with inline
                    % maths expressions).
  %\def\codefontsize{\@setfontsize\codefontsize{10}{11.5}}
\else\ifuibmi@osfmathpazo
  \RequirePackage[sc,osf]{mathpazo}
  \ifuibmi@nonhvmath
    \RequirePackage[scaled=.95]{helvet}
  \else
    \RequirePackage[sans,scaled=.95]{hvmaths}
  \fi
  \RequirePackage[scaled=0.84]{beramono}
  \linespread{1.15} % Palatino has two small leading by default
                    % (and this also helps a lot with inline
                    % maths expressions).
  %\def\codefontsize{\@setfontsize\codefontsize{10}{11.5}}
\else\ifuibmi@tmmath
  \RequirePackage{tmmaths}
  \ifuibmi@nonhvmath
    \RequirePackage[scaled=.95]{helvet}
  \else
    \RequirePackage[sans,scaled=.92]{hvmaths}
  \fi
  \RequirePackage[scaled=0.82]{beramono}
  %\def\codefontsize{\@setfontsize\codefontsize{9.5}{12}}
\else\ifuibmi@hvmath
  \RequirePackage{hvmaths}
  \RequirePackage[scaled=0.90]{beramono}
  %\def\codefontsize{\@setfontsize\codefontsize{10.8}{13.5}}
  \linespread{1.05}
\else\ifuibmi@charter
  \RequirePackage{charter}
  \RequirePackage[mdbch,ttscaled=true]{mathdesign} % Math font + scales Bera Mono automatically.
  \ifuibmi@nonhvmath
    \RequirePackage[scaled=.95]{helvet}
  \else
    \RequirePackage[sans]{hvmaths}
  \fi
  \RequirePackage{beramono}
  \linespread{1.05}
  \renewcommand*\@pnumwidth{1.70em} % Make room for page numbers > 99 in TOC.
  %\def\codefontsize{\@setfontsize\codefontsize{9.5}{12}}
\else\ifuibmi@kpfonts
  \RequirePackage{kpfonts}
  \linespread{1.05}
\else\ifuibmi@utopia
\let\my@@font@warning\@font@warning % Disable (unnecessary) warning.
\let\@font@warning\@font@info
  \RequirePackage[widespace]{fourier}
  \DeclareSymbolFont{upright}{T1}{fut\mathfamilyextension}{m}{n}
  \ifuibmi@nonhvmath
    \RequirePackage[scaled=.95]{helvet}
  \else
    \RequirePackage[sans]{hvmaths}
  \fi
  \RequirePackage[scaled=0.83]{beramono}
  \linespread{1.05}
  \renewcommand*\@pnumwidth{1.70em} % Make room for page numbers > 99 in TOC.
  %\def\codefontsize{\@setfontsize\codefontsize{9.5}{12}}
\let\@font@warning\my@@font@warning % Enable warnings again.
\fi\fi\fi\fi\fi\fi\fi\fi%
\fi\fi\fi\fi\fi\fi\fi\fi

% Embed each Helvetica and Symbol font only once, and replace
% (subsetted and non-subsetted) Helvetica in external images
% with HV-Math (if 'nonhvmath' is not used):
\ifpdf
\ifuibmi@nonhvmath
  \pdfmapline{=phvr8r Helvetica <8r.enc <uhvr8a.pfb}
  \pdfmapline{=phvb8r Helvetica-Bold <8r.enc <uhvb8a.pfb}
  \pdfmapline{=phvro8r Helvetica-Oblique <8r.enc <uhvro8a.pfb}
  \pdfmapline{=phvbo8r Helvetica-BoldOblique <8r.enc <uhvbo8a.pfb}
\else
  \pdfmapline{=hvrm108r Helvetica <8r.enc <mphv.pfb}
  \pdfmapline{=hvrb108r Helvetica-Bold <8r.enc <mphvb.pfb}
  \pdfmapline{=hvro108r Helvetica-Oblique <8r.enc <mphvo.pfb}
  \pdfmapline{=hvbo108r Helvetica-BoldOblique <8r.enc <mphvbo.pfb}
\fi
  \pdfmapline{=psyr Symbol <usyr.pfb}
\fi

\ifuibmi@phd\else
\ifpdf
\ifnum\pdftexversion>139
    \RequirePackage{embedfile} % Embed source files.
    \ifuibmi@nynorsk
    \embedfile[desc={Hovuddokument (LaTeX)},mimetype=text/plain,stringmethod=escape]{\jobname.tex} % Embed main document.
    \else
    \ifuibmi@bokmal
    \embedfile[desc={Hoveddokument (LaTeX)},mimetype=text/plain,stringmethod=escape]{\jobname.tex} % Embed main document.
    \else
    \embedfile[desc={Main document (LaTeX)},mimetype=text/plain,stringmethod=escape]{\jobname.tex} % Embed main document.
    \fi
    \fi
    \let\uibmi@OrigInclude\include % Embed included files.
     \def\include#1{%
     \ifuibmi@norwlang
      \embedfile[desc={Inkludert fil (LaTeX)},mimetype=text/plain,stringmethod=escape]{#1.tex}%
      \else
      \embedfile[desc={Included file (LaTeX)},mimetype=text/plain,stringmethod=escape]{#1.tex}%
      \fi
    \uibmi@OrigInclude{#1}%
    }
    \let\uibmi@OrigBib\bibliography % Embed bibliography.
     \def\bibliography#1{%
     \ifuibmi@norwlang
     \embedfile[desc={Litteraturliste (BibTeX)},mimetype=text/plain,stringmethod=escape]{#1.bib}
     \else
     \embedfile[desc={Bibliography (BibTeX)},mimetype=text/plain,stringmethod=escape]{#1.bib}
     \fi
     \uibmi@OrigBib{#1}%
    }
\fi
\fi
\fi

% For proof-reading:
\ifuibmi@korrektur
  \linespread{1.9}
\fi

% Complain about bad and outdated packages and code.
\RequirePackage[l2tabu,orthodox]{nag}

% Do not flag '\label in float, but not after \caption' when using subfigures.
\g@addto@macro{\nag@captions}{,subfloat}

% Only allow AMS-based math environments.
\RequirePackage[all]{onlyamsmath}

% Fix stuff
\RequirePackage{fixltx2e}

% Load Babel with the appropriate options.
% See also http://groups.google.com/group/no.it.programvare.tex/browse_thread/thread/1d6dcb6a21fc75eb/44f6ed7dd762683b
% for an explanation of why we need the \AtBeginDocument option.
\ifuibmi@nynorsk        \RequirePackage[USenglish,UKenglish,norsk,nynorsk]{babel}
                    \AtBeginDocument{\selectlanguage{nynorsk}}
\else\ifuibmi@bokmal    \RequirePackage[USenglish,UKenglish,nynorsk,norsk]{babel}
                    \AtBeginDocument{\selectlanguage{norsk}}
\else\ifuibmi@UKenglish \RequirePackage[norsk,nynorsk,USenglish,UKenglish]{babel}
                    \AtBeginDocument{\selectlanguage{UKenglish}}
\else\ifuibmi@USenglish \RequirePackage[norsk,nynorsk,UKenglish,USenglish]{babel}
                    \AtBeginDocument{\selectlanguage{USenglish}}
\fi\fi\fi\fi

% Turn "" into real quotes automatically, in case people use them inadvertently.
\usepackage[babel]{csquotes}
\MakeOuterQuote{"}
\AtBeginDocument{\EnableQuotes} % Needed in the latest version.

% Norwegian uses an en-dash as a list marker:
\ifuibmi@norwlang
  \renewcommand{\labelitemi}{\normalfont\textendash}
  \renewcommand{\labelitemii}{\normalfont\textperiodcentered}
\fi

% Support compact lists, and do not add trailing dot in Norwegian lists:
\ifuibmi@norwlang  \RequirePackage[pointlessenum]{paralist}
\else              \RequirePackage{paralist}
\fi
\RequirePackage{mdwlist}

% Allow comma as decimal separator in maths if language is Norwegian:
\ifuibmi@norwlang
  \RequirePackage{ncccomma}
\fi

% High-quality tables:
\RequirePackage{booktabs}
\RequirePackage{array} % Defines \@arrayrule, among other things.
\ifuibmi@showexpl\else % The documentation uses ugly tables rules to show why they must not be used.
  \renewcommand{\hline}{} % Disallow non-booktabs-style horizontal rules.
  \renewcommand*{\@arrayrule}{} % Disallow vertical rules in tables.
\fi

% For aligning numbers at decimal separator.
\RequirePackage{dcolumn}
\newcolumntype{d}{D{,}{\mathcomma}{-1}}

% Fancy cross-references. Must be defined before 'hyperref'.
\ifuibmi@nynorsk        \RequirePackage[nynorsk]{varioref}
\else\ifuibmi@bokmal    \RequirePackage[norsk]{varioref}
\else                \RequirePackage[english]{varioref}
\fi\fi

% Treat varioref errors as warnings unless the 'final' option is used.
\iffinal
  \vrefshowerrors
\else
  \vrefwarning
\fi

\RequirePackage[noconfig]{refstyle} % Much better than fancyref.
\input{refstyle-uib-mi-master.cfg}
\AtBeginDocument{\renewcommand*{\RSeqrefform}[1]{\textup{\ref{#1}}}} % Remove parenthesis.
                                                                     % I have no idea why the option
                                                                     % in the config file is not used ...

% Name the bibliography 'Litteratur' in Bokmål too.
\addto\captionsnorsk{%
  \renewcommand\bibname{Litteratur}
} 

% We use our own bibliography style by default. Must define natbib before hyperref.
\RequirePackage{natbib}
\ifuibmi@nynorsk        \bibliographystyle{uib-mi-nn}
\else\ifuibmi@bokmal    \bibliographystyle{uib-mi-nb}
\else\ifuibmi@UKenglish \bibliographystyle{uib-mi-en-gb}
\else\ifuibmi@USenglish \bibliographystyle{uib-mi-en-us}
\fi\fi\fi\fi
\bibpunct{(}{)}{;}{a}{}{,}
\def\urlprefix{URL:~\codefontsize}


% Insert author and document info in PDF
% version, and choose PDF viewing options:
\ifpdf
  \AtBeginDocument{%
  \ifuibmi@norwlang
    \let\oldand\and\def\and{og }
  \else
    \let\oldand\and\def\and{and }
  \fi% Handter \and i forfattarliste (iallfall med to)
  \hypersetup{unicode, a4paper, pdftitle=\@title, pdfauthor=\@author,pdfpagemode=UseOutlines, bookmarksopen=true, pdfpagelayout=OneColumn, pdfview=FitH, pdfstartview=Fit}%
  \let\and\oldand}
\fi

% Support for colours (do *not* use the package 'color').
% And support for images, automatic hyperlinks and fix colours in PDF:
\ifpdf
  \RequirePackage[hyperpageref]{backref}
  \RequirePackage[svgnames,dvipsnames,x11names,fixpdftex,\ifuibmi@uibcmyk cmyk\else\ifuibmi@uibrgb rgb\fi\fi,xcdraw,hyperref]{xcolor}
  \RequirePackage[pdftex]{graphicx}
  \RequirePackage[unicode,pdftex,linkcolor=AltColDark,linkbordercolor={0 .8 0},%
                    citecolor=AltColDark,citebordercolor={0 .8 0},%
                    urlcolor=PrimColDark,urlbordercolor={.8 0 0},%
                    filecolor=PrimColDark,filebordercolor={.8 0 0}%
                    \iffinal,pdfborderstyle={/S/U/W 1}%
                    \else,colorlinks=true\fi,%
    pdfescapeform=true,pagebackref]{hyperref} % fixme: rgb-fargeproblem.
  % hypertexnames=true causes error message when using arabic numerals.
  %\usepackage{pdfcolmk}
\else % fixme: legg til pagebackref for både DVI og PDF?
   \RequirePackage[svgnames,dvipsnames,x11names,rgb,xcdraw,hyperref]{xcolor}
   \RequirePackage[dvips]{graphicx}
   \RequirePackage[unicode,dvips,linkcolor=AltColDark,linkbordercolor={0 .8 0},%
                    citecolor=AltColDark,citebordercolor={0 .8 0},%
                    urlcolor=PrimColDark,urlbordercolor={.8 0 0},%
                    filecolor=PrimColDark,filebordercolor={.8 0 0},%
                    pagebackref\iffinal\else,colorlinks=true\fi]{hyperref} % fixme: pagebackref?
   \pagecolor{yellow!80!red!15} % Fixme: Does not hurt your eyes as much as white (only used for screen version!).
\fi


% Provide nice back references in bibliography:
\ifuibmi@norwlang%
    \renewcommand*{\backreftwosep}{ og~}%
    \renewcommand*{\backreflastsep}{ og~}%
\else
  \ifuibmi@UKenglish % We need to do it this way, since English is 'supported' in 'backref'.
\def\backrefenglish{%
  \def\backreflastsep{ and~}%
}
  \else % = USenglish
\def\backrefenglish{%
  \def\backreflastsep{, and~}%
}
  \fi
\fi
\renewcommand*{\backref}[1]{}
\renewcommand*{\backrefalt}[4]{%
\ifcase #1%
\or%
  \ifuibmi@norwlang
    Referert til på side~#2.%
  \else
    Cited on page~#2.%
  \fi
\else
  \ifuibmi@norwlang%
    Referert til på side~#2.%
  \else
    Cited on pages~#2.%
  \fi
\fi
}


% Load 'doi' package to create doi hyperlinks (that 
% handle nasty special characters in a sensible way).
% Must be loaded after hyperref stuff.
\usepackage{doi}
\renewcommand{\doitext}{DOI:~}

% For customised headings and footers:
\RequirePackage{scrpage2}

% Fixme notes and draft text:
\iffinal
  \RequirePackage[final]{fixme}
  \else
    \RequirePackage[draft]{fixme}
     \RequirePackage{clock}
     \cfoot[\upshape{\today~-- \texttime}]{\upshape{\today~-- \texttime}}
     \pagestyle{scrplain}
     \setlength\overfullrule{5pt}
\fi

% Norwegian FixMe label:
\ifuibmi@norwlang
  \renewcommand*\fixmelogo{\textsf{Rett opp}}
\fi
% Norwegian 'List of Corrections' heading:
\ifuibmi@nynorsk
  \renewcommand*{\listfixmename}{Ting eg m\aa{} retta opp f\o r eg leverer}
\else\ifuibmi@bokmal
  \renewcommand*{\listfixmename}{Ting jeg m\aa{} rette opp f\o r jeg leverer}
\fi\fi

% Float figures and tables (as default) as 'htbp' instead of default 'tbp':
\def\fps@figure{htbp}
\def\fps@table{htbp}

% Use not 'conservative' float parameters, to encourage TeX not
% to move figures and tables away. See http://www.tex.ac.uk/cgi-bin/texfaq2html?label=floats
\renewcommand{\topfraction}{.85}
\renewcommand{\bottomfraction}{.7}
\renewcommand{\textfraction}{.15}
\renewcommand{\floatpagefraction}{.66}
\renewcommand{\dbltopfraction}{.66}
\renewcommand{\dblfloatpagefraction}{.66}
\setcounter{topnumber}{9}
\setcounter{bottomnumber}{9}
\setcounter{totalnumber}{20}
\setcounter{dbltopnumber}{9}

% Support greek letters and maths in headings and title:
% ***** Start maths suport in headings *****
\ifuibmi@nonhvmath\else\ifuibmi@hvmath\else\ifuibmi@gyre\else\ifuibmi@kpfonts\else % The hvmaths package and others already defines these, and more.

\let\my@@font@warning\@font@warning % Disable (unnecessary) warning.
\let\@font@warning\@font@info

\DeclareMathVersion{sans}
\SetSymbolFont{operators}{sans}{OT1}{hvr}{bx}{n}%
\SetSymbolFont{letters}{sans}{OML}{hvm}{b}{it}%
\SetSymbolFont{symbols}{sans}{OMS}{hvsy}{b}{n}%
\SetMathAlphabet\mathbf{sans}{OT1}{hvr}{bx}{n}%
\SetMathAlphabet\mathit{sans}{OT1}{hvr}{bx}{it}%
\DeclareFontFamily{U}{hvrmex}{}
\DeclareFontShape{U}{hvrmex}{b}{n}{<->hvrb10ex}{}
\ifuibmi@charter\else
\SetSymbolFont{upright}{sans}{U}{hvrmex}{b}{n}
\fi

\let\@font@warning\my@@font@warning % Enable warnings again.
\fi\fi\fi\fi
% ***** Stop maths suport in headings *****

% Support greater number of math symbols, new and improved dots, and more:
% Must come after maths support in headings.
% The mathdesign package already supplies the needed glyphs:
\ifuibmi@charter\else\ifuibmi@utopia\else % 
  \RequirePackage{amssymb}
  \RequirePackage{amsfonts}
\fi\fi
\RequirePackage{amsmath}
\RequirePackage{amsbsy}
\RequirePackage{mathdots}  % More maths dots. Must be loaded *after* amsmath.
\RequirePackage{mathtools} % Fix math stuff.
\mathtoolsset{mathic}      % Ditto.
\RequirePackage[noamssymb]{dlfltxbcodetips} % For \PullBack and other useful commands.

% Simulate maths code (from Lars Madsen).
\newcommand\@dbx[1]{{\fboxsep=0pt\fboxrule=0.4pt\,\fbox{\phantom{\rule[-0.6mm]{#1}{3mm}}}\,}}
\newcommand\dbx[1][5mm]{\ifmmode\mathord{\@dbx{#1}}\else\@dbx{#1}\fi}

% Sometimes \varnothing is not defined. Then define it as an alias of \emptyset.
% This must come after all font and ams stuff.
% http://groups.google.com/group/comp.text.tex/browse_thread/thread/89bf3dcbd4dc74fd/6612b6281b874bca
\newcommand*{\OLDvarnothing}{}
\let\OLDvarnothing\varnothing
\@ifundefined{varnothing}{\newcommand*{\varnothing}{\emptyset}}{}

% Allow ragged-right margins *with* hyphenation.
\RequirePackage{ragged2e}

% Captions and subcaptions:
\RequirePackage[justification=justified,format=hang]{caption,subfig}[2004/07/16]

% Support forward search in DVI viewers:
\RequirePackage[active]{srcltx}


% Primary and alternative colour:
%\colorlet{PrimCol}{Crimson}
% Official UiB colours. See
% http://www.uib.no/form/protorg/01elementer/01d_farge.htm
\ifpdf
  \definecolor{UiBBlue}{cmyk}{1.0,0,0.12,0.43}
  \definecolor{UiBBlack}{cmyk}{0,0,0,1.0}
  \definecolor{UiBGreen}{cmyk}{0.5,0,1,0}
  \definecolor{UiBOrange}{cmyk}{0,0.61,0.97,0}
  \definecolor{UiBRed}{cmyk}{0,0.91,1.0,0.23}
\else
  \definecolor{UiBBlue}{HTML}{005473}
  \definecolor{UiBBlack}{HTML}{000000}
  \definecolor{UiBGreen}{HTML}{77AF00}
  \definecolor{UiBOrange}{HTML}{D95900}
  \definecolor{UiBRed}{HTML}{AA0000}
\fi

\definecolor{CMYKLight}{cmyk}{0,0,0,0}
\definecolor{CMYKDark}{cmyk}{1,1,1,0}
\definecolor{RGBLight}{HTML}{FFFFFF}
\definecolor{RGBDark}{HTML}{000000}

\colorlet{PrimCol}{UiBRed}
\colorlet{AltCol}{UiBGreen}

% Colour for theorem bars:
\AtBeginDocument{
\ifpdf
  \colorlet{PrimColDark}{PrimCol!60!CMYKDark}
  \colorlet{PrimColLight}{PrimCol!10!CMYKLight}
  \colorlet{AltColDark}{AltCol!60!CMYKDark}
  \colorlet{AltColLight}{AltCol!10!CMYKLight}
\else
  \colorlet{PrimColDark}{PrimCol!60!RGBDark}
  \colorlet{PrimColLight}{PrimCol!10!RGBLight}
  \colorlet{AltColDark}{AltCol!60!RGBDark}
  \colorlet{AltColLight}{AltCol!10!RGBLight}
\fi
\colorlet{thmcol}{PrimColLight}
}

\ifuibmi@article % Do not define chapter commands when using scrarticle.
\else
% This package is for chapter quotations, but we will
% use it for making nice-looking fancy chapter headings:
\RequirePackage{quotchap}

% We redefine one of the quotchap commands to give italics
% and not slanted quotations:
\renewenvironment{savequote}[1][10cm]{%
  \begin{lrbox}{\@quotebox}
    \begin{minipage}[t]{#1}\footnotesize\itshape
      \ignorespaces}{%
      \unskip\end{minipage}\end{lrbox}
  \global\setbox\@quotebox\copy\@quotebox
  \global\let\@printcites\@iprintcites
  \ignorespacesafterend}

\newif\ifuibmi@inappendix \uibmi@inappendixfalse % Some trickery to give us a way to discover if we are in an appendix.
\let\uibmi@oldappendix\appendix
\renewcommand*{\appendix}{\uibmi@oldappendix\uibmi@inappendixtrue}

% Use HVMaths (HVR) as the chapter heading font (or Helvetica (PHV) if it is not available):
\ifuibmi@nonhvmath
  \renewcommand*{\@defaultcnfont}{phv}
\else
  \renewcommand*{\@defaultcnfont}{hvr}
\fi

% Using lmodern sans as the chapter heading font if using lmodern:
\ifuibmi@lmodern
\renewcommand*{\chapnumfont}{%
    \usefont{T1}{lmss}{bx}{n}\fontsize{100}{130}\selectfont%
    \color{chaptergrey}}
\fi

% Colour the chapter headings. We need to do it this way to support custom colours.
\AtBeginDocument{\colorlet{chaptergrey}{AltCol}}

\fi % End !uibmi@article stuff.

% The following commands should really only be executed if \uibmi@article is false,
% but this gives an error message. Since it does no harm, we always execute them.

\providecommand*{\@makechapterhead}{} % Just to make sure the command exists.
% We also redefine one of the quotchap commands to improve
% the kerning on two-digit numbers containing 1:
\renewcommand{\@makechapterhead}[1]{\chapterheadstartvskip%
  {\size@chapter{\sectfont\raggedleft
      {\chapnumfont
        \ifnum \c@secnumdepth >\m@ne%
\if@mainmatter%
\ifuibmi@inappendix\thechapter\else% We are in an appendix ==> just write the letter (A, B, C, etc.).
  \ifnum\value{chapter}=1{1}\kern-11pt\else% Chapter=1 ==> move the 1 a bit to the right.
  \ifnum\value{chapter}=21{2}\kern-4pt{1}\kern-11pt\else% Chapter=21 ==> change the kerning between 2 and 1.
  \ifnum\value{chapter}>9\ifnum\value{chapter}<20% Chapter in [10,19] ==> Change the kerning after 1.
  \ifnum\value{chapter}=11{1}\kern-14pt{1}\kern-11pt\else% Special treatment for chapter 11.
  \count@=\value{chapter}\advance\count@-10 % Chapters 10, 12, 13, ..., 19.
1\kern-10pt\number\count@\fi\else\thechapter\fi\else\thechapter\fi\fi\fi\fi
        \fi\fi
        \par\nobreak}%
      {\raggedleft\advance\leftmargin10em\interlinepenalty\@M #1\par}}
    \nobreak\chapterheadendvskip}}

% Nice heading colours:
\ifuibmi@hvmath
  \newcommand{\uibmi@mv}{\relax}
\else\ifuibmi@nonhvmath
  \newcommand{\uibmi@mv}{\relax}
\else\ifuibmi@gyre
  \newcommand{\uibmi@mv}{\relax}
\else\ifuibmi@kpfonts
  \newcommand{\uibmi@mv}{\relax}
\else
  \newcommand{\uibmi@mv}{\mathversion{sans}}
\fi\fi\fi\fi
\addtokomafont{sectioning}{\color{PrimCol}\boldmath\uibmi@mv}

\ifuibmi@norwlang
\ifuibmi@nynorsk
  \DeclareRobustCommand{\@thesisdesc}{Oppg\aa ve for graden master i statistikk}
  \DeclareRobustCommand{\@country}{Noreg}
\else
  \DeclareRobustCommand{\@thesisdesc}{Oppgave for graden master i statistikk}
  \DeclareRobustCommand{\@country}{Norge}
\fi
\DeclareRobustCommand{\@major}{Matematisk statistikk}
\DeclareRobustCommand{\@department}{Matematisk institutt}
\DeclareRobustCommand{\@university}{Universitetet i Bergen}
\else
\DeclareRobustCommand{\@thesisdesc}{Thesis for the degree of Master of Statistics}
\DeclareRobustCommand{\@major}{Mathematical statistics}
\DeclareRobustCommand{\@department}{Department of Mathematics}
\DeclareRobustCommand{\@university}{University of Bergen}
\DeclareRobustCommand{\@country}{Norway}
\fi

\DeclareRobustCommand{\thesisdesc}[1]{%
  \DeclareRobustCommand{\@thesisdesc}{#1}%
}

\newcommand{\thesistype}[1]{} % This is not used anymore.

\DeclareRobustCommand{\title}[1]{%
\iffinal
  \DeclareRobustCommand{\@title}{#1}%
\else
  \ifuibmi@norwlang
    \DeclareRobustCommand{\@title}{#1 [UTKAST]}%
  \else
    \DeclareRobustCommand{\@title}{#1 [DRAFT]}%
\fi\fi
}

\DeclareRobustCommand{\major}[1]{%
  \DeclareRobustCommand{\@major}{#1}%
}

\DeclareRobustCommand{\department}[1]{%
  \DeclareRobustCommand{\@department}{#1}%
}

\DeclareRobustCommand{\university}[1]{%
  \DeclareRobustCommand{\@university}{#1}%
}

\DeclareRobustCommand{\country}[1]{%
  \DeclareRobustCommand{\@country}{#1}%
}

% For spacing out uppercase text.
\usepackage{soul}


\RequirePackage{naturalparbox}
% User-configurable: Which resolution should be used when searching for best width?
% User-configurable: Which badness should be tolerated as perfect (stopping the search for a better one).
\iffinal % Slow but good.
  \renewcommand{\optwidthsteps}{2000} % Use high resolution. We want the best solution (default: 50).
  \renewcommand{\optwidthlinetolerance}{0} % Do not accept much badness (default: 200).
\else % Fast and OK for previewing.
  \renewcommand{\optwidthsteps}{20} % Use high resolution. We want the best solution (default: 50).
  \renewcommand{\optwidthlinetolerance}{200} % Accept a little badness (default: 200).
\fi

% For absolute positioning (for making the title page).
\usepackage[absolute,overlay]{textpos}

\let\makeoldtitle\maketitle
\newcommand{\makephdtitle}{%
\begin{titlepage}

\null % To avoid the text on the next page overlapping this one (since this is formally empty).

\begin{textblock}{13}[1,1](13,6) % 13 is 13/16 of the paperwidth.
\hfill\begin{minipage}{.65\paperwidth}\raggedleft\Huge\@title% .65\paperwidth gives us some margin on the left.
\strut\end{minipage}\vspace{0pt} % To ensure proper spacing. \strut for ascender/descender issues, and \vspace to fix a bug in textpos with multiline titles when showboxes is false.
\end{textblock}

\begin{textblock}{16}[0,0](0,6) % Author box.
\ifuibmi@alpha%
\tikz[baseline=(author.base),outer sep=0pt, inner sep=0pt]\node[fill=PrimCol,fill opacity=0.4,text opacity=1](author){\parbox{.8125\paperwidth}{\vspace*{3pt}\raggedleft\LARGE\textit{\ifuibmi@papp\color{white}\fi\@author}\strut\hspace{1ex}\vspace*{1pt}}}; % fixme: Use real transparency. Disable by default for now, as kpdf does not handle printing this properly.
\else%
\tikz[baseline=(author.base),outer sep=0pt, inner sep=0pt]\node[fill=PrimCol!30](author){\parbox{.8125\paperwidth}{\vspace*{3pt}\raggedleft\LARGE\textit{\ifuibmi@papp\color{white}\fi\@author}\strut\hspace{1ex}\vspace*{1pt}}};
\fi
\end{textblock}

\begin{textblock}{3}[0,0](13.1,6) % Coloured box to the right of author box.
\tikz[baseline=(cbox.base),outer sep=0pt,inner sep=0pt]\node[fill=PrimCol](cbox){\parbox{2.7em}{\vspace*{3pt}\LARGE\strut\vspace*{1pt}}};
\end{textblock}

\begin{textblock}{11}[1,0](13,6) % Thesis description.
\raggedleft\LARGE\vspace{16pt}\strut\\ % Since the position is the same as for the author box, skip one line.
\Large\itshape%
\@thesisdesc\\
\@major\\
\@university, \@country\\
\@date
\end{textblock}

\begin{textblock}{10}[1,1](15,15.5) % Draw the logo at the bottom-right of the page.
 \raggedleft\includegraphics[scale=.7]{uib-logo-mi-h}
\end{textblock}

\ifuibmi@papp\else
 \begin{textblock}{16}[0,0](0,2.3) % Draw the UiB column.
 \includegraphics[scale=1.4]{uib-soyle}
 \end{textblock}
\fi

\end{titlepage}
}
\newcommand{\makenewtitle}{%
\begin{titlepage}
\begingroup
\null % To avoid the text on the next page overlapping this one (since this is formally empty).
\setlength{\parfillskip}{\z@ \@plus 1fil} % To avoid overfull boxes when using halfparskip.
\setlength{\parskip}{\z@ \@plus .3\p@}    % Ditto.

\begin{textblock}{10.4}[0,0](1.7,10.5)
\sffamily
\fontsize{40}{43}\selectfont
\raggedright\textcolor{white}{\@title}
\end{textblock}

\begin{textblock}{16}[1,1](15,15.293) % Draw the logo at the bottom-right of the page.
 \raggedleft\includegraphics{uib-emblem-kvit}
\end{textblock}

\begin{textblock}{14}[0,0](1,10)%
\noindent\begin{tikzpicture}
\node[fill=PrimCol,outer sep=0pt,inner sep=0pt] (bcbox)%
{
\begin{minipage}{\textwidth}{%
\hfill\vspace*{0.33\paperheight}%
}
\end{minipage}
};
\end{tikzpicture}
\end{textblock}


\begin{textblock}{11}[1,1](13,7.5) % Author box.
\fontsize{40}{43}\selectfont
\raggedleft
\@author\strut
\end{textblock}


\begin{textblock}{10}[1,0](14,7.8) % Thesis description.
\raggedleft\Large\itshape%
\@thesisdesc\\
\@major\\
\@university, \@country\\
\@date
\end{textblock}
\endgroup
\end{titlepage}
}
\ifuibmi@phd
\let\maketitle\makephdtitle
\else
\let\maketitle\makenewtitle
\fi


\newcommand{\makefront}{%
\pagenumbering{roman}
\maketitle
\clearpage
\thispagestyle{empty}
\enlargethispage{4\baselineskip}
\vspace*{\fill}
\hfill
\ifdraft\else\ifuibmi@article\else
\naturalparbox[.85\textwidth]{\small\begin{spacing}{1.05}
\ifuibmi@nynorsk
Denne oppg\aa va er skriven i \LaTeXe{} med dokument\-klassen \guillemotleft uib-mi-master\guillemotright, laga av Karl Ove Hufthammer. Ho vart kompilert med \@pdfproducer{} den \today. Br\o d\-teksten er sett i 11~punkts %
\else\ifuibmi@bokmal
Denne oppgaven er skrevet i \LaTeXe{} med dokument\-klassen \guillemotleft uib-mi-master\guillemotright, laget av Karl Ove Hufthammer. Den ble kompilert med \@pdfproducer{} den \today. Br\o d\-teksten er satt i 11~punkts %
\else
This thesis is written in \LaTeXe{} with the \enquote{uib-mi-master} document class, developed by Karl Ove Hufthammer. It was compiled using \@pdfproducer{} on \today. The body text is 11~point %
\fi\fi%
\ifuibmi@mathpazo%
URW Palladio \ifuibmi@nynorsk med kapit\'{e}lar\else\ifuibmi@bokmal med kapit\'{e}ler\else with small caps\fi\fi%
\else\ifuibmi@osfmathpazo%
URW Palladio \ifuibmi@nynorsk med kapit\'{e}lar og renessanse\-tal\else\ifuibmi@bokmal med kapit\'{e}ler og renessanse\-tall\else with small caps and old-style figures\fi\fi% End language.
\else\ifuibmi@lmodern%
Latin Modern.%
\else\ifuibmi@adventor%
\TeX{} Gyre Adventor.%
\else\ifuibmi@bonum%
\TeX{} Gyre Bonum.%
\else\ifuibmi@chorus%
\TeX{} Gyre Chorus.%
\else\ifuibmi@cursor%
\TeX{} Gyre Cursor.%
\else\ifuibmi@heros%
\TeX{} Gyre Heros.%
\else\ifuibmi@pagella%
\TeX{} Gyre Pagella.%
\else\ifuibmi@schola%
\TeX{} Gyre Schola.%
\else\ifuibmi@termes%
\TeX{} Gyre Termes.%
\else\ifuibmi@tmmath%
TM~Math%
\else\ifuibmi@hvmath%
HV~Math%
\else\ifuibmi@charter%
Bitstream Charter%
\else\ifuibmi@utopia%
Adobe Utopia%
\else\ifuibmi@kpfonts%
Kp-Fonts.%
\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi% End body font.
\ifuibmi@gyre\else%
\ifuibmi@kpfonts\else
\ifuibmi@hvmath
\ifuibmi@nynorsk, og programkode er sett i \else\ifuibmi@bokmal, og programkode er satt i \else, and the computer code is written in \fi\fi Bera Mono.% End hvmath
\else\ifuibmi@tmmath%
. \ifuibmi@norwlang Overskrifter er sett i \else The heading font is \fi \ifuibmi@nonhvmath URW Helvetica\else HV~Math\fi \ifuibmi@norwlang\xspace og programkode i \else, and the computer code font is \fi Bera Mono.%
\else. \ifuibmi@nynorsk Matematikken er sett i \else\ifuibmi@bokmal Matematikken er satt i \else The math\ifuibmi@UKenglish s\fi\xspace font is \fi\fi\ifuibmi@mathpazo URW Palladio \ifuibmi@norwlang og \else and \fi Pazo Math\else\ifuibmi@osfmathpazo URW Palladio \ifuibmi@norwlang og \else and \fi Pazo Math\else\ifuibmi@charter Math Design Charter\else\ifuibmi@utopia Math Design Utopia\fi\fi\fi\fi, \ifuibmi@norwlang overskrifter i \else the heading font is \fi \ifuibmi@nonhvmath URW Helvetica\else HV~Math\fi \ifuibmi@norwlang\xspace og programkode i \else, and the computer code font is \fi Bera Mono.\fi\fi\fi\fi{\parfillskip=0pt \par}\end{spacing}
}
\fi\fi
\clearpage


% \setcounter{page}{1}
\ifuibmi@norwlang
  \chapter*{Takk}
\else
  \ifuibmi@UKenglish
    \chapter*{Acknowledgements}
  \else
    \chapter*{Acknowledgments}
  \fi
\fi
} % End \makefront.

\newcommand{\makemain}{%
\listoffixmes
\cleardoublepage
\pagenumbering{arabic}
} % End \makefront.


% Since we use two-sided printing and much
% mathematics, we will need to loosen the
% default page-filling requirements of LaTeX:
\raggedbottom
\addtolength{\topskip}{0pt plus 40pt}
\widowpenalty=10000
\clubpenalty=10000


% For theorems:
\RequirePackage{thmbox-huftis}
\thmboxoptions{
               bodystyle=\upshape\noindent,
               headstyle=\sffamily\bfseries\boldmath\uibmi@mv#1 #2,
	       titlestyle=:~#1,
	       underline=false,
	       style=S,
               leftmargin=1.7em,
	       rightmargin=0pt,
               hskip=.6em,
	       thickness=1em
	       }

\ifuibmi@norwlang
  \newtheorem{theorem}{Teorem}[section]
  \newtheorem{definition}[theorem]{Definisjon}
  \newtheorem{lemma}[theorem]{Lemma}
  \newtheorem{example}[theorem]{Eksempel}
  \newtheorem{corollary}[theorem]{Korollar}
\else
  \newtheorem{theorem}{Theorem}[section]
  \newtheorem{definition}[theorem]{Definition}
  \newtheorem{lemma}[theorem]{Lemma}
  \newtheorem{example}[theorem]{Example}
  \newtheorem{corollary}[theorem]{Corollary}
\fi


%\RequirePackage{enumitem}

% Matrices and transpose symbol:
\newcommand{\matr}[1]{\boldsymbol{#1}}
\newcommand{\tr}{^{\mathrm{T}}} % Transponert

\RequirePackage{upgreek}

% Imaginary numbers
\DeclareRobustCommand{\I}[0]{\mathbb{I}}

% Real line
\DeclareRobustCommand{\R}[0]{\mathbb{R}}

% Extended real line (normal \overline is too long, and \bar is too short)
\newcommand{\RE}[0]{\mathbb{\overline{R\mkern-1.5mu}\mkern1.5mu}}
\ifuibmi@charter
  \renewcommand{\RE}[0]{\mathbb{\overline{R\mkern-2mu}\mkern2mu}}
\else\ifuibmi@hvmath
  \renewcommand{\RE}[0]{\mathbb{\overline{R}}}
\fi\fi

% Absolute value and norm
\DeclarePairedDelimiter\abs{\lvert}{\rvert}
\DeclarePairedDelimiter\norm{\lVert}{\rVert}

% Various mathematics and probability operators:

\DeclareMathOperator{\Prob}{\mathbb{P}} % Probability
\DeclareMathOperator{\SD}{SD}           % Standard deviance
\DeclareMathOperator{\Var}{Var}         % Variance
\DeclareMathOperator{\Cov}{Cov}         % Covariance
\DeclareMathOperator{\Kov}{Kov}         % Covariance
\DeclareMathOperator{\corr}{corr}       % Correlation
\DeclareMathOperator{\korr}{korr}       % Correlation
\DeclareMathOperator{\Ran}{Ran}         % Range
\DeclareMathOperator{\Dom}{Dom}         % Domain
\DeclareMathOperator{\sgn}{sgn}         % Sign symbol
\DeclareMathOperator{\sign}{sign}       % Sign symbol
\DeclareMathOperator{\E}{\mathbb{E}}    % Expected value

% Square root with hook; see http://groups.google.com/group/comp.text.tex/msg/686390a837dae2f2?dmode=source:
\let\originalsqrt\sqrt
\def\varsqrt #1#2{{\setbox0=\hbox{$#1\originalsqrt{#2\hskip2pt}$}%
                    \dimen0=\ht0 \advance\dimen0-.5pt
    \dimen2=\dimen0
         \advance\dimen2 -1.5pt
         \ifdim \dimen2 >10pt \advance\dimen2-.5pt \fi
         \ifdim \dimen2 >15pt \advance\dimen2 -1pt \fi
    \box0 \kern-.4pt
    \vrule width.4pt height\dimen0 depth -\dimen2 \kern.8pt
  }}
\def\Sqrt {\mathpalette \varsqrt }
% \let\sqrt\Sqrt


\ifuibmi@showexpl
% This is two environments to set LaTeX examples.
\RequirePackage{showexpl}

\setlength{\fboxrule}{0pt}
\lstset{explpreset={numbers=none,language=[LaTeX]TEX,pos=r,hsep=2em,keywords={toprule,bottomrule,midrule}}}
\fi

% Display the quote characters " and ' in program listings up-right, instead of as curly quotes.
\RequirePackage{upquote}

% For program code:
\RequirePackage{listings}
\lstloadlanguages{R,TeX,[LaTeX]Tex,Matlab,S,[PLUS]S,SAS}
\ifuibmi@showexpl
  \lstset{language=R, numbers=none, basicstyle={\codefontsize\ttfamily}, keywordstyle=\color{PrimColDark}\bfseries, stringstyle=\color{AltColDark}, fontadjust=true, xleftmargin=6pt, xrightmargin=6pt, breaklines=true, frame=single, backgroundcolor=\color{PrimColLight}, rulecolor=\color{PrimColLight}, framerule=0pt, framesep=6pt, extendedchars=true, showspaces=false, columns=flexible} % fixme: Something is wrong with showexpl. Does not accept belowskip.
\else
  \lstset{language=R, numbers=none, basicstyle={\codefontsize\ttfamily}, keywordstyle=\color{PrimColDark}\bfseries, stringstyle=\color{AltColDark}, fontadjust=true, xleftmargin=6pt, xrightmargin=6pt, breaklines=true, frame=single, backgroundcolor=\color{PrimColLight}, rulecolor=\color{PrimColLight}, framerule=0pt, framesep=6pt, extendedchars=true, showspaces=false, columns=flexible, aboveskip=1em, belowskip=1em} % fixme: Something is wrong with showexpl. Does not accept belowskip.
\fi

% For R code generated by Sweave:
\RequirePackage{fancyvrb}
%fixme: colours
\AtBeginDocument{
\DefineVerbatimEnvironment{Sinput}{Verbatim}{fontsize=\codefontsize,formatcom=\color{PrimColDark}}
\DefineVerbatimEnvironment{Soutput}{Verbatim}{fontsize=\codefontsize,formatcom=\color{AltColDark}}
\DefineVerbatimEnvironment{Scode}{Verbatim}{fontsize=\codefontsize,formatcom=\color{PrimColDark}}
\newenvironment{Schunk}{}{}
}

% Microtypography, for improving hyphenations, and more:
\ifdraft\else % One may add the 'draft' option for faster compiling.
\ifpdf
  \RequirePackage[final,step=2]{microtype}
   % Fix extra dot in leader dots in ToC; see
   % http://groups.google.com/group/comp.text.tex/browse_thread/thread/2eb80bc0b748cd30/e8598efd3db27655
   \let\uibmi@oldtoc\tableofcontents 
   \def\tableofcontents{%
   \microtypesetup{protrusion=false}\uibmi@oldtoc%
   \microtypesetup{protrusion=true}}

   % And sometimes we get an extra empty line
   % between two bibliography entries. Fix this also.
   \let\uibmi@oldbib\bibliography
   \def\bibliography#1{%
   \microtypesetup{protrusion=false}\uibmi@oldbib{#1}%
   \microtypesetup{protrusion=true}}
\fi % End \ifpdf
\fi % End \if@draft

% Allow no hyphenation (see 'datasett' command).
\RequirePackage{hyphenat}

\newcommand{\dnorm}[3][x]{\frac{1}{\sqrt{2\pi}#3}\exp\biggl[-\frac{1}{2}\Bigl(\frac{#1-#2}{#3}\Bigr)^2\biggr]}
\newcommandtwoopt{\mvdnorm}[7][x][y]{\frac{1}{2\pi#5#6\sqrt{1-#7^2}}\exp\Biggl\{-\frac{1}{2(1-#7^2)}\biggl[\Bigl(\frac{#1-#3}{#5}\Bigr)^2-2#7\Bigl(\frac{#1-#3}{#5}\Bigr)\Bigl(\frac{#2-#4}{#6}\Bigr) +\Bigl(\frac{#2-#4}{#6}\Bigr)^2\biggr]\Biggr\}}
\newcommand{\dgamma}[3][x]{\frac{1}{#3^#2\Gamma(#2)}#1^{#2-1}e^{-\frac{#1}{#3}}}
\newcommand{\dgammaalt}[3][x]{\frac{#3^#2}{\Gamma(#2)}#1^{#2-1}e^{-#3#1}}
\newcommand{\dexp}[2][x]{\frac{1}{#2}e^{-\frac{#1}{#2}}}
\newcommand{\dexpalt}[2][x]{#2e^{-#2#1}}
\newcommand{\dchisq}[2][x]{\frac{1}{2^{#2/2}\Gamma(#2/2)}#1^{#2/2-1}e^{-\frac{#1}{2}}}

% Equal sign with 'def' above:
\newcommand{\eqdef}{\ensuremath{\stackrel{\mathrm{def}}{=}}}

% URLs
\let\oldurl\url
\def\url{\codefontsize\oldurl}

% Commands and (uppercase) abbreviations (like ANOVA):
\let\oldtexttt\texttt
\def\texttt{\codefontsize\oldtexttt}
\newcommand{\datasett}[1]{\nohyphens{\codefontsize\texttt{\textcolor{AltColDark}{#1}}}}
\newcommand{\kommando}[1]{\datasett{#1}}
\newcommand{\klasse}[1]{\datasett{#1}}
\newcommand{\pakke}[1]{\datasett{#1}}
\newcommand{\program}[1]{\datasett{#1}}
\newcommand{\meny}[1]{\datasett{#1}}
\newcommand{\filnamn}[1]{\datasett{#1}}
\newcommand{\tast}[1]{\mbox{\datasett{#1}}}
\newcommand{\forkorting}[1]{\nohyphens{\ifuibmi@charter\uppercase{#1}\else\ifuibmi@utopia\uppercase{#1}\else\textsc{\lowercase{#1}}\fi\fi}}
\newcommand{\kort}[1]{\forkorting{#1}}


\ifuibmi@norwlang
  \newcommand{\prosent}{\:\%\xspace}
\else
  \newcommand{\prosent}{\%\xspace}
\fi
\newcommand{\percent}{\prosent}


% 'Magical' differential operator, that can be used for everything,
% taken from http://www.tug.org/TUGboat/Articles/tb18-1/tb54becc.pdf.
% Using improved (and much shorter) version from Morten Høgholm <mh.ctan@gmail.com>.
\newcommand*\diff{
   \mathop{}\nobreak
   \mskip-\thinmuskip\nobreak
   \mathrm{d}
}

\newcommand*\pdiff{
   \mathop{}\nobreak
   \mskip-\thinmuskip\nobreak
   \partial
}

% And a 'magical' fraction slash, also from Morten Høgholm:
\providecommand*\ifrac[2]{
  \begingroup #1 \endgroup
  % A Close / so no space before it (unless a punctuation atom is the
  % last item in #1 but unlikely).
  \mathclose{/}%
  % Followed by an empty Open. Thus no space is inserted before the
  % denominator.
  \mathopen{}%
  \begingroup #2 \endgroup
}

% The 'Vega' glyph. See f.i. http://stats.lse.ac.uk/norberg/links/papers/GREEKS.pdf
\newcommand{\vega}{\ensuremath{\mathcal{V}}}

% Be fussy in typesetting (may lead to bad hboxes and vboxes):
\fussy

% We do not use a fixed-width font, so frenchspacing is better (yes, even for English!):
\frenchspacing

% Hyphenation exceptions. Fixes incorrect hyphenation of some common (US English) words.
% See http://www.ctan.org/tex-archive/info/digests/tugboat/ushyphex.tex
\ifuibmi@USenglish
  \selectlanguage{USenglish}
  \input{ushyphex.tex}
\fi
\AtBeginDocument{\hyphenation{Huft-hammer}}

% According to http://www.tug.org/TUGboat/Articles/tb23-3-4/tb75hyf.pdf, it is considered
% bad style in UK English to hyphenate a word after only two letters.
% But this is not true after all (see, f.i., the New Oxford Spelling Dictionary).
%\ifuibmi@UKenglish
%  \selectlanguage{UKenglish} % Does this have any effect?
%  \lefthyphenmin=3
%\fi


% Questions and answers (FAQ).
\newenvironment{qanda}{}{\par}
\newcommand{\question}{\medskip\bfseries\sffamily\noindent\boldmath\uibmi@mv\leftskip=0em}
\newcommand{\answer}{\par\normalfont\mdseries\noindent\unboldmath\mathversion{normal}\leftskip=2em}


% For TikZ/PGF graphics
\usepackage{tikz}

% Set up margins (this needs be to run after all
% other font and linespacing commands):
\typearea[current]{last}

% \pdfortextstring
% anbefaling/fråbefaling av bøker
% latex-faqane (òg visuelle)
% meint som tosidig
% utskrift skal skje i pdf-format
% tips om feilting frå her:
% http://www.stud.math.ntnu.no/kurs/kurs.pdf
% Bruk vanlig logo (ikkje MI sin) viss \department er omdef.
% Fiks innebygging av symbol-skrifta.
% plassering av namn, tittel og sånt i rett rekkjefølgja på forsdia
% Dokumenter TeX Gyre-skriftene, og sjekk dei,
% når mattestøtte er komen.
% For skrifter uten kapitélar:
% \SetTracking{encoding=*}{50}
% \renewcommand{\forkorting}[1]{\textls{#1}}
% 
% \def\urlprefix{\kort{URL}:~\codefontsize}
