" Vim syntax file
" Language:     Asymptote

if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

" useful C/C++/Java keywords
syn keyword     asyStatement     break return continue unravel as
syn keyword     asyConditional   if else
syn keyword     asyRepeat        while for do
syn keyword     asyExternal      access from import include settings
syn keyword     asyOperator      new operator

" basic asymptote keywords
syn keyword     asyConstant      VERSION
syn keyword     asyConstant      true false default infinity inf nan
syn keyword     asyConstant      null nullframe nullpath nullpen
syn keyword     asyConstant      intMin intMax realMin realMax
syn keyword     asyConstant      realEpsilon realDigits
syn keyword     asyPathSpec      and cycle controls tension atleast curl
syn keyword     asyStorageClass  static public restricted private explicit
syn keyword     asyStructure     struct typedef
syn keyword     asyType          void bool bool3 int real string file arrowbar Label interpolate
syn keyword     asyType          pair triple transform guide path pen frame picture slice orientation arrowhead

syn keyword     asyType          smooth hole subset animation element deferredPath gauss dpar

" module specific keywords
syn keyword   asyConstant      currentpicture currentpen
syn keyword   asyConstant      inch inches in cm mm bp pt up down right left
syn keyword   asyConstant      E NE N NW W SW S SE
syn keyword   asyConstant      ENE NNE NNW WNW WSW SSW SSE ESE
syn keyword   asyConstant      pi twopi
syn keyword   asyConstant      CCW CW SimpleHead TeXHead HookHead
syn keyword   asyConstant      undefined sqrtEpsilon Align mantissaBits
syn keyword   asyConstant      identity zeroTransform invert
syn keyword   asyConstant      stdin stdout
syn keyword   asyConstant      unitsquare unitcircle circleprecision
syn keyword   asyConstant      solid dotted Dotted dashed dashdotted
syn keyword   asyConstant      longdashed longdashdotted
syn keyword   asyConstant      squarecap roundcap extendcap
syn keyword   asyConstant      miterjoin roundjoin beveljoin
syn keyword   asyConstant      invisible zerowinding evenodd basealign nobasealign
syn keyword   asyConstant      black white red green blue Cyan Magenta
syn keyword   asyConstant      Yellow Black cyan magenta yellow palered
syn keyword   asyConstant      palegreen paleblue palecyan palemagenta
syn keyword   asyConstant      paleyellow palegray lightred lightgreen
syn keyword   asyConstant      lightblue lightcyan lightmagenta lightyellow
syn keyword   asyConstant      lightgray mediumred mediumgreen mediumblue
syn keyword   asyConstant      mediumcyan mediummagenta mediumyellow
syn keyword   asyConstant      mediumgray heavyred heavygreen heavyblue
syn keyword   asyConstant      heavycyan heavymagenta lightolive heavygray
syn keyword   asyConstant      deepred deepgreen deepblue deepcyan
syn keyword   asyConstant      deepmagenta deepyellow deepgray darkred
syn keyword   asyConstant      darkgreen darkblue darkcyan darkmagenta
syn keyword   asyConstant      darkolive darkgray orange fuchsia chartreuse
syn keyword   asyConstant      springgreen purple royalblue salmon brown
syn keyword   asyConstant      olive darkbrown pink palegrey lightgrey
syn keyword   asyConstant      mediumgrey grey heavygrey deepgrey darkgrey

syn keyword   asyConstant      dn ucircle usquare smoothcolor subsetcolor convexpath concavepath cartesian combined free plain deferredPaths savedDeferredPaths simple simples


syn keyword asyConstant      GreenYellow Yellow Goldenrod Dandelion
syn keyword asyConstant      Apricot Peach Melon YellowOrange Orange
syn keyword asyConstant      BurntOrange Bittersweet RedOrange Mahogany
syn keyword asyConstant      Maroon BrickRed Red OrangeRed RubineRed
syn keyword asyConstant      WildStrawberry Salmon CarnationPink Magenta
syn keyword asyConstant      VioletRed Rhodamine Mulberry RedViolet
syn keyword asyConstant      Fuchsia Lavender Thistle Orchid DarkOrchid
syn keyword asyConstant      Purple Plum Violet RoyalPurple BlueViolet
syn keyword asyConstant      Periwinkle CadetBlue CornflowerBlue
syn keyword asyConstant      MidnightBlue NavyBlue RoyalBlue Blue
syn keyword asyConstant      Cerulean Cyan ProcessBlue SkyBlue Turquoise
syn keyword asyConstant      TealBlue Aquamarine BlueGreen Emerald
syn keyword asyConstant      JungleGreen SeaGreen Green ForestGreen
syn keyword asyConstant      PineGreen LimeGreen YellowGreen SpringGreen
syn keyword asyConstant      OliveGreen RawSienna Sepia Brown Tan Gray
syn keyword asyConstant      Black White

syn keyword asyConstant      AliceBlue AntiqueWhite Aqua Aquamarine Azure
syn keyword asyConstant      Beige Bisque Black BlanchedAlmond Blue
syn keyword asyConstant      BlueViolet Brown BurlyWood CadetBlue
syn keyword asyConstant      Chartreuse Chocolate Coral CornflowerBlue
syn keyword asyConstant      Cornsilk Crimson Cyan DarkBlue DarkCyan
syn keyword asyConstant      DarkGoldenrod DarkGray DarkGreen DarkKhaki
syn keyword asyConstant      DarkMagenta DarkOliveGreen DarkOrange
syn keyword asyConstant      DarkOrchid DarkRed DarkSalmon DarkSeaGreen
syn keyword asyConstant      DarkSlateBlue DarkSlateGray DarkTurquoise
syn keyword asyConstant      DarkViolet DeepPink DeepSkyBlue DimGray
syn keyword asyConstant      DodgerBlue FireBrick FloralWhite ForestGreen
syn keyword asyConstant      Fuchsia Gainsboro GhostWhite Gold Goldenrod
syn keyword asyConstant      Gray Green GreenYellow Honeydew HotPink
syn keyword asyConstant      IndianRed Indigo Ivory Khaki Lavender
syn keyword asyConstant      LavenderBlush LawnGreen LemonChiffon
syn keyword asyConstant      LightBlue LightCoral LightCyan
syn keyword asyConstant      LightGoldenrodYellow LightGreen LightGrey
syn keyword asyConstant      LightPink LightSalmon LightSeaGreen
syn keyword asyConstant      LightSkyBlue LightSlateGray LightSteelBlue
syn keyword asyConstant      LightYellow Lime LimeGreen Linen Magenta
syn keyword asyConstant      Maroon MediumAquamarine MediumBlue
syn keyword asyConstant      MediumOrchid MediumPurple MediumSeaGreen
syn keyword asyConstant      MediumSlateBlue MediumSpringGreen
syn keyword asyConstant      MediumTurquoise MediumVioletRed MidnightBlue
syn keyword asyConstant      MintCream MistyRose Moccasin NavajoWhite
syn keyword asyConstant      Navy OldLace Olive OliveDrab Orange
syn keyword asyConstant      OrangeRed Orchid PaleGoldenrod PaleGreen
syn keyword asyConstant      PaleTurquoise PaleVioletRed PapayaWhip
syn keyword asyConstant      PeachPuff Peru Pink Plum PowderBlue Purple
syn keyword asyConstant      Red RosyBrown RoyalBlue SaddleBrown Salmon
syn keyword asyConstant      SandyBrown SeaGreen Seashell Sienna Silver
syn keyword asyConstant      SkyBlue SlateBlue SlateGray Snow SpringGreen
syn keyword asyConstant      SteelBlue Tan Teal Thistle Tomato Turquoise
syn keyword asyConstant      Violet Wheat White WhiteSmoke Yellow
syn keyword asyConstant      YellowGreen

syn keyword asyType          path3 guide3 transform3
syn keyword asyType          projection light material patch surface tube
syn keyword asyConstant      currentprojection currentlight defaultrender
syn keyword asyConstant      identity4 O X Y Z
syn keyword asyConstant      nolight nullpens
syn keyword asyConstant      unitsphere unithemisphere unitplane octant1
syn keyword asyConstant      unitcone unitsolidcone unitcube unitcylinder
syn keyword asyConstant      unitdisk unittube

syn match asyModule display       "\(\(import\|acess\|unravel\|include\)\s\+\)\@<=\(plain\|simplex\|math\|interpolate\|geometry\|trembling\|stats\|patterns\|markers\|map\|tree\|binarytree\|drawtree\|syzygy\|feynman\|roundedpath\|animation\|embed\|slide\|MetaPost\|babel\|labelpath\|labelpath3\|annotate\|CAD\|graph\|palette\|three\|obj\|graph3\|grid3\|solids\|tube\|flowchart\|contour\|contour3\|smoothcontour3\|slopefield\|ode\|smoothmanifold\|export\|pathmethods\)"

syn match   asyConst       "default\w*"
" syn match   asyConst       "df\w*"
syn match   asyConst       "null\w*"
syn match   asyConst       "debug\w*"
syn match   asyConst       "current\w*"
" syn match   asyConst       "cr\w*"
hi def link asyConst Constant

" number constants
syn match  asyNumbers     display transparent "\<\d\|\.\d"
                        \ contains=asyNumber,asyNumberError
syn match  asyNumber      display contained "\d*\.\=\d*\(e[-+]\=\d\+\)\="
" highlight number constants with two '.' or with '.' after an 'e'
syn match  asyNumberError display contained "\d*\.\(\d\|e[-+]\=\)*\.[0-9.]*"
syn match  asyNumberError display contained "\d*e[-+]\=\d*\.[0-9.]*"
syn match  asyNumberError display contained "\d*e[-+]\=\(e[-+]\=\)*\.[0-9.]*"


" Highlight function names
syn match    asyCustomFunc     "\w\+\s*(\@="
syn match    asyCustomFunc     "\(\.\.\)\@<!\.\@<=\w\+"
hi def link  asyCustomFunc Function

syn match    asyPathOperator   "\.\@<!\.\.\.\@!"
syn match    asyPathOperator   "--"
syn match    asyPathOperator   "\^\^"
" syn match    asyPathOperator   "*"
hi def link  asyPathOperator Operator

" string constants
syn region asyCString start=+'+ end=+'+ skip=+\\\\\|\\'+ contains=asyCSpecial
syn match  asyCSpecial display contained +\\\(['"?\\abfnrtv]\|\o\{1,3}\)+
syn match  asyCSpecial display contained +\\\(x[0-9A-F]\{1,2\}\|$\)+
" double quoted strings only special character is \"
syn region asyString   start=+"+ end=+"+ skip=+\\\\\|\\"+ contains=asySpecial
syn match  asySpecial  display contained +\(\\\)\@1<!\(\\\\\)*\zs\\"+


" comments and comment strings
syn keyword  asyTodo            contained TODO FIXME XXX
syn sync     ccomment           asyComment minlines=15
if exists("asy_comment_strings")
  syn match  asyCommentSkip     contained "^\s*\*\($\|\s\+\)"
  syn region asyCommentString   contained start=+"+ skip=+\\\\\|\\"+ end=+"+
                              \ end=+\*/+me=s-1
                              \ contains=asySpecial,asyCommentSkip
  syn region asyCommentCString  contained start=+'+ skip=+\\\\\|\\'+ end=+'+
                              \ end=+\*/+me=s-1
                              \ contains=asyCSpecial,asyCommentSkip
  syn region asyCommentLString  contained start=+"+ skip=+\\\\\|\\"+ end=+"+
                              \ end="$" contains=asySpecial
  syn region asyCommentLCString contained start=+'+ skip=+\\\\\|\\'+ end=+'+
                              \ end="$" contains=asyCSpecial
  syn region asyCommentL        start="//" skip="\\$" end="$" keepend
                              \ contains=asyTodo,asyCommentLString,
                              \ asyCommentLCString,asyNumbers
  syn region asyComment         matchgroup=asyComment start="/\*" end="\*/"
                              \ contains=asyTodo,asyCommentStartError,
                              \ asyCommentString,asyCommentCString,asyNumbers
else
  syn region asyCommentL        start="//" skip="\\$" end="$" keepend
                              \ contains=asyTodo
  syn region asyComment         matchgroup=asyComment start="/\*" end="\*/"
                              \ contains=asyTodo,asyCommentStartError
endif

" highlight common errors when starting/ending C comments
syn match    asyCommentError      display "\*/"
syn match    asyCommentStartError display "/\*"me=e-1 contained

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_asy_syn_inits")
  if version < 508
    let did_asy_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink asyCommentL             Comment
  HiLink asyConditional          Conditional
  HiLink asyRepeat               Repeat
  HiLink asyNumber               Number
  HiLink asyNumberError          asyError
  HiLink asyCurlyError           asyError
  HiLink asyBracketError         asyError
  HiLink asyParenError           asyError
  HiLink asyCommentError         asyError
  HiLink asyCommentStartError    asyError
  HiLink asyOperator             Operator
  HiLink asyStructure            Operator
  HiLink asyStorageClass         StorageClass
  HiLink asyExternal             Include
  HiLink asyDefine               Macro
  HiLink asyError                Error
  HiLink asyStatement            Statement
  HiLink asyType                 Type
  HiLink asyConstant             Constant
  HiLink asyCommentString        asyString
  HiLink asyCommentCString       asyString
  HiLink asyCommentLString       asyString
  HiLink asyCommentLCString      asyString
  HiLink asyCommentSkip          asyComment
  HiLink asyString               String
  HiLink asyCString              String
  HiLink asyComment              Comment
  HiLink asySpecial              SpecialChar
  HiLink asyCSpecial             SpecialChar
  HiLink asyTodo                 Todo
  HiLink asyPathSpec             Statement
  HiLink asyModule               Constant

  delcommand HiLink
endif

let b:current_syntax = "asy"
