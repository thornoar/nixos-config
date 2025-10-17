" Language:     MLScript

if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword     mlsType          void bool bool3 int real string file arrowbar Label interpolate margin
syn keyword     mlsType          pair triple transform guide path pen frame picture slice orientation arrowhead
syn keyword     mlsType          smooth hole subset animation element deferredPath gauss dpar tarrow tbar
syn keyword     mlsStatement     break return continue unravel as
syn keyword     mlsConditional   if else
syn keyword     mlsRepeat        while for do
syn keyword     mlsExternal      access from import include config export settings " defaultconfig defaultexport defaultpaths
syn keyword     mlsOperator      new operator
syn keyword     mlsConstant      VERSION
syn keyword     mlsConstant      true false default infinity inf nan
syn keyword     mlsConstant      null nullframe nullpath nullpen
syn keyword     mlsConstant      intMin intMax realMin realMax
syn keyword     mlsConstant      realEpsilon realDigits
syn keyword     mlsPathSpec      and cycle controls tension atleast curl
syn keyword     mlsStorageClass  static public restricted private explicit
syn keyword     mlsStructure     struct typedef
syn keyword     mlsConstant      currentpicture currentpen
syn keyword     mlsConstant      inch inches in cm mm bp pt up down right left
syn keyword     mlsConstant      E NE N NW W SW S SE
syn keyword     mlsConstant      ENE NNE NNW WNW WSW SSW SSE ESE
syn keyword     mlsConstant      pi twopi None NoMargin
syn keyword     mlsConstant      CCW CW SimpleHead TeXHead HookHead
syn keyword     mlsConstant      undefined sqrtEpsilon Align mantissaBits
syn keyword     mlsConstant      identity zeroTransform invert
syn keyword     mlsConstant      stdin stdout
syn keyword     mlsConstant      unitsquare unitcircle circleprecision
syn keyword     mlsConstant      solid dotted Dotted dashed dashdotted
syn keyword     mlsConstant      longdashed longdashdotted
syn keyword     mlsConstant      squarecap roundcap extendcap
syn keyword     mlsConstant      miterjoin roundjoin beveljoin
syn keyword     mlsConstant      invisible zerowinding evenodd basealign nobasealign
syn keyword     mlsConstant      black white red green blue Cyan Magenta
syn keyword     mlsConstant      Yellow Black cyan magenta yellow palered
syn keyword     mlsConstant      palegreen paleblue palecyan palemagenta
syn keyword     mlsConstant      paleyellow palegray lightred lightgreen
syn keyword     mlsConstant      lightblue lightcyan lightmagenta lightyellow
syn keyword     mlsConstant      lightgray mediumred mediumgreen mediumblue
syn keyword     mlsConstant      mediumcyan mediummagenta mediumyellow
syn keyword     mlsConstant      mediumgray heavyred heavygreen heavyblue
syn keyword     mlsConstant      heavycyan heavymagenta lightolive heavygray
syn keyword     mlsConstant      deepred deepgreen deepblue deepcyan
syn keyword     mlsConstant      deepmagenta deepyellow deepgray darkred
syn keyword     mlsConstant      darkgreen darkblue darkcyan darkmagenta
syn keyword     mlsConstant      darkolive darkgray orange fuchsia chartreuse
syn keyword     mlsConstant      springgreen purple royalblue salmon brown
syn keyword     mlsConstant      olive darkbrown pink palegrey lightgrey
syn keyword     mlsConstant      mediumgrey grey heavygrey deepgrey darkgrey
                
syn keyword     mlsConstant      dn ucircle usquare smoothcolor subsetcolor convexpaths concavepaths debugpaths cartesian combined free plain deferredPaths savedDeferredPaths simple simples


syn keyword mlsConstant      GreenYellow Yellow Goldenrod Dandelion
syn keyword mlsConstant      Apricot Peach Melon YellowOrange Orange
syn keyword mlsConstant      BurntOrange Bittersweet RedOrange Mahogany
syn keyword mlsConstant      Maroon BrickRed Red OrangeRed RubineRed
syn keyword mlsConstant      WildStrawberry Salmon CarnationPink Magenta
syn keyword mlsConstant      VioletRed Rhodamine Mulberry RedViolet
syn keyword mlsConstant      Fuchsia Lavender Thistle Orchid DarkOrchid
syn keyword mlsConstant      Purple Plum Violet RoyalPurple BlueViolet
syn keyword mlsConstant      Periwinkle CadetBlue CornflowerBlue
syn keyword mlsConstant      MidnightBlue NavyBlue RoyalBlue Blue
syn keyword mlsConstant      Cerulean Cyan ProcessBlue SkyBlue Turquoise
syn keyword mlsConstant      TealBlue Aquamarine BlueGreen Emerald
syn keyword mlsConstant      JungleGreen SeaGreen Green ForestGreen
syn keyword mlsConstant      PineGreen LimeGreen YellowGreen SpringGreen
syn keyword mlsConstant      OliveGreen RawSienna Sepia Brown Tan Gray
syn keyword mlsConstant      Black White

syn keyword mlsConstant      AliceBlue AntiqueWhite Aqua Aquamarine Azure
syn keyword mlsConstant      Beige Bisque Black BlanchedAlmond Blue
syn keyword mlsConstant      BlueViolet Brown BurlyWood CadetBlue
syn keyword mlsConstant      Chartreuse Chocolate Coral CornflowerBlue
syn keyword mlsConstant      Cornsilk Crimson Cyan DarkBlue DarkCyan
syn keyword mlsConstant      DarkGoldenrod DarkGray DarkGreen DarkKhaki
syn keyword mlsConstant      DarkMagenta DarkOliveGreen DarkOrange
syn keyword mlsConstant      DarkOrchid DarkRed DarkSalmon DarkSeaGreen
syn keyword mlsConstant      DarkSlateBlue DarkSlateGray DarkTurquoise
syn keyword mlsConstant      DarkViolet DeepPink DeepSkyBlue DimGray
syn keyword mlsConstant      DodgerBlue FireBrick FloralWhite ForestGreen
syn keyword mlsConstant      Fuchsia Gainsboro GhostWhite Gold Goldenrod
syn keyword mlsConstant      Gray Green GreenYellow Honeydew HotPink
syn keyword mlsConstant      IndianRed Indigo Ivory Khaki Lavender
syn keyword mlsConstant      LavenderBlush LawnGreen LemonChiffon
syn keyword mlsConstant      LightBlue LightCoral LightCyan
syn keyword mlsConstant      LightGoldenrodYellow LightGreen LightGrey
syn keyword mlsConstant      LightPink LightSalmon LightSeaGreen
syn keyword mlsConstant      LightSkyBlue LightSlateGray LightSteelBlue
syn keyword mlsConstant      LightYellow Lime LimeGreen Linen Magenta
syn keyword mlsConstant      Maroon MediumAquamarine MediumBlue
syn keyword mlsConstant      MediumOrchid MediumPurple MediumSeaGreen
syn keyword mlsConstant      MediumSlateBlue MediumSpringGreen
syn keyword mlsConstant      MediumTurquoise MediumVioletRed MidnightBlue
syn keyword mlsConstant      MintCream MistyRose Moccasin NavajoWhite
syn keyword mlsConstant      Navy OldLace Olive OliveDrab Orange
syn keyword mlsConstant      OrangeRed Orchid PaleGoldenrod PaleGreen
syn keyword mlsConstant      PaleTurquoise PaleVioletRed PapayaWhip
syn keyword mlsConstant      PeachPuff Peru Pink Plum PowderBlue Purple
syn keyword mlsConstant      Red RosyBrown RoyalBlue SaddleBrown Salmon
syn keyword mlsConstant      SandyBrown SeaGreen Seashell Sienna Silver
syn keyword mlsConstant      SkyBlue SlateBlue SlateGray Snow SpringGreen
syn keyword mlsConstant      SteelBlue Tan Teal Thistle Tomato Turquoise
syn keyword mlsConstant      Violet Wheat White WhiteSmoke Yellow
syn keyword mlsConstant      YellowGreen

syn keyword mlsType          path3 guide3 transform3
syn keyword mlsType          projection light material patch surface tube
syn keyword mlsConstant      currentprojection currentlight defaultrender
syn keyword mlsConstant      identity4 O X Y Z
syn keyword mlsConstant      nolight nullpens
syn keyword mlsConstant      unitsphere unithemisphere unitplane octant1
syn keyword mlsConstant      unitcone unitsolidcone unitcube unitcylinder
syn keyword mlsConstant      unitdisk unittube

syn match mlsModule display       "\(\(import\|access\|unravel\|include\)\s\+\)\@<=\(plain\|simplex\|math\|interpolate\|geometry\|trembling\|stats\|patterns\|markers\|map\|tree\|binarytree\|drawtree\|syzygy\|feynman\|roundedpath\|animation\|embed\|slide\|MetaPost\|babel\|labelpath\|labelpath3\|annotate\|CAD\|graph\|palette\|three\|obj\|graph3\|grid3\|solids\|tube\|flowchart\|contour\|contour3\|smoothcontour3\|slopefield\|ode\|smoothmanifold\|export\|pathmethods\)"

" syn match   mlsConst       "default\w*"
" syn match   mlsConst       "df\w*"
syn match   mlsConst       "null\w*"
" syn match   mlsConst       "debug\w*"
" syn match   mlsConst       "current\w*"
" syn match   mlsConst       "cr\w*"
hi def link mlsConst Constant

" number constants
syn match  mlsNumbers     display transparent "\<\d\|\.\d"
                        \ contains=mlsNumber,mlsNumberError
syn match  mlsNumber      display contained "\d*\.\=\d*\(e[-+]\=\d\+\)\="
" highlight number constants with two '.' or with '.' after an 'e'
syn match  mlsNumberError display contained "\d*\.\(\d\|e[-+]\=\)*\.[0-9.]*"
syn match  mlsNumberError display contained "\d*e[-+]\=\d*\.[0-9.]*"
syn match  mlsNumberError display contained "\d*e[-+]\=\(e[-+]\=\)*\.[0-9.]*"


" Highlight function names
syn match    mlsCustomFunc     "\w\+\s*(\@="
syn match    mlsCustomFunc     "\(\.\.\)\@<!\.\@<=\w\+"
hi def link  mlsCustomFunc Function

syn match    mlsPathOperator   "\.\@<!\.\.\.\@!"
syn match    mlsPathOperator   "--"
syn match    mlsPathOperator   "\^\^"
" syn match    mlsPathOperator   "*"
hi def link  mlsPathOperator Operator

" string constants
syn region mlsCString start=+'+ end=+'+ skip=+\\\\\|\\'+ contains=mlsCSpecial
syn match  mlsCSpecial display contained +\\\(['"?\\abfnrtv]\|\o\{1,3}\)+
syn match  mlsCSpecial display contained +\\\(x[0-9A-F]\{1,2\}\|$\)+
" double quoted strings only special character is \"
syn region mlsString   start=+"+ end=+"+ skip=+\\\\\|\\"+ contains=mlsSpecial
syn match  mlsSpecial  display contained +\(\\\)\@1<!\(\\\\\)*\zs\\"+


" comments and comment strings
syn keyword  mlsTodo            contained TODO FIXME XXX
syn sync     ccomment           mlsComment minlines=15
if exists("mls_comment_strings")
  syn match  mlsCommentSkip     contained "^\s*\*\($\|\s\+\)"
  syn region mlsCommentString   contained start=+"+ skip=+\\\\\|\\"+ end=+"+
                              \ end=+\*/+me=s-1
                              \ contains=mlsSpecial,mlsCommentSkip
  syn region mlsCommentCString  contained start=+'+ skip=+\\\\\|\\'+ end=+'+
                              \ end=+\*/+me=s-1
                              \ contains=mlsCSpecial,mlsCommentSkip
  syn region mlsCommentLString  contained start=+"+ skip=+\\\\\|\\"+ end=+"+
                              \ end="$" contains=mlsSpecial
  syn region mlsCommentLCString contained start=+'+ skip=+\\\\\|\\'+ end=+'+
                              \ end="$" contains=mlsCSpecial
  syn region mlsCommentL        start="//" skip="\\$" end="$" keepend
                              \ contains=mlsTodo,mlsCommentLString,
                              \ mlsCommentLCString,mlsNumbers
  syn region mlsComment         matchgroup=mlsComment start="/\*" end="\*/"
                              \ contains=mlsTodo,mlsCommentStartError,
                              \ mlsCommentString,mlsCommentCString,mlsNumbers
else
  syn region mlsCommentL        start="//" skip="\\$" end="$" keepend
                              \ contains=mlsTodo
  syn region mlsComment         matchgroup=mlsComment start="/\*" end="\*/"
                              \ contains=mlsTodo,mlsCommentStartError
endif

" highlight common errors when starting/ending C comments
syn match    mlsCommentError      display "\*/"
syn match    mlsCommentStartError display "/\*"me=e-1 contained

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_mls_syn_inits")
  if version < 508
    let did_mls_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink mlsCommentL             Comment
  HiLink mlsConditional          Conditional
  HiLink mlsRepeat               Repeat
  HiLink mlsNumber               Number
  HiLink mlsNumberError          mlsError
  HiLink mlsCurlyError           mlsError
  HiLink mlsBracketError         mlsError
  HiLink mlsParenError           mlsError
  HiLink mlsCommentError         mlsError
  HiLink mlsCommentStartError    mlsError
  HiLink mlsOperator             Operator
  HiLink mlsStructure            Operator
  HiLink mlsStorageClass         StorageClass
  HiLink mlsExternal             Include
  HiLink mlsDefine               Macro
  HiLink mlsError                Error
  HiLink mlsStatement            Statement
  HiLink mlsType                 Type
  HiLink mlsConstant             Constant
  HiLink mlsCommentString        mlsString
  HiLink mlsCommentCString       mlsString
  HiLink mlsCommentLString       mlsString
  HiLink mlsCommentLCString      mlsString
  HiLink mlsCommentSkip          mlsComment
  HiLink mlsString               String
  HiLink mlsCString              String
  HiLink mlsComment              Comment
  HiLink mlsSpecial              SpecialChar
  HiLink mlsCSpecial             SpecialChar
  HiLink mlsTodo                 Todo
  HiLink mlsPathSpec             Statement
  HiLink mlsModule               Constant

  delcommand HiLink
endif

let b:current_syntax = "mlscript"
