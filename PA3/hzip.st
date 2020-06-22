
#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f

"=== Shorthand ========================================================"

nl := Character nl.

quit := [:status | ObjectMemory quit: status ].

"=== Miscellaneous utilities =========================================="

"Exit with usage message"
usageExit := [
  stderr << 'Usage: ./hzip.st [-t infile] [-c | -u infile [outfile]]'
         << nl.
  quit value: 1
].

"=== Character override ==============================================="

Character extend [
  isGraph [ ^ (Character space < self) & (self <= $~) ]
 
  "Display non-Graph Characters as hex"
  visible [
    self isGraph
      ifTrue: [^ (' ', self asString, ' ')]
      ifFalse: [
        self asInteger = 256
          ifTrue: [^ 'EOF']
          ifFalse: [
            ^ 'x', (self asInteger printPaddedWith: $0 to: 2 base: 16)
          ]
      ]
  ]
]

"=== Frequency (of ints) storing tree implementation =================="

Object subclass: Leaf [
  |char count|
  char [ ^ char ] "Integer representing a byte's value"
  count [ ^ count ]

  Leaf class >> new [
    self shouldNotImplement
  ]

  Leaf class >> new: aChar count: aCount [
    |result|
    result := super new.
    result setChar: aChar andCount: aCount.
    ^result
  ]

  isLeaf [ ^ true ]

  setChar: aChar andCount: aCount [
    char := aChar.
    count := aCount.
  ]

  <= other [
    ^ (count < other count)
    | ((count = other count) & (char <= other char))
  ]

  printBase: aStream [
    ^ aStream << self class << '(' << char << ',' << count
  ]

  printOn: aStream [
    (self printBase: aStream) << ')'.
  ]

  depthFirst: visitor prefix: string [
    visitor value: char value: string
  ]

  postOrder: leaf onNode: unused prefix: string [
    self depthFirst: leaf prefix: string
  ]
].

Leaf subclass: Tree [
  |left right|
  left [ ^ left ]
  right [ ^ right ]

  Tree class >> new: aChar count: aCount [
    self shouldNotImplement
  ]

  Tree class >> new: aChar count: aCount left: aLeft right: aRight [
    |result|
    result := super new: aChar count: aCount.
    result setLeft: aLeft andRight: aRight.
    ^ result
  ]

  setLeft: aLeft andRight: aRight [
    left := aLeft.
    right := aRight.
  ]

  printOn: aStream [
    (self printBase: aStream) << ',' << left << ',' << right << ')'.
  ]

  isLeaf [ ^ false ]

  depthFirst: visitor prefix: string [
    left depthFirst: visitor prefix: string, '0'.
    right depthFirst: visitor prefix: string, '1'.
  ]

  postOrder: leaf onNode: tree prefix: string [
    left postOrder: leaf onNode: tree prefix: string, '0'.
    right postOrder: leaf onNode: tree prefix: string, '1'.
    tree value
  ]
].

" === SortedCollection-based priority queue implementation ============"

Object subclass: PrioQueue [
  |queue size|

  PrioQueue class >> new [
    |res|
    res := super new.
    res init.
    ^ res
  ]

  init [ queue := SortedCollection new ]

  putChar: ch putCount: co [
    ^ queue add: (Leaf new: ch count: co)
  ]

  putLeft: left putRight: right [
    |node|
    node := Tree new: (left char)
      count: (left count + right count)
      left: left
      right: right.
    queue add: node
  ]

  squash [
    queue size = 0 ifTrue: [^ nil].
    [true] whileTrue:
      [ |left right|
        left := queue removeFirst.
        queue size = 0 ifTrue: [^ left].
        right := queue removeFirst.
        self putLeft: left putRight: right]
  ]
]

"=== Array-based frequency table implementation ======================="

Object subclass: FreqTable [
  |table|

  FreqTable class >> new [
    |res|
    res := super new.
    res init.
    ^ res
  ]

  init [ table := Array new: 257 ]

  size [ ^ table size ]

  at: index [ ^ table at: index + 1 ]

  at: index put: value [ ^ table at: index + 1 put: value ]

  incr: index [
    (self at: index) isNil
      ifTrue: [ self at: index put: 0].
    self at: index put: (self at: index) + 1.
  ]

  keysAndValuesDo: aBlock [
    (0 to: (self size) - 1) do: [:index |
      aBlock value: index value: (self at: index).
    ]
  ]
]

"=== File I/O utilities ==============================================="

"Bit-by-bit file reader"
Object subclass: BitStream [
  |bitIndex byte myStream|
  BitStream class >> new: fileStream [
    |result|
    result := super new.
    result init: fileStream.
    ^result
  ]
  init: fileStream [
    myStream := fileStream.
    bitIndex := 1.
  ]
  nextBit [
    bitIndex = 1 ifTrue: [
       byte := myStream next.
       bitIndex := 9.
    ].
    bitIndex := bitIndex - 1.
    ^byte value bitAt: bitIndex
  ]
  nextByte [
    | acc |
    acc := 0.
    1 to: 8 do: [:unused |
      | bit |
      bit := self nextBit.
      acc := acc * 2 + bit.
    ].
    ^acc
  ]
  atEnd [
    ^bitIndex = 1 and: [myStream atEnd]
  ]
  currByte [
    ^byte
  ]
]

"Bit-by-bit file writer"
Object subclass: OutBits [
  |bitIndex currentByte myStream|
  OutBits class >> new [
    self shouldNotImplement.
  ]
  OutBits class >> new: fileStream [
    |result|
    result := super new.
    result init: fileStream.
    ^result
  ]
  clearByte [
    bitIndex := 8.
    currentByte := 0.
  ]
  init: fileStream [
    myStream := fileStream.
    self clearByte.
  ]
  flushByte [
    bitIndex = 8 ifFalse: [
      myStream nextPutByte: currentByte.
      self clearByte.
    ]
  ]  
  writeBit: bit [
    currentByte := currentByte bitAt: bitIndex put: bit.
    bitIndex := bitIndex - 1.
    bitIndex = 0 ifTrue: [self flushByte].
  ]
  writeByte: byte [
    8 to: 1 by: -1 do: [:b | self writeBit: (byte bitAt: b)].
    byte = 0 ifTrue: [self writeBit: 0].
    byte = 256 ifTrue: [self writeBit: 1].
  ]
]

fileNameToReadStream := [ :name |
  | infile |
  [ infile := FileStream open: name mode: FileStream read ]
  on: SystemExceptions.FileError
  do: [
    stderr << 'Unable to open file: ' << name << nl.
    quit value: 1
  ]
].

fileNameToWriteStream := [ :name |
  | outfile |
  [outfile := FileStream open: name mode: FileStream write ]
  on: SystemExceptions.FileError
  do: [
    stderr << 'Error writing to file: ' << name << nl.
    quit value: 1
  ]
].

"=== Mode-specific logic =============================================="

   "=== Shared by compress and tabulate ============================"

getTable := [:infileName |
  | freq infile |
  freq := FreqTable new.
  infile := fileNameToReadStream value: infileName.
  [infile atEnd not] whileTrue: [
    |ordChar|
    ordChar := infile next asInteger.
    freq incr: ordChar.
  ].
  infile close.
  freq incr: 256.
  freq
].

getTree := [:freq |
  | queue tree |
  queue := PrioQueue new.
  freq keysAndValuesDo: [:key :value |
    (value notNil and: [value > 0])
    ifTrue: [
      queue putChar: key putCount: value.
    ]
  ].
  tree := queue squash.
  tree
].

   "=== Unique to each mode ========================================"

"-c logic"
compress := [:infileName :outfile |
  |infile freq tree encodings outBytes cenf|

  "Generate frequency table and Huffman tree"
  freq := getTable value: infileName.
  tree := getTree value: freq.

  "Single-pass encoding generation and table transcription"
  encodings := Dictionary new.
  outBytes := OutBits new: outfile.
  tree postOrder: [:char :string |
      outBytes writeBit: 0.
      outBytes writeByte: char.
      encodings at: char put: string.
    ]
    onNode: [ outBytes writeBit: 1 ]
    prefix: ''.
  "Extra 1 bit to mark tree end"
  outBytes writeBit: 1.

  "Reread input byte by byte, appending each byte's encoding to outfile"
  infile := fileNameToReadStream value: infileName.
  [infile atEnd not] whileTrue: [
    | ordChar encoding |
    ordChar := infile next asInteger.
    encoding := encodings at: ordChar.
    1 to: encoding size do: [:idx |
      outBytes writeBit: (encoding at: idx) asInteger - ($0 asInteger)
    ]
  ].
  infile close.
  "Write encoded EOF"
  cenf := encodings at: 256.
  1 to: cenf size do: [:idx |
    outBytes writeBit: (cenf at: idx) asInteger - ($0 asInteger)
  ].
  outBytes flushByte.
  outfile = stdout ifFalse: [ outfile close ].
  quit value: 0
].

"-t logic"
tabulate := [:infileName |
  |freq tree encodings|
  freq := getTable value: infileName.
  tree := getTree value: freq.
  encodings := Dictionary new.
  tree depthFirst: [:char :string | encodings at: char put: string]
    prefix: ''.
  freq keysAndValuesDo: [:key :value |
    (value notNil and: [value > 0])
    ifTrue: [
      "Note: matches sample-test5-output, differs from spec"
      stdout
        << key asCharacter visible
        << (value printPaddedWith: Character space to: 8)
        << '  '
        << (encodings at: key)
        << nl.
    ]
  ].
  quit value: 0
].

"-u helper to reconstruct Huffman tree"
rebuildTree := [:inbits |
  | stack res|
  stack := OrderedCollection new.
  res := nil.
  [res = nil] whileTrue: [ | bit |
    bit := inbits nextBit.
    bit = 0
      ifTrue: [ | byte |
        byte := inbits nextByte.
        byte = 0 ifTrue: [ | disam |
          disam := inbits nextBit.
          disam = 1 ifTrue: [ byte := 256 ]
        ].
        stack add: (Leaf new: byte count: 0) beforeIndex: 1.
      ]
      ifFalse: [ | oneTree zeroTree node |
        oneTree := stack removeFirst.
        stack size = 0 ifTrue: [ res := oneTree ]
          ifFalse: [
            zeroTree := stack removeFirst.
            node := Tree new: (zeroTree char)
              count: 0 left: zeroTree right: oneTree.
            stack add: node beforeIndex: 1.
          ]
      ]
  ].
  res
].

"-u helper to write single decompressed byte"
rewriteByte := [:inbits :outfile :tree |
  | byte cur |
  cur := tree.
  [ cur isLeaf ] whileFalse: [ | dir |
    dir := inbits nextBit.
    dir = 1 ifTrue: [ cur := cur right] ifFalse: [ cur := cur left]
  ].
  byte := cur char.
  byte = 256 ifFalse: [ outfile nextPutByte: byte ].
  byte = 256.
].

"-u logic"
decompress := [:infileName :outfile |
  | infile inbits tree written |
  infile := fileNameToReadStream value: infileName.
  inbits := BitStream new: infile.
  tree := rebuildTree value: inbits.

  written := false.
  [ written ] whileFalse: [
    written := (rewriteByte value: inbits value: outfile value: tree).
  ].
  outfile = stdout ifFalse: [ outfile close ].
  infile close.
  quit value: 0
].

"=== 'main' logic ====================================================="

args := Smalltalk arguments.
argc := Smalltalk arguments size.

argc < 2 ifTrue: usageExit.

mode := Smalltalk arguments at: 1.
(mode = '-t') | (mode = '-dt') | (mode = '-td') ifTrue: [
  argc = 2
    ifTrue: [
      tabulate value: (args at: 2).
      quit value: 0
    ]
    ifFalse: usageExit
].

(mode = '-c') | (mode = '-dc') | (mode = '-cd') ifTrue: [
  argc = 2
    ifTrue: [
      compress value: (args at: 2) value: stdout.
      quit value: 0
    ]
    ifFalse: [
      argc = 3
        ifTrue: [
          compress value: (args at: 2)
            value: (fileNameToWriteStream value: (args at: 3)).
          quit value: 0
        ]
        ifFalse: usageExit
    ]].

(mode = '-u') | (mode = '-du') | (mode = '-ud')
  ifTrue: [
    argc = 2
      ifTrue: [
        decompress value: (args at: 2) value: stdout.
        quit value: 0
      ]
      ifFalse: [
        argc = 3
          ifTrue: [
            decompress
              value: (args at: 2)
              value: (fileNameToWriteStream value: (args at: 3)).
            quit value: 0
          ]
          ifFalse: usageExit
      ]
  ]
  ifFalse: usageExit.
