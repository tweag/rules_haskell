# Implementation adapted from
# http://www.json.org/JSON_checker/
def _enumify_iterable(iterable, enum_dict):
    for i, t in enumerate(iterable):
        enum_dict[t] = i
    return enum_dict

__ = -1
_TOKEN_CLASSES = _enumify_iterable(iterable = [
    'C_SPACE',  # space
    'C_WHITE',  # other whitespace
    'C_LCURB',  # {
    'C_RCURB',  # }
    'C_LSQRB',  # [
    'C_RSQRB',  # ]
    'C_COLON',  # :
    'C_COMMA',  # ,
    'C_QUOTE',  # "
    'C_BACKS',  # \
    'C_SLASH',  # /
    'C_PLUS',   # +
    'C_MINUS',  # -
    'C_POINT',  # .
    'C_ZERO',  # 0
    'C_DIGIT',  # 123456789
    'C_LOW_A',  # a
    'C_LOW_B',  # b
    'C_LOW_C',  # c
    'C_LOW_D',  # d
    'C_LOW_E',  # e
    'C_LOW_F',  # f
    'C_LOW_L',  # l
    'C_LOW_N',  # n
    'C_LOW_R',  # r
    'C_LOW_S',  # s
    'C_LOW_T',  # t
    'C_LOW_U',  # u
    'C_ABCDF',  # ABCDF
    'C_E',      # E
    'C_ETC',    # everything else
#    'NR_CLASSES',
], enum_dict = {'__' : __})

_STATES = _enumify_iterable(iterable = [
    'GO',  # start
    'OK',  # ok
    'OB',  # object
    'KE',  # key
    'CO',  # colon
    'VA',  # value
    'AR',  # array
    'ST',  # string
    'ES',  # escape
    'U1',  # u1
    'U2',  # u2
    'U3',  # u3
    'U4',  # u4
    'MI',  # minus
    'ZE',  # zero
    'IN',  # integer
    'FR',  # fraction
    'E1',  # e
    'E2',  # ex
    'E3',  # exp
    'T1',  # tr
    'T2',  # tru
    'T3',  # true
    'F1',  # fa
    'F2',  # fal
    'F3',  # fals
    'F4',  # false
    'N1',  # nu
    'N2',  # nul
    'N3',  # null
#    'NR_STATES',
], enum_dict = {})
_S = _STATES

_STATE_NAMES = [
    'start',
    'ok',
    'object',
    'key',
    'colon',
    'value',
    'array',
    'string',
    'escape',
    'u1',
    'u2',
    'u3',
    'u4',
    'minus',
    'zero',
    'integer',
    'fraction',
    'e',
    'ex',
    'exp',
    'tr',
    'tru',
    'true',
    'fa',
    'fal',
    'fals',
    'false',
    'nu',
    'nul',
    'null',
]


_MODE_NAMES = [
    'ARRAY',
    'DONE',
    'KEY',
    'OBJECT',
]

_MODES = _enumify_iterable(iterable = _MODE_NAMES, enum_dict = {})

_ASCII_CODEPOINT_MAP = {
    # Currenlty non-printable ascii characters are simply not referencable in
    # Java skylark
    # https://github.com/bazelbuild/bazel/issues/4862
    #
    # For now these characters just do not exist in the map, this means unicode
    # isn't supported.

    # "\x00" : 0, "\x01" : 1, "\x02" : 2, "\x03" : 3, "\x04" : 4, "\x05" : 5, "\x06" : 6, "\a" : 7,
    # "\b" : 8, "\t" : 9, "\n" : 10, "\v" : 11, "\f" : 12, "\r" : 13, "\x0E" : 14, "\x0F" : 15,
    # "\x10" : 16, "\x11" : 17, "\x12" : 18, "\x13" : 19, "\x14" : 20, "\x15" : 21, "\x16" : 22, "\x17" : 23,
    # "\x18" : 24, "\x19" : 25, "\x1A" : 26, "\e" : 27, "\x1C" : 28, "\x1D" : 29, "\x1E" : 30, "\x1F" : 31,
              "\t" : 9, "\n" : 10,                         "\r" : 13,

    " " : 32, "!" : 33, "\"" : 34, "#" : 35, "$" : 36, "%" : 37, "&" : 38, "'" : 39,
    "(" : 40, ")" : 41, "*" : 42, "+" : 43, "," : 44, "-" : 45, "." : 46, "/" : 47,
    "0" : 48, "1" : 49, "2" : 50, "3" : 51, "4" : 52, "5" : 53, "6" : 54, "7" : 55,
    "8" : 56, "9" : 57, ":" : 58, ";" : 59, "<" : 60, "=" : 61, ">" : 62, "?" : 63,

    "@" : 64, "A" : 65, "B" : 66, "C" : 67, "D" : 68, "E" : 69, "F" : 70, "G" : 71,
    "H" : 72, "I" : 73, "J" : 74, "K" : 75, "L" : 76, "M" : 77, "N" : 78, "O" : 79,
    "P" : 80, "Q" : 81, "R" : 82, "S" : 83, "T" : 84, "U" : 85, "V" : 86, "W" : 87,
    "X" : 88, "Y" : 89, "Z" : 90, "[" : 91, "\\" : 92, "]" : 93, "^" : 94, "_" : 95,

    "`" : 96, "a" : 97, "b" : 98, "c" : 99, "d" : 100, "e" : 101, "f" : 102, "g" : 103,
    "h" : 104, "i" : 105, "j" : 106, "k" : 107, "l" : 108, "m" : 109, "n" : 110, "o" : 111,
    "p" : 112, "q" : 113, "r" : 114, "s" : 115, "t" : 116, "u" : 117, "v" : 118, "w" : 119,
#    "x" : 120, "y" : 121, "z" : 122, "{" : 123, "|" : 124, "}" : 125, "~" : 126, "\x7F" : 127,
    "x" : 120, "y" : 121, "z" : 122, "{" : 123, "|" : 124, "}" : 125, "~" : 126,
}

# This array maps the 128 ASCII characters into character classes.
# The remaining Unicode characters should be mapped to C_ETC.
# Non-whitespace control characters are errors.
_ascii_tokens = [
    '__',      '__',      '__',      '__',      '__',      '__',      '__',      '__',
    '__',      'C_WHITE', 'C_WHITE', '__',      '__',      'C_WHITE', '__',      '__',
    '__',      '__',      '__',      '__',      '__',      '__',      '__',      '__',
    '__',      '__',      '__',      '__',      '__',      '__',      '__',      '__',

    'C_SPACE', 'C_ETC',   'C_QUOTE', 'C_ETC',   'C_ETC',   'C_ETC',   'C_ETC',   'C_ETC',
    'C_ETC',   'C_ETC',   'C_ETC',   'C_PLUS',  'C_COMMA', 'C_MINUS', 'C_POINT', 'C_SLASH',
    'C_ZERO',  'C_DIGIT', 'C_DIGIT', 'C_DIGIT', 'C_DIGIT', 'C_DIGIT', 'C_DIGIT', 'C_DIGIT',
    'C_DIGIT', 'C_DIGIT', 'C_COLON', 'C_ETC',   'C_ETC',   'C_ETC',   'C_ETC',   'C_ETC',

    'C_ETC',   'C_ABCDF', 'C_ABCDF', 'C_ABCDF', 'C_ABCDF', 'C_E',     'C_ABCDF', 'C_ETC',
    'C_ETC',   'C_ETC',   'C_ETC',   'C_ETC',   'C_ETC',   'C_ETC',   'C_ETC',   'C_ETC',
    'C_ETC',   'C_ETC',   'C_ETC',   'C_ETC',   'C_ETC',   'C_ETC',   'C_ETC',   'C_ETC',
    'C_ETC',   'C_ETC',   'C_ETC',   'C_LSQRB', 'C_BACKS', 'C_RSQRB', 'C_ETC',   'C_ETC',

    'C_ETC',   'C_LOW_A', 'C_LOW_B', 'C_LOW_C', 'C_LOW_D', 'C_LOW_E', 'C_LOW_F', 'C_ETC',
    'C_ETC',   'C_ETC',   'C_ETC',   'C_ETC',   'C_LOW_L', 'C_ETC',   'C_LOW_N', 'C_ETC',
    'C_ETC',   'C_ETC',   'C_LOW_R', 'C_LOW_S', 'C_LOW_T', 'C_LOW_U', 'C_ETC',   'C_ETC',
    'C_ETC',   'C_ETC',   'C_ETC',   'C_LCURB', 'C_ETC',   'C_RCURB', 'C_ETC',   'C_ETC'
]

def _create_ascii_mappings():
    ascii_mappings = []
    for token in _ascii_tokens:
        ascii_mappings.append(_TOKEN_CLASSES[token])
    return ascii_mappings

_ASCII_CLASS_LIST = _create_ascii_mappings()

_STATE_TRANSITION_TABLE = [
#[NR_STATES][NR_CLASSES] = {
#    The state transition table takes the current state and the current symbol,
#    and returns either a new state or an action. An action is represented as a
#    negative number. A JSON text is accepted if at the end of the text the
#    state is OK and if the mode is MODE_DONE.
#
#        white                                      1-9                                   ABCDF  etc
#    space |  {  }  [  ]  :  ,  "  \  /  +  -  .  0  |  a  b  c  d  e  f  l  n  r  s  t  u  |  E  |
    [_S['GO'],_S['GO'],-6,__,-5,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__], # start  GO
    [_S['OK'],_S['OK'],__,-8,__,-7,__,-3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__], # ok     OK
    [_S['OB'],_S['OB'],__,-9,__,__,__,__,_S['ST'],__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__], # object OB
    [_S['KE'],_S['KE'],__,__,__,__,__,__,_S['ST'],__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__], # key    KE
    [_S['CO'],_S['CO'],__,__,__,__,-2,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__], # colon  CO
    [_S['VA'],_S['VA'],-6,__,-5,__,__,__,_S['ST'],__,__,__,_S['MI'],__,_S['ZE'],_S['IN'],__,__,__,__,__,_S['F1'],__,_S['N1'],__,__,_S['T1'],__,__,__,__], # value  VA
    [_S['AR'],_S['AR'],-6,__,-5,-7,__,__,_S['ST'],__,__,__,_S['MI'],__,_S['ZE'],_S['IN'],__,__,__,__,__,_S['F1'],__,_S['N1'],__,__,_S['T1'],__,__,__,__], # array  AR
    [_S['ST'],__,_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],-4,_S['ES'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST']], # string ST
    [__,__,__,__,__,__,__,__,_S['ST'],_S['ST'],_S['ST'],__,__,__,__,__,__,_S['ST'],__,__,__,_S['ST'],__,_S['ST'],_S['ST'],__,_S['ST'],_S['U1'],__,__,__], # escape ES
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['U2'],_S['U2'],_S['U2'],_S['U2'],_S['U2'],_S['U2'],_S['U2'],_S['U2'],__,__,__,__,__,__,_S['U2'],_S['U2'],__], # u1     U1
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['U3'],_S['U3'],_S['U3'],_S['U3'],_S['U3'],_S['U3'],_S['U3'],_S['U3'],__,__,__,__,__,__,_S['U3'],_S['U3'],__], # u2     U2
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['U4'],_S['U4'],_S['U4'],_S['U4'],_S['U4'],_S['U4'],_S['U4'],_S['U4'],__,__,__,__,__,__,_S['U4'],_S['U4'],__], # u3     U3
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],_S['ST'],__,__,__,__,__,__,_S['ST'],_S['ST'],__], # u4     U4
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['ZE'],_S['IN'],__,__,__,__,__,__,__,__,__,__,__,__,__,__,__], # minus  MI
    [_S['OK'],_S['OK'],__,-8,__,-7,__,-3,__,__,__,__,__,_S['FR'],__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__], # zero   ZE
    [_S['OK'],_S['OK'],__,-8,__,-7,__,-3,__,__,__,__,__,_S['FR'],_S['IN'],_S['IN'],__,__,__,__,_S['E1'],__,__,__,__,__,__,__,__,_S['E1'],__], # int    IN
    [_S['OK'],_S['OK'],__,-8,__,-7,__,-3,__,__,__,__,__,__,_S['FR'],_S['FR'],__,__,__,__,_S['E1'],__,__,__,__,__,__,__,__,_S['E1'],__], # frac   FR
    [__,__,__,__,__,__,__,__,__,__,__,_S['E2'],_S['E2'],__,_S['E3'],_S['E3'],__,__,__,__,__,__,__,__,__,__,__,__,__,__,__], # e      E1
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['E3'],_S['E3'],__,__,__,__,__,__,__,__,__,__,__,__,__,__,__], # ex     E2
    [_S['OK'],_S['OK'],__,-8,__,-7,__,-3,__,__,__,__,__,__,_S['E3'],_S['E3'],__,__,__,__,__,__,__,__,__,__,__,__,__,__,__], # exp    E3
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['T2'],__,__,__,__,__,__], # tr     T1
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['T3'],__,__,__], # tru    T2
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['OK'],__,__,__,__,__,__,__,__,__,__], # true   T3
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['F2'],__,__,__,__,__,__,__,__,__,__,__,__,__,__], # fa     F1
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['F3'],__,__,__,__,__,__,__,__], # fal    F2
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['F4'],__,__,__,__,__], # fals   F3
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['OK'],__,__,__,__,__,__,__,__,__,__], # false  F4
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['N2'],__,__,__], # nu     N1
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['N3'],__,__,__,__,__,__,__,__], # nul    N2
    [__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,_S['OK'],__,__,__,__,__,__,__,__], # null   N3
]


_MAX_DEPTH = 20


def _reject(reason = "unknown reason"):
    fail("failed to parse JSON: %s" % reason)


def _push(checker, mode):
    checker['top'] += 1
    if (checker['top'] >= checker['max_depth']):
        _reject("max depth exceeded")

    checker['stack'].insert(checker['top'], mode)


def _pop(checker, mode):
    top = checker['top']
    if (top < 0):
        _reject("invalid top index")
    elif (not _peek_mode(checker) == mode):
        _reject("cannot pop unexpected mode %s expected %s" % (mode, checker['stack'][top]))

    checker['stack'] = checker['stack'][0:checker['top']]
    checker['top'] -= 1


def _set_state(checker, state):
    print("state transition (mode:%s): %s -> %s" % (
        _MODE_NAMES[_peek_mode(checker)], _STATE_NAMES[checker['state']], _STATE_NAMES[state]))
    checker['state'] = state


def _peek_mode(checker):
    top = checker['top']
    return checker['stack'][top]


def _create_checker(max_depth = _MAX_DEPTH):
    checker = {
        'stack': [],
        'top': -1,
        'max_depth': max_depth,
        'state': _STATES['GO']
    }
    _push(checker, _MODES['DONE'])
    return checker

def _handle_next_char(checker, next_string_char):
    next_class = None
    next_state = None

    next_char = _ASCII_CODEPOINT_MAP[next_string_char[0]]
    if (next_char == None):
        _reject("unprintable Java/skylark char: %s" % next_string_char)

    next_class = _ASCII_CLASS_LIST[next_char]
    if (next_class <= __):
        _reject("unknown character class for char: %s" % next_string_char)

    next_state = _STATE_TRANSITION_TABLE[checker['state']][next_class]

    OK = _STATES['OK']
    OB = _STATES['OB']
    AR = _STATES['AR']
    CO = _STATES['CO']
    KE = _STATES['KE']
    VA = _STATES['VA']

    if (next_state >= 0):
        _set_state(checker, next_state)
    else:
        if next_state == -9: # empty }
            _pop(checker, _MODES['KEY'])
            _set_state(checker, OK)

        elif next_state == -8: # }
            _pop(checker, _MODES['OBJECT'])
            _set_state(checker, OK)

        elif next_state == -7: # ]
            _pop(checker, _MODES['ARRAY'])
            _set_state(checker, OK)

        elif next_state == -6: # {
            _push(checker, _MODES['KEY'])
            _set_state(checker, OB) # NOT OK, OB

        elif next_state == -5: # [
            _push(checker, _MODES['ARRAY'])
            _set_state(checker, AR)

        elif next_state == -4: # "
            current_mode = _peek_mode(checker)
            if current_mode == _MODES['KEY']:
                _set_state(checker, CO)
            elif current_mode == _MODES['ARRAY'] or current_mode == _MODES['OBJECT']:
                _set_state(checker, OK)
            else:
                _reject("invalid state transition from mode: %s" % current_mode)

        elif next_state == -3: # ,
            current_mode = _peek_mode(checker)
            if current_mode == _MODES['OBJECT']:
                # A comma causes a flip from object mode to key mode.
                _pop(checker, _MODES['OBJECT'])
                _push(checker, _MODES['KEY'])
                _set_state(checker, KE)

            elif current_mode == _MODES['ARRAY']:
                _set_state(checker, VA)

            else:
                _reject("invalid state transition from mode: %s" % current_mode)

        elif next_state == -2: # :
            # A colon causes a flip from key mode to object mode.
            _pop(checker, _MODES['KEY'])
            _push(checker, _MODES['OBJECT'])
            _set_state(checker, VA)

        else:
            _reject("invalid action: %s" % next_state)

    return True


def _verify_done(checker):
    return checker['state'] == _STATES['OK'] and _peek_mode(checker) == _MODES['DONE']


def json_checker():
#     print(_TOKEN_CLASSES)
#     print(_ASCII_CLASS_LIST)
#     print(_STATES)
#     print(_MODES)

#     fd_checker = _create_checker()
#     print(fd_checker)

#     _push(fd_checker, _MODES['KEY'])
#     print(fd_checker)

#     _pop(fd_checker, _MODES['KEY'])
#     print(fd_checker)

#     _pop(fd_checker, _MODES['DONE'])
#     print(fd_checker)

# #    print("chars: %s" % " ".join(["\U00", "\U01", "\U02"]))
#     print(_ASCII_CODEPOINT_MAP)

    checker = _create_checker()
    json = '{"foo": "bar", "biz": [1,2,3]}'
    for i  in range(0, len(json)):
        print("json char: %s" % json[i])
        print(_handle_next_char(checker, json[i]))

    print("is valid parse: %s" % _verify_done(checker))

#    print()
#    print("%s : %s" % (json[0], ord(json[0])))

