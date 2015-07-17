{--

type Verb = Text
type Fact = Text

data Log = Verb
         | Fact

type Entry = (LocalTime, [Log])
type Instead = [Log]
type And = [Log]
type Set = () --Not going to be implemented yet

data Mod = Instead
         | And

data Command = Add (Entry, [Mod])
             | Config [Set]

data State = State { totals :: Map Verb DiffTime }

type Tape = ((LocalTime, State))

--}

